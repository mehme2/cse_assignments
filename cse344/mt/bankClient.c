#include <linux/limits.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <signal.h>
#include <sys/stat.h>

#include "util.h"

typedef struct {
    char * bankID;
    requestType type;
    int amount;
    pid_t pid;
    char writeFIFOPath[PATH_MAX];
    char readFIFOPath[PATH_MAX];
    int writeFIFOExists, readFIFOExists;
} client;

client * clientTable = NULL;
int nClients = 0;
int clientTableSize = 0;

int terminate = 0;

pid_t rootpid;

void signalHandler(int signal) {
    switch(signal) {
        case SIGINT:
        case SIGTERM:
            terminate = 1;
            break;
    }
}

int addClient(const client * c) {
    if(clientTable == NULL) {
        int startSize = 4;
        void * mem = malloc(sizeof(*c) * startSize);
        if(mem == NULL) {
            return -1;
        }
        clientTable = mem;
        clientTableSize = startSize;
    }

    if(nClients == clientTableSize) {
        int newSize = clientTableSize * 2;
        void * mem = realloc(clientTable, sizeof(*c) * newSize);
        if(mem == NULL) {
            return -1;
        }
        clientTable = mem;
        clientTableSize = newSize;
    }

    nClients++;
    memcpy(&clientTable[nClients - 1], c, sizeof(*c));

    return nClients - 1;
}

void freeClients() {
    if(clientTable != NULL) {
        pid_t pid = getpid();
        for(int i = 0; i < nClients; ++i) {
            free(clientTable[i].bankID);
            if(rootpid == pid) {
                if(clientTable[i].writeFIFOExists) {
                    if(unlink(clientTable[i].writeFIFOPath) == -1) {
                        perror("ERROR: unlink");
                    }
                }
                if(clientTable[i].readFIFOExists) {
                    if(unlink(clientTable[i].readFIFOPath) == -1) {
                        perror("ERROR: unlink");
                    }
                }
            }
        }
        free(clientTable);
    }
}

void terminateClients() {
    for(int i = 0; i < nClients; ++i) {
        if(clientTable[i].pid > 0) kill(clientTable[i].pid, SIGTERM);
    }

    do {
        while(wait(NULL) > 0);
    }
    while(errno == EINTR);
}

void waitClients() {
    do {
        pid_t pid;
        while((pid = wait(NULL)) > 0) {
            for(int i = 0; i < nClients; ++i) {
                if(pid == clientTable[i].pid) clientTable[i].pid = 0;
            }
        }
    }
    while(errno == EINTR && !terminate);

    if(terminate) terminateClients();
}

int main(int argc, const char ** argv) {
    if(argc != 3) {
        printf("Usage: BankClient \"ClientFile\" \"FIFOName\"\n");
        return 0;
    }

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = &signalHandler;
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);

    int clientfd = open(argv[1], O_RDONLY);

    if(clientfd == -1) {
        fprintf(stderr, "ERROR: open client file (%s)\n", strerror(errno));
    }

    rootpid = getpid();

    int loadError = 0;

    while(1) {
        client c;
        c.pid = 0;
        c.writeFIFOExists = 0;
        c.readFIFOExists = 0;
        int status = nextWordInLine(clientfd, &c.bankID);

        if(status == 0) break;

        if(status == -1) {
            fprintf(stderr, "ERROR: read client file (%s)\n", strerror(errno));
            loadError = 1;
            break;
        }

        if(c.bankID == NULL) continue;

        char * nextToken;
        status = nextWordInLine(clientfd, &nextToken);

        if(status == -1) {
            fprintf(stderr, "ERROR: read client file (%s)\n", strerror(errno));
            free(c.bankID);
            loadError = 1;
            break;
        }

        if(status == 0 || nextToken == NULL) {
            fprintf(stderr, "ERROR: not enough tokens\n");
            free(c.bankID);
            loadError = 1;
            break;
        }

        if(strcmp("deposit", nextToken) == 0) {
            c.type = REQUEST_DEPOSIT;
        }
        else if(strcmp("withdraw", nextToken) == 0) {
            c.type = REQUEST_WITHDRAW;
        }
        else {
            fprintf(stderr, "ERROR: invalid token: %s\n", nextToken);
            free(nextToken);
            free(c.bankID);
            loadError = 1;
            break;
        }

        free(nextToken);

        status = nextWordInLine(clientfd, &nextToken);

        if(status == -1) {
            fprintf(stderr, "ERROR: read client file (%s)\n", strerror(errno));
            free(c.bankID);
            loadError = 1;
            break;
        }

        if(status == 0 || nextToken == NULL) {
            fprintf(stderr, "ERROR: not enough tokens\n");
            free(c.bankID);
            loadError = 1;
            break;
        }

        if(isInteger(nextToken)) {
            c.amount = atoi(nextToken);
        }
        else {
            fprintf(stderr, "ERROR: invalid token: %s\n", nextToken);
            free(nextToken);
            free(c.bankID);
            loadError = 1;
            break;
        }

        free(nextToken);

        int idx = addClient(&c);

        if(idx < 0) {
            fprintf(stderr, "ERROR: memory (%s)\n", strerror(errno));
            free(c.bankID);
            loadError = 1;
            break;
        }
    }

    int status;
    while((status = close(clientfd)) == -1 && errno == EINTR);
    if(status == -1) {
        fprintf(stderr, "ERROR: close client file (%s)\n", strerror(errno));
    }

    if(loadError) {
        printf("An error occurred while loading clients, exiting...");
        freeClients();
        return -1;
    }

    char nameBuf[32];
    int nameLength = snprintf(nameBuf, sizeof(nameBuf), "PIDClient%d", rootpid);

    int requestBytes = sizeof(nClients) + sizeof(nameLength) + nameLength;

    char workingDirectory[PATH_MAX];

    getcwd(workingDirectory, PATH_MAX);

    pid_t pid = -1;

    int i;
    for(i = 0; i < nClients; ++i) {
        int bil = strlen(clientTable[i].bankID);
        int wfl = snprintf(clientTable[i].writeFIFOPath, sizeof(clientTable[i].writeFIFOPath), "%s/writefifo_%d_%d_220104004051", workingDirectory, rootpid, i);
        int rfl = snprintf(clientTable[i].readFIFOPath, sizeof(clientTable[i].readFIFOPath), "%s/readfifo_%d_%d_220104004051", workingDirectory, rootpid, i);

        if(mkfifo(clientTable[i].writeFIFOPath, 0644) == -1) {
            fprintf(stderr, "ERROR: mkfifo (%s)\n", strerror(errno));
            break;
        }

        clientTable[i].writeFIFOExists = 1;

        if(mkfifo(clientTable[i].readFIFOPath, 0622) == -1) {
            fprintf(stderr, "ERROR: mkfifo (%s)\n", strerror(errno));
            break;
        }

        clientTable[i].readFIFOExists = 1;

        requestBytes += sizeof(clientTable[i].type) + sizeof(bil) + bil + sizeof(wfl) + wfl + sizeof(rfl) + rfl;

        pid = fork();
        if(pid == -1) {
            fprintf(stderr, "ERROR: fork (%s)\n", strerror(errno));
            break;
        }
        if(pid == 0) {
            break;
        }
        clientTable[i].pid = pid;
    }

    if(pid == 0) {
        client * c = &clientTable[i];
        int writeFIFO;
        int readFIFO;

        while((readFIFO = open(c->readFIFOPath, O_RDONLY)) == -1 && errno == EINTR) {
            if(terminate) {
                freeClients();
                return 0;
            }
        }

        while((writeFIFO = open(c->writeFIFOPath, O_WRONLY)) == -1 && errno == EINTR) {
            if(terminate) {
                freeClients();
                return 0;
            }
        }

        while(write(writeFIFO, &c->amount, sizeof(c->amount)) == -1 && errno == EINTR) {
            if(terminate) {
                freeClients();
                return 0;
            }
        }

        int bufSize = 4;
        char * buf = malloc(bufSize);
        int idx = 0;

        char next;
        
        int status;

        while(1) {
            status = read(readFIFO, &next, 1);

            if(status == -1) {
                if(errno == EINTR && !terminate) {
                    continue;
                }
                break;
            }

            if(status == 0) {
                write(STDOUT_FILENO, buf, idx);
                break;
            }

            if(bufSize <= idx) {
                bufSize *= 2;
                buf = realloc(buf, bufSize);
            }

            buf[idx++] = next;

            if(next == '\n') {
                write(STDOUT_FILENO, buf, idx);
                idx = 0;
            }
        }

        free(buf);

        freeClients();
        return 0;
    }

    char * transmitData = malloc(requestBytes);

    if(transmitData == NULL) {
        fprintf(stderr, "ERROR: malloc (%s)\n", strerror(errno));
    }

    if(i < nClients || transmitData == NULL) {
        printf("An error occurred while creating clients, exiting...");
        terminateClients();
        freeClients();
        return -1;
    }

    int idx = 0;

    memcpy(transmitData + idx, &nClients, sizeof(nClients));
    idx += sizeof(nClients);

    memcpy(transmitData + idx, &nameLength, sizeof(nameLength));
    idx += sizeof(nameLength);

    memcpy(transmitData + idx, nameBuf, nameLength);
    idx += nameLength;

    for(i = 0; i < nClients; ++i) {
        int bil = strlen(clientTable[i].bankID);
        int wfl = strlen(clientTable[i].writeFIFOPath);
        int rfl = strlen(clientTable[i].readFIFOPath);

        memcpy(transmitData + idx, &clientTable[i].type, sizeof(clientTable[i].type));
        idx += sizeof(clientTable[i].type);

        memcpy(transmitData + idx, &bil, sizeof(bil));
        idx += sizeof(bil);

        memcpy(transmitData + idx, clientTable[i].bankID, bil);
        idx += bil;

        memcpy(transmitData + idx, &wfl, sizeof(wfl));
        idx += sizeof(wfl);

        memcpy(transmitData + idx, clientTable[i].writeFIFOPath, wfl);
        idx += wfl;

        memcpy(transmitData + idx, &rfl, sizeof(rfl));
        idx += sizeof(bil);

        memcpy(transmitData + idx, clientTable[i].readFIFOPath, rfl);
        idx += rfl;
    }

    int fifofd = open(argv[2], O_WRONLY);

    if(fifofd == -1) {
        fprintf(stderr, "ERROR: open server fifo (%s)\n", strerror(errno));
        free(transmitData);
        terminateClients();
        freeClients();
        return -1;
    }

    while((status = write(fifofd, transmitData, requestBytes)) == -1 && errno == EINTR);
    if(status == -1) {
        fprintf(stderr, "ERROR: write server fifo (%s)\n", strerror(errno));
        while(close(fifofd) == -1 && errno == EINTR);
        free(transmitData);
        terminateClients();
        freeClients();
        return -1;
    }

    while(close(fifofd) == -1 && errno == EINTR);
    free(transmitData);
    waitClients();
    freeClients();
    return 0;
}
