#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <stdlib.h>

#include "teller.h"
#include "bankServer.h"

typedef struct {
    int status;
    int writefd;
    int readfd;
    int clientfd;
    int syncfd;
    client * clientTable;
    tellerSync * syncTable;
    int amount;
    response result;
    int bankID;
    int clientTableBytes;
    int syncTableBytes;
} tellerData;

int tellerInit(const request * r, tellerData * data) {
    int status;

    data->writefd = -1;
    data->readfd = -1;
    data->clientfd = -1;
    data->syncfd = -1;
    data->clientTable = NULL;
    data->syncTable = NULL;

    printf(" -- Teller PID%02d is active serving Client%02d...\n", getpid(), r->clientID + 1);
    while((data->writefd = open(r->writeFIFOPath, O_WRONLY)) == -1 && errno == EINTR) {
        if(terminate) {
            return RESPONSE_TERMINATION;
        }
    }
    if(data->writefd == -1) {
        fprintf(stderr, "ERROR: [Teller PID%02d] Could not open write fifo: %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        return RESPONSE_INVALID_REQUEST;
    }

    while((data->readfd = open(r->readFIFOPath, O_RDONLY)) == -1 && errno == EINTR) {
        if(terminate) {
            return RESPONSE_TERMINATION;
        }
    }
    if(data->readfd == -1) {
        fprintf(stderr, "ERROR: [Teller PID%02d] Could not open read fifo %s (%s)\n", getpid(), r->readFIFOPath, strerror(errno));
        if(dprintf(data->writefd, "Client%02d connected... Could not open read fifo\n", r->clientID + 1) < 0) {
            fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        }
        return RESPONSE_INVALID_REQUEST;
    }
    
    int bytesRemaining = sizeof(data->amount);
    while(bytesRemaining > 0) {
        while((status = read(data->readfd, ((char*)&data->amount) + sizeof(data->amount) - bytesRemaining, bytesRemaining)) == -1 && errno == EINTR) {
            if(terminate) {
                return RESPONSE_TERMINATION;
            }
        }
        if(status == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] read fifo (%s)\n", getpid(), strerror(errno));
            if(dprintf(data->writefd, "Client%02d connected... Could not read fifo\n", r->clientID + 1) < 0) {
                fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
            }
            return RESPONSE_INVALID_REQUEST;
        }
        if(status == 0) {
            break;
        }
        bytesRemaining -= status;
    }

    if(bytesRemaining > 0) {
        fprintf(stderr, "ERROR: [Teller PID%02d] Expected %d bytes from fifo, got %d\n", getpid(), (int)sizeof(data->amount), (int)sizeof(data->amount) - bytesRemaining);
        if(dprintf(data->writefd, "Client%02d connected... Invalid amount\n", r->clientID + 1) < 0) {
            fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        }
        return RESPONSE_INVALID_REQUEST;
    }

    if(data->amount <= 0) {
        if(dprintf(data->writefd, "Client%02d connected... Invalid amount (must be positive)\n", r->clientID + 1) < 0) {
            fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        }
        return RESPONSE_INVALID_REQUEST;
    }

    data->clientfd = shm_open(CLIENT_SHM_NAME, O_RDONLY, 0644);
    if(data->clientfd == -1) {
        fprintf(stderr, "ERROR: [Teller PID%02d] shm_open clients (%s)\n", getpid(), strerror(errno));
        return RESPONSE_INTERNAL_ERROR;
    }

    data->clientTableBytes = sizeof(client) * (r->clientID + 1);
    data->clientTable = mmap(NULL, data->clientTableBytes, PROT_READ, MAP_SHARED, data->clientfd, 0);
    if(data->clientTable == MAP_FAILED) {
        fprintf(stderr, "ERROR: [Teller PID%02d] mmap clients\n", getpid());
        return RESPONSE_INTERNAL_ERROR;
    }

    if(data->clientTable[r->clientID].accountID < 0) {
        fprintf(stderr, "ERROR: [Teller PID%02d] no account linked to client\n", getpid());
        return RESPONSE_INVALID_REQUEST;
    }

    data->bankID = data->clientTable[r->clientID].accountID;

    data->syncfd = shm_open(SYNC_SHM_NAME, O_RDWR, 0644);
    if(data->syncfd == -1) {
        fprintf(stderr, "ERROR: [Teller PID%02d] shm_open sync memory (%s)\n", getpid(), strerror(errno));
        return RESPONSE_INTERNAL_ERROR;
    }

    data->syncTableBytes = sizeof(tellerSync) + sizeof(tellerAccountSync) * (data->bankID + 1);
    data->syncTable = mmap(NULL, data->syncTableBytes, PROT_WRITE | PROT_READ, MAP_SHARED, data->syncfd, 0);
    if(data->syncTable == MAP_FAILED) {
        fprintf(stderr, "ERROR: [Teller PID%02d] mmap sync memory (%s)\n", getpid(), strerror(errno));
        return RESPONSE_INTERNAL_ERROR;
    }

    return RESPONSE_SUCCESS;
}

void tellerFinish(const request * req, response * res, tellerData * data) {
    int status;

    if(data->clientTable != NULL) {
        if(munmap(data->clientTable, data->clientTableBytes) == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] munmap client (%s)\n", getpid(), strerror(errno));
        }
    }

    if(data->syncTable != NULL) {
        if(munmap(data->syncTable, data->syncTableBytes) == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] munmap sync (%s)\n", getpid(), strerror(errno));
        }
    }

    if(data->writefd != -1) {
        if(res->type == RESPONSE_INVALID_REQUEST) {
            if(dprintf(data->writefd, "Client%02d served... Invalid request\n", req->clientID + 1) < 0) {
                fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), req->writeFIFOPath, strerror(errno));
            }
        }

        if(res->type == RESPONSE_INTERNAL_ERROR) {
            if(dprintf(data->writefd, "Client%02d served... Internal error\n", req->clientID + 1) < 0) {
                fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), req->writeFIFOPath, strerror(errno));
            }
        }

        if(res->type == RESPONSE_TERMINATION) {
            if(dprintf(data->writefd, "Client%02d served... Cancelled by server\n", req->clientID + 1) < 0) {
                fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), req->writeFIFOPath, strerror(errno));
            }
        }

        while((status = close(data->writefd)) == -1 && errno == EINTR);
        if(status == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] close write fifo (%s)\n", getpid(), strerror(errno));
        }
    }

    if(data->readfd != -1) {
        while((status = close(data->readfd)) == -1 && errno == EINTR);
        if(status == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] close read fifo (%s)\n", getpid(), strerror(errno));
        }
    }

    if(data->clientfd != -1) {
        while((status = close(data->clientfd)) == -1 && errno == EINTR);
        if(status == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] close client (%s)\n", getpid(), strerror(errno));
        }
    }

    if(data->syncfd != -1) {
        while((status = close(data->syncfd)) == -1 && errno == EINTR);
        if(status == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] close sync (%s)\n", getpid(), strerror(errno));
        }
    }

    while((status = write(req->pipe, res, sizeof(*res))) == -1 && errno == EINTR);
    if(status == -1) {
        fprintf(stderr, "ERROR: [Teller PID%02d] write to pipe (%s)\n", getpid(), strerror(errno));
    }

    while((status = close(req->pipe)) == -1 && errno == EINTR);
    if(status == -1) {
        fprintf(stderr, "ERROR: [Teller PID%02d] close pipe (%s)\n", getpid(), strerror(errno));
    }
}

int deposit(void * arg) {
    int status;

    request * r = arg;
    tellerData data;

    response result;
    result.type = tellerInit(r, &data);
    result.arg = 0;

    tellerAccountSync * syncAccountTable = (void*)&data.syncTable[1];
    char bankIDStr[sizeof(((tellerAccountSync*)0)->bankID)];

    int accountClosed = 0;

    if(result.type == RESPONSE_SUCCESS) {
        if(dprintf(data.writefd, "Client%02d connected... depositing %d credits\n", r->clientID + 1, data.amount) < 0) {
            fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        }

        while((status = sem_wait(&data.syncTable->mutex)) == -1 && errno == EINTR) {
            if(terminate) {
                result.type = RESPONSE_TERMINATION;
            }
        }
        if(status == -1 && errno != EINTR) {
            fprintf(stderr, "ERROR: [Teller PID%02d] sem_wait (%s)\n", getpid(), strerror(errno));
            result.type = RESPONSE_INTERNAL_ERROR;
        }
    }

    if(result.type == RESPONSE_SUCCESS) {
        syncAccountTable[data.bankID].depositsPending--;

        if(syncAccountTable[data.bankID].removed) {
            accountClosed = 1;
        }
        else {
            syncAccountTable[data.bankID].balance += data.amount;
            strcpy(bankIDStr, syncAccountTable[data.bankID].bankID);
        }

        result.arg = data.amount;

        if(sem_post(&data.syncTable->mutex) == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] sem_post (%s)\n", getpid(), strerror(errno));
        }

        if(!accountClosed) {
            status = dprintf(data.writefd, "Client%02d served... %s\n", r->clientID + 1, bankIDStr);
        }
        else {
            status = dprintf(data.writefd, "Client%02d served... account is already closed\n", r->clientID + 1);
            result.type = RESPONSE_NO_PERMISSION;
        }

        if(status < 0) {
            fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        }
    }

    tellerFinish(r, &result, &data);

    free(r);

    return 0;
}

int withdraw(void * arg) {
    int status;

    request * r = arg;
    tellerData data;

    response result;
    result.type = tellerInit(r, &data);
    result.arg = 0;

    tellerAccountSync * syncAccountTable = (void*)&data.syncTable[1];
    char bankIDStr[sizeof(((tellerAccountSync*)0)->bankID)];

    int alreadyClosed = 0;
    int accountClosed = 0;
    int notEnoughCredit = 0;

    if(result.type == RESPONSE_SUCCESS) {
        if(dprintf(data.writefd, "Client%02d connected... withdrawing %d credits\n", r->clientID + 1, data.amount) < 0) {
            fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        }

        do {
            while((status = sem_wait(&data.syncTable->mutex)) == -1 && errno == EINTR) {
                if(terminate) {
                    result.type = RESPONSE_TERMINATION;
                    break;
                }
            }
        }
        while(status != -1 && syncAccountTable[data.bankID].depositsPending > 0 && (status = sem_post(&data.syncTable->mutex)) != -1);

        if(status == -1 && errno != EINTR) {
            fprintf(stderr, "ERROR: [Teller PID%02d] semaphore (%s)\n", getpid(), strerror(errno));
            result.type = RESPONSE_INTERNAL_ERROR;
        }
    }

    if(result.type == RESPONSE_SUCCESS) {
        if(syncAccountTable[data.bankID].removed) {
            alreadyClosed = 1;
        }
        else if(syncAccountTable[data.bankID].balance < data.amount) {
            notEnoughCredit = 1;
        }
        else {
            syncAccountTable[data.bankID].balance -= data.amount;
            if(syncAccountTable[data.bankID].balance == 0) {
                accountClosed = 1;
            }
            strcpy(bankIDStr, syncAccountTable[data.bankID].bankID);
        }

        result.arg = data.amount;

        if(sem_post(&data.syncTable->mutex) == -1) {
            fprintf(stderr, "ERROR: [Teller PID%02d] sem_post (%s)\n", getpid(), strerror(errno));
        }

        if(alreadyClosed) {
            status = dprintf(data.writefd, "Client%02d served... account is already closed\n", r->clientID + 1);
            result.type = RESPONSE_NO_PERMISSION;
        }
        if(accountClosed) {
            status = dprintf(data.writefd, "Client%02d served... account closed\n", r->clientID + 1);
        }
        else if(notEnoughCredit) {
            status = dprintf(data.writefd, "Client%02d served... not enough credit\n", r->clientID + 1);
            result.type = RESPONSE_NO_PERMISSION;
        }
        else {
            status = dprintf(data.writefd, "Client%02d served... %s\n", r->clientID + 1, bankIDStr);
        }

        if(status < 0) {
            fprintf(stderr, "ERROR: [Teller PID%02d] Could not write to fifo %s (%s)\n", getpid(), r->writeFIFOPath, strerror(errno));
        }
    }

    tellerFinish(r, &result, &data);
    
    free(r);

    return 0;
}

pid_t Teller(int (*func)(void*), void * arg_func) {
    int pid;
    pid = fork();
    if(pid == 0) {
        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_handler = SIG_DFL;
        sigaction(SIGCHLD, &sa, NULL);
        releaseServerResources();
        _exit(func(arg_func));
    }
    return pid;
}

int waitTeller(pid_t pid, int * status) {
    return waitpid(pid, status, WNOHANG);
}
