#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/ucontext.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <linux/limits.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <semaphore.h>

#include "teller.h"
#include "bankServer.h"
#include "util.h"

typedef struct {
    enum {
        TRANSACTION_DEPOSIT,
        TRANSACTION_WITHDRAW
    } type;
    int amount;
} transaction;

typedef struct {
    char bankID[32];
    transaction * history;
    int nTransactions;
    int historySize;
} account;

typedef struct {
    pid_t pid;
    int pipe, done, type, clientID;
} tellerData;

account * accountTable = NULL;
int nAccounts = 0;
int accountTableSize = 0;

int syncshmfd = -1;
tellerSync * syncTable = NULL;

int clientshmfd = -1;
client * clientTable = NULL;
int nClients = 0;
int clientTableSize = 0;

int logfd = -1;
int fifofd = -1;

tellerData * tellerTable = NULL;
int nTellers;
int tellerTableSize;

int terminate = 0;

pid_t rootpid;

const char * bankName;
const char * fifoName;

request ** requestsToFree = NULL;
request * doNotFreeThisRequest = NULL;
int nRequestsToFree = 0;

int alsoCloseThisFD = 0;

int getAccountBalance(int id) {
    int res = 0;
    for(int i = 0; i < accountTable[id].nTransactions; ++i) {
        if(accountTable[id].history[i].type == TRANSACTION_DEPOSIT) {
            res += accountTable[id].history[i].amount;
        }
        if(accountTable[id].history[i].type == TRANSACTION_WITHDRAW) {
            res -= accountTable[id].history[i].amount;
        }
    }
    return res;
}

int addTransaction(int id, const transaction * t) {
    if(accountTable[id].history == NULL) {
        int startSize = 4;
        void * mem = malloc(sizeof(transaction) * startSize);
        if(mem == NULL) {
            return -1;
        }
        accountTable[id].history = mem;
        accountTable[id].historySize = startSize;
    }

    if(accountTable[id].nTransactions == accountTable[id].historySize) {
        int newSize = accountTable[id].historySize * 2;
        void * mem = realloc(accountTable[id].history, sizeof(transaction) * newSize);
        if(mem == NULL) {
            return -1;
        }
        accountTable = mem;
        accountTable[id].historySize = newSize;
    }

    accountTable[id].nTransactions++;
    memcpy(&accountTable[id].history[accountTable[id].nTransactions - 1], t, sizeof(*t));

    return accountTable[id].nTransactions - 1;
}

int searchAccountByID(const char * id) {
    for(int i = 0; i < nAccounts; ++i) {
        if(strncmp(id, accountTable[i].bankID, sizeof(accountTable[i].bankID)) == 0) {
            return i;
        }
    }
    return -1;
}

int addAccount(const account * a) {
    if(accountTableSize == 0) {
        int startSize = 32;
        void * mem = malloc(sizeof(account) * startSize);
        if(mem == NULL) {
            return -1;
        }
        accountTable = mem;

        int syncTableBytes = sizeof(tellerSync) + startSize * sizeof(tellerAccountSync);
        syncshmfd = shm_open(SYNC_SHM_NAME, O_CREAT | O_EXCL | O_RDWR, 0644);
        if(syncshmfd == -1 || ftruncate(syncshmfd, syncTableBytes) == -1) {
            return -1;
        }
        mem = mmap(NULL, syncTableBytes, PROT_READ | PROT_WRITE, MAP_SHARED, syncshmfd, 0);
        if(mem == MAP_FAILED) {
            return -1;
        }
        syncTable = mem;

        accountTableSize = startSize;

        if(sem_init(&syncTable->mutex, 1, 1) == -1) {
            return -1;
        }
    }

    if(accountTableSize < nAccounts + 1) {
        int newSize = accountTableSize * 2;
        void * mem = realloc(accountTable, sizeof(account) * newSize);
        if(mem == NULL) {
            return -1;
        }
        accountTable = mem;

        int syncTableBytesOld = sizeof(tellerSync) + accountTableSize * sizeof(tellerAccountSync);
        int syncTableBytes = sizeof(tellerSync) + newSize * sizeof(tellerAccountSync);
        if(munmap(syncTable, syncTableBytesOld) == -1 || ftruncate(syncshmfd, syncTableBytes == -1)) {
            return -1;
        }
        mem = mmap(NULL, syncTableBytes, PROT_READ | PROT_WRITE, MAP_SHARED, syncshmfd, 0);
        if(mem == MAP_FAILED) {
            return -1;
        }
        syncTable = mem;

        accountTableSize = newSize;
    }

    nAccounts++;

    memcpy(&accountTable[nAccounts - 1], a, sizeof(*a));

    tellerAccountSync * syncAccountTable = (void*)&syncTable[1];
    strcpy(syncAccountTable[nAccounts - 1].bankID, a->bankID);
    syncAccountTable[nAccounts - 1].balance = getAccountBalance(nAccounts - 1);
    syncAccountTable[nAccounts - 1].depositsPending = 0;
    syncAccountTable[nAccounts - 1].removed = 0;

    return nAccounts - 1;
}

int addClient(const client * c) {
    if(clientTableSize == 0) {
        int startSize = 32;
        clientshmfd = shm_open(CLIENT_SHM_NAME, O_CREAT | O_EXCL | O_RDWR, 0644);
        if(clientshmfd == -1 || ftruncate(clientshmfd, startSize * sizeof(client)) == -1) {
            return -1;
        }
        void * mem = mmap(NULL, startSize * sizeof(client), PROT_READ | PROT_WRITE, MAP_SHARED, clientshmfd, 0);
        if(mem == MAP_FAILED) {
            return -1;
        }
        clientTableSize = startSize;
        clientTable = mem;
    }

    if(clientTableSize < nClients + 1) {
        int newSize = clientTableSize * 2;
        if(munmap(clientTable, clientTableSize * sizeof(client)) == -1 || ftruncate(clientshmfd, newSize * sizeof(client)) == -1) {
            return -1;
        }
        void * mem = mmap(NULL, newSize * sizeof(client), PROT_READ | PROT_WRITE, MAP_SHARED, clientshmfd, 0);
        if(mem == MAP_FAILED) {
            return -1;
        }
        clientTableSize = newSize;
        clientTable = mem;
    }

    memcpy(&clientTable[nClients], c, sizeof(*c));

    nClients++;
    return nClients - 1;
}

int clientByBankID(const char * id) {
    int bankID = searchAccountByID(id);
    if(bankID != -1 && getAccountBalance(bankID) > 0) {
        for(int i = 0; i < nClients; ++i) {
            if(clientTable[i].accountID == bankID) {
                return i;
            }
        }
    }
    else if(strcmp("N", id) == 0) {
        account a;
        a.history = NULL;
        a.nTransactions = 0;
        int i = 1;
        do {
            snprintf(a.bankID, sizeof(a.bankID), "BankID_%02d", i);
            i++;
        }
        while(searchAccountByID(a.bankID) != -1);
        bankID = addAccount(&a);
        if(bankID == -1) return -1;
    }

    client c;
    c.accountID = bankID;
    return addClient(&c);
}

void releaseServerResources() {
    int res;

    if(requestsToFree != NULL) {
        for(int i = 0; i < nRequestsToFree; ++i) {
            if(requestsToFree[i] != doNotFreeThisRequest) free(requestsToFree[i]);
        }
        free(requestsToFree);
    }

    if(alsoCloseThisFD > 0) {
        while(close(alsoCloseThisFD) == -1 && EINTR == errno);
    }

    if(logfd != -1) {
        while((res = close(logfd)) == -1 && errno == EINTR);
        if(res == -1) {
            perror("ERROR: close log");
        }
        logfd = -1;
    }

    if(fifofd != -1) {
        while((res = close(fifofd)) == -1 && errno == EINTR);
        if(res == -1) {
            perror("ERROR: close fifo");
        }
        fifofd = -1;

        if(rootpid == getpid()) {
            if(unlink(fifoName) == -1) {
                perror("ERROR: unlink server fifo");
            }
        }
    }

    if(accountTable != NULL) {
        for(int i = 0; i < nAccounts; ++i) {
            if(accountTable[i].history != NULL) {
                free(accountTable[i].history);
            }
        }
        free(accountTable);
        nAccounts = accountTableSize = 0;
        accountTable = NULL;
    }

    if(tellerTable != NULL) {
        for(int i = 0; i < nTellers; ++i) {
            while((res = close(tellerTable[i].pipe)) == -1 && errno == EINTR);
            if(res == -1) {
                perror("ERROR: close teller pipe");
            }
        }
        free(tellerTable);
        nTellers = tellerTableSize = 0;
        tellerTable = NULL;
    }

    if(clientTable != NULL) {
        if(munmap(clientTable, clientTableSize * sizeof(client)) == -1) {
            perror("ERROR: munmap");
        }
        clientTable = NULL;
        nClients = clientTableSize = 0;
    }

    if(syncTable != NULL) {
        if(sem_destroy(&syncTable->mutex) == -1) {
            perror("ERROR: sem_destroy");
        }

        if(munmap(syncTable, sizeof(*syncTable) + accountTableSize * sizeof(tellerAccountSync)) == -1) {
            perror("ERROR: munmap");
        }
        clientTable = NULL;
        nClients = clientTableSize = 0;
    }

    if(clientshmfd != -1) {
        while((res = close(clientshmfd)) == -1 && errno == EINTR);
        if(res == -1) {
            perror("ERROR: close client shared memory");
        }
        clientshmfd = -1;

        if(rootpid == getpid()) {
            if(shm_unlink(CLIENT_SHM_NAME) == -1) {
                perror("ERROR: unlink client shm");
            }
        }
    }

    if(syncshmfd != -1) {
        while((res = close(syncshmfd)) == -1 && errno == EINTR);
        if(res == -1) {
            perror("ERROR: close sync shared memory");
        }
        syncshmfd = -1;

        if(rootpid == getpid()) {
            if(shm_unlink(SYNC_SHM_NAME) == -1) {
                perror("ERROR: unlink sync shm");
            }
        }
    }
}

int addTeller(const tellerData * t) {
    if(tellerTableSize == 0) {
        int startSize = 1;
        void * mem = malloc(sizeof(tellerData) * startSize);
        if(mem == NULL) {
            return -1;
        }
        tellerTableSize = startSize;
        tellerTable = mem;
    }

    if(tellerTableSize < nTellers + 1) {
        int newSize = tellerTableSize * 2;
        void * mem = realloc(tellerTable, sizeof(tellerData) * newSize);
        if(mem == NULL) {
            return -1;
        }
        tellerTableSize = newSize;
        tellerTable = mem;
    }

    memcpy(&tellerTable[nTellers], t, sizeof(*t));

    nTellers++;
    return nTellers - 1;
}

void signalHandler(int signal) {
    int pid;
    int status;
    switch(signal) {
        case SIGINT:
        case SIGTERM:
            terminate = 1;
            break;
        
        case SIGCHLD:
            while((pid = waitTeller(-1, &status)) > 0) {
                int tid;
                for(tid = 0; tid < nTellers && tellerTable[tid].pid != pid; ++tid);
                if(tid < nTellers) {
                    tellerTable[tid].done = 1;
                }
            }
            break;
    }
}

int updateLog() {
    int status;
    if(lseek(logfd, 0, SEEK_SET) == -1) {
        return -1;
    }

    while((status = ftruncate(logfd, 0)) == -1 && errno == EINTR);
    if(status == -1) {
        return -1;
    }

    for(int i = 0; i < nAccounts; ++i) {
        int balance = getAccountBalance(i);
        if(balance == 0) {
            dprintf(logfd, "# ");
        }

        dprintf(logfd, "%s", accountTable[i].bankID);

        for(int j = 0; j < accountTable[i].nTransactions; ++j) {
            if(accountTable[i].history[j].type == TRANSACTION_DEPOSIT) {
                dprintf(logfd, " D %d", accountTable[i].history[j].amount);
            }
            if(accountTable[i].history[j].type == TRANSACTION_WITHDRAW) {
                dprintf(logfd, " W %d", accountTable[i].history[j].amount);
            }
        }

        dprintf(logfd, " %d\n", balance);
    }

    dprintf(logfd, "# end of log.\n");
    
    return 0;
}

void handleTellers() {
    int tid;
    for(tid = 0; tid < nTellers; ++tid) {
        if(!tellerTable[tid].done) continue;

        int status;

        response r;
        while((status = read(tellerTable[tid].pipe, &r, sizeof(r))) == -1 && errno == EINTR);

        if(status == -1) {
            fprintf(stderr, "ERROR: read teller pipe (%s)\n", strerror(errno));
        }
        else if(status < sizeof(r)) {
            fprintf(stderr, "ERROR: read teller pipe (got %d bytes instead of %d)\n", status, (int)sizeof(r));
        }
        else {
            while((status = close(tellerTable[tid].pipe)) == -1 && errno == EINTR);
            if(status == -1) {
                fprintf(stderr, "ERROR: close teller pipe (%s)\n", strerror(errno));
            }

            switch(r.type) {
                case RESPONSE_SUCCESS:
                    if(tellerTable[tid].type == REQUEST_DEPOSIT) {
                        printf("Client%02d deposited %d credits... updating log...\n", tellerTable[tid].clientID + 1, r.arg);
                        transaction t;
                        t.amount = r.arg;
                        t.type = TRANSACTION_DEPOSIT;
                        addTransaction(clientTable[tellerTable[tid].clientID].accountID, &t);
                    }
                    else {
                        printf("Client%02d withdraws %d credits... updating log...\n", tellerTable[tid].clientID + 1, r.arg);
                        transaction t;
                        t.amount = r.arg;
                        t.type = TRANSACTION_WITHDRAW;
                        addTransaction(clientTable[tellerTable[tid].clientID].accountID, &t);
                    }
                    updateLog();
                    break;
                case RESPONSE_NO_PERMISSION:
                    if(tellerTable[tid].type == REQUEST_DEPOSIT) {
                        printf("Client%02d deposited %d credits... operation not permitted.\n", tellerTable[tid].clientID + 1, r.arg);
                    }
                    else {
                        printf("Client%02d withdraws %d credits... operation not permitted.\n", tellerTable[tid].clientID + 1, r.arg);
                    }
                    break;
                case RESPONSE_INTERNAL_ERROR:
                    printf("An error occurred while handling Client%02d's request.\n", tellerTable[tid].clientID + 1);
                    break;
                case RESPONSE_INVALID_REQUEST:
                    printf("Invalid request by Client%02d.\n", tellerTable[tid].clientID + 1);
                    break;
                case RESPONSE_TERMINATION:
                    printf("Client%02d's operation is cancelled.\n", tellerTable[tid].clientID + 1);
                    break;
            }
        }

        for(int i = tid; i < nTellers; ++i) {
            memcpy(&tellerTable[i], &tellerTable[i + 1], sizeof(tellerTable[i]));
        }
        tid--;
        nTellers--;
    }
}

int main(int argc, const char ** argv) {
    if(argc != 3) {
        printf("Usage: BankServer \"BankName\" \"FIFOName\"\n");
        return 0;
    }

    rootpid = getpid();

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = &signalHandler;
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);
    sigaction(SIGCHLD, &sa, NULL);

    bankName = argv[1];
    fifoName = argv[2];

    if(strlen(bankName) > NAME_MAX - 8) {
        printf("WARNING: Bank name too long. Might cause conflicts with other banks with similiar names.\n");
    }

    printf("%s is active...\n", bankName);

    char logPath[NAME_MAX + 1];

    strncpy(logPath, bankName, NAME_MAX - 8);
    strcat(logPath, ".bankLog");

    logfd = open(logPath, O_RDWR);

    if(logfd == -1 && errno == ENOENT) {
        printf("No previous logs... Creating bank database\n");
        logfd = open(logPath, O_RDWR | O_CREAT, 0644);
    }

    if(logfd == -1) {
        perror("ERROR: opening database");
        return -1;
    }

    int eofReached = 0;
    int line = 0;
    int loadError = 0;
    while(!eofReached && !loadError) {
        line++;

        char * nextToken;
        int status;
        account a;
        a.history = NULL;
        a.nTransactions = 0;
        a.historySize = 0;

        status = nextWordInLine(logfd, &nextToken);

        if(status == 0) {
            eofReached = 1;
            break;
        }
        else if(status == -1) {
            fprintf(stderr, "ERROR: read log (%s)\n", strerror(errno));
            loadError = 1;
            break;
        }

        if(nextToken == NULL) continue;

        if(strlen(nextToken) > sizeof(a.bankID)) {
            fprintf(stderr, "ERROR: bank id too long in line %d\n", line);
            free(nextToken);
            loadError = 1;
            break;
        }

        strcpy(a.bankID, nextToken);

        free(nextToken);

        int accountID = addAccount(&a);

        if(accountID == -1) {
            fprintf(stderr, "ERROR: memory (%s)\n", strerror(errno));
            loadError = 1;
            break;
        }

        int valid = 0;

        do {
            status = nextWordInLine(logfd, &nextToken);
            if(status == -1) {
                fprintf(stderr, "ERROR: read log (%s)\n", strerror(errno));
                loadError = 1;
                break;
            }
            if(status == 0) {
                eofReached = 1;
                break;
            }

            if(nextToken == NULL) break;

            if(strcmp("D", nextToken) == 0) {
                transaction t;
                t.type = TRANSACTION_DEPOSIT;

                free(nextToken);

                status = nextWordInLine(logfd, &nextToken);
                if(status == -1) {
                    fprintf(stderr, "ERROR: read log (%s)\n", strerror(errno));
                    loadError = 1;
                    break;
                }
                if(status == 0) {
                    eofReached = 1;
                    break;
                }
                if(nextToken == NULL) break;

                if(isInteger(nextToken)) {
                    t.amount = atoi(nextToken);
                    if(t.amount <= 0) {
                        fprintf(stderr, "ERROR: negative numbers are not accepted in log\n");
                        loadError = 1;
                    }
                    else {
                        addTransaction(accountID, &t);
                    }
                }
                else {
                    fprintf(stderr, "ERROR: %s is not a number (log)\n", nextToken);
                    loadError = 1;
                }
            }
            else if(strcmp("W", nextToken) == 0) {
                transaction t;
                t.type = TRANSACTION_WITHDRAW;

                free(nextToken);

                status = nextWordInLine(logfd, &nextToken);
                if(status == -1) {
                    fprintf(stderr, "ERROR: read log (%s)\n", strerror(errno));
                    loadError = 1;
                    break;
                }
                if(status == 0) {
                    eofReached = 1;
                    break;
                }

                if(nextToken == NULL) break;

                if(valid) {
                    valid = 0;
                    loadError = 1;
                    fprintf(stderr, "ERROR: too many tokens\n");
                }

                if(isInteger(nextToken)) {
                    t.amount = atoi(nextToken);
                    if(t.amount <= 0) {
                        fprintf(stderr, "ERROR: negative numbers are not accepted in log\n");
                        loadError = 1;
                    }
                    else {
                        addTransaction(accountID, &t);
                    }
                }
                else {
                    fprintf(stderr, "ERROR: %s is not a number (log)\n", nextToken);
                    loadError = 1;
                }
            }
            else if(isInteger(nextToken)) {
                int balance = atoi(nextToken);
                if(getAccountBalance(accountID) != balance) {
                    fprintf(stderr, "ERROR: checksum mismatch while loading log\n");
                    loadError = 1;
                }
                else {
                    valid = 1;
                }
            }
            else {
                fprintf(stderr, "ERROR: invalid token: %s\n", nextToken);
                loadError = 1;
            }
            free(nextToken);
        }
        while(!loadError && !eofReached);

        if(!valid && !loadError) {
            fprintf(stderr, "ERROR: not enough tokens\n");
            loadError = 1;
        }
        else {
            tellerAccountSync * syncAccountTable = (void*)&syncTable[1];
            syncAccountTable[accountID].balance = getAccountBalance(accountID);
        }
    }

    if(loadError) {
        printf("An error occurred while loading database. Exiting...\n");
        releaseServerResources();
        return -1;
    }
    
    for(int i = 0; i < nAccounts; ++i) {
        printf("Account %d: %s", i, accountTable[i].bankID);
        for(int j = 0; j < accountTable[i].nTransactions; ++j) {
            if(accountTable[i].history[j].type == TRANSACTION_DEPOSIT) {
                printf(" D %d", accountTable[i].history[j].amount);
            }
            if(accountTable[i].history[j].type == TRANSACTION_WITHDRAW) {
                printf(" W %d", accountTable[i].history[j].amount);
            }
        }
        printf(" %d\n", getAccountBalance(i));
    }

    if(mkfifo(fifoName, 0622) == -1) {
        perror("ERROR: mkfifo");
        releaseServerResources();
        return -1;
    }

    fifofd = open(fifoName, O_RDWR);
    if(fifofd == -1) {
        perror("ERROR: open fifo");
        releaseServerResources();
        return -1;
    }

    while(!terminate) {
        int nClients = 0;
        int nameLength = 0;
        int status = 0;

        char buf[sizeof(accountTable[0].bankID)] = {};

        handleTellers();

        printf("Waiting for clients @%s...\n", fifoName);

        while((status = read(fifofd, &nClients, sizeof(nClients))) == -1 && errno == EINTR) {
            handleTellers();
            if(terminate) {
                break;
            }
        }

        if(status == -1) {
            if(errno != EINTR) {
                fprintf(stderr, "ERROR: read fifo (%s)\n", strerror(errno));
            }
            break;
        }

        if(fifoReadN(fifofd, &nameLength, sizeof(nameLength)) == -1) {
            perror("ERROR: read");
            break;
        }

        while(nameLength > 0 && status != -1) {
            status = fifoReadCounted(fifofd, buf, sizeof(buf), &nameLength);
            if(status != -1) {
                printf(" - Received %d clients from  %*s...\n", nClients, status, buf);
            }
        }
        if(status == -1) {
            perror("ERROR: read");
            break;
        }

        request ** requests = malloc(sizeof(request*) * nClients);
        if(requests == NULL) {
            perror("ERROR: malloc");
            break;
        }
        int nRequests = 0;

        int i;
        for(i = 0; i < nClients; ++i) {
            request * r = malloc(sizeof(request));
            if(r == NULL) {
                perror("ERROR: malloc");
                break;
            }

            int typeStatus, bankIDStatus, readFIFOStatus, writeFIFOStatus;

            if((typeStatus = fifoReadN(fifofd, &r->type, sizeof(r->type)) == -1) ||
                    (bankIDStatus = fifoReadString(fifofd, buf, sizeof(buf)) == -1) ||
                    (readFIFOStatus = fifoReadString(fifofd, r->readFIFOPath, sizeof(r->readFIFOPath)) == -1) ||
                    (writeFIFOStatus = fifoReadString(fifofd, r->writeFIFOPath, sizeof(r->writeFIFOPath)) == -1)) {
                perror("ERROR: read");
                free(r);
                break;
            }

            if(r->type != REQUEST_DEPOSIT && r->type != REQUEST_WITHDRAW) {
                fprintf(stderr, "ERROR: Invalid request type (%d), skipping client...\n", r->type);
                free(r);
                continue;
            }

            if(bankIDStatus >= sizeof(buf)) {
                fprintf(stderr, "ERROR: Bank id too long, skipping client...\n");
                free(r);
                continue;
            }

            if(readFIFOStatus >= sizeof(r->readFIFOPath)) {
                fprintf(stderr, "ERROR: Read fifo path too long, skipping client...\n");
                free(r);
                continue;
            }

            if(writeFIFOStatus >= sizeof(r->writeFIFOPath)) {
                fprintf(stderr, "ERROR: Write fifo path too long, skipping client...\n");
                free(r);
                continue;
            }

            requests[nRequests++] = r;

            r->clientID = clientByBankID(buf);
            if(r->clientID == -1) {
                perror("ERROR: client shared memory");
                break;
            }

            if(r->type == REQUEST_DEPOSIT && clientTable[r->clientID].accountID >= 0) {
                while((status = sem_wait(&syncTable->mutex)) == -1 && errno == EINTR);
                if(status == -1) {
                    perror("ERROR: sem_wait");
                    break;
                }

                tellerAccountSync * syncAccountTable = (void*)&syncTable[1];
                syncAccountTable[clientTable[r->clientID].accountID].depositsPending++;

                if(sem_post(&syncTable->mutex) == -1) {
                    perror("ERROR: sem_post");
                    break;
                }
            }
        }

        if(i != nClients) {
            for(int i = 0; i < nRequests; ++i) {
                free(requests[i]);
            }
            free(requests);
            break;
        }
        else {
            for(i = 0; i < nRequests; ++i) {
                request * r = requests[i];

                int tellerPipe[2];
                if(pipe(tellerPipe) == -1) {
                    perror("ERROR: pipe");
                    break;
                }

                r->pipe = tellerPipe[1];

                tellerData t;
                t.pipe = tellerPipe[0];
                t.done = 0;
                t.clientID = r->clientID;
                t.type = r->type;

                requestsToFree = requests;
                doNotFreeThisRequest = r;
                nRequestsToFree = nRequests;
                alsoCloseThisFD = tellerPipe[0];
                t.pid = Teller(r->type == REQUEST_DEPOSIT ? deposit : withdraw, r);
                requestsToFree = NULL;
                doNotFreeThisRequest = NULL;
                nRequestsToFree = 0;
                alsoCloseThisFD = 0;

                if(addTeller(&t) == -1) {
                    perror("ERROR: memory");
                    while((status = close(tellerPipe[0])) == -1 && errno == EINTR);
                    if(status == -1) {
                        perror("ERROR: close");
                    }
                    while((status = close(tellerPipe[1])) == -1 && errno == EINTR);
                    if(status == -1) {
                        perror("ERROR: close");
                    }
                    requestsToFree = NULL;
                    doNotFreeThisRequest = NULL;
                    nRequestsToFree = 0;
                    break;
                }

                while((status = close(tellerPipe[1])) == -1 && errno == EINTR);
                if(status == -1) {
                    perror("ERROR: close");
                }
            }

            for(i = 0; i < nRequests; ++i) {
                free(requests[i]);
            }

            free(requests);
        }
    }

    printf("Signal received closing active Tellers\n");

    for(int i = 0; i < nTellers; ++i) {
        kill(tellerTable[i].pid, SIGTERM);
    }

    while(nTellers > 0) {
        handleTellers();
        sleep(1);
    }

    releaseServerResources();

    return 0;
}
