#ifndef _TELLER_H
#define _TELLER_H

#include <linux/limits.h>
#include <sys/wait.h>
#include <semaphore.h>

#include "util.h"

typedef struct {
    sem_t mutex;
} tellerSync;

typedef struct {
    char bankID[32];
    int balance, depositsPending, removed;
} tellerAccountSync;

typedef struct {
    int accountID;
} client;

typedef struct {
    int clientID, pipe;
    requestType type;
    char readFIFOPath[PATH_MAX];
    char writeFIFOPath[PATH_MAX];
} request;

typedef struct {
    int arg;
    enum {
        RESPONSE_SUCCESS,
        RESPONSE_INVALID_REQUEST,
        RESPONSE_INTERNAL_ERROR,
        RESPONSE_TERMINATION,
        RESPONSE_NO_PERMISSION
    } type;
} response;

int deposit(void * arg);

int withdraw(void * arg);

pid_t Teller(int (*func)(void*), void * arg_func);

int waitTeller(pid_t pid, int * status);

#endif // _TELLER_H
