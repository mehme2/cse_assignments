#include <bits/types/sigset_t.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <signal.h>

#include "buffer.h"

typedef struct {
    int workerNumber, linesChecked, matchesFound;
} workerSummary;

buffer * buf = NULL;

int bufferExclusiveAccessWorker = 0;
pthread_mutex_t bufferMutex;
pthread_cond_t bufferFilled;
pthread_cond_t bufferEmpty;

pthread_barrier_t barrier;

int terminate = 0;

const char * searchTerm = NULL;

int isUnsignedInteger(const char * str) {
    int res = 1;
    while(res && str[0] != '\0') {
        if(str[0] < '0' || str[0] > '9') {
            res = 0;
        }
        str++;
    }
    return res;
}

void * worker(void * arg) {
    workerSummary * sum = arg;
    size_t lineBufferSize = 4;
    char * lineBuffer = malloc(lineBufferSize);

    int searchTermLength = strlen(searchTerm);

    int eofReached = 0;

    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGINT);
    pthread_sigmask(SIG_BLOCK, &ss, NULL);

    while(!eofReached) {
        pthread_mutex_lock(&bufferMutex);

        while(buf->usage == 0 || bufferExclusiveAccessWorker) pthread_cond_wait(&bufferFilled, &bufferMutex);

        int lineLength = 0;

        while(1) {
            // buffer is empty while reading a line
            while(buf->usage == 0) {
                pthread_cond_signal(&bufferEmpty);

                // make sure other workers cannot access the buffer
                bufferExclusiveAccessWorker = 1;
                pthread_cond_wait(&bufferFilled, &bufferMutex);
                bufferExclusiveAccessWorker = 0;

                // if other workers acquired the mutex while exclusive access was set,
                // they wont be able to continue unless this broadcast happens
                pthread_cond_broadcast(&bufferFilled);
            }

            char next = bufferPeek(buf);

            if(next == '\0') {
                eofReached = 1;
                break;
            }

            bufferPop(buf);

            if(next == '\n') break;

            // expand line buffer if not enough space
            if(lineBufferSize == lineLength) {
                lineBufferSize *= 2;
                lineBuffer = realloc(lineBuffer, lineBufferSize);
            }

            lineBuffer[lineLength] = next;

            lineLength++;
        }

        pthread_mutex_unlock(&bufferMutex);

        if(buf->usage == 0) pthread_cond_signal(&bufferEmpty);

        if(eofReached && lineLength == 0) break;

        sum->linesChecked++;

        // search for occurences in the line
        for(int i = 0; i < lineLength - searchTermLength + 1; ++i) {
            int j;
            for(j = 0; j < searchTermLength && searchTerm[j] == lineBuffer[i + j]; ++j);
            if(j == searchTermLength) {
                sum->matchesFound++;
                i += j - 1;
            }
        }
    }

    free(lineBuffer);

    printf("WORKER %d SUMMARY: %d lines checked, %d mathces found\n", sum->workerNumber, sum->linesChecked, sum->matchesFound);

    pthread_barrier_wait(&barrier);

    return NULL;
}

void signalHandler(int sig) {
    terminate = 1;
}

int main(int argc, const char ** argv) {
    if(argc != 5) {
        printf("Usage: ./LogAnalyzer <buffer_size> <num_workers> <log_file> <search_term>\n");
        return 0;
    }

    size_t bufferSize;
    int numWorkers, logfd;
    searchTerm = argv[4];

    if(!isUnsignedInteger(argv[1]) || (bufferSize = atol(argv[1])) <= 0) {
        printf("Invalid buffer size.\n");
        return 0;
    }

    if(!isUnsignedInteger(argv[2]) || (numWorkers = atol(argv[2])) <= 0) {
        printf("Invalid number of workers.\n");
        return 0;
    }

    if(searchTerm[0] == '\0') {
        printf("Search term cannot be empty.\n");
        return 0;
    }

    if((logfd = open(argv[3], O_RDONLY)) < 0) {
        printf("File %s could not be opened: %s\n", argv[3], strerror(errno));
        return 0;
    }

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = signalHandler;
    sigaction(SIGINT, &sa, NULL);

    buf = bufferInit(bufferSize);

    pthread_barrier_init(&barrier, NULL, numWorkers + 1);

    pthread_mutex_init(&bufferMutex, NULL);

    pthread_cond_init(&bufferFilled, NULL);
    pthread_cond_init(&bufferEmpty, NULL);

    pthread_t * threads = malloc(sizeof(pthread_t) * numWorkers);
    workerSummary * workerSummaries = malloc(sizeof(workerSummary) * numWorkers);

    for(int i = 0; i < numWorkers; ++i) {
        workerSummaries[i].workerNumber = i;
        workerSummaries[i].linesChecked = 0;
        workerSummaries[i].matchesFound = 0;
        pthread_create(&threads[i], NULL, worker, &workerSummaries[i]);
    }

    int eofReached = 0;

    pthread_mutex_lock(&bufferMutex);

    // place text in the buffer
    while(!eofReached) {
        while(buf->usage == bufferSize && !terminate) pthread_cond_wait(&bufferEmpty, &bufferMutex);

        int status = 0;

        if(!terminate) {
            status = bufferFill(buf, logfd);
        }

        if(status == 0) {
            eofReached = 1;
            // place the eof marker in the buffer
            bufferPush(buf, '\0');
        }

        pthread_cond_broadcast(&bufferFilled);
    }

    pthread_mutex_unlock(&bufferMutex);

    int linesChecked = 0;
    int matchesFound = 0;

    pthread_barrier_wait(&barrier);

    for(int i = 0; i < numWorkers; ++i) {
        pthread_join(threads[i], NULL);
        linesChecked += workerSummaries[i].linesChecked;
        matchesFound += workerSummaries[i].matchesFound;
    }

    printf("OVERALL SUMMARY: %d lines checked, %d matches found\n", linesChecked, matchesFound);

    free(workerSummaries);
    free(threads);

    pthread_cond_destroy(&bufferEmpty);
    pthread_cond_destroy(&bufferFilled);

    pthread_mutex_destroy(&bufferMutex);

    pthread_barrier_destroy(&barrier);

    bufferDestroy(buf);
    
    close(logfd);

    return 0;
}
