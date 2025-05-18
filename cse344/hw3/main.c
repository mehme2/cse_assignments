#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

#define N_ENGINEERS 3
#define N_SATELLITES 5
#define REQUEST_TIMEOUT 6
#define ENGINEER_TIMEOUT 3
#define REQUEST_HANDLE_TIME 6

typedef struct {
    int n, priority, done;
} request;

request ** requestQueue;
int queueSize = 1;

int availableEngineers = 0;

pthread_mutex_t engineerMutex;

sem_t newRequest;
sem_t requestHandled;

void * satellite(void * arg) {
    // request of this satellite
    request r;
    r.priority = rand() % 10;
    r.n = (long)arg;
    r.done = 0;

    // insert request to the priority queue
    pthread_mutex_lock(&engineerMutex);
    // expand request queue if it is filled
    if(requestQueue[queueSize - 1] != 0) {
        queueSize *= 2;
        typeof(requestQueue) reallocated = realloc(requestQueue, sizeof(request*) * queueSize);
        if(reallocated == 0) {
            pthread_mutex_unlock(&engineerMutex);
            printf("[ERROR] realloc\n");
            return 0;
        }
        else {
            requestQueue = reallocated;
        }
    }

    // select a slot in the queue according to priority
    int slot;
    for(slot = 0; requestQueue[slot] != 0 && requestQueue[slot]->priority <= r.priority; ++slot);

    // shift requests after this one to back one place
    for(int i = queueSize - 1; i > slot; --i) {
        requestQueue[i] = requestQueue[i - 1];
    }

    requestQueue[slot] = &r;

    printf("[SATELLITE] Satellite %d requesting (priority %d)\n", r.n, r.priority);
    pthread_mutex_unlock(&engineerMutex);

    sem_post(&newRequest);

    // set timeout deadline
    // if an engineer does not pickup this request by the deadline cancel the request by removing it from queue
    struct timespec deadline;
    clock_gettime(CLOCK_REALTIME, &deadline);
    deadline.tv_sec += REQUEST_TIMEOUT;

    int wait = 1;
    while(wait) {
        wait = 0;

        int res;
        while((res = sem_timedwait(&requestHandled, &deadline)) == -1 && errno != ETIMEDOUT);

        // check the queue to check whether the request was picked up
        pthread_mutex_lock(&engineerMutex);
        // set slot to -1 meaning it was in the queue
        slot = -1;
        // search the queue and set slot to the index if there is a match
        for(int i = 0; slot == -1 && i < queueSize && requestQueue[i] != 0; ++i) {
            if(requestQueue[i] == &r) {
                slot = i;
            }
        }

        // if the request is still in the queue
        if(slot != -1) {
            // if request timed out
            if(res == -1) {
                // remove the request from the queue
                for(int i = slot; requestQueue[i] != 0 && i < queueSize - 1; ++i) {
                    requestQueue[i] = requestQueue[i + 1];
                }
                requestQueue[queueSize - 1] = 0;
                pthread_mutex_unlock(&engineerMutex);
                // decrement newRequest since we are cancelling it before it getting picked up
                while(sem_wait(&newRequest) == -1);
                printf("[TIMEOUT] Satellite %d timeout %d second.\n", r.n, REQUEST_TIMEOUT);
            }
            else {
                pthread_mutex_unlock(&engineerMutex);
                // the request handled was not for this satellite, continue waiting
                sem_post(&requestHandled);
                wait = 1;
            }
        }
        else {
            pthread_mutex_unlock(&engineerMutex);
            // if the request was picked up after the timeout and before we checked the queue we wait the engineer to finish
            // therefore the timeout is actually REQUEST_TIMEOUT + time to acquire the mutex lock
            if(res == -1) while(sem_wait(&requestHandled) == -1);
            while(!r.done) {
                sem_post(&requestHandled);
                while(sem_wait(&requestHandled) == -1);
            }
        }
    }

    return 0;
}

void * engineer(void * arg) {
    int n = (long)arg;

    while(1) {
        pthread_mutex_lock(&engineerMutex);
        availableEngineers++;
        pthread_mutex_unlock(&engineerMutex);

        // if a new request does not get posted before this deadline then the engineer quits
        struct timespec deadline;
        clock_gettime(CLOCK_REALTIME, &deadline);
        deadline.tv_sec += ENGINEER_TIMEOUT;
        int res;
        while((res = sem_timedwait(&newRequest, &deadline)) == -1 && errno != ETIMEDOUT);
        if(res == -1) {
            printf("[ENGINEER %d] Exiting...\n", n);
            break;
        }

        // pick up the first request from the queue
        pthread_mutex_lock(&engineerMutex);
        request * r = requestQueue[0];

        // if there is no remaining requests due to a timeout we increment the new request counter back up and wait again
        // if we do not increment this the timed out satellite will wait forever trying to decrement newRequest
        if(r == 0) {
            pthread_mutex_unlock(&engineerMutex);
            sem_post(&newRequest);
            continue;
        }

        availableEngineers--;

        // remove the request from the queue
        for(int i = 0; requestQueue[i] != 0 && i < queueSize - 1; ++i) {
            requestQueue[i] = requestQueue[i + 1];
        }
        requestQueue[queueSize - 1] = 0;

        printf("[ENGINEER %d] Handling Satellite %d (priority %d)\n", n, r->n, r->priority);
        pthread_mutex_unlock(&engineerMutex);

        // sleep is here to act as if the request handling takes some time
        sleep(REQUEST_HANDLE_TIME);

        printf("[ENGINEER %d] Finished Satellite %d\n", n, r->n);

        r->done = 1;
        sem_post(&requestHandled);
    }

    return 0;
}

int main() {
    pthread_t engineers[N_ENGINEERS];
    pthread_t satellites[N_SATELLITES];

    requestQueue = malloc(sizeof(request*) * queueSize);
    if(requestQueue == 0) {
        printf("[ERROR] malloc\n");
        return -1;
    }

    pthread_mutex_init(&engineerMutex, 0);

    sem_init(&newRequest, 0, 0);
    sem_init(&requestHandled, 0, 0);

    srand(time(0));

    for(long i = 0; i < N_ENGINEERS; ++i) {
        if(pthread_create(engineers + i, 0, engineer, (void*)i) == -1) {
            printf("[ERROR] pthread_create\n");
        }
    }

    for(long i = 0; i < N_SATELLITES; ++i) {
        if(pthread_create(satellites + i, 0, satellite, (void*)i) == -1) {
            printf("[ERROR] pthread_create\n");
        }
    }

    for(int i = 0; i < N_SATELLITES; ++i) {
        if(pthread_join(satellites[i], 0) == -1) {
            printf("[ERROR] pthread_join\n");
        }
    }

    for(int i = 0; i < N_ENGINEERS; ++i) {
        if(pthread_join(engineers[i], 0) == -1) {
            printf("[ERROR] pthread_join\n");
        }
    }

    free(requestQueue);

    pthread_mutex_destroy(&engineerMutex);

    sem_destroy(&newRequest);
    sem_destroy(&requestHandled);

    return 0;
}
