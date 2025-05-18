#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

int isInteger(const char * str) {
    int i = 0;
    if(i == '-') i++;
    while(str[i] != '\0') {
        if(str[i] < '0' || str[i] > '9') return 0;
        i++;
    }
    return 1;
}

int nextWordInLine(int fd, char ** out) {
    char next;
    int status;
    int offset;
    int eofReached = 0;

    *out = NULL;

    // skip white space
    do {
        status = read(fd, &next, sizeof(next));
        if(status <= 0) return 0;
    }
    while(next == ' ' || next == '\t');

    switch(next) {
        case '\n':
            break;

        case '#':
            do {
                status = read(fd, &next, sizeof(next));
                if(status <= 0) return status;
            }
            while(next != '\n');
            break;

        default:
            offset = 1;

            do {
                status = read(fd, &next, sizeof(next));
                if(status < 0) return status;
                if(status == 0) eofReached = 1;
                offset += status;
            }
            while(!eofReached && next != ' ' && next != '\n' && next != '\t' && next != '#');

            *out = malloc(sizeof(**out) * offset);

            lseek(fd, -offset, SEEK_CUR);

            status = read(fd, *out, eofReached ? offset : offset - 1);
            if(status < 0) {
                free(*out);
                *out = NULL;
                return status;
            }
            (*out)[offset - 1] = 0;

            break;
    }

    if(next == '#') {
        return 1;
    }

    if(next == '\n') {
        return 1;
    }

    return !eofReached;
}

int fifoReadCounted(int fd, void * buf, int buflen, int * counter) {
    int n = *counter > buflen ? buflen : *counter;
    int status;
    while((status = read(fd, buf, n)) == -1 && errno == EINTR);
    if(status > 0) {
        *counter -= status;
    }
    return status;
}

int fifoReadN(int fd, void * buf, int n) {
    int status;
    int counter = n;
    while(counter > 0 && (status = fifoReadCounted(fd, buf + n - counter, counter, &counter)) != -1);
    return status;
}

int fifoReadString(int fd, char * buf, int buflen) {
    int length;
    int status;
    status = fifoReadN(fd, &length, sizeof(length));
    if(status == -1) {
        return -1;
    }

    if(length >= buflen) {
        status = fifoReadN(fd, buf, buflen - 1);
        if(status != buflen - 1) {
            return -1;
        }
        buf[buflen - 1] = '\0';
        char discard[32];
        int remain = length - status;
        while(remain > 0 && (status = fifoReadCounted(fd, discard, sizeof(discard), &remain)));
        if(status < 0) {
            return status;
        }
        return length;
    }

    status = fifoReadN(fd, buf, length);
    buf[length] = '\0';

    return status;
}
