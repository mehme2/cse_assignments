#ifndef _UTIL_H
#define _UTIL_H

typedef enum {
    REQUEST_DEPOSIT,
    REQUEST_WITHDRAW
} requestType;

int isInteger(const char * str);

int nextWordInLine(int fd, char ** out);

int fifoReadCounted(int fd, void * buf, int buflen, int * counter);

int fifoReadN(int fd, void * buf, int n);

int fifoReadString(int fd, char * buf, int buflen);

#endif // _UTIL_H
