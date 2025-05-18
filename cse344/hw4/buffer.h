#ifndef _BUFFER_H
#define _BUFFER_H

#include <stdlib.h>

typedef struct {
    char * data;
    size_t size;
    size_t index;
    size_t usage;
} buffer;

buffer * bufferInit(size_t size);

char bufferPeek(const buffer * b);

void bufferPop(buffer * b);
void bufferPush(buffer * b, char c);
int bufferFill(buffer * b, int fd);
void bufferDestroy(buffer * b);

#endif // _BUFFER_H
