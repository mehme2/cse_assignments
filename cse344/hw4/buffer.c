#include <unistd.h>

#include "buffer.h"

buffer * bufferInit(size_t size) {
    buffer * b = malloc(sizeof(*b) + size);
    b->index = 0;
    b->usage = 0;
    b->size = size;
    b->data = (void*)&b[1];
    return b;
}

char bufferPeek(const buffer * b) {
    return b->data[b->index];
}

void bufferPop(buffer * b) {
    b->usage--;
    b->index = (b->index + 1) % b->size;
}

void bufferPush(buffer * b, char c) {
    b->data[(b->index + b->usage) % b->size] = c;
    b->usage++;
}

int bufferFill(buffer * b, int fd) {
    int start = (b->index + b->usage) % b->size;
    int length = b->size - b->usage;
    if(start + length > b->size) length = b->size - start;
    int status = read(fd, b->data + start, length);
    b->usage += status;
    if(b->usage != b->size && status != 0) status += bufferFill(b, fd);
    return status;
}

void bufferDestroy(buffer * b) {
    free(b);
}
