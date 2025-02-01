#ifndef _GPP_H
#define _GPP_H

#include "y.tab.h"

typedef struct frac frac;

typedef struct data data;

typedef struct node {
    struct node * child;
    struct node * sibling;
    int type;
    data d;
} node;

enum {
    BOOLEAN = 500,
    LIST,
    EXPLIST,
    STMTLIST,
    IDLIST,
    VARIABLE,
    FUNCTION,
    CALL
};

node * newNode(int type, data val);

node * addSibling(node * n, node * s);

node * addChild(node * n, node * c);

void destroyData(data d);

data eval(node * n);

extern int gppstop;

#endif
