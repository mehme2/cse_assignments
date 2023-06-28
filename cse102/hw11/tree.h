#ifndef _TREE_H
#define _TREE_H

typedef struct node {
	char name[64];
	int age;
	struct node * mother, * father;
} node;

node * addNode(const char * name, int age, node * mother, node * father);

/* Tries to find and return a node with the matching name that is an ancestor of the start node. */
node * searchNodeByName(node * start, const char * name);

/* Returns the destination node if it is an ancestor of the start node. */
node * searchNode(node * start, node * dest);

/* Returns 1 if node is uninitalized. */
/* Uninitalized nodes are nodes with unknown parents and ages. */
int isUninitalized(node * n);

/* Tries to get the root node by recursing starting from the start node. */
node * getRoot(node * start);

#endif
