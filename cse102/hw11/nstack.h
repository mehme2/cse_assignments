#ifndef _NSTACK_H
#define _NSTACK_H

#include "tree.h"

/* Used to make arrays of pointers to leaf nodes, children of nodes etc. */
typedef struct nstack {
	node * val;
	struct nstack * next;
} nstack;

/* Pushes a stack node that points to the given node to the given stack if does not already in there. */
nstack * push(nstack * stack, node * val);

/* Pops the stack node that is pointing to the given node if exists. */
nstack * pop(nstack * stack, node * val);

/* Searches the entire stack and nodes' ancestors to match and return a node with the given name */
node * searchStackByName(nstack * stack, const char * name);

/* Returns the first stack node that has the given node as ancestor. */
nstack * searchStack(nstack * stack, node * dest);

/* Same as pop but also pushes the parents of the given node to the stack if no other connection to them exists then frees the given node. */
nstack * popAndFree(nstack * stack, node * val);

/* Tries to pop the parents of the given value from stack. */
nstack * popParents(nstack * stack, node * val);

/* Combination of push and popParents functions. */
nstack * pushAndPopParents(nstack * stack, node * val);

/* Same as popAndFree but does not free the given node. */
nstack * popAndPushParents(nstack * stack, node * val);

/* Makes a copy of the given stack but does not copy the values inside so the original and the copy points to the same values. */
nstack * clone(nstack * stack);

/* Loads and returns a stack of leaf nodes from the given filename. */
nstack * load(const char * filename);

/* Saves the given leaf nodes and their ancestors to the given filename. */
void save(const char * filename, nstack * stack);

/* Prints the given stack as Node1, Node2, Node3, ... */
nstack * printStack(nstack * stack);

/* Starts recursing from the start node and pushes the children of the given parent to the given stack. */
nstack * getChildren(nstack * stack, node * start, node * parent);

/* Runs the getChildren function for every member of the stack. */
nstack * getChildrenInStack(nstack * stack, nstack * search, node * parent);

/* Pushes the siblings of the given node to the given stack. */
nstack * getSiblings(nstack * stack, nstack * search, node * n);

/* Pushes the cousins of the given node to the given stack. */
nstack * getCousins(nstack * stack, nstack * search, node * n);

/* Tries to find the spouse of the given node then return it. */
node * getSpouse(nstack * search, node * n);

#endif
