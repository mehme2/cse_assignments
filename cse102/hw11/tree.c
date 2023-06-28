#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "tree.h"

node * addNode(const char * name, int age, node * mother, node * father) {
	node * new = malloc(sizeof(node));
	strncpy(new->name, name, sizeof(new->name));
	new->name[sizeof(new->name) - 1] = '\0';
	new->age = age;
	new->mother = mother;
	new->father = father;
	return new;
}

node * searchNodeByName(node * start, const char * name) {
	node * ret = NULL;
	if(start) {
		if(!strcmp(start->name, name)) {
			ret = start;
		}
		else {
			(ret = searchNodeByName(start->father, name)) || (ret = searchNodeByName(start->mother, name));
		}
	}
	return ret;
}

node * searchNode(node * start, node * dest) {
	node * ret = NULL;
	if(start) {
		if(start == dest) {
			ret = start;
		}
		else {
			(ret = searchNode(start->father, dest)) || (ret = searchNode(start->mother, dest));
		}
	}
	return ret;
}

int isUninitalized(node * n) {
	return n->age == -1 && !n->mother && !n->father;
}

node * getRoot(node * start) {
	node * ret;
	return start ? (ret = getRoot(start->mother)) ? ret : (ret = getRoot(start->father)) ? ret : !start->mother && !start->father && start->age != -1 ? start : NULL : NULL;
}
