#include <stdlib.h>
#include <stdio.h>

#include "nstack.h"

nstack * push(nstack * stack, node * val) {
	nstack * new;
	for(new = stack; new && new->val != val; new = new->next);
	/* If val is not NULL and does not already exist in stack. */
	if(val && !new) {
		new = malloc(sizeof(nstack));
		new->val = val;
		new->next = stack;
	}
	else {
		new = stack;
	}
	return new;
}

nstack * pop(nstack * stack, node * val) {
	nstack ** cur;
	nstack * rm;
	for(cur = &stack; *cur && (*cur)->val != val; cur = &(*cur)->next);
	if(*cur) {
		rm = *cur;
		*cur = (*cur)->next;
		free(rm);
	}
	return stack;
}

node * searchStackByName(nstack * stack, const char * name) {
	node * ret;
	for(ret = NULL; stack && !(ret = searchNodeByName(stack->val, name)); stack = stack->next);
	return ret;
}

nstack * searchStack(nstack * stack, node * dest) {
	for(; stack && searchNode(stack->val, dest) != dest; stack = stack->next);
	return stack;
}

nstack * popAndFree(nstack * stack, node * val) {
	stack = pop(stack, val);
	if(val->mother && !searchStack(stack, val->mother) && !searchNode(val->father, val->mother)) {
		stack = push(stack, val->mother);
	}
	if(val->father && !searchStack(stack, val->father)) {
		stack = push(stack, val->father);
	}
	free(val);
	return stack;
}

nstack * popParents(nstack * stack, node * val) {
	stack = pop(stack, val->mother);
	stack = pop(stack, val->father);
	return stack;
}

nstack * pushAndPopParents(nstack * stack, node * val) {
	stack = popParents(stack, val);
	return push(stack, val);
}

nstack * popAndPushParents(nstack * stack, node * val) {
	stack = pop(stack, val);
	if(val->mother && !searchStack(stack, val->mother) && !searchNode(val->father, val->mother)) {
		stack = push(stack, val->mother);
	}
	if(val->father && !searchStack(stack, val->father)) {
		stack = push(stack, val->father);
	}
	return stack;
}

nstack * clone(nstack * stack) {
	nstack * ret;
	if(stack) {
		ret = malloc(sizeof(nstack));
		*ret = *stack;
		ret->next = clone(stack->next);
	}
	else {
		ret = NULL;
	}
	return ret;
}

nstack * load(const char * filename) {
	nstack * leafNodes = NULL;
	char name[sizeof(leafNodes->val->name)];
	char fatherName[sizeof(leafNodes->val->name)];
	char motherName[sizeof(leafNodes->val->name)];
	node * father;
	node * mother;
	node * n;
	int age;
	FILE * fp = fopen(filename, "r");
	if(fp) {
		fscanf(fp, "%[^,],%d", name, &age);
		leafNodes = push(leafNodes, n = addNode(name, age, NULL, NULL));
		while(fscanf(fp, " %[^,],%d,%[^,],%[^\n]", name, &age, fatherName, motherName) == 4) {
			if(!(father = searchStackByName(leafNodes, fatherName))) {
				father = addNode(fatherName, -1, NULL, NULL);
			}
			if(!(mother = searchStackByName(leafNodes, motherName))) {
				mother = addNode(motherName, -1, NULL, NULL);
			}
			if((n = searchStackByName(leafNodes, name))) {
				if(isUninitalized(n)) {
					n->age = age;
					n->mother = mother;
					n->father = father;
					leafNodes = popParents(leafNodes, n);
				}
				else {
					if(isUninitalized(father)) {
						free(father);
					}
					if(isUninitalized(mother)) {
						free(mother);
					}
				}
			}
			else {
				leafNodes = pushAndPopParents(leafNodes, addNode(name, age, mother, father));
			}
		}
		fclose(fp);
	}
	else {
		printf("\nAn error occurred while opening file %s.\n", filename);
	}
	return leafNodes;
}

void save(const char * filename, nstack * stack) {
	nstack * temp;
	node * cur;
	node * root;
	FILE * fp = fopen(filename, "w");
	if(fp) {
		temp = clone(stack);
		for(nstack * i = stack; (root = getRoot(i->val)) == NULL; i = i->next);
		fprintf(fp, "%s,%d", root->name, root->age);
		for(cur = temp->val; cur != root; cur = temp->val) {
			if(!isUninitalized(cur)) {
				fprintf(fp, "\n%s,%d,%s,%s", cur->name, cur->age, cur->father->name, cur->mother->name);
			}
			temp = popAndPushParents(temp, cur);
		}
		while((temp = pop(temp, temp->val)));
		fclose(fp);
	}
	else {
		printf("\nAn error occurred while writing to file %s.\n", filename);
	}
}

nstack * printStack(nstack * stack) {
	nstack * cur = stack;
	if(cur) {
		printf("%s", cur->val->name);
		for(cur = cur->next; cur; cur = cur->next) {
			printf(", %s", cur->val->name);
		}
	}
	else {
		printf("Unknown");
	}
	return stack;
}

nstack * getChildren(nstack * stack, node * start, node * parent) {
	if(start && parent) {
		if(start->father == parent) {
			stack = push(stack, start);
		}
		else {
			stack = getChildren(stack, start->father, parent);
		}
		if(start->mother == parent) {
			stack = push(stack, start);
		}
		else {
			stack = getChildren(stack, start->mother, parent);
		}
	}
	return stack;
}

nstack * getChildrenInStack(nstack * stack, nstack * search, node * parent) {
	for(nstack * cur = search; cur; cur = cur->next) {
		stack = getChildren(stack, cur->val, parent);
	}
	return stack;
}

nstack * getSiblings(nstack * stack, nstack * search, node * n) {
	if(n) {
		stack = getChildrenInStack(stack, search, n->father);
		stack = getChildrenInStack(stack, search, n->mother);
		stack = pop(stack, n);
	}
	return stack;
}

nstack * getCousins(nstack * stack, nstack * search, node * n) {
	if(n) {
		nstack * uncleAndAunts = getSiblings(getSiblings(NULL, search, n->father), search, n->mother);
		while(uncleAndAunts) {
			stack = getChildrenInStack(stack, search, uncleAndAunts->val);
			uncleAndAunts = pop(uncleAndAunts, uncleAndAunts->val);
		}
	}
	return stack;
}

node * getSpouse(nstack * search, node * n) {
	node * ret;
	if(n) {
		nstack * children = getChildrenInStack(NULL, search, n);
		if(children) {
			if(children->val->father == n) {
				ret = children->val->mother;
			}
			else {
				ret = children->val->father;
			}
		}
		else {
			ret = NULL;
		}
	}
	else {
		ret = NULL;
	}
	return ret;
}
