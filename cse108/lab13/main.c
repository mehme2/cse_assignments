#include <stdio.h>
#include <stdlib.h>

#define FILENAME "input.txt"

typedef struct node {
	int value;
	struct node * left;
	struct node * right;
} node;

typedef struct bst {
	node * root;
} bst;

void addNode(bst * b, int value) {
	node * new = malloc(sizeof(node));
	node ** cur = &b->root;
	new->value = value;
	new->left = NULL;
	new->right = NULL;
	while(*cur != NULL) {
		if(value > (*cur)->value) {
			cur = &(*cur)->right;
		}
		else if (value < (*cur)->value) {
			cur = &(*cur)->left;
		}
		else {
			free(new);
			new = NULL;
		}
	}
	(*cur) = new;
}

bst * generateBST(const char * filename) {
	int val;
	FILE * fp = fopen(filename, "r");
	bst * new = malloc(sizeof(bst));
	new->root = NULL;
	if(fp) {
		while(fscanf(fp, "%d", &val) != EOF) {
			addNode(new, val);
		}
		fclose(fp);
	}
	else {
		printf("\nCouldn't open file %s.\n", filename);
	}
	return new;
}

void removeNode(bst * b, int value) {
	node * left, * right;
	node ** comp;
	node ** cur = &b->root;
	while((*cur) != NULL && (*cur)->value != value) {
		if(value < (*cur)->value) {
			cur = &(*cur)->left;
		}
		else {
			cur = &(*cur)->right;
		}
	}
	if(*cur) {
		left = (*cur)->left;
		right = (*cur)->right;
		free(*cur);
		if(left) {
			*cur = left;
			for(comp = &left->right; *comp != NULL; comp = &(*comp)->right);
			*comp = right;
		}
		else if(right) {
			*cur = right;
			for(comp = &right->left; *comp != NULL; comp = &(*comp)->left);
			*comp = left;
		}
		else {
			*cur = NULL;
		}
	}
	else {
		printf("\nNo node found with the value of %d.\n", value);
	}
}

node * searchNode(bst * b, int value) {
	node * cur = b->root;
	while(cur && cur->value != value) {
		if(value > cur->value) {
			cur = cur->right;
		}
		else {
			cur = cur->left;
		}
	}
	return cur;
}

int countNodes(bst * b) {
	int count = 0;
	bst sub;
	if(b->root) {
		count = 1;
		if(b->root->left) {
			sub.root = b->root->left;
			count += countNodes(&sub);
		}
		if(b->root->right) {
			sub.root = b->root->right;
			count += countNodes(&sub);
		}
	}
	return count;
}

int getMaxDepth(bst * b) {
	int ld, rd;
	bst sub;
	ld = rd = 0;
	if(b->root) {
		sub.root = b->root->left;
		ld = 1 + getMaxDepth(&sub);
		sub.root = b->root->right;
		rd = 1 + getMaxDepth(&sub);
	}
	return rd > ld ? rd : ld;
}

int countLeafNodes(bst * b) {
	int leafCount;
	bst sub;
	if(b->root) {
		if(b->root->left || b->root->right) {
			leafCount = 0;
			sub.root = b->root->left;
			leafCount += countLeafNodes(&sub);
			sub.root = b->root->right;
			leafCount += countLeafNodes(&sub);
		}
		else {
			leafCount = 1;
		}
	}
	else {
		leafCount = 0;
	}
	return leafCount;
}

void printTree(bst * b) {
	bst sub;
	if(b->root) {
		sub.root = b->root->left;
		printTree(&sub);
		printf("%d ", b->root->value);
		sub.root = b->root->right;
		printTree(&sub);
	}
}

int main() {
	int val;
	int option;
	bst * b;
	b = generateBST(FILENAME);
	do {
		printf("\nOptions:\n0 - Exit\n1 - Add node\n2 - Remove node\n3 - Search node\n4 - Count nodes\n5 - Get max depth\n6 - Count leaf nodes\n7 - Print tree\n\n> ");
		scanf("%d", &option);
		switch(option) {
			case 1:
				printf("\nEnter new value: ");
				scanf("%d", &val);
				addNode(b, val);
				break;
			case 2:
				printf("\nEnter value: ");
				scanf("%d", &val);
				removeNode(b, val);
				break;
			case 3:
				printf("\nEnter value: ");
				scanf("%d", &val);
				printf("\nA node with the value of %d %s.\n", val, searchNode(b, val) == NULL ? "doesn't exist" : "exists");
				break;
			case 4:
				printf("\nNo of nodes: %d\n", countNodes(b));
				break;
			case 5:
				printf("\nMax depth: %d\n", getMaxDepth(b));
				break;
			case 6:
				printf("\nNo of leaf nodes: %d\n", countLeafNodes(b));
				break;
			case 7:
				printTree(b);
				break;
			case 0:
				break;
			default:
				printf("\nInvalid input.\n");
				break;
		}
	}
	while(option);
	while(b->root) removeNode(b, b->root->value);
}
