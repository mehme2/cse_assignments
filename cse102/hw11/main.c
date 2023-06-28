#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nstack.h"

/* Prompts the user with the given prompt then gets a yes or no answer. */
/* Defaults to def if an invalid or blank answer is given. */
int yesOrNo(const char * prompt, int def) {
	char input[32];
	int ret;
	while(fgetc(stdin) != '\n');
	printf("%s %s? ", prompt, def ? "[Y/n]" : "[y/N]");
	scanf("%*[ ]");
	fgets(input, sizeof(input), stdin);
	switch(input[0]) {
		case 'y':
		case 'Y':
			ret = 1;
			break;
		case 'n':
		case 'N':
			ret = 0;
			break;
		default:
			ret = def;
			break;
	}
	return ret;
}

/* Checks if the family file with given id exists. */
int fileExists(int id) {
	char filename[] = "familyX.txt";
	int ret;
	filename[6] = '0' + id;
	FILE * fp = fopen(filename, "r");
	if((ret = fp != NULL)) {
		fclose(fp);
	}
	return ret;
}

/* List the family files that exists or does not exist depending on the given input. */
int listFiles(int listExisting) {
	int count = 0;
	printf("\n");
	for(int i = 1; i <= 9; i++) {
		if(fileExists(i) == listExisting) {
			printf("%d - family%d.txt\n", i, i);
			count++;
		}
	}
	if(!count) {
		printf("\nNo available files.\n");
	}
	return count;
}

int main() {
	nstack * leafNodes = NULL;
	char filename[] = "familyX.txt";
	char name[sizeof(leafNodes->val->name)];
	char fatherName[sizeof(leafNodes->val->name)];
	char motherName[sizeof(leafNodes->val->name)];
	node * father;
	node * mother;
	node * n;
	int age;
	int option;
	do {
		printf("\nOptions:\n0 - Exit\n1 - Create New Family Tree\n2 - Load Family Tree from File\n3 - Save Family Tree\n4 - Add Node\n5 - Remove Node\n6 - Search Relatives of a Node\n7 - Print Nuclear Family of a Node\n\n> ");
		scanf("%d", &option);
		switch(option) {
			case 0:
				if(leafNodes) {
					if(yesOrNo("\nWould you like to save the family tree", 1)) {
						save(filename, leafNodes);
					}
					while((leafNodes = popAndFree(leafNodes, leafNodes->val)));
				}
				break;
			case 1:
				printf("\nSelect one of the following files:");
				if(!listFiles(0)) {
					break;
				}
				printf("\n> ");
				scanf("%d", &option);
				if(fileExists(option) || option > 9 || option < 1) {
					printf("\nInvalid input.\n");
					option = -1;
					break;
				}
				if(leafNodes) {
					if(yesOrNo("\nWould you like to save the old family tree", 1)) {
						save(filename, leafNodes);
					}
					while((leafNodes = popAndFree(leafNodes, leafNodes->val)));
				}
				filename[6] = '0' + option;
				printf("\nEnter the name of the root node: ");
				scanf(" %63[^\n]", name);
				printf("\nEnter the age of the root node: ");
				scanf("%d", &age);
				leafNodes = push(leafNodes, n = addNode(name, age, NULL, NULL));
				break;
			case 2:
				printf("\nSelect one of the following files:");
				if(!listFiles(1)) {
					break;
				}
				printf("\n> ");
				scanf("%d", &option);
				if(!fileExists(option) || option > 9 || option < 1) {
					printf("\nInvalid input.\n");
					option = -1;
					break;
				}
				if(leafNodes) {
					if(yesOrNo("\nWould you like to save the previous family tree", 1)) {
						save(filename, leafNodes);
					}
					while((leafNodes = popAndFree(leafNodes, leafNodes->val)));
				}
				filename[6] = '0' + option;
				leafNodes = load(filename);
				break;
			case 3:
				if(leafNodes) {
					save(filename, leafNodes);
				}
				else {
					printf("\nCreate or load a family tree first.\n");
				}
				break;
			case 4:
				if(!leafNodes) {
					printf("\nCreate or load a family tree first.\n");
					break;
				}
				printf("\nEnter the name of the new node: ");
				scanf(" %63[^\n]", name);
				printf("\nEnter the age of the new node: ");
				scanf("%d", &age);
				printf("\nEnter the name of the father: ");
				scanf(" %63[^\n]", fatherName);
				printf("\nEnter the name of the mother: ");
				scanf(" %63[^\n]", motherName);
				if(!(father = searchStackByName(leafNodes, fatherName))) {
					printf("\nNo nodes with the name %s is found. If you continue, an uninitalized node with the name entered will be created. ", fatherName);
					if(!yesOrNo("Continue", 1)) {
						break;
					}
					father = addNode(fatherName, -1, NULL, NULL);
				}
				if(!(mother = searchStackByName(leafNodes, motherName))) {
					printf("\nNo nodes with the name %s is found. If you continue an uninitalized node with the name entered will be created. ", motherName);
					if(!yesOrNo("Continue", 1)) {
						if(isUninitalized(father)) {
							free(father);
						}
						break;
					}
					mother = addNode(motherName, -1, NULL, NULL);
				}
				if((n = searchStackByName(leafNodes, name))) {
					if(isUninitalized(n)) {
						printf("\nAn uninitalized node with the name %s is found. If you continue, the uninitalized node will be turned into a initalized instead of creating a new node. ", name);
						if(yesOrNo("Continue", 1)) {
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
						printf("\nA node with the name %s already exists.\n", name);
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
				break;
			case 5:
				printf("\nEnter the name of the node to be removed: ");
				scanf(" %63[^\n]", name);
				if((n = searchStackByName(leafNodes, name))) {
					if(!(!n->father && !n->mother && n->age != -1)) {
						while(n) {
							leafNodes = popAndFree(leafNodes, searchStack(leafNodes, n)->val);
							n = searchStackByName(leafNodes, name);
						}
					}
					else {
						printf("\nYou cannot remove the root node.\n");
					}
				}
				else {
					printf("\nNo nodes with the name %s is found.\n", name);
				}
				break;
			case 6:
				printf("\nEnter the name of the node: ");
				scanf(" %63[^\n]", name);
				if(!(n = searchStackByName(leafNodes, name))) {
					printf("\nNo nodes with the name %s is found.\n", name);
					break;
				}
				printf("\nTypes of Relatives:\n1 - Parents\n2 - Spouse\n3 - Children\n4 - Grandparents\n5 - Cousins\n\n> ");
				scanf("%d", &option);
				switch(option) {
					case 1:
						printf("\nFather: %s\nMother: %s\n", n->father ? n->father->name : "Unknown", n->mother ? n->mother->name : "Unknown");
						break;
					case 2:
						printf("\nSpouse: %s\n", (n = getSpouse(leafNodes, n)) ? n->name : "Unknown");
						break;
					case 3:
						printf("\nChildren: ");
						for(nstack * children = printStack(getChildrenInStack(NULL, leafNodes, n)); children; children = pop(children, children->val));
						printf("\n");
						break;
					case 4:
						printf("\nFather Side: ");
						if(n->father) {
							printf("\nGrandfather: %s\nGrandmother: %s\n", n->father->father ? n->father->father->name : "Unknown", n->father->mother ? n->father->mother->name : "Unknown");
						}
						else {
							printf("Unknown\n");
						}
						printf("\nMother Side: ");
						if(n->father) {
							printf("\nGrandfather: %s\nGrandmother: %s\n", n->mother->father ? n->mother->father->name : "Unknown", n->mother->mother ? n->mother->mother->name : "Unknown");
						}
						else {
							printf("Unknown\n");
						}
						break;
					case 5:
						printf("\nCousins: ");
						for(nstack * cousins = printStack(getCousins(NULL, leafNodes, n)); cousins; cousins = pop(cousins, cousins->val));
						printf("\n");
						break;
					case 0:
						option = -1;
					default:
						printf("\nInvalid input.\n");
						break;
				}
				break;
			case 7:
				printf("\nEnter the name of the node: ");
				scanf(" %63[^\n]", name);
				if((n = searchStackByName(leafNodes, name))) {
					printf("\nFather: %s\nMother: %s\n", n->father ? n->father->name : "Unknown", n->mother ? n->mother->name : "Unknown");
					printf("Siblings: ");
					for(nstack * siblings = printStack(getSiblings(NULL, leafNodes, n)); siblings; siblings = pop(siblings, siblings->val));
					printf("\nChildren: ");
					for(nstack * children = printStack(getChildrenInStack(NULL, leafNodes, n)); children; children = pop(children, children->val));
					printf("\n");
				}
				else {
					printf("\nNo nodes with the name %s is found.\n", name);
				}
				break;
			default:
				printf("\nInvalid option.\n");
				break;
		}
	}
	while(option);
}
