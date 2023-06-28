#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define FILENAME "linkedlist.bin"

typedef struct Node {
	void * data;
	struct Node * next;
} Node;

typedef struct {
	char type[20];
	int ivals[1];
	double svals[1];
} Asset1;

typedef struct {
	char type[20];
	double svals[2];
	int ivals[2];
} Asset2;

typedef struct {
	char type[20];
	char string1[50];
	char string2[50];
} Asset3;

typedef struct {
	char type[20];
	double value1;
	float value2;
	double value3;
} Asset4;

void printAsset(void * data) {
	if(!strcmp(((Asset1*)data)->type, "Asset1")) {
		printf("Type: %s | ivals[0]: %d | svals[0]: %lf\n", ((Asset1*)data)->type, ((Asset1*)data)->ivals[0], ((Asset1*)data)->svals[0]);
	}
	else if(!strcmp(((Asset2*)data)->type, "Asset2")) {
		printf("Type: %s | ivals[0]: %d | ivals[1]: %d | svals[0]: %lf | svals[1]: %lf\n", ((Asset2*)data)->type, ((Asset2*)data)->ivals[0], ((Asset2*)data)->ivals[1], ((Asset2*)data)->svals[0], ((Asset2*)data)->svals[1]);
	}
	else if(!strcmp(((Asset3*)data)->type, "Asset3")) {
		printf("Type: %s | string1: %s | string2: %s\n", ((Asset3*)data)->type, ((Asset3*)data)->string1, ((Asset3*)data)->string2);
	}
	else if(!strcmp(((Asset4*)data)->type, "Asset4")) {
		printf("Type: %s | value1: %lf | value2: %f | value3: %lf\n", ((Asset4*)data)->type, ((Asset4*)data)->value1, ((Asset4*)data)->value2, ((Asset4*)data)->value3);
	}
	else {
		printf("Type: Error\n");
	}
}

double randd() {
	return (double)rand() / (double)RAND_MAX;
}

void randstr(char * buf, int size) {
	int i;
	for(i = 0; i < size - 1; buf[i++] = rand() % ('~' - ' ') + ' ');
	buf[i] = '\0';
}

void freeList(Node * head) {
	Node * rm;
	while(head) {
		rm = head;
		head = head->next;
		free(rm->data);
		free(rm);
	}
}

void fillLinkedList(Node ** head) {
	Node ** cur;
	int i, n = rand() % 11 + 10;
	for(cur = head, i = 0; i < n; i++, cur = &(*cur)->next) {
		*cur = malloc(sizeof(Node));
		switch(rand() % 4) {
			case 0:
				(*cur)->data = malloc(sizeof(Asset1));
				strcpy(((Asset1*)(*cur)->data)->type, "Asset1");
				((Asset1*)(*cur)->data)->ivals[0] = rand();
				((Asset1*)(*cur)->data)->svals[0] = randd();
				break;
			case 1:
				(*cur)->data = malloc(sizeof(Asset2));
				strcpy(((Asset2*)(*cur)->data)->type, "Asset2");
				((Asset2*)(*cur)->data)->ivals[0] = rand();
				((Asset2*)(*cur)->data)->ivals[1] = rand();
				((Asset2*)(*cur)->data)->svals[0] = randd();
				((Asset2*)(*cur)->data)->svals[1] = randd();
				break;
			case 2:
				(*cur)->data = malloc(sizeof(Asset3));
				strcpy(((Asset3*)(*cur)->data)->type, "Asset3");
				randstr(((Asset3*)(*cur)->data)->string1, rand() % 51);
				randstr(((Asset3*)(*cur)->data)->string2, rand() % 51);
				break;
			case 3:
				(*cur)->data = malloc(sizeof(Asset4));
				strcpy(((Asset4*)(*cur)->data)->type, "Asset4");
				((Asset4*)(*cur)->data)->value1 = randd();
				((Asset4*)(*cur)->data)->value2 = randd();
				((Asset4*)(*cur)->data)->value3 = randd();
				break;
		}
		printf("\nGenerated: ");
		printAsset((*cur)->data);
	}
	*cur = NULL;
	printf("\nGenerated %d assets.\n", n);
}

void serializeLinkedList(Node * head) {
	int n, size;
	FILE * fp = fopen(FILENAME, "wb");
	if(fp) {
		n = 0;
		while(head) {
			if(!strcmp(((Asset1*)head->data)->type, "Asset1")) {
				size = sizeof(Asset1);
			}
			else if(!strcmp(((Asset2*)head->data)->type, "Asset2")) {
				size = sizeof(Asset2);
			}
			else if(!strcmp(((Asset3*)head->data)->type, "Asset3")) {
				size = sizeof(Asset3);
			}
			else if(!strcmp(((Asset4*)head->data)->type, "Asset4")) {
				size = sizeof(Asset4);
			}
			else {
				size = 0;
			}
			if(fwrite(head->data, size, 1, fp) == 1) {
				printf("\nSerialized: ");
				n++;
			}
			else {
				printf("\nFailed to Serialize: ");
			}
			printAsset(head->data);
			head = head->next;
		}
		printf("\nSerialized %d assets to file %s.\n", n, FILENAME);
		fclose(fp);
	}
	else {
		printf("\nError writing to file %s.\n", FILENAME);
	}
}

void deserializeLinkedList(Node ** head) {
	int n, i, size;
	Node ** cur;
	char type[20];
	FILE * fp = fopen(FILENAME, "rb");
	if(fp) {
		for(cur = head, n = 0, i = ftell(fp); fread(type, sizeof(type), 1, fp) == 1; cur = &(*cur)->next, n++) {
			*cur = malloc(sizeof(Node));
			fseek(fp, i, SEEK_SET);
			if(!strcmp(type, "Asset1")) {
				size = sizeof(Asset1);
			}
			else if(!strcmp(type, "Asset2")) {
				size = sizeof(Asset2);
			}
			else if(!strcmp(type, "Asset3")) {
				size = sizeof(Asset3);
			}
			else if(!strcmp(type, "Asset4")) {
				size = sizeof(Asset4);
			}
			else {
				size = 0;
			}
			(*cur)->data = malloc(size);
			if(fread((*cur)->data, size, 1, fp) != 1) {
				break;
			}
			i = ftell(fp);
			printf("\nDeserialized: ");
			printAsset((*cur)->data);
		}
		*cur = NULL;
		printf("\nDeserialized %d assets from file %s.\n", n, FILENAME);
		fclose(fp);
	}
	else {
		printf("\nError reading from file %s.\n", FILENAME);
		*head = NULL;
	}
}

int main() {
	int input;
	Node * list = NULL;
	srand(time(NULL));
	do {
		printf("\nOptions:\n0 - Exit\n1 - Fill Linked List\n2 - Serialize Linked List\n3 - Deserialize Linked List\n\n> ");
		scanf("%d", &input);
		switch(input) {
			case 0:
				break;
			case 1:
				freeList(list);
				fillLinkedList(&list);
				break;
			case 2:
				if(list) {
					serializeLinkedList(list);
				}
				else {
					printf("\nNo data to serialize.\n");
				}
				break;
			case 3:
				freeList(list);
				deserializeLinkedList(&list);
				break;
			default:
				printf("\nInvalid input.\n");
				break;
		}
	}
	while(input);
	freeList(list);
}
