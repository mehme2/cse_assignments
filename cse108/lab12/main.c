#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILENAME "customers.txt"

struct customer {
	char name[20];
	int age;
	char group[20];
	int priority;
	struct customer * next;
	struct customer * prev;
};

typedef struct customer Customer;

void save(Customer * queue) {
	FILE * fp = fopen(FILENAME, "w");
	if(fp) {
		while(queue != NULL) {
			fprintf(fp, "%s %d %s %d\n", queue->name, queue->age, queue->group, queue->priority);
			queue = queue->next;
		}
		fclose(fp);
	}
}

Customer * insert(Customer * queue, char * name, int age, char * group, int priority) {
	Customer * prev;
	Customer * new = malloc(sizeof(Customer));
	strncpy(new->name, name, 20);
	new->name[20] = '\0';
	new->age = age;
	strncpy(new->group, group, 20);
	new->group[20] = '\0';
	new->priority = priority;
	if(queue == NULL) {
		new->next = NULL;
		new->prev = NULL;
		save(new);
		return new;
	}
	for(prev = queue; prev->next != NULL && prev->next->priority < priority; prev = prev->next);
	if(prev->priority >= priority) {
		new->next = prev;
		new->prev = prev->prev;
		prev->next = new;
		if(queue == prev) {
			save(new);
			return new;
		}
	}
	else {
		new->prev = prev;
		new->next = prev->next;
		prev->next = new;
	}
	save(queue);
	return queue;
}

Customer * removeCustomer(Customer * queue, char * name) {
	Customer * cur;
	for(cur = queue; cur != NULL; cur = cur->next) {
		if(!strcmp(name, cur->name)) {
			if(cur->prev != NULL) {
				cur->prev->next = cur->next;
			}
			else {
				queue = cur->next;
			}
			if(cur->next != NULL) {
				cur->next->prev = cur->prev;
			}
			free(cur);
			break;
		}
	}
	save(queue);
	return queue;
}

void display(Customer * queue) {
	while(queue != NULL) {
		printf("%s %d %s %d\n", queue->name, queue->age, queue->group, queue->priority);
		queue = queue->next;
	}
}

void search(Customer * queue, char * name) {
	while(queue != NULL) {
		if(!strcmp(name, queue->name)) {
			printf("%s %d %s %d\n", queue->name, queue->age, queue->group, queue->priority);
			break;
		}
		queue = queue->next;
	}
	if(queue == NULL) {
		printf("\nNot found.\n");
	}
}

Customer * changePrio(Customer * queue, char * name, int prio) {
	Customer * cur;
	Customer * comp;
	for(cur = queue; cur != NULL && strcmp(cur->name, name); cur = cur->next);
	if(cur != NULL) {
		if(prio > cur->priority && cur->next != NULL) {
			for(comp = cur->next; comp->next != NULL && comp->next->priority >= prio; comp = comp->next);
			cur->next = comp->next;
			cur->prev = comp;
			comp->next = cur;
			if(comp->next != NULL) {
				comp->next->prev = comp;
			}
		}
		else if(prio < cur->priority && cur->prev != NULL) {
			for(comp = cur->prev; comp->prev != NULL && comp->prev->priority < prio; comp = comp->prev);
			cur->prev = comp->prev;
			cur->next = comp;
			comp->prev = cur;
			if(comp->prev != NULL) {
				comp->prev->next = comp;
			}
			else {
				queue = cur;
			}
		}
	}
	return queue;
}

Customer * load() {
	FILE * fp = fopen(FILENAME, "r");
	Customer * queue = NULL;
	if(fp) {
	}
	return queue;
}

int main() {
	int input;
	Customer * queue;
	do {
		printf("\nOptions:\n1-Insert\n2-Remove\n3-Display\n4-Search\n5-Change prio\n6-Exit\n");
		scanf("%d", &input);
		switch(input) {
			case 1:
				break;
			case 2:
				break;
			case 3:
				break;
			case 4:
				break;
			case 5:
				break;
			case 6:
				break;
			default:
				printf("\nInvalid option.\n");
				break;
		}
	}
	while(input != 6);
}
