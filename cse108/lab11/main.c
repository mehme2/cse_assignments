#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct order {
	char customerName[20];
	int orderID;
	char items[64];
	char orderTime[20];
	struct order * next;
};

typedef struct order Order;

struct examPaper {
	char studentName[20];
	int studentNumber;
	int score;
	struct examPaper * next;
};

typedef struct examPaper ExamPaper;

Order * list_enqueue(Order * queue, char * customerName, int orderID, char * items, char * orderTime) {
	Order * cur;
	Order * new = malloc(sizeof(Order));
	strncpy(new->customerName, customerName, 20);
	new->customerName[19] = '\0';
	new->orderID = orderID;
	strncpy(new->items, items, 64);
	new->items[63] = '\0';
	strncpy(new->orderTime, orderTime, 20);
	new->orderTime[20] = '\0';
	new->next = NULL;
	if(queue != NULL) {
		for(cur = queue; cur->next != NULL; cur = cur->next);
		cur->next = new;
	}
	else {
		queue = new;
	}
	return queue;
}

Order * dequeue(Order * queue) {
	Order * new = NULL;
	if(queue != NULL) {
		new = queue->next;
		free(queue);
	}
	return new;
}

void display(Order * queue) {
	Order * cur;
	for(cur = queue; cur != NULL; cur = cur->next) {
		printf("\n- Order ID: %d, Customer Name: %s, Items: %s\n", cur->orderID, cur->customerName, cur->items);
	}
}

void updateOrder(Order * queue, int orderID, char * newItems) {
	Order * cur;
	for(cur = queue; cur != NULL && cur->orderID != orderID; cur = cur->next);
	if(cur != NULL) {
		strncpy(cur->items, newItems, 64);
		cur->items[63] = '\0';
	}
}

ExamPaper * push(ExamPaper * stack, char * studentName, int studentNumber, int score) {
	ExamPaper * new = malloc(sizeof(ExamPaper));
	strncpy(new->studentName, studentName, 20);
	new->studentName[19] = '\0';
	new->studentNumber = studentNumber;
	new->score = score;
	new->next = stack;
	return new;
}

ExamPaper * pop(ExamPaper * stack) {
	ExamPaper * newStart;
	if(stack != NULL) {
		newStart = stack->next;
		free(stack);
	}
	else {
		newStart = NULL;
	}
	return newStart;
}

int isEmpty(ExamPaper * stack) {
	return stack == NULL;
}

void displayStack(ExamPaper * stack) {
	ExamPaper * cur;
	for(cur = stack; cur != NULL; cur = cur->next) {
		printf("\n- Student ID: %d, Student Name: %s, Score: %d\n", cur->studentNumber, cur->studentName, cur->score);
	}
}

int selectionGet(int min, int max, const char options[], const char prompt[]) {
	int input;
	printf("%s", options);
	while(1) {
		printf("%s", prompt);
		if(!scanf("%*[^-0-9]%d", &input)) {
			scanf("%d", &input);
		}
		if(input >= min && input <= max) {
			break;
		}
		else {
			printf("\nInvalid input.\n");
		}
	}
	return input;
}

void part1() {
	int input;
	Order * fifo = NULL;
	char customerName[20];
	int orderID;
	char items[64];
	char orderTime[20];
	do {
		input = selectionGet(0, 4, "\nOptions\n0 - Exit\n1 - Add new items to the list\n2 - Update an order by ID\n3 - Display the list\n4 - Remove oldest order\n", "\n> ");
		switch(input) {
			case 1:
				printf("\nEnter customer name: ");
				scanf(" %19[^\n]", customerName);
				printf("\nEnter order ID: ");
				scanf("%d", &orderID);
				printf("\nEnter items: ");
				scanf(" %63[^\n]", items);
				printf("\nEnter order time: ");
				scanf(" %19[^\n]", orderTime);
				fifo = list_enqueue(fifo, customerName, orderID, items, orderTime);
				break;
			case 2:
				printf("\nEnter order ID: ");
				scanf("%d", &orderID);
				printf("\nEnter new items: ");
				scanf(" %63[^\n]", items);
				updateOrder(fifo, orderID, items);
				break;
			case 3:
				display(fifo);
				break;
			case 4:
				fifo = dequeue(fifo);
				break;
		}
	}
	while(input);
	while(fifo != NULL) {
		fifo = dequeue(fifo);
	}
}

void part2() {
	int input;
	ExamPaper * lifo = NULL;
	char studentName[20];
	int studentNumber;
	int score;
	do {
		input = selectionGet(0, 3, "\nOptions\n0 - Exit\n1 - Push new entry\n2 - Pop\n3 - Display the list\n", "\n> ");
		switch(input) {
			case 1:
				printf("\nEnter student name: ");
				scanf(" %19[^\n]", studentName);
				printf("\nEnter student number: ");
				scanf("%d", &studentNumber);
				printf("\nEnter score: ");
				scanf("%d", &score);
				lifo = push(lifo, studentName, studentNumber, score);
				break;
			case 2:
				lifo = pop(lifo);
				break;
			case 3:
				displayStack(lifo);
				break;
		}
	}
	while(input);
	while(lifo != NULL) {
		lifo = pop(lifo);
	}
}

int main() {
	int input;
	do {
		input = selectionGet(0, 2, "\nOptions\n0 - Exit\n1 - Part 1\n2 - Part 2\n", "\n> ");
		if(input == 1) {
			part1();
		}
		else if(input == 2) {
			part2();
		}
	}
	while(input);
}
