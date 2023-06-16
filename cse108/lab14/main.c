#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

typedef struct order {
	char customerName[32];
	int orderID;
	char items[64];
	time_t orderTime;
	struct order * next;
} order;

typedef struct lifo {
	int val;
	struct lifo * next;
} lifo;

typedef struct fifo {
	int val;
	struct fifo * next;
} fifo;

void enqueue(order ** list, char * customerName, int orderID, char * items, time_t orderTime) {
	order * new;
	order ** place;
	new = malloc(sizeof(order));
	strcpy(new->customerName, customerName);
	new->orderID = orderID;
	strcpy(new->items, items);
	new->orderTime = orderTime;
	for(place = list; *place && difftime(orderTime, (*place)->orderTime) < 0; place = &(*place)->next);
	new->next = *place;
	*place = new;
}

order * dequeue(order ** list, time_t tresholdTime) {
	order * ret;
	order ** older;
	for(older = list; *older && difftime(tresholdTime, (*older)->orderTime) < 0; older = &(*older)->next);
	ret = *older;
	*older = NULL;
	return ret;
}

void printAndFreeList(order * list) {
	if(list) {
		printAndFreeList(list->next);
		char timeStr[64];
		struct tm * ti;
		ti = localtime(&list->orderTime);
		strftime(timeStr, 64, "%c", ti);
		printf("\nOrder Removed from Queue: Customer Name: %s, Order ID: %d, Items: %s, Order Time: %s\n", list->customerName, list->orderID, list->items, timeStr);
		free(list);
	}
}

void serializeLIFO(lifo * stack, const char * filename) {
	FILE * fp = fopen(filename, "wb");
	if(fp) {
		while(stack) {
			fwrite(&stack->val, sizeof(int), 1, fp);
			stack = stack->next;
		}
		fclose(fp);
	}
}

lifo * deserializeLIFO(const char * filename) {
	FILE * fp = fopen(filename, "rb");
	lifo * stack = NULL;
	if(fp) {
		lifo * new;
		lifo ** cur;
		int read;
		while(fread(&read, sizeof(int), 1, fp) == 1) {
			new = malloc(sizeof(lifo));
			new->val = read;
			new->next = NULL;
			for(cur = &stack; *cur; cur = &(*cur)->next);
			*cur = new;
		}
		fclose(fp);
	}
	return stack;
}

void serializeFIFO(fifo * list, const char * filename) {
	FILE * fp = fopen(filename, "wb");
	if(fp) {
		while(list) {
			fwrite(&list->val, sizeof(int), 1, fp);
			list = list->next;
		}
		fclose(fp);
	}
}

fifo * deserializeFIFO(const char * filename) {
	FILE * fp = fopen(filename, "rb");
	fifo * list = NULL;
	if(fp) {
		fifo * new;
		fifo ** cur;
		int read;
		while(fread(&read, sizeof(int), 1, fp) == 1) {
			new = malloc(sizeof(fifo));
			new->val = read;
			new->next = NULL;
			for(cur = &list; *cur; cur = &(*cur)->next);
			*cur = new;
		}
		fclose(fp);
	}
	return list;
}

int main() {
	order * ol = NULL;
	fifo * list;
	lifo * stack;
	time_t treshold;
	enqueue(&ol, "Ali Yilmaz", 101, "Pizza, Salad, Ice Cream", time(NULL));
	treshold = time(NULL);
	scanf("%*c");
	enqueue(&ol, "Ayse Demir", 102, "Item2", time(NULL));
	printAndFreeList(dequeue(&ol, treshold));
	list = deserializeFIFO("fifo_data.bin");
	stack = deserializeLIFO("lifo_data.bin");
	serializeFIFO(list, "fifo_data.bin");
	serializeLIFO(stack, "lifo_data.bin");
}
