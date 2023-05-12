#include <stdio.h>
#include <string.h>

#define FILENAME "furniture_database.txt"
#define MAX_ITEMS 500
#define STR_SIZE 12

typedef struct{
	char name[STR_SIZE];
	char color[STR_SIZE];
	float price;
	char serial[7];
	int quantity;
} item;	

void add_item(item *database) {
	int i;
	for(i = 0; database[i].quantity != -1; i++);
	printf("Please enter properties(name, color, price, serial, quantity): ");
	scanf("%11s", database[i].name);
	scanf("%11s", database[i].color);
	scanf("%f", &database[i].price);
	scanf("%6s", database[i].serial);
	scanf("%d", &database[i].quantity);
	database[i + 1].quantity = -1;
}

void remove_item(item *database) {
	int idx;
	printf("Enter index you want to remove: ");
	scanf("%d", &idx);
	while(database[idx].quantity != -1) {
		database[idx] = database[idx + 1];
		idx++;
	}
}

void search_by_name(item *database) {
	char name[STR_SIZE];
	int i;
	printf("Enter name: ");
	scanf("%11s", name);
	for(i = 0; database[i].quantity != -1 && strcmp(name, database[i].name); i++);
	if(database[i].quantity != -1) {
		printf("Name: %s\nColor: %s\nPrice: $%.2f\nSerial Number: %s\nQuantity: %d\n", database[i].name, database[i].color, database[i].price, database[i].serial, database[i].quantity);
	}
	else {
		printf("not found.\n");
	}
}

void search_by_color(item *database) {
	char color[STR_SIZE];
	int i;
	printf("Enter color: ");
	scanf("%11s", color);
	for(i = 0; database[i].quantity != -1 && strcmp(color, database[i].color); i++);
	if(database[i].quantity != -1) {
		printf("Name: %s\nColor: %s\nPrice: $%.2f\nSerial Number: %s\nQuantity: %d\n", database[i].name, database[i].color, database[i].price, database[i].serial, database[i].quantity);
	}
	else {
		printf("not found.\n");
	}
}

int main() {
	int input;
	item database[MAX_ITEMS + 1];
	FILE* fp = fopen(FILENAME, "r");
	if(!fp) {
		fp = fopen(FILENAME, "w");
		if(!fp) {
			printf("Error opening file.");
			return -1;
		}
		database[0].quantity = -1;
	}
	else {
		int i;
		for(i = 0; fscanf(fp, " Name: %s Color: %s Price: $%f Serial Number: %6s Quantity: %d", database[i].name, database[i].color, &database[i].price, database[i].serial, &database[i].quantity) != EOF; i++);
		database[i].quantity = -1;
	}
	fclose(fp);
	printf("Options:\n0- exit\n1- add item\n2- remove item\n3- search by name\n4- search by color\n");
	do{
		printf("Select: ");
		scanf("%*[^0-9]");
		scanf("%d", &input);
		switch(input) {
			case 1:
				add_item(database);
				break;
			case 2:
				remove_item(database);
				break;
			case 3:
				search_by_name(database);
				break;
			case 4:
				search_by_color(database);
				break;
		}
	}
	while(input);
	fp = fopen(FILENAME, "w");
	if(!fp) {
		printf("Error opening file.");
		return -1;
	}
	for(int i = 0; database[i].quantity != -1; i++) {
		fprintf(fp, "Name: %s\nColor: %s\nPrice: $%.2f\nSerial Number: %s\nQuantity: %d\n\n", database[i].name, database[i].color, database[i].price, database[i].serial, database[i].quantity);
	}
	fclose(fp);
}
