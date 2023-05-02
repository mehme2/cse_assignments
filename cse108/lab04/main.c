#include <stdio.h>

#define FILE_NAME "shapes.txt"

void generate_file() {
	FILE* file;
	char c = 0;
	int size;
	file = fopen(FILE_NAME, "w");
	if(file == NULL) {
		printf("\nArror ocurred while trying to open file.\n");
	}
	else {
		while(c != 'e') {
			printf("\nEnter t, s or e: ");
			scanf(" %c", &c);
			switch(c) {
				case 't':
				case 's':
					do {
						printf("\nEnter size for shape: ");
						scanf("%d", &size);
						if(size < 3 || size > 10) {
							printf("\nSize should be between 3-10.\n");
						}
					}
					while(size < 3 || size > 10);
					fprintf(file, "%c,%d\n", c, size);
					break;
				case 'e':
					fprintf(file, "e");
					break;
				default:
					printf("\nInvalid input!\n");
			}
		}
		fclose(file);
	}
}

void draw_square(int size) {
	int i, j;
	/* top side */
	printf("\n");
	for(i = 0; i < size; i++) {
		printf("*");
	}
	printf("\n");
	/* middle part */
	for(i = 0; i < size - 2; i++) {
		printf("*");
		for(j = 0; j < size - 2; j++) {
			printf(" ");
		}
		printf("*\n");
	}
	/* bottom side */
	for(i = 0; i < size; i++) {
		printf("*");
	}
	printf("\n");
}

void draw_triangle(int size) {
	int i, j;
	/* tip of triangle */
	printf("\n");
	for(int i = 0; i < size - 1; i++) {
		printf(" ");
	}
	printf("*");
	printf("\n");
	/* middle part */
	for(i = 1; i < size - 1; i++) {
		for(int j = size - i - 1; j > 0; j--) {
			printf(" ");
		}
		printf("*");
		for(j = 2 * i - 1; j > 0; j--) {
			printf(" ");
		}
		printf("*\n");
	}
	/* bottom side */
	for(i = 2 * size - 1; i > 0; i--) {
		printf("*");
	}
	printf("\n");
}

void read_and_draw_file() {
	FILE* file;
	char type = 0;
	int size;
	file = fopen(FILE_NAME, "r");
	if(file == NULL) {
		printf("\nFile does not exist or an error occured while trying to open it.\n");
	}
	else {
		while(type != 'e') {
			fscanf(file, " %c", &type);
			switch(type) {
				case 't':
					while(type != ',') {
						fscanf(file, " %c", &type);
					}
					fscanf(file, "%d", &size);
					draw_triangle(size);
					break;
				case 's':
					while(type != ',') {
						fscanf(file, " %c", &type);
					}
					fscanf(file, "%d", &size);
					draw_square(size);
					break;
			}
		}
		fclose(file);
	}
}

int main() {
	int input = 0;
	while(input != 3) {
		printf("\nWelcome to Shape Reader! Please make your choice to continue:\n1-) Generate a shape file!\n2-) Read and draw a shape file\n3-)Terminate the program\nSelect: ");
		scanf("%d", &input);
		switch(input) {
			case 1:
				generate_file();
				break;
			case 2:
				read_and_draw_file();
				break;
		}
	}
}
