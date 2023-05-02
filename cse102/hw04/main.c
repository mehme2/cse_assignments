#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#define LINE_SEPERATOR "\n----------------------------------\n\n"

void part1() {
	int c, c_prev = '\n', prod = -1, serv = -1, item_count = 0, i, student;
	float total = 0.0f, item_price, discount = 0, vat;
	FILE *receipt, *menu = fopen("menu.txt", "r");
	time_t now;
	struct tm *tm_info;
	char time_buf[18];
	if(menu == NULL) {
		printf("\nAn error ocurred while trying to open the menu file.\n");
	}
	else {
        printf("\nMenu:\n\n");
		/* print the contents if i is not zero */
		i = 1;
		while(fscanf(menu, "%c", &c) != EOF) {
			/*
			The reason c_prev exists is because while reading file
			fscanf gives a new line character before EOF so item count
			increases by one and an extra entry gets created in output
			if only c is used.
			*/
			if(c_prev == '\n') {
				printf("%2d. ", ++item_count);
			}
			/*stop printing when a space is hit */
			if(c == ' ') {
				i = 0;
			}
			/* start printing again on a new line */
			if(c == '\n') {
				i = 1;
			}
			if(i) {
				printf("%c", c);
			}
			c_prev = c;
		}
		receipt = fopen("receipt.txt", "w+");
		if(receipt == NULL) {
			printf("\nAn error ocurred while trying to open the menu file.\n");
		}
		else {
			/* time will be filled later */
			fprintf(receipt, "220104004051      00.00.0000/00:00\n");
			fprintf(receipt, LINE_SEPERATOR);
			fprintf(receipt, "Product                 Price (TL)\n");
			fprintf(receipt, LINE_SEPERATOR);
			/* break the loop if input is zero */
			while(prod && serv) {
				printf("\nPlease choose a product (1-%d): ", item_count);
				scanf("%d", &prod);
				if(prod >= 1 && prod <= item_count) {
					printf("\nHow many servings do you want? ");
					scanf("%d", &serv);
					if(serv < 0) {
						printf("\nServings cannot be negative.\n");
					}
					/* if input is not zero */
					else if(serv) {
						/* start from the beginning and get to the selected items line */
						fseek(menu, 0, SEEK_SET);
						for(i = prod - 1; i; i--) {
							c = 0;
							while(c != '\n') {
								fscanf(menu, "%c", &c);
							}
						}
						fprintf(receipt, "%2d * ", serv);
						/* i = how many characters are put */
						i = 5;
						c = 0;
						/* print the items name until space is hit */
						while(c != ' ') {
							fscanf(menu,"%c", &c);
							fprintf(receipt, "%c", c);
							i++;
						}
						/* pad with space */
						while(i < 28) {
							fprintf(receipt, " ");
							i++;
						}
						fscanf(menu, "%f", &item_price);
						fprintf(receipt, "%6.2f\n", item_price * serv);
						total += item_price * serv;
					}
				}
				/* if input is not zero */
				else if(prod) {
					printf("\nInvalid option.\n");
				}
			}
			fprintf(receipt, "\nTotal:                      %6.2f\n", total);
			student = 0;
			while(1) {
				printf("\nAre you a student? (Y/N): ");
				scanf(" %c", &student);
				if(student == 'y' || student == 'Y' || student == 'n' || student == 'N') {
					break;
				}
				else {
					printf("\nInvalid input.\n");
				}
			}
			if(student == 'y' || student == 'Y') {
				fprintf(receipt, "\nStudent discount:           %6.2f\n", -total * 0.125);
				discount += total * 0.125;
			}
			if(total >= 150) {
				fprintf(receipt, "\n10%% discount:               %6.2f\n", -total * 0.1);
				discount += total * 0.1;
			}
			fprintf(receipt, LINE_SEPERATOR);
			vat = (total - discount) * 0.18;
			fprintf(receipt, "Price:                      %6.2f\n", total - discount);
			fprintf(receipt, "Price + VAT:                %6.2f\n\n", total - discount + vat);
			/* get the current time */
			now = time(NULL);
			tm_info = localtime(&now);
			strftime(time_buf, 18, "%d.%m.%Y/%H:%M", tm_info);
			/* print current time to the allocated space in the receipt */
			fseek(receipt, 18, SEEK_SET);
			fprintf(receipt, "%s", time_buf);
			printf("\nReceipt:\n\n");
			fseek(receipt, 0, SEEK_SET);
			/* print the receipt */
			while(fscanf(receipt, "%c", &c) != EOF) {
				printf("%c", c);
			}
			fclose(receipt);
		}
		fclose(menu);
	}
}

/* prints rock, paper or scissor depending on the input */
void printrps(int i) {
	switch(i) {
		case 1:
			printf("Rock");
			break;
		case 2:
			printf("Paper");
			break;
		case 3:
			printf("Scissors");
			break;
	}
}

void part2() {
	int play = 1, user, cpu, i;
	while(play) {
		printf("\nPlease make a choice!\n1: Rock, 2: Paper, 3: Scissors\n\nSelect: ");
		scanf("%d", &user);
		if(user == 1 || user == 2 || user == 3) {
			cpu = rand() % 3 + 1;
			printf("\nYou chose ");
			printrps(user);
			printf(". I chose ");
			printrps(cpu);
			printf(".");
			switch((user - cpu + 3) % 3) {
				case 0:
					printf("It's a tie!\n");
					break;
				case 1:
					printf("You won!\n");
					break;
				case 2:
					printf("I won!\n");
					break;
			}
			while(1) {
				printf("\nDo you want to play again? (Y/N): ");
				scanf(" %c", &user);
				if(user == 'y' || user == 'Y' || user == 'n' || user == 'N') {
					break;
				}
				else {
					printf("\nInvalid input.\n");
				}
			}
			play = user == 'y' || user == 'Y';
		}
		else {
			printf("\nInvalid input.\n");
		}
	}
}

int main() {
	int input = -1;
	srand(time(NULL));
	/* if input is not zero */
	while(input) {
		printf("\nOptions:\n0) Exit\n1) Part 1\n2) Part 2\n\nSelect: ");
		scanf("%d", &input);
		switch(input) {
			case 1:
				part1();
				break;
			case 2:
				part2();
				break;
			default:
				printf("\nInvalid input.\n");
			case 0:
				break;
		}
	}
}
