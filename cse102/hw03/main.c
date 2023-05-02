#include <stdio.h>
#include <stdlib.h> /* for srand and rand */
#include <time.h> /* to put time as seed */

#define UP 'w'
#define LEFT 'a'
#define DOWN 's'
#define RIGHT 'd'

/* Part 1 */
/* Prints the room */
void draw_room(int size, int cx, int cy, int dx, int dy) {
	int ix, iy;
	printf("\n");
	/* top border */
	for(ix = 2 * size + 1; ix > 0; ix--) {
		printf("-");
	}
	printf("\n");
	for(iy = size - 1; iy >= 0; iy--) {
		for(ix = 0; ix < size; ix++) {
			/* left border and cell seperator */
			printf("|");
			if(ix == cx && iy == cy) {
				printf("C");
			}
			else if(ix == dx && iy == dy) {
				printf("D");
			}
			else {
				printf(" ");

			}
		}
		/* right border */
		printf("|\n");
	}
	/* bottom border */
	for(ix = 2 * size + 1; ix > 0; ix--) {
		printf("-");
	}
	printf("\n");
}

/* Part 2 */
/* Asks the user to select a move until input is valid */
void move_char(int size, int *cx, int *cy) {
	int valid = 0;
	char input;
	while(!valid) {
		printf("\nYour move: ");
		scanf(" %c", &input);
		switch(input) {
			case UP:
				if(*cy < size - 1) {
					(*cy)++;
					valid = 1;
				}
				else {
					printf("\nColliding with upper border!\n");
				}
				break;
			case LEFT:
				if(*cx > 0) {
					(*cx)--;
					valid = 1;
				}
				else {
					printf("\nColliding with left border!\n");
				}
				break;
			case DOWN:
				if(*cy > 0) {
					(*cy)--;
					valid = 1;
				}
				else {
					printf("\nColliding with bottom border!\n");
				}
				break;
			case RIGHT:
				if(*cx < size - 1) {
					(*cx)++;
					valid = 1;
				}
				else {
					printf("\nColliding with right border!\n");
				}
				break;
			default:
				printf("\nInvalid option!\n");
				break;
		}
	}
}

/* Part 3 */
/* Returns 1 if character is in the tile as door */
int check_status(int cx, int cy, int dx, int dy) {
	return cx == dx && cy == dy;
}

/* Part 4 */
/* Returns 1 if user chose New Game */
int game_menu(int *size, int *cx, int *cy, int *dx, int *dy) {
	int input, start = -1;
	printf("\nWelcome to the 2D puzzle game!\n1. New Game\n2. Help\n3. Exit\n");
	while(start == -1) {
		printf("\nSelect: ");
		scanf("%d", &input);
		switch(input) {
			case 1:
				while(1) {
					printf("\nEnter room size: ");
					scanf("%d", &input);
					if(input < 5) {
						printf("\nThe room is too small!\n");
					}
					else if(input > 10) {
						printf("\nThe room is too big!\n");
					}
					else {
						*size = input;
						break;
					}
				}
				do {
					*cx = rand() % *size;
					*cy = rand() % *size;
					*dx = rand() % *size;
					*dy = rand() % *size;
				}
				while(check_status(*cx, *cy, *dx, *dy));
				start = 1;
				break;
			case 2:
				printf("\nHOW DO I PLAY:\nAfter you select \"New Game\" you will be prompted to select the size of the room and the game will generate a room with your character('C') and a door('D') at random places. The game will prompt you to select a movement option and your character will move accordingly. The goal is to get the character to the door.\n\nCONTROLS:\n%c - Move Up\n%c - Move Left\n%c - Move Down\n%c - Move Right\n(case sensitive)\n", UP, LEFT, DOWN, RIGHT);
				break;
			case 3:
				start = 0;
				break;
			default:
				printf("\nInvalid option!\n");
				break;
		}
	}
	return start;
}

int main() {
	int size, cx, cy, dx, dy, moves;
	srand(time(NULL));
	while(game_menu(&size, &cx, &cy, &dx, &dy)) {
		moves = 0;
		while(!check_status(cx, cy, dx, dy)) {
			draw_room(size, cx, cy, dx, dy);
			move_char(size, &cx, &cy);
			moves++;
		}
		/*
		__   _____  _   _  __        _____ _   _ _ 
		\ \ / / _ \| | | | \ \      / /_ _| \ | | |
		 \ V / | | | | | |  \ \ /\ / / | ||  \| | |
		  | || |_| | |_| |   \ V  V /  | || |\  |_|
		  |_| \___/ \___/     \_/\_/  |___|_| \_(_)
		*/
		printf("\n__   _____  _   _  __        _____ _   _ _ \n\\ \\ / / _ \\| | | | \\ \\      / /_ _| \\ | | |\n \\ V / | | | | | |  \\ \\ /\\ / / | ||  \\| | |\n  | || |_| | |_| |   \\ V  V /  | || |\\  |_|\n  |_| \\___/ \\___/     \\_/\\_/  |___|_| \\_(_)\n");
		printf("\nMoves used: %d\n", moves);
	}
	printf("\nGoodbye!\n");
}
