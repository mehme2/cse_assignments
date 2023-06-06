#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct {
	int row, col;
} point;

typedef struct {
	char type;
	int value;
} block;

block ***init_board() {
	block ***board;
	int r, c, ok, height;
	/* allocate rows */
	board = malloc(10 * sizeof(block**));
	/* allocate columns and make each cell empty */
	for(r = 0; r < 10; r++) {
		board[r] = malloc(10 * sizeof(block*));
		for(c = 0; c < 10; c++) {
			board[r][c] = malloc(sizeof(block));
			board[r][c]->type = 'e';
			board[r][c]->value = 0;
		}
	}
	/* find a suitable position and generate the bait tile */
	ok = 0;
	while(!ok) {
		r = rand() % 10;
		c = rand() % 10;
		ok = r != 0 || c != 0;
	}
	board[r][c]->type = 'b';
	/* find a suitable position and generate the obstacle tile */
	/* the first obstacle in the array is the height of the tile with decreasing by one in each block in the array and it is ended by an empty tile */
	ok = 0;
	while(!ok) {
		r = rand() % 10;
		c = rand() % 10;
		ok = r != 0 || c != 0 && board[r][c]->type == 'e';
	}
	for(height = 0; board[r][c][height].type != 'e'; height++);
	board[r][c] = realloc(board[r][c], sizeof(block) * (height++ + 2));
	board[r][c][height].type = 'e';
	board[r][c][height - 1].type = 'o';
	for(height--; height >= 0; height--) {
		board[r][c][height].value++;
	}
	return board;
}

void draw_board(block ***board, point *snake) {
	int r, c, i;
	char print;
	/* top edge */
	printf("\n|--------------------|\n|");
	for(r = 0; r < 10; r++) {
		for(c = 0; c < 10; c++) {
			/* check if the tile is captured by snake */
			for(i = 0; snake[i].row != -1; i++) {
				if(snake[i].row == r && snake[i].col == c) {
					printf(" %c", i ? 'X' : 'O');
					break;
				}
			}
			/* if the tile is not captured print depending on its type */
			if(snake[i].row == -1) {
				switch(board[r][c]->type) {
					case 'e':
						printf("  ");
						break;
					case 'b':
						printf(" .");
						break;
					case 'o':
						printf("%2d", board[r][c]->value % 100);
						break;
				}
			}
		}
		/* left and right edges */
		printf("|\n|");
	}
	/* bottom edge */
	printf("--------------------|\n");
}

char move(point *snake) {
	char input, cont;
	point headOld;
	headOld = snake[0];
	/* ask user until the move is legal */
	do {
		printf("\n> ");
		scanf(" %c", &input);
		cont = 0;
		switch(input) {
			case 'w':
			case 'W':
				snake[0].row--;
				break;
			case 'a':
			case 'A':
				snake[0].col--;
				break;
			case 's':
			case 'S':
				snake[0].row++;
				break;
			case 'd':
			case 'D':
				snake[0].col++;
				break;
			default:
				printf("\nIllegal move!\n");
				cont = 1;
				break;
		}
		/* if user is trying to go back */
		if(snake[1].row != -1 && snake[0].row == snake[1].row && snake[0].col == snake[1].col) {
			printf("\nIllegal move!\n");
			cont = 1;
			snake[0] = headOld;
		}
	}
	while(cont);
	return input;
}

int check_status(block ***board, point *snake) {
	int i;
	int over = 0;
	block *comp;
	/* check if the head is colliding with the body */
	for(i = 1; snake[i].row != -1; i++) {
		if(snake[i].row == snake[0].row && snake[i].col == snake[0].col) {
			over = 1;
			break;
		}
	}
	/* check wall collision */
	over = over || snake[0].row >= 10 || snake[0].row < 0 || snake[0].col >= 10 || snake[0].col < 0;
	/* check obstacle collision */
	over = over || (comp = board[snake[0].row][snake[0].col])->type == 'o' && comp->value >= i;
	return over;
}

point *update(block ***board, point *snake, char last_move, int nMoves) {
	int size, i, ok, r, c, height, oc;
	block *comp = board[snake[0].row][snake[0].col];
	for(size = 0; snake[size].row != -1; size++);
	/* if the tile is bait */
	if(comp->type == 'b') {
		/* expand the snake by reallocating */
		snake = realloc(snake, (size++ + 2) * sizeof(point));
		/* set the last member of the array to sentinel */
		snake[size].row = -1;
		/* set the tile to empty */
		comp->type = 'e';
		ok = 0;
		/* find a new suitable place for the bait tile */
		while(!ok) {
			r = rand() % 10;
			c = rand() % 10;
			if(board[r][c]->type == 'e') {
				ok = 1;
				for(i = 0; i < size; i++) {
					if(snake[i].row == r && snake[i].col == c) {
						ok = 0;
						break;
					}
				}
			}
		}
		board[r][c]->type = 'b';
	}
	/* if the is an obstacle */
	else if(comp->type == 'o') {
		/* set it to empty without checking becuause if the value was higher the program would exit the loop without calling this function */
		comp = board[snake[0].row][snake[0].col] = realloc(comp, sizeof(block));
		comp->type = 'e';
		comp->value = 0;
	}
	/* start from tail and set each part to the next ones position */
	for(i = size - 1; i > 0; snake[i--] = snake[i - 1]);
	/* the next part from the head will be on heads new position so we undo the the last move to get its position */
	if(size >= 2) {
		switch(last_move) {
			case 'w':
			case 'W':
				snake[1].row++;
				break;
			case 'a':
			case 'A':
				snake[1].col++;
				break;
			case 's':
			case 'S':
				snake[1].row--;
				break;
			case 'd':
			case 'D':
				snake[1].col--;
				break;
		}
	}
	/* every 5 moves excluding the 0th move */
	if(nMoves && nMoves % 5 == 0) {
		ok = 0;
		oc = 0;
		/* count the number of obstacle tiles */
		for(r = 0; r < 10; r++) {
			for(c = 0; c < 10; c++) {
				oc += board[r][c]->type == 'o';
			}
		}
		/* find a suitable place for the new obstacle */
		while(!ok) {
			r = rand() % 10;
			c = rand() % 10;
			switch(board[r][c]->type) {
				case 'e':
					ok = oc < 3;
					break;
				case 'b':
					ok = 0;
					break;
				case 'o':
					ok = 1;
					break;
			}
			if(ok) {
				for(i = 0; snake[i].row != -1; i++) {
					if(snake[i].row == r && snake[i].col == c) {
						ok = 0;
						break;
					}
				}
			}
		}
		/* get the height of the tile */
		for(height = 0; board[r][c][height].type != 'e'; height++);
		/* increase the height by reallocating */
		board[r][c] = realloc(board[r][c], sizeof(block) * (height++ + 2));
		/* set the last of the array to sentinel */
		board[r][c][height].type = 'e';
		/* set the previous sentinel to obstacle */
		board[r][c][height - 1].type = 'o';
		/* increase the previous obstacle blocks values */
		for(i = 0; i < height; i++) {
			board[r][c][i].value++;
		}
	}
	return snake;
}

void play(block ***board) {
	point *snake;
	int nMoves;
	char lastMove, size;
	/* initialize snake as head + sentinel */
	snake = malloc(2 * sizeof(point));
	snake[0].row = snake[0].col = 0;
	snake[1].row = -1;
	nMoves = 0;
	/* game loop */
	while(!check_status(board, snake)) {
		snake = update(board, snake, lastMove, nMoves);
		draw_board(board, snake);
		lastMove = move(snake);
		nMoves++;
	}
	/*
	  ____    _    __  __ _____    _____     _______ ____  
	 / ___|  / \  |  \/  | ____|  / _ \ \   / / ____|  _ \ 
	| |  _  / _ \ | |\/| |  _|   | | | \ \ / /|  _| | |_) |
	| |_| |/ ___ \| |  | | |___  | |_| |\ V / | |___|  _ < 
	 \____/_/   \_\_|  |_|_____|  \___/  \_/  |_____|_| \_\
	*/
	printf("\n  ____    _    __  __ _____    _____     _______ ____  \n / ___|  / \\  |  \\/  | ____|  / _ \\ \\   / / ____|  _ \\ \n| |  _  / _ \\ | |\\/| |  _|   | | | \\ \\ / /|  _| | |_) |\n| |_| |/ ___ \\| |  | | |___  | |_| |\\ V / | |___|  _ < \n \\____/_/   \\_\\_|  |_|_____|  \\___/  \\_/  |_____|_| \\_\\\n");
	for(size = 1; snake[size].row != -1; size++);
	printf("\nMoves: %d\nSize: %d\n", nMoves, size);
	free(snake);
}

int main() {
	int r, c;
	block ***board;
	srand(time(NULL));
	board = init_board();
	play(board);
	for(r = 0; r < 10; r++) {
		for(c = 0; c < 10; c++) {
			free(board[r][c]);
		}
		free(board[r]);
	}
	free(board);
}
