#include <stdio.h>
#include <math.h>
#include <string.h>

#define MAX_BUF_SIZE 50

void part1() {
	char filename[MAX_BUF_SIZE];
	int letters[26];
	int i = 0;
	char c;
	FILE* file;
	printf("\nEnter the file name: ");
	scanf(" %c", &c);
	/* read until new line or end of buffer */
	while(c != '\n' && i < MAX_BUF_SIZE - 1) {
		filename[i] = c;
		i++;
		scanf("%c", &c);
	}
	/* set final char to the sentinel value */
	filename[i] = '\0';
	file = fopen(filename, "r");
	if(file == NULL) {
		printf("\nAn error occurred while opening %s.\n", filename);
	}
	else {
		/* set counter to zero */
		for(i = 0; i < 26; i++) {
			letters[i] = 0;
		}
		/* count the letters */
		while(fscanf(file, "%c", &c) != EOF) {
			if(c >= 'A' && c <= 'Z') {
				letters[(c - 'A')]++;
			}
			if(c >= 'a' && c <= 'z') {
				letters[(c - 'a')]++;
			}
		}
		printf("\nLetter Frequency:\n");
		for(i = 0; i < 26; i++) {
			printf("%c: %d\n", i + 'A', letters[i]);
		}
		fclose(file);
	}
}

typedef enum {
	RED,
	GREEN,
	BLUE,
	YELLOW,
	ORANGE
} Color;

const float rgb[][3] = {
	{1.0, 0.0, 0.0}, //RED
	{0.0, 1.0, 0.0}, //GREEN
	{0.0, 0.0, 1.0}, //BLUE
	{0.5, 0.5, 0.0}, //YELLOW
	{0.5, 0.4, 0.2}  //ORANGE
};

/* mix and get closest color based on euclidean distance */
Color mixEuclidean(Color a, Color b) {
	float mixed[3];
	/* get average of both colors */
	mixed[0] = (rgb[a][0] + rgb[b][0]) / 2;
	mixed[1] = (rgb[a][1] + rgb[b][1]) / 2;
	mixed[2] = (rgb[a][2] + rgb[b][2]) / 2;
	Color closestColor;
	/* set initial distance to a large number */
	float closestDist = 1000.0;
	/* compare distances and set the closest if distance is smaller */
	for(int i = 0; i < 5; i++) {
		float diff[3];
		diff[0] = mixed[0] - rgb[i][0];
		diff[1] = mixed[1] - rgb[i][1];
		diff[2] = mixed[2] - rgb[i][2];
		float dist = sqrt(diff[0] * diff[0] + diff[1] * diff[1] + diff[2] * diff[2]);
		if(dist < closestDist) {
			closestDist = dist;
			closestColor = i;
		}
	}
	return closestColor;
}

Color colorMixer(Color func(Color, Color), Color a, Color b) {
	return func(a, b);
}

void part2() {
	char c;
	/* colors to be mixed, set to -1 to make them unset */
	Color col[2] = {-1, -1};
	Color mixed;
	int i;
	/* repeat for both colors */
	for(i = 0; i < 2; i++) {
		do {
			printf("\nEnter Color %d (r,g,b,y,o): ", i + 1);
			scanf(" %c", &c);
			switch(c) {
				case 'R':
				case 'r':
					col[i] = RED;
					break;
				case 'G':
				case 'g':
					col[i] = GREEN;
					break;
				case 'B':
				case 'b':
					col[i] = BLUE;
					break;
				case 'Y':
				case 'y':
					col[i] = YELLOW;
					break;
				case 'O':
				case 'o':
					col[i] = ORANGE;
					break;
				default:
					printf("\nInvalid input.\n");
					break;
			}
		}
		while(col[i] == -1);
	}
	mixed = colorMixer(mixEuclidean, col[0], col[1]);
	printf("\nMixed Color: ");
	switch(mixed) {
		case RED:
			printf("RED");
			break;
		case GREEN:
			printf("GREEN");
			break;
		case BLUE:
			printf("BLUE");
			break;
		case YELLOW:
			printf("YELLOW");
			break;
		case ORANGE:
			printf("ORANGE");
			break;
	}
	printf(" [%.1f, %.1f, %.1f]\n", rgb[mixed][0], rgb[mixed][1], rgb[mixed][2]);
}

/* return 1 if player using the symbol wins */
int checkWin(char board[], char symbol) {
	/* every alignment to win */
	const int winPositions[][3] = {
		{0, 1, 2}, //top row
		{3, 4, 5}, //middle row
		{6, 7, 8}, //bottom row
		{0, 3, 6}, //left column
		{1, 4, 7}, //middle column
		{2, 5, 8}, //right column
		{0, 4, 8}, //top left to bottom right diagonal
		{2, 4, 6}  //top right to bottom left diagonal
	};
	int win = 0;
	int i;
	for(i = 0; i < 8; i++) {
		win = win || (
				board[winPositions[i][0]] == symbol &&
				board[winPositions[i][1]] == symbol &&
				board[winPositions[i][2]] == symbol);
	}
	return win;
}

void printBoard(char board[]) {
	int y, x;
	printf("\n");
	for(y = 0; y < 3; y++) {
		for(x = 0; x < 3; x++) {
			printf("%c", board[y * 3 + x]);
		}
		printf("\n");
	}
}

void part3() {
	char blank = '_';
	char symbol[2] = {'X', 'O'};
	/* num_turns = how many turns have passed */
	/* turn = 0 if player 1's turn, 1 if player 2's turn */
	/* win = 0 if no winners, 1 if player 1 wins, 2 if player 2 wins */
	int x, y, num_turns = 0, turn = 0, win = 0;
	char board[9];
	/* initialize the board to be blank */
	memset(board, blank, 9);
	/* continue until the is a winner or board is full */
	while(!win && num_turns < 9) {
		printf("\nPlayer %d, enter your move (row, col): ", turn + 1);
		scanf("%d%d", &y, &x);
		/* check if move is legal */
		if(x >= 0 && x <= 3 && y >= 0 && y <= 3 && board[y * 3 + x] == blank) {
			/* set the selected space to players symbol */
			board[y * 3 + x] = symbol[turn];
			printBoard(board);
			if(checkWin(board, symbol[turn])) {
				win = turn + 1;
			}
			num_turns++;
			/* switch players turn */
			turn = turn == 1 ? 0 : 1;
		}
		else {
			printf("\nInvalid move.\n");
		}
	}
	if(win) {
		printf("\nPlayer %d wins!\n", win);
	}
	else {
		printf("\nDraw!\n");
	}
}

int main() {
	int input;
	do {
		printf("\nPlease enter number of the part (1-3) or 0 to exit: ");
		scanf("%d", &input);
		switch(input) {
			case 0:
				break;
			case 1:
				part1();
				break;
			case 2:
				part2();
				break;
			case 3:
				part3();
				break;
			default:
				printf("\nInvalid input.\n");
				break;
		}
	}
	while(input);
}
