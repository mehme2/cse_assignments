#include "util.h"

void part1() {
	printf("\n----------Start of part1----------\n");
	int num[3], median;
	printf("\nEnter 3 integers: ");
	scanf("%d%d%d", num, num + 1, num + 2);
	if(num[0] >= num[1]) {
		if(num[0] >= num[2]) {
			if(num[1] >= num[2]) {
				median = num[1];
			}
			else {
				median = num[2];
			}
		}
		else {
			median = num[0];
		}
	}
	else if(num[0] >= num[2]) {
		median = num[0];
	}
	else if(num[1] >= num[2]) {
		median = num[2];
	}
	else {
		median = num[1];
	}
	printf("\nMedian number is %d.\n", median);
	printf("\n----------End of part1------------\n");
}
void part2() {
	printf("\n----------Start of part2----------\n");
	float scores[3], average;
	printf("\nEnter your scores: ");
	scanf("%f%f%f", scores, scores + 1, scores + 2);
	average = (scores[0] + scores[1] + scores[2]) / 3;
	printf("\nYour letter grades are ");
	for(int i = 0; i < 3; i++) {
		/* index for the letter grades */
		int grade;
		if(scores[i] < 30) {
			grade = 0;
		}
		else {
			grade = ((int)(scores[i]) - 30) / 10;
		}
		switch(grade) {
			case 0:
				printf("F");
				break;
			case 1:
				printf("D");
				break;
			case 2:
				printf("C");
				break;
			case 3:
				printf("B");
				break;
			case 4:
				printf("B+");
				break;
			case 5:
				printf("A");
				break;
			case 6:
			case 7:
				printf("A+");
				break;
		}
		switch(i) {
			case 0:
				printf(", ");
				break;
			case 1:
				printf(" ");
				break;
			case 2:
				printf(" with an %.1f average.\n", average);
				break;
		}
	}
	printf("\n----------End of part2------------\n");
}
void part3() {
	printf("\n----------Start of part3----------\n");
	char op;
	int num1, num2, resIn, resReal;
	printf("\nEnter an arithmetic operation(+,-,*,/): ");
	scanf(" %c", &op);
	printf("\nEnter two numbers: ");
	scanf("%d%d", &num1, &num2);
	printf("\nResult: ");
	scanf("%d", &resIn);
	switch(op) {
		default:
		case '+':
			resReal = num1 + num2;
			break;
		case '-':
			resReal = num1 - num2;
			break;
		case '*':
			resReal = num1 * num2;
			break;
		case '/':
			resReal = num1 / num2;
			break;
	}
	if(resIn == resReal) {
		printf("\nBravo, correct answer!\n");
	}
	else {
		printf("\nWrong answer, try again!\n");
	}
	printf("\n----------End of part3------------\n");
}
