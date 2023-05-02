#include "util.h"

void part1() {
	printf("\n------Start of part1------\n");
	int a, b;
	printf("\nFirst number: ");
	scanf("%d", &a);
	printf("\nSecond number: ");
	scanf("%d", &b);
	/* subtract small from big number until both are equal */
	while(a != b) {
		if(a > b)
			a -= b;
		else
			b -= a;
	}
	printf("\nGCD is %d.\n", a);
	printf("\n------End of part1--------\n");
}

void part2() {
	printf("\n------Start of part2------\n");
	int a, b;
	printf("\nFirst number: ");
	scanf("%d", &a);
	printf("\nSecond number: ");
	scanf("%d", &b);
	printf("\nResult:\n\n %4d\n %4d\n+\n-----\n%5d\n", a, b, a + b);
	printf("\n------End of part2--------\n");
}

void part3() {
	printf("\n------Start of part3------\n");
	int a, b, digits[3];
	printf("\nFirst number: ");
	scanf("%d", &a);
	printf("\nSecond number: ");
	scanf("%d", &b);
	/* get value of each decimal digit */
	digits[0] = b % 10;
	digits[1] = (b % 100) / 10.0f;
	digits[2] = (b % 1000) / 100.0f;
	/* multiply with first digit */
	printf("\nResult:\n\n    %3d\n    %3d\n*\n-------\n   %4d\n", a, b, a * digits[0]);
	/* check if second digit is not zero */
	if(digits[1]) {
		printf("  %4d\n", a * digits[1]);
		/* check if third digit is not zero */
		if(digits[2])
			printf(" %4d\n", a * digits[2]);
		/* multiply with all the digits to get the sum */
		/* not necessary if there is only one digit */
		printf("+\n-------\n %6d\n", a * b);
	}
	printf("\n------End of part3--------\n");
}

void part4() {
	printf("\n------Start of part4------\n");
	int a;
	printf("\nEnter a number: ");
	scanf("%d", &a);
	if(a > 10 || a < 1) 
		printf("\nInvalid input.\n");
	else if(a > 5)
		printf("\nThe integer you entered is greater than 5.\n");
	else
		printf("\nThe integer you entered is less than or equal to 5.\n");
	printf("\n------End of part4--------\n");
}
