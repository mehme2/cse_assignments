#include "util.h"

void part1() {
	printf("\n----------Start of part1----------\n");
	int year, isLeapYear;
	printf("\nPlease enter a year: ");
	scanf("%d", &year);
	isLeapYear = year % 4 == 0;
	/* Uncomment for true leap years */
	/* isLeapYear = isLeapYear && (year % 100 != 0 || (year % 400 == 0 && year % 4000 != 0)); */
	if(isLeapYear) {
		printf("\n%d is a leap year.\n", year);
	}
	else {
		printf("\n%d is not a leap year.\n", year);
	}
	printf("\n----------End of part1------------\n");
}

void part2() {
	printf("\n----------Start of part2----------\n");
	int m, n;
	float num1, num2, result;
	char operation, format;
	printf("\nEnter the format of output (S or I): ");
	/* Space in format is to skip white spaces in stdin. */
	scanf(" %c", &format);
	if(format == 'S') {
		printf("\nEnter m and n values: ");
		scanf("%d%d", &m, &n);
		if(n >= m) {
			printf("\nn is equal or greater than m, there will be only one digit before decimal point.\n");
		}
	}
	else if(format != 'I') {
		printf("\nInvalid format, using \"I\".\n");
	}
	printf("\nEnter the operation(+,-,/,*,%%,!,^): ");
	scanf(" %c", &operation);
	if(operation != '!') {
		printf("\nEnter the first operand: ");
		scanf("%f", &num1);
		printf("\nEnter the second operand: ");
		scanf("%f", &num2);
	}
	else {
		printf("\nEnter the operand: ");
		scanf("%f", &num1);
	}
	switch(operation) {
		default:
			printf("\nInvalid operation, using \"+\".\n");
			operation = '+';
		case '+':
			result = num1 + num2;
			break;
		case '-':
			result = num1 - num2;
			break;
		case '/':
			result = num1 / num2;
			break;
		case '*':
			result = num1 * num2;
			break;
		case '%':
			/* result = (double)((int)num1 % (int)num2); */
			if(num2 != 0) {
				float num, den;
				num = num1 >= 0 ? num1 : -num1;
				den = num2 >= 0 ? num2 : -num2;
				result = num1;
				while(result > den) {
					result -= den;
				}
				if(num1 < 0) {
					result = -result;
				}
			}
			else {
				result = 0;
			}
			break;
		case '!':
			result = 1;
			for(int i = 2; i <= num1; i++) {
				result *= i;
			}
			break;
		case '^':
			result = pow(num1, num2);
			break;
	}
	printf("\n%.2f %c ", num1, operation);
	if(operation != '!') {
		printf("%.2f ", num2);
	}
	printf("= ");
	if(format == 'S') {
		/* To avoid outputs like 000-1.0000e+00 */
		if (result < 0) {
			printf("-");
			result = -result;
		}
		/* Padding with zeros before decimal point. */
		for(int i = m - n; i > 1; i--) {
			putchar('0');
		}
		printf("%.*e\n", n, result);
	}
	else {
		printf("%d\n", (int)result);
	}
	printf("\n----------End of part2------------\n");
}

void part3() {
	printf("\n----------Start of part3----------\n");
	float exam[3], assignment[2], final;
	printf("\nEnter 3 exam grades of student: ");
	scanf("%f%f%f", exam, exam + 1, exam + 2);
	printf("\nEnter 2 assignment grades of student: ");
	scanf("%f%f", assignment, assignment + 1);
	final = (exam[0] + exam[1] + exam[2]) / 3 * 0.6 + (assignment[0] + assignment[1]) / 2 * 0.4;
	printf("\nFinal Grade: %.1f ", final);
	if(final >= 60) {
		printf("Passed!\n");
	}
	else {
		printf("Failed!\n");
	}
	printf("\n----------End of part3------------\n");
}
