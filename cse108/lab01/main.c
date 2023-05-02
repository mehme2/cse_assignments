#include <stdio.h>

void part1();
void part2();
void part3();

int main() {
	part1();
	part2();
	part3();
}

void part1() {
	printf("\n---------------Start of part 1---------------\n");
	int input;
	printf("Please enter an integer between 1 and 100: ");
	scanf("%d", &input);
	printf("\n");
	if(input % 3 == 0)
		printf("Fizz");
	if(input > 5 && input < 50)
		printf("Buzz");
	printf("\n");
	printf("\n---------------End of part 1-----------------\n");
}

void part2() {
	printf("\n---------------Start of part 2---------------\n");
	float x, y, z, max;
	printf("\nPlease value of X: ");
	scanf("%f", &x);
	printf("\nPlease value of Y: ");
	scanf("%f", &y);
	max = x > y ? x : y;
	z = x / y + max / (x + y);
	printf("\nZ= %f\n", z);
	printf("\n---------------End of part 2-----------------\n");
}

void part3() {
	printf("\n---------------Start of part 3---------------\n");
	static const char table[3][2][6] = {
		/* 0-10     >10 */
		{"10000", "Error"}, /* <20 */
		{"15000", "20000"}, /* 20-50 */
		{"20000", "25000"}, /* >50 */
	};
	int age, exp;
	printf("\nPick your age:\n1) <20\n2) 20 - 50\n3) >50\n\nEnter(1-3): ");
	scanf("%d", &age);
	printf("\nPick your years of experience:\n1) 0 - 10\n2) >10\n\nEnter(1-2): ");
	scanf("%d", &exp);
	if(exp >= 1 && exp <= 2 && age >= 1 && age <= 3)
		printf("\nYour salary: %s\n", table[age - 1][exp - 1]);
	else
		printf("\nInvalid age or experience.\n");
	printf("\n---------------End of part 3-----------------\n");
}
