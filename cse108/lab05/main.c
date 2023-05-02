#include <stdio.h>

void part1() {
	int option = -1;
	float temp, conv;
	while(option != 3) {
		printf("\nTemperature Conversion Menu\n1. Convert Celsius to Fahrenheit\n2. Convert Fahrenheit to Celsius\n3. Quit\n");
		printf("\nEnter your choice (1-3): ");
		scanf("%d", &option);
		switch(option) {
			case 1:
				printf("\nEnter temperature value to convert: ");
				scanf("%f", &temp);
				conv = temp * 9.0 / 5.0 + 32;
				printf("\n%.2f Celsius = %.2f Fahrenheit\n", temp, conv);
				break;
			case 2:
				printf("\nEnter temperature value to convert: ");
				scanf("%f", &temp);
				conv = 5.0 / 9.0 * (temp - 32);
				printf("\n%.2f Fahrenheit = %.2f Celsius\n", temp, conv);
				break;
			case 3:
				break;
			default:
				printf("\nInvalid input.\n");
				break;
		}
	}
}

void part2() {
	int num, reversed = 0, digit, i;
	printf("\nEnter a number (3, 4 or 5 digits): ");
	scanf("%d", &num);
	for(digit = 1; digit * 10 < num; digit *= 10);
	if(digit >= 100 && digit <= 10000) {
		for(i = 1; digit >= 1; digit /= 10, i *= 10) {
			reversed += i * ((num / digit) % 10);
		}
		printf("\nReversed number: %d\n", reversed);
	}
	else {
		printf("\nDigits should be 3, 4 or 5.\n");
	}
}

int convert_base(int num, int base) {
	int num_abs, digit, digit_base, sum = 0;
	num_abs = num < 0 ? -num : num;
	for(digit = 1, digit_base = 1; digit < num; digit *= 10, digit_base *= base) {
		sum += digit_base * ((num_abs / digit) % 10);
	}
	if(num < 0) {
		sum = -sum;
	}
	return sum;
}

void part3() {
	int option = -1, num;
	while(option != 2) {
		printf("\nMenu\n1. Convert a number to decimal, binary, octal and hexadecimal\n2. Quit\n");
		printf("\nEnter your choice: ");
		scanf("%d", &option);
		switch(option) {
			case 1:
				printf("\nEnter an number: ");
				scanf("%d", &num);
				printf("\nDecimal equivalent: %d\n", num);
				printf("\nBinary equivalent: %d\n", convert_base(num, 2));
				printf("\nOctal equivalent: %d\n", convert_base(num, 8));
				printf("\nHexadecimal equivalent: %d\n", convert_base(num, 16));
				break;
			case 2:
				break;
			default:
				printf("\nInvalid input.\n");
				break;
		}
	}
}

int main() {
	part1();
	part2();
	part3();
}
