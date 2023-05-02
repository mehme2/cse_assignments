#include <stdio.h>
#include <math.h>

#define PI 3.141592653589793

void part1();
void part2();

int main() {
	part1();
	part2();
}

void part1() {
	float input[3];
	printf("Enter edge length for cube: ");
	scanf("%f", input);
	printf("Surface Area: %f, ", input[0] * input[0] * 6);
	printf("Volume: %f\n", input[0] * input[0] * input[0]);
	printf("Enter side length for rectangular prism: ");
	scanf("%f", input);
	printf("Enter side width: ");
	scanf("%f", input + 1);
	printf("Enter side height: ");
	scanf("%f", input + 2);
	printf("Surface Area: %f, ", 2 * (input[0] * input[1] + input[0] * input[2] + input[1] * input[2]));
	printf("Volume: %f\n", input[0] * input[1] * input[2]);
	printf("Enter radius for sphere: ");
	scanf("%f", input);
	printf("Surface Area: %f, ", 4 * PI * input[0] * input[0]);
	printf("Volume: %f\n", PI * input[0] * input[0] * input[0] * 4.0 / 3.0);
	printf("Enter radius for cone: ");
	scanf("%f", input);
	printf("Enter the height: ");
	scanf("%f", input + 1);
	printf("Surface Area: %f, ", PI * input[0] * (input[0] + sqrt(input[0] * input[0] + input[1] * input[1])));
	printf("Volume: %f\n", PI * input[0] * input[0] * input[1] * 1.0 / 3.0);
}

void part2() {
	float weight, height, bmi;
	printf("Enter your weight(kilograms): ");
	scanf("%f", &weight);
	printf("Enter your height(meters): ");
	scanf("%f", &height);
	bmi = weight / (height * height);
	printf("Your BMI: %f\n", bmi);
	if(bmi < 18.5)
		printf("BMI < 18.5: You are underweight.\n");
	else if(bmi < 25)
		printf("18.5 < BMI < 25: Your weight is average.\n");
	else if(bmi < 30)
		printf("25 < BMI < 30: You are overweight.\n");
	else
		printf("BMI > 30: You are obese.\n");
}
