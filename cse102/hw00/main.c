#include <stdio.h>
#include "util.h"


int main() {

    /* A fractional number: 13/7 */
    int num1 = 13, den1 = 7;
    /* A fractional number: 13/7 */
    int num2 = 30, den2 = 11;
    /* An unitilized fractional number */
    int num3, den3;

    printf("first number: ");
    fraction_print(num1, den1);
    printf("\n");

    printf("second number: ");
    fraction_print(num2, den2);
    printf("\n");

    printf("Addition: ");
    fraction_add(num1, den1, num2, den2, &num3, &den3);
    fraction_print(num3, den3);
    printf("\n");

    printf("Subtraction: ");
    fraction_sub(num1, den1, num2, den2, &num3, &den3);
    fraction_print(num3, den3);
    printf("\n");

    printf("Multiplication: ");
    fraction_mul(num1, den1, num2, den2, &num3, &den3);
    fraction_print(num3, den3);
    printf("\n");

    printf("Division: ");
    fraction_div(num1, den1, num2, den2, &num3, &den3);
    fraction_print(num3, den3);
    printf("\n");

    int run = 1;
    int num_new, den_new;
    printf("\nOptions:\n0 - Exit\n1 - Set numbers.\n2 - Add.\n3 - Subtract.\n4 - Multiply\n5 - Divide.\n");
    while (run) {
        int input = -1;

        printf("\nSelect an option: ");
        scanf("%d", &input);
	printf("\n");

	switch(input) {
            case 0:
                /*Exit*/
                run = 0;
                break;
            case 1:
                /*Set numbers*/
                printf("Select number(1-2): ");
                scanf("%d", &input);
		printf("\n");
                if (input == 1 || input == 2) {
                    printf("Enter numerator: ");
                    scanf("%d", &num_new);
                    printf("\nEnter denominator: ");
                    scanf("%d", &den_new);
                    printf("\n");
                    if(den_new == 0) {
                        printf("Denominator cannot be zero.\n");
                    }
                    else {
                        if(input == 1) {
                            num1 = num_new;
                            den1 = den_new;
                        }
                        else {
                            num2 = num_new;
                            den2 = den_new;
                        }
                    }
                }
                else {
                    printf("Invalid input.\n");
                }
                printf("first number: ");
                fraction_print(num1, den1);
                printf("\n");
                printf("second number: ");
                fraction_print(num2, den2);
                printf("\n");

                break;
            case 2:
                /*Add*/ 
                printf("Addition: ");
                fraction_add(num1, den1, num2, den2, &num3, &den3);
                fraction_print(num3, den3);
                printf("\n");
                break;
            case 3:
                /*Subtract*/
                printf("Subtraction: ");
                fraction_sub(num1, den1, num2, den2, &num3, &den3);
                fraction_print(num3, den3);
                printf("\n");
                break;
            case 4:
                /*Multiply*/
                printf("Multiplication: ");
                fraction_mul(num1, den1, num2, den2, &num3, &den3);
                fraction_print(num3, den3);
                printf("\n");
                break;
            case 5:
                /*Divide*/
                printf("Division: ");
                fraction_div(num1, den1, num2, den2, &num3, &den3);
                fraction_print(num3, den3);
                printf("\n");
                break;
            default:
                printf("Invalid input.\n");
                break;
        }
    }

    return(0);
}
