#include <stdio.h>
#include "userio.h"

int selectionGet(int min, int max, const char options[], const char prompt[]) {
	int input;
	printf(options);
	while(1) {
		printf(prompt);
		if(!scanf("%*[^-0-9]%d", &input)) {
			scanf("%d", &input);
		}
		if(input >= min && input <= max) {
			break;
		}
		else {
			printf("\nInvalid input.\n");
		}
	}
	return input;
}
