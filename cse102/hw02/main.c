#include "util.h"

int main() {
	int input;
	part1();
	part2();
	part3();
	do {
		printf("\nEnter (1-3) to repeat or anything else to quit: ");
		scanf("%d", &input);
		switch(input) {
			case 1:
				part1();
				break;
			case 2:
				part2();
				break;
			case 3:
				part3();
				break;
		}
	}
	while(input >= 1 && input <= 3);
}
