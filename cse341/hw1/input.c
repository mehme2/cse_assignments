#include <stdio.h>

int sum(int a, int b);

int sum(int a, int b) {
    return a + b;
}

int main() {
    int x = 10;
    int y = 20;
    int result = sum(x, y);

    if (result > 25) {
        printf("Result is greater than 25\n");
		x = 5;
    }

    int i = 0;
    while(i < 10) if(i >= 0) {
        printf("%d\n", ++i);
    }

    return 0;
}
