#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>

typedef struct {
	int id, age;
	float gpa;
} student;

int *generateArray(int size) {
	int *arr;
	int i;
	if(size <= 0) {
		printf("\ngenerateArray: Size of the array should be a positive integer.\n");
		arr = NULL;
	}
	else {
		arr = calloc(size, sizeof(int));
		printf("\nEnter %d integers: ", size);
		for(i = 0; i < size; i++) {
			scanf("%d", arr + i);
		}
	}
	return arr;
}

void printArray(int *arr, int size) {
	for(int i = 0; i < size; i++) {
		printf("%d ", arr[i]);
	}
}

float averageGPA(int size) {
	student *arr;
	int i;
	float sum, average;
	if(size <= 0) {
		printf("\naverageGPA: Size of the array should be a positive integer.\n");
		average = -1;
	}
	else {
		arr = calloc(size, sizeof(student));
		sum = 0;
		for(i = 0; i < size; i++) {
			arr[i].id = rand() % INT_MAX;
			arr[i].age = rand() % 16 + 18;
			arr[i].gpa = ((float)rand() / (float)INT_MAX) * 4;
			sum += arr[i].gpa;
		}
		average = sum / (float)size;
		free(arr);
	}
	return average;
}

int main() {
	int *arr1, *arr2, *sumArr;
	int size, i, min, sum;
	printf("\nStart of Part 1\n");
	do {
		printf("\nEnter size of the array: ");
		scanf("%d", &size);
		arr1 = generateArray(size);
	} while(arr1 == NULL);
	for(i = 0, min = INT_MAX; i < size; i++) {
		min = arr1[i] < min ? arr1[i] : min;
	}
	printf("\nMin of the array elements: %d\n", min);
	printf("\nStart of Part 2\n");
	do {
		printf("\nEnter size of the array: ");
		scanf("%d", &size);
		arr2 = generateArray(size);
	} while(arr2 == NULL);
	sumArr = calloc(size, sizeof(int));
	for(i = sum = 0; i < size; i++) {
		sum += arr2[i];
		sumArr[i] = sum;
	}
	printf("\nFirst array: ");
	printArray(arr2, size);
	printf("\n\nSecond array (culmulative sum): ");
	printArray(sumArr, size);
	printf("\n\nStart of Part 3\n");
	srand(time(NULL));
	printf("\nAverage GPA of 10000 students: %.2f\n", averageGPA(10000));
	free(arr1);
	free(arr2);
	free(sumArr);
}
