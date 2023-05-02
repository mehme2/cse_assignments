#include <stdio.h>

#define BUF_SIZE 1000

void odd_even_arr(const int in[], int sentinel, char out[]) {
	int i = 0;
	while(in[i] != sentinel) {
		out[i] = in[i] % 2 ? 'o' : 'e';
		i++;
	}
}

void part1() {
	int i, sentinel = -100;
	int in[BUF_SIZE];
	char out[BUF_SIZE];
	for(i = 0; i < BUF_SIZE; i++) {
		scanf("%d", &in[i]);
		if(in[i] == sentinel) {
			break;
		}
		else if(i == BUF_SIZE - 2) {
			printf("\nEnd of buffer.\n");
			in[BUF_SIZE - 1] = sentinel;
			break;
		}
	}
	odd_even_arr(in, sentinel, out);
	for(i = 0; i < BUF_SIZE; i++) {
		if(in[i] == sentinel) {
			break;
		}
		printf("%d %c\n", in[i], out[i]);
	}
}

void part2() {
	char cont, team_sel;
	int size, i, num_team;
	float sal_sum;
	int age[BUF_SIZE];
	char occupation[BUF_SIZE];
	float salary[BUF_SIZE];
	char team[BUF_SIZE];
	FILE *table = fopen("table.txt", "r");
	if(table == NULL) {
		printf("\nAn error occurred while opening table.txt.\n");
	}
	else {
		fscanf(table, "%d", &size);
		if(size > BUF_SIZE) {
			printf("\nSize of table bigger than buffer size.\n");
		}
		else {
			for(i = 0; i < size; i++) {
				fscanf(table, "%d %c%f %c", &age[i], &occupation[i], &salary[i], &team[i]);
				printf("\nEntry %d: age: %d occupation: %c salary: %.2f team: %c\n", i, age[i], occupation[i], salary[i], team[i]);
			}
			cont = 'y';
			while(cont == 'y' || cont == 'Y') {
				printf("\nPlease select a team: ");
				scanf(" %c", &team_sel);
				num_team = 0;
				sal_sum = 0;
				for(i = 0; i < size; i++) {
					if(team[i] == team_sel) {
						num_team++;
						sal_sum += salary[i];
					}
				}
				if(num_team) {
					printf("\nAverage salaries of fans of %c: %f\n", team_sel, sal_sum / num_team);
				}
				else {
					printf("\nThere are no fans of %c in the database!\n", team_sel);
				}
				printf("\nSelect another team? (y/N): ");
				scanf(" %c", &cont);
			}
		}
		fclose(table);
	}
}

int main() {
	part1();
	part2();
}
