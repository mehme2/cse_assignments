#include <stdio.h>

#define MAX_STR_BUF_SIZE 100

int min_number_substring(const char str[]) {
	char deleted[MAX_STR_BUF_SIZE];
	int i, si, status, di, min, tries;
	char c;
	min = MAX_STR_BUF_SIZE;
	for(i = 0; str[i] != '\0' && i < MAX_STR_BUF_SIZE; i++) {
		c = str[i];
		status = 0;
		di = 0;
		for(si = 0; str[si] != '\0' && si < MAX_STR_BUF_SIZE; si++) {
			switch(status) {
				case 0:
					if(si >= i && str[si] == c) {
						status = 1;
					}
					else {
						deleted[di++] = str[si];
					}
					break;
				case 1:
					if(str[si] != c) {
						status = 2;
						deleted[di++] = str[si];
					}
					else {
						i++;
					}
					break;
				case 2:
					deleted[di++] = str[si];
					break;
			}
		}
		deleted[di] = '\0';
		if(di == 0) {
			min = 1;
			break;
		}
		else {
			tries = 1 + min_number_substring(deleted);
			min = min > tries ? tries : min;
		}
	}
	return min;
}

void filter_string(const char str[], const char rule[], char out[]) {
	int si, ri, oi, wi;
	oi = 0;
	for(si = 0; str[si] != '\0'; si++) {
		ri = 0;
		for(wi = si; str[wi] != ' ' && str[wi] != '\0'; wi++) {
			switch(rule[ri]) {
				case '*':
					if(rule[ri + 1] == '\0') {
						ri++;
					}
					else if(rule[ri + 1] == str[wi + 1]) {
						ri++;
					}
					break;
				case '?':
					ri++;
					break;
				case '\0':
					break;
				default:
					if(str[wi] == rule[ri]) {
						ri++;
					}
					else {
						ri = 0;
					}
					break;
			}
		}
		if(rule[ri] != '\0') {
			while(si <= wi) {
				out[oi++] = str[si++];
			}
		}
		si = wi;
	}
	out[oi] = '\0';
}

int main() {
	char str[MAX_STR_BUF_SIZE];
	char rule[MAX_STR_BUF_SIZE];
	char out[MAX_STR_BUF_SIZE];
	printf("\nEnter a string: ");
	scanf("%s", str);
	printf("\nMin number of substings to delete: %d\n", min_number_substring(str));
	printf("\nEnter a string: ");
	scanf(" %[^\n]", str);
	printf("\nEnter rule: ");
	scanf("%s", rule);
	filter_string(str, rule, out);
	printf("\nFiltered: %s\n", out);
}
