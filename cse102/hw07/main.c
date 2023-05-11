#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#define MAX_WORD_COUNT 100
#define MAX_WORD_SIZE  30

#define DICTIONARY "dictionary.txt"
#define INPUT "input.txt"
#define IGNORE "ignore.txt"

int read_dict(const char * file_name, char dict[][MAX_WORD_SIZE]) {
	int status, i;
	FILE * fp = fopen(file_name, "r");
	if(fp) {
		/* skip first line */
		fscanf(fp, "%*[^\n]\n");
		for(i = 0; fscanf(fp, "%29s%*[^\n] ", dict[i]) != EOF; i++);
		/* set the last one to sentinel */
		dict[i][0] = '-';
		dict[i][1] = '\0';
		fclose(fp);
		status = i;
	}
	else {
		printf("\nAn error occurred while opening file %s.\n", file_name);
		status = -1;
	}
	return status;
}

int read_text(const char * text_file, const char * ignore_file, char words[][MAX_WORD_SIZE]) {
	char ignore[50][MAX_WORD_SIZE];
	int status, i, j, ni;
	FILE * fp = fopen(text_file, "r");
	if(fp) {
		/* number of ignore words */
		ni = 0;
		if(ignore_file) {
			ni = read_text(ignore_file, NULL, ignore);
		}
		for(i = 0; fscanf(fp, "%[a-zA-Z]%*[^a-zA-Z]", words[i]) != EOF; i++) {
			for(j = 0; j < ni; j++) {
				if(strcmp(words[i], ignore[j]) == 0) {
					i--;
					break;
				}
			}
		}
		words[i][0] = '-';
		words[i][1] = '\0';
		fclose(fp);
		status = i;
	}
	else {
		printf("\nAn error occurred while opening file %s.\n", text_file);
		status = -1;
	}
	return status;
}

double dissimilarity(const char * w1, const char * w2, const char dict[][MAX_WORD_SIZE], float treshold) {
	double res, *vec1, *vec2, diff;
	int dictSize, vectorSize, wi1, wi2, i, j;
	FILE* fp = fopen(DICTIONARY, "r");
	if(fp) {
		fscanf(fp, "%*[^0-9]%d%*[^0-9]%d%*[^\n]", &dictSize, &vectorSize);
		vec1 = malloc(sizeof(double) * vectorSize);
		vec2 = malloc(sizeof(double) * vectorSize);
		/* set index of w1 and w2 to error value */
		wi1 = wi2 = -1;
		/* try to find indices of w1 and w2 in dict */
		/* if duplicates exists in dict the last one is selected */
		for(i = 0; i < dictSize; i++) {
			if(!strcmp(w1, dict[i])) {
				wi1 = i;
			}
			if(!strcmp(w2, dict[i])) {
				wi2 = i;
			}
		}
		/* if both words are found */
		/* read the vector values to the allocated arrays */
		if(wi1 != -1 && wi2 != -1) {
			for(i = 0; i < dictSize; i++) {
				fscanf(fp, "%*[^\n]");
				fscanf(fp, "\n");
				if(wi1 == i) {
					fscanf(fp, "%*[^0-9]");
					for(j = 0; j < vectorSize; j++) {
						fscanf(fp, "%lf", vec1 + j);
					}
					if(wi1 == wi2) {
						for(j = 0; j < vectorSize; j++) {
							vec2[i] = vec1[i];
						}
						break;
					}
				}
				if(wi2 == i) {
					fscanf(fp, "%*[^0-9]");
					for(j = 0; j < vectorSize; j++) {
						fscanf(fp, "%lf", vec2 + j);
					}
					if(wi1 == wi2) {
						for(j = 0; j < vectorSize; j++) {
							vec1[i] = vec2[i];
						}
						break;
					}
				}
				if(wi1 <= i && wi2 <= i) {
					break;
				}
			}
			/* sum of squared differences of all elements of vectors */
			res = 0;
			for(i = 0; i < vectorSize; i++) {
				diff = vec1[i] - vec2[i];
				res += diff * diff;
			}
			/* assign error value if sum is bigger than treshold */
			/* 1 / -1 will always be smallest similarity */
			res = res > (treshold * treshold) ? -1 : sqrt(res);
		}
		/* if one or both of the words are not found */
		else {
			res = -1;
		}
		free(vec1);
		free(vec2);
		fclose(fp);
	}
	else {
		res = -1;
		printf("\nAn error occurred while opening file %s.\n", DICTIONARY);
	}
	return res;
}

int histogram(const char words[][MAX_WORD_SIZE], const int occurrences[], char hist[][2*MAX_WORD_SIZE+7+20]) {
	int scale, i, end, maxlen, maxoccur;
	for(i = 0, maxlen = 0, maxoccur = 0; words[i][0] != '-'; i++) {
		/* if another similar word is found and occurrence is not zero */
		/* extend the word with arrow like in the example */
		if(occurrences[i] && strcmp(words[i], hist[i] + 1)) {
			/* find the index of null terminator */
			for(end = 0; hist[i][end] != '\0'; end++);
			sprintf(hist[i] + end, "->%s", words[i]);
		}
		for(end = 0; hist[i][end] != '\0'; end++);
		/* close the word with \" */
		sprintf(hist[i] + end, "\"   ");
		/* increase the end point with extra characters added */
		end += 4;
		/* find the longest entry name to pad others with space */
		maxlen = end > maxlen ? end : maxlen;
		/* find the maximum number of occurences to get scale */
		maxoccur = occurrences[i] > maxoccur ? occurrences[i] : maxoccur;
	}
	scale = maxoccur / 20;
	for(i = 0; hist[i][0] != '-'; i++) {
		/* if scale == 0 > scale = 1 */
		/* if maxoccur == 0 > only print NO MATCHES */
		int start, numprint = scale ? maxoccur ? occurrences[i] * 20 / maxoccur : 0 : occurrences[i];
		char c = '*';
		/* get index of final words null terminator */
		for(end = 0; words[i][end] != '\0'; end++);
		/* if word is extended with arrow */
		if(hist[i][end + 1] != '"') {
			c = '+';
		}
		/* get to the end of entry */
		for(; hist[i][end] != '\0'; end++);
		/* pad with spaces if not the longest entry */
		while(end < maxlen) {
			sprintf(hist[i] + end++, " ");
		}
		/* if there are matches in input file */
		if(occurrences[i]) {
			start = end;
			/* print the line */
			while(end - start < numprint) {
				sprintf(hist[i] + end++, "%c", c);
			}
		}
		/* only print NO MATHCES if there are no occurrences */
		else {
			sprintf(hist[i] + end, "NO MATCHES");
		}
	}
	return scale;
}

/* make every word in an array lowercase */
void makeLowercase(char arr[][MAX_WORD_SIZE]) {
	int i, iw;
	for(i = 0; arr[i][0] != '-'; i++) {
		for(iw = 0; arr[i][iw] != '\0'; iw++) {
			if(arr[i][iw] >= 'A' && arr[i][iw] <= 'Z') {
				arr[i][iw] += 'a' - 'A';
			}
		}
	}
}

/* get the word with highest similarity value that is not itself */
int findClosest(const char *word, const char dict[][MAX_WORD_SIZE], float treshold) {
	int i, closest = -1;
	double dis, sim, max_sim = 1 / treshold;
	for(i = 0; dict[i][0] != '-'; i++) {
		if(strcmp(word, dict[i])) {
			dis = dissimilarity(word, dict[i], dict, treshold);
			sim = dis ? 1 / dis : 99999999;
			if(sim > max_sim) {
				max_sim = sim;
				closest = i;
			}
		}
	}
	return closest;
}

/* get number of lines in a file */
int lineCount(const char *filename) {
	int res;
	FILE* fp = fopen(filename, "r");
	if(fp) {
		for(res = 0; fscanf(fp, "%*[^\n]\n") != EOF; res++);
		fclose(fp);
	}
	else {
		res = -1;
	}
	return res;
}

/* get number of words in a file */
int wordCount(const char *filename) {
	int res;
	FILE* fp = fopen(filename, "r");
	if(fp) {
		for(res = 0; fscanf(fp, "%*s") != EOF; res++);
		fclose(fp);
	}
	else {
		res = -1;
	}
	return res;
}

/* get number of matching words in an array */
int wordOccurrences(const char *word, const char arr[][MAX_WORD_SIZE]) {
	int count = 0;
	for(int i = 0; arr[i][0] != '-'; i++) {
		if(!strcmp(word, arr[i])) {
			count++;
		}
	}
	return count;
}

int main(int argc, char *argv[]) {
	int i, scale;
	char userIn[MAX_WORD_COUNT + 1][MAX_WORD_SIZE];
	char hist[MAX_WORD_COUNT + 1][2 * MAX_WORD_SIZE + 7 + 20];
	int occurrences[MAX_WORD_COUNT];
	char (*dict)[MAX_WORD_SIZE] = malloc(MAX_WORD_SIZE * lineCount(DICTIONARY));
	char (*input)[MAX_WORD_SIZE] = malloc(MAX_WORD_SIZE * wordCount(DICTIONARY));
	read_dict(DICTIONARY, dict);
	read_text(INPUT, IGNORE, input);
	makeLowercase(dict);
	makeLowercase(input);
	printf("\nEnter word(s): ");
	for(i = 0; i < MAX_WORD_COUNT; i++) {
		scanf("%*[^a-zA-Z\n]");
		if(!scanf("%29[a-zA-Z]", userIn[i])) {
			break;
		}
	}
	userIn[i][0] = '-';
	userIn[i][1] = '\0';
	makeLowercase(userIn);
	switch(i) {
		case 0:
			printf("\nNo words given.\n");
			break;
		case 1:
			/* if there are matches with user input and input file */
			if((occurrences[0] = wordOccurrences(userIn[0], input))) {
				printf("\"%s\" appears in \"%s\" %d times.\n", userIn[0], INPUT, occurrences[0]);
			}
			else {
				int c;
				printf("\"%s\" doesn't appear in \"%s\"",userIn[0], INPUT);
				/* if another close word is found */
				if((c = findClosest(userIn[0], dict, 7)) != -1) {
					/* if matches with closest word is found */
					if((occurrences[0] = wordOccurrences(dict[c], input))) {
						printf(" but \"%s\" appears %d times.\n", dict[c], occurrences[0]);
					}
					else {
						printf(".\n");
					}
				}
				else {
					printf(".\n");
				}
			}
			break;
		default:
			for(i = 0; userIn[i][0] != '-'; i++) {
				/* make the first char of the entry \" to enclose it */
				sprintf(hist[i], "\"%s", userIn[i]);
				if(!(occurrences[i] = wordOccurrences(userIn[i], input))) {
					int c;
					/* another close word is found and matches exist */
					if((c = findClosest(userIn[i], dict, 7)) != -1 &&
							(occurrences[i] = wordOccurrences(dict[c], input))) {
						/* switch the word in user input array with the new one */
						sprintf(userIn[i], "%s", dict[c]);
					}
				}
			}
			hist[i][0] = '-';
			hist[i][1] = '\0';
			scale = histogram(userIn, occurrences, hist);
			if(scale > 1) {
				printf("Scale: %d\n", scale);
			}
			for(i = 0; hist[i][0] != '-'; i++) {
				printf("%s\n", hist[i]);
			}
			break;
	}
	free(dict);
	free(input);
}
