#include <stdio.h>
#include "feature.h"

int featureCount(const char filename[]) {
	int count = -1;
	char c;
	FILE* file = fopen(filename, "r");
	if(file) {
		count = 0;
		do{
			fscanf(file, "%*[^\n,]");
			fscanf(file, "%c", &c);
			count++;
		}
		while(c == ',');
		fclose(file);
	}
	else {
		printf("\nError opening file: %s\n", filename);
	}
	return count;
}

int featureName(const char filename[], int featureNo, char out[]) {
	int i, status;
	FILE* file = fopen(filename, "r");
	if(file) {
		for(i = 0; i < featureNo; i++) {
			fscanf(file, "%*[^\n,]");
			fscanf(file, ",");
		}
		out[0] = '\0';
		status = fscanf(file, "%[^\n,]", out);
		fclose(file);
	}
	else {
		printf("\nError opening file: %s\n", filename);
		status = -1;
	}
	return status;
}

int featureNo(const char filename[], const char featureName[]) {
	char format[50];
	char c;
	int no, i;
	FILE* file = fopen(filename, "r");
	if(file) {
		no = 0;
		sprintf(format, featureName);
		for(i = 0; format[i] != '\0'; i++);
		sprintf(&format[i], "%%1[,\n]");
		while(!fscanf(file, format, format)) {
			fscanf(file, "%*[^\n,]");
			fscanf(file, "%c", &c);
			if(c == '\n') {
				no = -1;
				break;
			}
			no++;
		}
		fclose(file);
	}
	else {
		printf("\nError opening file: %s\n", filename);
		no = -1;
	}
	return no;
}

int featureGet(const char filename[], int entry, int featureNo, char out[]) {
	int status, i;
	FILE* file = fopen(filename, "r");
	if(file) {
		out[0] = '\0';
		for(i = 0; i < entry; i++) {
			fscanf(file, "%*[^\n] ");
		}
		for(i = 0; i < featureNo; i++) {
			fscanf(file, "%*[^,]");
			fscanf(file, ",");
		}
		status = fscanf(file, "%[^\n,]", out);
		fclose(file);
	}
	else {
		status = -1;
		printf("\nError opening file: %s\n", filename);
	}
	return status;
}

int featureSet(const char filename[], int entry, int featureNo, const char in[]) {
	int status, size, i, start, end;
	char fileBuf[10000];
	FILE* file = fopen(filename, "r");
	if(file) {
		for(i = 0; fscanf(file, "%c", &fileBuf[i]) != EOF; i++);
		fileBuf[i] = '\0';
		start = 0;
		for(i = 0; i < entry; i++) {
			while(fileBuf[start++] != '\n');
		}
		for(i = 0; i < featureNo; i++) {
			while(fileBuf[start++] != ',');
		}
		for(end = start; fileBuf[end] != ',' && fileBuf[end] != '\n'; end++);
		fclose(file);
		file = fopen(filename, "w");
		fprintf(file, "%.*s", start, fileBuf);
		status = fprintf(file, "%s", in);
		fprintf(file, "%s", &fileBuf[end]);
		fclose(file);
	}
	else {
		status = -1;
		printf("\nError opening file: %s\n", filename);
	}
	return status;
}

int featureAdd(const char filename[], const char featureName[], const char defVal[]) {
	char fileBuf[10000];
	int i;
	int status = -1;
	FILE* file = fopen(filename, "r");
	if(file) {
		for(i = 0; fscanf(file, "%c", &fileBuf[i]) != EOF; i++);
		fileBuf[i] = '\0';
		fclose(file);
		file = fopen(filename, "w");
		for(i = 0; fileBuf[i] != '\n' && fileBuf[i] != '\0'; i++) {
			fprintf(file, "%c", fileBuf[i]);
		}
		fprintf(file, ",%s\n", featureName);
		for(i++; fileBuf[i] != '\0'; i++) {
			if(fileBuf[i] == '\n') {
				fprintf(file, ",");
				status = fprintf(file, "%s", defVal);
			}
			fprintf(file, "%c", fileBuf[i]);
		}
		fclose(file);
	}
	else {
		printf("\nError opening file: %s\n", filename);
	}
	return status;
}

int entryCount(const char filename[]) {
	int count = -1;
	char c;
	FILE* file = fopen(filename, "r");
	if(file) {
		do{
			fscanf(file, "%*[^\n]%*c");
			count++;
		}
		while(fscanf(file, "%c", &c) != EOF);
		fclose(file);
	}
	else {
		printf("\nError opening file: %s\n", filename);
	}
	return count;
}

int entryAdd(const char filename[], const char defVal[]) {
	int status, count, i;
	count = featureCount(filename);
	FILE* file = fopen(filename, "a");
	if(file) {
		for(i = 0; i < count; i++) {
			status = fprintf(file, "%s", defVal);
			fprintf(file, "%c", i == count - 1 ? '\n' : ',');
		}
		fclose(file);
	}
	else {
		status = -1;
		printf("\nError opening file: %s\n", filename);
	}
	return status;
}

int entryDelete(const char filename[], int n) {
	char fileBuf[10000];
	int status, i, start, end;
	FILE* file = fopen(filename, "r");
	if(file) {
		status = 0;
		for(i = 0; fscanf(file, "%c", &fileBuf[ftell(file)]) != EOF; i++);
		fileBuf[i] = '\0';
		fclose(file);
		file = fopen(filename, "w");
		for(i = 0, start = 0; i < n; i++) {
			while(fileBuf[start++] != '\n');
		}
		end = start;
		while(fileBuf[end++] != '\n');
		fprintf(file, "%.*s", start, fileBuf);
		fprintf(file, "%s", &fileBuf[end]);
		fclose(file);
	}
	else {
		status = -1;
		printf("\nError opening file: %s\n", filename);
	}
	return status;
}

int entryGetNext(const char filename[], int start, int featureNo, const char featureVal[]) {
	char format[50];
	int no, i, count;
	FILE* file;
	count = entryCount(filename);
	if(count != -1) {
		file = fopen(filename, "r");
		sprintf(format, "%s", featureVal);
		for(i = 0; format[i] != '\0'; i++);
		sprintf(&format[i], "%s", "%1[,\n]");
		for(no = 0; no < start; no++) {
			fscanf(file, "%*[^\n]");
			fscanf(file, "\n");
		}
		do {
			no++;
			if(no > count) {
				no = -1;
				break;
			}
			fscanf(file, "%*[^\n]");
			fscanf(file, "\n");
			for(i = 0; i < featureNo; i++) {
				fscanf(file, "%*[^,]");
				fscanf(file, ",");
			}
		}
		while(!fscanf(file, format, format));
		fclose(file);
	}
	else {
		no = -1;
	}
	return no;
}

int fileCreate(const char filename[], const char feature0[]) {
	int status;
	FILE* file = fopen(filename, "w");
	if(file) {
		status = fprintf(file, "%s", feature0);
		fclose(file);
	}
	else {
		status = -1;
		printf("\nError opening file: %s\n", filename);
	}
	return status;
}
