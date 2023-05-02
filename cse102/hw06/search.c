#include <stdio.h>
#include "search.h"
#include "macros.h"
#include "feature.h"

void listAllProducts() {
	char c;
	FILE* file = fopen(PRODUCTS, "r");
	if(file) {
		printf("\nList of all products:\n\n");
		while(fscanf(file, "%c", &c) != EOF) {
			printf("%c", c);
		}
		fclose(file);
	}
	else {
		printf("\nError opening file.\n", PRODUCTS);
	}
}

void filterProducts() {
	char filter[100];
	char line[500];
	int pID[50];
	char nameFeature[20];
	char valFeature[20];
	int iFilter, i, iPID, noFeature, lastMatch, iLine, noPrice, numProd;
	float price, priceMin, priceMax;
	FILE* file;
	char c;
	noPrice = featureNo(PRODUCTS, "price");
	printf("\nEnter filter string (feature1,val1,feature2,val2...): ");
	scanf("%99s", filter);
	iFilter = 0;
	iPID = 0;
	do {
		sscanf(&filter[iFilter], "%19[^,]", nameFeature);
		while(filter[iFilter] != ',' && filter[iFilter] != '\0') {
			iFilter++;
		}
		noFeature = featureNo(PRODUCTS, nameFeature);
		if(noFeature != noPrice) {
			if(filter[iFilter++] == '\0') {
				break;
			}
			sscanf(&filter[iFilter], "%19[^,]", valFeature);
			while(filter[iFilter] != ',' && filter[iFilter] != '\0') {
				iFilter++;
			}
		}
		else {
			printf("\nEnter min and max ranges: ");
			scanf("%f%f", &priceMin, &priceMax);
		}
		if(iPID) {
			for(i = 0; pID[i] != -1; i++) {
				if(noFeature != noPrice) {
					if(entryGetNext(PRODUCTS, pID[i] - 1, noFeature, valFeature) != pID[i]) {
						for(int j = i; pID[j] != -1; j++) {
							pID[j] = pID[j + 1];
						}
						i--;
					}
				}
				else {
					featureGet(PRODUCTS, pID[i], noPrice, valFeature);
					sscanf(valFeature, "%f", &price);
					if(price < priceMin || price > priceMax) {
						for(int j = i; pID[j] != -1; j++) {
							pID[j] = pID[j + 1];
						}
						i--;
					}
				}
			}
		}
		else {
			if(noFeature != noPrice) {
				for(lastMatch = 0; (lastMatch = pID[iPID] = entryGetNext(PRODUCTS, lastMatch, noFeature, valFeature)) != -1; iPID++);
			}
			else {
				numProd = entryCount(PRODUCTS);
				for(i = 0; i < numProd; i++) {
					featureGet(PRODUCTS, i, noPrice, valFeature);
					sscanf(valFeature, "%f", &price);
					if(price < priceMin || price > priceMax) {
						pID[iPID++] = i;
					}
					else {
						pID[iPID] = -1;
						break;
					}
				}
			}
		}
		if(pID[0] == -1) {
			break;
		}
	}
	while(filter[iFilter++] != '\0');
	file = fopen(PRODUCTS, "r");
	fscanf(file, "%[^\n]%*c", line);
	printf("%s\n", line);
	iLine = 1;
	for(int i = 0; pID[i] != -1; i++) {
		while(iLine++ < pID[i]) {
			fscanf(file, "%*[^\n]%*c");
		}
		fscanf(file, "%[^\n]%*c", line);
		iLine++;
		printf("%s\n", line);
	}
}
