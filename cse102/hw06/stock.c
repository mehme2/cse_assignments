#include <stdio.h>
#include "stock.h"
#include "macros.h"
#include "feature.h"

void checkStock() {
	char pIDStr[10];
	char branch[16];
	char stockStr[10];
	char name[9];
	int pIDInt, numProd, sID, noPID, noBranch, stock;
	printf("\nEnter pID: ");
	scanf("%d", &pIDInt);
	numProd = entryCount(PRODUCTS);
	if(pIDInt <= 0 || pIDInt > numProd) {
		printf("\nCould not match pID %d.\n", pIDInt);
	}
	else {
		noPID = featureNo(STOCKS, "pID");
		noBranch = featureNo(STOCKS, "branch");
		printf("\nEnter branch: ");
		scanf("%15s", branch);
		sprintf(pIDStr, "%d", pIDInt);
		sID = 0;
		while((sID = entryGetNext(STOCKS, sID, noPID, pIDStr)) != -1) {
			if(entryGetNext(STOCKS, sID - 1, noBranch, branch) == sID) {
				break;
			}
		}
		if(sID == -1) {
			printf("\nCould not find a stock entry using given inputs.\n");
			stock = 0;
		}
		else {
			featureGet(STOCKS, sID, featureNo(STOCKS, "current_stock"), stockStr);
			sscanf(stockStr, "%d", &stock);
		}
		featureGet(PRODUCTS, pIDInt, featureNo(PRODUCTS, "name"), name);
		printf("\nCurrent stock of product %s in %s: %d\n", name, branch, stock);
	}
}

void listBranch() {
	char line[500];
	char branch[16];
	char pIDStr[10];
	char stock[10];
	int sID, noBranch, noPID, noStock, pIDInt, iProd;
	FILE* file = fopen(PRODUCTS, "r");
	if(file) {
		noBranch = featureNo(STOCKS, "branch");
		noPID = featureNo(STOCKS, "pID");
		noStock = featureNo(STOCKS, "current_stock");
		printf("\nEnter branch: ");
		scanf("%15s", branch);
		fscanf(file, "%[^\n]%*c", &line);
		printf("\n%s,current_stock\n", line);
		for(sID = entryGetNext(STOCKS, 0, noBranch, branch); sID != -1; sID = entryGetNext(STOCKS, sID, noBranch, branch)) {
			featureGet(STOCKS, sID, noPID, pIDStr);
			sscanf(pIDStr, "%d", &pIDInt);
			fseek(file, 0, SEEK_SET);
			for(iProd = 0; iProd++ < pIDInt; fscanf(file, "%*[^\n]%*c"));
			fscanf(file, "%[^\n]", line);
			featureGet(STOCKS, sID, noStock, stock);
			printf("%s,%s\n", line, stock);
		}
		fclose(file);
	}
	else {
		printf("\nError opening file %s.\n", PRODUCTS);
	}
}

void listOutOfStock() {
	char line[500];
	char branch[16];
	char pIDStr[10];
	char stockStr[10];
	int noPID, noBranch, noStock, numProd, iProd, sID, stock;
	FILE* file = fopen(PRODUCTS, "r");
	if(file) {
		noPID = featureNo(STOCKS, "pID");
		noBranch = featureNo(STOCKS, "branch");
		noStock = featureNo(STOCKS, "current_stock");
		numProd = entryCount(PRODUCTS);
		printf("\nEnter branch: ");
		scanf("%15s", branch);
		fscanf(file, "%[^\n]%*c", line);
		printf("\n%s\n", line);
		for(iProd = 1; iProd <= numProd; iProd++) {
			sprintf(pIDStr, "%d", iProd);
			sID = 0;
			while((sID = entryGetNext(STOCKS, sID, noPID, pIDStr)) != -1) {
				if(entryGetNext(STOCKS, sID - 1, noBranch, branch) == sID) {
					break;
				}
			}
			if(sID == -1) {
				fscanf(file, "%[^\n]%*c", line);
				printf("%s\n", line);
			}
			else {
				featureGet(STOCKS, sID, noStock, stockStr);
				sscanf(stockStr, "%d", &stock);
				if(stock) {
					fscanf(file, "%*[^\n]%*c");
				}
				else {
					fscanf(file, "%[^\n]%*c", line);
					printf("%s\n", line);
				}
			}
		}
	}
	else {
		printf("\nError opening file %s.\n", PRODUCTS);
	}
}
