#include <stdio.h>
#include "fileOps.h"
#include "macros.h"
#include "feature.h"

void addNewProduct() {
	int numProd, i, line;
	char type[2] = {'\0', '\0'};
	char pidStr[10];
	char name[9];
	char brand[6];
	char price[20];
	line = entryCount(PRODUCTS) + 1;
	sprintf(pidStr, "%d", line);
	entryAdd(PRODUCTS, "None");
	featureSet(PRODUCTS, line, featureNo(PRODUCTS, "pID"), pidStr);
	printf("\nEnter type: ");
	scanf(" %c", type);
	featureSet(PRODUCTS, line, featureNo(PRODUCTS, "type"), type);
	printf("\nEnter name: ");
	scanf("%8s", name);
	featureSet(PRODUCTS, line, featureNo(PRODUCTS, "name"), name);
	printf("\nEnter brand: ");
	scanf("%5s", brand);
	featureSet(PRODUCTS, line, featureNo(PRODUCTS, "brand"), brand);
	printf("\nEnter price: ");
	scanf("%*[^.0-9]");
	scanf("%19[.0-9]", price);
	featureSet(PRODUCTS, line, featureNo(PRODUCTS, "price"), price);
}

void deleteProduct(){
	char pID[10];
	char pIDNew[10];
	char pIDOld[10];
	char name[9];
	int entryNo, noPIDStock, sIDMatch, numProd, i, noPID;
	printf("\nEnter pID: ");
	scanf("%9s", pID);
	entryNo = entryGetNext(PRODUCTS, 0, featureNo(PRODUCTS, "pID"), pID);
	if(entryNo == -1) {
		printf("\nCould not match pID: %s\n", pID);
	}
	else {
		featureGet(PRODUCTS, entryNo, featureNo(PRODUCTS, "name"), name);
		if(entryDelete(PRODUCTS, entryNo) == -1) {
			printf("\nError deleting %s.\n", name);
		}
		else {
			printf("\nSuccesfully deleted %s.\n", name);
			noPIDStock = featureNo(STOCKS, "pID");
			while((sIDMatch = entryGetNext(STOCKS, 0, noPIDStock, pID)) != -1) {
				deleteStock(sIDMatch);
			}
			numProd = entryCount(PRODUCTS);
			noPID = featureNo(PRODUCTS, "pID");
			for(i = entryNo; i <= numProd; i++) {
				sprintf(pIDNew, "%d", i);
				featureGet(PRODUCTS, i, noPID, pIDOld);
				featureSet(PRODUCTS, i, noPID, pIDNew);
				while((sIDMatch = entryGetNext(STOCKS, 0, noPIDStock, pIDOld)) != -1) {
					featureSet(STOCKS, sIDMatch, noPIDStock, pIDNew);
				}
			}
		}
	}
}

void updateProduct() {
	char pID[10];
	char nameFeature[20];
	char newVal[20];
	char name[9];
	char oldVal[20];
	int entryNo, noPID, noName, noFeature;
	noPID = featureNo(PRODUCTS, "pID");
	noName = featureNo(PRODUCTS, "name");
	printf("\nEnter pID: ");
	scanf("%9s", pID);
	entryNo = entryGetNext(PRODUCTS, 0, noPID, pID);
	if(entryNo == -1) {
		printf("\nCould not match pID: %s\n", pID);
	}
	else {
		printf("\nEnter feature name: ");
		scanf("%19s", nameFeature);
		noFeature = featureNo(PRODUCTS, nameFeature);
		if(noFeature == -1) {
			printf("\nCould no find feature %s.\n", nameFeature);
		}
		else if(noFeature == noPID) {
			printf("\nThe pID cannot be updated.\n");
		}
		else {
			printf("\nEnter new value: ");
			if(noFeature == featureNo(PRODUCTS, "type")) {
				scanf(" %1[a-zA-Z]", newVal);
			}
			else if(noFeature == noName) {
				scanf("%8s", newVal);
			}
			else if(noFeature == featureNo(PRODUCTS, "brand")) {
				scanf("%5s", newVal);
			}
			else if(noFeature == featureNo(PRODUCTS, "price")) {
				scanf("%*[^.0-9]");
				scanf("%19[.0-9]", newVal);
			}
			else  {
				scanf("%19s", newVal);
			}
			featureGet(PRODUCTS, entryNo, noFeature, oldVal);
			if(featureSet(PRODUCTS, entryNo, noFeature, newVal) < 0) {
				printf("\nAn error occurred.\n");
			}
			else {
				featureGet(PRODUCTS, entryNo, noName, name);
				printf("\nChanged product %s's %s from %s to %s.\n", name, nameFeature, oldVal, newVal);
			}
		}
	}
}

void addFeature() {
	char nameFeature[20];
	printf("\nEnter name of the new feature: ");
	scanf("%19s", nameFeature);
	if(featureNo(PRODUCTS, nameFeature) == -1) {
		featureAdd(PRODUCTS, nameFeature, "None");
	}
	else {
		printf("\nFeature %s already exists or an error occurred.\n", nameFeature);
	}
}

void addNewStock() {
	int line;
	char sidStr[10];
	char pID[10];
	char branch[16];
	char stock[10];
	line = entryCount(STOCKS) + 1;
	sprintf(sidStr, "%d", line);
	entryAdd(STOCKS, "None");
	featureSet(STOCKS, line, featureNo(STOCKS, "sID"), sidStr);
	while(1) {
		printf("\nEnter pID: ");
		scanf("%*[^0-9]");
		scanf("%9[0-9]", pID);
		if(entryGetNext(PRODUCTS, 0, featureNo(PRODUCTS, "pID"), pID) < 0) {
			printf("\nCould not match pID %s.\n", pID);
		}
		else {
			break;
		}
	}
	featureSet(STOCKS, line, featureNo(STOCKS, "pID"), pID);
	printf("\nEnter branch: ");
	scanf("%15s", branch);
	featureSet(STOCKS, line, featureNo(STOCKS, "branch"), branch);
	printf("\nEnter current stock: ");
	scanf("%*[^0-9]");
	scanf("%9[0-9]", stock);
	featureSet(STOCKS, line, featureNo(STOCKS, "current_stock"), stock);
}

void deleteStock(int sID) {
	char sIDStr[10];
	char pID[10];
	int numStock, i, noSID;
	featureGet(STOCKS, sID, featureNo(STOCKS, "pID"), pID);
	if(entryDelete(STOCKS, sID) == -1) {
		printf("\nError deleting stock related to pID %s.\n", pID);
	}
	else {
		printf("\nDeleted stock releated to pID %s.\n", pID);
		numStock = entryCount(STOCKS);
		noSID = featureNo(STOCKS, "sID");
		for(i = sID; i <= numStock; i++) {
			sprintf(sIDStr, "%d", i);
			featureSet(STOCKS, i, noSID, sIDStr);
		}
	}
}

void deleteStockWrapper() {
	int sID, numStock;
	numStock = entryCount(STOCKS);
	while(1) {
		printf("\nEnter sID: ");
		scanf("%d", &sID);
		if(sID >= 1 && sID <= numStock) {
			break;
		}
		printf("\nCould not match sID %d.\n", sID);
	}
	deleteStock(sID);
}

void updateStock() {
	char sID[10];
	char nameFeature[20];
	char newVal[20];
	char oldVal[20];
	int entryNo, noSID, noFeature;
	noSID = featureNo(STOCKS, "sID");
	printf("\nEnter sID: ");
	scanf("%9s", sID);
	entryNo = entryGetNext(STOCKS, 0, noSID, sID);
	if(entryNo == -1) {
		printf("\nCould not match sID: %s\n", sID);
	}
	else {
		printf("\nEnter feature name: ");
		scanf("%19s", nameFeature);
		noFeature = featureNo(STOCKS, nameFeature);
		if(noFeature == -1) {
			printf("\nCould no find feature %s.\n", nameFeature);
		}
		else if(noFeature == noSID) {
			printf("\nThe sID cannot be updated.\n");
		}
		else {
			printf("\nEnter new value: ");
			if(noFeature == featureNo(STOCKS, "pID")) {
				while(1) {
					scanf("%*[^0-9]");
					scanf("%19[0-9]", newVal);
					if(entryGetNext(PRODUCTS, 0, featureNo(PRODUCTS, "pID"), newVal) != -1) {
						break;
					}
					printf("\nCould not match pID %s.\n\nEnter new value: ", newVal);
				}
			}
			else if(noFeature == featureNo(STOCKS, "current_stock")) {
				scanf("%*[^0-9]");
				scanf("%19[0-9]", newVal);
			}
			else if(noFeature == featureNo(STOCKS, "branch")) {
				scanf("%15s", newVal);
			}
			else  {
				scanf("%19s", newVal);
			}
			featureGet(STOCKS, entryNo, noFeature, oldVal);
			if(featureSet(STOCKS, entryNo, noFeature, newVal) < 0) {
				printf("\nAn error occurred.\n");
			}
			else {
				printf("\nChanged stock with sID %s's %s from %s to %s.\n", sID, nameFeature, oldVal, newVal);
			}
		}
	}
}
