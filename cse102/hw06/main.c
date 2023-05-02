#include <stdio.h>
#include <limits.h>
#include "macros.h"
#include "feature.h"
#include "userio.h"
#include "fileOps.h"
#include "search.h"
#include "stock.h"

void fileOperations() {
	int sel;
	do {
		sel = selectionGet(1, 8, "\nFile Operations:\n\n1- Add a new product\n2- Delete a product\n3- Update a product\n4- Add feature to products\n5- Add a new stock entry\n6- Delete a stock entry\n7- Update a stock entry\n8- Back to main menu\n", "\nSelect: ");
		switch(sel) {
			case 1:
				addNewProduct();
				break;
			case 2:
				deleteProduct();
				break;
			case 3:
				updateProduct();
				break;
			case 4:
				addFeature();
				break;
			case 5:
				addNewStock();
				break;
			case 6:
				deleteStockWrapper();
				break;
			case 7:
				updateStock();
				break;
		}
	}
	while(sel != 8);
}

void searchProduct() {
	int sel;
	do {
		sel = selectionGet(1, 3, "\nSearch Product:\n\n1- List all products\n2- Filter products by brand, type, price or a user-defined feature\n3- Back to main menu\n", "\nSelect: ");
		switch(sel) {
			case 1:
				listAllProducts();
				break;
			case 2:
				filterProducts();
				break;
		}
	}
	while(sel != 3);
}

void stockStatus() {
	int sel;
	do {
		sel = selectionGet(1, 4, "\nCheck Stock Status:\n\n1- Check specific product\n2- List branch\n3- List out of stock in branch\n4- Back to main menu\n", "\nSelect: ");
		switch(sel) {
			case 1:
				checkStock();
				break;
			case 2:
				listBranch();
				break;
			case 3:
				listOutOfStock();
				break;
		}
	}
	while(sel != 4);
}

void stockByBrand(char brand[], int pID[], float price[], int stock[][10]) {
	char pIDStr[10];
	char priceStr[20];
	char stockStr[10];
	int noBrand, noPrice, noStock, noPID, pIDInt, ir, sID, is;
	noBrand = featureNo(PRODUCTS, "brand");
	noPrice = featureNo(PRODUCTS, "price");
	noStock = featureNo(STOCKS, "current_stock");
	noPID = featureNo(STOCKS, "pID");
	printf("\nEnter brand: ");
	scanf("%5s", brand);
	pIDInt = 0;
	ir = 0;
	while((pIDInt = entryGetNext(PRODUCTS, pIDInt, noBrand, brand)) != -1) {
		sID = 0;
		is = 0;
		sprintf(pIDStr, "%d", pIDInt);
		while((sID = entryGetNext(STOCKS, sID, noPID, pIDStr)) != -1) {
			featureGet(STOCKS, sID, noStock, stockStr);
			sscanf(stockStr, "%d", &stock[ir][is++]);
		}
		stock[ir][is] = -1;
		featureGet(PRODUCTS, pIDInt, noPrice, priceStr);
		sscanf(priceStr, "%f", &price[ir]);
		pID[ir++] = pIDInt;
	}
	pID[ir] = -1;
	printf("\npID,price,stock_values\n");
	for(int i = 0; pID[i] != -1; i++) {
		printf("%d,%.2f", pID[i], price[i]);
		for(int j = 0; stock[i][j] != -1; j++) {
			printf(",%d", stock[i][j]);
		}
		printf("\n");
	}
}

void sort(int arr[]) {
	int i, j, smallest, tmp;
	for(i = 0; arr[i] != -1; i++) {
		smallest = i;
		for(j = i + 1; arr[j] != -1; j++) {
			if(arr[smallest] > arr[j]) {
				smallest = j;
			}
		}
		tmp = arr[i];
		arr[i] = arr[smallest];
		arr[smallest] = tmp;
	}
}

void exportReport(char brand[], int pID[], float price[], int stock[][10]) {
	char filename[10];
	int i, n;
	FILE* file;
	sprintf(filename, "%s", brand);
	for(i = 0; brand[i] != '\0'; i++);
	sprintf(&filename[i], ".txt");
	file = fopen(filename, "w");
	if(file) {
		fprintf(file, "\npID,price,stock_min,stock_max,stock_mid\n");
		for(i = 0; pID[i] != -1; i++) {
			sort(stock[i]);
			for(n = 0; stock[i][n] != -1; n++);
			fprintf(file, "%d,%.2f,%d,%d,%d", pID[i], price[i], stock[i][0], stock[i][n - 1], stock[i][n / 2]);
			fprintf(file, "\n");
		}
		fclose(file);
		printf("\nSaved report to file %s. Exiting...\n\n", filename);
	}
	else {
		printf("\nCould not open file %s to write. Exiting...\n\n");
	}
}

int main() {
	int pID[50];
	float price[50];
	int stock[50][10];
	char brand[6];
	int createdReport = 0;
	FILE* fp;
	if(!(fp = fopen(PRODUCTS, "r"))) {
		printf("\nFile %s does not exist creating new file...\n", PRODUCTS);
		fileCreate(PRODUCTS, "pID");
		featureAdd(PRODUCTS, "type", "None");
		featureAdd(PRODUCTS, "name", "None");
		featureAdd(PRODUCTS, "brand", "None");
		featureAdd(PRODUCTS, "price", "None");
	}
	if(!(fp = fopen(STOCKS, "r"))) {
		printf("\nFile %s does not exist creating new file...\n", STOCKS);
		fileCreate(STOCKS, "sID");
		featureAdd(STOCKS, "pID", "None");
		featureAdd(STOCKS, "branch", "None");
		featureAdd(STOCKS, "current_stock", "None");
	}
	int input;
	do {
		input = selectionGet(1, 5, "\nWelcome operator, please select an option to continue:\n\n1- File operations\n2- Query products\n3- Check stock status\n4- Stock control by brand\n5- Export report\n", "\nSelect: ");
		switch(input) {
			case 1:
				fileOperations();
				break;
			case 2:
				searchProduct();
				break;
			case 3:
				stockStatus();
				break;
			case 4:
				stockByBrand(brand, pID, price, stock);
				createdReport = 1;
				break;
			case 5:
				if(createdReport) {
					exportReport(brand, pID, price, stock);
				}
				else {
					printf("\nNo report generated. Exiting...\n\n");
				}
				break;
		}
	}
	while(input != 5);
}
