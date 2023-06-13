#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LIBRARY_FILE "library.txt"
#define STUDENT_FILE "students.txt"

struct book {
	char isbn[14];
	char title[64];
	char author[32];
	int year;
	int status;
	struct book * next;
};

struct student {
	char name[32];
	int id;
	char * borrowed;
	struct student * next;
};

typedef struct book book;

typedef struct student student;

void displayBooks(student * studentList, book * library, int id) {
	student * cs;
	book * cb;
	int idx;
	char isbn[14];
	for(cs = studentList; cs != NULL && cs->id != id; cs = cs->next);
	if(cs) {
		if(cs->borrowed[0] == '\0') {
			printf("\nNo books borrowed by %s.\n", cs->name);
		}
		else {
			for(idx = 0; cs->borrowed[idx] != '\0';) {
				idx++;
				sscanf(cs->borrowed + idx, "%[^,]", isbn);
				for(cb = library; cb != NULL && strcmp(cb->isbn, isbn); cb = cb->next);
				if(cb) {
					printf("\nISBN: %s | Title: %s | Author: %s | Publication Year: %d\n", cb->isbn, cb->title, cb->author, cb->year);
				}
				for(; cs->borrowed[idx] != ',' && cs->borrowed[idx] != '\0'; idx++);
			}
		}
	}
}

book * addBook(book * library, char * isbn, char * title, char * author, int year, int method) {
	book * new = malloc(sizeof(book));
	strncpy(new->isbn, isbn, 14);
	strncpy(new->title, title, 64);
	strncpy(new->author, author, 32);
	new->year = year;
	new->status = 0;
	if(method == 1) {
		new->next = library;
		library = new;
	}
	else {
		new->next = NULL;
		book ** ptr;
		for(ptr = &library; *ptr != NULL; ptr = &(*ptr)->next);
		*ptr = new;
	}
	return library;
}

book * deleteBook(book * library, char * isbn) {
	book ** cur;
	for(cur = &library; *cur != NULL && strcmp((*cur)->isbn, isbn); cur = &(*cur)->next);
	if(*cur) {
		free(*cur);
		*cur = (*cur)->next;
	}
	else {
		printf("\nNo matches found with ISBN: %s\n", isbn);
	}
	return library;
}

void updateBook(book * library, char * isbn, char * feature, char * value) {
	book * cur;
	for(cur = library; cur != NULL && strcmp(cur->isbn, isbn); cur = cur->next);
	if(cur) {
		if(!strcmp(feature, "title")) {
			strncpy(cur->title, value, 64);
		}
		else if(!strcmp(feature, "author")) {
			strncpy(cur->author, value, 32);
			cur->author[31] = '\0';
		}
		else if(!strcmp(feature, "publication year") || !strcmp(feature, "year")) {
			sscanf(value, "%d", &cur->year);
		}
		else {
			printf("\nInvalid feature name: %s\n", feature);
		}
	}
	else {
		printf("\nNo matches found with ISBN: %s\n", isbn);
	}
}

book * filterAndSortBooks(book * library, int filterChoice, char * filter, int sortChoice) {
	book ** cur, * first, * cmp;
	int filterCheck0, filterCheck1, sortResult, year;
	if(filterChoice == 1) {
		sscanf(filter, "%d", &year);
	}
	for(cur = &library; *cur != NULL; cur = &(*cur)->next) {
		first = *cur;
		if(filterChoice == 0) {
			filterCheck0 = !strcmp(first->author, filter);
		}
		else {
			filterCheck0 = year == first->year;
		}
		for(cmp = first->next; cmp != NULL; cmp = cmp->next) {
			if(filterChoice == 0) {
				filterCheck1 = !strcmp(cmp->author, filter);
			}
			else {
				filterCheck1 = year == cmp->year;
			}
			if(filterCheck1) {
				if(!filterCheck0) {
					sortResult = -1;
				}
				else {
					switch(sortChoice) {
						case 0:
							sortResult = strcmp(cmp->title, first->title);
							break;
						case 1:
							sortResult = strcmp(cmp->author, first->author);
							break;
						case 2:
							sortResult = cmp->year - first->year;
							break;
					}
				}
			}
			else {
				sortResult = 0;
			}
			if(sortResult < 0) {
				first = cmp;
				if(filterChoice == 0) {
					filterCheck0 = !strcmp(first->author, filter);
				}
				else {
					filterCheck0 = year == first->year;
				}
			}
		}
		if(first != *cur) {
			book tmp = **cur;
			book * tmpNext = (*cur)->next;
			**cur = *first;
			*first = tmp;
			first->next = (*cur)->next;
			(*cur)->next = tmpNext;
		}
	}
	return library;
}

book * reverseBookList(book * library) {
	book * cur, * newList;
	for(newList = NULL, cur = library; cur != NULL; cur = library) {
		library = cur->next;
		cur->next = newList;
		newList = cur;
	}
	return newList;
}

void searchBooks(book * library, int searchChoice, char * searchValue) {
	book * cur;
	int found = 0;
	int match;
	for(cur = library; cur != NULL; cur = cur->next) {
		switch(searchChoice) {
			case 0:
				found += match = !strcmp(searchValue, cur->isbn);
				break;
			case 1:
				found += match = !strcmp(searchValue, cur->author);
				break;
			case 2:
				found += match = !strcmp(searchValue, cur->title);
				break;
			default:
				match = 0;
				break;
		}
		if(match) {
			printf("\nISBN: %s | Title: %s | Author: %s | Publication Year: %d | Borrowed: %s\n", cur->isbn, cur->title, cur->author, cur->year, cur->status ? "Yes" : "No");
		}
	}
	if(!found) {
		printf("\nNo results.\n");
	}
}

void borrowBook(student * studentList, book * library, int id, char * isbn) {
	book * cb;
	student * cs;
	for(cb = library; cb != NULL && strcmp(cb->isbn, isbn); cb = cb->next);
	if(!cb) {
		printf("\nNo book with ISBN %s found.\n", isbn);
	}
	else if(cb->status) {
		printf("\nThe book with ISBN %s is already borrowed.\n", isbn);
	}
	else {
		for(cs = studentList; cs != NULL && cs->id != id; cs = cs->next);
		if(cs) {
			cs->borrowed = realloc(cs->borrowed, strlen(cs->borrowed) + strlen(isbn) + 2);
			strcat(cs->borrowed, ",");
			strcat(cs->borrowed, isbn);
			cb->status = 1;
		}
		else {
			printf("\nCouldn't found a student with ID %d.\n", id);
		}
	}
}

void returnBook(student * studentList, book * library, int id, char * isbn) {
	student * cur;
	book * cb;
	int bp, bidx, iidx;
	for(cur = studentList; cur != NULL && cur->id != id; cur = cur->next);
	if(cur) {
		bidx = 0;
		while(cur->borrowed[bidx] != '\0') {
			bp = bidx++;
			for(iidx = 0; isbn[iidx] != '\0' && isbn[iidx] == cur->borrowed[bidx]; iidx++, bidx++);
			if(cur->borrowed[bidx] == '\0' || cur->borrowed[bidx] == ',' && isbn[iidx] == '\0') {
				strcpy(&cur->borrowed[bp], &cur->borrowed[bidx]);
				for(cb = library; cb != NULL && strcmp(cb->isbn, isbn); cb = cb->next);
				if(cb) {
					cb->status = 0;
				}
				bidx = -1;
				break;
			}
			for(; cur->borrowed[bidx] != '\0' && cur->borrowed[bidx] != ','; bidx++);
		}
		if(bidx != -1) {
			printf("\nStudent with ID %d did not borrow a book with ISBN %s.\n", id, isbn);
		}
	}
	else {
		printf("\nCouldn't found a student with ID %d.\n", id);
	}
}

student * addStudent(student * studentList, char * name, int id, int method) {;
	student * new = malloc(sizeof(student));
	strncpy(new->name, name, 32);
	new->id = id;
	new->borrowed = malloc(1);
	new->borrowed[0] = '\0';
	if(method == 1) {
		new->next = studentList;
		studentList = new;
	}
	else {
		new->next = NULL;
		student ** ptr;
		for(ptr = &studentList; *ptr != NULL; ptr = &(*ptr)->next);
		*ptr = new;
	}
	return studentList;
}

student * removeStudent(student * studentList, int id) {
	student ** cur;
	for(cur = &studentList; *cur != NULL && (*cur)->id != id; cur = &(*cur)->next);
	if(*cur) {
		free((*cur)->borrowed);
		free(*cur);
		*cur = (*cur)->next;
	}
	else {
		printf("\nNo matches found with ID: %d\n", id);
	}
	return studentList;
}

void load(char * libraryFile, char * studentFile, book ** library, student ** studentList) {
	FILE * fp;
	fp = fopen(libraryFile, "r");
	*library = NULL;
	*studentList = NULL;
	if(fp) {
		book ** ptr = library;
		book read;
		while(fscanf(fp, "%13[^,],%63[^,],%31[^,],%d,%d ", read.isbn, read.title, read.author, &read.year, &read.status) != EOF) {
			read.next = NULL;
			*ptr = malloc(sizeof(book));
			**ptr = read;
			ptr = &(*ptr)->next;
		}
		fclose(fp);
	}
	fp = fopen(studentFile, "r");
	if(fp) {
		student ** ptr = studentList;
		student read;
		int bs, l;
		while(fscanf(fp, "%d,%31[^,\n]", &read.id, read.name) != EOF) {
			bs = ftell(fp);
			fscanf(fp, "%*[^\n]");
			l = ftell(fp) - bs;
			read.borrowed = malloc(l + 1);
			fseek(fp, bs, SEEK_SET);
			fscanf(fp, "%[^\n]", read.borrowed);
			read.next = NULL;
			*ptr = malloc(sizeof(student));
			**ptr = read;
			ptr = &(*ptr)->next;
		}
		fclose(fp);
	}
}

void save(char * libraryFile, char * studentFile, book * library, student * studentList) {
	FILE * fp;
	fp = fopen(libraryFile, "w");
	if(fp) {
		for(book * cur = library; cur != NULL; cur = cur->next) {
			fprintf(fp, "%s,%s,%s,%d,%d\n", cur->isbn, cur->title, cur->author, cur->year, cur->status);
		}
		fclose(fp);
	}
	fp = fopen(studentFile, "w");
	if(fp) {
		for(student * cur = studentList; cur != NULL; cur = cur->next) {
			fprintf(fp, "%d,%s", cur->id, cur->name);
			if(cur->borrowed[0] != '\0') {
				fprintf(fp, "%s\n", cur->borrowed);
			}
			else {
				fprintf(fp, "\n");
			}
		}
		fclose(fp);
	}
}

int main() {
	book * library;
	student * studentList;
	int method;
	int option;
	char strin[3][64];
	int intin[2];
	while(1) {
		printf("\nPlease select the management method (0 for FIFO, 1 for LIFO): ");
		scanf("%d", &method);
		if(method == 0 || method == 1) {
			break;
		}
		else {
			printf("\nInvalid input!\n");
		}
	}
	load(LIBRARY_FILE, STUDENT_FILE, &library, &studentList);
	do {
		printf("\nWelcome to the library, please select an option:\n 0 - Exit\n 1 - Add a book\n 2 - Delete a book\n 3 - Update a book\n 4 - Filter and sort books\n 5 - Reverse the book list\n 6 - Search books\n 7 - Borrow a book\n 8 - Return a book\n 9 - Display books borrowed by a student\n10 - Add a student\n11 - Remove a student\n\n> ");
		scanf("%d", &option);
		switch(option) {
			case 0:
				break;
			case 1:
				printf("\nEnter ISBN: ");
				scanf(" %13[^\n]", strin[0]);
				printf("\nEnter the title: ");
				scanf(" %63[^\n]", strin[1]);
				printf("\nEnter the name of the author: ");
				scanf(" %31[^\n]", strin[2]);
				printf("\nEnter the year of publication: ");
				scanf("%d", &intin[0]);
				library = addBook(library, strin[0], strin[1], strin[2], intin[0], method);
				break;
			case 2:
				printf("\nEnter ISBN: ");
				scanf(" %13[^\n]", strin[0]);
				library = deleteBook(library, strin[0]);
				break;
			case 3:
				printf("\nEnter ISBN: ");
				scanf(" %13[^\n]", strin[0]);
				printf("\nEnter feature name: ");
				scanf(" %63[^\n]", strin[1]);
				printf("\nEnter new value: ");
				scanf(" %63[^\n]", strin[2]);
				updateBook(library, strin[0], strin[1], strin[2]);
				break;
			case 4:
				printf("\nEnter filter choice (0 for author, 1 for publication year): ");
				scanf("%d", &intin[0]);
				printf("\nEnter filter: ");
				scanf(" %31[^\n]", strin[0]);
				printf("\nEnter sort choice (0 for title, 1 for author, 2 for publication year): ");
				scanf("%d", &intin[1]);
				library = filterAndSortBooks(library, intin[0], strin[0], intin[1]);
				break;
			case 5:
				library = reverseBookList(library);
				break;
			case 6:
				printf("\nEnter search choice (0 for ISBN, 1 for author, 2 for title): ");
				scanf("%d", &intin[0]);
				printf("\nEnter search criteria: ");
				scanf(" %63[^\n]", strin[0]);
				searchBooks(library, intin[0], strin[0]);
				break;
			case 7:
				printf("\nEnter student ID: ");
				scanf("%d", &intin[0]);
				printf("\nEnter ISBN: ");
				scanf(" %13[^\n]", strin[0]);
				borrowBook(studentList, library, intin[0], strin[0]);
				break;
			case 8:
				printf("\nEnter student ID: ");
				scanf("%d", &intin[0]);
				printf("\nEnter ISBN: ");
				scanf(" %13[^\n]", strin[0]);
				returnBook(studentList, library, intin[0], strin[0]);
				break;
			case 9:
				printf("\nEnter student ID: ");
				scanf("%d", &intin[0]);
				displayBooks(studentList, library, intin[0]);
				break;
			case 10:
				printf("\nEnter student name: ");
				scanf(" %31[^\n]", strin[0]);
				printf("\nEnter student ID: ");
				scanf("%d", &intin[0]);
				studentList = addStudent(studentList, strin[0], intin[0], method);
				break;
			case 11:
				printf("\nEnter student ID: ");
				scanf("%d", &intin[0]);
				studentList = removeStudent(studentList, intin[0]);
				break;
			default:
				printf("\nInvalid input!\n");
				break;
		}
		save(LIBRARY_FILE, STUDENT_FILE, library, studentList);
	}
	while(option);
}
