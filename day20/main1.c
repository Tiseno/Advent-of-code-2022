#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define INPUTE1 "input.example.txt"
#define INPUT "input.txt"

struct List {
	int value;
	_Bool visited;
	struct List * prev;
	struct List * next;
};

#define N struct List

N* makeNode(int value, N* prev, N* next) {
	N* a = malloc(sizeof(N));
	a->value = value;
	a->visited = false;
	a->prev = prev;
	a->next = next;
	return a;
}

N* findNextUnvisited(N* list) {
	N* start = list->next;
	N* i = list->next;
	do {
		if (!(i->visited)) {
			return i;
		}
		i = i->next;
	} while (i != start);

	return NULL;
}

void pop(N* list) {
	list->prev->next = list->next;
	list->next->prev = list->prev;
}

void insertAfter(N* prev, N* inserted) {
	N* next = prev->next;
	next->prev = inserted;
	prev->next = inserted;
	inserted->prev = prev;
	inserted->next = next;
}

N* findOffset(N* list, int value) {
	if (value == 0) {
		return list;
	}

	N* n = list;

	for (int i = 0; i < abs(value); i++) {
		if (value < 0) {
			n = n->prev;
		} else {
			n = n->next;
		}
	}
	return n;
}

void printList(N* list) {
	if (list == NULL) {
		printf("NULL list when printing.");
		return;
	}
	N* j = list;
	do {
        printf("%i\n", j->value);
		j = j->next;
	} while (j != list);
	printf("\n");
}


N* find0(N* list) {
	while (list->value != 0) {
		list = list->next;
	}
	return list;
}

int grooveSum(N* list) {
	N* zero = find0(list);;
	N* c1000 = findOffset(zero, 1000);
	N* c2000 = findOffset(zero, 2000);
	N* c3000 = findOffset(zero, 3000);

	return c1000->value + c2000->value + c3000->value;
}

struct PointerList {
	struct List* value;
	struct PointerList * next;
};

#define P struct PointerList

P* makePtr(N* mix) {
	P* a = malloc(sizeof(P));
	a->value = mix;
	a->next = NULL;
	return a;
}

void printListP(P* list) {
	if (list == NULL) {
		printf("NULL list when printing.");
		return;
	}
	while (list != NULL) {
        printf("%p\n", list->value);
		list = list->next;
	}
}

int main() {
    FILE * file = fopen(INPUT, "r");
    char * line = NULL;
    size_t length = 0;
    size_t read;

    if (file == NULL) {
		return 1;
	}

	N* first = NULL;
	N* last = NULL;

	P* firstp = NULL;
	P* lastp = NULL;

    while ((read = getline(&line, &length, file)) != -1) {
		if (first == NULL) {
			first = makeNode(atoi(line), NULL, NULL);
			first->prev = first;
			first->next = first;
			last = first;

			firstp = makePtr(first);
			lastp = firstp;
		} else {
			N* n = makeNode(atoi(line), last, first);
			last->next = n;
			first->prev = n;
			last = n;

			lastp->next = makePtr(n);
			lastp = lastp->next;
		}
    }

	// printList(first);
	// printListP(firstp);

	N* i = first;
	do {
        // printf("Looking at %i\n", i->value);
		N* current = i;
		current->visited = true;

		N* prev = current->prev;
		pop(current);
		N* insertNode = findOffset(prev, current->value);
		insertAfter(insertNode, current);
		// printList(first);
		i = prev;
	} while ((i = findNextUnvisited(i)) != NULL);

	// printf("\n\n");
	// printList(first);

    printf("%i\n", grooveSum(first));

    fclose(file);
	return 0;
}
