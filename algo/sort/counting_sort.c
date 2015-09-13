#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#define PROBLEM 4

void print_array(int* ar, int size)
{
	for (int i = 0; i < size; i++) {
		printf("%d ", ar[i]);
	}
	putchar('\n');
}

#ifdef PROBLEM

#if (PROBLEM == 1)
int main(void) {

	int size;
	scanf("%d", &size);

	int ar[100];
	memset(ar, 0, sizeof(ar));

	for (int i = 0; i < size; i++) {
		int j = 0;
		scanf("%d", &j);
		ar[j]++;
	}

	print_array(ar, 100);
	return 0;
}
#endif

#if (PROBLEM == 2)
int main(void) {

	int size;
	scanf("%d", &size);

	int ar[100];
	memset(ar, 0, sizeof(ar));

	for (int i = 0; i < size; i++) {
		int j = 0;
		scanf("%d", &j);
		ar[j]++;
	}

	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < ar[i]; j++) {
			printf("%d ", i);
		}
	}

	return 0;
}
#endif

#if (PROBLEM == 3)
int main(void) {

	int size;
	scanf("%d", &size);

	int ar[100];
	memset(ar, 0, sizeof(ar));

	for (int i = 0; i < size; i++) {
		int j = 0;
		scanf("%d", &j);
		scanf("%*[^\n]");		/* discard the word */
		ar[j]++;
	}

	int running = 0;
	for (int i = 0; i < 100; i++) {
		running += ar[i];
		printf("%d ", running);
	}

	return 0;
}
#endif

#if (PROBLEM == 4)
const char *BLANK = "- ";

int main(void) {

	int size;
	scanf("%d", &size);

	char* ar[100];
	memset(ar, 0, sizeof(ar));

	for (int i = 0; i < size; i++) {
		int j = 0;
		scanf("%d", &j);

		if (i < size/2) {
			scanf("%*[^\n]");		/* discard the word */

			/*
			if (ar[j] == NULL) {
				ar[j] = (char*) malloc(strlen(BLANK) +1);
				ar[j][0] = '\0';
			} else {
				ar[j] = (char*) realloc(ar[j], sizeof(*ar[j]) + strlen(BLANK));
			}
			*/

			/* Fine, fuck you, I'll just malloc a HUGE chunk of memory */
			if (ar[j] == NULL) {
				ar[j] = (char*) malloc(size/2 * 10 * sizeof(char));
				ar[j][0] = '\0';
			}
			if (ar[j] == NULL) {
				printf("FAIL malloc/realloc");
				return 1;
			}
			strcat(ar[j], BLANK);
		}

		else {
			char buf[11] = "";
			scanf("%s ", buf);

			/*
			if (ar[j] == NULL) {
				ar[j] = (char*) malloc(strlen(buf) +1);
				ar[j][0] = '\0';
			} else {
				ar[j] = (char*) realloc(ar[j], sizeof(*ar[j]) + strlen(buf) +1);
			}
			*/

			/* Fine, fuck you, I'll just malloc a HUGE chunk of memory */
			if (ar[j] == NULL) {
				ar[j] = (char*) malloc(size/2 * 10 * sizeof(char));
				ar[j][0] = '\0';
			}
			if (ar[j] == NULL) {
				printf("FAIL malloc/realloc");
				return 1;
			}
			strcat(ar[j], buf);
			strcat(ar[j], " ");
		}
	}

	for (int i = 0; i < 100; i++) {
		if (ar[i]) {
			printf("%s", ar[i]);
			free(ar[i]);
		}
	}
	putchar('\n');
	return 0;
}
#endif

#endif
