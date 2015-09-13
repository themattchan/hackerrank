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

int main(void) {
#ifdef PROBLEM

	int size;
	scanf("%d", &size);

	int ar[100];
	memset(ar, 0, sizeof(ar));

	for (int i = 0; i < size; i++) {
		int j = 0;
		scanf("%d", &j);

#if (PROBLEM == 3)
		char ss[10] = "";
		scanf("%s", &ss);
#endif

		ar[j]++;
	}

#if (PROBLEM == 1)
	print_array(ar, 100);
#endif

#if (PROBLEM == 2)
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < ar[i]; j++) {
			printf("%d ", i);
		}
	}
#endif

#if (PROBLEM == 3)
	int running = 0;
	for (int i = 0; i < 100; i++) {
		running += ar[i];
		printf("%d ", running);
	}
#endif

#endif
	return 0;
}
