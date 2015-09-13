#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int linsearch(int val, int size, int* ar)
{
	for (int i = 0; i < size; i++) {
		if (ar[i] == val) {
			return i;
		}
	}
	return -1;
}

int main(void)
{
	int val;
	scanf("%d", &val);

	int size;
	scanf("%d", &size);

	int ar[size];
	for (int i = 0; i < size; i++) {
		scanf("%d", &ar[i]);
	}

	printf("%d", linsearch(val, size, ar));

    return 0;
}
