#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int size;

void print_array(int* ar)
{
	for (int i = 0; i < size; i++) {
		printf("%d ", ar[i]);
	}
	putchar('\n');
}

void swap(int* a, int* b)
{
	int tmp = *a;
	*a = *b;
	*b = tmp;
}

int partition(int lo, int hi, int* ar)
{
	int pivot = ar[hi];
	int swp_i = lo;

	for (int i = lo; i < hi; i++) {
		if (ar[i] <= pivot) {
			swap(&ar[swp_i], &ar[i]);
			swp_i++;
		}
	}
	swap(&ar[hi], &ar[swp_i]);
	return swp_i;
}

void quicksort(int lo, int hi, int* ar)
{
	if (lo < hi) {
		int pi = partition(lo, hi, ar);
		print_array(ar);
		quicksort(lo, pi-1, ar);
		quicksort(pi+1, hi, ar);
	}
}

int main(void)
{
	scanf("%d", &size);

	int ar[size];
	for (int i = 0; i < size; i++) {
		scanf("%d", &ar[i]);
	}

	quicksort(0, size-1, ar);

	return 0;
}
