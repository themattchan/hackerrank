#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int size = 0;
int swaps = 0;

void print_array(int* ar)
{
	for (int i = 0; i < size; i++) {
		printf("%d ", ar[i]);
	}
	putchar('\n');
}

void swap(int* a, int* b) {
	int tmp = *a;
	*a = *b;
	*b = tmp;
	swaps++;
}

int partition(int lo, int hi, int* ar) {
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

void quicksort(int lo, int hi, int* ar) {
	if (lo < hi) {
		int pi = partition(lo, hi, ar);
//		print_array(ar);
		quicksort(lo, pi-1, ar);
		quicksort(pi+1, hi, ar);
	}
}

// this does not feel like insertion sort...
int insertionSort(int ar_size, int* ar)
{
	int shifts = 0;
	 for (int i = 1; i < ar_size; i++) {
		  for (int j = i-1; j > -1; j--) {
			  int r = ar[j+1];
			  int l = ar[j];
			  if (l > r) {
				  ar[j] = ar[j+1];
				  ar[j+1] = l;
				  shifts++;
			  }
		  }
	 }
	 return shifts;
}


int main(void) {
	scanf("%d", &size);

	int ar1[size], ar2[size];
	for (int i = 0; i < size; i++) {
		scanf("%d", &ar1[i]);
		ar2[i] = ar1[i];
	}

	quicksort(0, size-1, ar1);
	int ss = insertionSort(size, ar2);

	printf("%d", ss - swaps);
	return 0;
}
