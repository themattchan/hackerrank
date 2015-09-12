#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>

void print_array(int ar_size, int* ar)
{
	 for (int i = 0; i < ar_size; i++) {
		  printf("%d ", ar[i]);
	 }
	 putchar('\n');
}

// this does not feel like insertion sort...
void insertionSort(int ar_size, int*  ar)
{
	 for (int i = 1; i < ar_size; i++) {
		  for (int j = i-1; j > -1; j--) {
			  int r = ar[j+1];
			  int l = ar[j];
			  if (l > r) {
				  ar[j] = ar[j+1];
				  ar[j+1] = l;
			  }
		  }
		  print_array(ar_size,ar);
	 }
}

int main(void)
{
	 int _ar_size;
	 scanf("%d", &_ar_size);

	 int _ar[_ar_size];
	 for (int i = 0; i < _ar_size; i++) {
		  scanf("%d", &_ar[i]);
	 }

	 insertionSort(_ar_size, _ar);

	 return 0;
}
