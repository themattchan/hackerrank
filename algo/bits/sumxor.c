#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

/*
  a+b = a^b
  ==> a^b + a &b = a^b
  ==> a & b = 0
  given fixed a, there are 2^ (#num 0s in a) possibilities
*/
long int solve(long int n) {
	int zeros = 0;
	while (n) {
		if ((n&1) == 0)
			zeros++;
		n >>= 1;
	}
	return ((long int) 1) << zeros;
}

int main() {
	long int n;
	scanf("%li", &n);
	long int result = solve(n);
	printf("%ld\n", result);
	return 0;
}
