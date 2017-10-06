#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>


// P = permutation of [1..N]
// pos_i = index of i in P (1 based indexing)
// P is absolute if |pos_i - i| = K forall i in P.
// the lex. smallest permutation is a left rotation
// of the [1..n] array by k
// 1 <= N <= 10^5
// 0 <= K < N

// want to partition n into 2K partitions
// each pair of partitions will correspond to the pos_i's and i's

//eg
// n = 8, k = 2
// 1 2 3 4 5 6 7 8
// 3 4 1 2 7 8 5 6

// n = 6, k = 3
// 1 2 3 4 5 6
// 4 5 6 1 2 3
void solve(int n, int k)
{
	if (k == 0) {
		for (int i = 1; i <= n; i++) {
			printf("%d ", i);
		}
	}
	else if ((n % (2*k)) == 0) {
		for (int i = 1; i <= n; i++) {
			printf("%d ", i+k);
			if (i % k == 0) k = k*-1;
		}
	}
	else {
		printf("%d",-1);
	}
	printf("\n");
}

int main()
{
	int t;
	scanf("%d",&t);
	for(int a0 = 0; a0 < t; a0++){
		int n;
		int k;
		scanf("%d %d",&n,&k);
		solve(n,k);
	}
	return 0;
}
