#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int main()
{
	int n,k,q;
	scanf("%d",&n); 			/* arr size */
	scanf("%d",&k);				/* k rotations, but only rotate %n, */
	k %= n;
	scanf("%d",&q);				/* queries */

	int a[n];
	for (int i = 0; i < n; i++) {
		scanf("%d", &a[i]);
	}

	int t;
	for (int i = 0; i < q; i++) {
		scanf("%d",&t);
		printf("%d\n",a[(n-k+t)%n]); /* 0-th elem for k rots is (n-k)%n */
	}
    return 0;
}
