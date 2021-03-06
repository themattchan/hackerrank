#include <stdio.h>
#include <stdlib.h>

#define GET(arr,i) (i<0?0:arr[i])
#define MAX(x,y) (x<y?y:x)

int lcs (char* xs, size_t lenxs, char* ys, size_t lenys) {
	int* old = calloc(lenxs, sizeof(int));
	int* new = calloc(lenxs, sizeof(int));

	for (int j = 0; j < (int)lenys; j++) {
		char y = ys[j];
		for (int i = 0; i < (int)lenxs; i++) {
			if (y == xs[i]) {
				new[i] = 1 + GET(old,i-1);
			} else {
				int a = GET(new,i-1);
				int b = GET(old,i);

				new[i] = MAX(a,b);
			}
		}

		int* tmp = old;
		old = new;
		new = tmp;
	}

	int res = GET(old, lenxs-1);
	free(old); free(new);
	return res;
}

// benchmark
// on input05: ./CommonChildC  0.06s user 0.00s system 94% cpu 0.062 total
// 6.2x faster than the fastest Haskell version (lcs5, 0.384)
int main () {
	char xs[10000];
	char ys[10000];
	gets(xs); gets(ys);

	size_t xslen = strlen(xs);
	size_t yslen = strlen(ys);

	printf ("%d\n", lcs(xs,xslen, ys,yslen));
}
