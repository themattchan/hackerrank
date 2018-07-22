#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define GET(arr,i) (i<0?0:arr[i])
#define MAX(x,y) (x<y?y:x)

int lcs (char* xs, size_t lenxs, char* ys, size_t lenys) {
	int* old;
	old = calloc(lenxs, sizeof(int));
	int* new;
	new = calloc(lenxs, sizeof(int));

	for (size_t j = 0; j < lenys; j++) {
		char y = ys[j];
		for (size_t i = 0; i < lenxs; i++) {
			if (y == xs[i]) {
				new[i] = 1 + GET(old,i-1);
			} else {
				int a =  xGET(new,i-1);
				int b =  GET(old,i);
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


int main () {
	char xs[10000];
	char ys[10000];
	gets(xs); gets(ys);
	size_t xslen = strlen(xs);
	size_t yslen = strlen(ys);

	printf ("%d\n",xslen);
	printf ("%d\n",yslen);
	printf ("%d\n", lcs(xs,xslen, ys,yslen));
}
