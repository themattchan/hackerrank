#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define NEED(x) ((x==0?1:0))
#define MAX(x,y) (x<y?y:x)

int main () {
	int n;
	scanf("%d\n", &n);
	char pass[n];
	gets(pass);

	int len = 6;
	int d = 0;
	int l = 0;
	int u = 0;
	int p = 0;

	char* cur = pass;
	while (*cur) {
		char c = *cur++;
		len--;
		if (isdigit(c)) d++;
		if (islower(c)) l++;
		if (isupper(c)) u++;
		if (ispunct(c)) p++;
		if (len <= 0 && d && l && u && p) break;
	}

	int need = MAX(len, NEED(d)+NEED(l)+NEED(u)+NEED(p));
	printf("%d\n",need);
}
