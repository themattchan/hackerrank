#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int main() {
	int n;
	scanf("%d",&n);
	int nums[n];
	for (int i = 0; i < n; i++) {
		scanf("%d",&nums[i]);
	}
	for (int i = n-1; i > -1; i--) {
		printf("%d ", nums[i]);
	}
	putchar('\n');
    return 0;
}
