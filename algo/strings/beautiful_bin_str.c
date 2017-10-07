#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

// the number of steps needed is the number of
// nonoverlapping occs of 010
int minSteps(int n, char* B){
	int x = 0;
	for (int i = 0; i < n-2; i++) {
		if (B[i] == '0' && B[i+1] == '1' && B[i+2] == '0') {
			x++;
			i+=2;
		}
	}
	return x;
}

int main() {
    int n;
    scanf("%d",&n);
    char* B = (char *)malloc(512000 * sizeof(char));
    scanf("%s",B);
    int result = minSteps(n, B);
    printf("%d\n", result);
    return 0;
}
