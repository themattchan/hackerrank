#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

long int solve(long int n) {
    long int count = 0;
    for (long int i = 0; i <= n; i++) {
        if ((i+n) == (i^n)) count++;
    }
    return count;
}

int main() {
    long int n;
    scanf("%li", &n);
    long int result = solve(n);
    printf("%ld\n", result);
    return 0;
}
