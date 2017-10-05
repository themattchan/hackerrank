#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

char* kangaroo(int x1, int v1, int x2, int v2)
{
	//x1 + v1 t = x2 + v2 t => t = (x1-x2)/(v2-v1)
	// for them to land at the same place at the same time, the location has
	// to be a whole number
	int xx = x1-x2;
	int vv = v2-v1;
	if (vv == 0) return "NO";
	return xx/vv>= 0 && xx % vv == 0 ? "YES" : "NO";
}

int main()
{
	int x1;
	int v1;
	int x2;
	int v2;
	scanf("%i %i %i %i", &x1, &v1, &x2, &v2);
	int result_size;
	char* result = kangaroo(x1, v1, x2, v2);
	printf("%s\n", result);
	return 0;
}
