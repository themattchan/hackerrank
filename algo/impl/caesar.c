#include <math.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

char
cipher(char c, int k)
{
	if (isupper(c)) return (c - 'A' + k) % 26 + 'A';
	if (islower(c)) return (c - 'a' + k) % 26 + 'a';
	return c;
}

int
main()
{
	int n;
	scanf("%d", &n);
	char *s = (char *) malloc(10240 * sizeof(char));
	scanf("%s", s);
	int k;
	scanf("%d", &k);

	for (int i = 0; i < n; i++)
		s[i] = cipher(s[i],k);

	printf("%s\n", s);
	free(s);
	return 0;
}
