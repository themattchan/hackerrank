#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int funny(const char *str)
{
	int len = strlen(str);
	int fun = 1;
	for (int i = 1, j = len-1; i < len; i++, j--) {
		fun = abs(str[i] - str[i-1]) == abs(str[j-1] - str[j]);
		if (!fun) {
			return 0;
		}
	}

	return 1;
}

int main()
{
	int n;
	scanf("%d", &n);

	char *str;
	str = (char*) malloc(10000 * sizeof(char));

	while (n--) {
		memset(str, 0, strlen(str));
		scanf("%s", str);
		printf("%s\n", funny(str) ? "Funny" : "Not Funny");
	}

	free(str);
	return 0;
}
