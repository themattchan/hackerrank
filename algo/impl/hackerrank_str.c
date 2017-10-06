#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

int main(){
    int q;
    scanf("%d",&q);
    for(int a0 = 0; a0 < q; a0++){
        char* s = (char *)malloc(512000 * sizeof(char));
        scanf("%s",s);
        char *hack = "hackerrank";
	while (*s) {
		if (*hack != '\0' && *s == *hack) {
			hack++;
		}
		s++;
	}
	printf("%s\n", *hack == '\0' ? "YES" : "NO");
    }
    return 0;
}
