#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <ctype.h>

int main(){
    char* s = (char *)malloc(100240 * sizeof(char));
    scanf("%s",s);
    int c = 1;
    while (*s++) {
	    if (isupper(*s)) c++;
    }
    printf("%d\n",c);
    return 0;
}
