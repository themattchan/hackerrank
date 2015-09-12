#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int
main()
{
    int MAX;
    scanf("%d", &MAX);

    unsigned int inp;

    for (int i = 0; i < MAX; ++i) {
        scanf("%u", &inp);
        printf("%u\n", ~inp);
    }

    return 0;
}
