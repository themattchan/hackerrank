#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int main()
{
	char time[10];
	fgets(time, 10, stdin);
	char AorP = time[8];
	time[8] = '\0';

	if (time[0] == '1' && time[1] == '2') {
		if (AorP == 'A') {
			time[0] = '0';
			time[1] = '0';
		}
	} else if (AorP == 'P') {
		time[0] += 1;
		time[1] += 2;
	}

	printf("%s", time);
    return 0;
}
