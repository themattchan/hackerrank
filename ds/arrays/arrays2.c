#include <stdio.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>

int main() {
	int grid[6][6];
	for (int i = 0; i < 6; i++) {
		for (int j = 0; j < 6; j++) {
			scanf("%d",&grid[i][j]);
		}
	}

	int max = INT_MIN;
	int go = 0;
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			go = grid[i][j] + grid[i][j+1] + grid[i][j+2]
				+ grid[i+1][j+1]
				+ grid[i+2][j] + grid[i+2][j+1] + grid[i+2][j+2];

			if (go > max) {
				max = go;
			}
		}
	}
	printf("%d\n", max);

    return 0;
}
