#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdint.h>

int main() {
	char buf[65535];
	fgets(buf, sizeof(buf), stdin);
	char* c = buf;
	// upper 6 bits
	uint32_t count = 0x3f << 26;
	while (*c) {
		if (*c != ' ') {
			count |= (1 << (tolower(*c)-97));
			if (count == 0xffffffff) {
				printf("pangram\n");
				return 0;
			}
		}
		c++;
	}
	printf("not pangram\n");
	return 0;
}
