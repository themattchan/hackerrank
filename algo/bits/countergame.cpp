#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <iostream>

using namespace std;

int play(uint64_t c)
{
	int move = 0;
	while (c > 1) {
		// test power of 2
		if ((c != 0) && ! (c & (c-1))) {
			c >>= 1;
		}
		// unset MSB
		else {
			uint64_t msb = 1;
			uint64_t tmp = c;
			while (tmp) {
				tmp >>= 1;
				msb <<= 1;
			}
			msb >>= 1; // started with 1 to get the bit going, so undo
			c &= ~msb;
		}

		move++;

	}
	return (move % 2);
}

int main()
{
	int n;
	cin >> n;

	while (n--) {
		uint64_t c;
		cin >> c;
		cout << (play(c) ? "Louise" : "Richard") << endl;
	}
}
