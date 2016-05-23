#include <cmath>
#include <climits>
#include <cstdio>
#include <vector>
#include <set>
#include <iostream>
#include <algorithm>
using namespace std;

constexpr unsigned int MOD { 1u << 31 };

int main()
{
	unsigned int N,S,P,Q;
    cin >> N >> S >> P >> Q;

	vector<bool> seen(MOD);
	unsigned long last = S % MOD;
	unsigned int unique = 1;
	seen[last] = true;

	for (unsigned int i = 1; i < N; i++) {
		last = (last * P + Q) % MOD;
		if (!seen[last]) {
			unique++;
			seen[last] = true;
		}
	}

	cout << unique << endl;
}
