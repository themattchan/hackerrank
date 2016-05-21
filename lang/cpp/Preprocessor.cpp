/* Enter your macros here */
#include <climits>
#define INF INT_MAX
#define toStr(x) #x
#define io cin >>
#define FUNCTION(name, a) void name(int& m, int v) { m a v ? v : m = v; }

#define foreach(container, var)									\
	for (int var = 0;											\
		 var < (container).size();								\
		 ++var)

#include <iostream>
#include <vector>
using namespace std;

#if !defined toStr || !defined io || !defined FUNCTION || !defined INF
#error Missing preprocessor definitions
#endif

FUNCTION(minimum, <)
FUNCTION(maximum, >)

int main(){
	int n; cin >> n;
	int i;
	vector<int> v(n);
	foreach(v, i) {
		io(v)[i];
	}
	int mn = INF;
	int mx = -INF;
	foreach(v, i) {
		minimum(mn, v[i]);
		maximum(mx, v[i]);
	}
	int ans = mx - mn;
	cout << toStr(Result =) <<' '<< ans;
	return 0;

}
