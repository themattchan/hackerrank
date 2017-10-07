#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

// takeaway game, normal play, sizes 2,3,5
// N = first player win
// P = second player win
// all terminal pos. are P
// all moves from P are to N
// for each N, exists move to P

// 0 is P
// 2,3,5 is N
// backwards induction
// 0 1 2 3 4 5 6 | 7 8 9 10 11 12 13 | 14 15
// P P N N N N N | P P N N  N  N  N  | P  P
// ^   ^ ^   ^
int solve(int n) {
  int m = n % 7;
  return (m == 0 || m == 1);
}

int main() {
  int T;
  cin >> T;
  while (T--) {
    int n;
    cin >> n;
    cout << (solve(n) ? "Second" : "First") << endl;
  }
  return 0;
}
