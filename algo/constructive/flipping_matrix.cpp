#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;


int main() {
  int q;
  cin >> q;
  while (q--) {
    int n;
    cin >> n;
    int M[2*n][2*n];
    for (int i = 0; i < 2*n; i++) {
      for (int j = 0; j < 2*n; j++) {
        cin >> M[i][j];
      }
    }
    int sum = 0;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        sum += max(M[i][j],
               max(M[i][2*n - j - 1],
               max(M[2*n - i - 1][j],
                   M[2*n - i - 1][2*n - j - 1]
                 )));
      }
    }
    cout << sum << endl;
  }
}
