#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

int main() {
  int N,K;
  cin >> N >> K;

  int64_t luck = 0;
  vector<int> importants;
  for (int i=0; i<N; i++) {
    bool imp;
    int64_t myluck;
    cin >> myluck >> imp;
    if (imp) {
      importants.push_back(myluck);
    }
    luck += myluck;
  }

  // select K min important contests to win (subtract luck)
  sort(begin(importants), end(importants));
  if (K <= importants.size()) {
    for (int i = 0; i < importants.size()-K; i++) {
      luck -= (importants[i]<<1);
    }
  }
  cout << luck << endl;
}
