#include <bits/stdc++.h>

using namespace std;

int main(){
  int n;
  cin >> n;
  vector<int> calories(n);
  for (int i = 0; i < n; i++){
    cin >> calories[i];
  }
  // reverse sort; greatest first
  sort(calories.begin(), calories.end(), greater<int>());
  uint64_t miles = 0;
  uint64_t pow2 = 1;
  for (auto c : calories) {
    miles += c * pow2;
    pow2 <<= 1;
  }
  cout << miles << endl;
  return 0;
}
