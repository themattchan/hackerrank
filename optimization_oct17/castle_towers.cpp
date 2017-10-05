#include <bits/stdc++.h>

using namespace std;
int castleTowers(int n, vector <int> ar) {
  int maxint = 0;
  int count  = 0;
  for (int x : ar) {
    if (maxint == x)
      count++;
    else if (maxint < x)  {
      maxint = x;
      count = 1;
    }
  }
  return count;
}


int main() {
  int n;
  cin >> n;
  vector<int> ar(n);
  for(int ar_i = 0; ar_i < n; ar_i++){
    cin >> ar[ar_i];
  }
  int result = castleTowers(n, ar);
  cout << result << endl;
  return 0;
}
