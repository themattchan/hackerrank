#include <bits/stdc++.h>

using namespace std;

int minimumAbsoluteDifference(int n, vector<int> arr) {
  sort(arr.begin(), arr.end());
  int m = abs(arr[0]-arr[1]);
  for (int i = 1; i < n-1; i++) {
    m = min(m, abs(arr[i]-arr[i+1]));
  }
  return m;
}

int main() {
  int n;
  cin >> n;
  vector<int> arr(n);
  for (int i = 0; i < n; i++){
    cin >> arr[i];
  }
  int result = minimumAbsoluteDifference(n, arr);
  cout << result << endl;
  return 0;
}
