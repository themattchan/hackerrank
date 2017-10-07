#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

int main() {
  int N,K;
  cin >> N >> K;

  int A[N+1];
  int I[N+1];

  for (int i=1; i<=N; i++) {
    cin >> A[i];
    I[A[i]] = i;
  }

  for (int i=0; i<K && i<N; i++) {
    // swap number N-i with number at front
    // 1. replace the entry in A of N-i with the thing at the head
    A[I[N-i]] = A[i+1];
    // 2. the thing prev at A[i+1] has moved to N-i's old loc
    I[A[i+1]] = I[N-1];
    // 3. actually move N-i up front
    A[i+1] = N-i;
    // 4. N-i is now at the front
    I[N-1] = i+1;
  }
  for (int i=1; i<=N; i++) cout << A[i] << " ";
  cout << endl;
}
