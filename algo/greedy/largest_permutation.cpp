#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

int main() {
  int N,K;
  cin >> N >> K;

  int A[N];
  int I[N+1];

  for (int i=0; i<N; i++) {
    cin >> A[i];
    I[A[i]] = i;
  }


  // at MOST K swaps!
  // what is the stopping condition?
  for (int i=0; i<K && i<N; i++) {
    cout << "i IS " << i  <<endl;
    if (A[i] == N-i) continue;

    // swap number N-i with number at front
    // 1. replace the entry in A of N-i with the thing at the head
    A[I[N-i]] = A[i];
    // 2. the thing prev at A[i] has moved to N-i's old loc
    I[A[i]] = I[N-i];
    // 3. actually move N-i up front
    A[i] = N-i;
    // 4. N-i is now at the front
    I[N-i] = i;
  }

  for (int i=0; i<N; i++) cout << A[i] << " ";
}
