// #include <bits/stdc++.h >
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <iostream>

using namespace std;


uint16_t collatzSequenceLen(uint64_t n) {
  if (n == 0) return 0;
  uint16_t len = 1;
  while (n > 1) {
    len++;
    if ((n & 1) == 1) {
      n = 3*n+1;
    } else if ((n & 1) == 0) {
      n = n >> 1;
    }
  }
  return len;
}

static uint16_t COLLATZ[5003];
void collatzSeqMemo() {
  for (int i = 0; i < 5003; i++) {
    COLLATZ[i] = collatzSequenceLen(i);
  }
}

uint32_t collatzSequenceSum(uint8_t T, uint16_t  A, uint16_t B) {
  collatzSeqMemo();
  uint16_t n = 0;
  uint32_t result = 0;
  while (T--) {
    n = (A*n + B) % 5003;
    uint32_t best_len = 0;
    uint16_t best_num = 0;
    for (uint16_t k = 0; k <= n; ++k) {
      uint32_t cur_len = COLLATZ[k];
      if (cur_len >= best_len) {
        best_len = cur_len;
        best_num = k;
      }
    }
    result += best_num;
  }
  return result;
}


// N[0] = 0
// for i = 1 to T:
//    N[i] = (A * N[i-1] + B) mod 5003
int main() {
  uint8_t T;
  uint16_t A;
  uint16_t B;
  cin >> T >> A >> B;
  cout << collatzSequenceSum(T, A, B) << endl;
  return 0;
}
