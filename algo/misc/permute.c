// permutation-equation
// https://www.hackerrank.com/challenges/permutation-equation/problem

#include <stdio.h>
#include <stdlib.h>

int main()
{
  int n;
  scanf("%d\n", &n);

  /*
  // sample input
  // p(1) = 2, p(2) = 3, p(3) = 1
  int P[n]; // i -> p(i)
  int Q[n]; // p(x) -> x
  for (int i = 0; i < n; i++) {
    scanf("%d\n", P+i); // if p(i) = j
    Q[P[i]] = i; // then q(j) = i
  }

  // for each x find the INDEX OF P that maps to x, i.e. lookup in Q.
  //

  for (int x = 0; x < n; x++) {
    printf("%d\n", Q[Q[x]]);
  }
  */


  int Q[n+1];
  for (int i = 1; i <= n; i++) {
    int tmp;
    scanf("%d", &tmp);
    Q[tmp] = i;
  }

  for (int x = 1; x <= n; x++) {
    printf("%d\n", Q[Q[x]]);
  }

  return 0;
}
