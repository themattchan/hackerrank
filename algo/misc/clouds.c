// jumping on the clouds
// https://www.hackerrank.com/challenges/jumping-on-the-clouds-revisited/problem

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define setbit(bv, bit) (bv |= (1 << bit))
#define unsetbit(bv, bit) (bv &= ~(1 << bit))
#define testbit(bv, bit) ((bv >> bit) & 1)

#define ispow2(x) ((((x) != 0) && (((x) & (x)-1) == 0)))

int main()
{
  int n, k;
  scanf("%d %d\n", &n, &k);

  uint32_t c = 0;
  {
    int tmp;
    for (int i = 0; i < n; i++) {
      scanf("%d", &tmp);
      if (tmp == 1) setbit(c,i);
    }
  }

  int e = 100;
  int cur = 0;
  do {
    cur = (cur + k) % n;
    e--;
    if (testbit(c, cur)) e -= 2;
    printf("cur is: %d  e is: %d testbit: %d\n", cur,e,(testbit(c, cur)));
  } while (cur != 0);

  printf("%d\n", e);

  return 0;
}
