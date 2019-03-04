/* designed pdf viewer */

#include <stdio.h>
#include <stdlib.h>

int main ()
{
  int ws[26] = {0};
  for (int i = 0; i < 26; i++) {
    scanf("%d",ws+i);
  }

  char s[11] = {0};
  scanf("%s", s);

  int m = 0;
  int i = 0;
  while (i < 10 && s[i] != 0) {
    int w = ws[s[i]-'a'];
    m = w > m ? w : m;
    i++;
  }

  printf("%d\n", m*i);
}
