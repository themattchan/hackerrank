#include <stdio.h>

int main()
{
  int s; 
  int t; 
  scanf("%d %d",&s,&t);
  int a; 
  int b; 
  scanf("%d %d",&a,&b);
  int m; 
  int n; 
  scanf("%d %d",&m,&n);

  int apples = 0;
  for (int i = 0; i < m; i++) {
    int apple;
    scanf("%d",&apple);
    if (a + apple >= s && a + apple <= t) apples++;
  }
  printf("%d\n",apples);
  
  int oranges = 0;
  for (int i = 0; i < n; i++) {
    int orange;
    scanf("%d",&orange);
    if (b + orange >= s && b + orange <= t) oranges++;
  }
  printf("%d\n",oranges);

  return 0;
}

