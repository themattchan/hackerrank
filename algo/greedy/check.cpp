#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>
using namespace std;

int main(int argc,char *argv[]) {
  if (argc != 3) return -1;
  ifstream in1(argv[1], ifstream::in);
  ifstream in2(argv[2], ifstream::in);
  int a, b;
  int i = 0;
  while (in1 >> a && in2 >> b) {
    i++;
    if (a != b) {
      cout << "NUM= " << i << "  FILE1=" << a << "  FILE2="  << b <<endl;
    }
  }
}
