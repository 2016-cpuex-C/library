#include <cstdio>
#include <iostream>

using namespace std;

union IaF 
{
  int i;
  float f;
};

int main(){
  IaF a;
  a.f = 8388608.0;
  printf("%d\n", a.i);

  return 0;
}
