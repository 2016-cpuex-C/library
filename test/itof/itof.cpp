#include <iostream>
#include <cmath>
#include <cstdio>

using namespace std;

float itof(int i){
  int ai,ans=0;
  if(i<0) ai = -i;
  else ai = i;

  if(i<8388608){
    float fofi = ai + 8388608; //8388608を整数+で足して足されたものをfloatとして解釈
    ans = fofi - 8388608.0;　// fsub
  }
  if(i>=8388608){
    int n= ai/8388608, m = ai%8388608;
    for(int i=0;i<n;i++){
      ans += 8388608.0;
    }
    ans += itof(m);
  }

  if(i<0) return -ans;
  else return ans;
}


int main(){

  for(int i=0;i<100;i++){
    int s = 111111111;
    cout << s+i << " : ";
    printf("%8f \n" , itof(s+i));
}

  return 0;
}
