#include <iostream>
#include <cmath>
#include <cstdio>

using namespace std;

int ftoi(float x){
  int ans=0;
  float ax;
  if(x<0) ax = -x;
  else ax = x;


  if(ax < 8388608.0){
    float xx = ax+8388608.0; 	//faddとしてたす 
    ans = (int)xx - 8388608;    //整数演算の引き算
  }
  else if(ax >= 8388608.0){
    int m=1; ax-=8388608.0;
    while(ax >= 8388608.0 ){
      ax -= 8388608.0;
      m++;
    }
    ans = m*8388608 + ftoi(ax);
  }

  if(x<0) return -ans;
  else return ans;

}

int main(){
  float x;
  scanf("%f", &x);
  cout << x <<" : " <<  ftoi(x) << " : " << endl;
  return 0;
}
