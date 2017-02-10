#include <iostream>
#include <cmath>

#define PI 3.1415926535

using namespace std;


float kernel_atan(float x){
  float x2,x3,x5,x7,x9,x11,x13;
  x2 = x*x;
  x3 = x*x2;
  x5 = x2*x3;
  x7 = x2*x5;
  x9 = x2*x7;
  x11 = x2*x9;
  x13 = x2*x11;

  return x - 0.333333*x3 + 0.2*x5 - 0.142857142*x7 + 0.111111104*x9 - 0.08976446*x11 +0.060035485 * x13;

}

float Atan(float x){
  float ax,ans;
  ax = x<0 ? -x : x;
  if(ax <= 0.4375) ans=kernel_atan(x);
  else if(ax <= 2.4375){
   ans =  PI / 4 + kernel_atan((ax-1.0)/(ax+1.0));
   if(x<0) ans = -ans;
  }
  else {
    ans = PI/2 - kernel_atan(1/ax);
    if(x<0) ans = -ans;
  }
  return ans;
}

int main(){
  float x = -2*PI;
  float d = PI/50;
  for(int i =0; i<200;i++){
    cout << x << " : "<< Atan(x) << " : " << atan(x) << " : " << Atan(x)-atan(x) << endl;
    x+=d;
  }
  return 0;
}
