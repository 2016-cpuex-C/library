#include <iostream>
#include <cmath>

#define PI 3.1415926535

using namespace std;

float red2pi(float x){
  float p = 2 * PI;
  while(x >= p){
    p = p * 2;
  }
  while(x >= PI * 2){
    if(x >= p){
      x = x - p;
    }
    p = p / 2;
  }
  return x;
}

float kernel_cos(float x){
  float x2,x4,x6;
  x2 = x * x;
  x4 = x2 * x2;
  x6 = x4 * x2;
  return 1 - 0.499998521812 *x2 + 0.041668949132*x4 - 0.001386642456*x6;
}
float kernel_sin(float x){
  float x2,x3,x5,x7;
  x2 = x * x;
  x3 = x * x2;
  x5 = x2 * x3;
  x7 = x2 * x5;

  return x - 0.16666625976 * x3 + 0.008333557129 * x5 - 0.000198896484 * x7;
}

float redquopi(float x,int flag){
  float ans;
  if(x >= PI) ans = - redquopi(x-PI,flag);
  else if(x >= PI / 2){
    if(!flag) ans = - redquopi(PI-x,flag); 
    else ans = redquopi(PI-x, flag);}
  else if(x <= PI / 4){
    if(flag) ans = kernel_sin(x);
    else ans = kernel_cos(x);
  }
  else if(x >= PI / 4){
    if(flag) ans = kernel_cos(PI / 2 - x);
    else ans = kernel_sin(PI / 2 - x);
  }
  return ans;

}


float Cos(float x){
  if (x < 0) x = -x;
  float xred2pi = red2pi(x);
  float ans = redquopi(xred2pi,0); 
  return ans;
}

float Sin(float x){
  int flag = 1;
  if(x < 0){x = -x; flag=-1;}
  float xred2pi = red2pi(x);
  float ans = redquopi(xred2pi,1);
  return flag*ans;
}

int main(){
  float x= - 2 * PI;
  float d = 4 * PI / 200.0;
  for(int i = 0; i < 200; i++){
    cout << x << " : "<< Sin(x) << ":" << sin(x) << ":" << Sin(x)-sin(x) << endl;
    x += d;
  }
}













