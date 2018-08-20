//# test llvmir generation
#include "stdio.h"
struct S{
  int a;
};

int main(){
  S s;
  if (s.a > 0){
    s.a = -s.a;
  }
  printf("%d", s.a);


}
