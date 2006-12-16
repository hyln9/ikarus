#include <stdio.h>

long long foo(long long int x){
  return x+1;
}

#include <stdlib.h>
int main(int argc, char** argv){
  fprintf(stderr, "sizeof(long long int)=%ld\n", 
      sizeof(long long int));
  long long int x = 57;
  x = foo(x);
  exit(-1);
}
