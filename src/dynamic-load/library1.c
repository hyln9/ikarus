
#include <stdio.h>

int library_print(char* x){
  fprintf(stderr, "LIB1: %s\n", x);
  return 0;
}

