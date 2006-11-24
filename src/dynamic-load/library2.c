
#include <stdio.h>

int library_print(char* x){
  fprintf(stderr, "LIB2: %s\n", x);
  return 0;
}

