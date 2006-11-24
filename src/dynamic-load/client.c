
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

void do_lib(char* libname){
  fprintf(stderr, "loading %s... ", libname);
  void* dh = dlopen(libname, RTLD_NOW | RTLD_LOCAL);
  if(dh == NULL){
    fprintf(stderr, "failed: %s\n", dlerror());
    exit(-1);
  }
  fprintf(stderr, "0x%08x\n", (int)dh);

  fprintf(stderr, "loading library_print ... ");
  int(*my_print)(char*) = dlsym(dh, "library_print");
  if(my_print == NULL){
    fprintf(stderr, "failed: %s\n", dlerror());
    exit(-1);
  }
  fprintf(stderr, "0x%08x\n", (int)my_print);

  fprintf(stderr, "Calling it ... ");
  my_print("Hello There");
  fprintf(stderr, "done\n");
}

int main(int argc, char** argv){
  do_lib("./library1.so");
  do_lib("./library2.so");
  do_lib("./library1.so");
  return 0;
}



