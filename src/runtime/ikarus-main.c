

#include "ikarus.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>


int main(int argc, char** argv){
  if(argc < 2){
    fprintf(stderr, "insufficient arguments\n");
    exit(-1);
  }
  ikpcb* pcb = ik_make_pcb();
  int i;
  for(i=1; i<argc; i++){
    char* fasl_file = argv[i];
    ik_fasl_load(pcb, fasl_file);
  }
  ik_delete_pcb(pcb);
  return 0;
}



