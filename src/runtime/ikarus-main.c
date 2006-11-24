

#include "ikarus.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <gmp.h>

int main(int argc, char** argv){
  if(argc < 2){
    fprintf(stderr, "insufficient arguments\n");
    exit(-1);
  }
  if(sizeof(mp_limb_t) != sizeof(int)){
    fprintf(stderr, "ERROR: limb size\n");
  }
  if(mp_bits_per_limb != (8*sizeof(int))){
    fprintf(stderr, "ERROR: bits_per_limb=%d\n", mp_bits_per_limb);
  }
  ikpcb* pcb = ik_make_pcb();
  int i;
  for(i=1; i<argc; i++){
    char* fasl_file = argv[i];
    ik_fasl_load(pcb, fasl_file);
  }
  fprintf(stderr, "collect time: %d.%03d utime, %d.%03d stime (%d collections)\n", 
   pcb->collect_utime.tv_sec, 
   pcb->collect_utime.tv_usec/1000, 
   pcb->collect_stime.tv_sec, 
   pcb->collect_stime.tv_usec/1000,
   pcb->collection_id );
  ik_delete_pcb(pcb);
  return 0;
}



