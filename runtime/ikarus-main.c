

#include "ikarus.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <gmp.h>

/* get_option
   
   takes pointers to argc and argv and looks for the first
   option matching opt.  If one exists, it removes it from the argv
   list, updates argc, and returns a pointer to the option value.
   returns null if option is not found.
   */
char* 
get_option(char* opt, int argc, char** argv){
  int i;
  for(i=1; i<argc; i++){
    if(strcmp(opt, argv[i]) == 0){
      if((i+1) < argc){
        char* rv = argv[i+1];
        int j;
        for(j=i+2; j<argc; j++, i++){
          argv[i] = argv[j];
        }
        return rv;
      } 
      else {
        fprintf(stderr, "Error: option %s not provided\n", opt);
        exit(-1);
      }
    }
    else if(strcmp("--", argv[i]) == 0){
      return 0;
    }
  }
  return 0;
}


int main(int argc, char** argv){
  char ikarus_boot[FILENAME_MAX];
  char* boot_file = get_option("-b", argc, argv);
  if(boot_file){
    argc -= 2;
  } else {
    boot_file = ikarus_boot;
    char* x = ikarus_boot;
    x = stpcpy(x, argv[0]);
    x = stpcpy(x, ".boot");
  }

  if(sizeof(mp_limb_t) != sizeof(int)){
    fprintf(stderr, "ERROR: limb size\n");
  }
  if(mp_bits_per_limb != (8*sizeof(int))){
    fprintf(stderr, "ERROR: bits_per_limb=%d\n", mp_bits_per_limb);
  }
  ikpcb* pcb = ik_make_pcb();
  { /* set up arg_list */
    ikp arg_list = null_object;
    int i = argc-1;
    while(i > 0){
      char* s = argv[i];
      int n = strlen(s);
      ikp str = ik_alloc(pcb, align(n+disp_string_data+1));
      ref(str, disp_string_length) = fix(n);
      strcpy((char*)str+disp_string_data, s);
      ikp p = ik_alloc(pcb, pair_size);
      ref(p, disp_car) = str + string_tag;
      ref(p, disp_cdr) = arg_list;
      arg_list = p+pair_tag;
      i--;
    }
    pcb->arg_list = arg_list;
  }
  ik_fasl_load(pcb, boot_file);
  /*
  fprintf(stderr, "collect time: %d.%03d utime, %d.%03d stime (%d collections)\n", 
                  pcb->collect_utime.tv_sec, 
                  pcb->collect_utime.tv_usec/1000, 
                  pcb->collect_stime.tv_sec, 
                  pcb->collect_stime.tv_usec/1000,
                  pcb->collection_id );
                  */
  ik_delete_pcb(pcb);
  return 0;
}



