

#include "ikarus.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <gmp.h>
#include <signal.h>

void register_handlers();
void register_alt_stack();

ikpcb* the_pcb;

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

int
file_exists(char* filename){
  struct stat sb;
  int s = stat(filename, &sb);
  return (s == 0);
}

int
copy_fst_path(char* buff, char* x){
  int i = 0;
  while(1){
    char c = x[i];
    if ((c == 0) || (c == ':')){
      return i;
    } 
    buff[i] = c;
    i++;
  }
}

int global_exe(char* x){
  while(1){
    char c = *x;
    if(c == 0){
      return 1;
    } 
    if(c == '/'){
      return 0;
    }
    x++;
  }
}

int main(int argc, char** argv){
  char buff[FILENAME_MAX];
  char* boot_file = get_option("-b", argc, argv);
  if(boot_file){
    argc -= 2;
  }
  else if(global_exe(argv[0])){
    /* search path name */
    char* path = getenv("PATH");
    if(path == NULL){
      fprintf(stderr, "unable to locate boot file\n");
      exit(-1);
    }
    while(*path){
      int len = copy_fst_path(buff, path);
      char* x = buff + len;
      x = stpcpy(x, "/");
      x = stpcpy(x, argv[0]);
      if(file_exists(buff)){
        x = stpcpy(x, ".boot");
        boot_file = buff;
        path = "";
      }
      else {
        if(path[len]){
          path += (len+1);
        } else {
          fprintf(stderr, "unable to locate %s\n", argv[0]);
          exit(-1);
        }
      }
    }
  }
  else {
    char* x = buff;
    x = stpcpy(x, argv[0]);
    x = stpcpy(x, ".boot");
    boot_file = buff;
  }



  if(sizeof(mp_limb_t) != sizeof(int)){
    fprintf(stderr, "ERROR: limb size\n");
  }
  if(mp_bits_per_limb != (8*sizeof(int))){
    fprintf(stderr, "ERROR: bits_per_limb=%d\n", mp_bits_per_limb);
  }
  ikpcb* pcb = ik_make_pcb();
  the_pcb = pcb;
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
  register_handlers();
  register_alt_stack();
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

#if 0
     #include <signal.h>

     struct  sigaction {
             union {
                     void    (*__sa_handler)(int);
                     void    (*__sa_sigaction)(int, struct __siginfo *, void *);
             } __sigaction_u;                /* signal handler */
             int     sa_flags;               /* see signal options below */
             sigset_t sa_mask;               /* signal mask to apply */
     };

     #define sa_handler      __sigaction_u.__sa_handler
     #define sa_sigaction    __sigaction_u.__sa_sigaction

     int
     sigaction(int sig, const struct sigaction * restrict act,
         struct sigaction * restrict oact);
#endif

void handler(int signo, struct __siginfo* info, ucontext_t* uap){
  the_pcb->engine_counter = -1;
  the_pcb->interrupted = 1;
}

void
register_handlers(){
  struct sigaction sa;
  sa.sa_sigaction = (void(*)(int,struct __siginfo*,void*)) handler;
  sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sa.sa_mask = 0;
  int err = sigaction(SIGINT, &sa, 0);
  if(err){
    fprintf(stderr, "Sigaction Failed: %s\n", strerror(errno));
    exit(-1);
  }
}


#if 0
SYNOPSIS
     #include <sys/types.h>
     #include <signal.h>

     struct sigaltstack {
             char   *ss_sp;
             int     ss_size;
             int     ss_flags;
     };

     int
     sigaltstack(const struct sigaltstack *ss, struct sigaltstack *oss);
#endif

void
register_alt_stack(){
  char* stk = ik_mmap(SIGSTKSZ);
  if(stk == 0){
    fprintf(stderr, "Cannot maloc an alt stack\n");
    exit(-1);
  }

  struct sigaltstack sa;
  sa.ss_sp = stk;
  sa.ss_size = SIGSTKSZ;
  sa.ss_flags = 0;
  int err = sigaltstack(&sa, 0);
  if(err){
    fprintf(stderr, "Cannot set alt stack: %s\n", strerror(errno));
    exit(-1);
  }
}
