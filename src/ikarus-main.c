/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007  Abdulaziz Ghuloum
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *  
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */




#include "ikarus-data.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <gmp.h>
#include <signal.h>
#include <sys/mman.h>

void register_handlers();
void register_alt_stack();

void ikarus_usage_short(){
  fprintf(stderr, "ikarus -h for more help\n");
}

void ikarus_usage(){
  static char* helpstring = 
"\n\
Options for running ikarus scheme:\n\
\n  ikarus -h\n\
    Prints this help message then exits.\n\
\n  ikarus [-b <bootfile>] --r6r-script <scriptfile> opts ...\n\
    Starts ikarus in r6rs-script mode.  The script file is treated\n\
    as an R6RS-script.  The options opts ... can be obtained using\n\
    the \"command-line\" procedure in the (rnrs programs) library.\n\
\n  ikarus [-b <bootfile>] <file> ... [-- opts ...]\n\
    Starts ikarus in interactive mode.  Each of the files is first\n\
    loaded into the interaction environment before the interactive\n\
    repl is started.  The options opts can be obtained using the\n\
    \"command-line\" procedure.\n\
  \n\
  If the option [-b <bootfile>] is provided, the bootfile is used\n\
  as the system's initial boot file from which the environment is\n\
  initialized.  If the -b option is not supplied, the default boot\n\
  file (ikarus.boot if the executable name is ikarus) is used based\n\
  on where the executable file is located in the PATH.  If ikarus\n\
  was invoked using a path (e.g. ./ikarus or /bin/ikarus), then the\n\
  PATH is not searched, instead, the path to the executable is used\n\
  to locate the boot file (e.g. ./ikarus.boot or /bin/ikarus.boot).\n\
  Consult the ikarus manual for more details.\n\n";
  fprintf(stderr, helpstring);
}


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
        fprintf(stderr, 
                "ikarus error: option %s requires a value, none provided\n",
                opt);
        ikarus_usage_short();
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
get_option0(char* opt, int argc, char** argv){
  int i;
  for(i=1; i<argc; i++){
    if(strcmp(opt, argv[i]) == 0){
      int j;
      for(j=i+1; j<argc; j++, i++){
          argv[i] = argv[j];
        }
      return 1;
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

static char* mystpcpy(char* x, char* y){
  while(*y){
    *x = *y;
    x++; y++;
  }
  *x = 0;
  return x;
}


int main(int argc, char** argv){
  if(get_option0("-h", argc, argv)){
    ikarus_usage();
    exit(0);
  }
  char buff[FILENAME_MAX];
  char* boot_file = get_option("-b", argc, argv);
  if(boot_file){
    argc -= 2;
  }
  else if(global_exe(argv[0])){
    /* search path name */
    char* path = getenv("PATH");
    if(path == NULL){
      fprintf(stderr,
              "ikarus: unable to locate boot file in PATH=%s\n",
              path);
      ikarus_usage_short();
      exit(-1);
    }
    while(*path){
      int len = copy_fst_path(buff, path);
      char* x = buff + len;
      x = mystpcpy(x, "/");
      x = mystpcpy(x, argv[0]);
      if(file_exists(buff)){
        x = mystpcpy(x, ".boot");
        boot_file = buff;
        path = "";
      }
      else {
        if(path[len]){
          path += (len+1);
        } else {
          fprintf(stderr,
                  "ikarus: unable to locate executable \"%s\" in PATH=%s\n",
                  argv[0],
                  getenv("PATH"));
          ikarus_usage_short();
          exit(-1);
        }
      }
    }
  }
  else {
    char* x = buff;
    x = mystpcpy(x, argv[0]);
    x = mystpcpy(x, ".boot");
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
      ikp str = ik_alloc(pcb, align(n*string_char_size+disp_string_data+1))
                + string_tag;
      ref(str, off_string_length) = fix(n);
      {
        int i;
        for(i=0; i<n; i++){
          string_set(str, i, integer_to_char(s[i]));
        }
      }
      ikp p = ik_alloc(pcb, pair_size);
      ref(p, disp_car) = str;
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
Notice how the bsd manpages have incorrect type for the handler.

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

void handler(int signo, siginfo_t* info, void* uap){
  the_pcb->engine_counter = -1;
  the_pcb->interrupted = 1;
}

void
register_handlers(){
  struct sigaction sa;
  sa.sa_sigaction = handler;
#ifdef __CYGWIN__
  sa.sa_flags = SA_SIGINFO;
#else
  sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
#endif
  sigemptyset(&sa.sa_mask);
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
#ifndef __CYGWIN__
  char* stk = mmap(0, SIGSTKSZ, PROT_READ|PROT_WRITE|PROT_EXEC, 
                   MAP_PRIVATE|MAP_ANON, -1, 0);
//  char* stk = ik_mmap(SIGSTKSZ);
  if(stk == (char*)-1){
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
#endif
}
