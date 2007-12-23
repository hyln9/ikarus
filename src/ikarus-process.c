#include <unistd.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ikarus-data.h"
#include <errno.h>

extern ikptr ikrt_io_error();

static int 
list_length(ikptr x){
  int n = 0;
  while(tagof(x) == pair_tag){
    n++;
    x = ref(x, off_cdr);
  }
  return n;
}

static char**
list_to_vec(ikptr x){
  int n = list_length(x);
  char** vec = malloc((n+1) * sizeof(char*));
  if (vec == NULL) exit(-1);
  int i;
  for(i=0; i<n; i++){
    vec[i] = (char*)ref(x, off_car) + off_bytevector_data;
    x = ref(x, off_cdr);
  }
  vec[n] = 0;
  return vec;
}

ikptr 
ikrt_process(ikptr rvec, ikptr cmd, ikptr argv, ikpcb* pcb){
  int infds[2];
  int outfds[2];
  int errfds[2];
  if(pipe(infds))  return ikrt_io_error();
  if(pipe(outfds)) return ikrt_io_error();
  if(pipe(errfds)) return ikrt_io_error();
  pid_t pid = fork();
  if(pid == 0){
    /* child */
    if(close(infds[1]))  exit(1);
    if(close(outfds[0])) exit(1);
    if(close(errfds[0])) exit(1);
    if(close(0))         exit(1);
    if(dup(infds[0]) == -1)  exit(1);
    if(close(1))         exit(1);
    if(dup(outfds[1]) == -1) exit(1);
    if(close(2))         exit(2);
    if(dup(errfds[1]) == -1) exit(1);
    execvp((char*)cmd+off_bytevector_data, list_to_vec(argv));
    fprintf(stderr, "failed to exec %s: %s\n", 
        (char*)cmd+off_bytevector_data,
        strerror(errno));
    exit(-1);
  } else if(pid > 0){
    /* parent */
    close(infds[0]); /* ignore errors */
    close(outfds[1]);
    close(errfds[1]);
    ref(rvec,off_vector_data+0*wordsize) = fix(pid);
    ref(rvec,off_vector_data+1*wordsize) = fix(infds[1]);
    ref(rvec,off_vector_data+2*wordsize) = fix(outfds[0]);
    ref(rvec,off_vector_data+3*wordsize) = fix(errfds[0]);
    return rvec;
  } else {
    return ikrt_io_error();
  }
}

ikptr 
ikrt_waitpid(ikptr pid, ikpcb* pcb){
  int status;
  waitpid(unfix(pid), &status, 0);
  return fix(status);
}
