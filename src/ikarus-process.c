/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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


#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "ikarus-data.h"

extern ikptr ik_errno_to_code();

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
    vec[i] = (char*)(long)ref(x, off_car) + off_bytevector_data;
    x = ref(x, off_cdr);
  }
  vec[n] = 0;
  return vec;
}

ikptr 
ikrt_process(ikptr rvec, ikptr cmd, ikptr argv /*, ikpcb* pcb */){
  int infds[2];
  int outfds[2];
  int errfds[2];
  if(pipe(infds))  return ik_errno_to_code();
  if(pipe(outfds)) return ik_errno_to_code();
  if(pipe(errfds)) return ik_errno_to_code();
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
    execvp((char*)(long)(cmd+off_bytevector_data), list_to_vec(argv));
    fprintf(stderr, "failed to exec %s: %s\n", 
        (char*)(long)(cmd+off_bytevector_data),
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
    return ik_errno_to_code();
  }
}

ikptr 
ikrt_waitpid(ikptr pid /*, ikpcb* pcb */){
  int status;
  pid_t r = waitpid(unfix(pid), &status, 0);
  if(r >= 0){
    return fix(status);
  } else {
    return ik_errno_to_code();
  }
}

