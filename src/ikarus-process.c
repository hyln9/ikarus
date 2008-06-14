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
#include <signal.h>
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

typedef struct signal_info {
  int n;
  ikptr c;
} signal_info;

#define signal_info_table_len 28

static signal_info signal_info_table[signal_info_table_len] = {
  /* Signals from POSIX */
  {SIGABRT,     fix(1)},
  {SIGALRM,     fix(2)},
  {SIGBUS,      fix(3)},
  {SIGCHLD,     fix(4)},
  {SIGCONT,     fix(5)},
  {SIGFPE,      fix(6)},
  {SIGHUP,      fix(7)},
  {SIGILL,      fix(8)},
  {SIGINT,      fix(9)},
  {SIGKILL,     fix(10)},
  {SIGPIPE,     fix(11)},
  {SIGQUIT,     fix(12)},
  {SIGSEGV,     fix(13)},
  {SIGSTOP,     fix(14)},
  {SIGTERM,     fix(15)},
  {SIGTSTP,     fix(16)},
  {SIGTTIN,     fix(17)},
  {SIGTTOU,     fix(18)},
  {SIGUSR1,     fix(19)},
  {SIGUSR2,     fix(20)},
#ifdef SIGPOLL
  {SIGPOLL,     fix(21)},
#else
  {SIGEMT,      fix(21)},
#endif
  {SIGPROF,     fix(22)},
  {SIGSYS,      fix(23)},
  {SIGTRAP,     fix(24)},
  {SIGURG,      fix(25)},
  {SIGVTALRM,   fix(26)},
  {SIGXCPU,     fix(27)},
  {SIGXFSZ,     fix(28)}
};


ikptr
ik_signal_num_to_code(int signum){
  signal_info* si;
  int i;
  for(i=0; i < signal_info_table_len; i++){
    si = &signal_info_table[i];
    if(si->n == signum){
      return si->c;
    }
  }
  fprintf(stderr, "\n*** ik_signal_num_to_code: Don't know signal %d ***\n\n", 
          signum);
  return fix(99999);
}

int
ik_signal_code_to_num(ikptr sigcode){
  signal_info* si;
  int i;
  for(i=0; i < signal_info_table_len; i++){
    si = &signal_info_table[i];
    if(si->c == sigcode){
      return si->n;
    }
  }
  fprintf(stderr, "ik_signal_code_to_num: Don't know code %ld\n",
          unfix(sigcode));
  exit(EXIT_FAILURE);
  return 0;
}

ikptr
ikrt_kill(ikptr pid, ikptr sigcode /*, ikpcb* pcb */){
  int r = kill((pid_t)unfix(pid), ik_signal_code_to_num(sigcode));
  if(r == 0){
    return fix(0);
  }
  return ik_errno_to_code();
}

ikptr 
ikrt_waitpid(ikptr rvec, ikptr pid, ikptr block /*, ikpcb* pcb */){
  /* rvec is assumed to come in as #(#f #f #f) */
  int status, options = 0;
  if(block == false_object){
    options = WNOHANG;
  }
  pid_t r = waitpid(unfix(pid), &status, options);
  if(r > 0){
    ref(rvec, off_record_data+0*wordsize) = fix(r);
    if(WIFEXITED(status)) {
      ref(rvec, off_record_data+1*wordsize) = fix(WEXITSTATUS(status));
    }
    if(WIFSIGNALED(status)) {
      ref(rvec, off_record_data+2*wordsize) = 
        ik_signal_num_to_code(WTERMSIG(status));
    }
    return rvec;
  } else if(r == 0){  /* would have blocked */
    return fix(0);
  } else {
    return ik_errno_to_code();
  }
}

