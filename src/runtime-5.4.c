
#include <stdio.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>                
#include <sys/types.h>                                     
#include <sys/stat.h>                                      
#include <fcntl.h>                                         

#include "scheme.h"
#define mask(x,m) (((int)(x)) & (int)(m))
#define unshift(n) (((int)(n)) >> fx_shift)
#define shift(n) ((ptr)((n) << fx_shift))
#define ref(x,i) (*((ptr*)((x)+(i))))
#define pagesize 4096

static void install_handlers();

static char* char_string[128] = {
  "#\\nul","#\\soh","#\\stx","#\\etx","#\\eot","#\\enq","#\\ack","#\\bel",
  "#\\bs", "#\\tab","#\\lf", "#\\vt", "#\\ff", "#\\cr", "#\\so", "#\\si",
  "#\\dle","#\\dc1","#\\dc2","#\\dc3","#\\dc4","#\\nak","#\\syn","#\\etb",
  "#\\can","#\\em", "#\\sub","#\\esc","#\\fs", "#\\gs", "#\\rs", "#\\us",
  "#\\space","#\\!","#\\\"","#\\#","#\\$","#\\%","#\\&","#\\'",
  "#\\(","#\\)","#\\*","#\\+","#\\,","#\\-","#\\.","#\\/",
  "#\\0","#\\1","#\\2","#\\3","#\\4","#\\5","#\\6","#\\7",
  "#\\8","#\\9","#\\:","#\\;","#\\<","#\\=","#\\>","#\\?",
  "#\\@","#\\A","#\\B","#\\C","#\\D","#\\E","#\\F","#\\G",
  "#\\H","#\\I","#\\J","#\\K","#\\L","#\\M","#\\N","#\\O",
  "#\\P","#\\Q","#\\R","#\\S","#\\T","#\\U","#\\V","#\\W",
  "#\\X","#\\Y","#\\Z","#\\[","#\\\\","#\\]","#\\^","#\\_",
  "#\\`","#\\a","#\\b","#\\c","#\\d","#\\e","#\\f","#\\g",
  "#\\h","#\\i","#\\j","#\\k","#\\l","#\\m","#\\n","#\\o",
  "#\\p","#\\q","#\\r","#\\s","#\\t","#\\u","#\\v","#\\w",
  "#\\x","#\\y","#\\z","#\\{","#\\|","#\\}","#\\~","#\\del"};

static ptr s_car(ptr x){
  ptr* p = (ptr*)(x - pair_tag);
  return p[0];
}
static ptr s_cdr(ptr x){
  ptr* p = (ptr*)(x - pair_tag);
  return p[1];
}

static void print_object(ptr x);

static void print_rest(ptr x){
  while(1){
    if (x == empty_list) {
      return;
    } else if(mask(x,pair_mask) == pair_tag){
      printf(" ");
      print_object(s_car(x));
      x = s_cdr(x);
    } else {
      printf(" . ");
      print_object(x);
      return;
    }
  }
}
  
static void print_vector(ptr p){
  int len = (int) ref(p, disp_vector_length - vector_tag);
  int i;
  printf("#(");
  if(len > 0){
    print_object(ref(p, disp_vector_data - vector_tag));
    for(i=disp_vector_data; i<len; i+=wordsize){
      printf(" ");
      print_object(ref(p, disp_vector_data + i - vector_tag));
    }
  }
  printf(")");
}
  
static void print_string(ptr x){
  int len = (int) ref(x, disp_string_length - string_tag);
  len = unshift(len);
  printf("\"");
  int i;
  for(i=0; i<len; i++){
    char c = x[disp_string_data + i - string_tag];
    if((c == '"') || (c == '\\')){
      printf("\\");
    }
    printf("%c", c);
  }
  printf("\"");
}

static void print_symbol(ptr x){
  ptr str = ref(x, disp_symbol_string - symbol_tag);
  int len = (int) ref(str, disp_string_length - string_tag);
  len = unshift(len);
  int i;
  for(i=0; i<len; i++){
    char c = str[disp_string_data + i - string_tag];
    printf("%c", c);
  } 
}

static void print_object(ptr x){
  if(mask(x, fx_mask) == fx_tag){
    printf("%d", unshift(x));
  } else if(x == bool_f){
    printf("#f");
  } else if(x == bool_t){
    printf("#t");
  } else if(x == empty_list){
    printf("()");
  } else if(mask(x,char_mask) == char_tag){
    printf("%s", char_string[((int)x >> char_shift) & 127]);
  } else if(((int)x & pair_mask) == pair_tag){
    printf("(");
    print_object(s_car(x));
    print_rest(s_cdr(x));
    printf(")");
  } else if(mask(x, vector_mask) == vector_tag){
    print_vector(x);
  } else if(mask(x, string_mask) == string_tag){
    print_string(x);
  } else if(mask(x, symbol_mask) == symbol_tag){
    print_symbol(x);
  } else {
    printf("#<unknown 0x%08x>", (int)x);
  }
}

static void print_ptr(ptr x){
  print_object(x);
  printf("\n");
  return;
  print_ptr(x);
}

static char* allocate_unprotected_space(int size){
  int aligned_size = ((size + pagesize - 1) / pagesize) * pagesize;
  char* p = mmap(0, aligned_size,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if(p == MAP_FAILED){
    perror("allocate_unprotected_space failed to mmap");
    exit(-10);
  }
  return p;
}



static char* allocate_protected_space(int size){
  int status;
  int aligned_size = ((size + pagesize - 1) / pagesize) * pagesize;
  char* p = mmap(0, aligned_size + 2*pagesize,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if(p == MAP_FAILED){
    perror("allocate_protected_space failed to mmap");
    exit(-10);
  }
  status = mprotect(p, pagesize, PROT_NONE);
  if(status != 0){
    perror("allocate_protected_space failed to protect");
    exit(-10);
  }
  status = mprotect(p+pagesize+aligned_size, pagesize, PROT_NONE);
  if(status != 0){
    perror("allocate_protected_space failed to protect");
    exit(-10);
  }
  return (p + pagesize);
}

static void deallocate_unprotected_space(char* p, int size){
  int status;
  int aligned_size = ((size + pagesize - 1) / pagesize) * pagesize;
  status = munmap(p, aligned_size);
  if(status != 0){
    perror("deallocate_unprotected_space failed to unmap");
    exit(-10);
  }
}

static void deallocate_protected_space(char* p, int size){
  int status;
  int aligned_size = ((size + pagesize - 1) / pagesize) * pagesize;
  status = munmap(p-pagesize, aligned_size + 2*pagesize);
  if(status != 0){
    perror("deallocate_protected_space failed to unmap");
    exit(-10);
  }
}

char* global_heap_start;
char* global_heap_end;
char* global_stak_start;
char* global_stak_end;

int main(int argc, char** argv){
  install_handlers();
  if (pagesize != getpagesize()){
    fprintf(stderr, "Pagesize Mismatch\n");
    exit(-1);
  }
  int stak_size = (256 * pagesize); /* holds 128k cells */
  int heap_size =  (640 * pagesize); /* holds 1M cells */
  char* heap = allocate_unprotected_space(heap_size);
  char* stak = allocate_unprotected_space(stak_size);
  global_heap_start = heap;
  global_stak_start = stak;
  global_heap_end = heap+heap_size;
  global_stak_end = stak+stak_size;
  pcb_t* pcb = (pcb_t*) allocate_protected_space(sizeof(pcb_t));
  pcb->heap_base = heap;
  pcb->heap_size = (char*) heap_size;
  pcb->allocation_pointer = heap;
  pcb->allocation_redline = heap + heap_size - 2 * pagesize;
  pcb->stack_top = stak;
  pcb->stack_size = (char*) stak_size;
  pcb->frame_redline = stak + 2 * pagesize;
  pcb->frame_base = stak + stak_size - wordsize;
#if 0
  fprintf(stderr, "stack=0x%08x .. 0x%08x (redline=0x%08x)\n", 
      (int) pcb->frame_base,
      (int) pcb->stack_top,
      (int) pcb->frame_redline);
#endif
  scheme_main(pcb);
#if 0
  fprintf(stderr, "%d bytes used\n", pcb->allocation_pointer - heap);
#endif
  deallocate_unprotected_space(stak, stak_size);
  deallocate_unprotected_space(pcb->heap_base, (int) pcb->heap_size);
  deallocate_protected_space((char*)pcb, sizeof(pcb_t));
  return 0;
}


ptr S_error(ptr args){
  if(mask(args,pair_mask) != pair_tag){
    fprintf(stderr, "Error in Error: no arguments\n");
    exit(-1);
  }
  ptr fst = ref(args, disp_car - pair_tag);
  if(fst == bool_f){
    fprintf(stderr, "Error\n");
    exit(0);
  }
  if(mask(fst, symbol_mask) == symbol_tag){
    ptr str = ref(fst, disp_symbol_string - symbol_tag);
    fprintf(stderr, "Error in %s\n", str+disp_string_data-string_tag);
    exit(0);
  } 
  fprintf(stderr, "Invalid argument 0x%08x to S_error\n", (int)fst);
  exit(-1);
}

/* 
 * From the manpages:
 *
 * int open(const char *pathname, int flags);                 
 * int open(const char *pathname, int flags, mode_t mode);    
 * flags = (O_RDONLY | O_WRONLY | O_RDWR) ? O_CREAT ? O_TRUNC ? O_APPEND
 * return -1 on failure
 * 
 * int unlink(const char *pathname);  
 */

ptr S_open_file(ptr str, ptr flagptr){
  int flags;
  int f = unshift(flagptr);
  char* path = str + disp_string_data - string_tag;
  if(f == 0){
    flags = O_WRONLY;
  } else if(f == 1){
    flags = O_WRONLY | O_APPEND;
  } else if(f == 2){
    unlink(path);
    flags = O_WRONLY | O_CREAT;
  } else if(f == 3){
    flags = O_WRONLY | O_TRUNC;
  } else if(f == 4){
    flags = O_RDONLY;
  } else {
    fprintf(stderr, "Error in S_open_file: invalid mode 0x%08x\n", 
                  (int)flagptr);
    exit(-10);
  }
  int fd = open(path, flags, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if(fd == -1){
     fprintf(stderr, "Cannot open %s\n", path);
     perror("S_open_file");
     exit(-10);
  }
  if(fd != unshift(shift(fd))){
     fprintf(stderr, "fd %d too big\n", fd);
     exit(-10);
  }
  return shift(fd);
}
  
ptr S_write(ptr fdptr, ptr idx, ptr str){
  int fd = unshift(fdptr);
  int len = unshift(idx);
  char* buf = str+disp_string_data-string_tag;
  int bytes = write(fd, buf, len);
  if(bytes != len){
    perror("S_write");
    exit(-10);
  }
  return bool_t;
} 

ptr S_read(ptr fdptr, ptr bufptr, ptr lenptr){
  int fd = unshift(fdptr);
  int len = unshift(lenptr);
  char* buf = bufptr+disp_string_data-string_tag;
  int bytes = read(fd, buf, len);
  if(bytes == -1){
    perror("S_read");
    exit(-10);
  }
  return shift(bytes);
} 

ptr S_close(ptr fd){
  int err = close(unshift(fd));
  if(err != 0){
    perror("S_close");
    exit(-10);
  }
  return bool_t;
}

/*
 * From the manpage:
 *
 * int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact);
 * int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
 * int sigpending(sigset_t *set);
 * int sigsuspend(const sigset_t *mask);

struct sigaction {
  void (*sa_handler)(int);
  void (*sa_sigaction)(int, siginfo_t *, void *);
  sigset_t sa_mask;
  int sa_flags;
  void (*sa_restorer)(void);
}

siginfo_t {                                                      
    int      si_signo;
    int      si_errno;
    int      si_code;
    pid_t    si_pid;
    uid_t    si_uid;
    int      si_status;
    clock_t  si_utime;
    clock_t  si_stime;
    sigval_t si_value;
    int      si_int;
    void *   si_ptr;
    void *   si_addr;
    int      si_band;
    int      si_fd;
}                                                                
si_code:
SEGV_MAPERR | address not mapped to object          
SEGV_ACCERR | invalid permissions for mapped object 

*/

void segv_handler(int signum, siginfo_t * info, void* ctxt){
  fprintf(stderr, "segv cought\n");
  fprintf(stderr, "address of fault = 0x%08x\n", (int) info->si_addr);
  fprintf(stderr, "heap is 0x%08x .. 0x%08x\n", 
                  (int) global_heap_start,
                  (int) global_heap_end - 1);
  fprintf(stderr, "stak is 0x%08x .. 0x%08x\n", 
                  (int) global_stak_start,
                  (int) global_stak_end - 1);
  fprintf(stderr, "caused by: %s\n", 
              (info->si_code == SEGV_MAPERR)
              ? "unmapped object" 
              : "invalid permission");
  
  exit(-10);
}

static void install_handlers(){
  return;
  stack_t ss;
  ss.ss_sp = malloc(SIGSTKSZ);
  if (ss.ss_sp == NULL){
    fprintf(stderr, "Error allocating altstack\n");
    exit(-10);
  }
  ss.ss_size = SIGSTKSZ;            
  ss.ss_flags = 0;                  
  if (sigaltstack(&ss, NULL) == -1) {
    fprintf(stderr, "Error installing altstack\n");
    exit(-10);
  }
  
  struct sigaction act;
  sigset_t set;
  sigemptyset(&set);
  act.sa_sigaction = segv_handler;
  act.sa_flags = SA_RESTART | SA_SIGINFO | SA_ONSTACK;
  act.sa_mask = set;
  int err = sigaction(SIGSEGV, &act, NULL);
  if(err){
    perror("installing handlers failed");
    exit(-10);
  }
}

