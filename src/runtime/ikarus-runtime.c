#include "ikarus.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <strings.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>
#include <assert.h>

#include <uuid/uuid.h>

int total_allocated_pages = 0;

void* 
ik_mmap(int size){
  int pages = (size + pagesize - 1) / pagesize;
  total_allocated_pages += pages;
  int mapsize = pages * pagesize;
  assert(size == mapsize);
  char* mem = mmap(
      0,
      mapsize,
      PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_ANONYMOUS,
      -1,
      0);
  if(mem == MAP_FAILED){
    fprintf(stderr, "Mapping failed: %s\n", strerror(errno));
    exit(-1);
  }
  memset(mem, -1, mapsize);
  return mem;
}

void
ik_munmap(void* mem, int size){
  int pages = (size + pagesize - 1) / pagesize;
  int mapsize = pages * pagesize;
  assert(size == mapsize);
  assert(((-pagesize) & (int)mem) == (int)mem);
//int err = mprotect(mem, mapsize, PROT_NONE);
//if(err != 0){
//  fprintf(stderr, "error protecting unmapped pages\n");
//  exit(-1);
//}
  total_allocated_pages -= pages;
  int err = munmap(mem, mapsize);
  if(err != 0){
    fprintf(stderr, "ik_munmap failed: %s\n", strerror(errno));
    exit(-1);
  }
}

int total_malloced = 0;

void* 
ik_malloc(int size){
  void* x = malloc(size);
  if(x == NULL){
    fprintf(stderr, "malloc failed: %s\n", strerror(errno));
    exit(-1);
  }
  total_malloced += size;
  return x;
}

void ik_free(void* x, int size){
  total_malloced -= size;
  free(x);
}

ikp ik_mmap_protected(int size){
  ikp x = ik_mmap(size + 2*pagesize);
  {
    int err = mprotect(x, pagesize, PROT_NONE);
    if(err){
      fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
      exit(-1);
    }
  }
  {
    int err = mprotect(x+pagesize+size, pagesize, PROT_NONE);
    if(err){
      fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
      exit(-1);
    }
  }
  return x+pagesize;
}



ikpcb* ik_make_pcb(){
  ikpcb* pcb = malloc(sizeof(ikpcb));
  if(pcb == NULL){
    fprintf(stderr, "Failed to allocate pcb\n");
    exit(-1);
  }
  bzero(pcb, sizeof(ikpcb));
  #define HEAPSIZE (1024 * 4096)
  #define STAKSIZE (1024 * 4096)
  pcb->heap_base = ik_mmap_protected(HEAPSIZE);
  pcb->heap_size = HEAPSIZE;
  pcb->allocation_pointer = pcb->heap_base;
  pcb->allocation_redline = pcb->heap_base + HEAPSIZE - 2 * 4096;

  pcb->stack_base = ik_mmap_protected(STAKSIZE);
  pcb->stack_size = STAKSIZE;
  pcb->frame_pointer = pcb->stack_base + pcb->stack_size;
  pcb->frame_base = pcb->frame_pointer;
  pcb->frame_redline = pcb->stack_base + 2 * 4096;
  ikdl* codes = &(pcb->codes);
  codes->next = codes;
  codes->prev = codes;

  /* initialize base rtd */
  {
    ikp s = ik_cstring_to_symbol("$base-rtd", pcb);
    ikp r = ik_alloc(pcb, align(rtd_size)) + rtd_tag;
    ref(r, off_rtd_rtd) = r;
    ref(r, off_rtd_length) = (ikp) (rtd_size-wordsize);
    ref(r, off_rtd_name) = 0;
    ref(r, off_rtd_fields) = 0;
    ref(r, off_rtd_printer) = 0;
    ref(s, off_symbol_system_value) = r;
    ref(s, off_symbol_value) = r;
  }
  return pcb;
}

void ik_delete_pcb(ikpcb* pcb){
  free(pcb);
}


ikp
ik_alloc(ikpcb* pcb, int size){
  assert(size == align(size));
  ikp ap = pcb->allocation_pointer;
  ikp ep = pcb->heap_base + pcb->heap_size;
  ikp nap = ap + size;
  if(nap < ep){
    pcb->allocation_pointer = nap;
    return ap;
  } 
  else {
    fprintf(stderr, "EXT\n");
    if(ap){
      ikpages* p = ik_malloc(sizeof(ikpages));
      p->base = pcb->heap_base;
      p->size = pcb->heap_size;
      p->next = pcb->heap_pages;
      pcb->heap_pages = p;
    }
    int new_size = (size > IK_HEAP_EXT_SIZE) ? size : IK_HEAP_EXT_SIZE;
    new_size += 2 * 4096;
    new_size = align_to_next_page(new_size);
    ap = ik_mmap(new_size);
    pcb->heap_base = ap;
    pcb->heap_size = new_size;
    pcb->allocation_redline = ap + new_size - 2 * 4096;
    nap = ap + size;
    pcb->allocation_pointer = nap;
    return ap;
  }
}

void ik_error(ikp args){
  fprintf(stderr, "Error: ");
  ik_fprint(stderr, args);
  fprintf(stderr, "\n");
  exit(0);
}


void ik_stack_overflow(){
  fprintf(stderr, "entered ik_stack_overflow\n");
  exit(-1);
}

char* ik_uuid(char* str){
  assert((36 << fx_shift) == (int) ref(str, disp_string_length - string_tag));
  uuid_t u;
  uuid_clear(u); 
  uuid_generate(u);
  uuid_unparse_upper(u, str + disp_string_data - string_tag);
  return str;
}


ikp ik_read(ikp fdptr, ikp bufptr, ikp lenptr){
  int fd = unfix(fdptr);
  int len = unfix(lenptr);
  char* buf = (char*)(bufptr+disp_string_data-string_tag);
  int bytes = read(fd, buf, len);
  if(bytes == -1){
    perror("S_read");
    exit(-10);
  }
  return fix(bytes);
}  


ikp ik_write(ikp fdptr, ikp idx, ikp str){
  int fd = unfix(fdptr);
  int len = unfix(idx);
  char* buf = (char*)(str+disp_string_data-string_tag);
  int bytes = write(fd, buf, len);
  if(bytes != len){
    perror("S_write");
    exit(-10);
  }
  return true_object;
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

ikp ik_open_file(ikp str, ikp flagptr){
  int flags;
  int f = unfix(flagptr);
  char* path = (char*)(str + disp_string_data - string_tag);
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
  if(fd != unfix(fix(fd))){
     fprintf(stderr, "fd %d too big\n", fd);
     exit(-10);
  }
  return fix(fd);
}
 
ikp ik_close(ikp fd){
  int err = close(unfix(fd));
  if(err != 0){
    perror("S_close");
    exit(-10);
  }
  return true_object;
}

ikp 
ik_system(ikp str){
  return fix(system(string_data(str)));
}
