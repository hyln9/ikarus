#include "ikarus.h"
#include <time.h>
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
#include <sys/time.h>
#include <sys/resource.h>

#include <uuid/uuid.h>

int total_allocated_pages = 0;
         
extern char **environ;


#define segment_size  (pagesize*pagesize/wordsize)
#define segment_shift (pageshift+pageshift-wordshift)
#define segment_index(x) (((unsigned int)(x)) >> segment_shift)

static void
extend_table_maybe(unsigned char*p,  int size, ikpcb* pcb){
  assert(size == align_to_next_page(size));
  unsigned char* q = p + size;
  if(p < pcb->memory_base){
    int new_lo = segment_index(p);
    int old_lo = segment_index(pcb->memory_base);
    int hi = segment_index(pcb->memory_end);
    int new_vec_size = (hi - new_lo) * pagesize;
    int old_vec_size = (hi - old_lo) * pagesize;
    unsigned char* v = ik_mmap(new_vec_size);
    bzero(v, new_vec_size - old_vec_size);
    memcpy(v+new_vec_size-old_vec_size, pcb->dirty_vector_base, old_vec_size);
    ik_munmap(pcb->dirty_vector_base, old_vec_size);
    pcb->dirty_vector_base = (unsigned int*) v;
    pcb->dirty_vector = (unsigned int*)(v - new_lo * pagesize);
    unsigned char* s = ik_mmap(new_vec_size);
    bzero(s, new_vec_size - old_vec_size);
    memcpy(s+new_vec_size-old_vec_size, pcb->segment_vector_base, old_vec_size);
    ik_munmap(pcb->segment_vector_base, old_vec_size);
    pcb->segment_vector_base = (unsigned int*) s;
    pcb->segment_vector = (unsigned int*)(s - new_lo * pagesize);
    pcb->memory_base = (unsigned char*)(new_lo * segment_size);
  } 
  else if (q > pcb->memory_end){
    int lo = segment_index(pcb->memory_base);
    int old_hi = segment_index(pcb->memory_end);
    int new_hi = segment_index(q+segment_size-1);
    int new_vec_size = (new_hi - lo) * pagesize;
    int old_vec_size = (old_hi - lo) * pagesize;
    unsigned char* v = ik_mmap(new_vec_size);
    memcpy(v, pcb->dirty_vector_base, old_vec_size);
    bzero(v+old_vec_size, new_vec_size - old_vec_size);
    ik_munmap(pcb->dirty_vector_base, old_vec_size);
    pcb->dirty_vector_base = (unsigned int*) v;
    pcb->dirty_vector = (unsigned int*)(v - lo * pagesize);
    unsigned char* s = ik_mmap(new_vec_size);
    memcpy(s, pcb->segment_vector_base, old_vec_size);
    bzero(s+old_vec_size, new_vec_size - old_vec_size);
    ik_munmap(pcb->segment_vector_base, old_vec_size);
    pcb->segment_vector_base = (unsigned int*) s;
    pcb->segment_vector = (unsigned int*)(s - lo * pagesize);
    pcb->memory_end = (unsigned char*)(new_hi * segment_size);
  }
}


static void
set_segment_type(unsigned char* base, int size, unsigned int type, ikpcb* pcb){
  assert(base >= pcb->memory_base);
  assert((base+size) <= pcb->memory_end);
  assert(size == align_to_next_page(size));
  unsigned int* p = pcb->segment_vector + page_index(base);
  unsigned int* q = p + page_index(size);
  while(p < q){
    *p = type;
    p++;
  }
}

void
ik_munmap_from_segment(unsigned char* base, int size, ikpcb* pcb){
  assert(base >= pcb->memory_base);
  assert((base+size) <= pcb->memory_end);
  assert(size == align_to_next_page(size));
  unsigned int* p = pcb->segment_vector + page_index(base);
  unsigned int* s = pcb->dirty_vector + page_index(base);
  unsigned int* q = p + page_index(size);
  while(p < q){
    assert(*p != hole_mt);
    *p = hole_mt; /* holes */
    *s = 0;  
    p++; s++;
  }
  ikpage* r = pcb->uncached_pages;
  if (r){
    ikpage* cache = pcb->cached_pages;
    do{
      r->base = base;
      ikpage* next = r->next;
      r->next = cache;
      cache = r;
      r = next;
      base += pagesize;
      size -= pagesize;
    } while(r && size);
    pcb->cached_pages = cache;
    pcb->uncached_pages = r;
  }
  if(size){
    ik_munmap(base, size);
  }
}



void* 
ik_mmap_typed(int size, unsigned int type, ikpcb* pcb){
  unsigned char* p;
  if(size == pagesize) {
    ikpage* s = pcb->cached_pages;
    if(s){
      p = s->base;
      pcb->cached_pages = s->next;
      s->next = pcb->uncached_pages;
      pcb->uncached_pages = s; 
    } 
    else {
      p = ik_mmap(size);
    }
  } 
  else {
    p = ik_mmap(size);
  }
  extend_table_maybe(p, size, pcb);
  set_segment_type(p, size, type, pcb);
  return p;
}

void* 
ik_mmap_ptr(int size, int gen, ikpcb* pcb){
  return ik_mmap_typed(size, pointers_mt | gen, pcb);
}

void* 
ik_mmap_data(int size, int gen, ikpcb* pcb){
  return ik_mmap_typed(size, data_mt | gen, pcb);
}

void* 
ik_mmap_code(int size, int gen, ikpcb* pcb){
  ikp p = ik_mmap_typed(size, code_mt | gen, pcb);
  if(size > pagesize){
    set_segment_type(p+pagesize, size-pagesize, data_mt|gen, pcb);
  }
  int err = mprotect(p, size, PROT_READ | PROT_WRITE | PROT_EXEC);
  if(err){
    fprintf(stderr, "cannot mprotect code: %s\n", strerror(errno));
    exit(-1);
  }
  return p;
}


void* 
ik_mmap_mixed(int size, ikpcb* pcb){
  return ik_mmap_typed(size, mainheap_mt, pcb);
}





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
      MAP_PRIVATE | MAP_ANON,
      -1,
      0);
  if(mem == MAP_FAILED){
    fprintf(stderr, "Mapping failed: %s\n", strerror(errno));
    exit(-1);
  }
  memset(mem, -1, mapsize);
#ifndef NDEBUG
  fprintf(stderr, "MMAP 0x%08x .. 0x%08x\n", (int)mem,
      ((int)(mem))+mapsize-1);
#endif
  return mem;
}

void
ik_munmap(void* mem, int size){
  int pages = (size + pagesize - 1) / pagesize;
  int mapsize = pages * pagesize;
  assert(size == mapsize);
  assert(((-pagesize) & (int)mem) == (int)mem);
  total_allocated_pages -= pages;
  int err = munmap(mem, mapsize);
  if(err != 0){
    fprintf(stderr, "ik_munmap failed: %s\n", strerror(errno));
    exit(-1);
  }
#ifndef NDEBUG
  fprintf(stderr, "UNMAP 0x%08x .. 0x%08x\n", (int)mem,
      ((int)(mem))+mapsize-1);
#endif
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


#define CACHE_SIZE (pagesize * 8) /* must be multiple of pagesize*/

ikpcb* ik_make_pcb(){
  ikpcb* pcb = ik_malloc(sizeof(ikpcb));
  bzero(pcb, sizeof(ikpcb));
  #define HEAPSIZE (1024 * 4096)
  #define STAKSIZE (1024 * 4096)
  //#define STAKSIZE (256 * 4096)
  pcb->heap_base = ik_mmap_protected(HEAPSIZE);
  pcb->heap_size = HEAPSIZE;
  pcb->allocation_pointer = pcb->heap_base;
  pcb->allocation_redline = pcb->heap_base + HEAPSIZE - 2 * 4096;

  pcb->stack_base = ik_mmap(STAKSIZE);
  pcb->stack_size = STAKSIZE;
  pcb->frame_pointer = pcb->stack_base + pcb->stack_size;
  pcb->frame_base = pcb->frame_pointer;
  pcb->frame_redline = pcb->stack_base + 2 * 4096;


  { /* make cache ikpage */
    ikpage* p = ik_mmap(CACHE_SIZE * sizeof(ikpage));
    ikpage* q = 0;
    ikpage* e = p + CACHE_SIZE;
    while(p < e){
      p->next = q;
      q = p;
      p++;
    }
    pcb->uncached_pages = q;
  }

  {
    /* compute extent of heap and stack */
    unsigned char* lo_mem;
    unsigned char* hi_mem;
    if(pcb->heap_base < pcb->stack_base){
      lo_mem = pcb->heap_base - pagesize;
      hi_mem = pcb->stack_base + pcb->stack_size + pagesize;
    } else {
      lo_mem = pcb->stack_base - pagesize;
      hi_mem = pcb->heap_base + pcb->heap_size + pagesize;
    }

    int lo_seg = segment_index(lo_mem);
    int hi_seg = segment_index(hi_mem+segment_size-1);
    int vec_size = (hi_seg - lo_seg) * pagesize;
    char* dvec = ik_mmap(vec_size);
    bzero(dvec, vec_size);
    pcb->dirty_vector_base = (unsigned int*) (dvec);
    pcb->dirty_vector = (unsigned int*) (dvec - lo_seg * pagesize);
    char* svec = ik_mmap(vec_size);
    bzero(svec, vec_size);
    pcb->segment_vector_base = (unsigned int*) (svec);
    pcb->segment_vector = (unsigned int*) (svec - lo_seg * pagesize);
    pcb->memory_base = (unsigned char*)(lo_seg * segment_size);
    pcb->memory_end = (unsigned char*)(hi_seg * segment_size);
    set_segment_type(pcb->heap_base-pagesize, 
        pcb->heap_size+2*pagesize, 
        mainheap_mt,
        pcb);
    set_segment_type(pcb->stack_base, 
        pcb->stack_size, 
        mainstack_mt,
        pcb);
  }
  /* initialize base rtd */
  {
    ikp r = ik_alloc(pcb, align(rtd_size)) + rtd_tag;
    ref(r, off_rtd_rtd) = r;
    ref(r, off_rtd_length) = (ikp) (rtd_size-wordsize);
    ref(r, off_rtd_name) = 0;
    ref(r, off_rtd_fields) = 0;
    ref(r, off_rtd_printer) = 0;
    ref(r, off_rtd_symbol) = 0;
    pcb->base_rtd = r;
  }
  return pcb;
}

void ik_delete_pcb(ikpcb* pcb){
  unsigned char* base = pcb->memory_base;
  unsigned char* end = pcb->memory_end;
  unsigned int* segment_vec = pcb->segment_vector;
  int i = page_index(base);
  int j = page_index(end);
  while(i < j){
    unsigned int t = segment_vec[i];
    if(t != hole_mt){
      ik_munmap((ikp)(i<<pageshift), pagesize);
    }
    i++;
  }
  int vecsize = (segment_index(end) - segment_index(base)) * pagesize;
  ik_munmap(pcb->dirty_vector_base, vecsize);
  ik_munmap(pcb->segment_vector_base, vecsize);
   
  ik_free(pcb, sizeof(ikpcb));
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
    if(ap){
      ikpages* p = ik_malloc(sizeof(ikpages));
      p->base = pcb->heap_base;
      p->size = pcb->heap_size;
      p->next = pcb->heap_pages;
      pcb->heap_pages = p;
    }

    { /* ACCOUNTING */
      int bytes = ((int)pcb->allocation_pointer) -
                  ((int)pcb->heap_base);
      int minor = bytes + pcb->allocation_count_minor;
      while(minor >= most_bytes_in_minor){
        minor -= most_bytes_in_minor;
        pcb->allocation_count_major++;
      }
      pcb->allocation_count_minor = minor;
    }

    int new_size = (size > IK_HEAP_EXT_SIZE) ? size : IK_HEAP_EXT_SIZE;
    new_size += 2 * 4096;
    new_size = align_to_next_page(new_size);
    ap = ik_mmap_mixed(new_size, pcb);
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


void ik_stack_overflow(ikpcb* pcb){
#ifndef NDEBUG
  fprintf(stderr, "entered ik_stack_overflow pcb=0x%08x\n", (int)pcb);
#endif
  set_segment_type(pcb->stack_base, pcb->stack_size, data_mt, pcb);
  
  ikp frame_base = pcb->frame_base;
  ikp underflow_handler = ref(frame_base, -wordsize);
#ifndef NDEBUG
  fprintf(stderr, "underflow_handler = 0x%08x\n", (int)underflow_handler);
#endif
  /* capture continuation and set it as next_k */
  ikp k = ik_alloc(pcb, align(continuation_size)) + vector_tag;
  ref(k, -vector_tag) = continuation_tag;
  ref(k, off_continuation_top) = pcb->frame_pointer;
  ref(k, off_continuation_size) = 
    pcb->frame_base - (int)pcb->frame_pointer - wordsize;
  ref(k, off_continuation_next) = pcb->next_k;
  pcb->next_k = k;

  pcb->stack_base = ik_mmap_typed(STAKSIZE, mainstack_mt, pcb);
  pcb->stack_size = STAKSIZE;
  pcb->frame_base = pcb->stack_base + pcb->stack_size;
  pcb->frame_pointer = pcb->frame_base - wordsize;
  pcb->frame_redline = pcb->stack_base + 2 * 4096;
  ref(pcb->frame_pointer, 0) = underflow_handler;
  return;
}

/*
char* ik_uuid(char* str){
  assert((36 << fx_shift) == (int) ref(str, disp_string_length - string_tag));
  uuid_t u;
  uuid_clear(u); 
  uuid_generate(u);
  uuid_unparse_upper(u, str + disp_string_data - string_tag);
  return str;
}
*/


static const char* uuid_chars = 
"!$%&/0123456789<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static int uuid_strlen = 1;

ikp ik_uuid(ikp str){
  static int fd = -1;
  if(fd == -1){
    fd = open("/dev/urandom", O_RDONLY);
    if(fd == -1){
      return false_object;
    }
    uuid_strlen = strlen(uuid_chars);
  }
  int n = unfix(ref(str, off_string_length));
  unsigned char* data = str+off_string_data;
  read(fd, data, n);
  unsigned char* p = data;
  unsigned char* q = data + n;
  while(p < q){
    *p = uuid_chars[*p % uuid_strlen];
    p++;
  }
  return str;
}


#if 0
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
#endif

ikp ik_write(ikp fdptr, ikp idx, ikp str){
  fprintf(stderr, "IK_WRITE\n");
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
    flags = O_WRONLY | O_CREAT;
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
 

/*
     #include <sys/types.h>
     #include <sys/stat.h>
     int
     stat(const char *path, struct stat *sb);
ERRORS
     Stat() and lstat() will fail if:
     [ENOTDIR]          A component of the path prefix is not a directory.
     [ENAMETOOLONG]     A component of a pathname exceeded {NAME_MAX} charac-
                        ters, or an entire path name exceeded {PATH_MAX} char-
                        acters.
     [ENOENT]           The named file does not exist.
     [EACCES]           Search permission is denied for a component of the
                        path prefix.
     [ELOOP]            Too many symbolic links were encountered in translat-
                        ing the pathname.
     [EFAULT]           Sb or name points to an invalid address.
     [EIO]              An I/O error occurred while reading from or writing to
                        the file system.
*/
ikp 
ikrt_file_exists(ikp filename){
  char* str = string_data(filename);
  struct stat sb;
  int st = stat(str, &sb);
  if(st == 0){
    /* success */
    return true_object;
  } else {
    int err = errno;
    if(err == ENOENT){
      return false_object;
    } 
    else if(err == ENOTDIR){
      return fix(1);
    } 
    else if(err == ENAMETOOLONG){
      return fix(2);
    } 
    else if(err == EACCES){
      return fix(3);
    } 
    else if(err == ELOOP){
      return fix(4);
    } 
    else if(err == EFAULT){
      return fix(5);
    } 
    else if(err == EIO){
      return fix(6);
    } 
    else {
      return fix(-1);
    }
  }
}


/*
     [ENOTDIR]          A component of the path prefix is not a directory.
     [ENAMETOOLONG]     A component of a pathname exceeded {NAME_MAX} charac-
                        ters, or an entire path name exceeded {PATH_MAX} char-
                        acters.
     [ENOENT]           The named file does not exist.
     [EACCES]           Search permission is denied for a component of the
                        path prefix.
     [EACCES]           Write permission is denied on the directory containing
                        the link to be removed.
     [ELOOP]            Too many symbolic links were encountered in translat-
                        ing the pathname.
     [EPERM]            The named file is a directory and the effective user
                        ID of the process is not the super-user.
     [EPERM]            The directory containing the file is marked sticky,
                        and neither the containing directory nor the file to
                        be removed are owned by the effective user ID.
     [EBUSY]            The entry to be unlinked is the mount point for a
                        mounted file system.
     [EIO]              An I/O error occurred while deleting the directory
                        entry or deallocating the inode.
     [EROFS]            The named file resides on a read-only file system.
     [EFAULT]           Path points outside the process's allocated address
                        space.
*/


ikp
ikrt_delete_file(ikp filename){
  char* str = string_data(filename);
  int err = unlink(str);
  if(err == 0){
    return 0;
  } 
  switch (err){
    case ENOTDIR:  return fix(1);    
    case ENAMETOOLONG: return fix(2);
    case ENOENT: return fix(3);     
    case EACCES: return fix(4);     
    case ELOOP: return fix(5);      
    case EPERM: return fix(6);      
    case EBUSY: return fix(7);      
    case EIO: return fix(8);        
    case EROFS: return fix(9);      
    case EFAULT: return fix(10);     
  }
  return fix(-1);
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

static char*
mtname(unsigned int n){
  if(n == mainheap_type)  { return "HEAP_T"; }
  if(n == mainstack_type) { return "STAK_T"; }
  if(n == pointers_type)  { return "PTER_T"; }
  if(n == data_type)      { return "DATA_T"; }
  if(n == code_type)      { return "CODE_T"; }
  if(n == hole_type)      { return "      "; }
  return "WHAT_T";
}

ikp
ik_dump_metatable(ikpcb* pcb){
  unsigned int* s = pcb->segment_vector_base;
  unsigned char* p = pcb->memory_base;
  unsigned char* hi = pcb->memory_end;
  while(p < hi){
    unsigned int t = *s & type_mask;
    unsigned char* start = p;
    p += pagesize;
    s++;
    while((p < hi) && ((*s & type_mask) == t)){
      p += pagesize;
      s++;
    }
    fprintf(stderr, "0x%08x + %5d pages = %s\n", 
        (int) start,
        ((int)p-(int)start)/pagesize,
        mtname(t));
  }
  return void_object;
}

ikp
ik_dump_dirty_vector(ikpcb* pcb){
  unsigned int* s = pcb->dirty_vector_base;
  unsigned char* p = pcb->memory_base;
  unsigned char* hi = pcb->memory_end;
  while(p < hi){
    unsigned int t = *s;
    unsigned char* start = p;
    p += pagesize;
    s++;
    while((p < hi) && (*s == t)){
      p += pagesize;
      s++;
    }
    fprintf(stderr, "0x%08x + %5d pages = 0x%08x\n", 
        (int) start,
        ((int)p-(int)start)/pagesize,
        t);
  }
  return void_object;
}

ikp 
ikrt_make_code(ikp codesizeptr, ikp freevars, ikp rvec, ikpcb* pcb){
  assert((fx_mask & (int)codesizeptr) == 0);
  int code_size = unfix(codesizeptr);
  int memreq = align_to_next_page(code_size + disp_code_data);
  ikp mem = ik_mmap_code(memreq, 0, pcb);
  bzero(mem, memreq);
  ref(mem, 0) = code_tag;
  ref(mem, disp_code_code_size) = codesizeptr;
  ref(mem, disp_code_freevars) = freevars;
  ref(mem, disp_code_reloc_vector) = rvec;
  ik_relocate_code(mem);
  return mem+vector_tag;
}

ikp
ikrt_set_code_reloc_vector(ikp code, ikp vec, ikpcb* pcb){
  ref(code, off_code_reloc_vector) = vec;
  ik_relocate_code(code-vector_tag);
  pcb->dirty_vector[page_index(code)] = -1;
  return void_object;
}

ikp
ikrt_bvftime(ikp outbv, ikp fmtbv){
  time_t t;
  struct tm* tmp;
  t = time(NULL);
  tmp = localtime(&t);
  if(tmp == NULL){
    fprintf(stderr, "Error in time: %s\n", strerror(errno));
  }
  int rv = 
    strftime((char*)outbv+off_bytevector_data,
             unfix(ref(outbv, off_bytevector_length)) + 1,
             (char*)fmtbv+off_bytevector_data,
             tmp);
  if(rv == 0){
    fprintf(stderr, "Error in strftime: %s\n", strerror(errno));
  }
  return fix(rv);
}




ikp
ikrt_close_file(ikp fd, ikpcb* pcb){
  int err = close(unfix(fd));
  if(err == -1){
    return false_object;
  } else {
    return true_object;
  }
}

ikp ikrt_read(ikp fd, ikp buff, ikpcb* pcb){
  if(tagof(buff) != bytevector_tag){
    fprintf(stderr, "%p is not a bytevector", buff);
    exit(-1);
  }
  int bytes =
    read(unfix(fd), buff+off_bytevector_data, unfix(ref(buff, off_bytevector_length)));
  ikp fbytes = fix(bytes);
  if (bytes == unfix(fbytes)){
    return fbytes;
  } else {
    fprintf(stderr, "ERR: ikrt_read: too big\n");
    exit(-1);
  }
}


ikp
ikrt_open_input_file(ikp fname, ikpcb* pcb){
  int fd = open(string_data(fname), O_RDONLY);
  if(fd == -1){
    return false_object;
  } else {
    return fix(fd);
  }
}

ikp
ikrt_open_output_file(ikp fname, ikp flagptr, ikpcb* pcb){
  /* [(error)    0] */ 
  /* [(replace)  1] */
  /* [(truncate) 2] */
  /* [(append)   3] */
  int flags;
  int f = unfix(flagptr);
  if(f == 0){
    flags = O_WRONLY | O_CREAT;
  } else if(f == 1){
    unlink(string_data(fname));
    flags = O_WRONLY | O_CREAT;
  } else if(f == 2){
    flags = O_WRONLY | O_TRUNC | O_CREAT;
  } else if(f == 3){
    flags = O_WRONLY | O_APPEND;
  } else {
    fprintf(stderr, "Error in S_open_file: invalid mode 0x%08x\n", 
                  (int)flagptr);
    exit(-10);
  }
  int fd = open(string_data(fname), flags,
                S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if(fd == -1){
    fprintf(stderr, "openfile failed: %s\n", strerror(errno));
    return false_object;
  } else {
    return fix(fd);
  }
}


ikp
ikrt_write_file(ikp fd, ikp buff, ikp idx, ikpcb* pcb){
  int bytes;
  if(tagof(buff) == bytevector_tag){
    bytes = write(unfix(fd), buff+off_bytevector_data, unfix(idx));
  } else {
    bytes = write(unfix(fd), string_data(buff), unfix(idx));
  }
  return fix(bytes);
}

ikp 
ikrt_write_char(){
  fprintf(stderr, "ikrt_write_char\n");
  return void_object;
}



ikp 
ikrt_register_guardian(ikp tc, ikp obj, ikpcb* pcb){
  ik_guardian_table* g = pcb->guardians[0];
  if((!g) || (g->count == ik_guardian_table_size)){
    if(sizeof(ik_guardian_table) != pagesize){
      fprintf(stderr, "ERR: invaldi guardian table size\n");
      exit(-1);
    }
    ik_guardian_table* p = 
      (ik_guardian_table*)ik_mmap(sizeof(ik_guardian_table));
    p->next = g;
    p->count = 0;
    pcb->guardians[0] = p;
    g=p;
  } 
  ik_guardian_pair* p = &(g->p[g->count]);
  p->tc = tc;
  p->obj = obj;
  g->count++;
  return 0;
}


ikp 
ikrt_stats_now(ikp t, ikpcb* pcb){
  struct rusage r;
  struct timeval s;

  gettimeofday(&s, 0);
  getrusage(RUSAGE_SELF, &r);
  ref(t, off_record_data)                = fix(r.ru_utime.tv_sec);
  ref(t, off_record_data + wordsize)     = fix(r.ru_utime.tv_usec);
  ref(t, off_record_data + 2 * wordsize) = fix(r.ru_stime.tv_sec);
  ref(t, off_record_data + 3 * wordsize) = fix(r.ru_stime.tv_usec);
  ref(t, off_record_data + 4 * wordsize) = fix(s.tv_sec);
  ref(t, off_record_data + 5 * wordsize) = fix(s.tv_usec);
  ref(t, off_record_data + 6 * wordsize) = fix(pcb->collection_id);
  return void_object;
}

ikp
ikrt_bytes_allocated(ikpcb* pcb){
  int bytes_in_heap = ((int) pcb->allocation_pointer) -
                      ((int) pcb->heap_base);
  int bytes = bytes_in_heap + pcb->allocation_count_minor;
  return fix(bytes);
}


ikp
ikrt_bytes_allocated_major(ikpcb* pcb){
  return fix(pcb->allocation_count_major);
}


ikp 
ikrt_fork(){
  int pid = fork();
  return fix(pid);
}

ikp 
ikrt_waitpid(ikp pid){
  int status;
  /*pid_t t = */ waitpid(unfix(pid), &status, 0);
  return fix(status);
}

ikp 
ikrt_getenv(ikp str, ikpcb* pcb){
  char* v = getenv(string_data(str));
  if(v){
    int n = strlen(v);
    ikp s = ik_alloc(pcb, align(n+disp_string_data+1)) + string_tag;
    ref(s, -string_tag) = fix(n);
    memcpy(s+off_string_data, v, n+1);
    return s;
  } 
  else {
    ikp s = ik_alloc(pcb, align(disp_string_data+1)) + string_tag;
    ref(s, -string_tag) = fix(0);
    ref(s, off_string_data) = 0;
    return s;
  }
}

ikp 
ikrt_setenv(ikp key, ikp val, ikp overwrite){
  int err = setenv(string_data(key), string_data(val), 
                   overwrite!=false_object);
  if(err){
    return false_object;
  } else {
    return true_object;
  }
}


ikp 
ikrt_environ(ikpcb* pcb){
  char** es = environ;
  int i; char* e;
  ikp ac = null_object;
  for(i=0; (e=es[i]); i++){
    int n = strlen(e);
    ikp s = ik_alloc(pcb, align(n+disp_string_data+1)) + string_tag;
    ref(s, -string_tag) = fix(n);
    memcpy(s+off_string_data, e, n+1);
    ikp p = ik_alloc(pcb, pair_size) + pair_tag;
    ref(p, off_cdr) = ac;
    ref(p, off_car) = s;
    ac = p;
  }
  return ac;
}
