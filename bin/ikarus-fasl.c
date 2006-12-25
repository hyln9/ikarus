

#include "ikarus.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include <sys/mman.h>
#include <dlfcn.h>

#ifndef RTLD_DEFAULT
#define RTLD_DEFAULT 0
#endif

typedef struct {
  char* membase;
  char* memp;
  char* memq;
  ikp* marks;
  int marks_size;
} fasl_port;

static ikp ik_fasl_read(ikpcb* pcb, fasl_port* p);

void ik_fasl_load(ikpcb* pcb, char* fasl_file){ 
  int fd = open(fasl_file, O_RDONLY);
  if(fd == -1){
    fprintf(stderr, "failed to open %s: %s\n", fasl_file, strerror(errno));
    exit(-1);
  }

  int filesize;
  {
    struct stat buf;
    int err = fstat(fd, &buf);
    if(err != 0){
      fprintf(stderr, "failed to stat %s: %s\n", fasl_file, strerror(errno));
      exit(-1);
    }
    filesize = buf.st_size;
  }
  int mapsize = ((filesize + pagesize - 1) / pagesize) * pagesize;

  char* mem = mmap(
      0,
      mapsize,
      PROT_READ,
      MAP_PRIVATE,
      fd,
      0);

  if(mem == MAP_FAILED){
    fprintf(stderr, "Mapping failed for %s: %s\n", fasl_file, strerror(errno));
    exit(-1);
  }

  fasl_port p;
  p.membase = mem;
  p.memp = mem;
  p.memq = mem + filesize;
  p.marks = NULL;
  p.marks_size = 0;


  while(p.memp < p.memq){
    ikp v = ik_fasl_read(pcb, &p);
    if(p.marks){
      bzero(p.marks, p.marks_size * sizeof(ikp*));
    }
    ikp val = ik_exec_code(pcb, v);
    if(val != void_object){
      ik_print(val);
    }
  }

  if(p.memp != p.memq){
    fprintf(stderr, "fasl-read did not reach eof!\n");
    exit(-10);
  }
  if(p.marks){
    ik_munmap(p.marks, p.marks_size*sizeof(ikp*));
  }
  {
    int err = munmap(mem, mapsize);
    if(err != 0){
      fprintf(stderr, "Failed to unmap fasl file: %s\n", strerror(errno));
      exit(-1);
    }
  }
  close(fd);
}



static ikp 
alloc_code(int size, ikpcb* pcb){
  int required_memory = align_to_next_page(size);
  ikp mem = ik_mmap_code(required_memory, 0, pcb);
  return (ikp)mem;
}


void
ik_relocate_code(ikp code){
  ikp vec = ref(code, disp_code_reloc_vector);
  ikp size = ref(vec, off_vector_length);
  ikp data = code + disp_code_data;
  ikp p = vec + off_vector_data;
  ikp q = p + (int)size;
  while(p < q){
    int r = unfix(ref(p, 0));
    if(r == 0){
      fprintf(stderr, "unset reloc!\n");
      exit(-1);
    }
    int tag = r & 3;
    int code_off = r >> 2;
//    fprintf(stderr, "data=0x%08x, off=0x%08x, data+off=0x%08x, r=0x%08x\n",
//        (int)data, code_off, (int)data+code_off, r);
//    fprintf(stderr, "setting 0x%08x from r=0x%08x\n", (int)(data+code_off), r);
    if(tag == 0){
      /* vanilla object */
      ref(data, code_off) = ref(p, wordsize);
      p += (2*wordsize);
    } 
    else if(tag == 2){
      /* displaced object */
      int obj_off = unfix(ref(p, wordsize));
      ikp obj = ref(p, 2*wordsize);
      ref(data, code_off) = obj + obj_off;
      p += (3*wordsize);
    }
    else if(tag == 3){
      /* jump label */
      int obj_off = unfix(ref(p, wordsize));
      ikp obj = ref(p, 2*wordsize);
      ikp displaced_object = obj + obj_off;
      ikp next_word = data + code_off + wordsize;
      ikp relative_distance = displaced_object - (int)next_word;
      ref(next_word, -wordsize) = relative_distance;
      p += (3*wordsize);
    }
    else if(tag == 1){
      /* foreign object */
      ikp str = ref(p, wordsize);
      char* name = string_data(str);
      void* sym = dlsym(RTLD_DEFAULT, name);
      char* err = dlerror();
      if(err){
        fprintf(stderr, "failed to find foreign name %s: %s\n", name, err);
        exit(-1);
      }
      ref(data,code_off) = sym;
      p += (2*wordsize);
    }
    else {
      fprintf(stderr, "invalid reloc 0x%08x (tag=%d)\n", r, tag);
      exit(-1);
    }
  }
}


static char fasl_read_byte(fasl_port* p){
  if(p->memp < p->memq){
    char c = *(p->memp);
    p->memp++;
    return c;
  } else {
    fprintf(stderr, "fasl_read_byte: read beyond eof\n");
    exit(-1);
  }
}

static void fasl_read_buf(fasl_port* p, void* buf, int n){
  if((p->memp+n) <= p->memq){
    memcpy(buf, p->memp, n);
    p->memp += n;
  } else {
    fprintf(stderr, "fasl_read_buf: read beyond eof\n");
    exit(-1);
  }
}
typedef struct{
  int code_size;
  int reloc_size;
  ikp closure_size;
} code_header;



static ikp do_read(ikpcb* pcb, fasl_port* p){
  char c = fasl_read_byte(p);
  int put_mark_index = 0;
  if(c == '>'){
    int idx = 0;
    fasl_read_buf(p, &idx, sizeof(int));
    put_mark_index = idx;
    c = fasl_read_byte(p);
    if(idx <= 0){
      fprintf(stderr, "fasl_read: invalid index %d\n", idx);
      exit(-1);
    }
    if(p->marks){
      if(idx >= p->marks_size){
        fprintf(stderr, "BUG: mark too big: %d\n", idx);
        exit(-1);
      }
      if(idx < p->marks_size){
        if(p->marks[idx] != 0){
          fprintf(stderr, "mark %d already set (fileoff=%d)\n", 
              idx,
              (int)p->memp - (int)p->membase - 6);
          ik_print(p->marks[idx]);
          exit(-1);
        }
      } 
    }
    else {
      /* allocate marks */
      p->marks = ik_mmap(pagesize*sizeof(ikp*));
      bzero(p->marks, pagesize*sizeof(ikp*));
      p->marks_size = pagesize;
    }
  }
  if(c == 'x'){
    int code_size;
    ikp freevars;
    fasl_read_buf(p, &code_size, sizeof(int));
    fasl_read_buf(p, &freevars, sizeof(ikp));
    ikp code = alloc_code(align(code_size+disp_code_data), pcb);
    ref(code, 0) = code_tag;
    ref(code, disp_code_code_size) = fix(code_size);
    ref(code, disp_code_freevars) = freevars;
    fasl_read_buf(p, code+disp_code_data, code_size);
    if(put_mark_index){
      p->marks[put_mark_index] = code+vector_tag;
    }
    ref(code, disp_code_reloc_vector) = do_read(pcb, p);
    ik_relocate_code(code);
    return code+vector_tag;
  }
  else if(c == 'P'){
    ikp pair = ik_alloc(pcb, pair_size) + pair_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = pair;
    }
    ref(pair, off_car) = do_read(pcb, p);
    ref(pair, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if(c == 'M'){
    /* symbol */
    ikp str = do_read(pcb, p);
    ikp sym = ikrt_string_to_symbol(str, pcb);
    if(put_mark_index){
      p->marks[put_mark_index] = sym;
    }
    return sym;
  }
  else if(c == 'S'){
    /* string */
    int len;
    fasl_read_buf(p, &len, sizeof(int));
    int size = align(len + disp_string_data + 1);
    ikp str = ik_alloc(pcb, size) + string_tag;
    ref(str, off_string_length) = fix(len);
    fasl_read_buf(p, str+off_string_data, len);
    str[off_string_data+len] = 0;
    if(put_mark_index){
      p->marks[put_mark_index] = str;
    }
    return str;
  }
  else if(c == 'V'){
    int len;
    fasl_read_buf(p, &len, sizeof(int));
    int size = align(len * wordsize + disp_vector_data);
    ikp vec = ik_alloc(pcb, size) + vector_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = vec;
    }
    ref(vec, off_vector_length) = fix(len);
    int i;
    for(i=0; i<len; i++){
      ref(vec, off_vector_data + i*wordsize) = do_read(pcb, p);
    }
    return vec;
  }
  else if(c == 'I'){
    ikp fixn;
    fasl_read_buf(p, &fixn, sizeof(int));
    return fixn;
  }
  else if(c == 'F'){
    return false_object;
  }
  else if(c == 'T'){
    return true_object;
  }
  else if(c == 'N'){
    return IK_NULL_OBJECT;
  }
  else if(c == 'C'){
    char x = fasl_read_byte(p);
    return byte_to_scheme_char(x);
  }
  else if(c == 'G'){ 
    /* G is for gensym */
    ikp pretty = do_read(pcb, p);
    ikp unique = do_read(pcb, p);
    ikp sym = ikrt_strings_to_gensym(pretty, unique, pcb);
    if(put_mark_index){
      p->marks[put_mark_index] = sym;
    }
    return sym;
  }
  else if(c == 'R'){ /* R is for RTD */
    ikp name = do_read(pcb, p);
    ikp symb = do_read(pcb, p);
    int i, n;
    fasl_read_buf(p, &n, sizeof(int));
    ikp fields;
    if(n == 0){
      fields = null_object;
    } else {
      fields = ik_alloc(pcb, n * align(pair_size)) + pair_tag;
      ikp ptr = fields;
      for(i=0; i<n; i++){
        ref(ptr, off_car) = do_read(pcb, p);
        ref(ptr, off_cdr) = ptr + align(pair_size);
        ptr += align(pair_size);
      }
      ptr -= pair_size;
      ref(ptr, off_cdr) = null_object;
    }
    ikp rtd = ik_alloc(pcb, align(rtd_size)) + vector_tag;
    ikp base_sym = ik_cstring_to_symbol("$base-rtd", pcb);
    ikp base_rtd = ref(base_sym, off_symbol_system_value);
    ref(rtd, off_rtd_rtd) = base_rtd;
    ref(rtd, off_rtd_name) = name;
    ref(rtd, off_rtd_length) = fix(n);
    ref(rtd, off_rtd_fields) = fields;
    ref(rtd, off_rtd_printer) = false_object;
    ref(rtd, off_rtd_symbol) = symb;
    if(put_mark_index){
      p->marks[put_mark_index] = rtd;
    }
    return rtd;
  }
  else if(c == 'Q'){ /* thunk */
    ikp proc = ik_alloc(pcb, align(disp_closure_data)) + closure_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = proc;
    }
    ikp code = do_read(pcb, p);
    ref(proc, -closure_tag) = code + off_code_data;
    return proc;
  }
  else if(c == '<'){
    int idx;
    fasl_read_buf(p, &idx, sizeof(int));
    if(idx <= 0){
      fprintf(stderr, "invalid index for ref %d\n", idx);
      exit(-1);
    }
    if(idx >= p->marks_size){
      fprintf(stderr, "invalid index for ref %d\n", idx);
      exit(-1);
    }
    ikp obj = p->marks[idx];
    if(obj){
      return obj;
    } else {
      fprintf(stderr, "reference to uninitialized mark %d\n", idx);
      exit(-1);
    }
  }
  else {
    fprintf(stderr, 
        "invalid type '%c' (0x%02x) found in fasl file at byte 0x%08x\n", 
        c, c,
        (int) p->memp - (int) p->membase - 1);
    exit(-1);
  }
}


static ikp ik_fasl_read(ikpcb* pcb, fasl_port* p){
  /* first check the header */
  char buf[IK_FASL_HEADER_LEN];
  fasl_read_buf(p, buf, IK_FASL_HEADER_LEN);
  if(strncmp(buf, IK_FASL_HEADER, IK_FASL_HEADER_LEN) != 0){
    fprintf(stderr, "invalid fasl header\n");
    exit(-1);
  }
  return do_read(pcb, p);
}

