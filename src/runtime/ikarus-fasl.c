

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


typedef struct {
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
  p.memp = mem;
  p.memq = mem + filesize;
  p.marks = NULL;
  p.marks_size = 0;


  while(p.memp < p.memq){
    ikp v = ik_fasl_read(pcb, &p);
    if(p.marks){
      bzero(p.marks, p.marks_size);
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
    ik_munmap(p.marks, pagesize);
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
ik_make_code(int code_size, int reloc_size, ikp closure_size, ikpcb* pcb){
  int required_memory = 
    align_to_next_page(code_size + reloc_size + disp_code_data);
  ikp mem = ik_mmap_code(required_memory, 0, pcb);
  ref(mem, 0) = code_tag;
  ref(mem, disp_code_code_size) = (ikp) code_size;
  ref(mem, disp_code_reloc_size) = (ikp) reloc_size;
  ref(mem, disp_code_closure_size) = closure_size;
  ref(mem,disp_code_data+code_size+reloc_size) = 0;
  return (ikp)(mem+vector_tag);
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
    if(idx >= 1024){
      fprintf(stderr, "BUG: mark too big: %d\n", idx);
      exit(-1);
    }
    if(idx < p->marks_size){
      if(p->marks[idx] != 0){
        fprintf(stderr, "mark %d already set\n", idx);
        exit(-1);
      }
    } 
    else {
      /* allocate marks */
      if(p->marks){
        fprintf(stderr, "BUG: extension to marks not implemented\n");
        exit(-1);
      } 
      else {
        p->marks = ik_mmap(pagesize);
        bzero(p->marks, pagesize);
        p->marks_size = 1024;
      }
    }
  }
  if(c == 'X'){
    code_header ch;
    fasl_read_buf(p, &ch, sizeof(ch));
    ikp code = ik_make_code(ch.code_size, ch.reloc_size, ch.closure_size, pcb);
    if(put_mark_index){
      p->marks[put_mark_index] = code;
    }
    ikp code_data = code + IK_OFF_CODE_DATA;
    fasl_read_buf(p, code_data, ch.code_size);
    ikp reloc_table = code_data + ch.code_size;
    int i = 0;
    while(i < ch.reloc_size){
      char t = fasl_read_byte(p);
      if(t == 'O'){
        int offset;
        fasl_read_buf(p, &offset, sizeof(int));
        ikp object = do_read(pcb, p);
        REF(code_data,offset) = object;
        REF(reloc_table, i) = (ikp)(offset << 2);
        i += wordsize;
      }
      else if(t == 'F'){ /* foreign call */
        int offset;
        fasl_read_buf(p, &offset, sizeof(int));
        ikp str = do_read(pcb, p);
        char* name = string_data(str);
        void* sym = dlsym(NULL, name);
        char* err = dlerror();
        if(err){
          fprintf(stderr, "failed to find foreign name %s: %s\n", name, err);
          exit(-1);
        }
        ref(code_data,offset) = sym;
        ref(reloc_table, i) = (ikp)((offset << 2) | 3);
        ref(reloc_table, i+wordsize) = str;
        i += 2*wordsize;
      }
      else if(t == 'D'){ /* displaced reloc */
        int code_offset;
        int object_offset;
        fasl_read_buf(p, &code_offset, sizeof(int));
        fasl_read_buf(p, &object_offset, sizeof(int));
        ikp object = do_read(pcb, p);
        REF(reloc_table, i) = (ikp)((code_offset << 2) | 1);
        REF(reloc_table, i+wordsize) = (ikp)object_offset;
        REF(code_data, code_offset) = object + object_offset;
        i += (2*wordsize);
      }
      else if(t == 'J'){ /* jump reloc */
        int code_offset;
        int object_offset;
        fasl_read_buf(p, &code_offset, sizeof(int));
        fasl_read_buf(p, &object_offset, sizeof(int));
        ikp object = do_read(pcb, p);
        REF(reloc_table, i) = (ikp)((code_offset << 2) | 2);
        REF(reloc_table, i+wordsize) = (ikp)object_offset;
        ikp next_word = code_data + code_offset + wordsize;
        ikp displaced_object = object + object_offset;
        REF(next_word, -wordsize) = displaced_object - (int) next_word;
        i += (2*wordsize);
      }
      else {
        fprintf(stderr, "invalid reloc type '%c'\n", t);
        exit(-1);
      }
    }
    assert(i==ch.reloc_size);
    return code;
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
    ikp sym = ik_intern_string(str, pcb);
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
    fprintf(stderr, "invalid type '%c' found in fasl file\n", c);
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

