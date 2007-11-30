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
  ikp code_ap;
  ikp code_ep;
  ikp* marks;
  int marks_size;
} fasl_port;

static ikp ik_fasl_read(ikpcb* pcb, fasl_port* p);

void ik_fasl_load(ikpcb* pcb, char* fasl_file){ 
  int fd = open(fasl_file, O_RDONLY);
  if(fd == -1){
    fprintf(stderr, 
            "ikarus: failed to open boot file \"%s\": %s\n",
            fasl_file,
            strerror(errno));
    ikarus_usage_short();
    exit(-1);
  }
  int filesize;
  {
    struct stat buf;
    int err = fstat(fd, &buf);
    if(err != 0){
      fprintf(stderr,  
              "ikarus: failed to stat \"%s\": %s\n",
              fasl_file, 
              strerror(errno));
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
    fprintf(stderr, 
            "ikarus: mapping failed for %s: %s\n",
            fasl_file,
            strerror(errno));
    exit(-1);
  }
  fasl_port p;
  p.membase = mem;
  p.memp = mem;
  p.memq = mem + filesize;
  p.marks = 0;
  p.marks_size = 0;
  while(p.memp < p.memq){
    p.code_ap = 0;
    p.code_ep = 0;
    ikp v = ik_fasl_read(pcb, &p);
    if(p.marks_size){
      ik_munmap((unsigned char*) p.marks, p.marks_size*sizeof(ikp*));
      p.marks = 0;
      p.marks_size = 0;
    }
    if(p.memp == p.memq){
      int err = munmap(mem, mapsize);
      if(err != 0){
        fprintf(stderr, "Failed to unmap fasl file: %s\n", strerror(errno));
        exit(-1);
      }
      close(fd);
    }
    ikp val = ik_exec_code(pcb, v);
    val = void_object;
    if(val != void_object){
      /* this is from revision 1 
         and is no longer needed 
         and should be removed */
      ik_print(val);
    }
  }
  if(p.memp != p.memq){
    fprintf(stderr, "fasl-read did not reach eof!\n");
    exit(-10);
  }
}

static ikp 
alloc_code(int size, ikpcb* pcb, fasl_port* p){
  int asize = align(size);
  ikp ap = p->code_ap;
  ikp nap = ap + asize;
  if(nap <= p->code_ep){
    p->code_ap = nap;
    return ap;
  } else if (asize < pagesize){
    ikp mem = ik_mmap_code(pagesize, 0, pcb);
    int bytes_remaining = pagesize - asize;
    int previous_bytes = 
      ((unsigned int)p->code_ep) - ((unsigned int)ap);
    if(bytes_remaining <= previous_bytes){
      return mem;
    } else {
      p->code_ap = mem+asize;
      p->code_ep = mem+pagesize;
      return mem;
    }
  } else {
    int asize = align_to_next_page(size);
    ikp mem = ik_mmap_code(asize, 0, pcb);
    return mem;
  }
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
      char* name;
      if(tagof(str) == bytevector_tag){
        name = (char*) str + off_bytevector_data;
      } else {
        fprintf(stderr, "foreign name is not a bytevector\n");
        exit(-1);
      }
      dlerror();
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
          fprintf(stderr, "mark %d already set\n", idx);
          ik_print(p->marks[idx]);
          exit(-1);
        }
      } 
    }
    else {
      /* allocate marks */
      p->marks = (ikp*)ik_mmap(pagesize*sizeof(ikp*));
      bzero(p->marks, pagesize*sizeof(ikp*));
      p->marks_size = pagesize;
    }
  }
  if(c == 'x'){
    int code_size;
    ikp freevars;
    fasl_read_buf(p, &code_size, sizeof(int));
    fasl_read_buf(p, &freevars, sizeof(ikp));
    ikp annotation = do_read(pcb, p);
    ikp code = alloc_code(align(code_size+disp_code_data), pcb, p);
    ref(code, 0) = code_tag;
    ref(code, disp_code_code_size) = fix(code_size);
    ref(code, disp_code_freevars) = freevars;
    ref(code, disp_code_annotation) = annotation;
    fasl_read_buf(p, code+disp_code_data, code_size);
    if(put_mark_index){
      p->marks[put_mark_index] = code+vector_tag;
    }
    ref(code, disp_code_reloc_vector) = do_read(pcb, p);
    ik_relocate_code(code);
    return code+vector_tag;
  }
  else if(c == 'P'){
    ikp pair = ik_unsafe_alloc(pcb, pair_size) + pair_tag;
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
  else if(c == 's'){
    /* ascii string */
    int len;
    fasl_read_buf(p, &len, sizeof(int));
    int size = align(len*string_char_size + disp_string_data);
    ikp str = ik_unsafe_alloc(pcb, size) + string_tag;
    ref(str, off_string_length) = fix(len);
    fasl_read_buf(p, str+off_string_data, len);
    {
      unsigned char* pi = (unsigned char*) (str+off_string_data);
      ikp* pj = (ikp*) (str+off_string_data);
      int i = len-1;
      for(i=len-1; i >= 0; i--){
        pj[i] = integer_to_char(pi[i]);
      }
    }
    //str[off_string_data+len] = 0;
    if(put_mark_index){
      p->marks[put_mark_index] = str;
    }
    return str;
  }
  else if(c == 'S'){
    /* string */
    int len;
    fasl_read_buf(p, &len, sizeof(int));
    int size = align(len*string_char_size + disp_string_data);
    ikp str = ik_unsafe_alloc(pcb, size) + string_tag;
    ref(str, off_string_length) = fix(len);
    int i;
    for(i=0; i<len; i++){
      int c;
      fasl_read_buf(p, &c, sizeof(int));
      string_set(str, i, integer_to_char(c));
    }
    //str[off_string_data+len*string_char_size] = 0;
    if(put_mark_index){
      p->marks[put_mark_index] = str;
    }
    return str;
  }

  else if(c == 'V'){
    int len;
    fasl_read_buf(p, &len, sizeof(int));
    int size = align(len * wordsize + disp_vector_data);
    ikp vec = ik_unsafe_alloc(pcb, size) + vector_tag;
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
  else if(c == 'c'){
    unsigned char x = (unsigned char) fasl_read_byte(p);
    return int_to_scheme_char(x);
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
      fields = ik_unsafe_alloc(pcb, n * align(pair_size)) + pair_tag;
      ikp ptr = fields;
      for(i=0; i<n; i++){
        ref(ptr, off_car) = do_read(pcb, p);
        ref(ptr, off_cdr) = ptr + align(pair_size);
        ptr += align(pair_size);
      }
      ptr -= pair_size;
      ref(ptr, off_cdr) = null_object;
    }
    ikp gensym_val = ref(symb, off_symbol_record_value);
    ikp rtd;
    if(gensym_val == unbound_object){
      rtd = ik_unsafe_alloc(pcb, align(rtd_size)) + vector_tag;
      ikp base_rtd = pcb->base_rtd;
      ref(rtd, off_rtd_rtd) = base_rtd;
      ref(rtd, off_rtd_name) = name;
      ref(rtd, off_rtd_length) = fix(n);
      ref(rtd, off_rtd_fields) = fields;
      ref(rtd, off_rtd_printer) = false_object;
      ref(rtd, off_rtd_symbol) = symb;
      ref(symb, off_symbol_record_value) = rtd;
      pcb->dirty_vector[page_index(symb+off_symbol_record_value)] = -1;
    } else {
      rtd = gensym_val;
    }
    if(put_mark_index){
      p->marks[put_mark_index] = rtd;
    }
    return rtd;
  }
  else if(c == 'Q'){ /* thunk */
    ikp proc = ik_unsafe_alloc(pcb, align(disp_closure_data)) + closure_tag;
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
  else if(c == 'v'){
    /* bytevector */
    int len;
    fasl_read_buf(p, &len, sizeof(int));
    int size = align(len + disp_bytevector_data + 1);
    ikp x = ik_unsafe_alloc(pcb, size) + bytevector_tag;
    ref(x, off_bytevector_length) = fix(len);
    fasl_read_buf(p, x+off_bytevector_data, len);
    x[off_bytevector_data+len] = 0;
    if(put_mark_index){
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if(c == 'l'){
    int len = (unsigned char) fasl_read_byte(p);
    if(len < 0){
      fprintf(stderr, "invalid len=%d\n", len);
      exit(-1);
    }
    ikp pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) + pair_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = pair;
    }
    int i; ikp pt = pair;
    for(i=0; i<len; i++){
      ref(pt, off_car) = do_read(pcb, p);
      ref(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    ref(pt, off_car) = do_read(pcb, p);
    ref(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if(c == 'L'){
    int len;
    fasl_read_buf(p, &len, sizeof(int));
    if(len < 0){
      fprintf(stderr, "invalid len=%d\n", len);
      exit(-1);
    }
    ikp pair = ik_unsafe_alloc(pcb, pair_size * (len+1)) + pair_tag;
    if(put_mark_index){
      p->marks[put_mark_index] = pair;
    }
    int i; ikp pt = pair;
    for(i=0; i<len; i++){
      ref(pt, off_car) = do_read(pcb, p);
      ref(pt, off_cdr) = pt + pair_size;
      pt += pair_size;
    }
    ref(pt, off_car) = do_read(pcb, p);
    ref(pt, off_cdr) = do_read(pcb, p);
    return pair;
  }
  else if(c == 'f'){
    ikp x = ik_unsafe_alloc(pcb, flonum_size) + vector_tag;
    ref(x, -vector_tag) = flonum_tag;
    fasl_read_buf(p, x+disp_flonum_data-vector_tag, 8);
    if(put_mark_index){
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else if(c == 'C'){
    int n;
    fasl_read_buf(p, &n, sizeof(int));
    return int_to_scheme_char(n);
  }
  else if(c == 'b'){
    int len;
    int sign = 0;
    fasl_read_buf(p, &len, sizeof(int));
    if(len < 0) {
      sign = 1;
      len = -len;
    }
    if(len & 3){
      fprintf(stderr, "Error in fasl-read: invalid bignum length %d\n", len);
      exit(-1);
    }
    unsigned int tag = bignum_tag | (sign << bignum_sign_shift) | 
      ((len >> 2) << bignum_length_shift);
    ikp x = ik_unsafe_alloc(pcb, align(len + disp_bignum_data)) + vector_tag;
    ref(x, -vector_tag) = (ikp) tag;
    fasl_read_buf(p, x+off_bignum_data, len);
    if(put_mark_index){
      p->marks[put_mark_index] = x;
    }
    return x;
  }
  else {
    fprintf(stderr, "invalid type '%c' (0x%02x) found in fasl file\n", c, c);
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
