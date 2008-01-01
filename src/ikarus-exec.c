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
#include <assert.h>
#include <string.h>

typedef struct {
  ikptr tag;
  ikptr top;
  int size;
  ikptr next;
} cont;


ikptr ik_exec_code(ikpcb* pcb, ikptr code_ptr){
  ikptr argc = ik_asm_enter(pcb, code_ptr+off_code_data,0);
  ikptr next_k =  pcb->next_k;
  while(next_k){
    cont* k = (cont*)(next_k - vector_tag);
    ikptr top = k->top;
    ikptr rp = ref(top, 0);
    long int framesize = (long int) ref(rp, disp_frame_size);
    if(framesize <= 0){
      fprintf(stderr, "invalid framesize %ld\n", framesize);
      exit(-1);
    }
    if(framesize < k->size){
      cont* nk = (cont*) ik_unsafe_alloc(pcb, sizeof(cont));
      nk->tag = k->tag;
      nk->next = k->next;
      nk->top = top + framesize;
      nk->size = k->size - framesize;
      k->size = framesize;
      k->next = vector_tag + (ikptr)nk;
      /* record side effect */
      unsigned long int idx = ((unsigned long int)(&k->next)) >> pageshift;
      pcb->dirty_vector[idx] = -1;
    }
    pcb->next_k = k->next;
    ikptr fbase = pcb->frame_base - wordsize;
    ikptr new_fbase = fbase - framesize;
    memmove(new_fbase + (long int)argc,
            fbase  + (long int)argc,
            -(long int)argc);
    memcpy(new_fbase, top, framesize);
    argc = ik_asm_reenter(pcb, new_fbase, argc);
    next_k =  pcb->next_k;
  }
  return ref(pcb->frame_base, -2*wordsize);
  return argc;
}


