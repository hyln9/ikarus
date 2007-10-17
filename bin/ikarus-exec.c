#include "ikarus-data.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

typedef struct {
  ikp tag;
  ikp top;
  int size;
  ikp next;
} cont;


ikp ik_exec_code(ikpcb* pcb, ikp code_ptr){
  ikp argc = ik_asm_enter(pcb, code_ptr+off_code_data,0);
  ikp next_k =  pcb->next_k;
  while(next_k){
    cont* k = (cont*)(next_k - vector_tag);
    ikp top = k->top;
    ikp rp = ref(top, 0);
    int framesize = (int) ref(rp, disp_frame_size);
    if(framesize <= 0){
      fprintf(stderr, "invalid framesize %d\n", framesize);
      exit(-1);
    }
    if(framesize < k->size){
      cont* nk = (cont*) ik_alloc(pcb, sizeof(cont));
      nk->tag = k->tag;
      nk->next = k->next;
      nk->top = top + framesize;
      nk->size = k->size - framesize;
      k->size = framesize;
      k->next = vector_tag + (ikp)nk;
      /* record side effect */
      unsigned int idx = ((unsigned int)(&k->next)) >> pageshift;
      pcb->dirty_vector[idx] = -1;
    }
    pcb->next_k = k->next;
    ikp fbase = pcb->frame_base - wordsize;
    ikp new_fbase = fbase - framesize;
    memmove(new_fbase + (int)argc,
            fbase  + (int)argc,
            -(int)argc);
    memcpy(new_fbase, top, framesize);
    argc = ik_asm_reenter(pcb, new_fbase, argc);
    next_k =  pcb->next_k;
  }
  return ref(pcb->frame_base, -2*wordsize);
  return argc;
}


