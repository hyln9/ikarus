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
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>   
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/time.h>
#include <assert.h>
#include <errno.h>

#define forward_ptr ((ikp)-1)
#define minimum_heap_size (pagesize * 1024 * 4)
#define maximum_heap_size (pagesize * 1024 * 8)
#define minimum_stack_size (pagesize * 128)

#define accounting 0

#if accounting
static int pair_count = 0;
static int symbol_count = 0;
static int closure_count = 0;
static int vector_count = 0;
static int record_count = 0;
static int continuation_count = 0;
static int string_count = 0;
static int htable_count = 0;
#endif

typedef struct qupages_t{
  ikp p;    /* pointer to the scan start */
  ikp q;    /* pointer to the scan end */
  struct qupages_t* next;
} qupages_t;


typedef struct{
  ikp ap;
  ikp aq;
  ikp ep;
  ikp base;
} meta_t;


#define meta_ptrs 0
#define meta_code 1
#define meta_data 2
#define meta_weak 3
#define meta_pair 4
#define meta_symbol 5
#define meta_count 6

static int extension_amount[meta_count] = {
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
  1 * pagesize,
};

static unsigned int meta_mt[meta_count] = {
  pointers_mt,
  code_mt,
  data_mt,
  weak_pairs_mt,
  pointers_mt,
  symbols_mt
};

typedef struct gc_t{
  meta_t meta [meta_count];
  qupages_t* queues [meta_count];
  ikpcb* pcb;
  unsigned int* segment_vector;
  int collect_gen;
  int collect_gen_tag;
  ikp tconc_ap;
  ikp tconc_ep;
  ikp tconc_base;
  ikpages* tconc_queue;
} gc_t;

static unsigned int 
next_gen_tag[generation_count] = {
  (4 << meta_dirty_shift) | 1 | new_gen_tag,
  (2 << meta_dirty_shift) | 2 | new_gen_tag,
  (1 << meta_dirty_shift) | 3 | new_gen_tag,
  (0 << meta_dirty_shift) | 4 | new_gen_tag,
  (0 << meta_dirty_shift) | 4 | new_gen_tag
};

static ikp
meta_alloc_extending(int size, gc_t* gc, int meta_id){
  int mapsize = align_to_next_page(size);
  if(mapsize < extension_amount[meta_id]){
    mapsize = extension_amount[meta_id];
  }
  meta_t* meta = &gc->meta[meta_id];
  if((meta_id != meta_data) &&  meta->base){
    qupages_t* p = ik_malloc(sizeof(qupages_t));
    ikp aq = meta->aq;
    ikp ap = meta->ap;
    ikp ep = meta->ep; 
    p->p = aq;
    p->q = ap;
    p->next = gc->queues[meta_id];
    gc->queues[meta_id] = p;
    ikp x = ap;
    while(x < ep){
      ref(x, 0) = 0;
      x += wordsize;
    }
  }
  ikp mem = ik_mmap_typed(
      mapsize, 
      meta_mt[meta_id] | gc->collect_gen_tag,
      gc->pcb);
  gc->segment_vector = gc->pcb->segment_vector;
  meta->ap = mem + size;
  meta->aq = mem;
  meta->ep = mem + mapsize;
  meta->base = mem;
  return mem;
}




static inline ikp
meta_alloc(int size, gc_t* gc, int meta_id){
  assert(size == align(size));
  meta_t* meta = &gc->meta[meta_id];
  ikp ap = meta->ap;
  ikp ep = meta->ep;
  ikp nap = ap + size;
  if(nap > ep){
    return meta_alloc_extending(size, gc, meta_id);
  } else {
    meta->ap = nap;
    return ap;
  }
}

static inline ikp 
gc_alloc_new_ptr(int size, gc_t* gc){
  assert(size == align(size));
  return meta_alloc(size, gc, meta_ptrs);
}

static inline ikp 
gc_alloc_new_large_ptr(int size, gc_t* gc){
  int memreq = align_to_next_page(size);
  ikp mem = 
      ik_mmap_typed(memreq, 
        pointers_mt | large_object_tag | gc->collect_gen_tag,
        gc->pcb);
  gc->segment_vector = gc->pcb->segment_vector;
  qupages_t* p = ik_malloc(sizeof(qupages_t));
  p->p = mem;
  p->q = mem+size;
  bzero(mem+size, memreq-size);
  p->next = gc->queues[meta_ptrs];
  gc->queues[meta_ptrs] = p;
  return mem;
}


static inline void 
enqueue_large_ptr(ikp mem, int size, gc_t* gc){
  int i = page_index(mem);
  int j = page_index(mem+size-1);
  while(i<=j){
    gc->segment_vector[i] = 
      pointers_mt | large_object_tag | gc->collect_gen_tag;
    i++;
  }
  qupages_t* p = ik_malloc(sizeof(qupages_t));
  p->p = mem;
  p->q = mem+size;
  p->next = gc->queues[meta_ptrs];
  gc->queues[meta_ptrs] = p;
}


static inline ikp 
gc_alloc_new_symbol_record(gc_t* gc){
  assert(symbol_record_size == align(symbol_record_size));
  return meta_alloc(symbol_record_size, gc, meta_symbol);
}




static inline ikp 
gc_alloc_new_pair(gc_t* gc){
  return meta_alloc(pair_size, gc, meta_pair);
}



static inline ikp 
gc_alloc_new_weak_pair(gc_t* gc){
  meta_t* meta = &gc->meta[meta_weak];
  ikp ap = meta->ap;
  ikp ep = meta->ep;
  ikp nap = ap + pair_size;
  if(nap > ep){
      ikp mem = ik_mmap_typed(
                   pagesize, 
                   meta_mt[meta_weak] | gc->collect_gen_tag,
                   gc->pcb);
      gc->segment_vector = gc->pcb->segment_vector;
      meta->ap = mem + pair_size;
      meta->aq = mem;
      meta->ep = mem + pagesize;
      meta->base = mem;
      return mem;
  } else {
    meta->ap = nap;
    return ap;
  }
}

static inline ikp 
gc_alloc_new_data(int size, gc_t* gc){
  assert(size == align(size));
  return meta_alloc(size, gc, meta_data);
}

static inline ikp 
gc_alloc_new_code(int size, gc_t* gc){
  assert(size == align(size));
  if(size < pagesize){
    return meta_alloc(size, gc, meta_code);
  } else {
    int memreq = align_to_next_page(size);
    ikp mem = ik_mmap_code(memreq, gc->collect_gen, gc->pcb);
    gc->segment_vector = gc->pcb->segment_vector;
    qupages_t* p = ik_malloc(sizeof(qupages_t));
    p->p = mem;
    p->q = mem+size;
    bzero(mem+size, memreq-size);
    p->next = gc->queues[meta_code];
    gc->queues[meta_code] = p;
    return mem;
  }
}

static void 
add_to_collect_count(ikpcb* pcb, int bytes){
  int minor = bytes + pcb->allocation_count_minor;
  while(minor >= most_bytes_in_minor){
    minor -= most_bytes_in_minor;
    pcb->allocation_count_major++;
  }
  pcb->allocation_count_minor = minor;
}




static void
gc_tconc_push_extending(gc_t* gc, ikp tcbucket){
  if(gc->tconc_base){
    ikpages* p = ik_malloc(sizeof(ikpages));
    p->base = gc->tconc_base;
    p->size = pagesize;
    p->next = gc->tconc_queue;
    gc->tconc_queue = p;
  }
  ikp ap = 
     ik_mmap_typed(pagesize, 
        meta_mt[meta_ptrs] | gc->collect_gen_tag,
        gc->pcb);
  add_to_collect_count(gc->pcb, pagesize);
  gc->segment_vector = gc->pcb->segment_vector;
  bzero(ap, pagesize);
  ikp nap = ap + 2*wordsize;
  gc->tconc_base = ap;
  gc->tconc_ap = nap;
  gc->tconc_ep = ap + pagesize;
  ref(ap,0) = tcbucket;
}


static inline void
gc_tconc_push(gc_t* gc, ikp tcbucket){
  ikp ap = gc->tconc_ap;
  ikp nap = ap + 2*wordsize;
  if(nap > gc->tconc_ep){
    gc_tconc_push_extending(gc, tcbucket);
  } else {
    gc->tconc_ap = nap;
    ref(ap,0) = tcbucket;
  }
}


#ifndef NDEBUG
static ikp add_object_proc(gc_t* gc, ikp x, char* caller);
#define add_object(gc,x,caller) add_object_proc(gc,x,caller)
#else
static ikp add_object_proc(gc_t* gc, ikp x);
#define add_object(gc,x,caller) add_object_proc(gc,x)
#endif

static void collect_stack(gc_t*, ikp top, ikp base);
static void collect_loop(gc_t*);
static void forward_guardians(gc_t*);
static void fix_weak_pointers(gc_t*);
static void gc_add_tconcs(gc_t*);
static void empty_dropped_guardians(gc_t*);

/* ik_collect is called from scheme under the following conditions:
 * 1. An attempt is made to allocate a small object and the ap is above
 *    the red line.
 * 2. The current frame of the call is dead, so, upon return from ik_collect,
 *    the caller returns to its caller.
 * 3. The frame-pointer of the caller to S_collect is saved at
 *    pcb->frame_pointer.  No variables are live at that frame except for
 *    the return point (at *(pcb->frame_pointer)).
 * 4. S_collect must return a new ap (in pcb->allocation_pointer) that has
 *    at least 2 pages of memory free.
 * 5. S_collect must also update pcb->allocaton_redline to be 2 pages below
 *    the real end of heap.
 * 6. ik_collect should not move the stack.
 */


ikpcb* ik_collect_vararg(int req, ikpcb* pcb){
  return ik_collect(req, pcb);
}

static int collection_id_to_gen(int id){
  if((id & 255) == 255) { return 4; }
  if((id & 63) == 63) { return 3; }
  if((id & 15) == 15) { return 2; }
  if((id & 3) == 3) { return 1; }
  return 0;
}



static void scan_dirty_pages(gc_t*);

static void deallocate_unused_pages(gc_t*);

static void fix_new_pages(gc_t* gc);

extern void verify_integrity(ikpcb* pcb, char*);


ikpcb* 
ik_collect(int mem_req, ikpcb* pcb){
#ifndef NDEBUG
  verify_integrity(pcb, "entry");
#endif

  { /* ACCOUNTING */
    int bytes = ((int)pcb->allocation_pointer) -
                ((int)pcb->heap_base);
    add_to_collect_count(pcb, bytes);
  }

  struct rusage t0, t1;
  struct timeval rt0, rt1;
  gettimeofday(&rt0, 0);
  getrusage(RUSAGE_SELF, &t0);

  pcb->collect_key = false_object;
  gc_t gc;
  bzero(&gc, sizeof(gc_t));
  gc.pcb = pcb;
  gc.segment_vector = pcb->segment_vector;

  gc.collect_gen = collection_id_to_gen(pcb->collection_id);
  gc.collect_gen_tag = next_gen_tag[gc.collect_gen];
  pcb->collection_id++;
#ifndef NDEBUG
  fprintf(stderr, "ik_collect entry %d free=%d (collect gen=%d/id=%d)\n",
      mem_req,
      (unsigned int) pcb->allocation_redline
        - (unsigned int) pcb->allocation_pointer,
      gc.collect_gen, pcb->collection_id-1);
#endif

  /* cache heap-pages to delete later */
  ikpages* old_heap_pages = pcb->heap_pages;
  pcb->heap_pages = 0;

  /* the roots are:
   *  0. dirty pages not collected in this run
   *  1. the stack
   *  2. the next continuation
   *  3. the symbol-table
   */

  scan_dirty_pages(&gc);

  collect_stack(&gc, pcb->frame_pointer, pcb->frame_base - wordsize);
  pcb->next_k = add_object(&gc, pcb->next_k, "next_k"); 
  pcb->symbol_table = add_object(&gc, pcb->symbol_table, "symbol_table"); 
  pcb->gensym_table = add_object(&gc, pcb->gensym_table, "gensym_table"); 
  pcb->arg_list = add_object(&gc, pcb->arg_list, "args_list_foo");
  pcb->base_rtd = add_object(&gc, pcb->base_rtd, "base_rtd");
  if(pcb->root0) *(pcb->root0) = add_object(&gc, *(pcb->root0), "root0");
  if(pcb->root1) *(pcb->root1) = add_object(&gc, *(pcb->root1), "root1");

  /* now we trace all live objects */
  collect_loop(&gc);
  
  /* next we trace all guardian/guarded objects,
     the procedure does a collect_loop at the end */
#ifndef NDEBUG
  fprintf(stderr, "forwarding guardians ...\n");
#endif
  forward_guardians(&gc);
#ifndef NDEBUG
  fprintf(stderr, "done\n");
#endif
  collect_loop(&gc);

  //guardians_loop_old(&gc);

  /* does not allocate, only bwp's dead pointers */
  fix_weak_pointers(&gc); 
  /* now deallocate all unused pages */
  deallocate_unused_pages(&gc);

  fix_new_pages(&gc);
  pcb->allocation_pointer = pcb->heap_base;
  /* does not allocate */
  gc_add_tconcs(&gc);
  /* does not allocate */
#ifndef NDEBUG
  fprintf(stderr, "emptying guardians ...\n");
#endif
  empty_dropped_guardians(&gc);
#ifndef NDEBUG
  fprintf(stderr, "done\n");
#endif
  pcb->weak_pairs_ap = 0;
  pcb->weak_pairs_ep = 0;

#if accounting
    fprintf(stderr, 
        "[%d cons|%d sym|%d cls|%d vec|%d rec|%d cck|%d str|%d htb]\n",
        pair_count,
        symbol_count,
        closure_count,
        vector_count,
        record_count,
        continuation_count,
        string_count,
        htable_count);
    pair_count = 0;
    symbol_count = 0;
    closure_count = 0;
    vector_count = 0;
    record_count = 0;
    continuation_count = 0;
    string_count = 0;
    htable_count = 0;
#endif
  //ik_dump_metatable(pcb);
#ifndef NDEBUG
  fprintf(stderr, "collect done\n");
#endif




  /* delete all old heap pages */
  if(old_heap_pages){
    ikpages* p = old_heap_pages;
    do{
      ikpages* next = p->next;
      ik_munmap_from_segment(p->base, p->size, pcb);
      ik_free(p, sizeof(ikpages));
      p=next;
    } while(p);
    old_heap_pages = 0;
  }

  int free_space = 
    ((unsigned int)pcb->allocation_redline) - 
    ((unsigned int)pcb->allocation_pointer);
  if((free_space <= mem_req) || (pcb->heap_size < IK_HEAPSIZE)){
#ifndef NDEBUG
    fprintf(stderr, "REQ=%d, got %d\n", mem_req, free_space);
#endif
    int memsize = (mem_req > IK_HEAPSIZE) ? mem_req : IK_HEAPSIZE;
    memsize = align_to_next_page(memsize);
    ik_munmap_from_segment(
        pcb->heap_base,
        pcb->heap_size,
        pcb);
    ikp ptr = ik_mmap_mixed(memsize+2*pagesize, pcb);
    pcb->allocation_pointer = ptr;
    pcb->allocation_redline = ptr+memsize;
    pcb->heap_base = ptr;
    pcb->heap_size = memsize+2*pagesize;
  }

#ifndef NDEBUG
  ikp x = pcb->allocation_pointer;
  while(x < pcb->allocation_redline){
    ref(x, 0) = (ikp)(0x1234FFFF);
    x+=wordsize;
  }
#endif
#ifndef NDEBUG
  verify_integrity(pcb, "exit");
#endif

  getrusage(RUSAGE_SELF, &t1);
  gettimeofday(&rt1, 0);

  pcb->collect_utime.tv_usec += t1.ru_utime.tv_usec - t0.ru_utime.tv_usec;
  pcb->collect_utime.tv_sec += t1.ru_utime.tv_sec - t0.ru_utime.tv_sec;
  if (pcb->collect_utime.tv_usec >= 1000000){
   pcb->collect_utime.tv_usec -= 1000000;
   pcb->collect_utime.tv_sec += 1;
  }
  else if (pcb->collect_utime.tv_usec < 0){
   pcb->collect_utime.tv_usec += 1000000;
   pcb->collect_utime.tv_sec -= 1;
  }
 
  pcb->collect_stime.tv_usec += t1.ru_stime.tv_usec - t0.ru_stime.tv_usec;
  pcb->collect_stime.tv_sec += t1.ru_stime.tv_sec - t0.ru_stime.tv_sec;
  if (pcb->collect_stime.tv_usec >= 1000000){
   pcb->collect_stime.tv_usec -= 1000000;
   pcb->collect_stime.tv_sec += 1;
  }
  else if (pcb->collect_stime.tv_usec < 0){
   pcb->collect_stime.tv_usec += 1000000;
   pcb->collect_stime.tv_sec -= 1;
  }
 
  pcb->collect_rtime.tv_usec += rt1.tv_usec - rt0.tv_usec;
  pcb->collect_rtime.tv_sec += rt1.tv_sec - rt0.tv_sec;
  if (pcb->collect_rtime.tv_usec >= 1000000){
   pcb->collect_rtime.tv_usec -= 1000000;
   pcb->collect_rtime.tv_sec += 1;
  }
  else if (pcb->collect_rtime.tv_usec < 0){
   pcb->collect_rtime.tv_usec += 1000000;
   pcb->collect_rtime.tv_sec -= 1;
  }
 

  return pcb;
}

static inline int
is_live(ikp x, gc_t* gc){
  if(is_fixnum(x)){ 
    return 1;
  } 
  int tag = tagof(x);
  if(tag == immediate_tag){
    return 1;
  }
  if(ref(x, -tag) == forward_ptr){
    return 1;
  }
  unsigned int t = gc->segment_vector[page_index(x)];
  int gen = t & gen_mask;
  if(gen > gc->collect_gen){
    return 1;
  }
  return 0;
}

static inline int 
next_gen(int i){
  return ((i == generation_count) ? generation_count : (i+1));
}

static ik_ptr_page* 
move_guardian(ikp x, ik_ptr_page* dst, ik_ptr_page** cache){
  if((dst == 0) || (dst->count == ik_ptr_page_size)){
    ik_ptr_page* y = *cache;
    if(y){
      *cache = y->next;
    } else {
      y = ik_mmap(sizeof(ik_ptr_page));
    }
    y->count = 0;
    y->next = dst;
    dst = y;
  }
  dst->ptr[dst->count++] = x;
  return dst;
}


static int
forward_guardians_initial(gc_t* gc, 
    ik_ptr_page** cache,
    ik_ptr_page** dead_tc_dead_obj_list,
    ik_ptr_page** live_tc_dead_obj_list,
    ik_ptr_page** dead_tc_live_obj_list,
    ik_ptr_page** live_tc_live_obj_list){
  int gen;
  int seen_live_tc_dead_obj = 0;
  ikpcb* pcb = gc->pcb;
  /* move guardians[0..g] to dead/live lists */
  for(gen=0; gen<=gc->collect_gen; gen++){
    ik_ptr_page* src = pcb->guardians[gen];
    ik_ptr_page* dead_dead = 0;
    ik_ptr_page* dead_live = 0;
    ik_ptr_page* live_dead = 0;
    ik_ptr_page* live_live = 0;
    while(src){
      int i;
      int n = src->count;
      for(i=0; i<n; i++){
        ikp a = src->ptr[i];
        ikp tc = ref(a, off_car);
        ikp obj = ref(a, off_cdr);
        if(is_live(tc, gc)){
          if(is_live(obj, gc)){
            live_live = move_guardian(a, live_live, cache);
          } else {
            live_dead = move_guardian(a, live_dead, cache);
          }
        } else {
          if(is_live(obj, gc)){
            dead_live = move_guardian(a, dead_live, cache);
          } else {
            dead_dead = move_guardian(a, dead_dead, cache);
          }
        }
      }
      ik_ptr_page* next = src->next;
      src->next = *cache;
      *cache = src;
      src = next;
    }
    if(live_dead) {
      seen_live_tc_dead_obj = 1;
    }
    dead_tc_dead_obj_list[gen] = dead_dead;
    live_tc_dead_obj_list[gen] = live_dead;
    dead_tc_live_obj_list[gen] = dead_live;
    live_tc_live_obj_list[gen] = live_live;
    pcb->guardians[gen] = 0;
  }
  return seen_live_tc_dead_obj;
}

static void
forward_guardians_revive_dead(gc_t* gc, 
    ik_ptr_page** live_tc_dead_obj_list){
  int gen;
  for(gen=0; gen<=gc->collect_gen; gen++){
    ik_ptr_page* src = live_tc_dead_obj_list[gen];
    while(src){
      int i;
      int n = src->count;
      for(i=0; i<n; i++){
        ikp a = src->ptr[i];
        ikp obj = ref(a, off_cdr);
        add_object(gc, obj, "guardian1");
      }
      src = src->next;
    }
  }
}

static int
forward_guardians_process_tconcs(gc_t* gc, 
    ik_ptr_page** cache,
    ik_ptr_page** dead_tc_dead_obj_list,
    ik_ptr_page** live_tc_dead_obj_list,
    ik_ptr_page** dead_tc_live_obj_list,
    ik_ptr_page** live_tc_live_obj_list){
  int gen;
  int some_were_revived = 0;
  /* objects in dead_tc_live_obj_list whos tconcs have become 
     alive must move to live_tc_live_obj list */
  for(gen=0; gen<=gc->collect_gen; gen++){
    ik_ptr_page* src = dead_tc_live_obj_list[gen];
    ik_ptr_page* live = live_tc_live_obj_list[gen];
    ik_ptr_page* dead = 0;
    while(src){
      int i;
      int n = src->count;
      for(i=0; i<n; i++){
        ikp a = src->ptr[i];
        ikp tc = ref(a, off_car);
        if(is_live(tc, gc)){
          live = move_guardian(a, live, cache);
        } else {
          dead = move_guardian(a, dead, cache);
        }
      }
      ik_ptr_page* next = src->next;
      src->next = *cache;
      *cache = src;
      src = next;
    }
    live_tc_live_obj_list[gen] = live;
    dead_tc_live_obj_list[gen] = dead;
  }
  /* objects in dead_tc_dead_obj_list whos tconcs have become
     alive must move to live_tc_dead_obj list
     obj must be resurrected */
  for(gen=0; gen<=gc->collect_gen; gen++){
    ik_ptr_page* src = dead_tc_dead_obj_list[gen];
    ik_ptr_page* dead = 0;
    ik_ptr_page* live = live_tc_dead_obj_list[gen];
    while(src){
      int i;
      int n = src->count;
      for(i=0; i<n; i++){
        ikp a = src->ptr[i];
        ikp tc = ref(a, off_car);
        if(is_live(tc, gc)){
          some_were_revived = 1;
          add_object(gc, ref(a, off_cdr), "guardian2");
          live = move_guardian(a, live, cache);
        } else {
          dead = move_guardian(a, dead, cache);
        }
      }
      ik_ptr_page* next = src->next;
      src->next = *cache;
      *cache = src;
      src = next;
    }
    live_tc_dead_obj_list[gen] = live;
    dead_tc_dead_obj_list[gen] = dead;
  }
  return some_were_revived;
}




static void 
forward_guardians(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  ik_ptr_page* dead_tc_dead_obj_list[generation_count];
  ik_ptr_page* live_tc_dead_obj_list[generation_count];
  ik_ptr_page* dead_tc_live_obj_list[generation_count];
  ik_ptr_page* live_tc_live_obj_list[generation_count];
  ik_ptr_page* cache = 0;
  int seen_live_tc_dead_obj =
    forward_guardians_initial(gc, &cache,
      dead_tc_dead_obj_list, live_tc_dead_obj_list,
      dead_tc_live_obj_list, live_tc_live_obj_list);
  if (seen_live_tc_dead_obj){
  /* objects in live_tc_dead_obj_list must be revived */
    forward_guardians_revive_dead(gc, live_tc_dead_obj_list);
    int some_were_revived = 1;
    while(some_were_revived){
      collect_loop(gc);
      some_were_revived = 
        forward_guardians_process_tconcs(gc, &cache,
            dead_tc_dead_obj_list, live_tc_dead_obj_list,
            dead_tc_live_obj_list, live_tc_live_obj_list);
    }
  }
  /* dead_tc_dead_obj_list and dead_tc_live_obj_list must go away */
  int gen;
  for(gen=0; gen<=gc->collect_gen; gen++){
    ik_ptr_page* src,* dst;
    src = dead_tc_dead_obj_list[gen];
    while(src){
      ik_ptr_page* next = src->next;
      src->next = cache;
      cache = src;
      src = next;
    }
    src = dead_tc_live_obj_list[gen];
    while(src){
      ik_ptr_page* next = src->next;
      src->next = cache;
      cache = src;
      src = next;
    }
    /* make all pairs in live_tc_dead_obj live */
    src = live_tc_dead_obj_list[gen];
    pcb->guardians_dropped[gen] = src;
    while(src){
      int i;
      int n = src->count;
      for(i=0; i<n; i++){
        src->ptr[i] = 
          add_object(gc, src->ptr[i], "guardian3");
      }
      src = src->next;
    }
    /* make all pairs in live_tc_live_obj live and 
       add them to next gen */
    src = live_tc_live_obj_list[gen];
    dst = pcb->guardians[next_gen(gen)];
    while(src){
      int i;
      int n = src->count;
      for(i=0; i<n; i++){
        dst = move_guardian(add_object(gc, src->ptr[i], "g4"), dst, &cache);
      }
      ik_ptr_page* next = src->next;
      src->next = cache;
      cache = src;
      src = next;
    }
    pcb->guardians[next_gen(gen)] = dst;
  }
  collect_loop(gc);
  while(cache){
    ik_ptr_page* next = cache->next;
    ik_munmap((unsigned char*)cache, sizeof(ik_ptr_page));
    cache = next;
  }
}

static void 
empty_dropped_guardians(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  int gen;
  for(gen = 0; gen<=gc->collect_gen; gen++){
    ik_ptr_page* src = pcb->guardians_dropped[gen];
    while(src){
      int i;
      int n = src->count;
      for(i=0; i<n; i++){
        ikp a = src->ptr[i];
        ikp tc = ref(a, off_car);
        ikp obj = ref(a, off_cdr);
        assert(tagof(tc) == pair_tag);
        ikp d = ref(tc, off_cdr);
        assert(tagof(d) == pair_tag);
        ref(d, off_car) = obj;
        ref(d, off_cdr) = a;
        ref(a, off_car) = false_object;
        ref(a, off_cdr) = false_object;
        ref(tc, off_cdr) = a;
        pcb->dirty_vector[page_index(tc)] = -1;
        //pcb->dirty_vector[page_index(d)] = -1;
        {
          int dgen = pcb->segment_vector[page_index(d)] & gen_mask;
          if( (dgen > (pcb->segment_vector[page_index(obj)] & gen_mask)) 
              ||
              (dgen > (pcb->segment_vector[page_index(a)] & gen_mask))){
            pcb->dirty_vector[page_index(d)] = -1;
          }
        }
      }
      ik_ptr_page* next = src->next;
      ik_munmap((unsigned char*) src, sizeof(ik_ptr_page));
      src = next;
    }
    pcb->guardians_dropped[gen] = 0;
  }
}

 
#define disp_frame_offset -13
#define disp_multivalue_rp -9


#define CODE_EXTENSION_SIZE (pagesize)

static int alloc_code_count = 0;


static ikp 
add_code_entry(gc_t* gc, ikp entry){
  ikp x = entry - disp_code_data;
  if(ref(x,0) == forward_ptr){
    return ref(x,wordsize) + off_code_data;
  }
  int idx = page_index(x);
  unsigned int t = gc->segment_vector[idx];
  int gen = t & gen_mask;
  if(gen > gc->collect_gen){
    return entry;
  }
  int code_size = unfix(ref(x, disp_code_code_size));
  ikp reloc_vec = ref(x, disp_code_reloc_vector);
  ikp freevars = ref(x, disp_code_freevars);
  ikp annotation = ref(x, disp_code_annotation);
  int required_mem = align(disp_code_data + code_size);
  if(required_mem >= pagesize){
    int new_tag = gc->collect_gen_tag;
    int idx = page_index(x);
    gc->segment_vector[idx] = new_tag | code_mt;
    int i;
    for(i=pagesize, idx++; i<required_mem; i+=pagesize, idx++){
      gc->segment_vector[idx] = new_tag | data_mt;
    }
    qupages_t* p = ik_malloc(sizeof(qupages_t));
    p->p = x;
    p->q = x+required_mem;
    p->next = gc->queues[meta_code];
    gc->queues[meta_code] = p;
    return entry;
  } else {
    ikp y = gc_alloc_new_code(required_mem, gc);
    ref(y, 0) = code_tag;
    ref(y, disp_code_code_size) = fix(code_size);
    ref(y, disp_code_reloc_vector) = reloc_vec;
    ref(y, disp_code_freevars) = freevars;
    ref(y, disp_code_annotation) = annotation;
    memcpy(y+disp_code_data, x+disp_code_data, code_size);
    ref(x, 0) = forward_ptr;
    ref(x, wordsize) = y + vector_tag;
    return y+disp_code_data;
  }
}


#define DEBUG_STACK 0

static void collect_stack(gc_t* gc, ikp top, ikp end){
  if(DEBUG_STACK){
    fprintf(stderr, "collecting stack from 0x%08x .. 0x%08x\n", 
        (int) top, (int) end);
  }
  while(top < end){
    if(DEBUG_STACK){
      fprintf(stderr, "collecting frame at 0x%08x: ", (int) top);
    }
    ikp rp = ref(top, 0);
    int rp_offset = unfix(ref(rp, disp_frame_offset));
    if(DEBUG_STACK){
      fprintf(stderr, "rp_offset=%d\n", rp_offset);
    }
    if(rp_offset <= 0){
      fprintf(stderr, "invalid rp_offset %d\n", rp_offset);
      exit(-1);
    }
    /* since the return point is alive, we need to find the code
     * object containing it and mark it live as well.  the rp is
     * updated to reflect the new code object. */

    int code_offset = rp_offset - disp_frame_offset;
    ikp code_entry = rp - code_offset;
    ikp new_code_entry = add_code_entry(gc, code_entry);
    ikp new_rp = new_code_entry + code_offset;
    ref(top, 0) = new_rp;

    /* now for some livemask action.
     * every return point has a live mark above it.  the live mask
     * is a sequence of bytes (every byte for 8 frame cells).  the 
     * size of the live mask is determined by the size of the frame.
     * this is how the call frame instruction sequence looks like:
     *
     *   |    ...     |
     *   | code  junk |
     *   +------------+
     *   |   byte 0   |   for fv0 .. fv7
     *   |   byte 1   |   for fv8 .. fv15
     *   |    ...     |   ...
     *   +------------+
     *   |  framesize |
     *   |    word    |
     *   +------------+
     *   | multivalue |
     *   |    word    |
     *   +------------+
     *   | frameoffst |  the frame offset determined how far its
     *   |    word    |  address is off from the start of the code
     *   +------------+
     *   |  padding   |  the size of this part is fixed so that we
     *   |  and call  |  can correlate the frame info (above) with rp
     *   +------------+
     *   | code  junk | <---- rp
     *   |    ...     | 
     *
     *   WITH ONE EXCEPTION:
     *   if the framesize is 0, then the actual frame size is stored
     *   on the stack immediately below the return point.
     *   there is no live mask in this case, instead all values in the
     *   frame are live.
     */
    int framesize = (int) ref(rp, disp_frame_size);
    if(DEBUG_STACK){
      fprintf(stderr, "fs=%d\n", framesize);
    }
    if(framesize < 0){
      fprintf(stderr, "invalid frame size %d\n", framesize);
      exit(-1);
    } 
    else if(framesize == 0){
      framesize = (int)ref(top, wordsize);
      if(framesize <= 0){
        fprintf(stderr, "invalid redirected framesize=%d\n", framesize);
        exit(-1);
      }
      ikp base = top + framesize - wordsize;
      while(base > top){
        ikp new_obj = add_object(gc,ref(base,0), "frame");
        ref(base,0) = new_obj;
        base -= wordsize;
      }
    } else {
      int frame_cells = framesize >> fx_shift;
      int bytes_in_mask = (frame_cells+7) >> 3;
      unsigned char* mask = rp + disp_frame_size  - bytes_in_mask;
  
      ikp* fp = (ikp*)(top + framesize);
      int i;
      for(i=0; i<bytes_in_mask; i++, fp-=8){
        unsigned char m = mask[i];
#if DEBUG_STACK
        fprintf(stderr, "m[%d]=0x%x\n", i, m);
#endif
        if(m & 0x01) { fp[-0] = add_object(gc, fp[-0], "frame0"); }
        if(m & 0x02) { fp[-1] = add_object(gc, fp[-1], "frame1"); }
        if(m & 0x04) { fp[-2] = add_object(gc, fp[-2], "frame2"); }
        if(m & 0x08) { fp[-3] = add_object(gc, fp[-3], "frame3"); }
        if(m & 0x10) { fp[-4] = add_object(gc, fp[-4], "frame4"); }
        if(m & 0x20) { fp[-5] = add_object(gc, fp[-5], "frame5"); }
        if(m & 0x40) { fp[-6] = add_object(gc, fp[-6], "frame6"); }
        if(m & 0x80) { fp[-7] = add_object(gc, fp[-7], "frame7"); }
      }
    }
    top += framesize;
  }
  if(top != end){
    fprintf(stderr, "frames did not match up 0x%08x .. 0x%08x\n",
        (int) top, (int) end);
    exit(-1);
  }
  if(DEBUG_STACK){
    fprintf(stderr, "done with stack!\n");
  }
}


static void 
add_list(gc_t* gc, unsigned int t, ikp x, ikp* loc){
  int collect_gen = gc->collect_gen;
  while(1){
    ikp fst = ref(x, off_car);
    ikp snd = ref(x, off_cdr);
    ikp y;
    if((t & type_mask) != weak_pairs_type){
      y = gc_alloc_new_pair(gc) + pair_tag;
    } else {
      y = gc_alloc_new_weak_pair(gc) + pair_tag;
    }
    *loc = y;
    ref(x,off_car) = forward_ptr;
    ref(x,off_cdr) = y;
    ref(y,off_car) = fst;
    int stag = tagof(snd);
    if(stag == pair_tag){
      if(ref(snd, -pair_tag) == forward_ptr){
        ref(y, off_cdr) = ref(snd, wordsize-pair_tag);
        return;
      } 
      else {
        t = gc->segment_vector[page_index(snd)];
        int gen = t & gen_mask;
        if(gen > collect_gen){
          ref(y, off_cdr) = snd;
          return;
        } else {
          x = snd;
          loc = (ikp*)(y + off_cdr);
          /* don't return */
        }
      }
    }
    else if(   (stag == immediate_tag)
            || (stag == 0) 
            || (stag == (1<<fx_shift))) {
      ref(y,off_cdr) = snd;
      return;
    }
    else if (ref(snd, -stag) == forward_ptr){
      ref(y, off_cdr) = ref(snd, wordsize-stag);
      return;
    }
    else {
      ref(y, off_cdr) = add_object(gc, snd, "add_list");
      return;
    }
  }
}


static ikp 
#ifndef NDEBUG
add_object_proc(gc_t* gc, ikp x, char* caller)
#else
add_object_proc(gc_t* gc, ikp x)
#endif
{
  if(is_fixnum(x)){ 
    return x;
  } 
  assert(x != forward_ptr);
  int tag = tagof(x);
  if(tag == immediate_tag){
    return x;
  }
  ikp fst = ref(x, -tag);
  if(fst == forward_ptr){
    /* already moved */
    return ref(x, wordsize-tag);
  }
  unsigned int t = gc->segment_vector[page_index(x)];
  int gen = t & gen_mask;
  if(gen > gc->collect_gen){
    return x;
  }
  if(tag == pair_tag){
    ikp y;
    add_list(gc, t, x, &y);
    return y;
  }
#if 0
  else if(tag == symbol_tag){
    //ikp y = gc_alloc_new_ptr(align(symbol_size),gen, gc) + symbol_tag;
    ikp y = gc_alloc_new_symbol(gen, gc) + symbol_tag;
    ref(y, off_symbol_string)       = ref(x, off_symbol_string);
    ref(y, off_symbol_ustring)      = ref(x, off_symbol_ustring);
    ref(y, off_symbol_value)        = ref(x, off_symbol_value);
    ref(y, off_symbol_plist)        = ref(x, off_symbol_plist);
    ref(y, off_symbol_system_value) = ref(x, off_symbol_system_value);
    ref(y, off_symbol_code)         = ref(x, off_symbol_code);
    ref(y, off_symbol_errcode)      = ref(x, off_symbol_errcode);
    ref(y, off_symbol_unused)       = 0;
    ref(x, -symbol_tag) = forward_ptr;
    ref(x, wordsize-symbol_tag) = y;
#if accounting
      symbol_count++;
#endif
    return y;
  }
#endif
  else if(tag == closure_tag){
    int size = disp_closure_data+
              (int) ref(fst, disp_code_freevars - disp_code_data);
    if(size > 1024){
      fprintf(stderr, "large closure size=0x%08x\n", size);
    }
    int asize = align(size);
    ikp y = gc_alloc_new_ptr(asize, gc) + closure_tag;
    ref(y, asize-closure_tag-wordsize) = 0;
    memcpy(y-closure_tag, x-closure_tag, size);
    ref(y,-closure_tag) = add_code_entry(gc, ref(y,-closure_tag));
    ref(x,-closure_tag) = forward_ptr;
    ref(x,wordsize-closure_tag) = y;
#if accounting
    closure_count++;
#endif
    return y;
  }
  else if(tag == vector_tag){
    if(is_fixnum(fst)){
      /* real vector */
      //fprintf(stderr, "X=0x%08x, FST=0x%08x\n", (int)x, (int)fst);
      int size = (int)fst;
      assert(size >= 0);
      int memreq = align(size + disp_vector_data);
      if(memreq >= pagesize){
        if((t & large_object_mask) == large_object_tag){
          enqueue_large_ptr(x-vector_tag, size+disp_vector_data, gc);
          return x;
        } else {
          ikp y = gc_alloc_new_large_ptr(size+disp_vector_data, gc)
                   + vector_tag;
          ref(y, disp_vector_length-vector_tag) = fst;
          ref(y, memreq-vector_tag-wordsize) = 0;
          memcpy(y+off_vector_data, x+off_vector_data, size);
          ref(x,-vector_tag) = forward_ptr;
          ref(x,wordsize-vector_tag) = y;
          return y;
        }
      } else {
        ikp y = gc_alloc_new_ptr(memreq, gc) + vector_tag;
        ref(y, disp_vector_length-vector_tag) = fst;
        ref(y, memreq-vector_tag-wordsize) = 0;
        memcpy(y+off_vector_data, x+off_vector_data, size);
        ref(x,-vector_tag) = forward_ptr;
        ref(x,wordsize-vector_tag) = y;
        return y;
      }
#if accounting
      vector_count++;
#endif
    } 
    else if(fst == symbol_record_tag){
      ikp y = gc_alloc_new_symbol_record(gc) + record_tag;
      ref(y, -record_tag)               = symbol_record_tag;
      ref(y, off_symbol_record_string)  = ref(x, off_symbol_record_string);
      ref(y, off_symbol_record_ustring) = ref(x, off_symbol_record_ustring);
      ref(y, off_symbol_record_value)   = ref(x, off_symbol_record_value);
      ref(y, off_symbol_record_proc)    = ref(x, off_symbol_record_proc);
      ref(y, off_symbol_record_plist)   = ref(x, off_symbol_record_plist);
      ref(x, -record_tag) = forward_ptr; 
      ref(x, wordsize-record_tag) = y;
      return y;
    }
    else if(tagof(fst) == rtd_tag){
      /* struct / record */
      int size = (int) ref(fst, off_rtd_length);
      if(size & ((1<<align_shift)-1)) {
        /* size = n * object_alignment + 4 =>
           memreq = n * object_alignment + 8 
                  = (n+1) * object_alignment  => aligned */
        ikp y = gc_alloc_new_ptr(size+wordsize, gc) + vector_tag;
        ref(y, -vector_tag) = fst;
        {
          int i;
          ikp p = y+disp_record_data-vector_tag;
          ikp q = x+disp_record_data-vector_tag;
          ref(p, 0) = ref(q, 0);
          for(i=wordsize; i<size; i+=(2*wordsize)){
            ref(p, i) = ref(q, i);
            ref(p, i+wordsize) = ref(q, i+wordsize);
          }
        }
        ref(x,-vector_tag) = forward_ptr;
        ref(x,wordsize-vector_tag) = y;
        return y;
      } else {
        /* size = n * object_alignment =>
           memreq = n * object_alignment + 4 + 4 (pad) */
        ikp y = gc_alloc_new_ptr(size+(2*wordsize), gc) + vector_tag;
        ref(y, -vector_tag) = fst;
        {
          int i;
          ikp p = y+disp_record_data-vector_tag;
          ikp q = x+disp_record_data-vector_tag;
          for(i=0; i<size; i+=(2*wordsize)){
            ref(p, i) = ref(q, i);
            ref(p, i+wordsize) = ref(q, i+wordsize);
          }
        }
        ref(y, size+disp_record_data-vector_tag) = 0;
        ref(x,-vector_tag) = forward_ptr;
        ref(x,wordsize-vector_tag) = y;
        return y;
      }
    }
    else if(fst == code_tag){
      ikp entry = x + off_code_data;
      ikp new_entry = add_code_entry(gc, entry);
      return new_entry - off_code_data;
    } 
    else if(fst == continuation_tag){
      ikp top = ref(x, off_continuation_top);
      int size = (int) ref(x, off_continuation_size);
#ifndef NDEBUG
      if(size > 4096){
        fprintf(stderr, "large cont size=0x%08x\n", size);
      }
#endif
      ikp next = ref(x, off_continuation_next);
      ikp y = gc_alloc_new_ptr(continuation_size, gc) + vector_tag;
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ikp new_top = gc_alloc_new_data(align(size), gc);
      memcpy(new_top, top, size);
      collect_stack(gc, new_top, new_top + size);
      ref(y, -vector_tag) = continuation_tag;
      ref(y, off_continuation_top) = new_top;
      ref(y, off_continuation_size) = (ikp) size;
      ref(y, off_continuation_next) = next;
#if accounting
      continuation_count++;
#endif
      return y;
    }
    else if(tagof(fst) == pair_tag){
      /* tcbucket */
      ikp y = gc_alloc_new_ptr(tcbucket_size, gc) + vector_tag;
      ref(y,off_tcbucket_tconc) = fst;
      ikp key = ref(x, off_tcbucket_key);
      ref(y,off_tcbucket_key) = key;
      ref(y,off_tcbucket_val) = ref(x, off_tcbucket_val);
      ref(y,off_tcbucket_next) = ref(x, off_tcbucket_next);
      if((! is_fixnum(key)) && (tagof(key) != immediate_tag)){
        unsigned int kt = gc->segment_vector[page_index(key)];
        if((kt & gen_mask) <= gc->collect_gen){
          /* key will be moved */
          gc_tconc_push(gc, y);
        }
      }
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      return y;
    }
    else if((((int)fst) & port_mask) == port_tag){
      ikp y = gc_alloc_new_ptr(port_size, gc) + vector_tag;
      ref(y, -vector_tag) = fst;
      int i;
      for(i=wordsize; i<port_size; i+=wordsize){
        ref(y, i-vector_tag) = ref(x, i-vector_tag);
      }
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      return y;
    }
    else if(fst == flonum_tag){
      ikp new = gc_alloc_new_data(flonum_size, gc) + vector_tag;
      ref(new, -vector_tag) = flonum_tag;
      flonum_data(new) = flonum_data(x);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = new;
      return new;
    }
    else if((((int)fst) & bignum_mask) == bignum_tag){
      int len = ((unsigned int)fst) >> bignum_length_shift;
      int memreq = align(disp_bignum_data + len*wordsize);
      ikp new = gc_alloc_new_data(memreq, gc) + vector_tag;
      memcpy(new-vector_tag, x-vector_tag, memreq);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = new;
      return new;
    }
    else if(fst == ratnum_tag){
      ikp y = gc_alloc_new_data(ratnum_size, gc) + vector_tag;
      ikp num = ref(x, disp_ratnum_num-vector_tag);
      ikp den = ref(x, disp_ratnum_den-vector_tag);
      ref(x, -vector_tag) = forward_ptr;
      ref(x, wordsize-vector_tag) = y;
      ref(y, -vector_tag) = fst;
      ref(y, disp_ratnum_num-vector_tag) = add_object(gc, num, "num");
      ref(y, disp_ratnum_den-vector_tag) = add_object(gc, den, "den");
      return y;
    }
    else {
      fprintf(stderr, "unhandled vector with fst=0x%08x\n", (int)fst);
      assert(0);
      exit(-1);
    }
  }
  else if(tag == string_tag){
    if(is_fixnum(fst)){
      int strlen = unfix(fst);
      int memreq = align(strlen*string_char_size + disp_string_data);
      ikp new_str = gc_alloc_new_data(memreq, gc) + string_tag;
      ref(new_str, off_string_length) = fst;
      memcpy(new_str+off_string_data,
             x + off_string_data,
             strlen*string_char_size);
      ref(x, -string_tag) = forward_ptr;
      ref(x, wordsize-string_tag) = new_str;
#if accounting
      string_count++;
#endif
      return new_str;
    }
    else {
      fprintf(stderr, "unhandled string 0x%08x with fst=0x%08x\n",
          (int)x, (int)fst);
      exit(-1);
    }
  }
  else if(tag == bytevector_tag){
    int len = unfix(fst);
    int memreq = align(len + disp_bytevector_data + 1);
    ikp new_bv = gc_alloc_new_data(memreq, gc) + bytevector_tag;
    ref(new_bv, off_bytevector_length) = fst;
    memcpy(new_bv+off_bytevector_data,
           x + off_bytevector_data,
           len + 1);
    ref(x, -bytevector_tag) = forward_ptr;
    ref(x, wordsize-bytevector_tag) = new_bv;
    return new_bv;
  }
  fprintf(stderr, "unhandled tag: %d\n", tag);
  exit(-1);
}

static void
relocate_new_code(ikp x, gc_t* gc){
  ikp relocvector = ref(x, disp_code_reloc_vector);
  relocvector = add_object(gc, relocvector, "relocvec");
  ref(x, disp_code_reloc_vector) = relocvector;
  ref(x, disp_code_annotation) = 
    add_object(gc, ref(x, disp_code_annotation), "annotation");
  int relocsize = (int)ref(relocvector, off_vector_length);
  ikp p = relocvector + off_vector_data;
  ikp q = p + relocsize;
  ikp code = x + disp_code_data;
  while(p < q){
    int r = unfix(ref(p, 0));
    int tag = r & 3;
    int code_off = r >> 2;
    if(tag == 0){
      /* undisplaced pointer */
#ifndef NDEBUG
     // fprintf(stderr, "r=0x%08x code_off=%d reloc_size=0x%08x\n",
     //     r, code_off, relocsize);
#endif
      ikp old_object = ref(p, wordsize);
      ikp new_object = add_object(gc, old_object, "reloc1");
      ref(code, code_off) = new_object;
      p += (2*wordsize);
    }
    else if(tag == 2){
      /* displaced pointer */
      int obj_off = unfix(ref(p, wordsize));
      ikp old_object = ref(p, 2*wordsize);
      ikp new_object = add_object(gc, old_object, "reloc2");
      ref(code, code_off) = new_object + obj_off;
      p += (3 * wordsize);
    } 
    else if(tag == 3){
      /* displaced relative pointer */
      int obj_off = unfix(ref(p, wordsize));
      ikp obj = ref(p, 2*wordsize);
#ifndef NDEBUG
      //fprintf(stderr, "obj=0x%08x, obj_off=0x%08x\n", (int)obj,
      //    obj_off);
#endif
      obj = add_object(gc, obj, "reloc3");
      ikp displaced_object = obj + obj_off;
      ikp next_word = code + code_off + wordsize;
      ikp relative_distance = displaced_object - (int)next_word;
      ref(next_word, -wordsize) = relative_distance;
      p += (3*wordsize);
    }
    else if(tag == 1){
      /* do nothing */
      p += (2 * wordsize);
    }
    else {
      fprintf(stderr, "invalid rtag %d in 0x%08x\n", tag, r);
      exit(-1);
    }
  }
}



static void 
collect_loop(gc_t* gc){
  int done;
  do{
    done = 1;
    { /* scan the pending pairs pages */
      qupages_t* qu = gc->queues[meta_pair];
      if(qu){
        done = 0;
        gc->queues[meta_pair] = 0;
        do{
          ikp p = qu->p;
          ikp q = qu->q;
          while(p < q){
            ref(p,0) = add_object(gc, ref(p,0), "loop");
            p += (2*wordsize);
          }
          qupages_t* next = qu->next;
          ik_free(qu, sizeof(qupages_t));
          qu = next;
        } while(qu);
      }
    }

    { /* scan the pending pointer pages */
      qupages_t* qu = gc->queues[meta_ptrs];
      if(qu){
        done = 0;
        gc->queues[meta_ptrs] = 0;
        do{
          ikp p = qu->p;
          ikp q = qu->q;
          while(p < q){
            ref(p,0) = add_object(gc, ref(p,0), "pending");
            p += wordsize;
          }
          qupages_t* next = qu->next;
          ik_free(qu, sizeof(qupages_t));
          qu = next;
        } while(qu);
      }
    }

    { /* scan the pending symbol pages */
      qupages_t* qu = gc->queues[meta_symbol];
      if(qu){
        done = 0;
        gc->queues[meta_symbol] = 0;
        do{
          ikp p = qu->p;
          ikp q = qu->q;
          while(p < q){
            ref(p,0) = add_object(gc, ref(p,0), "symbols");
            p += wordsize;
          }
          qupages_t* next = qu->next;
          ik_free(qu, sizeof(qupages_t));
          qu = next;
        } while(qu);
      }
    }

    { /* scan the pending code objects */
      qupages_t* codes = gc->queues[meta_code];
      if(codes){
        gc->queues[meta_code] = 0;
        done = 0;
        do{
          ikp p = codes->p;
          ikp q = codes->q;
          while(p < q){
            relocate_new_code(p, gc);
            alloc_code_count--;
            p += align(disp_code_data + unfix(ref(p, disp_code_code_size))); 
          }
          qupages_t* next = codes->next;
          ik_free(codes, sizeof(qupages_t));
          codes = next;
        } while(codes);
      }
    }
    {/* see if there are any remaining in the main ptr segment */
      {
        meta_t* meta = &gc->meta[meta_pair];
        ikp p = meta->aq;
        ikp q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            while(p < q){
              ref(p,0) = add_object(gc, ref(p,0), "rem");
              p += (2*wordsize);
            }
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
      {
        meta_t* meta = &gc->meta[meta_symbol];
        ikp p = meta->aq;
        ikp q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            while(p < q){
              ref(p,0) = add_object(gc, ref(p,0), "sym");
              p += wordsize;
            }
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
      {
        meta_t* meta = &gc->meta[meta_ptrs];
        ikp p = meta->aq;
        ikp q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            while(p < q){
              ref(p,0) = add_object(gc, ref(p,0), "rem2");
              p += wordsize;
            }
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
      {
        meta_t* meta = &gc->meta[meta_code];
        ikp p = meta->aq;
        ikp q = meta->ap;
        if(p < q){
          done = 0;
          do{
            meta->aq = q;
            do{
              alloc_code_count--;
              relocate_new_code(p, gc);
              p += align(disp_code_data + unfix(ref(p, disp_code_code_size)));
            } while (p < q);
            p = meta->aq;
            q = meta->ap;
          } while (p < q);
        }
      }
    }
    /* phew */
  } while (! done);
  {
    /* zero out remaining pointers */
    /* FIXME: did you hear of code reuse? */
    {
      meta_t* meta = &gc->meta[meta_pair];
      ikp p = meta->ap;
      ikp q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_symbol];
      ikp p = meta->ap;
      ikp q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_ptrs];
      ikp p = meta->ap;
      ikp q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_weak];
      ikp p = meta->ap;
      ikp q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
    {
      meta_t* meta = &gc->meta[meta_code];
      ikp p = meta->ap;
      ikp q = meta->ep;
      while(p < q){
        ref(p, 0) = 0;
        p += wordsize;
      }
    }
  }
}

static void
fix_weak_pointers(gc_t* gc){
  unsigned int* segment_vec = gc->segment_vector;
  ikpcb* pcb = gc->pcb;
  int lo_idx = page_index(pcb->memory_base);
  int hi_idx = page_index(pcb->memory_end);
  int i = lo_idx;
  int collect_gen = gc->collect_gen;
  while(i < hi_idx){
    unsigned int t = segment_vec[i];
    if((t & (type_mask|new_gen_mask)) ==
        (weak_pairs_type|new_gen_tag)){
      //int gen = t & gen_mask;
      if (1) { //(gen > collect_gen){
        ikp p = (ikp)(i << pageshift);
        ikp q = p + pagesize;
        while(p < q){
          ikp x = ref(p, 0);
          if(! is_fixnum(x)){
            int tag = tagof(x);
            if(tag != immediate_tag){
              ikp fst = ref(x, -tag);
              if(fst == forward_ptr){
                ref(p, 0) = ref(x, wordsize-tag);
              } else {
                int x_gen = segment_vec[page_index(x)] & gen_mask;
                if(x_gen <= collect_gen){
                  ref(p, 0) = bwp_object;
                } 
              }
            }
          }
          p += (2*wordsize); 
        }
      }
    }
    i++;
  }
}

static unsigned int dirty_mask[generation_count] = {
  0x88888888,
  0xCCCCCCCC,
  0xEEEEEEEE,
  0xFFFFFFFF,
  0x00000000
};


static unsigned int cleanup_mask[generation_count] = {
  0x00000000,
  0x88888888,
  0xCCCCCCCC,
  0xEEEEEEEE,
  0xFFFFFFFF
};



static void 
scan_dirty_pointers_page(gc_t* gc, int page_idx, int mask){
  unsigned int* segment_vec = gc->segment_vector;
  unsigned int* dirty_vec = gc->pcb->dirty_vector;
  unsigned int t = segment_vec[page_idx];
  unsigned int d = dirty_vec[page_idx];
  unsigned int masked_d = d & mask;
  ikp p = (ikp)(page_idx << pageshift);
  int j;
  unsigned int new_d = 0;
  for(j=0; j<cards_per_page; j++){
    if(masked_d & (0xF << (j*meta_dirty_shift))){
      /* dirty card */
      ikp q = p + cardsize;
      unsigned int card_d = 0;
      while(p < q){
        ikp x = ref(p, 0);
        if(is_fixnum(x) || (tagof(x) == immediate_tag)){
          /* do nothing */
        } else {
          ikp y = add_object(gc, x, "nothing");
          segment_vec = gc->segment_vector;
          ref(p, 0) = y;
          card_d = card_d | segment_vec[page_index(y)];
        }
        p += wordsize;
      }
      card_d = (card_d & meta_dirty_mask) >> meta_dirty_shift;
      new_d = new_d | (card_d<<(j*meta_dirty_shift));
    } else {
      p += cardsize;
      new_d = new_d | (d & (0xF << (j*meta_dirty_shift)));
    }
  }
  dirty_vec = gc->pcb->dirty_vector;
  new_d = new_d & cleanup_mask[t & gen_mask];
  dirty_vec[page_idx] = new_d;
}

static void            
scan_dirty_code_page(gc_t* gc, int page_idx, unsigned int mask){
  ikp p = (ikp)(page_idx << pageshift);
  ikp start = p;
  ikp q = p + pagesize;
  unsigned int* segment_vec = gc->segment_vector;
  unsigned int* dirty_vec = gc->pcb->dirty_vector;
  //unsigned int d = dirty_vec[page_idx];
  unsigned int t = segment_vec[page_idx];
  //unsigned int masked_d = d & mask;
  unsigned int new_d = 0;
  while(p < q){
    if(ref(p, 0) != code_tag){
      p = q;
    } 
    else {
      int j = ((int)p - (int)start) / cardsize;
      int code_size = unfix(ref(p, disp_code_code_size));
      relocate_new_code(p, gc);
      segment_vec = gc->segment_vector;
      ikp rvec = ref(p, disp_code_reloc_vector);
      int len = (int)ref(rvec, off_vector_length);
      assert(len >= 0);
      int i;
      unsigned int code_d = segment_vec[page_index(rvec)];
      for(i=0; i<len; i+=wordsize){
        ikp r = ref(rvec, i+off_vector_data);
        if(is_fixnum(r) || (tagof(r) == immediate_tag)){
          /* do nothing */
        } else {
          r = add_object(gc, r, "nothing2");
          segment_vec = gc->segment_vector;
          code_d = code_d | segment_vec[page_index(r)];
        }
      }
      new_d = new_d | (code_d<<(j*meta_dirty_shift));
      p += align(code_size + disp_code_data);
    }
  }
  dirty_vec = gc->pcb->dirty_vector;
  new_d = new_d & cleanup_mask[t & gen_mask];
  dirty_vec[page_idx] = new_d;
}





static void
scan_dirty_pages(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  int lo_idx = page_index(pcb->memory_base);
  int hi_idx = page_index(pcb->memory_end);
  unsigned int* dirty_vec = pcb->dirty_vector;
  unsigned int* segment_vec = pcb->segment_vector;
  int collect_gen = gc->collect_gen;
  unsigned int mask = dirty_mask[collect_gen];
  int i = lo_idx;
  while(i < hi_idx){
    unsigned int d = dirty_vec[i];
    if(d & mask){
      unsigned int t = segment_vec[i];
      if((t & gen_mask) > collect_gen){
        int type = t & type_mask;
        if(type == pointers_type){
          scan_dirty_pointers_page(gc, i, mask);
          dirty_vec = pcb->dirty_vector;
          segment_vec = pcb->segment_vector;
        }
        else if(type == symbols_type){
          scan_dirty_pointers_page(gc, i, mask);
          dirty_vec = pcb->dirty_vector;
          segment_vec = pcb->segment_vector;
        }
        else if (type == weak_pairs_type){
          scan_dirty_pointers_page(gc, i, mask);
          dirty_vec = pcb->dirty_vector;
          segment_vec = pcb->segment_vector;
        }
        else if (type == code_type){
          if((t & gen_mask) > collect_gen){
            scan_dirty_code_page(gc, i, mask);
            dirty_vec = pcb->dirty_vector;
            segment_vec = pcb->segment_vector;
          }
        }
        else if (t & scannable_mask) {
          fprintf(stderr, "BUG: unhandled scan of type 0x%08x\n", t);
          exit(-1);
        }
      }
    }
    i++;
  }
}




static void
deallocate_unused_pages(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  int collect_gen =  gc->collect_gen;
  unsigned int* segment_vec = pcb->segment_vector;
  unsigned char* memory_base = pcb->memory_base;
  unsigned char* memory_end = pcb->memory_end;
  int lo_idx = page_index(memory_base);
  int hi_idx = page_index(memory_end);
  int i = lo_idx;
  while(i < hi_idx){
    unsigned int t = segment_vec[i];
    if(t & dealloc_mask){
      int gen = t & old_gen_mask;
      if(gen <= collect_gen){
        /* we're interested */
        if(t & new_gen_mask){
          /* do nothing yet */
        } else {
          ik_munmap_from_segment((unsigned char*)(i<<pageshift),pagesize,pcb);
        }
      }
    }
    i++;
  }
}


static void
fix_new_pages(gc_t* gc){
  ikpcb* pcb = gc->pcb;
  unsigned int* segment_vec = pcb->segment_vector;
  unsigned char* memory_base = pcb->memory_base;
  unsigned char* memory_end = pcb->memory_end;
  int lo_idx = page_index(memory_base);
  int hi_idx = page_index(memory_end);
  int i = lo_idx;
  while(i < hi_idx){
    segment_vec[i] &= ~new_gen_mask;
    /*
    unsigned int t = segment_vec[i];
    if(t & new_gen_mask){
      segment_vec[i] = t & ~new_gen_mask;
    }
    */
    i++;
  }
} 

static void
add_one_tconc(ikpcb* pcb, ikp p){
  ikp tcbucket = ref(p,0);
  ikp tc = ref(tcbucket, off_tcbucket_tconc);
  assert(tagof(tc) == pair_tag);
  ikp d = ref(tc, off_cdr);
  assert(tagof(d) == pair_tag);
  ikp new_pair = p + pair_tag; 
  ref(d, off_car) = tcbucket;
  ref(d, off_cdr) = new_pair;
  ref(new_pair, off_car) = false_object;
  ref(new_pair, off_cdr) = false_object;
  ref(tc, off_cdr) = new_pair;
  ref(tcbucket, -vector_tag) = (ikp)(tcbucket_size - wordsize);
  pcb->dirty_vector[page_index(tc)] = -1;
  pcb->dirty_vector[page_index(d)] = -1;
}

static void
gc_add_tconcs(gc_t* gc){
  if(gc->tconc_base == 0){
    return;
  }
  ikpcb* pcb = gc->pcb;
  {
    ikp p = gc->tconc_base;
    ikp q = gc->tconc_ap;
    while(p < q){
      add_one_tconc(pcb, p);
      p += 2*wordsize;
    }
  }
  ikpages* qu = gc->tconc_queue;
  while(qu){
    ikp p = qu->base;
    ikp q = p + qu->size;
    while(p < q){
      add_one_tconc(pcb, p);
      p += 2*wordsize;
    }
    ikpages* next = qu->next;
    ik_free(qu, sizeof(ikpages));
    qu = next;
  }
}


