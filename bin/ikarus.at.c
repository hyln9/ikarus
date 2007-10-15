
junk junk junk

#include "ikarus.at.h"
#include <stdio.h>
#include <strings.h>
#include <assert.h>

#define page_size (4096)
#define segment_size (page_size * segment_pages)
#define align_to_next_segment(x) \
  ((((x)+segment_size-1)/segment_size)*segment_size)
#define align_to_next_page(x) \
  ((((x)+page_size-1)/page_size)*page_size)
#define segment_index(x) \
  (((unsigned int)(x)) / segment_size)
#define page_index(x) \
  (((unsigned int)(x)) / page_size)

#ifndef NDEBUG
static int malloc_count = 0;
static unsigned int mmap_count = 0;
static void ikat_malloc_count(size_t n){
  malloc_count += n;
  fprintf(stderr, "ikat_malloc_count=0x%08x (%ld)\n", malloc_count, n);
  assert(malloc_count >= 0);
}
static void ikat_mmap_count(size_t n){
  mmap_count += n;
  fprintf(stderr, "ikat_mmap_count=0x%08x (%ld)\n", mmap_count, n);
  assert(mmap_count >= 0);
}

static void ikat_done(){
  assert(malloc_count == 0);
  assert(mmap_count == 0);
}
#else
static inline void ikat_malloc_count(size_t n){}
static inline void ikat_done(){}
static inline void ikat_mmap_count(size_t n){}
#endif

static inline void*
ikat_malloc(size_t n){
  ikat_malloc_count(n);
  void* x =  malloc(n);
  if (x){
    return x;
  }
  fprintf(stderr, "Error in ikat_malloc\n");
  exit(-1);
}


static inline void
ikat_free(void* addr, size_t n){
  ikat_malloc_count(-n);
  free(addr);
}


static inline void* 
ikat_mmap(size_t n){
  assert(n == align_to_next_segment(n));
  ikat_mmap_count(n);
  void* x = mmap(0, n, PROT_READ|PROT_WRITE, MAP_ANON, -1, 0);
  if(x != (void*)-1){
    return x;
  }
  fprintf(stderr, "Error in ikat_mmap\n");
  exit(-1);
}


static inline void
ikat_munmap(void* addr, size_t n){
  fprintf(stderr, "unmap: ");
  ikat_mmap_count(-n);
  munmap(addr, n);
}


ikat*
ikat_make_allocation_table(unsigned int types){
  unsigned int* tbl = ikat_mmap(segment_size);
  bzero(tbl, segment_size);
  ikat* x = ikat_malloc(sizeof(ikat));
  bzero(x, sizeof(ikat));
  x->alloc_table = tbl;
  ikat_ll** lls = ikat_malloc(types * sizeof(ikat_ll*));
  bzero(lls, types * sizeof(ikat_ll*));
  x->lls_count = types;
  x->lls = lls;
  return x;
}

static void
ikat_free_ll(ikat* x, ikat_ll* p){
  while(p){
    ikat_ll* next = p->next;
    ikat_unmap(x, p->base, p->size);
    ikat_free(p, sizeof(ikat_ll));
    p = next;
  }
}


void 
ikat_free_allocation_table(ikat* x){
  int i;
  for(i=0; i<x->lls_count; i++){
    ikat_free_ll(x, x->lls[i]);
  }
  ikat_free_ll(x, x->llcache);
  ikat_free(x->lls, x->lls_count * sizeof(ikat_ll*));
  ikat_munmap(x->alloc_table, segment_size);
  ikat_free(x, sizeof(ikat));
  ikat_done();
}


unsigned char*
ikat_map_bigpage(ikat* x, size_t size){
  assert(size == align_to_next_segment(size));
  unsigned char* p = ikat_mmap(size);
  
  if((unsigned int)p != align_to_next_segment((unsigned int)p)){
    ikat_munmap(p, size);
    p = ikat_mmap(size+segment_size);
    unsigned char* q = (unsigned char*)align_to_next_segment((unsigned int)p);
    if(p == q){
      fprintf(stderr, "retry1\n");
      ikat_munmap(p+size, segment_size);
    } else {
      fprintf(stderr, "retry2\n");
      size_t fst = q - p;
      ikat_munmap(p, fst);
      ikat_munmap(q+size, segment_size-fst);
      p = q;
    }
  } else {
    fprintf(stderr, "noretry\n");
  }
  unsigned int idx = segment_index(p);
  unsigned int idx_hi = idx + size/segment_size;
  while(idx < idx_hi){
    x->alloc_table[idx] = -1;
    idx++;
  }
  return p;
}


void
ikat_unmap(ikat* x, unsigned char* addr, size_t size){
  assert(size == align_to_next_page(size));
  size_t pages = page_index(size);
  if(pages < segment_pages){
    size_t segment = segment_index(addr);
    size_t page_offset = page_index(addr) & (segment_pages-1);
    unsigned int alloc_bits = x->alloc_table[segment];
    unsigned int this_bits = ((1 << pages) - 1) << page_offset;
    unsigned int new_bits = alloc_bits & ~ this_bits;
    fprintf(stderr, "0x%08x bits=0x%08x ^~ 0x%08x = 0x%08x    pages=%d/%d, m=0x%08x\n", 
        addr, alloc_bits, this_bits, new_bits, pages, size, ((1<<pages)-1));
    assert((alloc_bits & this_bits) == this_bits);
    x->alloc_table[segment] = new_bits;
    if(new_bits == 0){
      ikat_munmap((unsigned char*)(segment * segment_size), segment_size);
    }
  } else {
    fprintf(stderr, "ikat_unmap large\n");
    exit(-1);
  }
}


unsigned char*
ikat_map(ikat* x, size_t size, unsigned int type){
  assert(size == align_to_next_page(size));
  assert(type < x->lls_count);
  ikat_ll* llp = x->lls[type];
  size_t pages = page_index(size);
  if(pages < segment_pages){
    ikat_ll* ll = llp;
    ikat_ll** prev = &(x->lls[type]);
    while(ll){
      size_t lsize = ll->size;
      if(lsize == size){
        /* unwire */
        *prev = ll->next;
        ll->next = x->llcache;
        x->llcache = ll;
        return ll->base;
      } else if (lsize > size){
        unsigned char* addr = ll->base;
        ll->size -= size;
        ll->base += size;
        return addr;
      } else {
        prev = &(ll->next);
        ll = ll->next;
      }
    }
    unsigned char* base = ikat_map_bigpage(x, segment_size);
    ikat_ll* cache = x->llcache;
    if(cache){
      x->llcache = cache->next;
    } else {
      cache = ikat_malloc(sizeof(ikat_ll));
    }
    cache->base = base + size;
    cache->size = segment_size - size;
    cache->next = x->lls[type];
    x->lls[type] = cache;
    return base;
  } else {
    size_t aligned_size = align_to_next_segment(size);
    unsigned char* base = ikat_map_bigpage(x, aligned_size);
    if(aligned_size != size){
      ikat_ll* cache = x->llcache;
      if(cache){
        x->llcache = cache->next;
      } else {
        cache = ikat_malloc(sizeof(ikat_ll));
      }
      cache->base = base + size;
      cache->size = aligned_size - size;
      cache->next = x->lls[type];
      x->lls[type] = cache;
    }
    return base;
  }
}


