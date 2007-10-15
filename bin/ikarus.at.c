
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
  assert(malloc_count >= 0);
}
static void ikat_mmap_count(size_t n){
  mmap_count += n;
  fprintf(stderr, "mmap_count=0x%08x\n", mmap_count);
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
  ikat_mmap_count(-n);
  munmap(addr, n);
}


ikat*
ikat_make_allocation_table(int types){
  unsigned int* tbl = ikat_mmap(segment_size);
  bzero(tbl, segment_size);
  ikat* x = ikat_malloc(sizeof(ikat));
  bzero(x, sizeof(ikat));
  x->alloc_table = tbl;
  ikat_meta** meta = ikat_malloc(types * sizeof(ikat_meta));
  bzero(meta, types * sizeof(ikat_meta));
  x->meta_count = types;
  x->meta = meta;
  return x;
}

void 
ikat_free_allocation_table(ikat* x){
  ikat_munmap(x->alloc_table, segment_size);
  ikat_free(x, sizeof(ikat));
  ikat_done();
}



unsigned char*
ikat_map_bigpage(ikat* x, size_t size){
  assert(size == align_to_next_segment(size));
  unsigned char* p = ikat_mmap(size);
  unsigned int idx = segment_index(p);
  unsigned int idx_hi = idx + size/segment_size;
  while(idx < idx_hi){
    x->alloc_table[idx] = -1;
    idx++;
  }
  return p;
}

void
ikat_unmap(ikat* x, unsigned char* addr, size_t size, unsigned int type){
  fprintf(stderr, "ikat_unmap\n");
}

unsigned char*
ikat_map(ikat* x, size_t size, unsigned int type){
  ikat_meta* llp = x->meta[type];
  assert(size == align_to_next_page(size));
  size_t pages = page_index(size);
  if(pages < segment_pages){
    ikat_ll* ll = llp->segments[pages];
    unsigned int pages_mask = (1 << pages) - 1;
    while(ll){
      unsigned char* base = ll->base;
      llp->segments[pages] = ll->next;
      ll->next = x->llcache;
      x->llcache = ll;
      size_t seg_idx = segment_index(base);
      unsigned int alloc_bits = x->alloc_table[seg_idx];
      if(alloc_bits != 0){
        unsigned int page_idx = (page_index(base) & (segment_size-1));
        unsigned int new_bits = pages_mask << page_idx;
        assert((new_bits & alloc_bits) == 0);
        x->alloc_table[seg_idx] = alloc_bits | new_bits;
        return base;
      }
    }
    unsigned char* base = ikat_map_bigpage(x, segment_size);
    size_t seg_idx = segment_index(base);
    x->alloc_table[seg_idx] = pages_mask;
    ikat_ll* cache = x->llcache;
    if(cache){
      x->llcache = cache->next;
    } else {
      cache = ikat_malloc(sizeof(ikat_ll));
    }
    cache->base = base + pages * page_size;
    cache->next = llp->segments[segment_pages - pages];
    llp->segments[segment_pages - pages] = cache;
    return base;
  } else {
    size_t aligned_size = align_to_next_segment(size);
    unsigned char* mem = ikat_mmap(aligned_size);
    unsigned int idx = segment_index(mem);
    unsigned int sz = segment_index(size);
    while(idx < sz){
      x->alloc_table[idx] = -1;
      idx++;
    }
    if(aligned_size != size){
      ikat_unmap(x, mem+size, aligned_size-size, type);
    }
    return mem;
  }
}





