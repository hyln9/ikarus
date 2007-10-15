#ifndef IKARUS_AT
#define IKARUS_AT

#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>

#define segment_pages (8 * sizeof(unsigned int))

typedef struct ikat_ll{
  unsigned char* base;
  struct ikat_ll* next;
} ikat_ll;

typedef struct ikat_meta{
  ikat_ll* segments[segment_pages];
} ikat_meta;

typedef struct {
  unsigned int* alloc_table;
  ikat_meta** meta;
  int meta_count;
  ikat_ll* llcache;
} ikat;

ikat* ikat_make_allocation_table();

void ikat_free_allocation_table(ikat*);

unsigned char* ikat_map_bigpage(ikat*, size_t size);

unsigned char* ikat_map(ikat*, size_t size, unsigned int type);

unsigned char* ikat_map_code(ikat*, size_t size, unsigned int type);

void ikat_unmap(ikat*, unsigned char* addr, size_t size, unsigned int type);

#endif

