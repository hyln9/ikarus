
#include <stdlib.h>
#ifdef __CYGWIN__
void win_munmap(char* addr, size_t size);

char* win_mmap(size_t size);
#endif
