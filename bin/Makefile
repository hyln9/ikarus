
CFLAGS = -I/opt/local/include -Wall -DNDEBUG -O3 
#CFLAGS = -I/opt/local/include -Wall -g
LDFLAGS = -L/opt/local/lib -g -ldl -lgmp -lm
CC = gcc

ifeq ("$(shell uname)", "Linux")
  LDFLAGS += -rdynamic # dlsym(local_symbol) won't work otherwise
endif

objects = ikarus-collect.o ikarus-runtime.o ikarus-main.o ikarus-fasl.o \
  ikarus-exec.o ikarus-print.o ikarus-enter.o ikarus-symbol-table.o  \
  ikarus-weak-pairs.o ikarus-numerics.o ikarus-flonums.o \
  verify-integrity.o winmmap.o

all: ikarus

ikarus: $(objects)
	$(CC) -o ikarus $(objects) $(LDFLAGS)

ikarus-main.o: ikarus-main.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-main.c

ikarus-enter.o: ikarus-enter.s ikarus.h
	$(CC) $(CFLAGS) -c ikarus-enter.s

ikarus-runtime.o: ikarus-runtime.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-runtime.c

ikarus-fasl.o: ikarus-fasl.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-fasl.c

verify-integrity.o: verify-integrity.c ikarus.h
	$(CC) $(CFLAGS) -c verify-integrity.c

ikarus-exec.o: ikarus-exec.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-exec.c

ikarus-print.o: ikarus-print.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-print.c

ikarus-collect.o: ikarus-collect.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-collect.c

ikarus-weak-pairs.o: ikarus-weak-pairs.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-weak-pairs.c

ikarus-symbol-table.o: ikarus-symbol-table.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-symbol-table.c

ikarus-numerics.o: ikarus-numerics.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-numerics.c

ikarus-flonums.o: ikarus-flonums.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-flonums.c

winmmap.o: winmmap.c winmmap.h
	$(CC) $(CFLAGS) -c winmmap.c

ikarus.h: ikarus-data.h
	touch ikarus.h

clean:
	rm -f $(objects)

realclean:
	rm -f $(objects) ikarus
