
#CFLAGS = -Wall -DNDEBUG -O3
CFLAGS = -Wall -g
LDFLAGS = -g -ldl -rdynamic
CC = gcc
all: ikarus

ikarus: ikarus-collect.o ikarus-runtime.o ikarus-main.o ikarus-fasl.o \
  ikarus-exec.o ikarus-print.o ikarus-enter.s ikarus-symbol-table.o  \
  ikarus-weak-pairs.o
	$(CC)  $(LDFLAGS) -o ikarus \
	ikarus-main.o ikarus-runtime.o \
	ikarus-fasl.o ikarus-exec.o ikarus-print.o ikarus-enter.s \
	ikarus-symbol-table.o ikarus-collect.o ikarus-weak-pairs.o

ikarus-main.o: ikarus-main.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-main.c

ikarus-runtime.o: ikarus-runtime.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-runtime.c

ikarus-fasl.o: ikarus-fasl.c ikarus.h
	$(CC) $(CFLAGS) -c ikarus-fasl.c

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

ikarus.h: ikarus-data.h
	touch ikarus.h

clean:
	rm -f *.o ikarus
