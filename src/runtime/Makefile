
all: ikarus

ikarus: ikarus-collect.o ikarus-runtime.o ikarus-main.o ikarus-fasl.o \
  ikarus-exec.o ikarus-print.o ikarus-enter.s ikarus-symbol-table.o  \
  ikarus-hash-tables.o
	gcc -g -Wall -ldl -luuid -rdynamic -o ikarus \
	ikarus-main.o ikarus-runtime.o \
	ikarus-fasl.o ikarus-exec.o ikarus-print.o ikarus-enter.s \
	ikarus-symbol-table.o ikarus-collect.o ikarus-hash-tables.o

ikarus-main.o: ikarus-main.c ikarus.h
	gcc -Wall -g -c ikarus-main.c

ikarus-runtime.o: ikarus-runtime.c ikarus.h
	gcc -Wall -g -c ikarus-runtime.c

ikarus-fasl.o: ikarus-fasl.c ikarus.h
	gcc -Wall -g -c ikarus-fasl.c

ikarus-exec.o: ikarus-exec.c ikarus.h
	gcc -Wall -g -c ikarus-exec.c

ikarus-print.o: ikarus-print.c ikarus.h
	gcc -Wall -g -c ikarus-print.c

ikarus-collect.o: ikarus-collect.c ikarus.h
	gcc -Wall -g -c ikarus-collect.c

ikarus-symbol-table.o: ikarus-symbol-table.c ikarus.h
	gcc -Wall -g -c ikarus-symbol-table.c

ikarus-hash-tables.o: ikarus-hash-tables.c ikarus.h
	gcc -Wall -g -c ikarus-hash-tables.c

ikarus.h: ikarus-data.h
	touch ikarus.h
