
all:
	./gen.pl	> tmp.s
	gcc	-o tmp.o -c tmp.s
	otool	-t tmp.o 
	otool -tv tmp.o

