
all: ikarus.boot

ikarus.boot: *.ss
	echo '(load "makefile.ss")' | ../runtime/ikarus ikarus.boot

clean:
	rm -f *.fasl
