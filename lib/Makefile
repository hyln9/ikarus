
all: ikarus.boot

ikarus.boot: *.ss
	cp ikarus.boot ikarus.boot.back
	echo '(load "makefile.ss")' | time ../bin/ikarus -b ikarus.boot

clean:
	rm -f *.fasl
