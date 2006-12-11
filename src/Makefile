
all: ikarus.boot

ikarus.boot: *.ss
	cp ikarus.boot ikarus.boot.back
	../bin/ikarus -b ikarus.boot --script makefile.ss

clean:
	rm -f *.fasl
