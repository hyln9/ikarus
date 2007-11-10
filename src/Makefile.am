
bin_PROGRAMS = ikarus scheme-script
ikarus_SOURCES = ikarus-collect.c ikarus-exec.c ikarus-fasl.c ikarus-flonums.c ikarus-main.c ikarus-numerics.c ikarus-print.c ikarus-runtime.c ikarus-symbol-table.c ikarus-verify-integrity.c ikarus-weak-pairs.c ikarus-winmmap.c ikarus-data.h ikarus-winmmap.h ikarus-enter.s
ikarus_CFLAGS=-DBOOTFILE="\"$(pkgdatadir)/ikarus.boot\""

scheme-script: 
	echo "#!/bin/sh" > scheme-script
	echo "$(bindir)/ikarus --r6rs-script \$$@" >> scheme-script
