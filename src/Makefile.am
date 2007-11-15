
bin_PROGRAMS = ikarus

ikarus_SOURCES = ikarus-collect.c ikarus-exec.c ikarus-fasl.c ikarus-flonums.c ikarus-main.c ikarus-numerics.c ikarus-print.c ikarus-runtime.c ikarus-symbol-table.c ikarus-verify-integrity.c ikarus-weak-pairs.c ikarus-winmmap.c ikarus-data.h ikarus-winmmap.h ikarus-enter.s
          
nodist_ikarus_SOURCES = bootfileloc.h
BUILT_SOURCES = bootfileloc.h

bootfileloc.h: Makefile
	echo '#define BOOTFILE "$(pkglibdir)/ikarus.boot"' >$@

bin_SCRIPTS = scheme-script

scheme-script: 
	echo "#!/bin/sh" > scheme-script
	echo "exec $(bindir)/ikarus --r6rs-script \$$@" >> scheme-script

CLEANFILES = scheme-script bootfileloc.h

