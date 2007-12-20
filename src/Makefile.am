
bin_PROGRAMS = ikarus scheme-script

ikarus_SOURCES = ikarus-collect.c ikarus-exec.c ikarus-fasl.c \
  ikarus-flonums.c ikarus-main.c ikarus-numerics.c ikarus-print.c \
  ikarus-runtime.c ikarus-symbol-table.c ikarus-verify-integrity.c \
  ikarus-weak-pairs.c ikarus-winmmap.c ikarus-data.h \
  ikarus-winmmap.h ikarus-enter.s cpu_has_sse2.s ikarus-io.c \
  ikarus-process.c

scheme_script_SOURCES = scheme-script.c
          
nodist_ikarus_SOURCES = bootfileloc.h
BUILT_SOURCES = bootfileloc.h

bootfileloc.h: Makefile
	echo '#define BOOTFILE "$(pkglibdir)/ikarus.boot"' >$@
	echo '#define EXEFILE "$(bindir)/ikarus"' >>$@

CLEANFILES = bootfileloc.h
