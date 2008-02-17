
nodist_pkglib_DATA=ikarus.boot

EXTRA_DIST=ikarus.boot.prebuilt ikarus.enumerations.ss \
  ikarus.exceptions.ss ikarus.apply.ss ikarus.bytevectors.ss \
  ikarus.cafe.ss ikarus.chars.ss ikarus.code-objects.ss \
  ikarus.codecs.ss ikarus.collect.ss ikarus.command-line.ss \
  ikarus.compiler.altcogen.ss ikarus.compiler.ss \
  ikarus.compiler.source-optimizer.ss ikarus.control.ss \
  ikarus.date-string.ss ikarus.fasl.ss ikarus.fasl.write.ss \
  ikarus.fixnums.ss ikarus.guardians.ss ikarus.handlers.ss \
  ikarus.hash-tables.ss ikarus.intel-assembler.ss \
  ikarus.lists.ss ikarus.load.ss ikarus.main.ss \
  ikarus.multiple-values.ss ikarus.numerics.ss \
  ikarus.pairs.ss ikarus.posix.ss ikarus.predicates.ss \
  ikarus.pretty-print.ss ikarus.promises.ss ikarus.reader.ss \
  ikarus.records.procedural.ss ikarus.conditions.ss \
  ikarus.singular-objects.ss ikarus.sort.ss ikarus.strings.ss \
  ikarus.structs.ss ikarus.symbols.ss ikarus.timer.ss ikarus.trace.ss \
  ikarus.unicode-conversion.ss ikarus.unicode-data.ss \
  ikarus.vectors.ss ikarus.writer.ss makefile.ss \
  pass-specify-rep-primops.ss pass-specify-rep.ss psyntax.builders.ss \
  psyntax.compat.ss psyntax.config.ss psyntax.expander.ss \
  psyntax.internal.ss psyntax.library-manager.ss \
  unicode/unicode-char-cases.ss unicode/unicode-charinfo.ss \
  ikarus.io.ss ikarus.time-and-date.ss \
  ikarus.separate-compilation.ss
 
all: $(nodist_pkglib_DATA)

revno = "$(shell sed 's/ .*//' ../.bzr/branch/last-revision 2>/dev/null)"
last-revision: $(shell ls ../.bzr/branch/last-revision 2>/dev/null) 
	echo $(revno) >$@

ikarus.config.ss: Makefile last-revision ../config.h
	echo '(define ikarus-version "$(PACKAGE_VERSION)")' >$@
	echo '(define ikarus-revision "$(shell cat last-revision)")' >>$@
	echo '(define ikarus-lib-dir "$(pkglibdir)")' >>$@
	echo '(define wordsize $(shell grep SIZEOF_VOID_P ../config.h | sed "s/.*\(.\)/\1/g"))' >>$@


CLEANFILES=$(nodist_pkglib_DATA) ikarus.config.ss
MAINTAINERCLEANFILES=last-revision

ikarus.boot: $(EXTRA_DIST) ikarus.config.ss
	../src/ikarus -b ./ikarus.boot.prebuilt --r6rs-script makefile.ss

