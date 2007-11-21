
nodist_pkglib_DATA=ikarus.boot
EXTRA_DIST=ikarus.boot.orig ikarus.enumerations.ss ikarus.exceptions.ss ikarus.apply.ss ikarus.bytevectors.ss ikarus.cafe.ss ikarus.chars.ss ikarus.code-objects.ss ikarus.codecs.ss ikarus.collect.ss ikarus.command-line.ss ikarus.compiler.altcogen.ss ikarus.compiler.ss ikarus.control.ss ikarus.date-string.ss ikarus.fasl.ss ikarus.fasl.write.ss ikarus.fixnums.ss ikarus.guardians.ss ikarus.handlers.ss ikarus.hash-tables.ss ikarus.intel-assembler.ss ikarus.io-ports.ss ikarus.io-primitives.ss ikarus.io-primitives.unsafe.ss ikarus.io.input-files.ss ikarus.io.input-strings.ss ikarus.io.output-files.ss ikarus.io.output-strings.ss ikarus.lists.ss ikarus.load.ss ikarus.main.ss ikarus.multiple-values.ss ikarus.numerics.ss ikarus.pairs.ss ikarus.posix.ss ikarus.predicates.ss ikarus.pretty-print.ss ikarus.promises.ss ikarus.reader.ss ikarus.records.procedural.ss ikarus.conditions.ss  ikarus.singular-objects.ss ikarus.sort.ss ikarus.strings.ss ikarus.structs.ss ikarus.symbols.ss ikarus.timer.ss ikarus.trace.ss ikarus.transcoders.ss ikarus.unicode-data.ss ikarus.vectors.ss ikarus.writer.ss makefile.ss pass-specify-rep-primops.ss pass-specify-rep.ss psyntax.builders.ss psyntax.compat.ss psyntax.config.ss psyntax.expander.ss psyntax.internal.ss psyntax.library-manager.ss r6rs-records.ss ikarus/code-objects.ss ikarus/compiler.ss ikarus/intel-assembler.ss ikarus/fasl/write.ss unicode/unicode-char-cases.ss unicode/unicode-charinfo.ss

all: $(nodist_pkglib_DATA)

revno = "$(shell sed 's/ .*//' ../.bzr/branch/last-revision)"
last-revision: $(shell ls ../.bzr/branch/last-revision) 
	echo $(revno) >$@

ikarus.config.ss: Makefile last-revision
	echo '(define ikarus-version "$(PACKAGE_VERSION)")' >$@
	echo '(define ikarus-revision "$(shell cat last-revision)")' >>$@
	echo '(define ikarus-lib-dir "$(pkglibdir)")' >>$@

CLEANFILES=$(nodist_pkglib_DATA) ikarus.config.ss last-revision

ikarus.boot: $(EXTRA_DIST) ikarus.config.ss
	../src/ikarus -b ./ikarus.boot.orig --r6rs-script makefile.ss

