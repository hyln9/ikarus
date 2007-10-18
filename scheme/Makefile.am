
nodist_bin_SCRIPTS=ikarus.boot
EXTRA_DIST=ikarus.boot.orig ikarus.apply.ss ikarus.bytevectors.ss ikarus.cafe.ss ikarus.chars.ss ikarus.code-objects.ss ikarus.codecs.ss ikarus.collect.ss ikarus.command-line.ss ikarus.compiler.altcogen.ss ikarus.compiler.ss ikarus.control.ss ikarus.date-string.ss ikarus.fasl.ss ikarus.fasl.write.ss ikarus.fixnums.ss ikarus.guardians.ss ikarus.handlers.ss ikarus.hash-tables.ss ikarus.intel-assembler.ss ikarus.io-ports.ss ikarus.io-primitives.ss ikarus.io-primitives.unsafe.ss ikarus.io.input-files.ss ikarus.io.input-strings.ss ikarus.io.output-files.ss ikarus.io.output-strings.ss ikarus.lists.ss ikarus.load.ss ikarus.main.ss ikarus.multiple-values.ss ikarus.numerics.ss ikarus.pairs.ss ikarus.posix.ss ikarus.predicates.ss ikarus.pretty-print.ss ikarus.promises.ss ikarus.reader.ss ikarus.records.procedural.ss ikarus.singular-objects.ss ikarus.sort.ss ikarus.strings.ss ikarus.structs.ss ikarus.symbols.ss ikarus.timer.ss ikarus.trace.ss ikarus.transcoders.ss ikarus.unicode-data.ss ikarus.vectors.ss ikarus.writer.ss makefile.ss pass-specify-rep-primops.ss pass-specify-rep.ss psyntax.builders.ss psyntax.compat.ss psyntax.config.ss psyntax.expander.ss psyntax.internal.ss psyntax.library-manager.ss r6rs-records.ss ikarus/code-objects.ss ikarus/compiler.ss ikarus/intel-assembler.ss ikarus/fasl/write.ss unicode/unicode-char-cases.ss unicode/unicode-charinfo.ss

all: $(dist_bin_SCRIPTS)

CLEANFILES=$(dist_bin_SCRIPTS)

ikarus.boot: $(ikarus_boot_OBJECTS) $(ikarus_boot_DEPENDENCIES) 
	../src/ikarus -b ./ikarus.boot.orig --r6rs-script makefile.ss

