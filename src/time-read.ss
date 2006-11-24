
(define scheme-library-files
  '(["libhandlers-6.9.ss"    "libhandlers.fasl"]
    ["libcontrol-6.1.ss"     "libcontrol.fasl"]
    ["libcollect-6.1.ss"     "libcollect.fasl"]
    ["librecord-6.4.ss"      "librecord.fasl"]
    ["libcxr-6.0.ss"         "libcxr.fasl"]
    ["libcore-6.9.ss"        "libcore.fasl"]
    ["libio-6.9.ss"          "libio.fasl"]
    ["libwriter-6.2.ss"      "libwriter.fasl"]
    ["libtokenizer-6.1.ss"   "libtokenizer.fasl"]
    ["libassembler-6.7.ss"   "libassembler.ss"]
    ["libintelasm-6.9.ss"    "libintelasm.fasl"]
    ["libfasl-6.7.ss"        "libfasl.fasl"]
    ["libcompile-6.7.ss"     "libcompile.fasl"]
    ["psyntax-7.1-6.9.ss"    "psyntax.fasl"]
    ["libinterpret-6.5.ss"   "libinterpret.fasl"]
    ["libcafe-6.1.ss"        "libcafe.fasl"]
    ["libtrace-6.9.ss"       "libtrace.fasl"]
    ["libposix-6.0.ss"       "libposix.fasl"]
    ["libhash-6.2.ss"        "libhash.fasl"]
    ["libtoplevel-6.9.ss"    "libtoplevel.fasl"]
    ))

(define (read-file x)
  (with-input-from-file x
    (lambda ()
      (let f ()
        (unless (eof-object? (read-char)) (f))))))

(define (read-all)
  (for-each
    (lambda (x)
      (read-file (car x)))
    scheme-library-files))

(define (do-times n f)
  (unless (fxzero? n)
    (f)
    (do-times (fx- n 1) f)))

(do-times 10 read-all)

