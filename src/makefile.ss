#!/usr/bin/env ikarus -b ikarus.boot --script

(library (ikarus makefile)
  (export)
  (import (scheme))
  
  (define scheme-library-files
    ;;; Listed in the order in which they're loaded.
    ;;;
    ;;; Loading of the boot file may segfault if a library is
    ;;; loaded before its dependencies are loaded first.
    ;;;
    ;;; reason is that the base libraries are not a hierarchy of
    ;;; dependencies but rather an eco system in which every
    ;;; part depends on the other.
    ;;;
    ;;; For example, the printer may call error if it finds
    ;;;  an error (e.g. "not an output port"), while the error
    ;;;  procedure may call the printer to display the message.
    ;;;  This works fine as long as error does not itself cause
    ;;;  an error (which may lead to the infamous Error: Error: 
    ;;;  Error: Error: Error: Error: Error: Error: Error: ...).
    ;;;
    '("libhandlers.ss"
      "libcontrol.ss"
      "libcollect.ss"
      "librecord.ss"
      "libcxr.ss"
      "libnumerics.ss"
      "libguardians.ss"
      "libcore.ss"
      "libchezio.ss"
      "libhash.ss"
      "libwriter.ss"
      "libtokenizer.ss"
      "libassembler.ss"
      "libintelasm.ss"
      "libfasl.ss"
      "libtrace.ss"
      "libcompile.ss"
      "libsyntax.ss"
      "libpp.ss"
      "libcafe.ss"
      "libposix.ss"
      "libtimers.ss"
      "libtoplevel.ss"))

  (define (read-file file)
    (with-input-from-file file
      (lambda ()
        (let f ()
          (let ([x (read)])
            (if (eof-object? x)
                '()
                (cons x (f))))))))
  
  (define (expand-library-file ifile)
    (map boot-library-expand (read-file ifile)))

  (define (expand-all ls)
    (apply append (map expand-library-file ls)))

  (printf "expanding ...\n")
  
  (let ([core* (expand-all scheme-library-files)])
    (printf "compiling ...\n")
    (let ([p (open-output-file "ikarus.boot" 'replace)])
      (for-each 
        (lambda (x) (compile-core-expr-to-port x p))
        core*)
      (close-output-port p)))

  (printf "Happy Happy Joy Joy\n"))


;;; ;;; NEW ARCHITECTURE
;;;
;;; (define expander-input-env
;;;   '(;[prim-name      label       (core-prim . prim-name)]
;;;     [car            car-label    (core-prim . car)]))
;;; 
;;; (define expander-output-env 
;;;   '(;[export-name    export-loc]
;;;     [ikarus-car      #{ikarus-car |174V9RJ/FjzvmJVu|}]))
;;; 
;;; (define bootstrap-knot
;;;   '(;[prim-name      export-name]
;;;     [car             ikarus-car]))
;;; 
;;; (define compile-input-env
;;;   '(;[prim-name      export-loc]
;;;     [car             #{ikarus-car |174V9RJ/FjzvmJVu|}]))
