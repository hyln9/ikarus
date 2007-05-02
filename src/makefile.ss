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
      "library-manager.ss"
      "libtoplevel.ss"))

  (define (read-file file)
    (with-input-from-file file
      (lambda ()
        (let f ()
          (let ([x (read)])
            (if (eof-object? x)
                '()
                (cons x (f))))))))

  (define-record library (code env))
  

  (define (expand-file filename)
    (map (lambda (x)
           (let-values ([(code env) 
                         (boot-library-expand x)])
             (make-library code env)))
         (read-file filename)))

  (define (make-primloc-library env)
    `(library (ikarus primlocs)
       (export)
       (import (scheme))
       (primitive-set! 'primitive-location
         (make-parameter
           (lambda (x)
             (cond
               [(assq x ',env) =>
                (lambda (x)
                  (let ([type (caddr x)] [loc (cadddr x)])
                    (case type
                      [(global) (cons type loc)]
                      [else #f])))]
               [else #f]))
           (lambda (x)
             (if (procedure? x) 
                 x 
                 (error 'primitive-location 
                   "~s is not a procedure" x)))))))

  (define (expand-all ls)
    (define (insert x ls)
      (cond ;;; insert before last library
        [(null? (cdr ls))
         (list x (library-code (car ls)))]
        [else
         (cons (library-code (car ls)) 
           (insert x (cdr ls)))]))
    (let ([libs (apply append (map expand-file ls))])
      (let ([env (apply append (map library-env libs))])
        (let-values ([(code _)
                      (boot-library-expand 
                        (make-primloc-library env))])
          (printf "ENV=~s\n" env)
          (values (insert code libs) env)))))

  (printf "expanding ...\n")
  
  (let-values ([(core* env) (expand-all scheme-library-files)])
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
