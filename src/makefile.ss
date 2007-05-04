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

  (define ikarus-environment-map
    '([define            (define)]
      [define-syntax     (define-syntax)]
      [module            (module)]
      [begin             (begin)]
      [set!              (set!)]
      [foreign-call      (core-macro . foreign-call)]
      [quote             (core-macro . quote)]
      [syntax-case       (core-macro . syntax-case)]
      [syntax            (core-macro . syntax)]
      [lambda            (core-macro . lambda)]
      [case-lambda       (core-macro . case-lambda)]
      [type-descriptor   (core-macro . type-descriptor)]
      [letrec            (core-macro . letrec)]
      [if                (core-macro . if)]
      [when              (core-macro . when)]         
      [unless            (core-macro . unless)]
      [parameterize      (core-macro . parameterize)]
      [case              (core-macro . case)]
      [let-values        (core-macro . let-values)]
      [define-record     (macro . define-record)]
      [include           (macro . include)]
      [syntax-rules      (macro . syntax-rules)]
      [quasiquote        (macro . quasiquote)]
      [with-syntax       (macro . with-syntax)]
      [let               (macro . let)]
      [let*              (macro . let*)]
      [cond              (macro . cond)]
      [and               (macro . and)]
      [or                (macro . or)]))

  (define (make-system-data subst env)
    (define (add x s r l)
      (let ([name (car x)] [binding (cadr x)])
        (case (car binding)
          [(core-prim) 
           (error 'make-system-subst/env "cannot handle ~s" x)]
          [else 
           (let ([label (gensym)])
             (values (cons (cons name label) s)
                     (cons (cons label binding) r)
                     l))])))
    (let f ([ls ikarus-environment-map])
      (cond
        [(null? ls) (values '() '() '())]
        [else
         (let-values ([(subst env primlocs) (f (cdr ls))])
           (add (car ls) subst env primlocs))])))

  (define (build-system-library export-subst export-env primlocs)
    (let-values ([(code empty-subst empty-env)
                  (boot-library-expand
                    `(library (ikarus primlocs)
                       (export) ;;; must be empty
                       (import (scheme))
                       (current-primitive-locations 
                         (lambda (x) 
                           (cond
                             [(assq x ',primlocs) => cdr]
                             [else #f])))
                       (install-library 
                          ',(gensym "system")   ;;; id
                          '(system)             ;;; name
                          '()                   ;;; version
                          '()                   ;;; import libs 
                          '()                   ;;; visit libs
                          '()                   ;;; invoke libs
                          ',export-subst        ;;; substitution
                          ',export-env          ;;; environment
                          void void             ;;; visit/invoke codes
                          )))])
       (pretty-print code)
       code))

;  (define (env->primlocs env)
;    (let f ([ls env])
;      (cond
;        [(null? ls) '()]
;        [else
;         (let ([x (car ls)])
;           (let ([label (car x)] [binding (cdr x)])
;             (let ([type (car binding)] [value (cdr binding)])
;               (case type
;                 [(global) 

  (define (expand-all files)
    (let ([code* '()]
          [subst '()]
          [env   '()])
      (for-each
        (lambda (file)
          (load file
            (lambda (x) 
              (let-values ([(code export-subst export-env)
                            (boot-library-expand x)])
                 (set! code* (cons code code*))
                 (set! subst (append export-subst subst))
                 (set! env (append export-env env))))))
        files)
      (let-values ([(export-subst export-env export-locs)
                    (make-system-data subst env)])
        (let ([code (build-system-library export-subst export-env export-locs)])
          (values 
            (reverse (list* (car code*) code (cdr code*)))
            export-locs)))))

  (printf "expanding ...\n")
  
  (let-values ([(core* locs) (expand-all scheme-library-files)])
    (printf "compiling ...\n")
    (parameterize ([current-primitive-locations
                    (lambda (x)
                      (cond
                        [(assq x locs) => cdr]
                        [else #f]))])
      (let ([p (open-output-file "ikarus.boot" 'replace)])
        (for-each 
          (lambda (x) (compile-core-expr-to-port x p))
          core*)
        (close-output-port p))))

  (printf "Happy Happy Joy Joy\n"))

(invoke (ikarus makefile))
