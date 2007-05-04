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
 
  (define (read-file file)
    (with-input-from-file file
      (lambda ()
        (let f ()
          (let ([x (read)])
            (if (eof-object? x)
                '()
                (cons x (f))))))))

  (define-record library (code export-subst export-env))
  
  (define export-as-primitive '())

  (define (expand-file filename)
    (map (lambda (x)
           (let-values ([(code export-subst export-env) 
                         (boot-library-expand x)])
             (make-library code export-subst export-env)))
         (read-file filename)))

  (define (inv-assq x ls)
    (cond
      [(null? ls) #f]
      [(eq? x (cdar ls)) (car ls)]
      [else (inv-assq x (cdr ls))]))

  (define (sanitize-export-env subst r)
    (define (add x r)
      (let ([label (car x)] [b (cdr x)])
        (let ([type (car b)] [val (cdr b)])
          (case type
            [(global) 
             (cond
               [(inv-assq label subst) =>
                (lambda (v)
                  (let ([name (car v)])
                    (cond 
                      [(memq name export-as-primitive) 
                       (cons (cons label (cons 'core-prim name)) r)]
                      [else 
                       (cons (cons label (cons 'global val)) r)])))]
               [else (error #f "cannot find binding for ~s" x)])]
            [else (error #f "cannot handle export for ~s" x)]))))
    (let f ([r r])
      (cond
        [(null? r) '()]
        [else (add (car r) (f (cdr r)))])))

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
      (let ([env (sanitize-export-env subst env)])
        (let ([code (build-system-library subst env '())])
          (values 
            (reverse (list* (car code*) code (cdr code*)))
            subst env)))))

  (printf "expanding ...\n")
  
  (let-values ([(core* subst env) (expand-all scheme-library-files)])
    (printf "compiling ...\n")
    (let ([p (open-output-file "ikarus.boot" 'replace)])
      (for-each 
        (lambda (x) (compile-core-expr-to-port x p))
        core*)
      (close-output-port p)))

  (printf "Happy Happy Joy Joy\n"))

(invoke (ikarus makefile))
