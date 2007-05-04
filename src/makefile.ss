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

  (define ikarus-system-macros
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

  (define ikarus-system-primitives
    '(print-greeting))

  (define library-legend
    '([s   (ikarus system)]
      [i   (ikarus)]
      [r   (r6rs)]))

  (define ikarus-library-map
    '([define           s i r]
      [define-syntax    s i r]
      [module           s i  ]
      [begin            s i r]
      [set!             s i r]
      [foreign-call     s i r]
      [quote            s i r]
      [syntax-case      s i r]
      [syntax           s i r]
      [lambda           s i r]
      [case-lambda      s i r]
      [type-descriptor  s i  ]
      [letrec           s i r]
      [if               s i r]
      [when             s i r]
      [unless           s i r]
      [parameterize     s i  ]
      [case             s i r]
      [let-values       s i r]
      [define-record    s i r]
      [include          s i r]
      [syntax-rules     s i r]
      [quasiquote       s i r]
      [with-syntax      s i r]
      [let              s i r]
      [let*             s i r]
      [cond             s i r]
      [and              s i r]
      [or               s i r]
      [print-greeting   s    ]
      ))

  (define (make-collection)
    (let ([set '()])
      (case-lambda
        [() set]
        [(x) (set! set (cons x set))])))

  (define (make-system-data subst env)
    (define who 'make-system-data)
    (let ([export-subst    (make-collection)] 
          [export-env      (make-collection)]
          [export-primlocs (make-collection)])
      (for-each
        (lambda (x)
          (let ([name (car x)] [binding (cadr x)])
            (let ([label (gensym)])
              (export-subst (cons name label))
              (export-env   (cons label binding)))))
        ikarus-system-macros)
      (for-each
        (lambda (x)
          (cond
            [(assq x (export-subst))
             (error who "ambiguous export of ~s" x)]
            [(assq x subst) =>
             (lambda (p)
               (let ([label (cdr p)])
                 (cond
                   [(assq label env) =>
                    (lambda (p)
                      (let ([binding (cdr p)])
                        (case (car binding)
                          [(global) 
                           (export-subst (cons x label))
                           (export-env   (cons label (cons 'core-prim x)))
                           (export-primlocs (cons x (cdr binding)))]
                          [else 
                           (error #f "invalid binding ~s for ~s" p x)])))]
                   [else (error #f "cannot find binding for ~s" x)])))]
            [else (error #f "cannot find export for ~s" x)]))
        ikarus-system-primitives)
      (values (export-subst) (export-env) (export-primlocs))))

  (define (get-export-subset key subst)
    (let f ([ls subst])
      (cond
        [(null? ls) '()]
        [else
         (let ([x (car ls)])
           (let ([name (car x)])
             (cond
               [(assq name ikarus-library-map) =>
                (lambda (q)
                  (cond
                    [(memq key (cdr q)) 
                     (cons x (f (cdr ls)))]
                    [else (f (cdr ls))]))]
               [else 
                ;;; not going to any library?
                (f (cdr ls))])))])))

  (define (build-system-library export-subst export-env primlocs)
    (define (build-library legend-entry)
      (let ([key (car legend-entry)] [name (cadr legend-entry)]) 
        (let ([id     (gensym)]
              [name       name]
              [version     '()]
              [import-libs '()]
              [visit-libs  '()]
              [invoke-libs '()]
              [subst (get-export-subset key export-subst)]
              [env (if (equal? name '(ikarus system)) export-env '())])
          `(install-library 
             ',id ',name ',version ',import-libs ',visit-libs ',invoke-libs
             ',subst ',env void void))))
    (let ([code `(library (ikarus primlocs)
                    (export) ;;; must be empty
                    (import (scheme))
                    (current-primitive-locations 
                      (lambda (x) 
                        (cond
                          [(assq x ',primlocs) => cdr]
                          [else #f])))
                    ,@(map build-library library-legend))])
      (parameterize ([print-gensym #f])
        (pretty-print code))
      (let-values ([(code empty-subst empty-env)
                    (boot-library-expand code)])
         code)))

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
      (printf "building system ...\n")
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
