#!/usr/bin/env ikarus -b ikarus.boot --script

(library (ikarus makefile)
  (export)
  (import
    (ikarus system $bootstrap)
    (ikarus))
  
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
    '("ikarus.singular-objects.ss"
      "ikarus.handlers.ss"
      "ikarus.multiple-values.ss"
      "ikarus.control.ss"
      "ikarus.collect.ss"
      "ikarus.apply.ss"
      "ikarus.predicates.ss"
      "ikarus.pairs.ss"
      "ikarus.lists.ss"
      "ikarus.fixnums.ss"
      "ikarus.chars.ss"
      "ikarus.records.ss"
      "ikarus.strings.ss"
      "ikarus.date-string.ss"
      "ikarus.symbols.ss"
      "ikarus.vectors.ss"
      "ikarus.numerics.ss"
      "ikarus.guardians.ss"
      "ikarus.command-line.ss"
      "ikarus.io-ports.ss"
      "ikarus.io-primitives.unsafe.ss"
      "ikarus.io-primitives.ss"
      "ikarus.io.input-files.ss"
      "ikarus.io.output-files.ss"
      "ikarus.io.output-strings.ss"
      "ikarus.hash-tables.ss"
      "ikarus.writer.ss"
      "ikarus.reader.ss"
      "ikarus.code-objects.ss"
      "ikarus.intel-assembler.ss"
      "ikarus.trace.ss"
      "ikarus.fasl.ss"
      "ikarus.compiler.ss"
      "ikarus.library-manager.ss"
      "ikarus.syntax.ss"
      "ikarus.load.ss"
      "ikarus.pretty-print.ss"
      "ikarus.cafe.ss"
      "ikarus.posix.ss"
      "ikarus.timer.ss"
      "ikarus.main.ss"))

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

  (define library-legend
    '([i           (ikarus)]
      [r           (r6rs)]
      [$all        (ikarus system $all)]
      [$pairs      (ikarus system $pairs)]
      [$lists      (ikarus system $lists)]
      [$chars      (ikarus system $chars)]
      [$strings    (ikarus system $strings)]
      [$vectors    (ikarus system $vectors)]
      [$fx         (ikarus system $fx)]
      [$symbols    (ikarus system $symbols)]
      [$records    (ikarus system $records)]
      [$ports      (ikarus system $ports)]
      [$codes      (ikarus system $codes)]
      [$tcbuckets  (ikarus system $tcbuckets)]
      [$io         (ikarus system $io)]
      [$arg-list   (ikarus system $arg-list)]
      [$stack      (ikarus system $stack)]
      [$interrupts (ikarus system $interrupts)]
      [$boot       (ikarus system $bootstrap)]
      ))

  (define ikarus-macros-map
    '([define           i r]
      [define-syntax    i r]
      [module           i  ]
      [begin            i r]
      [set!             i r]
      [foreign-call     i r]
      [quote            i r]
      [syntax-case      i r]
      [syntax           i r]
      [lambda           i r]
      [case-lambda      i r]
      [type-descriptor  i  ]
      [letrec           i r]
      [if               i r]
      [when             i r]
      [unless           i r]
      [parameterize     i  ]
      [case             i r]
      [let-values       i r]
      [define-record    i r]
      [include          i r]
      [syntax-rules     i r]
      [quasiquote       i r]
      [with-syntax      i r]
      [let              i r]
      [let*             i r]
      [cond             i r]
      [and              i r]
      [or               i r]))

  (define ikarus-procedures-map
    '([void                              i]
      [not                               i]
      [boolean?                          i]
      [null?                             i]
      [procedure?                        i]
      [eof-object?                       i]
      [eof-object                        i]
      [eq?                               i]
      [eqv?                              i]
      [equal?                            i]
      [cons                              i]
      [pair?                             i]
      [car                               i]
      [cdr                               i]
      [set-car!                          i]
      [set-cdr!                          i]
      [caar                              i]
      [cdar                              i]
      [cadr                              i]
      [cddr                              i]
      [caaar                             i]
      [cdaar                             i]
      [cadar                             i]
      [cddar                             i]
      [caadr                             i]
      [cdadr                             i]
      [caddr                             i]
      [cdddr                             i]
      [caaaar                            i]
      [cdaaar                            i]
      [cadaar                            i]
      [cddaar                            i]
      [caadar                            i]
      [cdadar                            i]
      [caddar                            i]
      [cdddar                            i]
      [caaadr                            i]
      [cdaadr                            i]
      [cadadr                            i]
      [cddadr                            i]
      [caaddr                            i]
      [cdaddr                            i]
      [cadddr                            i]
      [cddddr                            i]
      [list                              i]
      [list-ref                          i]
      [make-list                         i]
      [list*                             i]
      [list?                             i]
      [append                            i]
      [last-pair                         i]
      [reverse                           i]
      [length                            i]
      [assq                              i]
      [assv                              i]
      [assoc                             i]
      [memq                              i]
      [memv                              i]
      [member                            i]
      [bwp-object?                       i]
      [weak-cons                         i]
      [weak-pair?                        i]
      [char?                             i]
      [char=?                            i]
      [char<?                            i]
      [char>?                            i]
      [char<=?                           i]
      [char>=?                           i]
      [integer->char                     i]
      [char->integer                     i]
      [char-whitespace?                  i]
      [string?                           i]
      [string                            i]
      [make-string                       i]
      [string-ref                        i]
      [string-set!                       i]
      [string-length                     i]
      [string=?                          i]
      [substring                         i]
      [string-append                     i]
      [string->list                      i]
      [list->string                      i]
      [uuid                              i]
      [date-string                       i]
      [vector                            i]
      [make-vector                       i]
      [vector-ref                        i]
      [vector-set!                       i]
      [vector?                           i]
      [vector-length                     i]
      [list->vector                      i]
      [vector->list                      i]
      [for-each                          i]
      [map                               i]
      [andmap                            i]
      [ormap                             i]
      [fixnum?                           i]
      [fx<                               i]
      [fx<=                              i]
      [fx>                               i]
      [fx>=                              i]
      [fx=                               i]
      [fx-                               i]
      [fx+                               i]
      [fx*                               i]
      [fxzero?                           i]
      [fxadd1                            i]
      [fxsub1                            i]
      [fxquotient                        i]
      [fxremainder                       i]
      [fxmodulo                          i]
      [fxsll                             i]
      [fxsra                             i]
      [fxlogand                          i]
      [fxlogxor                          i]
      [fxlogor                           i]
      [fxlognot                          i]
      [fixnum->string                    i]
      [string->flonum                    i]
      [-                                 i]
      [=                                 i]
      [<                                 i]
      [>                                 i]
      [<=                                i]
      [>=                                i]
      [*                                 i]
      [+                                 i]
      [add1                              i]
      [sub1                              i]
      [number?                           i]
      [bignum?                           i]
      [integer?                          i]
      [flonum?                           i]
      [quotient                          i]
      [remainder                         i]
      [quotient+remainder                i]
      [number->string                    i]
      [string->number                    i]
      [flonum->string                    i]
      [symbol?                           i]
      [gensym?                           i]
      [gensym                            i]
      [getprop                           i]
      [putprop                           i]
      [remprop                           i]
      [property-list                     i]
      [string->symbol                    i]
      [symbol->string                    i]
      [gensym->unique-string             i]
      [symbol-bound?                     i]
      [symbol-value                      i]
      [set-symbol-value!                 i]
      [make-guardian                     i]
      [make-input-port                   i]
      [make-output-port                  i]
      [make-input/output-port            i]
      [port-output-index                 i]
      [port-output-size                  i]
      [port-output-buffer                i]
      [set-port-output-index!            i]
      [set-port-output-size!             i]
      [port-input-buffer                 i]
      [port-input-index                  i]
      [port-input-size                   i]
      [set-port-input-index!             i]
      [set-port-input-size!              i]
      [output-port?                      i]
      [input-port?                       i]
      [port?                             i]
      [port-name                         i]
      [input-port-name                   i]
      [output-port-name                  i]
      [open-input-file                   i]
      [with-input-from-file              i]
      [with-output-to-file               i]
      [open-output-file                  i]
      [open-output-string                i]
      [get-output-string                 i]
      [close-input-port                  i]
      [close-output-port                 i]
      [console-input-port                i]
      [console-output-port               i]
      [current-input-port                i]
      [current-output-port               i]
      [standard-input-port               i]
      [standard-output-port              i]
      [standard-error-port               i]
      [flush-output-port                 i]
      [reset-input-port!                 i]
      [display                           i]
      [write                             i]
      [write-char                        i]
      [read                              i]
      [read-char                         i]
      [read-token                        i]
      [peek-char                         i]
      [unread-char                       i]
      [newline                           i]
      [printf                            i]
      [format                            i]
      [pretty-print                      i]
      [comment-handler                   i]
      [print-gensym                      i]
      [gensym-count                      i]
      [gensym-prefix                     i]
      [make-hash-table                   i]
      [hash-table?                       i]
      [get-hash-table                    i]
      [put-hash-table!                   i]
      [make-parameter                    i]
      [apply                             i]
      [values                            i]
      [call-with-values                  i]
      [call/cc                           i]
      [call/cf                           i]
      [dynamic-wind                      i]
      [error                             i]
      [print-error                       i]
      [error-handler                     i]
      [interrupt-handler                 i]
      [exit                              i]
      [load                              i]
      [assembler-output                  i]
      [new-cafe                          i]
      [command-line-arguments            i]
      [record?                           i]
      [make-record-type                  i]
      [record-type-descriptor            i]
      [record-type-field-names           i]
      [record-type-symbol                i]
      [record-type-name                  i]
      [record-name                       i]
      [record-constructor                i]
      [record-predicate                  i]
      [record-length                     i]
      [record-printer                    i]
      [record-ref                        i]
      [record-field-accessor             i]
      [record-field-mutator              i]
      [identifier?                       i]
      [syntax-error                      i]
      [generate-temporaries              i]
      [free-identifier=?                 i]
      [code?                             i]
      [immediate?                        i]
      [pointer-value                     i]

      [current-library-collection        $boot]
      [compile-core-expr-to-port         $boot]
      [current-primitive-locations       $boot]
      [boot-library-expand               $boot]
      ; (ikarus system $pairs)
      [$car               $pairs]
      [$cdr               $pairs]
      [$set-car!          $pairs]
      [$set-cdr!          $pairs]
      ; (ikarus system $lists)
      [$memq              $lists]
      [$memv              $lists]
      ; (ikarus system $chars)
      [$char?             $chars]
      [$char=             $chars]
      [$char<             $chars]
      [$char>             $chars]
      [$char<=            $chars]
      [$char>=            $chars]
      [$char->fixnum      $chars]
      [$fixnum->char      $chars]
      ; (ikarus system $strings)
      [$make-string       $strings]
      [$string-ref        $strings]
      [$string-set!       $strings]
      [$string-length     $strings]
      ; (ikarus system $vectors)
      [$make-vector       $vectors]
      [$vector-length     $vectors]
      [$vector-ref        $vectors]
      [$vector-set!       $vectors]
      ; (ikarus system $fx)
      [$fxzero?           $fx]
      [$fxadd1            $fx]
      [$fxsub1            $fx]
      [$fx>=              $fx]
      [$fx<=              $fx]
      [$fx>               $fx]
      [$fx<               $fx]
      [$fx=               $fx]
      [$fxsll             $fx]
      [$fxsra             $fx]
      [$fxquotient        $fx]
      [$fxmodulo          $fx]
      [$fxlogxor          $fx]
      [$fxlogor           $fx]
      [$fxlognot          $fx]
      [$fxlogand          $fx]
      [$fx+               $fx]
      [$fx*               $fx]
      [$fx-               $fx]
      ; (ikarus system $symbols)
      [$make-symbol               $symbols]
      [$symbol-unique-string      $symbols]
      [$symbol-value              $symbols]
      [$symbol-string             $symbols]
      [$symbol-plist              $symbols]
      [$set-symbol-value!         $symbols]
      [$set-symbol-string!        $symbols]
      [$set-symbol-unique-string! $symbols]
      [$set-symbol-plist!         $symbols]
      [$unbound-object?           $symbols]
      ; (ikarus system $records)
      [base-rtd           $records]
      [$record-set!       $records]
      [$record-ref        $records]
      [$record-rtd        $records]
      [$record            $records]
      [$make-record       $records]
      [$record?           $records]
      [$record/rtd?       $records]
      ; (ikarus system $ports)
      [$make-port/input         $ports]
      [$make-port/output        $ports]
      [$make-port/both          $ports]
      [$port-handler            $ports]
      [$port-input-buffer       $ports]
      [$port-input-index        $ports]
      [$port-input-size         $ports]
      [$port-output-buffer      $ports]
      [$port-output-index       $ports]
      [$port-output-size        $ports]
      [$set-port-input-index!   $ports]
      [$set-port-input-size!    $ports]
      [$set-port-output-index!  $ports]
      [$set-port-output-size!   $ports]
      ; (ikarus system $codes)
      [$closure-code            $codes]
      [$code?                   $codes]
      [$code->closure           $codes]
      [$code-reloc-vector       $codes]
      [$code-freevars           $codes]
      [$code-size               $codes]
      [$code-ref                $codes]
      [$code-set!               $codes]
      ; (ikarus system $tcbuckets)
      [$make-tcbucket           $tcbuckets]
      [$tcbucket-key            $tcbuckets]
      [$tcbucket-val            $tcbuckets]
      [$tcbucket-next           $tcbuckets]
      [$set-tcbucket-val!       $tcbuckets]
      [$set-tcbucket-next!      $tcbuckets]
      [$set-tcbucket-tconc!     $tcbuckets]
      ; (ikarus system $io)
      [$flush-output-port     $io]
      [$reset-input-port!     $io]
      [$close-input-port      $io]
      [$close-output-port     $io]
      [$write-char            $io]
      [$read-char             $io]
      [$peek-char             $io]
      [$unread-char           $io]
      ; (ikarus system $arg-list)
      [$arg-list              $arg-list]
      ; (ikarus system $stack)
      [$$apply                          $stack]
      [$fp-at-base                      $stack]
      [$primitive-call/cc               $stack]
      [$frame->continuation             $stack]
      [$current-frame                   $stack]
      [$seal-frame-and-call             $stack]
      [$make-call-with-values-procedure $stack]
      [$make-values-procedure           $stack]
      ; (ikarus system $interrupts)
      [$interrupted?                     $interrupts]
      [$unset-interrupted!               $interrupts]
      ;;; the following must be defined but they don't have 
      ;;; to reside in any library since they're here so that
      ;;; the compiler can target them.  They're not usable 
      ;;; by the end user.
      [$apply-nonprocedure-error-handler ]
      [$incorrect-args-error-handler     ]
      [$multiple-values-error            ]
      [$debug                            ]
      [$underflow-misaligned-error       ]
      [top-level-value-error             ]
      [car-error                         ]
      [cdr-error                         ]
      [fxadd1-error                      ]
      [fxsub1-error                      ]
      [cadr-error                        ]
      [fx+-type-error                    ]
      [fx+-types-error                   ]
      [fx+-overflow-error                ]
      [$do-event                         ]
      [do-overflow                       ]
      [do-overflow-words                 ]
      [do-vararg-overflow                ]
      [collect                           ]
      [do-stack-overflow                 ]
      [syntax-dispatch                   ]
      ))

  (define (verify-procedures-map)
    (for-each 
      (lambda (x)
        (for-each 
          (lambda (x) 
            (unless (assq x library-legend)
              (error 'verify "~s is not in the libraries list" x)))
          (cdr x)))
      ikarus-procedures-map))

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
            [else 
             ;;; core primitive with no backing definition
             (let ([label (gensym)])
               (export-subst (cons x label))
               (export-env (cons label (cons 'core-prim x))))]))
        (map car ikarus-procedures-map))
      (values (export-subst) (export-env) (export-primlocs))))

  (define (get-export-subset key subst)
    (let f ([ls subst])
      (cond
        [(null? ls) '()]
        [else
         (let ([x (car ls)])
           (let ([name (car x)])
             (cond
               [(or (assq name ikarus-procedures-map)
                    (assq name ikarus-macros-map))
                =>
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
              [invoke-libs '()])
          (let-values ([(subst env)
                        (if (equal? name '(ikarus system $all)) 
                            (values export-subst export-env)
                            (values
                              (get-export-subset key export-subst)
                              '()))])
            `(install-library 
               ',id ',name ',version ',import-libs ',visit-libs ',invoke-libs
               ',subst ',env void void)))))
    (let ([code `(library (ikarus primlocs)
                    (export) ;;; must be empty
                    (import 
                      (only (ikarus library-manager)
                            install-library)
                      (only (ikarus compiler)
                            current-primitive-locations)
                      (ikarus))
                    (current-primitive-locations 
                      (lambda (x) 
                        (cond
                          [(assq x ',primlocs) => cdr]
                          [else #f])))
                    ,@(map build-library library-legend))])
      (let-values ([(code empty-subst empty-env)
                    (boot-library-expand code)])
         code)))

  (define (expand-all files)
    (let ([code* '()]
          [subst '()]
          [env   '()])
      (for-each
        (lambda (file)
          (printf "expanding ~s\n" file)
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

  (verify-procedures-map)

  (printf "expanding ...\n")
  
  (let-values ([(core* locs) (expand-all scheme-library-files)])
    (printf "compiling ...\n")
    (parameterize ([current-primitive-locations
                    (lambda (x)
                      (cond
                        [(assq x locs) => cdr]
                        [else 
                         (error 'bootstrap "no location for ~s" x)]))])
      (let ([p (open-output-file "ikarus.boot" 'replace)])
        (for-each 
          (lambda (x) (compile-core-expr-to-port x p))
          core*)
        (close-output-port p))))

  (printf "Happy Happy Joy Joy\n"))

(invoke (ikarus makefile))

;;; vim:syntax=scheme
