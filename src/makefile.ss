#!/usr/bin/env ikarus -b ikarus.boot --script

(library (ikarus makefile)
  (export)
  (import (ikarus))
  
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
      "ikarus.void.ss"
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
      "ikarus.trace.ss"
      "ikarus.intel-assembler.ss"
      "ikarus.fasl.ss"
      "ikarus.compiler.ss"
      "ikarus.library-manager.ss"
      "ikarus.syntax.ss"
      "ikarus.pretty-print.ss"
      "ikarus.cafe.ss"
      "ikarus.posix.ss"
      "ikarus.timer.ss"
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

  (define library-legend
    '([$all (ikarus system $all)]
      [s   (ikarus system)]
      [u   (ikarus system unsafe)]
      [i   (ikarus)]
      [r   (r6rs)]
      [$pairs (ikarus system $pairs)]
      [$lists (ikarus system $lists)]
      [$chars (ikarus system $chars)]
      [$strings (ikarus system $strings)]
      [$vectors (ikarus system $vectors)]
      [$fx (ikarus system $fx)]
      [$symbols (ikarus system $symbols)]
      [$records (ikarus system $records)]
      [$ports (ikarus system $ports)]
      [$codes (ikarus system $codes)]
      [$tcbuckets (ikarus system $tcbuckets)]
      [$io (ikarus system $io)]
      [$arg-list (ikarus system $arg-list)]
      [$stack (ikarus system $stack)]
      ;[$lists (ikarus system $lists)]
      ;[$lists (ikarus system $lists)]
      ))

  (define ikarus-macros-map
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
      [or               s i r]))

  (define ikarus-procedures-map
    '([void                             s i]
      [not                              s i]
      [boolean?                         s i]
      [null?                            s i]
      [procedure?                       s i]
      [eof-object?                      s i]
      [eof-object                       s i]
      [eq?                              s i]
      [eqv?                             s i]
      [equal?                           s i]
      [cons                             s i]
      [pair?                            s i]
      [car                              s i]
      [cdr                              s i]
      [set-car!                         s i]
      [set-cdr!                         s i]
      [caar                             s i]
      [cdar                             s i]
      [cadr                             s i]
      [cddr                             s i]
      [caaar                            s i]
      [cdaar                            s i]
      [cadar                            s i]
      [cddar                            s i]
      [caadr                            s i]
      [cdadr                            s i]
      [caddr                            s i]
      [cdddr                            s i]
      [caaaar                           s i]
      [cdaaar                           s i]
      [cadaar                           s i]
      [cddaar                           s i]
      [caadar                           s i]
      [cdadar                           s i]
      [caddar                           s i]
      [cdddar                           s i]
      [caaadr                           s i]
      [cdaadr                           s i]
      [cadadr                           s i]
      [cddadr                           s i]
      [caaddr                           s i]
      [cdaddr                           s i]
      [cadddr                           s i]
      [cddddr                           s i]
      [list                             s i]
      [list-ref                         s i]
      [make-list                        s i]
      [list*                            s i]
      [list?                            s i]
      [append                           s i]
      [last-pair                        s i]
      [reverse                          s i]
      [length                           s i]
      [assq                             s i]
      [assv                             s i]
      [assoc                            s i]
      [memq                             s i]
      [memv                             s i]
      [member                           s i]
      [bwp-object?                      s i]
      [weak-cons                        s i]
      [weak-pair?                       s i]
      [char?                            s i]
      [char=?                           s i]
      [char<?                           s i]
      [char>?                           s i]
      [char<=?                          s i]
      [char>=?                          s i]
      [integer->char                    s i]
      [char->integer                    s i]
      [char-whitespace?                 s i]
      [string?                          s i]
      [string                           s i]
      [make-string                      s i]
      [string-ref                       s i]
      [string-set!                      s i]
      [string-length                    s i]
      [string=?                         s i]
      [substring                        s i]
      [string-append                    s i]
      [string->list                     s i]
      [list->string                     s i]
      [uuid                             s i]
      [date-string                      s i]
      [vector                           s i]
      [make-vector                      s i]
      [vector-ref                       s i]
      [vector-set!                      s i]
      [vector?                          s i]
      [vector-length                    s i]
      [list->vector                     s i]
      [vector->list                     s i]
      [for-each                         s i]
      [map                              s i]
      [andmap                           s i]
      [ormap                            s i]
      [fixnum?                          s i]
      [fx<                              s i]
      [fx<=                             s i]
      [fx>                              s i]
      [fx>=                             s i]
      [fx=                              s i]
      [fx-                              s i]
      [fx+                              s i]
      [fx*                              s i]
      [fxzero?                          s i]
      [fxadd1                           s i]
      [fxsub1                           s i]
      [fxquotient                       s i]
      [fxremainder                      s i]
      [fxmodulo                         s i]
      [fxsll                            s i]
      [fxsra                            s i]
      [fxlogand                         s i]
      [fxlogxor                         s i]
      [fxlogor                          s i]
      [fxlognot                         s i]
      [fixnum->string                   s i]
      [string->flonum                   s i]
      [-                                s i]
      [=                                s i]
      [<                                s i]
      [>                                s i]
      [<=                               s i]
      [>=                               s i]
      [*                                s i]
      [+                                s i]
      [add1                             s i]
      [sub1                             s i]
      [number?                          s i]
      [bignum?                          s i]
      [integer?                         s i]
      [flonum?                          s i]
      [quotient                         s i]
      [remainder                        s i]
      [quotient+remainder               s i]
      [number->string                   s i]
      [string->number                   s i]
      [flonum->string                   s i]
      [symbol?                          s i]
      [gensym?                          s i]
      [gensym                           s i]
      [getprop                          s i]
      [putprop                          s i]
      [remprop                          s i]
      [property-list                    s i]
      [string->symbol                   s i]
      [symbol->string                   s i]
      [gensym->unique-string            s i]
      [symbol-bound?                    s i]
      [symbol-value                     s i]
      [set-symbol-value!                s i]
      [top-level-bound?                 s  ]
      [top-level-value                  s  ]
      [set-top-level-value!             s  ]
      [make-guardian                    s i]
      [make-input-port                  s i]
      [make-output-port                 s i]
      [make-input/output-port           s i]
      [port-output-index                s i]
      [port-output-size                 s i]
      [port-output-buffer               s i]
      [set-port-output-index!           s i]
      [set-port-output-size!            s i]
      [port-input-buffer                s i]
      [port-input-index                 s i]
      [port-input-size                  s i]
      [set-port-input-index!            s i]
      [set-port-input-size!             s i]
      [*standard-input-port*            s i]
      [*standard-output-port*           s i]
      [*standard-error-port*            s i]
      [*current-input-port*             s i]
      [*current-output-port*            s i]
      [output-port?                     s i]
      [input-port?                      s i]
      [port?                            s i]
      [port-name                        s i]
      [input-port-name                  s i]
      [output-port-name                 s i]
      [open-input-file                  s i]
      [with-input-from-file             s i]
      [with-output-to-file              s i]
      [open-output-file                 s i]
      [open-output-string               s i]
      [get-output-string                s i]
      [close-input-port                 s i]
      [close-output-port                s i]
      [console-input-port               s i]
      [console-output-port              s i]
      [current-input-port               s i]
      [current-output-port              s i]
      [standard-input-port              s i]
      [standard-output-port             s i]
      [standard-error-port              s i]
      [flush-output-port                s i]
      [reset-input-port!                s i]
      [display                          s i]
      [write                            s i]
      [write-char                       s i]
      [read                             s i]
      [read-char                        s i]
      [read-token                       s i]
      [peek-char                        s i]
      [unread-char                      s i]
      [newline                          s i]
      [printf                           s i]
      [format                           s i]
      [pretty-print                     s i]
      [comment-handler                  s i]
      [print-gensym                     s i]
      [gensym-count                     s i]
      [gensym-prefix                    s i]
      [make-hash-table                  s i]
      [hash-table?                      s i]
      [get-hash-table                   s i]
      [put-hash-table!                  s i]
      [make-parameter                   s i]
      [apply                            s i]
      [values                           s i]
      [call-with-values                 s i]
      [call/cc                          s i]
      [call/cf                          s i]
      [dynamic-wind                     s i]
      [error                            s i]
      [print-error                      s i]
      [error-handler                    s i]
      [interrupt-handler                s i]
      [exit                             s i]
      [compile-core-expr-to-port        s i]
      [eval-core                        s i]
      [load                             s i]
      [assembler-output                 s i]
      ;[fasl-write                       s i]
      [new-cafe                         s i]
      [command-line-arguments           s i]
      [list*->code*                     s i]
      [eval-top-level                   s i]
      [current-primitive-locations      s i]
      [record?                          s i]
      [make-record-type                 s i]
      [record-type-descriptor           s i]
      [record-type-field-names          s i]
      [record-type-symbol               s i]
      [record-type-name                 s i]
      [record-name                      s i]
      [record-constructor               s i]
      [record-predicate                 s i]
      [record-length                    s i]
      [record-printer                   s i]
      [record-ref                       s i]
      [record-field-accessor            s i]
      [record-field-mutator             s i]
      [identifier?                      s i]
      [syntax-error                     s i]
      [generate-temporaries             s i]
      [free-identifier=?                s i]
      [boot-library-expand              s i]
      [code?                            s i]
      [make-code                        s i]
      [code-reloc-vector                s i]
      [set-code-reloc-vector!           s i]
      [code-size                        s i]
      [code-freevars                    s i]
      [code-ref                         s i]
      [code-set!                        s i]
      [immediate?                       s i]
      [pointer-value                    s i]
      [installed-libraries              s i]
      [library-subst/env                s i]
      [find-library-by-name             s i]
      [imported-label->binding          s i]
      [imported-loc->library            s i]
      [library-spec                     s i]
      [current-library-collection       s i]
      [invoke-library                   s i]
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
      ; (ikarus system)
      [$forward-ptr?                    s  ]
      [$interrupted?                    s  ]
      [$unset-interrupted!              s  ]
      [$apply-nonprocedure-error-handler s]
      [$incorrect-args-error-handler     s]
      [$multiple-values-error            s]
      [$debug                            s]
      [$underflow-misaligned-error       s]
      [top-level-value-error             s]
      [car-error                         s]
      [cdr-error                         s]
      [fxadd1-error                      s]
      [fxsub1-error                      s]
      [cadr-error                        s]
      [fx+-type-error                    s]
      [fx+-types-error                   s]
      [fx+-overflow-error                s]
      [$do-event                         s]
      [do-overflow            s]
      [do-overflow-words      s]
      [do-vararg-overflow     s]
      [collect                s]
      [do-stack-overflow      s]
      [syntax-dispatch        s]
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
                      (ikarus))
                    (current-primitive-locations 
                      (lambda (x) 
                        (cond
                          [(assq x ',primlocs) => cdr]
                          [else #f])))
                    ,@(map build-library library-legend))])
      ;(parameterize ([print-gensym #f])
      ;  (pretty-print code))
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
          ;    (pretty-print x)
              (let-values ([(code export-subst export-env)
                            (boot-library-expand x)])
                 (set! code* (cons code code*))
                 (set! subst (append export-subst subst))
                 (set! env (append export-env env))))))
        files)
      (printf "building system ...\n")
      (let-values ([(export-subst export-env export-locs)
                    (make-system-data subst env)])
        ;(printf "export-subst=~s\n" export-locs)
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
                        [else #f]))])
      (let ([p (open-output-file "ikarus.boot" 'replace)])
        (for-each 
          (lambda (x) (compile-core-expr-to-port x p))
          core*)
        (close-output-port p))))

  (printf "Happy Happy Joy Joy\n"))

(invoke (ikarus makefile))

;;; vim:syntax=scheme
