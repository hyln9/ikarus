#!/usr/bin/env ikarus -b ikarus.boot --r6rs-script


(import (except (ikarus) assembler-output)
        (ikarus compiler)
        (except (ikarus system $bootstrap)
                eval-core
                current-primitive-locations
                compile-core-expr-to-port))


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
    "ikarus.io.input-strings.ss"
    "ikarus.io.output-strings.ss"
    "ikarus.hash-tables.ss"
    "ikarus.writer.ss"
    "ikarus.reader.ss"
    "ikarus.code-objects.ss"
    "ikarus.intel-assembler.ss"
    "ikarus.trace.ss"
    "ikarus.fasl.write.ss"
    "ikarus.fasl.ss"
    "ikarus.compiler.ss"
    "ikarus.library-manager.ss"
    "ikarus.syntax.ss"
    "ikarus.load.ss"
    "ikarus.pretty-print.ss"
    "ikarus.cafe.ss"
    "ikarus.posix.ss"
    "ikarus.timer.ss"
    "ikarus.bytevectors.ss"
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
    [letrec*           (core-macro . letrec*)]
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
    [do                (macro . do)]
    [and               (macro . and)]
    [or                (macro . or)]))

(define library-legend
  '([i           (ikarus)                     #t]
    [symbols     (ikarus symbols)             #t]
    [parameters  (ikarus parameters)          #t]
    [interaction (ikarus interaction)         #t]
    [r           (r6rs)                       #t]
    [syncase     (r6rs syntax-case)           #t]
    [cm          (chez modules)               #t]
    [$all        (ikarus system $all)         #f]
    [$pairs      (ikarus system $pairs)       #f]
    [$lists      (ikarus system $lists)       #f]
    [$chars      (ikarus system $chars)       #f]
    [$strings    (ikarus system $strings)     #f]
    [$vectors    (ikarus system $vectors)     #f]
    [$bytes      (ikarus system $bytevectors) #f]
    [$fx         (ikarus system $fx)          #f]
    [$symbols    (ikarus system $symbols)     #f]
    [$records    (ikarus system $records)     #f]
    [$ports      (ikarus system $ports)       #f]
    [$codes      (ikarus system $codes)       #f]
    [$tcbuckets  (ikarus system $tcbuckets)   #f]
    [$io         (ikarus system $io)          #f]
    [$arg-list   (ikarus system $arg-list)    #f]
    [$stack      (ikarus system $stack)       #f]
    [$interrupts (ikarus system $interrupts)  #f]
    [$boot       (ikarus system $bootstrap)   #f]
    ))


(define bootstrap-collection
  (let ([ls (map 
              (lambda (x) 
                (find-library-by-name (cadr x)))
              library-legend)])
    (case-lambda
      [() ls]
      [(x) (unless (memq x ls) 
             (set! ls (cons x ls)))])))



(define ikarus-macros-map
  '([define           i r]
    [define-syntax    i r]
    [module           i cm]
    [begin            i r]
    [set!             i r]
    [foreign-call     i]
    [quote            i r]
    [syntax-case      i syncase]
    [syntax           i syncase]
    [lambda           i r]
    [case-lambda      i r]
    [type-descriptor  i  ]
    [letrec           i r]
    [letrec*          i r]
    [if               i r]
    [when             i r]
    [unless           i r]
    [parameterize     i parameters]
    [case             i r]
    [let-values       i r]
    [define-record    i r]
    [include          i r]
    [syntax-rules     i r]
    [quasiquote       i r]
    [with-syntax      i syncase]
    [let              i r]
    [let*             i r]
    [cond             i r]
    [do               i r]
    [and              i r]
    [or               i r]))

(define ikarus-procedures-map
  '([void                    i]
    [not                     i r]
    [boolean?                i r]
    [null?                   i r]
    [procedure?              i r]
    [eof-object?             i r]
    [eof-object              i]
    [eq?                     i r]
    [eqv?                    i r]
    [equal?                  i r]
    [cons                    i r]
    [pair?                   i r]
    [car                     i r]
    [cdr                     i r]
    [set-car!                i]
    [set-cdr!                i]
    [caar                    i r]
    [cdar                    i r]
    [cadr                    i r]
    [cddr                    i r]
    [caaar                   i r]
    [cdaar                   i r]
    [cadar                   i r]
    [cddar                   i r]
    [caadr                   i r]
    [cdadr                   i r]
    [caddr                   i r]
    [cdddr                   i r]
    [caaaar                  i r]
    [cdaaar                  i r]
    [cadaar                  i r]
    [cddaar                  i r]
    [caadar                  i r]
    [cdadar                  i r]
    [caddar                  i r]
    [cdddar                  i r]
    [caaadr                  i r]
    [cdaadr                  i r]
    [cadadr                  i r]
    [cddadr                  i r]
    [caaddr                  i r]
    [cdaddr                  i r]
    [cadddr                  i r]
    [cddddr                  i r]
    [list                    i r]
    [list-ref                i r]
    [make-list               i r]
    [list*                   i]
    [list?                   i r]
    [append                  i r]
    [last-pair               i r]
    [reverse                 i r]
    [length                  i r]
    [assq                    i r]
    [assv                    i r]
    [assoc                   i r]
    [memq                    i r]
    [memv                    i r]
    [member                  i r]
    [bwp-object?             i]
    [weak-cons               i]
    [weak-pair?              i]
    [char?                   i r]
    [char=?                  i r]
    [char<?                  i r]
    [char>?                  i r]
    [char<=?                 i r]
    [char>=?                 i r]
    [integer->char           i r]
    [char->integer           i r]
    [char-whitespace?        i r]
    [string?                 i r]
    [string                  i r]
    [make-string             i r]
    [string-ref              i r]
    [string-set!             i r]
    [string-length           i r]
    [string=?                i r]
    [substring               i r]
    [string-append           i r]
    [string->list            i r]
    [list->string            i r]
    [uuid                    i]
    [date-string             i]
    [vector                  i r]
    [make-vector             i r]
    [vector-ref              i r]
    [vector-set!             i r]
    [vector?                 i r]
    [vector-length           i r]
    [list->vector            i r]
    [vector->list            i r]
    [make-bytevector         i]
    [bytevector-length       i]
    [bytevector-s8-ref       i]
    [bytevector-u8-ref       i]
    [bytevector-s8-set!      i]
    [bytevector-u8-set!      i]

    [for-each                i r]
    [map                     i r]
    [andmap                  i]
    [ormap                   i]
    [fixnum?                 i]
    [fx<                     i]
    [fx<=                    i]
    [fx>                     i]
    [fx>=                    i]
    [fx=                     i]
    [fx-                     i]
    [fx+                     i]
    [fx*                     i]
    [fxzero?                 i]
    [fxadd1                  i]
    [fxsub1                  i]
    [fxquotient              i]
    [fxremainder             i]
    [fxmodulo                i]
    [fxsll                   i]
    [fxsra                   i]
    [fxlogand                i]
    [fxlogxor                i]
    [fxlogor                 i]
    [fxlognot                i]
    [fixnum->string          i]
    [string->flonum          i]
    [-                       i r]
    [=                       i r]
    [<                       i r]
    [>                       i r]
    [<=                      i r]
    [>=                      i r]
    [zero?                   i r]
    [*                       i r]
    [+                       i r]
    [add1                    i]
    [sub1                    i]
    [number?                 i r]
    [bignum?                 i]
    [integer?                i]
    [flonum?                 i]
    [positive?               i r]
    [quotient                i r]
    [remainder               i r]
    [quotient+remainder      i r]
    [number->string          i r]
    [string->number          i r]
    [flonum->string          i]
    [symbol?                 i r symbols]
    [gensym?                 i symbols]
    [gensym                  i symbols]
    [getprop                 i symbols]
    [putprop                 i symbols]
    [remprop                 i symbols]
    [property-list           i symbols]
    [string->symbol          i r symbols]
    [symbol->string          i r symbols]
    [gensym->unique-string   i symbols]
    [symbol-bound?           i symbols]
    [symbol-value            i symbols]
    [set-symbol-value!       i symbols]
    [make-guardian           i]
    [make-input-port         i]
    [make-output-port        i]
    [make-input/output-port  i]
    [port-output-index       i]
    [port-output-size        i]
    [port-output-buffer      i]
    [set-port-output-index!  i]
    [set-port-output-size!   i]
    [port-input-buffer       i]
    [port-input-index        i]
    [port-input-size         i]
    [set-port-input-index!   i]
    [set-port-input-size!    i]
    [output-port?            i r]
    [input-port?             i r]
    [port?                   i r]
    [port-name               i]
    [input-port-name         i]
    [output-port-name        i]
    [open-input-file         i r]
    [with-input-from-file    i r]
    [with-output-to-file     i r]
    [open-output-file        i r]
    [open-output-string      i]
    [open-input-string       i]
    [get-output-string       i]
    [with-output-to-string   i]
    [close-input-port        i r]
    [close-output-port       i r]
    [console-input-port      i]
    [console-output-port     i]
    [current-input-port      i]
    [current-output-port     i]
    [standard-input-port     i]
    [standard-output-port    i]
    [standard-error-port     i]
    [flush-output-port       i]
    [reset-input-port!       i]
    [file-exists?            i]
    [display                 i r]
    [write                   i r]
    [write-char              i]
    [read                    i r]
    [read-char               i r]
    [read-token              i]
    [peek-char               i]
    [unread-char             i]
    [newline                 i r]
    [printf                  i]
    [format                  i]
    [pretty-print            i]
    [comment-handler         i]
    [print-gensym            i symbols]
    [gensym-count            i symbols]
    [gensym-prefix           i symbols]
    [make-hash-table         i]
    [hash-table?             i]
    [get-hash-table          i]
    [put-hash-table!         i]
    [make-parameter          i parameters]
    [apply                   i r]
    [values                  i r]
    [call-with-values        i r]
    [call/cc                 i r]
    [call-with-current-continuation i r]
    [call/cf                 i]
    [dynamic-wind            i r]
    [error                   i]
    [print-error             i]
    [error-handler           i]
    [interrupt-handler       i]
    [exit                    i]
    [load                    i]
    [assembler-output        i]
    [new-cafe                i]
    [eval                    i]
    [environment             i]
    [environment?            i]
    [time-it                 i]
    [command-line-arguments  i]
    [record?                 i]
    [make-record-type        i]
    [record-type-descriptor  i]
    [record-type-field-names i]
    [record-type-symbol      i]
    [record-type-name        i]
    [set-rtd-printer!        i]
    [record-name             i]
    [record-constructor      i]
    [record-predicate        i]
    [record-length           i]
    [record-printer          i]
    [record-ref              i]
    [record-set!             i]
    [record-field-accessor   i]
    [record-field-mutator    i]
    [identifier?             i syncase]
    [syntax-error            i syncase]
    [generate-temporaries    i syncase]
    [free-identifier=?       i syncase]
    [bound-identifier=?      i syncase]
    [syntax->datum           i syncase]
    [datum->syntax           i syncase]
    [code?                   i]
    [immediate?              i]
    [pointer-value           i]

    [installed-libraries     i]
    [compile-core-expr-to-port   $boot]
    [current-primitive-locations $boot]
    [boot-library-expand         $boot]
    [eval-core                   $boot]
    [current-library-collection  $boot]
    [library-name                $boot]
    [find-library-by-name        $boot]

    [$car               $pairs]
    [$cdr               $pairs]
    [$set-car!          $pairs]
    [$set-cdr!          $pairs]
    
    [$memq              $lists]
    [$memv              $lists]
   
    [$char?             $chars]
    [$char=             $chars]
    [$char<             $chars]
    [$char>             $chars]
    [$char<=            $chars]
    [$char>=            $chars]
    [$char->fixnum      $chars]
    [$fixnum->char      $chars]
   
    [$make-string       $strings]
    [$string-ref        $strings]
    [$string-set!       $strings]
    [$string-length     $strings]
  
    [bytevector?        i]

    [$make-bytevector   $bytes]
    [$bytevector-length $bytes]
    [$bytevector-s8-ref $bytes]
    [$bytevector-u8-ref $bytes]
    [$bytevector-set!   $bytes]


    [$make-vector       $vectors]
    [$vector-length     $vectors]
    [$vector-ref        $vectors]
    [$vector-set!       $vectors]
 
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

    [base-rtd           $records]
    [$record-set!       $records]
    [$record-ref        $records]
    [$record-rtd        $records]
    [$record            $records]
    [$make-record       $records]
    [$record?           $records]
    [$record/rtd?       $records]

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

    [$closure-code            $codes]
    [$code->closure           $codes]
    [$code-reloc-vector       $codes]
    [$code-freevars           $codes]
    [$code-size               $codes]
    [$code-ref                $codes]
    [$code-set!               $codes]

    [$make-tcbucket           $tcbuckets]
    [$tcbucket-key            $tcbuckets]
    [$tcbucket-val            $tcbuckets]
    [$tcbucket-next           $tcbuckets]
    [$set-tcbucket-val!       $tcbuckets]
    [$set-tcbucket-next!      $tcbuckets]
    [$set-tcbucket-tconc!     $tcbuckets]

    [$flush-output-port     $io]
    [$reset-input-port!     $io]
    [$close-input-port      $io]
    [$close-output-port     $io]
    [$write-char            $io]
    [$read-char             $io]
    [$peek-char             $io]
    [$unread-char           $io]

    [$arg-list              $arg-list]

    [$$apply                          $stack]
    [$fp-at-base                      $stack]
    [$primitive-call/cc               $stack]
    [$frame->continuation             $stack]
    [$current-frame                   $stack]
    [$seal-frame-and-call             $stack]
    [$make-call-with-values-procedure $stack]
    [$make-values-procedure           $stack]

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

(define (verify-map)
  (define (f x)
    (for-each 
      (lambda (x) 
        (unless (assq x library-legend)
          (error 'verify "~s is not in the libraries list" x)))
      (cdr x)))
  (for-each f ikarus-procedures-map)
  (for-each f ikarus-macros-map))

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
           ;;; primitive defined (exported) within the compiled libraries
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
           ;;; core primitive with no backing definition, assumed to
           ;;; be defined in other strata of the system
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
    (let ([key (car legend-entry)]
          [name (cadr legend-entry)]
          [visible? (caddr legend-entry)]) 
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
             ',subst ',env void void ',visible?)))))
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
    (let-values ([(export-subst export-env export-locs)
                  (make-system-data subst env)])
      (let ([code (build-system-library export-subst export-env export-locs)])
        (values 
          (reverse (list* (car code*) code (cdr code*)))
          export-locs)))))

(verify-map)

(time-it "the entire bootstrap process"
  (lambda ()
    (let-values ([(core* locs) 
                  (time-it "macro expansion"
                    (lambda () 
                      (parameterize ([current-library-collection
                                       bootstrap-collection])
                        (expand-all scheme-library-files))))])
        (current-primitive-locations
          (lambda (x)
            (cond
              [(assq x locs) => cdr]
              [else 
               (error 'bootstrap "no location for ~s" x)])))
        (let ([p (open-output-file "ikarus.boot" 'replace)])
          (for-each 
            (lambda (x) (compile-core-expr-to-port x p))
            core*)
          (close-output-port p)))))

(printf "Happy Happy Joy Joy\n")


;;; vim:syntax=scheme
