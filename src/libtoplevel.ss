

(library (flush me top-level-and-module-init)
  (export)
  (import (scheme))

;;; this junk should all go away soon
;;; this file is one big hack that initializes the whole system.

(define (macros)
  '(|#primitive| lambda case-lambda set! quote begin define if letrec
    foreign-call ;$apply
    quasiquote unquote unquote-splicing
    define-syntax identifier-syntax let-syntax letrec-syntax
    fluid-let-syntax alias meta eval-when with-implicit with-syntax
    type-descriptor
    syntax-case syntax-rules module $module import $import import-only
    syntax quasisyntax unsyntax unsyntax-splicing datum
    let let* let-values cond case define-record or and when unless do
    include parameterize trace untrace trace-lambda trace-define
    rec library
    time))

(define (public-primitives)
  '(
    null? pair? char? fixnum? bignum? symbol? gensym? string? vector? list?
    boolean? procedure?  not eof-object eof-object? bwp-object?
    void fx= fx< fx<= fx> fx>= fxzero?  fx+ fx- fx* fxadd1 fxsub1
    fxquotient fxremainder fxmodulo fxsll fxsra fxlognot fxlogor
    fxlogand fxlogxor integer->char char->integer char=? char<?
    char<=? char>? char>=?  cons car cdr set-car! set-cdr!  caar
    cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
    cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr list list*
    make-list length list-ref append make-vector vector-ref
    vector-set! vector-length vector vector->list list->vector
    make-string string-ref string-set! string-length string
    string->list list->string uuid string-append substring string=?
    string<?  string<=? string>? string>=? remprop putprop getprop
    property-list $$apply apply map for-each andmap ormap memq memv assq
    assv assoc eq? eqv? equal? reverse string->symbol symbol->string
    top-level-value set-top-level-value!  top-level-bound?
    gensym gensym-count gensym-prefix print-gensym
    gensym->unique-string call-with-values values make-parameter
    dynamic-wind display write print-graph fasl-write printf fprintf format
    print-error read-token read comment-handler error warning exit call/cc
    error-handler eval current-eval compile alt-compile compile-file
    alt-compile-file
    new-cafe load system expand sc-expand current-expand expand-mode
    environment? interaction-environment identifier?
    free-identifier=? bound-identifier=? literal-identifier=?
    datum->syntax-object syntax-object->datum syntax-error
    syntax->list generate-temporaries record? record-set! record-ref
    record-length record-type-descriptor make-record-type
    record-printer record-name record-field-accessor
    record-field-mutator record-predicate record-constructor
    record-type-name record-type-symbol record-type-field-names
    hash-table? make-hash-table get-hash-table put-hash-table!
    assembler-output $make-environment 
    command-line-arguments port? input-port? output-port?
    make-input-port make-output-port make-input/output-port
    port-handler port-input-buffer port-input-index port-input-size
    port-output-buffer port-output-index port-output-size
    set-port-input-index! set-port-input-size!
    set-port-output-index! set-port-output-size!  port-name
    input-port-name output-port-name write-char read-char
    unread-char peek-char newline reset-input-port!
    flush-output-port close-input-port close-output-port
    console-input-port current-input-port standard-output-port
    standard-error-port console-output-port current-output-port
    open-output-file open-input-file open-output-string
    with-output-to-string
    get-output-string with-output-to-file call-with-output-file
    open-input-string
    with-input-from-file call-with-input-file date-string
    file-exists? delete-file + - add1 sub1 * / expt 
    quotient+remainder quotient remainder modulo number? positive?
    negative? zero? number->string logand = < > <= >=
    last-pair
    make-guardian weak-cons collect 
    interrupt-handler
    time-it 
    posix-fork fork waitpid env environ
    pretty-print
    even? odd? member char-whitespace? char-alphabetic?
    char-downcase max min complex? real? rational? 
    exact? inexact? integer?
    string->number exact->inexact
    flonum? flonum->string string->flonum
    sin cos atan sqrt
    ))

(define (system-primitives)
  '(
    $primitive-call/cc
    $closure-code immediate? $unbound-object? $forward-ptr?
    pointer-value primitive-ref primitive-set!  $fx= $fx< $fx<= $fx>
    $fx>= $fxzero?  $fx+ $fx- $fx* $fxadd1 $fxsub1 $fxquotient
    $fxremainder $fxmodulo $fxsll $fxsra $fxlognot $fxlogor
    $fxlogand $fxlogxor $fixnum->char $char->fixnum $char= $char<
    $char<= $char> $char>= $car $cdr $set-car! $set-cdr!
    $make-vector $vector-ref $vector-set! $vector-length
    $make-string $string-ref $string-set!  $string-length $string
    $symbol-string $symbol-unique-string $symbol-value
    $set-symbol-string! $set-symbol-unique-string!
    $set-symbol-value! $set-symbol-function! $make-symbol $set-symbol-plist!
    $symbol-plist $sc-put-cte $record? $record/rtd? $record-set!
    $record-ref $record-rtd $make-record $record $base-rtd $code?
    $code-reloc-vector $code-freevars $code-size $code-ref
    $code-set!  $code->closure list*->code* make-code code?
    set-code-reloc-vector!  code-reloc-vector code-freevars
    code-size code-ref code-set!  $frame->continuation $fp-at-base
    $current-frame $arg-list $seal-frame-and-call
    $make-call-with-values-procedure $make-values-procedure
    do-overflow $make-tcbucket $tcbucket-next $tcbucket-key
    $tcbucket-val $set-tcbucket-next!  $set-tcbucket-val!
    $set-tcbucket-tconc!  
    call/cf
    trace-symbol! untrace-symbol!  make-traced-procedure
    fixnum->string 
    $interrupted? $unset-interrupted! $do-event
    $fasl-read
    ;;; TODO: must open-code
    $make-port/input $make-port/output $make-port/both
    $make-input-port $make-output-port $make-input/output-port
    $port-handler $port-input-buffer $port-input-index
    $port-input-size $port-output-buffer $port-output-index
    $port-output-size $set-port-input-index! $set-port-input-size!
    $set-port-output-index! $set-port-output-size!
    ;;; better open-code
    $write-char $read-char $peek-char $unread-char
    ;;; never open-code 
    $reset-input-port! $close-input-port $close-output-port
    $flush-output-port *standard-output-port* *standard-error-port*
    *current-output-port* *standard-input-port* *current-input-port*
    ;;; 
    compile-core-expr-to-port
    compiler-giveup-tally
    ))

;;; first, it defines all public primitives to their primref values.
;;;       (cross your fingers they're all defined in code)
(for-each
  (lambda (x)
    ($set-symbol-value! x (primitive-ref x)))
  (public-primitives))

;;; second, it hacks a |#system| module by defining all system and
;;; public primitives to be (core-primitive . name) syntaxes.
(let ()
  (define add-prim 
    (lambda (x)
      (let ([g (gensym (symbol->string x))])
        (putprop x '|#system| g)
        (putprop g '*sc-expander* (cons 'core-primitive x)))))
  (for-each add-prim (public-primitives))
  (for-each add-prim (system-primitives)))

;;; third, all macros that are defined in the compiler |#system| are
;;;  added to the top-level, and those defined in the top-level are
;;;  added to the |#system|.
(for-each
  (lambda (x)
    (cond
      [(getprop x '*sc-expander*) =>
       (lambda (p)
         (let ([g (gensym (symbol->string x))])
           (putprop x '|#system| g)
           (putprop g '*sc-expander* p)))]
      [(getprop x '|#system|) =>
       (lambda (g)
         (let ([p (getprop g '*sc-expander*)])
           (putprop x '*sc-expander* p)))]
      [else (error #f "~s is not a macro" x)]))
  (macros))

;;; Now we hack the read #system and scheme modules by forging
;;; interfaces and putting property lists.
(let ([gsys (gensym "#system")] [gsch (gensym "*scheme*")])
  (define (make-stx x)
    (vector 'syntax-object x 
            (list '(top) 
                  (vector 'ribcage 
                          (vector x)
                          (vector '(top))
                          (vector (getprop x '|#system|))))))
  (define (make-module stx* name)
    (cons '$module (vector 'interface '(top) (list->vector stx*) name)))
  (putprop '|#system| '|#system| gsys)
  (putprop 'scheme  '|#system| gsch)
  (putprop 'scheme '*scheme* gsch)
  (let* ([schls (append '(scheme) (public-primitives) (macros))]
         [sysls (append '(|#system|) (system-primitives) schls)])
    (let ([sysmod (make-module (map make-stx sysls) '|#system|)]
          [schmod (make-module (map make-stx schls) '*scheme*)])
      (for-each 
        (lambda (x)
          (putprop x '*scheme* (getprop x '|#system|)))
        schls)
      (putprop gsch '*sc-expander* schmod)
      (putprop gsys '*sc-expander* sysmod)
      (putprop '|#system| '*sc-expander* sysmod)
      (putprop 'scheme '*sc-expander* schmod))))
)

;;; Finally, we're ready to evaluate the files and enter the cafe.
(library (ikarus interaction)
  (export)
  (import (scheme))
  (let-values ([(files script args)
                (let f ([args (command-line-arguments)])
                  (cond
                    [(null? args) (values '() #f '())]
                    [(string=? (car args) "--")
                     (values '() #f (cdr args))]
                    [(string=? (car args) "--script")
                     (let ([d (cdr args)])
                       (cond
                         [(null? d) 
                          (error #f "--script requires a script name")]
                         [else
                          (values '() (car d) (cdr d))]))]
                    [else
                     (let-values ([(f* script a*) (f (cdr args))])
                       (values (cons (car args) f*) script a*))]))])
    (current-eval compile)
    (cond
      [script ; no greeting, no cafe
       (command-line-arguments (cons script args))
       (for-each load files)
       (load script)
       (exit 0)]
      [else
       (printf "Ikarus Scheme (Build ~a)\n" "NO TIME STRING")
       ;(printf "Ikarus Scheme (Build ~a)\n" (compile-time-date-string))
       (display "Copyright (c) 2006-2007 Abdulaziz Ghuloum\n\n")
       (command-line-arguments args)
       (for-each load files)
       (new-cafe)
       (exit 0)])))

