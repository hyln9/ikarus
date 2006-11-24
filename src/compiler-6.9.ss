

;;; 
;;; 6.9: * creating a *system* environment
;;; 6.8: * creating a core-primitive form in the expander
;;; 6.2: * side-effects now modify the dirty-vector
;;;      * added bwp-object?
;;;      * added pointer-value
;;;      * added tcbuckets
;;; 6.1: * added case-lambda, dropped lambda
;;; 6.0: * basic compiler




(define macros
  '(|#primitive| lambda case-lambda set! quote begin define if letrec
    foreign-call $apply
    quasiquote unquote unquote-splicing
    define-syntax identifier-syntax let-syntax letrec-syntax
    fluid-let-syntax alias meta eval-when with-implicit with-syntax
    type-descriptor
    syntax-case syntax-rules module $module import $import import-only
    syntax quasisyntax unsyntax unsyntax-splicing datum
    let let* let-values cond case define-record or and when unless do
    include parameterize trace untrace trace-lambda))



(define public-primitives
  '(null? pair? char? fixnum? symbol? gensym? string? vector? list?
    boolean? procedure? 
    not
    eof-object eof-object? bwp-object?
    void
    fx= fx< fx<= fx> fx>= fxzero?
    fx+ fx- fx* fxadd1 fxsub1 fxquotient fxremainder fxmodulo 
    fxsll fxsra fxlognot fxlogor fxlogand fxlogxor
    integer->char char->integer
    char=? char<? char<=? char>? char>=?
    cons car cdr set-car! set-cdr!
    caar cadr cdar cddr 
    caaar caadr cadar caddr cdaar cdadr cddar cdddr 
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr 
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr 
    list list* make-list length list-ref
    append
    make-vector vector-ref vector-set! vector-length vector
    vector->list list->vector
    make-string string-ref string-set! string-length string list->string
    uuid
    string-append substring 
    string=? string<? string<=? string>? string>=?
    remprop putprop getprop property-list
    apply
    map for-each andmap ormap
    memq memv assq 
    eq? equal?
    reverse
    string->symbol symbol->string oblist 
    top-level-value set-top-level-value! top-level-bound?
    gensym gensym-count gensym-prefix print-gensym
    gensym->unique-string
    call-with-values values
    make-parameter dynamic-wind
    output-port? current-output-port standard-output-port console-output-port
    open-output-file close-output-port flush-output-port output-port-name
    with-output-to-file with-input-from-file
    input-port? current-input-port standard-input-port console-input-port
    reset-input-port!
    open-input-file close-input-port input-port-name
    standard-error-port
    open-output-string get-output-string 
    newline write-char peek-char read-char unread-char
    display write fasl-write printf format print-error
    read-token read
    error exit call/cc
    current-error-handler
    eval current-eval interpret compile compile-file new-cafe load
    system
    expand sc-expand current-expand expand-mode 
    environment? interaction-environment
    identifier? free-identifier=? bound-identifier=? literal-identifier=?
    datum->syntax-object syntax-object->datum syntax-error
    syntax->list
    generate-temporaries
    record? record-set! record-ref record-length
    record-type-descriptor make-record-type
    record-printer record-name record-field-accessor
    record-field-mutator record-predicate record-constructor
    record-type-name record-type-symbol record-type-field-names 
    hash-table? make-hash-table get-hash-table put-hash-table!
    assembler-output
    $make-environment
    features
    ))

(define system-primitives
  '(immediate? $unbound-object? $forward-ptr?
    pointer-value 
    primitive-ref primitive-set!
    $fx= $fx< $fx<= $fx> $fx>= $fxzero?
    $fx+ $fx- $fx* $fxadd1 $fxsub1 $fxquotient $fxremainder $fxmodulo 
    $fxsll $fxsra $fxlognot $fxlogor $fxlogand $fxlogxor
    $fixnum->char $char->fixnum
    $char= $char< $char<= $char> $char>=
    $car $cdr $set-car! $set-cdr!
    $make-vector $vector-ref $vector-set! $vector-length
    $make-string $string-ref $string-set! $string-length $string
    $symbol-string $symbol-unique-string $symbol-value
    $set-symbol-string! $set-symbol-unique-string! $set-symbol-value!
    $make-symbol $set-symbol-plist!  $symbol-plist
    $sc-put-cte 
    $record? $record/rtd? $record-set! $record-ref $record-rtd
    $make-record $record
    $base-rtd
    $code? $code-reloc-vector $code-freevars $code-size $code-ref $code-set!
    $code->closure list*->code* 
    make-code code? set-code-reloc-vector! code-reloc-vector code-freevars
    code-size code-ref code-set!
    $frame->continuation $fp-at-base $current-frame $seal-frame-and-call
    $make-call-with-values-procedure $make-values-procedure
    do-overflow collect
    $make-tcbucket $tcbucket-next $tcbucket-key $tcbucket-val
    $set-tcbucket-next! $set-tcbucket-val! $set-tcbucket-tconc!
    call/cf trace-symbol! untrace-symbol! make-traced-procedure
    fixnum->string date-string
    vector-memq vector-memv
    ))
 


(define (whack-system-env setenv?)
  (define add-prim
    (lambda (x)
      (let ([g (gensym (symbol->string x))])
        (putprop x '|#system| g)
        (putprop g '*sc-expander* (cons 'core-primitive x)))))
  (define add-macro
    (lambda (x)
      (let ([g (gensym (symbol->string x))]
            [e (getprop x '*sc-expander*)])
        (when e 
          (putprop x '|#system| g)
          (putprop g '*sc-expander* e)))))
  (define (foo)
    (eval 
      `(begin
         (define-syntax compile-time-date-string
           (lambda (x)
             #'(quote ,(#%date-string))))
         (define-syntax public-primitives
           (lambda (x)
             #'(quote ,public-primitives)))
         (define-syntax system-primitives
           (lambda (x)
             #'(quote ,system-primitives))) 
         (define-syntax macros
           (lambda (x)
             #'(quote ,macros))))))
  (set! system-env ($make-environment '|#system| #t))
  (for-each add-macro macros)
  (for-each add-prim public-primitives)
  (for-each add-prim system-primitives)
  (if setenv?
      (parameterize ([interaction-environment system-env])
        (foo))
      (foo)))



(when (eq? "" "")
  (load "chez-compat.ss")
  (set! primitive-ref top-level-value)
  (set! primitive-set! set-top-level-value!)
  (set! chez-expand sc-expand)
  (set! chez-current-expand current-expand)
  (printf "loading psyntax.pp ...\n")
  (load "psyntax-7.1.pp")
  (chez-current-expand
    (lambda (x . args)
      (apply chez-expand (sc-expand x) args)))
  (whack-system-env #f)
  (printf "loading psyntax.ss ...\n")
  (load "psyntax-7.1-6.9.ss")
  (chez-current-expand
    (lambda (x . args)
      (apply chez-expand (sc-expand x) args)))
  (whack-system-env #t)
  (printf "ok\n")
  (load "libassembler-compat-6.7.ss") ; defines make-code etc.
  (load "libintelasm-6.6.ss") ; uses make-code, etc.
  (load "libfasl-6.7.ss") ; uses code? etc.
  (load "libcompile-6.7.ss") ; uses fasl-write
)


(whack-system-env #t)

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



(define (compile-library ifile ofile)
  (parameterize ([assembler-output #f] 
                 [expand-mode 'bootstrap]
                 [interaction-environment system-env])
     (printf "compiling ~a ...\n" ifile)
     (compile-file ifile ofile 'replace)))

(for-each 
  (lambda (x)
    (compile-library (car x) (cadr x)))
  scheme-library-files)


(define (join s ls)
  (cond
    [(null? ls) ""]
    [else
     (let ([str (open-output-string)])
       (let f ([a (car ls)] [d (cdr ls)])
         (cond
           [(null? d)
            (display a str)
            (get-output-string str)]
           [else
            (display a str)
            (display s str)
            (f (car d) (cdr d))])))]))
   

(system
  (format "cat ~a > ikarus.fasl"
          (join " " (map cadr scheme-library-files))))
