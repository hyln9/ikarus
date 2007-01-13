#!/usr/bin/env ikarus -b ikarus.boot --script

;;; 9.1: * starting with libnumerics
;;; 9.0: * graph marks for both reader and writer 
;;;      * circularity detection during read 
;;; 8.1: * using chez-style io ports
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
    include parameterize trace untrace trace-lambda trace-define
    time))



(define public-primitives
  '(
    
    null? pair? char? fixnum? symbol? gensym? string? vector? list?
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
    property-list apply map for-each andmap ormap memq memv assq
    assv assoc eq? eqv? equal? reverse string->symbol symbol->string
    top-level-value set-top-level-value!  top-level-bound?
    gensym gensym-count gensym-prefix print-gensym
    gensym->unique-string call-with-values values make-parameter
    dynamic-wind display write print-graph fasl-write printf format
    print-error read-token read comment-handler error exit call/cc
    error-handler eval current-eval compile compile-file
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
    assembler-output $make-environment features
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
    get-output-string with-output-to-file call-with-output-file
    with-input-from-file call-with-input-file date-string
    file-exists? delete-file + - add1 sub1 * expt 
    quotient+remainder quotient remainder number? positive?
    negative? zero? number->string logand = < > <= >=
    make-guardian weak-cons collect 
    interrupt-handler
    time-it 
    posix-fork fork waitpid env environ
    ))

(define system-primitives
  '(

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
    $set-symbol-value!  $make-symbol $set-symbol-plist!
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
    $set-tcbucket-tconc!  $tcbucket-dlink-prev $tcbucket-dlink-next
    $set-tcbucket-dlink-prev!  $set-tcbucket-dlink-next!  call/cf
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
    compiler-giveup-tally
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
             #'(quote ,(date-string))))
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
  (error #f "SEVERELY OUT OF DATE!\n")
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
  (load "libintelasm-6.9.ss") ; uses make-code, etc.
  (load "libfasl-6.7.ss") ; uses code? etc.
  (load "libcompile-8.1.ss") ; uses fasl-write
)


(whack-system-env #t)

(define scheme-library-files
  '(["libhandlers.ss"   "libhandlers.fasl"  p0]
    ["libcontrol.ss"    "libcontrol.fasl"   p0]
    ["libcollect.ss"    "libcollect.fasl"   p0]
    ["librecord.ss"     "librecord.fasl"    p0]
    ["libcxr.ss"        "libcxr.fasl"       p0]
    ["libnumerics.ss"   "libnumerics.fasl"  p0]
    ["libguardians.ss"  "libguardians.fasl" p0]
    ["libcore.ss"       "libcore.fasl"      p0]
    ["libchezio.ss"     "libchezio.fasl"    p0]
    ["libhash.ss"       "libhash.fasl"      p0]
    ["libwriter.ss"     "libwriter.fasl"    p0]
    ["libtokenizer.ss"  "libtokenizer.fasl" p0]
    ["libassembler.ss"  "libassembler.fasl" p0]
    ["libintelasm.ss"   "libintelasm.fasl"  p0]
    ["libfasl.ss"       "libfasl.fasl"      p0]
    ["libcompile.ss"    "libcompile.fasl"   p1]
    ["psyntax-7.1.ss"   "psyntax.fasl"      p0]
    ["libcafe.ss"       "libcafe.fasl"      p0]
    ["libtrace.ss"      "libtrace.fasl"     p0]
    ["libposix.ss"      "libposix.fasl"     p0]
    ["libtimers.ss"     "libtimers.fasl"    p0]
    ["libtoplevel.ss"   "libtoplevel.fasl"  p0]
    ))


(define (read-file ifile)
  (with-input-from-file ifile
    (lambda ()
      (let f ()
        (let ([x (read)])
          (if (eof-object? x)
              '()
              (cons x (f))))))))

(define (expand-file ifile)
  (map sc-expand (read-file ifile)))

(define (compile-library ifile ofile)
  (parameterize ([assembler-output #f] 
                 [expand-mode 'bootstrap]
                 [interaction-environment system-env])
     (printf "compiling ~a ... \n" ifile)
     (compile-file ifile ofile 'replace)))



(let ()
  (define (compile-all who)
    (for-each
      (lambda (x)
        (when (eq? who (caddr x))
          (compile-library (car x) (cadr x))))
      scheme-library-files))
  (define (time x) x)
  (fork
    (lambda (pid) 
      (time (compile-all 'p1))
      (unless (fxzero? (waitpid pid))
        (exit -1)))
    (lambda ()
      (time (compile-all 'p0))
      (exit))))

#;(for-each 
  (lambda (x)
    (when (cadr x)
      (compile-library (car x) (cadr x))))
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
  (format "cat ~a > ikarus.boot"
          (join " " (map cadr scheme-library-files))))

;(#%compiler-giveup-tally)
; vim:syntax=scheme
