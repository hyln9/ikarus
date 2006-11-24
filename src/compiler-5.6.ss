


(print-gensym #f)

(define inline-primitives (make-parameter #f))

(define signal-error-on-undefined-pcb (make-parameter #t))

(load "record-case.ss")
(load "set-operations.ss")
(load "tests-driver.ss")
;(load "tests-5.6-req.scm")
;(load "tests-5.3-req.scm")
;(load "tests-5.2-req.scm")
;(load "tests-5.1-req.scm")
;(load "tests-4.3-req.scm")
;(load "tests-4.2-req.scm")
;(load "tests-4.1-req.scm")
;(load "tests-3.4-req.scm")
;(load "tests-3.3-req.scm")
;(load "tests-3.2-req.scm")
;(load "tests-3.1-req.scm")
;(load "tests-2.9-req.scm")
;(load "tests-2.8-req.scm")
;(load "tests-2.6-req.scm")
;(load "tests-2.4-req.scm")
;(load "tests-2.3-req.scm")
;(load "tests-2.2-req.scm")
;(load "tests-2.1-req.scm")
;(load "tests-1.9-req.scm")
;(load "tests-1.8-req.scm")
;(load "tests-1.7-req.scm")
;(load "tests-1.6-req.scm")
;(load "tests-1.5-req.scm")
;(load "tests-1.4-req.scm")
;(load "tests-1.3-req.scm")
;(load "tests-1.2-req.scm")
;(load "tests-1.1-req.scm")
;

(define scheme-library-files
  '(
    ["libsymboltable-5.6.ss" "libsymboltable-5.6.s" "libsymboltable"]
    ["libhandlers-5.5.ss"    "libhandlers-5.5.s"    "libhandlers"   ]
    ["libcontrol-5.5.ss"     "libcontrol-5.5.s"     "libcontrol"    ]
    ["libintelasm-5.6.ss"    "libintelasm-5.6.s"    "libintelasm"   ]
    ["libcollect-5.3.ss"     "libcollect-5.3.s"     "libcollect"    ]
    ["librecord-5.6.ss"      "librecord-5.6.s"      "librecord"     ]
    ["libcore-5.6.ss"        "libcore-5.6.s"        "libcore"       ]
    ["libio-5.6.ss"          "libio-5.6.s"          "libio"         ]
    ["libwriter-5.6.ss"      "libwriter-5.6.s"      "libwriter"     ]
    ["libtokenizer-5.6.ss"   "libtokenizer-5.6.s"   "libtokenizer"  ]
    ["libexpand-5.6.ss"      "libexpand-5.6.s"      "libexpand"     ]
    ["libinterpret-5.6.ss"   "libinterpret-5.6.s"   "libinterpret"  ]
    ["libcafe-5.6.ss"        "libcafe-5.6.s"        "libcafe"       ]
    ["libtrace-5.3.ss"       "libtrace-5.3.s"       "libtrace"      ]
    ))



(define open-coded-primitives
;;; these primitives, when found in operator position with the correct
;;; number of arguments, will be open-coded by the generator.  If an 
;;; incorrect number of args is detected, or if they appear in non-operator
;;; position, then they cannot be open-coded, and the pcb-primitives table
;;; is consulted for a reference of the pcb slot containing the primitive.
;;; If it's not found there, an error is signalled.
;;;
;;; prim-name          args
  '([$constant-ref      1   value]
    [$constant-set!     2   effect]
    [$pcb-ref           1   value]
    [$pcb-set!          2   effect]
    ;;; type predicates
    [fixnum?            1   pred]
    [immediate?         1   pred]
    [boolean?           1   pred]
    [char?              1   pred]
    [pair?              1   pred]
    [symbol?            1   pred]
    [vector?            1   pred]
    [string?            1   pred]
    [procedure?         1   pred]
    [null?              1   pred]
    [eof-object?        1   pred]
    [$unbound-object?   1   pred]
    [not                1   pred]
    [eq?                2   pred]
    ;;; fixnum primitives
    [$fxadd1            1   value]
    [$fxsub1            1   value]
    [$fx+               2   value]
    [$fx-               2   value]
    [$fx*               2   value]
    [$fxsll             2   value]
    [$fxsra             2   value]
    [$fxlogand          2   value]
    [$fxlogor           2   value]
    [$fxlogxor          2   value]
    [$fxlognot          1   value]
    [$fxquotient        2   value]
    [$fxmodulo          2   value]
    ;;; fixnum predicates
    [$fxzero?           1   pred]
    [$fx=               2   pred]
    [$fx<               2   pred]
    [$fx<=              2   pred]
    [$fx>               2   pred]
    [$fx>=              2   pred]
    ;;; character predicates
    [$char=             2   pred]
    [$char<             2   pred]
    [$char<=            2   pred]
    [$char>             2   pred]
    [$char>=            2   pred]
    ;;; character conversion
    [$fixnum->char      1   value]
    [$char->fixnum      1   value]
    ;;; lists/pairs
    [cons               2   value]
    [$car               1   value]
    [$cdr               1   value]
    [$set-car!          2   effect]
    [$set-cdr!          2   effect]
    ;;; vectors
    [$make-vector       1   value]
    [vector           any   value]
    [$vector-length     1   value]
    [$vector-ref        2   value]
    [$vector-set!       3   effect]
    ;;; strings
    [$make-string       1   value]
    [$string          any   value]
    [$string-length     1   value]
    [$string-ref        2   value]
    [$string-set!       3   effect]
    ;;; symbols
    [$make-symbol       1   value]
    [$symbol-value      1   value]
    [$symbol-string     1   value]
    [$symbol-unique-string     1   value]
    [$set-symbol-value! 2   effect]
    [$set-symbol-unique-string! 2   effect]
    [$symbol-plist      1   value]
    [$set-symbol-plist! 2   effect]
    ;;; misc
    [eof-object         0   value]
    [void               0   value]
    [$exit              1   effect]
    [$fp-at-base        0   pred]
    [$current-frame     0   value]
    [$seal-frame-and-call 1  tail]
    [$frame->continuation 1 value]
    ;;; 
    ;;; records
    ;;;
    [$make-record       2   value]
    [$record?           1    pred]
    [$record-rtd        1   value]
    [$record-ref        2   value]
    [$record-set!       3  effect]
    ;;;
    ;;; asm
    ;;;
    [code?              1    pred]
    [$code-instr-size   1    value]
    [$code-reloc-size   1    value]
    [$code-closure-size 1    value]
    [$code->closure     1    value]
    [$set-code-byte!    3    effect]
    [$set-code-word!    3    effect]
    [$set-code-object!  4    effect]
    [$set-code-object+offset!  5    effect]
    [$set-code-object+offset/rel!  5    effect]
   ))

(define (primitive-context x)
  (cond
    [(assq x open-coded-primitives) => caddr]
    [else (error 'primitive-context "unknown prim ~s" x)]))

;;; pcb table section
(define pcb-table
 '(;;; system locations used by the C/Scheme interface
   [$system-stack       system       "system_stack"]
   [$stack-top          system          "stack_top"] ; top of stack
   [$stack-size         system         "stack_size"] ; its size
   [$frame-base         system         "frame_base"] ; base of the frame
   [$frame-redline      system      "frame_redline"] ; top + 2 pages
   [$frame-pointer      system      "frame_pointer"] ;
   [$heap-base          system          "heap_base"]   
   [$heap-size          system          "heap_size"]   
   [$allocation-redline system "allocation_redline"]
   [$allocation-pointer system "allocation_pointer"] 
   [$roots              system              "roots"]
   [$string-base        system        "string_base"]
   [$string-ap          system          "string_ap"]
   [$string-eap         system         "string_eap"]
   [$string-pages       system       "string_pages"]
   [$allocated-megs     system     "allocated_megs"]
   [$allocated-bytes    system    "allocated_bytes"]
   [$reclaimed-megs     system     "reclaimed_megs"]
   [$reclaimed-bytes    system    "reclaimed_bytes"]
   ;;; scheme_objects comes before all scheme objects
   [$scheme-objects     system     "scheme_objects"]
   [$next-continuation  system  "next_continuation"]
   ;;; error handling procedures used by the codegen
   [$apply-nonprocedure-error-handler library]
   [$incorrect-args-error-handler     library]
   [$multiple-values-error            library]
   [$intern                           library]
   [do-overflow                       library]
   [do-overflow-with-byte-count       library]
   [do-stack-overflow                 library]
   ;;; type predicates
   [fixnum?                 public]
   [immediate?              public]
   [boolean?                public]
   [char?                   public]
   [null?                   public]
   [pair?                   public]
   [symbol?                 public]
   [vector?                 public]
   [string?                 public]
   [procedure?              public]
   [eof-object?             public]
   [not                     public]
   [eq?                     public]
   [equal?                  public]
   ;;; fixnum primitives
   [fxadd1                  public]
   [fxsub1                  public]
   [fx+                     public]
   [fx-                     public]
   [fx*                     public]
   [fxsll                   public]
   [fxsra                   public]
   [fxlogor                 public]
   [fxlogand                public]
   [fxlogxor                public]
   [fxlognot                public]
   [fxquotient              public]
   [fxremainder             public]
   [fxmodulo                public]
   ;;; fixnum predicates
   [fxzero?                 public]
   [fx=                     public]
   [fx<                     public]
   [fx<=                    public]
   [fx>                     public]
   [fx>=                    public]
   ;;; characters
   [char=                   public]
   [char<                   public]
   [char<=                  public]
   [char>                   public]
   [char>=                  public]
   [fixnum->char            public]
   [char->fixnum            public]
   ;;; lists
   [cons                    public]
   [car                     public]
   [cdr                     public]
   [caar                    public]
   [cadr                    public]
   [cdar                    public]
   [cddr                    public]
   [caddr                   public]
   [cadddr                  public]
   [cddddr                  public]
   [set-car!                public]
   [set-cdr!                public]
   [list                    public]
   [list*                   ADDME]
   [list?                   public]
   [list-ref                public]
   [length                  public]
   [make-list               public]
   [reverse                 public]
   [append                  public]
   [list-ref                public]
   [memq                    public]
   [assq                    public]
   [map                     public]
   [for-each                public]
   [andmap                  public]
   [ormap                   public]
   ;;; vectors
   [make-vector             public]
   [vector                  public]
   [vector-length           public]
   [vector-ref              public]
   [vector-set!             public]
   [list->vector            public]
   [vector->list            public]
   ;;; strings
   [make-string             public]
   [string                  public]
   [string-length           public]
   [string-ref              public]
   [string-set!             public]
   [list->string            public]
   [string->list            ADDME]
   [string-append           public]
   [substring               public]
   [string=?                public]
   ;;; symbols
   [gensym                  public]
   [gensym?                 public]
   [symbol->string          public]
   [gensym->unique-string   public]
   [string->symbol          public]
   [top-level-value         public]
   [top-level-bound?        public]
   [set-top-level-value!    public]
   [getprop                 public]
   [putprop                 public]
   [remprop                 public]
   [property-list           public]
   [oblist                  public]
   ;;; eof
   [eof-object              public]
   [void                    public]
   ;;; control/debugging
   [print-error             public]
   [error                   public]
   [current-error-handler   public]
   [exit                    public]
   [apply                   public]
   [make-parameter          public]
   ;;; output
   [output-port?            public]
   [console-output-port     public]
   [current-output-port     public]
   [standard-output-port    public]
   [standard-error-port     public]
   [open-output-file        public]
   [close-output-port       public]
   [flush-output-port       public]
   [write-char              public]
   [output-port-name        public]
   [newline                 public]
   ;;; input
   [input-port?             public]
   [standard-input-port     public]
   [console-input-port      public]
   [current-input-port      public]
   [open-input-file         public]
   [close-input-port        public]
   [reset-input-port!       public]
   [read-char               public]
   [peek-char               public]
   [unread-char             public]
   [input-port-name         public]
   [write                   public]
   [display                 public]
   [read-token              public]
   [read                    public]
   ;;; evaluation
   [expand                  public]
   [core-expand             public]
   [current-expand          public]
   [interpret               public]
   [eval                    public]
   [current-eval            public]
   [load                    public]
   [new-cafe                public]
   [collect                 public]
   [call/cc                 public]
   [call/cf                 library]
   [dynamic-wind            public]
   [values                  public]
   [call-with-values        public]
   [make-traced-procedure   library]
   [trace-symbol!           library]
   [untrace-symbol!         library]
   ;;; record
   [record?                 public]
   [record-rtd              public]
   [record-name             public]
   [record-printer          public]
   [record-length           public]
   [record-ref              public]
   [record-set!             public]
   ;;; record rtds
   [make-record-type        public]
   [record-constructor      public]
   [record-predicate        public]
   [record-field-accessor   public]
   [record-field-mutator    public]
   ;;; asm
   [make-code               public]
   [code?                   public]
   [make-code-executable!   public]
   [code-instr-size         public]
   [code-reloc-size         public]
   [code-closure-size       public]
   [set-code-byte!          public]
   [set-code-word!          public]
   [set-code-object!        public]
   [set-code-object+offset! public]
   [set-code-object+offset/rel! public]
   [set-code-object/reloc/relative!  public]
   [code->closure           public]
   ;;;
   [$scheme-objects-end     system     "scheme_objects_end"]
   ))

(define (public-primitives)
  (let f ([ls pcb-table])
    (cond
      [(null? ls) '()]
      [(eq? (cadar ls) 'public) 
       (cons (caar ls) (f (cdr ls)))]
      [else (f (cdr ls))])))

(define (pcb-system-loc? x)
  (cond
    [(assq x pcb-table) =>
     (lambda (x) (eq? (cadr x) 'system))]
    [else (error 'pcb-system-loc? "not in table ~s" x)]))

(define *pcb-set-marker* (gensym))

(define *pcb-ref-marker* (gensym))

(define (mark-pcb-set-found x)
  (putprop x *pcb-set-marker* #t))
  
(define (mark-pcb-ref-found x)
;;(when (and (signal-error-on-undefined-pcb)
;;           (not (getprop x *pcb-set-marker*))
;;           (not (pcb-system-loc? x)))
;;  (error 'compile "found reference to unset primitive ~s" x))
  (putprop x *pcb-ref-marker* #t))
  
(define (pcb-referenced? x)
  (getprop x *pcb-ref-marker*))
  
(define (pcb-assigned? x)
  (getprop x *pcb-set-marker*))
  
(define (pcb-index x)
  (mark-pcb-ref-found x)
  (let f ([i 0] [ls pcb-table])
    (cond
      [(null? ls) 
       (error 'pcb-index "not in table ~s" x)]
      [(eq? x (caar ls)) i]
      [else (f (add1 i) (cdr ls))])))

(define (pcb-offset x)
  (* (pcb-index x) wordsize))

(define (primitive? x)
  (cond
    [(assq x pcb-table) #t]
    [(assq x open-coded-primitives) #t]
    [else #f]))

(define (open-codeable? x)
  (cond
    [(assq x open-coded-primitives) #t]
    [(assq x pcb-table) #f]
    [else (error 'open-codeable "invalid primitive ~s" x)]))

(define (open-coded-primitive-args x)
  (cond
    [(assq x open-coded-primitives) => cadr]
    [else (error 'open-coded-primitive-args "invalid ~s" x)]))

(define (pcb-cname x)
  (define (cname x i)
    (cond
      [(eq? (cadr x) 'system) (caddr x)]
      [else (format "prim_~a" i)]))
  (let f ([ls pcb-table] [i 0])
    (cond
      [(null? ls) (error 'pcb-cname "invalid name ~s" x)]
      [(eq? (caar ls) x) (cname (car ls) i)]
      [else (f (cdr ls) (add1 i))])))


(define (pcb-cnames)
  (define (cname x i)
    (cond
      [(eq? (cadr x) 'system) (caddr x)]
      [else (format "prim_~a" i)]))
  (let f ([ls pcb-table] [i 0])
    (cond
      [(null? ls) '()]
      [else
       (cons (cname (car ls) i) (f (cdr ls) (add1 i)))])))

;;; end of pcb table section

  
(define-record constant (value))
(define-record constant-loc (label))
(define-record code-loc (label))
(define-record foreign-label (label))
(define-record var (name))
(define-record cp-var (idx))
(define-record frame-var (idx))
(define-record new-frame (base-idx size body))
(define-record save-cp (loc))
(define-record eval-cp (check body))
(define-record return (value))
(define-record call-cp
  (call-convention rp-convention base-idx arg-count live-mask))
(define-record primcall (op arg*))
(define-record primref (name))
(define-record conditional (test conseq altern))
(define-record bind (lhs* rhs* body))
(define-record seq (e0 e1))
(define-record function (arg* proper body))
(define-record closure (code free*))
(define-record funcall (op rand*))
(define-record appcall (op rand*))
(define-record forcall (op rand*))

(define-record code (arg* proper free* body))
(define-record codes (lhs* rhs* body))
(define-record constants (name* body))
(define-record assign (lhs rhs))

(define unique-var
  (let ([counter 0])
    (lambda (x)
      (let ([g (string->symbol (format "~a:~a" x counter))])
        (set! counter (add1 counter))
        (make-var g)))))

(define (make-bind^ lhs* rhs* body)
  (if (null? lhs*)
      body
      (make-bind lhs* rhs* body)))



; (define (recordize x)
;   (define who 'recordize)
;   (define (self-evaluating? x)
;     (or (number? x) (boolean? x) (null? x) (char? x) (string? x)))
;   (define (verify-proper-bindings b* expr)
;     (unless (list? b*)
;       (error 'parse "invalid bindings in expression ~s" expr))
;     (for-each
;       (lambda (x)
;         (unless (and (list? x)
;                      (= (length x) 2)
;                      (symbol? (car x)))
;           (error 'parse "invalid binding ~a in expresison ~a" x expr)))
;       b*))
;   (define (Internal body* r x)
;     (when (null? body*) (error 'compile "No body in ~s" x))
;     (let f ([fst (car body*)] [body* (cdr body*)] [bind* '()])
;       (cond
;         [(and (pair? fst) (eq? (car fst) 'define) 
;               (not (assq 'define bind*))
;               (not (assq 'define r)))
;          (unless (and (list? fst) (= (length fst) 3))
;            (error 'parse "malformed internal definition ~s in ~s" fst x))
;          (unless (symbol? (cadr fst)) 
;             (error 'parse "invalid name in ~s" fst))
;          (when (null? body*)
;            (error 'parse "no expression in body of ~s" x))
;          (f (car body*) (cdr body*) (cons (cdr fst) bind*))]
;         [(and (pair? fst) (eq? (car fst) 'begin) 
;               (not (assq 'begin bind*))
;               (not (assq 'begin r)))
;          (let ([b* (cdr fst)])
;            (unless (list? b*) (error 'parse "invalid begin ~s" fst))
;            (let ([body* (append b* body*)])
;              (when (null? body*) 
;                (error 'parse "no expression in body of ~s" x))
;              (f (car body*) (cdr body*) bind*)))]
;         [else
;          (let ([lhs* (map car bind*)] [rhs* (map cadr bind*)])
;            (let ([name* (map unique-var lhs*)])
;              (let ([r (append (map cons lhs* name*) r)])
;                (let ([rhs*
;                       (let f ([rhs* rhs*] [ac '()])
;                         (cond
;                           [(null? rhs*) ac]
;                           [else
;                            (f (cdr rhs*) (cons (Expr (car rhs*) r) ac))]))])
;                  (build-letrec (reverse name*) rhs*
;                     (list->seq (Expr* (cons fst body*) r)))))))])))
;    (define (build-letrec lhs* rhs* body)
;      (if (null? lhs*) 
;          body
;          (let ([tmp* (map (lambda (x) (make-var 'tmp)) lhs*)]) 
;            (make-bind lhs* (map (lambda (x) (make-primcall 'void '())) lhs*)
;              (make-bind tmp* rhs* 
;                (make-seq (list->seq (map make-assign lhs* tmp*)) body))))))
;   (define (list->seq e*)
;     (let f ([ac (car e*)] [e* (cdr e*)])
;       (cond
;         [(null? e*) ac]
;         [else (f (make-seq ac (car e*)) (cdr e*))])))
;   (define (Expr* x* r)
;     (cond
;        [(null? x*) '()]
;        [else
;         (cons (Expr (car x*) r) (Expr* (cdr x*) r))]))
;   (define (Expr x r)
;     (cond
;       [(self-evaluating? x) (make-constant x)]
;       [(symbol? x)
;        (cond
;          [(assq x r) => cdr]
;          [(primitive? x) (make-primref x)]
;          [else (error 'recordize "unbound variable ~s" x)])]
;       [(not (list? x))
;        (error 'recordize "invalid expression ~s" x)]
;       [(and (symbol? (car x)) (assq (car x) r)) =>
;        (lambda (b)
;          (make-funcall (cdr b) (Expr* (cdr x) r)))]
;       [(eq? (car x) 'quote)
;        (unless (= (length x) 2)
;          (error who "invalid syntax ~s" 'quote))
;        (make-constant (cadr x))]
;       [(and (>= (length x) 2) (eq? (car x) 'begin))
;        (list->seq (Expr* (cdr x) r))]
;       [(eq? (car x) 'if)
;        (unless (= (length x) 4) 
;          (error who "invalid syntax ~s" x))
;        (make-conditional (Expr (cadr x) r)
;          (Expr (caddr x) r)
;          (Expr (cadddr x) r))]
;       [(and (eq? (car x) 'let) (pair? (cdr x)) (symbol? (cadr x)))
;        ;; named let
;        (unless (>= (length x) 4)
;          (error 'compile "invalid let ~s" x))
;        (let ([name (cadr x)] [bindings (caddr x)] [body* (cdddr x)])
;          (verify-proper-bindings bindings x)
;          (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
;            (let ([n-name (make-var name)] [nrhs* (Expr* rhs* r)])
;              (let ([r (cons (cons name n-name) r)]) 
;                (let ([nlhs* (map make-var lhs*)])
;                  (let ([r (append (map cons lhs* nlhs*) r)])
;                    (make-funcall
;                      (make-bind (list n-name)
;                                 (list (make-primcall 'void '()))
;                        (make-seq 
;                          (make-assign n-name
;                            (make-function nlhs* #t 
;                              (Internal body* r x)))
;                          n-name))
;                      nrhs*)))))))]
;       [(eq? (car x) 'let)
;        (unless (>= (length x) 3)
;          (error 'compile "invalid let ~s" x))
;        (let ([bindings (cadr x)] [body* (cddr x)])
;          (verify-proper-bindings bindings x)
;          (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
;            (let ([nlhs* (map make-var lhs*)] [nrhs* (Expr* rhs* r)])
;              (let ([r (append (map cons lhs* nlhs*) r)])
;                (make-bind nlhs* nrhs* 
;                (Internal body* r x))))))]
;       [(and (>= (length x) 3) (eq? (car x) 'let*))
;        (let ([bindings (cadr x)] [body* (cddr x)])
;          (verify-proper-bindings bindings x)
;          (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
;            (let ([nlhs* (map make-var lhs*)])
;              (let f ([lhs* lhs*] [nlhs* nlhs*] [rhs* rhs*] [r r])
;                (cond
;                  [(null? lhs*) (Internal body* r x)]
;                  [else
;                   (make-bind (list (car nlhs*))
;                              (list (Expr (car rhs*) r))
;                     (f (cdr lhs*)
;                        (cdr nlhs*)
;                        (cdr rhs*)
;                        (cons (cons (car lhs*) (car nlhs*)) r)))])))))]
;       [(and (>= (length x) 3) (eq? (car x) 'letrec))
;        (let ([bindings (cadr x)] [body* (cddr x)])
;          (verify-proper-bindings bindings x)
;          (cond
;            [(null? bindings) (list->seq (Expr* body* r))]
;            [else
;             (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
;               (let ([nlhs* (map make-var lhs*)]
;                     [tmp* (map make-var lhs*)]) 
;                 (let ([r (append (map cons lhs* nlhs*) r)])
;                   (make-bind nlhs* (map (lambda (x) (make-primcall 'void '())) nlhs*)
;                     (make-seq
;                       (make-bind tmp* (Expr* rhs* r)
;                         (list->seq (map make-assign nlhs* tmp*)))
;                       (Internal body* r x))))))]))]
;       [(and (>= (length x) 3) (eq? (car x) 'letrec*))
;        (let ([bindings (cadr x)] [body* (cddr x)])
;          (verify-proper-bindings bindings x)
;          (cond
;            [(null? bindings) (list->seq (Expr* body* r))]
;            [else
;             (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
;               (let ([nlhs* (map make-var lhs*)])
;                 (let ([r (append (map cons lhs* nlhs*) r)])
;                   (make-bind nlhs* (map (lambda (x) (make-primcall 'void '())) nlhs*)
;                     (make-seq
;                        (list->seq
;                          (map make-assign nlhs* (Expr* rhs* r)))
;                        (Internal body* r x))))))]))]
;       [(and (>= (length x) 3) (eq? (car x) 'lambda))
;        (let ([arg* (cadr x)] [body* (cddr x)])
;           (define (new-arg* arg*)
;             (cond
;               [(null? arg*) '()]
;               [(symbol? arg*) (list (make-var arg*))]
;               [else 
;                (cons (make-var (car arg*)) (new-arg* (cdr arg*)))]))
;           (define (verify-proper-args args expr)
;             (define (proper-args args)
;               (or (null? args)
;                   (symbol? args)
;                   (and (pair? args)
;                        (symbol? (car args))
;                        (proper-args (cdr args)))))
;             (unless (proper-args args)
;               (error 'parse "invalid arguments in ~s" expr)))
;           (define (extend-args lhs* rhs* r)
;             (cond
;               [(null? lhs*) r]
;               [(symbol? lhs*) (cons (cons lhs* (car rhs*)) r)]
;               [else
;                (extend-args (cdr lhs*) (cdr rhs*)
;                  (cons (cons (car lhs*) (car rhs*)) r))]))
;           (verify-proper-args arg* x)
;           (let ([narg* (new-arg* arg*)])
;             (let ([r (extend-args arg* narg* r)])
;               (make-function narg* (list? arg*)
;                 (Internal body* r x)))))]
;       [(eq? (car x) 'and)
;        (if (null? (cdr x))
;            (make-constant #t)
;            (let f ([a (cadr x)] [d (cddr x)])
;              (cond
;                [(null? d) (Expr a r)]
;                [else
;                 (make-conditional (Expr a r)
;                   (f (car d) (cdr d))
;                   (make-constant #f))])))]
;       [(eq? (car x) 'or)
;        (if (null? (cdr x))
;            (make-constant #f)
;            (let f ([a (cadr x)] [d (cddr x)])
;              (cond
;                [(null? d) (Expr a r)]
;                [else
;                 (let ([t (make-var 'tmp)])
;                   (make-bind (list t) (list (Expr a r))
;                     (make-conditional t t (f (car d) (cdr d)))))])))]
;       [(and (>= (length x) 3) (eq? (car x) 'when))
;        (let ([test (cadr x)] [body* (cddr x)])
;          (make-conditional (Expr test r)
;            (list->seq (Expr* body* r))
;            (make-primcall 'void '())))]
;       [(and (>= (length x) 3) (eq? (car x) 'unless))
;        (let ([test (cadr x)] [body* (cddr x)])
;          (make-conditional (Expr test r)
;            (make-primcall 'void '())
;            (list->seq (Expr* body* r))))]
;       [(and (>= (length x) 2) (eq? (car x) 'cond))
;        (let f ([cls (cadr x)] [cls* (cddr x)])
;          (cond
;            [(not (list? cls))
;             (error who "malformed cond clause ~s in ~s" cls x)]
;            [(not (pair? cls))
;             (error who "malformed cond clause ~s in ~s" cls x)]
;            [(null? cls*)
;             (cond
;               [(and (eq? (car cls) 'else)
;                     (not (assq 'else r)))
;                (unless (>= (length cls) 2)
;                  (error who "malformed cond else clause ~s in ~s" cls x))
;                (list->seq (Expr* (cdr cls) r))]
;               [(and (>= (length cls) 2)
;                     (eq? (cadr cls) '=>)
;                     (not (assq '=> r)))
;                (unless (= (length cls) 3)
;                  (error who "malformed cond last => clause ~s in ~s" cls x))
;                (let ([t (make-var 'tmp)])
;                  (make-bind (list t) (list (Expr (car cls) r))
;                    (make-conditional t
;                      (make-funcall (Expr (caddr cls) r) (list t))
;                      (make-primcall 'void '()))))]
;               [(= (length cls) 1)
;                (let ([t (make-var 'tmp)])
;                  (make-bind (list t) (list (Expr (car cls) r))
;                    (make-conditional t t (make-primcall 'void '()))))]
;               [else
;                (make-conditional (Expr (car cls) r)
;                  (list->seq (Expr* (cdr cls) r))
;                  (make-primcall 'void '()))])]
;            [else
;             (cond
;              [(and (>= (length cls) 2)
;                   (eq? (cadr cls) '=>)
;                   (not (assq '=> r)))
;               (unless (= (length cls) 3)
;                 (error who "malformed cond => clause ~s in ~s" cls x))
;               (let ([t (make-var 'tmp)])
;                 (make-bind (list t) (list (Expr (car cls) r))
;                   (make-conditional t
;                     (make-funcall (Expr (caddr cls) r) (list t))
;                     (f (car cls*) (cdr cls*)))))]
;              [(= (length cls) 1)
;               (let ([t (make-var 'tmp)])
;                 (make-bind (list t) (list (Expr (car cls) r))
;                   (make-conditional t t 
;                      (f (car cls*) (cdr cls*)))))]
;              [else
;               (make-conditional (Expr (car cls) r)
;                 (list->seq (Expr* (cdr cls) r))
;                 (f (car cls*) (cdr cls*)))])]))]
;       [(and (= (length x) 3) (eq? (car x) 'set!))
;        (let ([var (cadr x)] [val (caddr x)])
;          (unless (symbol? var)
;            (error who "invalid syntax in ~s" x))
;          (cond
;            [(assq var r) =>
;             (lambda (p)
;               (make-assign (cdr p) (Expr val r)))]
;            [else 
;             (error who "unbound variable ~s in ~s" var x)]))]
;       [(and (eq? (car x) '$apply))
;        (unless (>= (length (cdr x)) 2)
;          (error who "insufficient arguments to $apply in ~s" x))
;        (let ([rator (cadr x)] [rand* (cddr x)])
;          (make-appcall (Expr rator r) (Expr* rand* r)))]
;       [(eq? (car x) 'foreign-call)
;        (unless (and (>= (length x) 2) (string? (cadr x)))
;          (error who "invalid syntax ~s" x))
;        (make-forcall (cadr x) (Expr* (cddr x) r))]
;       [(eq? (car x) '$pcb-set!)
;        (unless (= (length x) 3)
;          (error who "incorrect number of args in ~s" x))
;        (mark-pcb-set-found (cadr x))
;        (make-primcall '$pcb-set! 
;          (list (make-constant (pcb-index (cadr x))) (Expr (caddr x) r)))]
;       [else 
;        (make-funcall (Expr (car x) r) (Expr* (cdr x) r))]))
;   (Expr x '()))

;;; <CS> ::= (quote datum)
;;;        | <gensym>
;;;        | (if <CS> <CS> <CS>)
;;;        | (set! <gensym> <CS>)
;;;        | (begin <CS> <CS> ...)
;;;        | (lambda <FMLS> <CS> <CS> ...)
;;;        | (<prim> <CS> <CS> ...)
;;;        | (<CS> <CS> ...)
;;; <FML> ::= ()
;;;         | <gensym>
;;;         | (<gensym> . <FML>)
;;; <prim> ::= void | memv | top-level-value | set-top-level-value! | $pcb-set!

(define-syntax $pcb-set!
  (syntax-rules ()
    [(_ name val) 
     (set-top-level-value! 'name val)]))

(define (immediate? x)
  (or (fixnum? x)
      (char? x)
      (boolean? x)
      (eof-object? x)
      (eq? x (void))))


(load "libexpand-5.6.ss")

(define (recordize x)
  (define (gen-fml* fml*)
    (cond
      [(pair? fml*)
       (cons (unique-var (car fml*))
             (gen-fml* (cdr fml*)))]
      [(symbol? fml*)
       (unique-var fml*)]
      [else '()]))
  (define (properize fml*)
    (cond
      [(pair? fml*)
       (cons (car fml*) (properize (cdr fml*)))]
      [(null? fml*) '()]
      [else (list fml*)]))
  (define (extend-env fml* nfml* env)
    (cons (cons fml* nfml*) env))
  (define (quoted-sym x)
    (if (and (list? x)
             (= (length x) 2)
             (eq? 'quote (car x))
             (symbol? (cadr x)))
        (cadr x)
        (error 'quoted-sym "not a quoted symbol ~s" x)))
   (define (quoted-string x)
    (if (and (list? x)
             (= (length x) 2)
             (eq? 'quote (car x))
             (string? (cadr x)))
        (cadr x)
        (error 'quoted-string "not a quoted string ~s" x)))
  (define (lookup^ x lhs* rhs*)
    (cond
      [(pair? lhs*)
       (if (eq? x (car lhs*))
           (car rhs*)
           (lookup^ x (cdr lhs*) (cdr rhs*)))]
      [(eq? x lhs*) rhs*]
      [else #f]))
  (define (lookup x env)
    (cond
      [(pair? env)
       (or (lookup^ x (caar env) (cdar env))
           (lookup x (cdr env)))]
      [else #f]))
  (define (E x env)
    (cond
      [(pair? x)
       (case (car x)
         [(quote)  (make-constant (cadr x))]
         [(if) 
          (make-conditional 
            (E (cadr x) env)
            (E (caddr x) env)
            (E (cadddr x) env))]
         [(set!)
          (let ([lhs (cadr x)] [rhs (caddr x)])
            (make-assign
              (or (lookup lhs env) 
                  (error 'recordize "invalid assignment ~s" x))
              (E rhs env)))]
         [(begin)
          (let f ([a (cadr x)] [d (cddr x)])
            (cond
              [(null? d) (E a env)]
              [else
               (make-seq 
                 (E a env)
                 (f (car d) (cdr d)))]))]
         [(lambda)
          (unless (= (length x) 3)
            (error 'recordize "invalid ~s" x))
          (let ([fml* (cadr x)] [body (caddr x)])
            (let ([nfml* (gen-fml* fml*)])
              (make-function 
                (properize nfml*)
                (list? fml*)
                (E body (extend-env fml* nfml* env)))))]
         [($pcb-set!) 
          (let ([var (quoted-sym (cadr x))] [val (caddr x)])
              (mark-pcb-set-found var)
              (make-primcall '$pcb-set! 
                (list (make-constant (pcb-index var))
                      (E val env))))]
         [(foreign-call)
          (let ([name (quoted-string (cadr x))] [arg* (cddr x)])
            (make-forcall name
              (map (lambda (x) (E x env)) arg*)))]
         [(top-level-value)
          (let ([var (quoted-sym (cadr x))])
            (if (primitive? var)
                (make-primref var)
                (error 'recordize "invalid top-level var ~s" var)))]
         [($apply)
          (let ([proc (cadr x)] [arg* (cddr x)])
            (make-appcall
              (E proc env)
              (map (lambda (x) (E x env)) arg*)))]
         [(void) 
          (make-constant (void))]
         [else
          (make-funcall 
            (E (car x) env)
            (map (lambda (x) (E x env)) (cdr x)))])]
      [(symbol? x)
       (or (lookup x env)
           (error 'recordize "invalid reference in ~s" x))]
      [else (error 'recordize "invalid expression ~s" x)]))
  (E (expand x) '()))


(define (unparse x)
  (define (E-args proper x)
    (if proper 
        (map E x)
        (let f ([a (car x)] [d (cdr x)])
          (cond
            [(null? d) (E a)]
            [else (cons (E a) (f (car d) (cdr d)))]))))
  (define (E x)
    (record-case x
      [(constant c) `(quote ,c)]
      [(constant-loc x) `(constant-loc ,x)]
      [(code-loc x) `(code-loc ,x)]
      [(var x) (string->symbol (format "v:~a" x))]
      [(primref x) x]
      [(conditional test conseq altern) 
       `(if ,(E test) ,(E conseq) ,(E altern))]
      [(primcall op arg*) `(,op . ,(map E arg*))]
      [(bind lhs* rhs* body) 
       `(let ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(seq e0 e1) `(begin ,(E e0) ,(E e1))]
      [(function args proper body)
       `(lambda ,(E-args proper args) ,(E body))]
      [(closure code free*)
       `(closure ,(E code) ,(map E free*))]
      [(code arg* proper free* body) 
       `(code [arg: ,(E-args proper arg*)]
              [free: ,(map E free*)]
          ,(E body))]
      [(codes lhs* rhs* body)
       `(codes ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(funcall rator rand*) `(funcall ,(E rator) . ,(map E rand*))]
      [(appcall rator rand*) `(appcall ,(E rator) . ,(map E rand*))]
      [(forcall rator rand*) `(foreign-call ,rator . ,(map E rand*))]
      [(assign lhs rhs) `(set! ,(E lhs) ,(E rhs))]
      [(constants lhs* body) `(constants ,(map E lhs*) ,(E body))]
      [(return x) `(return ,(E x))]
      ;;; (define-record new-frame (base-idx size body))
      [(new-frame base-idx size body)
       `(new-frame [base: ,base-idx]
                   [size: ,size]
          ,(E body))]
      [(frame-var idx) 
       (string->symbol (format "fv.~a" idx))]
      [(cp-var idx) 
       (string->symbol (format "cp.~a" idx))]
      [(save-cp expr)
       `(save-cp ,(E expr))]
      [(eval-cp check body)
       `(eval-cp ,check ,(E body))]
      [(call-cp call-convention rp-convention base-idx arg-count live-mask)
       `(call-cp [conv: ,call-convention]
                 [rpconv: ,rp-convention]
                 [base-idx: ,base-idx]
                 [arg-count: ,arg-count]
                 [live-mask: ,live-mask])]
      [else (error 'unparse "invalid record ~s" x)]))
  (E x))

(define (optimize-direct-calls x)
  (define who 'optimize-direct-calls)
  (define (make-conses ls)
    (cond
      [(null? ls) (make-constant '())]
      [else 
       (make-primcall 'cons 
         (list (car ls) (make-conses (cdr ls))))]))      
  (define (properize lhs* rhs*)
    (cond
      [(null? lhs*) (error who "improper improper")]
      [(null? (cdr lhs*)) 
       (list (make-conses rhs*))]
      [else (cons (car rhs*) (properize (cdr lhs*) (cdr rhs*)))]))
  (define (inline rator rand*)
    (record-case rator
      [(function fml* proper body)
       (cond
         [proper 
          (if (= (length fml*) (length rand*))
              (make-bind fml* rand* body)
              (begin
                (warning 'compile "possible application error in ~s"
                         (unparse (make-funcall rator rand*)))
                (make-funcall rator rand*)))]
         [else
          (if (<= (length fml*) (length rand*))
              (make-bind fml* (properize fml* rand*) body)
              (begin
                (warning 'compile "possible application error in ~s"
                         (unparse (make-funcall rator rand*)))
                (make-funcall rator rand*)))])]
      [else (make-funcall rator rand*)]))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test)
         (Expr conseq)
         (Expr altern))]
      [(seq e0 e1) 
       (make-seq (Expr e0) (Expr e1))]
      [(function fml* proper body) 
       (make-function fml* proper (Expr body))]
      [(primcall rator rand*) 
       (make-primcall rator (map Expr rand*))]
      [(funcall rator rand*)
       (inline (Expr rator) (map Expr rand*))]
      [(appcall rator rand*)
       (make-appcall (Expr rator) (map Expr rand*))]
      [(forcall rator rand*) 
       (make-forcall rator (map Expr rand*))]
      [(assign lhs rhs)
       (make-assign lhs (Expr rhs))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x))



(define (uncover-assigned x)
  (define who 'uncover-assigned)
  (define (Expr* x*)
    (cond
      [(null? x*) '()]
      [else (union (Expr (car x*)) (Expr* (cdr x*)))]))
  (define (Expr x)
    (record-case x
      [(constant) '()]
      [(var) '()]
      [(primref) '()]
      [(bind lhs* rhs* body)
       (union (Expr body) (Expr* rhs*))]
      [(conditional test conseq altern)
       (union (Expr test) (union (Expr conseq) (Expr altern)))]
      [(seq e0 e1) (union (Expr e0) (Expr e1))]
      [(function fml* proper body) (Expr body)]
      [(primcall rator rand*) (Expr* rand*)]
      [(funcall rator rand*)
       (union (Expr rator) (Expr* rand*))]
      [(appcall rator rand*)
       (union (Expr rator) (Expr* rand*))]
      [(forcall rator rand*) (Expr* rand*)]
      [(assign lhs rhs)
       (union (singleton lhs) (Expr rhs))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x))

(define (rewrite-assignments assigned x)
  (define who 'rewrite-assignments)
  (define (fix lhs*)
    (cond
      [(null? lhs*) (values '() '() '())]
      [else
       (let ([x (car lhs*)])
         (let-values ([(lhs* a-lhs* a-rhs*) (fix (cdr lhs*))])
           (cond
             [(memq x assigned) 
              (let ([t (make-var 'assignment-tmp)])
                (values (cons t lhs*) (cons x a-lhs*) (cons t a-rhs*)))]
             [else
              (values (cons x lhs*) a-lhs* a-rhs*)])))]))
  (define (bind-assigned lhs* rhs* body)
    (cond
      [(null? lhs*) body]
      [else
       (make-bind lhs*
         (map (lambda (rhs) (make-primcall 'vector (list rhs))) rhs*)
         body)]))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) 
       (cond
         [(memq x assigned) 
          (make-primcall '$vector-ref (list x (make-constant 0)))]
         [else x])]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let-values ([(lhs* a-lhs* a-rhs*) (fix lhs*)]) 
         (make-bind lhs* (map Expr rhs*) 
           (bind-assigned a-lhs* a-rhs* (Expr body))))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(function fml* proper body) 
       (let-values ([(fml* a-lhs* a-rhs*) (fix fml*)]) 
         (make-function fml* proper 
           (bind-assigned a-lhs* a-rhs* (Expr body))))]
      [(primcall op rand*)
       (make-primcall op (map Expr rand*))]
      [(forcall op rand*)
       (make-forcall op (map Expr rand*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(appcall rator rand*)
       (make-appcall (Expr rator) (map Expr rand*))]
      [(assign lhs rhs)
       (unless (memq lhs assigned)
         (error 'rewrite-assignments "not assigned ~s in ~s" lhs x))
       (make-primcall '$vector-set! (list lhs (make-constant 0) (Expr rhs)))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x))
  
(define (remove-assignments x)
  (let ([assigned (uncover-assigned x)])
    (rewrite-assignments assigned x)))
  

(define (convert-closures prog)
  (define who 'convert-closures)
  (define (Expr* x*)
    (cond
      [(null? x*) (values '() '())]
      [else
       (let-values ([(a a-free) (Expr (car x*))]
                    [(d d-free) (Expr* (cdr x*))])
         (values (cons a d) (union a-free d-free)))]))
  (define (Expr ex)
    (record-case ex
      [(constant) (values ex '())]
      [(var) (values ex (singleton ex))]
      [(primref) (values ex '())]
      [(bind lhs* rhs* body)
       (let-values ([(rhs* rhs-free) (Expr* rhs*)] 
                    [(body body-free) (Expr body)])
          (values (make-bind lhs* rhs* body)
                  (union rhs-free (difference body-free lhs*))))]
      [(conditional test conseq altern)
       (let-values ([(test test-free) (Expr test)]
                    [(conseq conseq-free) (Expr conseq)]
                    [(altern altern-free) (Expr altern)])
         (values (make-conditional test conseq altern)
                 (union test-free (union conseq-free altern-free))))]
      [(seq e0 e1) 
       (let-values ([(e0 e0-free) (Expr e0)]
                    [(e1 e1-free) (Expr e1)])
         (values (make-seq e0 e1) (union e0-free e1-free)))]
      [(function fml* proper body) 
       (let-values ([(body body-free) (Expr body)])
         (let ([free (difference body-free fml*)])
           (values (make-closure (make-code fml* proper free body) free)
                   free)))]
      [(primcall op rand*)
       (let-values ([(rand* rand*-free) (Expr* rand*)])
         (values (make-primcall op rand*)  rand*-free))]
      [(forcall op rand*)
       (let-values ([(rand* rand*-free) (Expr* rand*)])
         (values (make-forcall op rand*)  rand*-free))]
      [(funcall rator rand*)
       (let-values ([(rator rat-free) (Expr rator)]
                    [(rand* rand*-free) (Expr* rand*)])
         (values (make-funcall rator rand*) 
                 (union rat-free rand*-free)))]
      [(appcall rator rand*)
       (let-values ([(rator rat-free) (Expr rator)]
                    [(rand* rand*-free) (Expr* rand*)])
         (values (make-appcall rator rand*) 
                 (union rat-free rand*-free)))]
      [else (error who "invalid expression ~s" (unparse ex))]))
  (let-values ([(prog free) (Expr prog)])
    (unless (null? free) 
      (error 'convert-closures "free vars ~s encountered in ~a"
          free (unparse prog)))
   prog))


(define (lift-codes x)
  (define who 'lift-codes)
  (define (Expr* x*)
    (cond
     [(null? x*) (values '() '())]
     [else
      (let-values ([(a a-free) (Expr (car x*))]
                   [(d d-free) (Expr* (cdr x*))])
       (values (cons a d) (append a-free d-free)))]))
  (define (Expr x)
    (record-case x
      [(constant) (values x '())]
      [(var) (values x '())]
      [(primref) (values x '())]
      [(bind lhs* rhs* body)
       (let-values ([(rhs* rhs-codes) (Expr* rhs*)] 
                    [(body body-codes) (Expr body)])
         (values (make-bind lhs* rhs* body)
                 (append rhs-codes body-codes)))]
      [(conditional test conseq altern)
       (let-values ([(test test-codes) (Expr test)]
                    [(conseq conseq-codes) (Expr conseq)]
                    [(altern altern-codes) (Expr altern)])
         (values (make-conditional test conseq altern)
                 (append test-codes conseq-codes altern-codes)))]
      [(seq e0 e1) 
       (let-values ([(e0 e0-codes) (Expr e0)]
                    [(e1 e1-codes) (Expr e1)])
         (values (make-seq e0 e1) (append e0-codes e1-codes)))]
      [(closure c free) 
       (let-values ([(c codes) 
                     (record-case c
                       [(code arg* proper free* body)
                        (let-values ([(body body-codes) (Expr body)])
                          (let ([g (make-code-loc 'code)])
                            (values g 
                              (cons 
                                (cons g (make-code arg* proper free* body))
                                body-codes))))]
                       [else (error #f "invalid code ~s" c)])])
         (values (make-closure c free) codes))]
      [(primcall op rand*)
       (let-values ([(rand* rand*-codes) (Expr* rand*)])
         (values (make-primcall op rand*) rand*-codes))]
      [(forcall op rand*)
       (let-values ([(rand* rand*-codes) (Expr* rand*)])
         (values (make-forcall op rand*) rand*-codes))]
      [(funcall rator rand*)
       (let-values ([(rator rat-codes) (Expr rator)]
                    [(rand* rand*-codes) (Expr* rand*)])
         (values
           (make-funcall rator rand*) 
           (append rat-codes rand*-codes)))]
      [(appcall rator rand*)
       (let-values ([(rator rat-codes) (Expr rator)]
                    [(rand* rand*-codes) (Expr* rand*)])
         (values
           (make-appcall rator rand*) 
           (append rat-codes rand*-codes)))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (let-values ([(x codes) (Expr x)])
    (make-codes (map car codes) (map cdr codes) x)))

(define (lift-complex-constants x)
  (define who 'lift-complex-constants)
  (define complex-lhs* '())
  (define complex-rhs* '())
  (define symbols-lhs* '())
  (define symbols-rhs* '())
  (define *symbol-key* (gensym))
  (define (symbol-convert x)
   (make-funcall 
     (make-primcall '$pcb-ref 
       (list (make-constant (pcb-index '$intern))))
     (list (convert (symbol->string x)))))
  (define (convert x)
   (cond
    [(pair? x) 
     (make-primcall 'cons 
       (list (convert (car x))
             (convert (cdr x))))]
    [(vector? x)
     (make-primcall 'vector
       (map convert (vector->list x)))]
    [(string? x)
     (make-primcall '$string
       (map make-constant (string->list x)))]
    [(symbol? x) (intern x)]
    [else (make-constant x)]))
  (define (intern x)
    (cond
      [(and (symbol? x) (getprop x *symbol-key*))]
      [(symbol? x)
       (let ([t (make-constant-loc 'constant)]
             [v (symbol-convert x)])
         (set! symbols-lhs* (cons t symbols-lhs*))
         (set! symbols-rhs* (cons v symbols-rhs*))
         (putprop x *symbol-key* t)
         t)]
      [else
       (let ([t (make-constant-loc 'constant)]
             [v (convert x)])
         (set! complex-lhs* (cons t complex-lhs*))
         (set! complex-rhs* (cons v complex-rhs*))
         t)]))
  (define (assign-complex* lhs* rhs* body)
    (cond
     [(null? lhs*) body]
     [else
      (assign-complex* (cdr lhs*) (cdr rhs*)
        (make-seq
          (make-primcall '$set-constant! (list (car lhs*) (car rhs*)))
          body))]))
  (define (Expr x)
    (record-case x
      [(constant c) 
       (cond
        [(or (pair? c) (string? c) (vector? c) (symbol? c))
         (intern c)]
        [(or (boolean? c) (integer? c) (char? c) (null? c) 
             (eof-object? c) (eq? c (void))) 
         x]
        [else (error who "what constant ~s" c)])]
      [(var) x]
      [(primref) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) 
       (make-seq (Expr e0) (Expr e1))]
      [(closure c free) x] 
      [(primcall op rand*)
       (make-primcall op (map Expr rand*))]
      [(forcall op rand*)
       (make-forcall op (map Expr rand*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(appcall rator rand*)
       (make-appcall (Expr rator) (map Expr rand*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (CodeExpr x)
    (record-case x
     [(code fml* proper free* body)
      (make-code fml* proper free* (Expr body))]))
  (record-case x
   [(codes lhs* rhs* body)
    (let ([rhs* (map CodeExpr rhs*)] [body (Expr body)])
      (let ([init-lhs (make-code-loc 'init)]
            [init-rhs 
             (make-code '() #t '()
               (assign-complex* symbols-lhs* symbols-rhs*
                 (assign-complex* complex-lhs* complex-rhs* 
                   (make-constant #t))))]) 
        (make-constants (append complex-lhs* symbols-lhs*)
          (make-codes (cons init-lhs lhs*)
                      (cons init-rhs rhs*)
            (make-seq (make-funcall (make-closure init-lhs '()) '()) 
                      body)))))]))

            
(define (syntactically-valid? op rand*)
  (define (valid-arg-count? op rand*)
    (let ([n (open-coded-primitive-args op)] [m (length rand*)])
      (cond
        [(eq? n 'any) #t]
        [(eq? n 'no-code) 
         (error 'syntactically-valid
           "should not primcall non codable prim ~s" op)]
        [(fixnum? n) 
         (cond
          [(= n m) #t]
          [else
           (warning 'compile 
             "Possible incorrect number of args in ~s"
              (cons op (map unparse rand*)))
           #f])]
        [else (error 'do-primcall "BUG: what ~s" n)])))
  (define (check op pred?)
    (lambda (arg)
      (record-case arg
        [(constant c)
         (cond
           [(pred? c) #t]
           [else
            (warning 'compile "Possible argument error to primitive ~s" op)
            #f])]
        [(primref) 
         (cond
           [(pred? (lambda (x) x)) #t]
           [else
            (warning 'compile "Possible argument error to primitive ~s" op)
            #f])]
        [else #t])))
  (define (nonnegative-fixnum? n)
    (and (fixnum? n) (>= n 0)))
  (define (byte? n)
    (and (fixnum? n) (<= 0 n 127)))
  (define (valid-arg-types? op rand*)
    (case op
      [(fixnum? immediate? boolean? char? vector? string? procedure?
        null? pair? not cons eq? vector symbol? error eof-object eof-object? 
        void $unbound-object? code?)
       '#t]
      [($fxadd1 $fxsub1 $fxzero? $fxlognot $fxlogor $fxlogand $fx+ $fx- $fx* 
        $fx= $fx< $fx<= $fx> $fx>= $fxquotient $fxmodulo $fxsll $fxsra $fxlogxor $exit) 
       (andmap (check op fixnum?) rand*)]
      [($fixnum->char) 
       (andmap (check op byte?) rand*)]
      [($char->fixnum $char= $char< $char<= $char> $char>= $string)
       (andmap (check op char?) rand*)]
      [($make-vector $make-string)
       (andmap (check op nonnegative-fixnum?) rand*)]
      [($car $cdr) 
       (andmap (check op pair?) rand*)]
      [($vector-length) 
       (andmap (check op vector?) rand*)]
      [($string-length) 
       (andmap (check op string?) rand*)]
      [($set-car! $set-cdr!)
       ((check op pair?) (car rand*))]
      [($vector-ref $vector-set!)
       (and ((check op vector?) (car rand*))
            ((check op nonnegative-fixnum?) (cadr rand*)))]
      [($string-ref $string-set!
        $string-ref-16+0 $string-ref-16+1 $string-ref-8+0 $string-ref-8+2)
       (and ((check op string?) (car rand*))
            ((check op nonnegative-fixnum?) (cadr rand*)))]
      [($symbol-string $symbol-unique-string)
       (andmap (check op symbol?) rand*)]
      [($constant-ref $set-constant! $intern $pcb-set! $pcb-ref $make-symbol
        $symbol-value $set-symbol-value! $symbol-plist $set-symbol-plist!
        $set-symbol-unique-string!
        $seal-frame-and-call  $frame->continuation $code->closure
        $code-instr-size $code-reloc-size $code-closure-size
        $set-code-byte! $set-code-word! 
        $set-code-object! $set-code-object+offset! $set-code-object+offset/rel!
        $make-record $record? $record-rtd $record-ref $record-set!)
       #t]
      [else (error 'valid-arg-types? "unhandled op ~s" op)]))
  (and (valid-arg-count? op rand*)
       (or (null? rand*)
           (valid-arg-types? op rand*))))


;;; the output of simplify-operands differs from the input in that the 
;;; operands to primcalls are all simple (variables, primrefs, or constants).
;;; funcalls to open-codable primrefs whos arguments are "ok" are converted to
;;; primcalls.  

(define (introduce-primcalls x)
  (define who 'introduce-primcalls)
  (define (simple? x) 
    (or (constant-loc? x) (constant? x) (var? x) (primref? x)))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(constant-loc) x]
      [(var) x]
      [(primref) x]
      [(closure) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(primcall op arg*)
       (case op
         ;[(values) 
         ; (if (= (length arg*) 1)
         ;     (Expr (car arg*))
         ;     (begin
         ;       (warning 'compile "possible incorrect number of values")
         ;       (make-funcall (make-primref 'values) (map Expr arg*))))]
         [else 
          (make-primcall op (map Expr arg*))])]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator rand*)
       (cond
         [(and (primref? rator) 
               (inline-primitives) 
               (open-codeable? (primref-name rator))
               (syntactically-valid? (primref-name rator) rand*))
          (Expr (make-primcall (primref-name rator) rand*))]
         [else
          (make-funcall (Expr rator) (map Expr rand*))])]
      [(appcall op arg*)
       (make-appcall (Expr op) (map Expr arg*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(constant) (make-return x)]
      [(constant-loc) (make-return x)]
      [(var) (make-return x)]
      [(primref) (make-return x)]
      [(closure) (make-return x)]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Tail body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Tail e1))]
      [(primcall op arg*)
       (case op
         ;[(values) 
         ; (if (= (length arg*) 1)
         ;     (make-return (Expr (car arg*)))
         ;     (make-return* (map Expr arg*)))]
         [else
          (make-return (make-primcall op (map Expr arg*)))])]
      [(forcall op arg*)
       (make-return (make-forcall op (map Expr arg*)))]
      [(funcall rator rand*)
       (cond
         [(and (primref? rator)
               (inline-primitives) 
               (open-codeable? (primref-name rator))
               (syntactically-valid? (primref-name rator) rand*))
          (Tail (make-primcall (primref-name rator) rand*))]
         [else
          (make-funcall (Expr rator) (map Expr rand*))])]
      [(appcall op arg*)
       (make-appcall (Expr op) (map Expr arg*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (CodeExpr x)
    (record-case x 
      [(code fml* proper free* body)
       (make-code fml* proper free* (Tail body))]))
  (define (CodesExpr x)
    (record-case x 
      [(codes lhs* rhs* body)
       (make-codes lhs* (map CodeExpr rhs*) (Tail body))]))
  (define (ConstantsExpr x)
    (record-case x 
      [(constants lhs* body)
       (make-constants lhs* (CodesExpr body))]))
  (ConstantsExpr x))


(define (simplify-operands x)
  (define who 'simplify-operands)
  (define (simple? x) 
    (or (constant-loc? x) (constant? x) (var? x) (primref? x)))
  (define (simplify arg lhs* rhs* k)
    (if (simple? arg)
        (k arg lhs* rhs*)
        (let ([v (unique-var 'tmp)])
          (k v (cons v lhs*) (cons (Expr arg) rhs*)))))
  (define (simplify* arg* lhs* rhs* k)
    (cond
      [(null? arg*) (k '() lhs* rhs*)]
      [else 
       (simplify (car arg*) lhs* rhs*
         (lambda (a lhs* rhs*)
           (simplify* (cdr arg*) lhs* rhs*
             (lambda (d lhs* rhs*)
               (k (cons a d) lhs* rhs*)))))]))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(constant-loc) x]
      [(var) x]
      [(primref) x]
      [(closure) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(primcall op arg*)
       (simplify* arg* '() '()
         (lambda (arg* lhs* rhs*)
           (make-bind^ lhs* rhs* 
             (make-primcall op arg*))))]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(appcall op arg*)
       (make-appcall (Expr op) (map Expr arg*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(return v) (make-return (Expr v))]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Tail body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Tail e1))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(appcall op arg*)
       (make-appcall (Expr op) (map Expr arg*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (CodeExpr x)
    (record-case x 
      [(code fml* proper free* body)
       (make-code fml* proper free* (Tail body))]))
  (define (CodesExpr x)
    (record-case x 
      [(codes lhs* rhs* body)
       (make-codes lhs* (map CodeExpr rhs*) (Tail body))]))
  (define (ConstantsExpr x)
    (record-case x 
      [(constants lhs* body)
       (make-constants lhs* (CodesExpr body))]))
  (ConstantsExpr x))


(define (insert-stack-overflow-checks x)
  (define who 'insert-stack-overflow-checks)
  (define (insert-check body)
    (make-seq
      (make-conditional 
        (make-primcall '$fp-overflow '())
        (make-funcall (make-primref 'do-stack-overflow) '())
        (make-primcall 'void '()))
      body))
  (define (Expr x)
    (record-case x
      [(constant) #f]
      [(constant-loc) #f]
      [(var) #f]
      [(primref) #f]
      [(closure code free*) #f]
      [(bind lhs* rhs* body)
       (or (ormap Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (or (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (or (Expr e0) (Expr e1))]
      [(primcall op arg*) (ormap Expr arg*)]
      [(forcall op arg*)  (ormap Expr arg*)]
      [(funcall rator arg*) #t] 
      [(appcall rator arg*)    #t]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(return v) (Expr v)]
      [(bind lhs* rhs* body)
       (or (ormap Expr rhs*) (Tail body))]
      [(conditional test conseq altern)
       (or (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (or (Expr e0) (Tail e1))]
      [(funcall rator arg*) (or (Expr rator) (ormap Expr arg*))] 
      [(appcall rator arg*) (or (Expr rator) (ormap Expr arg*))]
      [else (error who "invalid tail expression ~s" (unparse x))]))
  (define (CodeExpr x)
    (record-case x 
      [(code fml* proper free* body)
       (if (Tail body)
           (make-code fml* proper free* 
             (insert-check body))
           x)]))
  (define (CodesExpr x)
    (record-case x 
      [(codes lhs* rhs* body)
       (make-codes lhs* (map CodeExpr rhs*) (insert-check body))]))
  (define (ConstantsExpr x)
    (record-case x 
      [(constants lhs* body)
       (make-constants lhs* (CodesExpr body))]))
  (ConstantsExpr x))


(define (insert-allocation-checks x)
  (define who 'insert-allocation-checks)
  (define (check-bytes n var body)
    (make-seq
      (make-conditional 
        (make-primcall '$ap-check-bytes
          (list (make-constant n) var))
        (make-funcall (make-primref 'do-overflow) '())
        (make-primcall 'void '()))
      body))
  (define (check-words n var body)
    (make-seq
      (make-conditional 
        (make-primcall '$ap-check-words
          (list (make-constant n) var))
        (make-funcall (make-primref 'do-overflow) '())
        (make-primcall 'void '()))
      body))
  (define (check-const n body)
    (make-seq
      (make-conditional 
        (make-primcall '$ap-check-const
          (list (make-constant n)))
        (make-funcall (make-primref 'do-overflow) '())
        (make-primcall 'void '()))
      body))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(constant-loc) x]
      [(var) x]
      [(primref) x]
      [(closure code free*) 
       (check-const (+ disp-closure-data (* (length free*) wordsize)) x)]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(primcall op arg*)
       (let ([x (make-primcall op (map Expr arg*))])
         (case op
           [(cons) (check-const pair-size x)]
           [($make-symbol) (check-const symbol-size x)]
           [($frame->continuation $code->closure) 
            (check-const (+ disp-closure-data (* (length arg*) wordsize)) x)]
           [($make-string) 
            (record-case (car arg*)
              [(constant i)
               (check-const (+ i disp-string-data 1) x)]
              [else
               (check-bytes (add1 disp-string-data) (car arg*) x)])]
           [($string)
            (check-const (+ (length arg*) disp-string-data 1) x)]
           [($make-vector) 
            (record-case (car arg*)
              [(constant i)
               (check-const (+ (* i wordsize) disp-vector-data) x)]
              [else
               (check-words (add1 disp-vector-data) (car arg*) x)])]
           [($make-record) 
            (record-case (cadr arg*)
              [(constant i)
               (check-const (+ (* i wordsize) disp-record-data) x)]
              [else
               (check-words (add1 disp-record-data) (cadr arg*) x)])]
           [(vector)
            (check-const (+ (* (length arg*) wordsize) disp-vector-data) x)]
           [else x]))]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(appcall op arg*)
       (make-appcall (Expr op) (map Expr arg*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(return v) (make-return (Expr v))]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Tail body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Tail e1))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(appcall op arg*)
       (make-appcall (Expr op) (map Expr arg*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (CodeExpr x)
    (record-case x 
      [(code fml* proper free* body)
       (make-code fml* proper free* (Tail body))]))
  (define (CodesExpr x)
    (record-case x 
      [(codes lhs* rhs* body)
       (make-codes lhs* (map CodeExpr rhs*) (Tail body))]))
  (define (ConstantsExpr x)
    (record-case x 
      [(constants lhs* body)
       (make-constants lhs* (CodesExpr body))]))
  (ConstantsExpr x))


(define (remove-local-variables x)
  (define who 'remove-local-variables)
  (define (simple* x* r)
    (map (lambda (x) 
           (cond
             [(assq x r) => cdr]
             [else
               (when (var? x) (error who "unbound var ~s" x))
               x]))
          x*))
  (define (env->mask r sz)
    (let ([s (make-vector (fxsra (+ sz 7) 3) 0)])
      (for-each 
        (lambda (idx) 
           (let ([q (fxsra idx 3)]
                 [r (fxlogand idx 7)])
             (vector-set! s q 
               (fxlogor (vector-ref s q) (fxsll 1 r)))))
        r)
      s))
  (define (do-new-frame op rand* si r call-convention rp-convention orig-live)
    (make-new-frame (add1 si) (+ (length rand*) 2)
      (let f ([r* rand*] [nsi (+ si 2)] [live orig-live])
        (cond
          [(null? r*) 
           (make-seq
             (make-seq 
               (make-save-cp (make-frame-var si))
               (case call-convention
                 [(normal apply)
                  (make-eval-cp #t (Expr op nsi r (cons si live)))]
                 [(foreign)
                  (make-eval-cp #f (make-foreign-label op))]
                 [else (error who "invalid convention ~s" convention)]))
             (make-call-cp call-convention 
                           rp-convention
                           (add1 si) 
                           (length rand*) 
                           (env->mask (cons si orig-live)
                                      (add1 si))))]
          [else
            (make-seq 
              (make-assign (make-frame-var nsi)
                (Expr (car r*) nsi r live))
              (f (cdr r*) (add1 nsi) (cons nsi live)))]))))
  (define (nop) (make-primcall 'void '()))
  (define (do-bind lhs* rhs* body si r live k)
    (let f ([lhs* lhs*] [rhs* rhs*] [si si] [nr r] [live live])
      (cond
        [(null? lhs*) (k body si nr live)]
        [else
         (let ([v (make-frame-var si)])
           (make-seq
             (make-assign v (Expr (car rhs*) si r live))
             (f (cdr lhs*) (cdr rhs*) (add1 si)
                (cons (cons (car lhs*) v) nr)
                (cons si live))))])))
  (define (Tail x si r live)
    (record-case x
      [(return v) (make-return (Expr v si r live))]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body si r live Tail)]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test si r live)
         (Tail conseq si r live) 
         (Tail altern si r live))]
      [(seq e0 e1) (make-seq (Effect e0 si r live) (Tail e1 si r live))]
      [(primcall op arg*)
       (case op
;         [(values) (make-primcall op (simple* arg* r))]
         [else (make-return (make-primcall op (simple* arg* r)))])]
      [(funcall op rand*)
       (do-new-frame op rand* si r 'normal 'tail live)]
      [(appcall op rand*)
       (do-new-frame op rand* si r 'apply 'tail live)]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Effect x si r live)
    (record-case x
      [(constant) (nop)]
      [(constant-loc) (nop)]
      [(var) (nop)] 
      [(primref) (nop)]
      [(closure code free*) (nop)]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body si r live Effect)]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test si r live)
         (Effect conseq si r live) 
         (Effect altern si r live))]
      [(seq e0 e1) (make-seq (Effect e0 si r live) (Effect e1 si r live))]
      [(primcall op arg*)
       (make-primcall op (simple* arg* r))]
      [(forcall op rand*)
       (do-new-frame op rand* si r 'foreign 'effect live)]
      [(funcall op rand*)
       (do-new-frame op rand* si r 'normal 'effect live)]
      [(appcall op rand*)
       (do-new-frame op rand* si r 'apply 'effect live)]
      [else (error who "invalid effect expression ~s" (unparse x))]))
  (define (Expr x si r live)
    (record-case x
      [(constant) x]
      [(constant-loc) x]
      [(var) 
       (cond
         [(assq x r) => cdr]
         [else (error who "unbound var ~s" x)])]
      [(primref) x]
      [(closure code free*) 
       (make-closure code (simple* free* r))]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body si r live Expr)]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test si r live)
         (Expr conseq si r live) 
         (Expr altern si r live))]
      [(seq e0 e1) (make-seq (Effect e0 si r live) (Expr e1 si r live))]
      [(primcall op arg*)
       (make-primcall op (simple* arg* r))]
      [(forcall op rand*)
       (do-new-frame op rand* si r 'foreign 'value live)]
      [(funcall op rand*)
       (do-new-frame op rand* si r 'normal 'value live)]
      [(appcall op rand*)
       (do-new-frame op rand* si r 'apply 'value live)]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (bind-fml* fml* r)
    (let f ([si 1] [fml* fml*])
      (cond
        [(null? fml*) (values '() si r '())]
        [else
          (let-values ([(nfml* nsi r live) (f (add1 si) (cdr fml*))])
            (let ([v (make-frame-var si)])
              (values (cons v nfml*)
                      nsi
                      (cons (cons (car fml*) v) r)
                      (cons si live))))])))
  (define (bind-free* free*)
    (let f ([free* free*] [idx 0] [r '()])
      (cond
        [(null? free*) r]
        [else
          (f (cdr free*) (add1 idx)
             (cons (cons (car free*) (make-cp-var idx)) r))])))
  (define (CodeExpr x)
    (record-case x 
      [(code fml* proper free* body)
       (let-values ([(fml* si r live) (bind-fml* fml* (bind-free* free*))])
         (make-code fml* proper free* (Tail body si r live)))]))
  (define (CodesExpr x)
    (record-case x 
      [(codes lhs* rhs* body)
       (make-codes lhs* (map CodeExpr rhs*) (Tail body 1 '() '()))]))
  (define (ConstantsExpr x)
    (record-case x 
      [(constants lhs* body)
       (make-constants lhs* (CodesExpr body))]))
  (ConstantsExpr x))
         

(begin
  (define fx-shift 2)
  (define fx-mask #x03)
  (define fx-tag 0)
  (define bool-f #x2F)
  (define bool-t #x3F)
  (define bool-mask #xEF)
  (define bool-tag bool-f)
  (define bool-shift 4)
  (define nil     #x4F)
  (define eof     #x5F) ; double check
  (define unbound #x6F) ; double check
  (define void-object #x7F) ; double check
  (define wordsize  4)
  (define char-shift 8)
  (define char-tag #x0F)
  (define char-mask #xFF)
  (define pair-mask 7)
  (define pair-tag  1)
  (define disp-car  0)
  (define disp-cdr  4)
  (define pair-size 8)
  
  (define symbol-mask 7)
  (define symbol-tag 2)
  (define disp-symbol-string         0)
  (define disp-symbol-unique-string  4)
  (define disp-symbol-value          8)
  (define disp-symbol-plist         12)
  (define symbol-size 16)
  
  (define vector-tag 5)
  (define vector-mask 7)
  (define disp-vector-length 0)
  (define disp-vector-data 4)
  (define string-mask 7)
  (define string-tag 6)
  (define disp-string-length 0)
  (define disp-string-data 4)
  (define closure-mask 7)
  (define closure-tag 3)
  (define disp-closure-data 4)
  (define disp-closure-code 0)
  (define continuation-size       16)
  (define continuation-tag      #x1F)
  (define disp-continuation-top    4)
  (define disp-continuation-size   8)
  (define disp-continuation-next  12)
  (define code-tag              #x2F)
  (define disp-code-instrsize       4)
  (define disp-code-relocsize      8)
  (define disp-code-closuresize   12)
  (define disp-code-data          16)

  (define record-ptag vector-tag)
  (define record-pmask vector-mask)
  (define disp-record-rtd     0)
  (define disp-record-data    4)

  (define disp-frame-size -13)
  (define disp-multivalue-rp -9)
  (define object-alignment 8)
  (define align-shift 3)
  (define pagesize 4096))

(begin
  (trace-define (mem off val) (list 'mem off val))
  (trace-define (int x) (list 'integer x))
  (trace-define (byte x) (list 'byte x))
  (trace-define (byte-vector x) (list 'byte-vector x))
  (trace-define (movzbl src targ) (list 'movzbl src targ))
  (trace-define (sall src targ) (list 'sall src targ))
  (trace-define (sarl src targ) (list 'sarl src targ))
  (trace-define (shll src targ) (list 'shll src targ))
  (trace-define (shrl src targ) (list 'shrl src targ))
  (trace-define (notl src) (list 'notl src))
  (trace-define (pushl src) (list 'pushl src))
  (trace-define (popl src) (list 'popl src))
  (trace-define (orl src targ) (list 'orl src targ))
  (trace-define (xorl src targ) (list 'xorl src targ))
  (trace-define (andl src targ) (list 'andl src targ))
  (trace-define (movl src targ) (list 'movl src targ))
  (trace-define (movb src targ) (list 'movb src targ))
  (trace-define (addl src targ) (list 'addl src targ))
  (trace-define (imull src targ) (list 'imull src targ))
  (trace-define (idivl src) (list 'idivl src))
  (trace-define (subl src targ) (list 'subl src targ))
  (trace-define (push src) (list 'push src))
  (trace-define (pop targ) (list 'pop targ))
  (trace-define (sete targ) (list 'sete targ))
  (trace-define (call targ) (list 'call targ))
  (trace-define (tail-indirect-cpr-call) (jmp (list 'indirect (mem (- disp-closure-code closure-tag) cpr))))
  (trace-define (indirect-cpr-call) (call (list 'indirect (mem (- disp-closure-code closure-tag) cpr))))
  (trace-define (negl targ) (list 'negl targ))
  (trace-define (label x) (list 'label x))
  (trace-define (label-address x) (list 'label-address x))
  (trace-define (ret) '(ret))
  (trace-define (cltd) '(cltd))
  (trace-define (cmpl arg1 arg2) (list 'cmpl arg1 arg2))
  (trace-define (je label) (list 'je label))
  (trace-define (jne label) (list 'jne label))
  (trace-define (jle label) (list 'jle label))
  (trace-define (jge label) (list 'jge label))
  (trace-define (jg label) (list 'jg label))
  (trace-define (jl label) (list 'jl label))
  (trace-define (jb label) (list 'jb label))
  (trace-define (ja label) (list 'ja label))
  (trace-define (jmp label) (list 'jmp label))
  (define edi '(register %edi)) ; closure pointer
  (define esi '(register %esi)) ; pcb
  (define ebp '(register %ebp)) ; allocation pointer
  (define esp '(register %esp)) ; stack base pointer 
  (define al '(register %al))
  (define ah '(register %ah))
  (define bh '(register %bh))
  (define cl '(register %cl))
  (define eax '(register %eax))
  (define ebx '(register %ebx))
  (define ecx '(register %ecx))
  (define edx '(register %edx))
  (define apr '(register %ebp))
  (define fpr '(register %esp))
  (define cpr '(register %edi))
  (define pcr '(register %esi))

  (define (argc-convention n)
    (- (fxsll n fx-shift)))
  )

(begin
  (define (mem off val) (list 'mem off val))
  (define (int x) (list 'integer x))
  (define (byte x) (list 'byte x))
  (define (byte-vector x) (list 'byte-vector x))
  (define (movzbl src targ) (list 'movzbl src targ))
  (define (sall src targ) (list 'sall src targ))
  (define (sarl src targ) (list 'sarl src targ))
  (define (shll src targ) (list 'shll src targ))
  (define (shrl src targ) (list 'shrl src targ))
  (define (notl src) (list 'notl src))
  (define (pushl src) (list 'pushl src))
  (define (popl src) (list 'popl src))
  (define (orl src targ) (list 'orl src targ))
  (define (xorl src targ) (list 'xorl src targ))
  (define (andl src targ) (list 'andl src targ))
  (define (movl src targ) (list 'movl src targ))
  (define (movb src targ) (list 'movb src targ))
  (define (addl src targ) (list 'addl src targ))
  (define (imull src targ) (list 'imull src targ))
  (define (idivl src) (list 'idivl src))
  (define (subl src targ) (list 'subl src targ))
  (define (push src) (list 'push src))
  (define (pop targ) (list 'pop targ))
  (define (sete targ) (list 'sete targ))
  (define (call targ) (list 'call targ))
  (define (tail-indirect-cpr-call) (jmp (list 'indirect (mem (- disp-closure-code closure-tag) cpr))))
  (define (indirect-cpr-call) (call (list 'indirect (mem (- disp-closure-code closure-tag) cpr))))
  (define (negl targ) (list 'negl targ))
  (define (label x) (list 'label x))
  (define (label-address x) (list 'label-address x))
  (define (ret) '(ret))
  (define (cltd) '(cltd))
  (define (cmpl arg1 arg2) (list 'cmpl arg1 arg2))
  (define (je label) (list 'je label))
  (define (jne label) (list 'jne label))
  (define (jle label) (list 'jle label))
  (define (jge label) (list 'jge label))
  (define (jg label) (list 'jg label))
  (define (jl label) (list 'jl label))
  (define (jb label) (list 'jb label))
  (define (ja label) (list 'ja label))
  (define (jmp label) (list 'jmp label))
  (define edi '(register %edi)) ; closure pointer
  (define esi '(register %esi)) ; pcb
  (define ebp '(register %ebp)) ; allocation pointer
  (define esp '(register %esp)) ; stack base pointer 
  (define al '(register %al))
  (define ah '(register %ah))
  (define bh '(register %bh))
  (define bl '(register %bl))
  (define cl '(register %cl))
  (define eax '(register %eax))
  (define ebx '(register %ebx))
  (define ecx '(register %ecx))
  (define edx '(register %edx))
  (define apr '(register %ebp))
  (define fpr '(register %esp))
  (define cpr '(register %edi))
  (define pcr '(register %esi))

  (define (argc-convention n)
    (- (fxsll n fx-shift)))
  )

(define (generate-code x main-name)
  (define who 'generate-code)
  (define (rp-label x)
    (case x
      [(value) (label-address "SL_multiple_values_error_rp")]
      [(effect) (label-address "SL_multiple_values_ignore_rp")]
      [else (error who "invalid rp-convention ~s" x)]))
  (define (align n)
    (fxsll (fxsra (+ n object-alignment -1) align-shift) align-shift))
  (define unique-label
    (let ([count 0])
      (lambda ()
        (let ([L (format "L_~a_~a" main-name count)])
          (set! count (add1 count))
          (label L)))))
  (define (constant-val x)
    (cond
      [(fixnum? x) (int (ash x fx-shift))]
      [(boolean? x) (int (if x bool-t bool-f))]
      [(null? x) (int nil)]
      [(char? x) (int (+ (ash (char->integer x) char-shift) char-tag))]
      [(eq? x (void)) (int void-object)]
      [else (error 'constant-val "invalid immcprate ~s" x)]))
  (define (primref-loc op)
    (mem (* (pcb-index op) wordsize) pcr))
  (define (immediate-rep x)
    (cond
      [(fixnum? x) (ash x fx-shift)]
      [(boolean? x) (if x bool-t bool-f)]
      [(null? x) nil]
      [(char? x) (+ (ash (char->integer x) char-shift) char-tag)]
      [else (error 'immediate-rep "invalid immediate ~s" x)]))
;;;  (define (bool-bit-to-boolean ac)
;;;    (list* 
;;;      (movzbl al eax)
;;;      (shll (int bool-shift) eax)
;;;      (orl (int bool-tag) eax)
;;;      ac))
  (define (cond-branch op Lt Lf ac)
    (define (opposite x)
      (cadr (assq x '([je jne] [jl jge] [jle jg] [jg jle] [jge jl]))))
    (unless (or Lt Lf)
      (error 'cond-branch "no labels"))
    (cond
      [(not Lf) (cons (list op Lt) ac)]
      [(not Lt) (cons (list (opposite op) Lf) ac)] 
      [else (list* (list op Lt) (jmp Lf) ac)]))
  (define (indirect-type-pred pri-mask pri-tag sec-mask sec-tag rand* Lt Lf ac)
    (cond
      [(and Lt Lf)
       (list* (movl (Simple (car rand*)) eax)
              (movl eax ebx)
              (andl (int pri-mask) ebx)
              (cmpl (int pri-tag) ebx)
              (jne Lf)
              (movl (mem (- pri-tag) eax) ebx)
              (if sec-mask
                  (andl (int sec-mask) ebx)
                  '(nop))
              (cmpl (int sec-tag) ebx)
              (jne Lf)
              (jmp Lt)
              ac)]
      [Lf
       (list* (movl (Simple (car rand*)) eax)
              (movl eax ebx)
              (andl (int pri-mask) ebx)
              (cmpl (int pri-tag) ebx)
              (jne Lf)
              (movl (mem (- pri-tag) eax) ebx)
              (if sec-mask
                  (andl (int sec-mask) ebx)
                  '(nop))
              (cmpl (int sec-tag) ebx)
              (jne Lf)
              ac)]
      [Lt
       (let ([L_END (unique-label)])
         (list* (movl (Simple (car rand*)) eax)
                (movl eax ebx)
                (andl (int pri-mask) ebx)
                (cmpl (int pri-tag) ebx)
                (jne L_END)
                (movl (mem (- pri-tag) eax) ebx)
                (if sec-mask
                    (andl (int sec-mask) ebx)
                    '(nop))
                (cmpl (int sec-tag) ebx)
                (je Lt)
                L_END
                ac))]
      [else ac]))
  (define (type-pred mask tag rand* Lt Lf ac)
    (cond
      [mask
       (list* 
         (movl (Simple (car rand*)) eax)
         (andl (int mask) eax)
         (cmpl (int tag) eax)
         (cond-branch 'je Lt Lf ac))]
      [else 
       (let ([v (Simple (car rand*))])
         (cond
           [(memq (car v) '(mem register)) 
            (list*
              (cmpl (int tag) (Simple (car rand*)))
              (cond-branch 'je Lt Lf ac))]
           [else
            (list*
              (movl (Simple (car rand*)) eax)
              (cmpl (int tag) eax)
              (cond-branch 'je Lt Lf ac))]))])) 
  (define (compare-and-branch op rand* Lt Lf ac)
    (define (opposite x)
      (cadr (assq x '([je je] [jl jg] [jle jge] [jg jl] [jge jle]))))
    (cond
     [(constant? (cadr rand*))
      (list* 
        (cmpl (Simple (cadr rand*)) (Simple (car rand*)))
        (cond-branch op Lt Lf ac))]
     [(constant? (car rand*))
      (list*
        (cmpl (Simple (car rand*)) (Simple (cadr rand*)))
        (cond-branch (opposite op) Lt Lf ac))]
     [else
      (list* 
        (movl (Simple (car rand*)) eax)
        (cmpl (Simple (cadr rand*)) eax)
        (cond-branch op Lt Lf ac))]))
  (define (do-pred-prim op rand* Lt Lf ac)
    (case op
     [(fixnum?)    (type-pred fx-mask fx-tag rand* Lt Lf ac)]
     [(pair?)      (type-pred pair-mask pair-tag rand* Lt Lf ac)]
     [(char?)      (type-pred char-mask char-tag rand* Lt Lf ac)]
     [(string?)    (type-pred string-mask string-tag rand* Lt Lf ac)]
     [(symbol?)    (type-pred symbol-mask symbol-tag rand* Lt Lf ac)]
     [(procedure?) (type-pred closure-mask closure-tag rand* Lt Lf ac)]
     [(boolean?)   (type-pred bool-mask bool-tag rand* Lt Lf ac)]
     [(null?)      (type-pred #f nil rand* Lt Lf ac)]
     [($unbound-object?) (type-pred #f unbound rand* Lt Lf ac)]
     [(not)        (type-pred #f bool-f rand* Lt Lf ac)]
     [(eof-object?) (type-pred #f eof rand* Lt Lf ac)]
     [($fxzero?)   (type-pred #f 0 rand* Lt Lf ac)]
     [($fx= $char= eq?) (compare-and-branch 'je rand* Lt Lf ac)]
     [($fx< $char<)     (compare-and-branch 'jl rand* Lt Lf ac)]
     [($fx<= $char<=)   (compare-and-branch 'jle rand* Lt Lf ac)]
     [($fx> $char>)     (compare-and-branch 'jg rand* Lt Lf ac)]
     [($fx>= $char>=)   (compare-and-branch 'jge rand* Lt Lf ac)]
     [(vector?) 
      (indirect-type-pred vector-mask vector-tag fx-mask fx-tag 
         rand* Lt Lf ac)]
     [($record?) 
      (indirect-type-pred record-pmask record-ptag record-pmask record-ptag
         rand* Lt Lf ac)]
     [(code?) 
      (indirect-type-pred vector-mask vector-tag #f code-tag 
         rand* Lt Lf ac)]
     [(immediate?)
      (cond
        [(and Lt Lf)
         (list* (movl (Simple (car rand*)) eax)
             (movl eax ebx)
             (andl (int fx-mask) ebx)
             (cmpl (int 0) ebx)
             (je Lt)
             (andl (int 7) eax)
             (cmpl (int 7) eax)
             (je Lt)
             (jmp Lf)
             ac)]
        [Lt
         (list* (movl (Simple (car rand*)) eax)
             (movl eax ebx)
             (andl (int fx-mask) ebx)
             (cmpl (int 0) ebx)
             (je Lt)
             (andl (int 7) eax)
             (cmpl (int 7) eax)
             (je Lt)
             ac)]
        [Lf
         (let ([Ljoin (unique-label)])
           (list* 
             (movl (Simple (car rand*)) eax)
             (movl eax ebx)
             (andl (int fx-mask) ebx)
             (cmpl (int 0) ebx)
             (je Ljoin)
             (andl (int 7) eax)
             (cmpl (int 7) eax)
             (jne Lf)
             Ljoin
             ac))]
        [else ac])]
     [($ap-check-words)
      (record-case (car rand*)
        [(constant i)
         (list* (movl (primref-loc '$allocation-redline) eax)
                (subl (Simple (cadr rand*)) eax)
                (subl (int i) eax)
                (cmpl eax apr)
                (cond-branch 'jge Lt Lf ac))]
        [else (error who "ap-check-words")])]
     [($ap-check-bytes)
      (record-case (car rand*)
        [(constant i)
         (list* (movl (Simple (cadr rand*)) eax)
                (negl eax)
                (addl (primref-loc '$allocation-redline) eax)
                (subl (int i) eax)
                (cmpl eax apr)
                (cond-branch 'jge Lt Lf ac))]
        [else (error who "ap-check-bytes")])]
     [($ap-check-const) 
      (record-case (car rand*)
        [(constant i) 
         (if (< i pagesize)
             (list* 
               (cmpl (primref-loc '$allocation-redline) apr)
               (cond-branch 'jge Lt Lf ac))
             (list* 
               (movl (primref-loc '$allocation-redline) eax)
               (subl (int i) eax)
               (cmpl eax apr)
               (cond-branch 'jge Lt Lf ac)))]
        [else (error who "ap-check-const")])]
     [($fp-at-base) 
      (list* (cmpl (mem (pcb-offset '$frame-base) pcr) fpr)
             (cond-branch 'je Lt Lf ac))]
     [($fp-overflow) 
      (list* (cmpl (mem (pcb-offset '$frame-redline) pcr) fpr)
             (cond-branch 'jle Lt Lf ac))]
     [($vector-ref) 
      (do-value-prim op rand*
        (do-simple-test eax Lt Lf ac))]
     [(cons void)
      ;;; always true
      (do-effect-prim op rand*
        (cond
          [(not Lt) ac]
          [else (cons (jmp Lt) ac)]))]
     [else 
      (error 'pred-prim "HERE unhandled ~s" op)]))
  (define (do-pred->value-prim op rand* ac)
    (case op
      [else
       (let ([Lf (unique-label)] [Lj (unique-label)])
         (do-pred-prim op rand* #f Lf
           (list* (movl (constant-val #t) eax)
                  (jmp Lj)
                  Lf
                  (movl (constant-val #f) eax)
                  Lj
                  ac)))]))
  (define (indirect-ref arg* off ac)
    (list*
      (movl (Simple (car arg*)) eax)
      (movl (mem off eax) eax)
      ac))
  (define (do-value-prim op arg* ac)
    (case op
      [(eof-object) (cons (movl (int eof) eax) ac)]
      [(void)       (cons (movl (int void-object) eax) ac)]
      [($fxadd1) 
       (list* (movl (Simple (car arg*)) eax)
              (addl (constant-val 1) eax)
              ac)]
      [($fxsub1) 
       (list* (movl (Simple (car arg*)) eax)
              (addl (constant-val -1) eax)
              ac)]
      [($fx+) 
       (list* (movl (Simple (car arg*)) eax)
              (addl (Simple (cadr arg*)) eax)
              ac)]
      [($fx-) 
       (list* (movl (Simple (car arg*)) eax)
              (subl (Simple (cadr arg*)) eax)
              ac)]
      [($fx*)
       (cond
         [(constant? (car arg*))
          (record-case (car arg*)
            [(constant c)
             (unless (integer? c)
               (error who "invalid arg ~s to fx*" c))
             (list* (movl (Simple (cadr arg*)) eax)
                    (imull (int c) eax)
                    ac)])]
         [(constant? (cadr arg*))
          (record-case (cadr arg*)
            [(constant c)
             (unless (integer? c)
               (error who "invalid arg ~s to fx*" c))
             (list* (movl (Simple (car arg*)) eax)
                    (imull (int c) eax)
                    ac)])]
         [else
          (list* (movl (Simple (car arg*)) eax)
                 (shrl (int fx-shift) eax)
                 (imull (simple (cadr arg*)) eax)
                 ac)])]
      [($fxquotient)
       (list* (movl (Simple (car arg*)) eax)
              (cltd)
              (idivl (Simple (cadr arg*)))
              (sall (int fx-shift) eax)
              ac)]
      [($fxmodulo)
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl eax ecx)
              (xorl ebx ecx)
              (sarl (int (sub1 (* wordsize 8))) ecx)
              (andl ebx ecx)
              (cltd)
              (idivl ebx)
              (movl edx eax)
              (addl ecx eax)
              ac)]
      [($fxlogor) 
       (list* (movl (Simple (car arg*)) eax)
              (orl (Simple (cadr arg*)) eax)
              ac)]
      [($fxlogand) 
       (list* (movl (Simple (car arg*)) eax)
              (andl (Simple (cadr arg*)) eax)
              ac)]
      [($fxlogxor) 
       (list* (movl (Simple (car arg*)) eax)
              (xorl (Simple (cadr arg*)) eax)
              ac)]
      [($fxsra)
       (record-case (cadr arg*)
         [(constant i)
          (unless (fixnum? i) (error who "invalid arg to fxsra"))
          (list* (movl (Simple (car arg*)) eax)
                 (sarl (int (+ i fx-shift)) eax)
                 (sall (int fx-shift) eax)
                 ac)]
         [else
          (list* (movl (Simple (car arg*)) eax)
                 (movl (Simple (cadr arg*)) ecx)
                 (sarl (int fx-shift) ecx)
                 (sarl (int fx-shift) eax)
                 (sarl cl eax)
                 (sall (int fx-shift) eax)
                 ac)])]
      [($fxsll)
       (record-case (cadr arg*)
         [(constant i)
          (unless (fixnum? i) (error who "invalid arg to fxsll"))
          (list* (movl (Simple (car arg*)) eax)
                 (sall (int i) eax)
                 ac)]
         [else
          (list* (movl (Simple (car arg*)) eax)
                 (movl (Simple (cadr arg*)) ecx)
                 (sarl (int fx-shift) ecx)
                 (sall cl eax)
                 ac)])]
      [($fixnum->char)  
       (list* (movl (Simple (car arg*)) eax)
              (shll (int (- char-shift fx-shift)) eax)
              (orl (int char-tag) eax)
              ac)]
      [($char->fixnum) 
       (list* (movl (Simple (car arg*)) eax)
              (shrl (int (- char-shift fx-shift)) eax)
              ac)]
      [($fxlognot) 
       (list* (movl (Simple (car arg*)) eax)
              (orl (int fx-mask) eax)
              (notl eax)
              ac)]
      [($car) (indirect-ref arg* (- disp-car pair-tag) ac)]
      [($cdr) (indirect-ref arg* (- disp-cdr pair-tag) ac)]
      [($vector-length) 
       (indirect-ref arg* (- disp-vector-length vector-tag) ac)]
      [($string-length) 
       (indirect-ref arg* (- disp-string-length string-tag) ac)]
      [($symbol-string) 
       (indirect-ref arg* (- disp-symbol-string symbol-tag) ac)]
      [($symbol-unique-string) 
       (indirect-ref arg* (- disp-symbol-unique-string symbol-tag) ac)]
      [($symbol-value) 
       (indirect-ref arg* (- disp-symbol-value symbol-tag) ac)]
      [($symbol-plist) 
       (indirect-ref arg* (- disp-symbol-plist symbol-tag) ac)]
      [($record-rtd) 
       (indirect-ref arg* (- disp-record-rtd record-ptag) ac)]
      [($constant-ref)
       (list* (movl (Simple (car arg*)) eax) ac)]
      [($vector-ref) 
       (list* (movl (Simple (car arg*)) ebx)
              (addl (Simple (cadr arg*)) ebx)
              (movl (mem (- disp-vector-data vector-tag) ebx) eax)
              ac)]
      [($record-ref) 
       (list* (movl (Simple (car arg*)) ebx)
              (addl (Simple (cadr arg*)) ebx)
              (movl (mem (- disp-record-data record-ptag) ebx) eax)
              ac)]
      [($string-ref) 
       (list* (movl (Simple (cadr arg*)) ebx)
              (shrl (int fx-shift) ebx)
              (addl (Simple (car arg*)) ebx)
              (movl (int char-tag) eax)
              (movb (mem (- disp-string-data string-tag) ebx) ah)
              ac)]
      [($make-string) 
       (list* (movl (Simple (car arg*)) eax)
              (movl eax (mem disp-string-length apr))
              (movl eax ebx)
              (movl apr eax)
              (addl (int string-tag) eax)
              (sarl (int fx-shift) ebx)
              (addl ebx apr)
              (addl (int (+ disp-string-data object-alignment)) apr)
              (sarl (int align-shift) apr)
              (sall (int align-shift) apr)
              ac)]
      [($make-vector) 
       (list* (movl (Simple (car arg*)) eax)
              (movl eax (mem disp-vector-length apr))
              (movl apr eax)
              (addl (int vector-tag) eax)
              (addl (mem disp-vector-length apr) apr)
              (addl (int (+ disp-vector-data object-alignment -1)) apr)
              (sarl (int align-shift) apr)
              (sall (int align-shift) apr)
              ac)]
      [($make-record) 
       (list* (movl (Simple (car arg*)) eax)
              (movl eax (mem disp-record-rtd apr))
              (movl apr eax)
              (addl (int record-ptag) eax)
              (addl (Simple (cadr arg*)) apr)
              (addl (int (+ disp-record-data object-alignment -1)) apr)
              (sarl (int align-shift) apr)
              (sall (int align-shift) apr)
              ac)]
      [(cons)
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl eax (mem disp-car apr))
              (movl apr eax)
              (movl ebx (mem disp-cdr apr))
              (addl (int pair-tag) eax)
              (addl (int (align pair-size)) apr)
              ac)]
      [($make-symbol)
       (list* (movl (Simple (car arg*)) eax)
              (movl (int unbound) (mem disp-symbol-value apr))
              (movl (int nil) (mem disp-symbol-plist apr))
              (movl (int 0) (mem disp-symbol-unique-string apr))
              (movl eax (mem disp-symbol-string apr))
              (movl apr eax)
              (addl (int symbol-tag) eax)
              (addl (int (align symbol-size)) apr)
              ac)]
      [(vector) 
       (let f ([arg* arg*] [idx disp-vector-data])
         (cond
           [(null? arg*) 
            (list* (movl apr eax)
                   (addl (int vector-tag) eax)
                   (movl (int (- idx disp-vector-data))
                         (mem disp-vector-length apr))
                   (addl (int (align idx)) apr)
                   ac)]
           [else 
            (list* (movl (Simple (car arg*)) eax)
                   (movl eax (mem idx apr))
                   (f (cdr arg*) (+ idx wordsize)))]))]
     [($pcb-ref)
      (let ([loc (car arg*)])
        (record-case loc
          [(constant i) 
           (unless (fixnum? i) (error who "invalid loc ~s" loc))
           (list* (movl (mem (* i wordsize) pcr) eax) ac)]
          [else (error who "invalid loc ~s" loc)]))]
     [($string)
      (let f ([arg* arg*] [idx disp-string-data])
        (cond
          [(null? arg*) 
           (list* (movl apr eax)
                  (addl (int string-tag) eax)
                  (movl (int (* (- idx disp-string-data) wordsize))
                        (mem disp-string-length apr))
                  (addl (int (align (add1 idx))) apr)
                  ac)]
          [else
           (record-case (car arg*)
             [(constant c) 
              (unless (char? c) (error who  "invalid arg to string ~s" x))
              (list* (movb (int (char->integer c)) (mem idx apr))
                     (f (cdr arg*) (add1 idx)))]
             [else
              (list* (movl (Simple (car arg*)) ebx)
                     (movb bh (mem idx apr))
                     (f (cdr arg*) (add1 idx)))])]))]
     [($current-frame)
      (list* (movl (mem (pcb-offset '$next-continuation) pcr) eax)
             ac)]
     [($seal-frame-and-call) 
      (list* (movl (Simple (car arg*)) cpr) ; proc
             (movl (mem (pcb-offset '$frame-base) pcr) eax)
             ; eax=baseofstack
             (movl (mem 0 eax) ebx) ; underflow handler
             (movl ebx (mem (- wordsize) fpr)) ; set
             ; create a new cont record
             (movl (int continuation-tag) (mem 0 apr))
             (movl fpr (mem disp-continuation-top apr))
             ; compute the size of the captured frame
             (movl eax ebx) 
             (subl fpr ebx) 
             ; and store it
             (movl ebx (mem disp-continuation-size apr))
             ; load next cont
             (movl (mem (pcb-offset '$next-continuation) pcr) ebx)
             ; and store it
             (movl ebx (mem disp-continuation-next apr))
             ; adjust ap
             (movl apr eax)
             (addl (int vector-tag) eax)
             (addl (int continuation-size) apr)
             ; store new cont in current-cont
             (movl eax (mem (pcb-offset '$next-continuation) pcr))
             ; adjust fp
             (subl (int wordsize) fpr)
             (movl fpr (mem (pcb-offset '$frame-base) pcr))
             ; tail-call f
             (movl eax (mem (- wordsize) fpr))
             (movl (int (argc-convention 1)) eax)
             (tail-indirect-cpr-call)
             ac)]
     [($code-instr-size) 
      (indirect-ref arg* (- disp-code-instrsize vector-tag) 
         (cons (shll (int fx-shift) eax) ac))]
     [($code-reloc-size) 
       (indirect-ref arg* (- disp-code-relocsize vector-tag) ac)]
     [($code-closure-size) 
       (indirect-ref arg* (- disp-code-closuresize vector-tag) ac)]
     [($pcb-set! $set-car! $set-cdr! $vector-set! $string-set! $exit
       $set-symbol-value! $set-symbol-plist! 
       $set-code-byte! $set-code-word! 
       $set-code-object! $set-code-object+offset! $set-code-object+offset/rel!
       $record-set!) 
      (do-effect-prim op arg*
        (cons (movl (int void-object) eax) ac))]
     [(fixnum? immediate? $fxzero? boolean? char? pair? vector? string? symbol?
       procedure? null? not eof-object? $fx= $fx< $fx<= $fx> $fx>= eq?
       $char= $char< $char<= $char> $char>= $unbound-object? code?
       $record?) 
      (do-pred->value-prim op arg* ac)]
     [($code->closure)
      (list* 
        (movl (Simple (car arg*)) eax)
        (addl (int (- disp-code-data vector-tag)) eax)
        (movl eax (mem 0 apr))
        (movl apr eax)
        (addl (int closure-tag) eax)
        (addl (int (align disp-closure-data)) apr)
        ac)]
     [($frame->continuation)
      (NonTail 
        (make-closure (make-code-loc (label "SL_continuation_code")) arg*)
        ac)]
     [else
      (error 'value-prim "unhandled ~s" op)]))
  (define (do-effect-prim op arg* ac)
    (case op
     [($vector-set!) 
      (list* (movl (Simple (car arg*)) ebx)
             (addl (Simple (cadr arg*)) ebx)
             (movl (Simple (caddr arg*)) eax)
             (movl eax (mem (- disp-vector-data vector-tag) ebx))
             ac)]
     [($string-set!) 
      (list* (movl (Simple (cadr arg*)) eax)
             (shrl (int fx-shift) eax)
             (addl (Simple (car arg*)) eax)
             (movl (Simple (caddr arg*)) ebx)
             (movb bh (mem (- disp-string-data string-tag) eax))
             ac)]
     [($set-constant!) 
      (NonTail (cadr arg*) 
        (list* (movl eax (Simple (car arg*))) ac))]
     [($pcb-set!)
      (let ([loc (car arg*)] [val (cadr arg*)])
        (record-case loc
          [(constant i) 
           (unless (fixnum? i) (error who "invalid loc ~s" loc))
           (list* (movl (Simple val) eax)
                  (movl eax (mem (* i wordsize) pcr))
                  ac)]
         [else (error who "invalid loc ~s" loc)]))]
    [($set-car!) 
     (list* (movl (Simple (car arg*)) eax)
            (movl (Simple (cadr arg*)) ebx)
            (movl ebx (mem (- disp-car pair-tag) eax))
            ac)]
    [($set-cdr!) 
     (list* (movl (Simple (car arg*)) eax)
            (movl (Simple (cadr arg*)) ebx)
            (movl ebx (mem (- disp-cdr pair-tag) eax))
            ac)]
    [($set-symbol-value!) 
     (list* (movl (Simple (car arg*)) eax)
            (movl (Simple (cadr arg*)) ebx)
            (movl ebx (mem (- disp-symbol-value symbol-tag) eax))
            ac)]
    [($set-symbol-plist!) 
     (list* (movl (Simple (car arg*)) eax)
            (movl (Simple (cadr arg*)) ebx)
            (movl ebx (mem (- disp-symbol-plist symbol-tag) eax))
            ac)]
    [($set-symbol-unique-string!) 
     (list* (movl (Simple (car arg*)) eax)
            (movl (Simple (cadr arg*)) ebx)
            (movl ebx (mem (- disp-symbol-unique-string symbol-tag) eax))
            ac)]
    [($record-set!) 
     (list* (movl (Simple (car arg*)) ebx)
            (addl (Simple (cadr arg*)) ebx)
            (movl (Simple (caddr arg*)) eax)
            (movl eax (mem (- disp-record-data record-ptag) ebx))
            ac)]
    [($exit)
     (list*
       (movl (Simple (car arg*)) eax)
       (movl (mem (pcb-offset '$frame-base) pcr) fpr)
       (movl (int 0) (mem (pcb-offset '$next-continuation) pcr))
       (jmp (label "SL_scheme_exit"))
       ac)]
    [($set-code-byte!) 
      (list* (movl (Simple (cadr arg*)) eax)
             (shrl (int fx-shift) eax)
             (addl (Simple (car arg*)) eax)
             (movl (Simple (caddr arg*)) ebx)
             (shrl (int fx-shift) ebx)
             (movb bl (mem (- disp-code-data vector-tag) eax))
             ac)]
    [($set-code-word!) 
     (list* (movl (Simple (cadr arg*)) eax)
            (shrl (int fx-shift) eax)
            (addl (Simple (car arg*)) eax)
            (movl (Simple (caddr arg*)) ebx)
            (movl ebx (mem (- disp-code-data vector-tag) eax))
            ac)]
    [($set-code-object!)
     (let ([code (car arg*)] [object (cadr arg*)] 
           [code-offset (caddr arg*)] [reloc-idx (cadddr arg*)])
       (list*
         (movl (Simple code) eax)
         (movl (Simple object) ebx)
         (movl (Simple code-offset) edx)
         (movl edx ecx)
         (shrl (int fx-shift) edx)
         (addl eax edx)
         (movl ebx (mem (- disp-code-data vector-tag) edx))
         (addl (mem (- disp-code-instrsize vector-tag) eax) eax)
         (addl (Simple reloc-idx) eax)
         (movl ecx (mem (- disp-code-data vector-tag) eax))
         ac))]
    [($set-code-object+offset!)
     (let ([code (car arg*)] [object (cadr arg*)] 
           [code-offset (caddr arg*)] [object-offset (cadddr arg*)]
           [reloc-idx (car (cddddr arg*))])
       (list*
         (movl (Simple code) eax)
         (movl (Simple object-offset) ebx) ; ebx = fxdisp
         (shrl (int fx-shift) ebx)         ; ebx = disp in bytes
         (movl ebx ecx)                    ; ecx = disp in bytes
         (addl (Simple object) ecx)      ; ecx = object + disp
         (movl (Simple code-offset) edx) ; edx = fx codeoffset
         (shrl (int fx-shift) edx)       ; edx = codeoffset in bytes
         (addl eax edx)
         (movl ecx (mem (- disp-code-data vector-tag) edx))
         (subl eax edx) 
         (addl (mem (- disp-code-instrsize vector-tag) eax) eax)
         (addl (Simple reloc-idx) eax)
         (sall (int fx-shift) edx)
         (orl (int 1) edx)
         (movl edx (mem (- disp-code-data vector-tag) eax))
         (movl ebx (mem (- (+ disp-code-data wordsize) vector-tag) eax))
         ac))]
    [($set-code-object+offset/rel!)
     (let ([code (car arg*)] [object (cadr arg*)] 
           [code-offset (caddr arg*)] [object-offset (cadddr arg*)]
           [reloc-idx (car (cddddr arg*))])
       (list*
         (movl (Simple code) eax)
         (movl (Simple object-offset) ebx)
         (shrl (int fx-shift) ebx)
         (movl (Simple code-offset) ecx)
         (orl (int 2) ecx)
         (movl (mem (- disp-code-instrsize vector-tag) eax) edx)
         (addl (Simple reloc-idx) edx)
         (addl eax edx)
         (movl ecx (mem (- disp-code-data vector-tag) edx))
         (movl ebx (mem (- (+ wordsize disp-code-data) vector-tag) edx))
         (shrl (int fx-shift) ecx) ; code offset in bytes
         (addl eax ecx)
         (addl (int (- (+ wordsize disp-code-data) vector-tag)) ecx)
         ; ecx points to next word in stream
         (addl (Simple object) ebx) ; ebx is object+objectoffset
         (subl ecx ebx)             ; ebx is relative offset
         (movl ebx (mem (- wordsize) ecx))
         ac))]
    [(cons void)
     (let f ([arg* arg*])
       (cond
         [(null? arg*) ac]
         [else 
          (Effect (car arg*) (f (cdr arg*)))]))]
    [else 
     (error 'do-effect-prim "unhandled op ~s" op)]))
 (define (do-simple-test x Lt Lf ac)
    (unless (or Lt Lf)
      (error 'Pred "no labels"))
    (cond
      [(not Lt)
       (list* (cmpl (int bool-f) x) (je Lf) ac)]
      [(not Lf)
       (list* (cmpl (int bool-f) x) (jne Lt) ac)]
      [else
       (list* (cmpl (int bool-f) x) (je Lf) (jmp Lt) ac)]))
  (define (Simple x)
    (record-case x
      [(cp-var i) 
       (mem (+ (* i wordsize) (- disp-closure-data closure-tag)) cpr)]
      [(frame-var i) (mem (* i (- wordsize)) fpr)]
      [(constant c) (constant-val c)]
      [(constant-loc label) label]
      [(code-loc label) (label-address (label-name label))]
      [(primref op) (primref-loc op)]
      [else (error 'Simple "what ~s" x)]))
  (define (frame-adjustment offset)
    (* (sub1 offset) wordsize -1))
  (define (NonTail x ac) 
    (record-case x
      [(constant c)
       (cons (movl (constant-val c) eax) ac)]
      [(frame-var)
       (cons (movl (Simple x) eax) ac)]
      [(cp-var)
       (cons (movl (Simple x) eax) ac)]
      [(constant-loc label)
       (cons (movl label eax) ac)]
      [(foreign-label L) 
       (cons (movl (label-address L) eax) ac)]
      [(primref c)
       (cons (movl (primref-loc c) eax) ac)]
      [(closure label arg*)
       (let f ([arg* arg*] [off disp-closure-data])
         (cond
           [(null? arg*)
            (list* (movl (Simple label) (mem 0 apr))
                   (movl apr eax)
                   (addl (int (align off)) apr)
                   (addl (int closure-tag) eax)
                   ac)]
           [else
            (list* (movl (Simple (car arg*)) eax)
                   (movl eax (mem off apr))
                   (f (cdr arg*) (+ off wordsize)))]))]
      [(conditional test conseq altern)
       (let ([Lj (unique-label)] [Lf (unique-label)])
         (Pred test #f Lf
           (NonTail conseq
             (list* (jmp Lj) Lf (NonTail altern (cons Lj ac))))))]
      [(seq e0 e1)
       (Effect e0 (NonTail e1 ac))]
      [(primcall op rand*)
       (do-value-prim op rand* ac)]
      [(new-frame base-idx size body)
       (NonTail body ac)]
      [(call-cp call-convention rp-convention offset size mask)
       (let ([L_CALL (unique-label)])
         (case call-convention
           [(normal)
            (list* (addl (int (frame-adjustment offset)) fpr)
                   (movl (int (argc-convention size)) eax)
                   (jmp L_CALL)
                   ; NEW FRAME
                   (byte-vector mask)
                   (int (* offset wordsize))
                   (rp-label rp-convention)
                   (byte 0) ; padding for indirect calls only
                   (byte 0) ; direct calls are ok
                   L_CALL
                   (indirect-cpr-call)
                   (movl (mem 0 fpr) cpr)
                   (subl (int (frame-adjustment offset)) fpr)
                   ac)]
           [(apply)
            (list* (addl (int (frame-adjustment offset)) fpr)
                   (movl (int (argc-convention size)) eax)
                   (jmp L_CALL)
                   ; NEW FRAME
                   (byte-vector mask)
                   (int (* offset wordsize))
                   (rp-label rp-convention)
                   L_CALL
                   (call (label "SL_apply"))
                   (movl (mem 0 fpr) cpr)
                   (subl (int (frame-adjustment offset)) fpr)
                   ac)]
           [(foreign) 
            (list* (addl (int (frame-adjustment offset)) fpr)
                   (movl (int (argc-convention size)) eax)
                   (jmp L_CALL)
                   ; NEW FRAME
                   (byte-vector mask)
                   (int (* offset wordsize))
                   (rp-label rp-convention)
                   L_CALL
                   (call (label "SL_foreign_call"))
                   (movl (mem 0 fpr) cpr)
                   (subl (int (frame-adjustment offset)) fpr)
                   ac)]
           [else (error who "invalid convention ~s for call-cp" convention)]))]
      [else (error 'NonTail "invalid expression ~s" x)]))
  (define (Pred x Lt Lf ac)
    (record-case x
      [(frame-var i) 
       (do-simple-test (idx->frame-loc i) Lt Lf ac)]
      [(cp-var i) 
       (do-simple-test (Simple x) Lt Lf ac)]
      [(constant-loc)
       (if Lt (cons (jmp Lt) ac) ac)]
      [(constant c) 
       (if c
           (if Lt (cons (jmp Lt) ac) ac)
           (if Lf (cons (jmp Lf) ac) ac))]
      [(primcall op rand*)
       (do-pred-prim op rand* Lt Lf ac)]
      [(conditional test conseq altern)
       (cond
         [(not Lt) 
          (let ([Lj^ (unique-label)] [Lf^ (unique-label)])
            (Pred test #f Lf^
              (Pred conseq Lj^ Lf
                (cons Lf^
                  (Pred altern #f Lf 
                    (cons Lj^ ac))))))]
         [(not Lf) 
          (let ([Lj^ (unique-label)] [Lf^ (unique-label)])
            (Pred test #f Lf^
              (Pred conseq Lt Lj^
                (cons Lf^
                  (Pred altern Lt #f 
                    (cons Lj^ ac))))))]
         [else 
          (let ([Lf^ (unique-label)])
            (Pred test #f Lf^
              (Pred conseq Lt Lf
                (cons Lf^
                  (Pred altern Lt Lf ac)))))])] 
      [(seq e0 e1)
       (Effect e0 (Pred e1 Lt Lf ac))]
      [(new-frame)
       (NonTail x (do-simple-test eax Lt Lf ac))]
      [else (error 'Pred "invalid expression ~s" x)]))
  (define (idx->frame-loc i)
    (mem (* i (- wordsize)) fpr))
  (define (Effect x ac)
   (record-case x
     [(constant) ac]
     [(constant-loc) ac]
     [(primcall op rand*)
      (do-effect-prim op rand* ac)]
     [(conditional test conseq altern)
      (let ([Lf (unique-label)] [Ljoin (unique-label)])
        (Pred test #f Lf
          (Effect conseq
            (list* (jmp Ljoin) Lf (Effect altern (cons Ljoin ac))))))]
     [(seq e0 e1)
      (Effect e0 (Effect e1 ac))]
     [(assign loc val)
      (record-case loc
        [(frame-var i)
         (NonTail val
           (cons (movl eax (idx->frame-loc i)) ac))]
        [else (error who "invalid assign loc ~s" loc)])]
     [(eval-cp check body)
      (NonTail body
        (cond
          [check
           (list* 
             (movl eax cpr) 
             (andl (int closure-mask) eax)
             (cmpl (int closure-tag) eax)
             (jne (label "SL_nonprocedure"))
             ac)]
          [else
           (list* 
             (movl eax cpr) 
             ac)]))]
     [(save-cp loc)
      (record-case loc
        [(frame-var i)
         (cons (movl cpr (idx->frame-loc i)) ac)]
        [else (error who "invalid cpr loc ~s" x)])]
     [(new-frame) (NonTail x ac)]
     [else (error 'Effect "invalid expression ~s" x)]))
  (define (Tail x ac) 
    (record-case x
     [(return x) 
      (NonTail x (cons (ret) ac))]
     [(conditional test conseq altern)
      (let ([L (unique-label)])
        (Pred test #f L
          (Tail conseq 
            (cons L (Tail altern ac)))))]
     [(seq e0 e1)
      (Effect e0 (Tail e1 ac))]
     [(new-frame idx size body)
      (Tail body ac)]
     [(call-cp call-convention rp-convention idx argc mask)
      (unless (eq? rp-convention 'tail)
        (error who "nontail rp (~s) in tail context" rp-convention))
      (let f ([i 0])
        (cond
          [(= i argc)
           (case call-convention
             [(normal) 
              (list* 
                (movl (int (argc-convention argc)) eax)
                (tail-indirect-cpr-call)
                ac)]
             [(apply)
              (list* 
                (movl (int (argc-convention argc)) eax)
                (jmp (label "SL_apply")) 
                ac)]
             [else (error who "invalid conv ~s in tail call-cpr" convention)])]
          [else
            (list* (movl (mem (* (+ idx i 1) (- wordsize)) fpr) eax)
                   (movl eax (mem (* (+ i 1) (- wordsize)) fpr))
                   (f (add1 i)))]))]
     [else (error 'Tail "invalid expression ~s" x)]))
  (define (handle-vararg fml-count ac)
    (define CONTINUE_LABEL (unique-label))
    (define DONE_LABEL (unique-label))
    (define CONS_LABEL (unique-label))
    (define LOOP_HEAD (unique-label))
    (define L_CALL (unique-label))
    (list* (cmpl (int (argc-convention (sub1 fml-count))) eax)
           (jg (label "SL_invalid_args"))
           (jl CONS_LABEL)
           (movl (int nil) ebx)
           (jmp DONE_LABEL)
           CONS_LABEL
           (movl (primref-loc '$allocation-redline) ebx)
           (addl eax ebx)
           (addl eax ebx)
           (cmpl ebx apr)
           (jle LOOP_HEAD)
           (addl eax esp) ; advance esp to cover args
           (pushl cpr)    ; push current cp
           (pushl eax)    ; push argc
           (negl eax)     ; make argc positive
           (addl (int (* 4 wordsize)) eax) ; add 4 words to adjust frame size
           (pushl eax)    ; push frame size
           (addl eax eax) ; double the number of args
           (movl eax (mem (* -2 wordsize) fpr)) ; pass it as first arg
           (movl (int (argc-convention 1)) eax) ; setup argc
           (movl (primref-loc 'do-overflow-with-byte-count) cpr) ; load handler
           (jmp L_CALL)   ; go to overflow handler
           ; NEW FRAME
           (int 0)        ; if the framesize=0, then the framesize is dynamic
           (int 0)
           (byte 0)
           (byte 0)
           L_CALL
           (indirect-cpr-call)
           (popl eax)     ; pop framesize and drop it
           (popl eax)     ; reload argc
           (popl cpr)     ; reload cp
           (subl eax esp) ; readjust fp
           LOOP_HEAD
           (movl (int nil) ebx)
           CONTINUE_LABEL
           (movl ebx (mem disp-cdr apr))
           (movl (mem fpr eax) ebx)
           (movl ebx (mem disp-car apr))
           (movl apr ebx)
           (addl (int pair-tag) ebx)
           (addl (int pair-size) apr)
           (addl (int (fxsll 1 fx-shift)) eax)
           (cmpl (int (- (fxsll fml-count fx-shift))) eax)
           (jle CONTINUE_LABEL)
           DONE_LABEL
           (movl ebx (mem (- (fxsll fml-count fx-shift)) fpr))
           ac))
  (define (handle-procedure-entry proper fml-count ac)
    (cond
      [proper 
       (list* (cmpl (int (argc-convention fml-count)) eax)
              (jne (label "SL_invalid_args"))
              ac)]
      [else (handle-vararg fml-count ac)]))
  (define emit-code
    (lambda (label x)
      (record-case x 
        [(code fml* proper free* body)
         (list* 'local-function 
                (label-name label) 
                (+ disp-closure-data (* wordsize (length free*)))
            (handle-procedure-entry proper (length fml*)
            (Tail body '())))])))
  (define (emit-codes prog)
    (record-case prog
      [(codes lhs* rhs* body)
       (let ([label* (map (lambda (x) (unique-label)) lhs*)])
         (for-each set-code-loc-label! lhs* label*)
         (let ([procs (map emit-code label* rhs*)]
               [main-proc 
                (list* 'local-function "L_scheme_entry" 
                       0
                       (Tail body '()))])
           (cons main-proc procs)))]))
  (define label-name cadr)
  (define (emit-constants prog)
    (record-case prog
      [(constants lhs* body)
       (let ([label* (map (lambda (x) (unique-label)) lhs*)])
         (for-each 
           set-constant-loc-label! 
           lhs* label*)
         (cons
           (list 'global-data (string-append main-name "_constant_count") 
                 (length lhs*))
           (append
             (map (lambda (x) (list 'data (label-name x) 0)) label*)
             (emit-codes body))))]))
  (define (emit-prog prog main-name)
    (list*
      (list 'public-function 
            (format "~a_entry" main-name)
             0
            (movl (mem 4 esp) eax) ; pcb
            (push ebx)
            (push esi)
            (push edi)
            (push ebp)
            (movl eax pcr)
            (movl (mem (pcb-offset '$allocation-pointer) pcr) apr) 
            (movl esp (mem (pcb-offset '$system-stack) pcr)) 
            (movl (mem (pcb-offset '$frame-base) pcr) fpr)
            (movl (label-address "SL_underflow_handler") (mem 0 fpr))
            (jmp (label "L_scheme_entry")))
     (emit-constants prog)))
  (emit-prog x main-name))
 


(define (asm-helper-code)
  (list
    (list 'public-function
          "SL_call_with_values"
          disp-closure-data
          (cmpl (int (argc-convention 2)) eax)
          (jne (label "SL_invalid_args"))
          (movl (mem (- wordsize) fpr) ebx) ; producer
          (movl ebx cpr)
          (andl (int closure-mask) ebx)
          (cmpl (int closure-tag) ebx)
          (jne (label "SL_nonprocedure"))
          (movl (int (argc-convention 0)) eax)
          (subl (int (* wordsize 2)) fpr)
          (jmp (label "L_cwv_call"))
          ; MV NEW FRAME
          (byte-vector '#(#b110))
          (int (* wordsize 3))
          (label-address "L_cwv_multi_rp")
          (byte 0)
          (byte 0)
          (label "L_cwv_call")
          (indirect-cpr-call)
          ;;; one value returned
          (addl (int (* wordsize 2)) fpr)
          (movl (mem (* -2 wordsize) fpr) ebx) ; consumer
          (movl ebx cpr)
          (movl eax (mem (- wordsize) fpr))
          (movl (int (argc-convention 1)) eax)
          (andl (int closure-mask) ebx)
          (cmpl (int closure-tag) ebx)
          (jne (label "SL_nonprocedure"))
          (tail-indirect-cpr-call)
          ;;; multiple values returned
          (label "L_cwv_multi_rp")
          ; because values does not pop the return point
          ; we have to adjust fp one more word here
          (addl (int (* wordsize 3)) fpr) 
          (movl (mem (* -2 wordsize) fpr) cpr) ; consumer
          (cmpl (int (argc-convention 0)) eax)
          (je (label "L_cwv_done"))
          (movl (int (* -4 wordsize)) ebx)
          (addl fpr ebx)  ; ebx points to first value
          (movl ebx ecx)
          (addl eax ecx)  ; ecx points to the last value
          (label "L_cwv_loop")
          (movl (mem 0 ebx) edx)
          (movl edx (mem (* 3 wordsize) ebx))
          (subl (int wordsize) ebx)
          (cmpl ecx ebx)
          (jge (label "L_cwv_loop"))
          (label "L_cwv_done")
          (movl cpr ebx)
          (andl (int closure-mask) ebx)
          (cmpl (int closure-tag) ebx)
          (jne (label "SL_nonprocedure"))
          (tail-indirect-cpr-call))
    (list 'public-function
          "SL_values"
          disp-closure-data
          (cmpl (int (argc-convention 1)) eax)
          (je (label "L_values_one_value"))
          (label "L_values_many_values")
          (movl (mem 0 fpr) ebx) ; return point
          (jmp (list 'indirect (mem disp-multivalue-rp ebx)))     ; go
          (label "L_values_one_value")
          (movl (mem (- wordsize) fpr) eax)
          (ret))
    (list 'public-function
          "SL_multiple_values_error_rp"
          0
          (movl (mem (pcb-offset '$multiple-values-error) pcr) cpr)
          (tail-indirect-cpr-call))
    (list 'public-function
          "SL_multiple_values_ignore_rp"
          0
          (ret))
    (list 'public-function
          "SL_scheme_exit"
          0
          (movl apr (mem (pcb-offset '$allocation-pointer) pcr))
          (cmpl (mem (pcb-offset '$frame-base) pcr) fpr)
          (jne (label "L_scheme_exit_fp_mismatch"))
          (movl (mem (pcb-offset '$system-stack) pcr) esp)
          (pop ebp)
          (pop edi)
          (pop esi)
          (pop ebx)
          (ret)
          (label "L_scheme_exit_fp_mismatch")
          (movl (int 0) eax)
          (movl (mem 0 eax) eax))
    (list 'public-function
          "L_underflow"
          0
          (label-address "SL_underflow_multiple_values")
          (byte-vector (make-vector (- (+ wordsize disp-multivalue-rp)) 0))
          '(global "SL_underflow_handler")
          (label "SL_underflow_handler")
          ; since we underflow with a call to (ret), the current fp
          ; is below the valid stack, so we advance it up to point
          ; to the underflow handler that caused the ret
          (subl (int wordsize) fpr)
          ; load next continuation into ebx, and if ebx=0, exit
          ; since the computation is complete
          (movl (mem (pcb-offset '$next-continuation) pcr) ebx)
          (cmpl (int 0) ebx)
          (je (label "SL_scheme_exit"))
          ; sanity check that fpr *is* where it should be
          (cmpl (mem (pcb-offset '$frame-base) pcr) fpr)
          (jne (label "L_underflow_misaligned"))
          (label "L_underflow_frame_ok")
                                       ;(movl (int 0) eax)
                                       ;(movl (mem 0 eax) eax)
          ; sanity check that 0(fpr) does contain underflow hander
          (cmpl (label-address "SL_underflow_handler") (mem 0 fpr))
          (jne (label "L_underflow_no_rp"))
          ; save the value of eax
          (pushl eax)
          ; now ebx=next_cont
          (movl (mem (- disp-continuation-top vector-tag) ebx) ecx)
          ; ebx=cc, ecx=cont_top
          (movl (mem (- disp-continuation-size vector-tag) ebx) eax)
          ; ebx=cc, ecx=cont_top, eax=cont_size
          (movl (mem 0 ecx) edx) ; return point is in edx
          ; ebx=cc, ecx=cont_top, eax=cont_size, edx=rp
          (movl (mem disp-frame-size edx) edx) ; size
          ; ebx=cc, ecx=cont_top, eax=cont_size, edx=top_frame_size
          (cmpl (int 0) edx)
          (jne (label "L_underflow_normal_frame"))
          (label "L_underflow_special_frame")
          (movl (int 0) eax)
          (movl (mem 0 eax) eax)
          (label "L_underflow_normal_frame") 
          ; ebx=cc, ecx=cont_top, eax=cont_size, edx=top_frame_size
          (cmpl eax edx)
          (je (label "L_underflow_single_frame"))
          (label "L_underflow_multiple_frames") 
          (cmpl (mem (pcb-offset '$allocation-redline) pcr) apr)
          (jge (label "L_underflow_heap_overflow"))
          ; ebx=cc, ecx=cont_top, eax=cont_size, edx=top_frame_size
          (movl (int continuation-tag) (mem 0 apr))
          (subl edx eax) 
          ; ebx=cc, ecx=cont_top, eax=remaining_size, edx=top_frame_size
          (movl eax (mem disp-continuation-size apr))
          (movl edx (mem (- disp-continuation-size vector-tag) ebx))
          (addl edx ecx) 
          ; ebx=cc, ecx=next_cont_top, eax=remaining_size, edx=top_frame_size
          (movl ecx (mem disp-continuation-top apr))
          (subl edx ecx) 
          ; ebx=cc, ecx=cont_top, eax=next_cont, edx=top_frame_size
          (movl (mem (- disp-continuation-next vector-tag) ebx) eax)
          (movl eax (mem disp-continuation-next apr))
          (movl apr eax)
          (addl (int vector-tag) eax)
          (addl (int continuation-size) apr)
          (movl eax (mem (- disp-continuation-next vector-tag) ebx))
          ; framesize=edx, top=ecx, cc=ebx
          (label "L_underflow_single_frame")
          ; advance cc
          (movl (mem (- disp-continuation-next vector-tag) ebx) eax)
          (movl eax (mem (pcb-offset '$next-continuation) pcr))
          (popl eax) ; pop the return value
          (label "L_underflow_copy_loop")
          (subl (int wordsize) edx)
          (movl (mem ecx edx) ebx)
          (pushl ebx)
          (cmpl (int 0) edx)
          (jg (label "L_underflow_copy_loop"))
          (ret)
          (label "L_underflow_no_rp")
          (movl (int 0) eax)
          (movl (mem 0 eax) eax)
          (label "L_underflow_misaligned")
          (movl (mem (pcb-offset '$frame-base) pcr) ebx)
          (movl (int 0) eax)
          (movl (mem 0 eax) eax)
          (label "L_underflow_heap_overflow")
          ; the return value that was in %eax was pushed previously
          ; so, we push the frame size next
          (pushl (int (* 3 wordsize)))
          (movl (mem (pcb-offset 'do-overflow) pcr) cpr)
          (movl (int (argc-convention 0)) eax)
          (jmp (label "L_underflow_overflow_call"))
          ; NEW FRAME
          (int 0)
          (int 0)
          (byte 0)
          (byte 0)
          (label "L_underflow_overflow_call")
          (indirect-cpr-call)
          (popl eax) ; pop framesize
          (popl eax) ; actual return value and underflow again
          (ret))
    (list 'public-function
          "SL_underflow_multiple_values"
          0
          ;;; So, we are underflowing with multiple values
          ;;; the index of the last value is in %eax
          ;;; so, the last value is in 0(%fpr,%eax)
          ;;; What we need to do is shift the values up by the
          ;;; size of the next frame, copy the frame over, 
          ;;; adjust the frame pointer, then mv-return to the
          ;;; next frame.
          ;;; Caveats:
          ;;; * may need to split the next-k if it's more than
          ;;;   one frame
          ;;; * splitting the continuation may heap-overflow
          ;;; * the required stack size (to hold the values and
          ;;;   the previous frame) may actually cause a stack
          ;;;   overflow!
          ;;;
          ; First, do some assertions
          (cmpl (mem (pcb-offset '$frame-base) pcr) fpr)
          (jne (label "L_umv_bad_fpr"))
          (cmpl (label-address "SL_underflow_handler") (mem 0 fpr))
          (jne (label "L_umv_bad_rp"))
          (movl (mem (pcb-offset '$next-continuation) pcr) ebx)
          (cmpl (int 0) ebx)
          (je (label "L_umv_last_continuation"))
          ; all is good, now check that we have one frame
          (movl (mem (- disp-continuation-top vector-tag) ebx) ecx) ; top
          (movl (mem 0 ecx) edx) ; return-point
          (movl (mem disp-frame-size edx) edx) ; framesize
          (cmpl (int 0) edx)
          (jne (label "L_umv_framesz_ok"))
          (movl (mem wordsize ecx) edx) ; load framesize from top[1]
          ; argc=%eax, next_k=%ebx, frametop=%ecx, framesize=%edx
          (label "L_umv_framesz_ok")
          (cmpl (mem (- disp-continuation-size vector-tag) ebx) edx)
          (je (label "L_umv_single_frame"))
;;;
          (cmpl (mem (pcb-offset '$allocation-redline) pcr) apr)
          (jge (label "L_umv_heap_overflow"))
          (label "L_umv_split_continuation")
          ; ebx=cc, ecx=cont_top, edx=top_frame_size
          (movl (int continuation-tag) (mem 0 apr))
          (addl edx ecx)
          (movl ecx (mem disp-continuation-top apr))
          (movl (mem (- disp-continuation-size vector-tag) ebx) ecx)
          (subl edx ecx)
          (movl ecx (mem disp-continuation-size apr))
          (movl edx (mem (- disp-continuation-size vector-tag) ebx))
          (movl (mem (- disp-continuation-next vector-tag) ebx) ecx)
          (movl ecx (mem disp-continuation-next apr))
          (movl apr ecx)
          (addl (int vector-tag) ecx)
          (movl ecx (mem (- disp-continuation-next vector-tag) ebx))
          (addl (int continuation-size) apr)
          (movl (mem (- disp-continuation-top vector-tag) ebx) ecx)
;;;
          (label "L_umv_single_frame")
          ; argc=%eax, next_k=%ebx, frametop=%ecx, framesize=%edx
          (negl edx)      
          (addl eax edx) ; %edx is the offset to the last req cell
          (addl fpr edx) ; %edx is the address of the last req cell
          (cmpl (mem (pcb-offset '$frame-redline) pcr) edx)
          (jle (label "L_umv_stack_overflow"))
          (label "L_umv_no_stack_overflow")
          (movl (mem (- disp-continuation-size vector-tag) ebx) edx)
          (cmpl (int 0) eax)
          (je (label "L_umv_copy_values_done"))
          ; make ecx point to the last arg, edx is the shift amount
          (negl edx)
          (movl fpr ecx)
          (addl eax ecx)
          (label "L_umv_copy_values_loop")
          (movl (mem 0 ecx) ebx)
          (movl ebx (mem edx ecx))
          (addl (int wordsize) ecx)
          (cmpl ecx fpr)
          (jne (label "L_umv_copy_values_loop"))
          (negl edx)
          (label "L_umv_copy_values_done")
          ; now all the values were copied to their new locations
          ; so, now, we copy the next frame
          (movl (mem (pcb-offset '$next-continuation) pcr) ebx)
          (movl (mem (- disp-continuation-top vector-tag) ebx) ecx)
          ; %ebx=next_k, %ecx=frame_top, %edx=framesize, %eax=argc
          (label "L_umv_copy_frame_loop")
          (subl (int wordsize) edx)
          (pushl (mem edx ecx))
          (cmpl (int 0) edx)
          (jne (label "L_umv_copy_frame_loop"))
          (label "L_umv_copy_frame_done")
          ;;; okay, almost done
          ;;; set next k appropriately
          (movl (mem (- disp-continuation-next vector-tag) ebx) ebx)
          (movl ebx (mem (pcb-offset '$next-continuation) pcr))
          (movl (mem 0 fpr) ebx)
          (jmp (list 'indirect (mem disp-multivalue-rp ebx)))     ; go
          ;;;
          (label "L_umv_bad_fpr")
             (movl (int 0) eax) (movl (mem 0 eax) eax)
          (label "L_umv_bad_rp") 
             (movl (int 0) eax) (movl (mem 0 eax) eax)
          (label "L_umv_heap_overflow")
             (movl (int 0) eax) (movl (mem 0 eax) eax)
          (label "L_umv_stack_overflow") 
             (movl (int 0) eax) (movl (mem 0 eax) eax)
          (label "L_umv_last_continuation") 
             (ret)
          )
    (list 'public-function
          "SL_continuation_code"
          (+ disp-closure-data wordsize)
          (movl (mem (- disp-closure-data closure-tag) cpr) ebx) ; captured-k
          (movl ebx (mem (pcb-offset '$next-continuation) pcr)) ; set
          (movl (mem (pcb-offset '$frame-base) pcr) ebx)
          (cmpl (int (argc-convention 1)) eax)
          (jg (label "L_cont_zero_args"))
          (jl (label "L_cont_mult_args"))
          (label "L_cont_one_arg")
          (movl (mem (- wordsize) fpr) eax)
          (movl ebx fpr)
          (ret)
          (label "L_cont_zero_args")
          (movl ebx fpr)
          (jmp (label "SL_underflow_multiple_values"))
          (label "L_cont_mult_args")
          (cmpl ebx fpr)
          (jne (label "L_cont_mult_move_args"))
          (jmp (label "SL_underflow_multiple_values"))
          (label "L_cont_mult_move_args")
          ; move args from fpr to ebx
          (movl (int 0) ecx)
          (label "L_cont_mult_copy_loop")
          (subl (int wordsize) ecx)
          (movl (mem fpr ecx) edx)
          (movl edx (mem ebx ecx))
          (cmpl ecx eax)
          (jne (label "L_cont_mult_copy_loop"))
          (movl ebx fpr)
          (jmp (label "SL_underflow_multiple_values")))

    (list 'public-function
          "SL_foreign_call"
          0
          (movl fpr (mem (pcb-offset '$frame-pointer) pcr))
          (movl apr (mem (pcb-offset '$allocation-pointer) pcr))
          (movl fpr ebx)
          (movl (mem (pcb-offset '$system-stack) pcr) esp)
          (pushl pcr)
          (cmpl (int 0) eax)
          (je (label "L_foreign_call_set"))
          (label "L_foreign_call_loop")
          (movl (mem ebx eax) ecx)
          (pushl ecx)
          (addl (int 4) eax)
          (cmpl (int 0) eax)
          (jne (label "L_foreign_call_loop"))
          (label "L_foreign_call_set")
          ; FOREIGN NEW FRAME
          (call (list 'indirect cpr))
          (movl (mem (pcb-offset '$frame-pointer) pcr) fpr)
          (movl (mem (pcb-offset '$allocation-pointer) pcr) apr)
          (ret))
    (list 'public-function
          "SL_apply"
          0
          (movl (mem fpr eax) ebx)
          (cmpl (int nil) ebx)
          (je (label "L_apply_done"))
          (label "L_apply_loop")
          (movl (mem (- disp-car pair-tag) ebx) ecx)
          (movl (mem (- disp-cdr pair-tag) ebx) ebx)
          (movl ecx (mem fpr eax))
          (subl (int wordsize) eax)
          (cmpl (int nil) ebx)
          (jne (label "L_apply_loop"))
          (label "L_apply_done")
          (addl (int wordsize) eax)
          (tail-indirect-cpr-call))
    (list 'public-function
           "SL_nonprocedure"
           0
           ;;;
           (movl cpr (mem (- wordsize) fpr)) ; first arg
           (movl (mem (pcb-offset '$apply-nonprocedure-error-handler)
                      pcr) 
                 cpr)
           (movl (int (argc-convention 1)) eax)
           (tail-indirect-cpr-call))
    (list 'public-function
          "SL_invalid_args"
          0
          ;;;
          (movl cpr (mem (- wordsize) fpr)) ; first arg
          (negl eax)
          (movl eax (mem (- (* 2 wordsize)) fpr))
          (movl (mem (pcb-offset '$incorrect-args-error-handler)
                     pcr) 
                cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
 

(define (emit-linear-code obj*)
  (define who 'emit-linear-code)
  (define (arg x)
    (cond
      [(not (pair? x)) (error who "invalid arg ~s" x)]
      [else
       (case (car x)
         [(register) (cadr x)]
         [(label) (cadr x)]
         [(label-address) (format "$~a" (cadr x))]
         [(integer) (format "$~a" (cadr x))]
         [(mem)
          (cond
            [(integer? (cadr x))
             (format "~a(~a)" (cadr x) (arg (caddr x)))]
            [else
             (format "(~a,~a)" (arg (cadr x)) (arg (caddr x)))])]
         [(indirect) (format "*~a" (arg (cadr x)))]
         [else (error who "invalid arg ~s" x)])]))
  (define (emit-generic x)
    (case (length x)
      [(1) (emit "   ~a" (car x))]
      [(2) (emit "   ~a ~a" (car x) (arg (cadr x)))]
      [(3) (emit "   ~a ~a, ~a" (car x) (arg (cadr x)) (arg (caddr x)))]
      [else (error 'emit-generic "invalid format ~s" x)]))
  (define (emit-instruction x)
    (case (car x)
      [(pop movl movb push call ret cltd
        cmpl je jne jl jle jg jge jb jbe ja jae
        jmp sete setl setle setg setge movzbl pushl popl
        addl subl orl xorl andl notl shll shrl sall sarl imull idivl negl)
       (emit-generic x)]
      [(nop) (void)]
      [(label) (emit "~a:" (cadr x))]
      [(comment) (emit "/* ~s */" (cadr x))]
      [(integer) 
       (emit ".long ~s" (cadr x))]
      [(byte) 
       (emit ".byte ~s" (cadr x))]
      [(byte-vector)
       (let f ([v (cadr x)] [i 0])
         (unless (= i (vector-length v))
           (emit ".byte ~s" (vector-ref v i))
           (f v (add1 i))))]
      [(label-address)
       (emit ".long ~a" (cadr x))]
      [(global)
       (emit ".globl ~a" (cadr x))]
      [else (error 'emit-instruction "unsupported instruction ~s" (car x))]))
  (define (emit-function-header x)
    (let ([t (car x)] [label (cadr x)] [closure-size (caddr x)])
      (emit ".text")
      (when (eq? t 'public-function)
        (emit ".globl ~a" label))
      (emit ".type ~a @function" label)
      (emit ".align 8")
      (emit ".long ~a" code-tag) ; tag
      (emit ".long 0") ; instr size
      (emit ".long 0") ; reloc size
      (emit ".long ~s" closure-size)
      (emit "~a:" label)))
  (define (emit-function x)
    (emit-function-header x)
    (for-each emit-instruction (cdddr x)))
  (define (emit-data x)
    (let ([t (car x)] [label (cadr x)] [value (caddr x)])
      (emit ".data")
      (emit ".align 4")        
      (when (eq? t 'global-data)
        (emit ".globl ~a" label))
      (emit ".type ~a, @object" label)
      (emit ".size ~a, 4" label)       
      (emit "~a:" label)                 
      (emit ".long ~s" value)))
  (define (emit-object x)
    (case (car x)
      [(public-function local-function) (emit-function x)]
      [(data global-data) (emit-data x)]
      [else (error who "invalid object ~s" (car x))]))
  (for-each emit-object obj*))


(define (compile-program-with-entry original-program scheme-entry)
  (let* (;;;
         [p (recordize original-program)]
         [p (optimize-direct-calls p)]
         [p (remove-assignments p)]
         [p (convert-closures p)]
         [p (lift-codes p)]
         [p (lift-complex-constants p)]
         [p (introduce-primcalls p)]
         [p (simplify-operands p)]
         [p (insert-stack-overflow-checks p)]
         [p (insert-allocation-checks p)]
         [p (remove-local-variables p)]
         ;[f (pretty-print (unparse p))]
         [p (generate-code p scheme-entry)])
    (emit-linear-code p)))

(define (compile-program x)
  (compile-program-with-entry x "scheme"))



(define (file-content x)
  (let ([p (open-input-file x)])
    (let f ()
      (let ([x (read p)])
        (cond
          [(eof-object? x)
           (close-input-port p)
           '()]
          [else 
           (cons x (f))])))))
       

(define (generate-library x)
  (let ([input-file-name (car x)]
        [output-file-name (cadr x)]
        [entry-name (caddr x)])
   (printf "compiling ~s\n" input-file-name)
   (let ([prog (cons 'begin (file-content input-file-name))])
     (let ([op (open-output-file output-file-name 'replace)])
       (parameterize ([compile-port op]
                      [inline-primitives #t]
                      [signal-error-on-undefined-pcb #f])
         (compile-program-with-entry prog entry-name))
       (close-output-port op)))))

(define (generate-top-level)
  (printf "compiling top-level\n")
  (let ([prog (cons 'begin 
                 (map (lambda (x) `($set-symbol-value! ',x ,x))
                      (public-primitives)))])
    (let ([op (open-output-file "libtoplevel.s" 'replace)])
      (parameterize ([compile-port op]
                     [inline-primitives #t])
        (compile-program-with-entry prog "libtoplevel"))
      (close-output-port op))))

(define (generate-scheme-h)
  (let ([p (open-output-file "scheme.h" 'replace)])
    (define (def name val) 
      (fprintf p "#define ~a ~a\n" name val))
    (define (defp name val) 
      (fprintf p "#define ~a ((ptr)~a)\n" name val))
    (fprintf p "/* automatically generated, do not edit */\n")
    (fprintf p "#ifndef SCHEME_H\n")
    (fprintf p "#define  SCHEME_H\n")
    (fprintf p "typedef char* ptr;\n")
    (def "fx_shift" fx-shift)
    (def "fx_mask" fx-mask)
    (def "fx_tag" fx-tag)
    (defp "bool_f" bool-f)
    (defp "bool_t" bool-t)
    (def "bool_mask" bool-mask)
    (def "bool_tag" bool-tag)
    (def "bool_shift" bool-shift)
    (defp "empty_list" nil)
    (def "wordsize" wordsize)
    (def "char_shift" char-shift)
    (def "char_tag" char-tag)
    (def "char_mask" char-mask)
    (def "pair_mask" pair-mask)
    (def "pair_tag" pair-tag)
    (def "disp_car" disp-car)
    (def "disp_cdr" disp-cdr)
    (def "pair_size" pair-size)
    (def "symbol_mask" symbol-mask)
    (def "symbol_tag" symbol-tag)
    (def "disp_symbol_string" disp-symbol-string)
    (def "disp_symbol_value" disp-symbol-value)
    (def "symbol_size" symbol-size)
    (def "vector_tag" vector-tag)
    (def "vector_mask" vector-mask)
    (def "disp_vector_length" disp-vector-length)
    (def "disp_vector_data" disp-vector-data)
    (def "string_mask" string-mask)
    (def "string_tag" string-tag)
    (def "disp_string_length" disp-string-length) 
    (def "disp_string_data" disp-string-data)
    (def "closure_mask" closure-mask)
    (def "closure_tag" closure-tag)
    (def "disp_closure_data" disp-closure-data)
    (def "disp_closure_code" disp-closure-code)
    (def "record_pmask" record-pmask)
    (def "record_ptag" record-ptag)
    (def "disp_record_data" disp-record-data)
    (def "disp_record_rtd" disp-record-rtd)

    (def "continuation_tag" continuation-tag)
    (def "disp_continuation_top" disp-continuation-top)
    (def "disp_continuation_size" disp-continuation-size)
    (def "disp_continuation_next" disp-continuation-next)
    (def "continuation_size" continuation-size)

    (def "code_tag"              code-tag)
    (def "disp_code_instrsize"   disp-code-instrsize)
    (def "disp_code_relocsize"   disp-code-relocsize)
    (def "disp_code_closuresize" disp-code-closuresize)
    (def "disp_code_data"        disp-code-data)

    (def "disp_frame_size" disp-frame-size)
    (def "object_alignment" object-alignment)
    (def "align_shift" align-shift)

    (fprintf p "typedef struct {\n")
    (for-each 
       (lambda (x) (fprintf p "   ptr ~a;\n" x))
       (pcb-cnames))
    (fprintf p "} pcb_t;\n")
    (fprintf p "ptr scheme_entry(pcb_t* pcb);\n")
    (fprintf p "extern ptr scheme_main(pcb_t* pcb);\n")
    (fprintf p "#endif /* SCHEME_H */\n")
    (close-output-port p)))

(define (generate-scheme-c)
  (let ([p (open-output-file "scheme.c" 'replace)])
    (fprintf p "/* automatically generated, do not edit */\n")
    (fprintf p "#include \"scheme.h\"\n")
    (fprintf p "#include <stdio.h>\n")
    (fprintf p "ptr scheme_main(pcb_t* pcb){\n")
    (fprintf p "extern void S_add_roots(pcb_t*,int*);\n")
    (fprintf p "extern void S_check_roots(pcb_t*,int*);\n")
    (fprintf p "extern void SL_values();\n")
    (fprintf p "extern void SL_call_with_values();\n")
    (for-each (lambda (x)
                (let ([name (caddr x)])
                  (fprintf p "extern void ~a_entry(pcb_t*);\n" name)
                  (fprintf p "extern int ~a_constant_count;\n" name)))
              scheme-library-files)
    (fprintf p "extern void ~a_entry(pcb_t*);\n" "libtoplevel")
    (fprintf p "char** ap = (char**) pcb->allocation_pointer;\n")
    (fprintf p "ap[0] = (char*) SL_values;\n")
    (fprintf p "ap[1] = 0;\n")
    (fprintf p "pcb->~a = ((char*)ap) + closure_tag;\n"
             (pcb-cname 'values))
    (fprintf p "ap += 2;\n")
    (fprintf p "ap[0] = (char*) SL_call_with_values;\n")
    (fprintf p "ap[1] = 0;\n")
    (fprintf p "pcb->~a = ((char*)ap) + closure_tag;\n"
             (pcb-cname 'call-with-values))
    (fprintf p "ap += 2;\n")
    (fprintf p "pcb->allocation_pointer = (char*)ap;\n")
    (mark-pcb-set-found 'values)
    (mark-pcb-set-found 'call-with-values)
    (for-each 
       (lambda (x)
         (let ([name (caddr x)])
          (fprintf p "   S_add_roots(pcb, &~a_constant_count);\n" name)
          (fprintf p "   ~a_entry(pcb);\n" name)
          (fprintf p "   S_check_roots(pcb, &~a_constant_count);\n" name)))
       scheme-library-files)
    (fprintf p "   libtoplevel_entry(pcb);\n");
    (fprintf p "   return scheme_entry(pcb);\n");
    (fprintf p "}\n")
    (close-output-port p)))

(define (generate-scheme-asm)
  (let ([p (open-output-file "scheme_asm.s" 'replace)])
    (parameterize ([compile-port p])
      (emit "# AUTOMATICALLY GENERATED, DO NOT EDIT")
      (emit-linear-code (asm-helper-code)))
    (close-output-port p)))

(define (generate-scheme-runtime-helpers)
  (generate-scheme-h)
  (generate-scheme-c)
  (generate-scheme-asm))



(define (string-join sep str*)
  (cond
    [(null? str*) ""]
    [(null? (cdr str*)) (car str*)]
    [else (string-append (car str*) sep (string-join sep (cdr str*)))]))

(printf "Generating C Helpers\n")
(generate-scheme-runtime-helpers)
(printf "Generating libraries\n")
(for-each generate-library scheme-library-files)
(generate-top-level)

;;; ensure that we did not emit a reference to an unset pcb cell.
(printf "Checking PCB\n")

(let ([undefined '()])
  (for-each 
    (lambda (x)
      (when (and (pcb-referenced? (car x))
                 (not (pcb-assigned? (car x)))
                 (not (pcb-system-loc? (car x))))
        (set! undefined (cons (car x) undefined))))
    pcb-table)
  (unless (null? undefined)
    ((if (signal-error-on-undefined-pcb)
         error
         warning)
     'compile "undefined primitives found ~s" undefined)))

  
(runtime-file 
  (string-join " " 
    (list* "scheme.c" "scheme_asm.s" "runtime-5.4.c" "collect-5.6.c" 
           "libtoplevel.s"
       (map cadr scheme-library-files))))

(with-output-to-file "Makefile"
  (lambda ()
    (printf "stst: stst.s ~a\n" (runtime-file))
    (printf "\tgcc -Wall -o stst stst.s ~a\n" (runtime-file)))
  'replace)

(printf "Testing ...\n")

;(test-all)
;(parameterize ([inline-primitives #f]) (test-all))
;(parameterize ([inline-primitives #t]) (test-all))
(parameterize ([inline-primitives #t]
               [input-filter 
                (lambda (x)
                  `(begin
                     (write ,x)
                     (newline)
                     (exit)
                     ))])
  (test-all))

;  (parameterize ([inline-primitives #t]
;                 [input-filter 
;                  (lambda (x)
;                    `(let ([expr ',x])
;                       (let ([p (open-output-file "stst.tmp" 'replace)])
;                         (write expr p)
;                         (close-output-port p))
;                       (let ([p (open-input-file "stst.tmp")])
;                         (let ([t (read p)])
;                           (unless (equal? t expr)
;                             (error 'test
;                                    "not equal: got ~s, should be ~s"
;                                    t expr)))
;                         (close-input-port p))
;                       (write ,x) ; as usual
;                       (newline)
;                       (exit)))])
;    (test-all))

;(parameterize ([inline-primitives #t]
;               [input-filter 
;                (lambda (x)
;                  `(begin 
;                     (write (eval ',x))
;                     (newline)
;                     (exit 0)
;                     ))])
;  (test-all))
;
(define (get-date)
  (let ([ls (process "date +\"%F\"")])
    (let ([ip (car ls)])
      (list->string
        (let f ()
          (let ([x (read-char ip)])
            (if (char=? x #\newline)
                '()
                (cons x (f)))))))))
  
(build-program
  `(begin
     (display ,(format "Petite Ikarus Scheme (Build ~a)\n" (get-date)))
     (display "Copyright (c) 2006 Abdulaziz Ghuloum\n\n")
     (new-cafe)))

(system "cp stst petite-ikarus")
