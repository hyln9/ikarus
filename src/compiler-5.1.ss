


(print-gensym #f)

(define inline-primitives (make-parameter #f))

(define signal-error-on-undefined-pcb (make-parameter #t))

(load "record-case.ss")
(load "set-operations.ss")
(load "tests-driver.ss")
(load "tests-5.1-req.scm")
(load "tests-4.3-req.scm")
(load "tests-4.2-req.scm")
(load "tests-4.1-req.scm")
(load "tests-3.4-req.scm")
(load "tests-3.3-req.scm")
(load "tests-3.2-req.scm")
(load "tests-3.1-req.scm")
(load "tests-2.9-req.scm")
(load "tests-2.8-req.scm")
(load "tests-2.6-req.scm")
(load "tests-2.4-req.scm")
(load "tests-2.3-req.scm")
(load "tests-2.2-req.scm")
(load "tests-2.1-req.scm")
(load "tests-1.9-req.scm")
(load "tests-1.8-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")


(define scheme-library-files
  '(["libsymboltable-4.4.ss" "libsymboltable-4.4.s" "libsymboltable"]
    ["libhandlers-3.3.ss" "libhandlers-3.3.s" "libhandlers"]
    ["libcore-4.4.ss" "libcore-4.4.s" "libcore"]
    ["libio-4.2.ss" "libio-4.2.s" "libio"]
    ["libwriter-4.4.ss" "libwriter-4.4.s" "libwriter"]
    ["libtokenizer-4.3.ss" "libtokenizer-4.3.s" "libtokenizer"]
    ["libeval-4.4.ss" "libeval-4.4.s" "libeval"]
    ["libcafe-4.4.ss" "libecafe-4.4.s" "libcafe"]
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
  '([$constant-ref      1]
    [$constant-set!     2]
    [$pcb-ref           1]
    [$pcb-set!          2]
    ;;; type predicates
    [fixnum?            1]
    [boolean?           1]
    [char?              1]
    [pair?              1]
    [symbol?            1]
    [vector?            1]
    [string?            1]
    [procedure?         1]
    [null?              1]
    [eof-object?        1]
    [$unbound-object?   1]
    [not                1]
    [eq?                2]
    ;;; fixnum primitives
    [$fxadd1            1]
    [$fxsub1            1]
    [$fx+               2]
    [$fx-               2]
    [$fx*               2]
    [$fxsll             2]
    [$fxsra             2]
    [$fxlogand          2]
    [$fxlogor           2]
    [$fxlogxor          2]
    [$fxlognot          1]
    [$fxquotient        2]
    ;;; fixnum predicates
    [$fxzero?           1]
    [$fx=               2]
    [$fx<               2]
    [$fx<=              2]
    [$fx>               2]
    [$fx>=              2]
    ;;; character predicates
    [$char=             2]
    [$char<             2]
    [$char<=            2]
    [$char>             2]
    [$char>=            2]
    ;;; character conversion
    [$fixnum->char      1]
    [$char->fixnum      1]
    ;;; lists/pairs
    [cons               2]
    [$car               1]
    [$cdr               1]
    [$set-car!          2]
    [$set-cdr!          2]
    ;;; vectors
    [$make-vector       1]
    [vector           any]
    [$vector-length     1]
    [$vector-ref        2]
    [$vector-set!       3]
    ;;; strings
    [$make-string       1]
    [$string          any]
    [$string-length     1]
    [$string-ref        2]
    [$string-set!       3]
    [$string-ref-16+0   2] 
    [$string-ref-16+1   2] 
    [$string-ref-8+2    2] 
    [$string-ref-8+0    2] 
    ;;; symbols
    [$make-symbol       1]
    [$symbol-value      1]
    [$symbol-string     1]
    [$set-symbol-value! 2]
    ;;; misc
    [eof-object         0]
    [void               0]
    [$exit              1]
    [$apply       no-code]
   ))

;;; pcb table section
(define pcb-table
 '(;;; system locations used by the C/Scheme interface
   [$allocation-pointer system "allocation_pointer"]
   [$scheme-stack       system       "scheme_stack"]
   [$system-stack       system       "system_stack"]
   ;;; error handling procedures used by the codegen
   [$apply-nonprocedure-error-handler library]
   [$incorrect-args-error-handler     library]
   [$intern                           library]
   ;;; type predicates
   [fixnum?              public]
   [boolean?             public]
   [char?                public]
   [null?                public]
   [pair?                public]
   [symbol?              public]
   [vector?              public]
   [string?              public]
   [procedure?           public]
   [eof-object?          public]
   [not                  public]
   [eq?                  public]
   [equal?               public]
   ;;; fixnum primitives
   [fxadd1               public]
   [fxsub1               public]
   [fx+                  public]
   [fx-                  public]
   [fx*                  public]
   [fxsll                public]
   [fxsra                public]
   [fxlogor              public]
   [fxlogand             public]
   [fxlogxor             public]
   [fxlognot             public]
   [fxquotient           public]
   [fxremainder          public]
   ;;; fixnum predicates
   [fxzero?              public]
   [fx=                  public]
   [fx<                  public]
   [fx<=                 public]
   [fx>                  public]
   [fx>=                 public]
   ;;; characters
   [char=                public]
   [char<                public]
   [char<=               public]
   [char>                public]
   [char>=               public]
   [fixnum->char         public]
   [char->fixnum         public]
   ;;; lists
   [cons                 public]
   [car                  public]
   [cdr                  public]
   [cadr                 public]
   [caddr                public]
   [cadddr               public]
   [cddddr               public]
   [set-car!             public]
   [set-cdr!             public]
   [list                 public]
   [list*                ADDME]
   [list?                public]
   [length               public]
   [make-list            public]
   [reverse              public]
   [append               public]
   [list-ref             ADDME]
   [memq                 public]
   [assq                 public]
   [map                  public]
   [andmap               public]
   [ormap                ADDME]
   ;;; vectors
   [make-vector          public]
   [vector               public]
   [vector-length        public]
   [vector-ref           public]
   [vector-set!          public]
   [list->vector         public]
   [vector->list         public]
   ;;; strings
   [make-string          public]
   [string               public]
   [string-length        public]
   [string-ref           public]
   [string-set!          public]
   [list->string         public]
   [string->list         ADDME]
   ;;; symbols
   [gensym               public]
   [symbol->string       public]
   [string->symbol       public]
   [top-level-value      public]
   [top-level-bound?     public]
   [set-top-level-value! public]
   [oblist               public]
   ;;; eof
   [eof-object           public]
   [void                 public]
   ;;; control/debugging
   [error                public]
   [exit                 public]
   [apply                public]
   [make-parameter       public]
   ;;; output
   [output-port?         public]
   [console-output-port  public]
   [current-output-port  public]
   [standard-output-port public]
   [standard-error-port  public]
   [open-output-file     public]
   [close-output-port    public]
   [flush-output-port    public]
   [write-char           public]
   [output-port-name     public]
   [newline              public]
   ;;; input
   [input-port?          public]
   [standard-input-port  public]
   [console-input-port   public]
   [current-input-port   public]
   [open-input-file      public]
   [close-input-port     public]
   [read-char            public]
   [peek-char            public]
   [unread-char          public]
   [input-port-name      public]
   [write                public]
   [display              public]
   [read-token           public]
   [read                 public]
   ;;; evaluation
   [eval                 public]
   [current-eval         public]
   [load                 public]
   [new-cafe             public]
   ))

(define (public-primitives)
  (let f ([ls pcb-table])
    (cond
      [(null? ls) '()]
      [(eq? (cadar ls) 'public) 
       (cons (caar ls) (f (cdr ls)))]
      [else (f (cdr ls))])))
          
(define *pcb-set-marker* (gensym))

(define *pcb-ref-marker* (gensym))

(define (mark-pcb-set-found x)
  (putprop x *pcb-set-marker* #t))
  
(define (mark-pcb-ref-found x)
  (when (and (signal-error-on-undefined-pcb)
             (not (getprop x *pcb-set-marker*)))
    (error 'compile "found reference to unset primitive ~s" x))
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
(define-record var (name))
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

(define (recordize x)
  (define who 'recordize)
  (define (self-evaluating? x)
    (or (number? x) (boolean? x) (null? x) (char? x) (string? x)))
  (define (verify-proper-bindings b* expr)
    (unless (list? b*)
      (error 'parse "invalid bindings in expression ~s" expr))
    (for-each
      (lambda (x)
        (unless (and (list? x)
                     (= (length x) 2)
                     (symbol? (car x)))
          (error 'parse "invalid binding ~a in expresison ~a" x expr)))
      b*))
  (define (Internal body* r x)
    (when (null? body*) (error 'compile "No body in ~s" x))
    (let f ([fst (car body*)] [body* (cdr body*)] [bind* '()])
      (cond
        [(and (pair? fst) (eq? (car fst) 'define) 
              (not (assq 'define bind*))
              (not (assq 'define r)))
         (unless (and (list? fst) (= (length fst) 3))
           (error 'parse "malformed internal definition ~s in ~s" fst x))
         (unless (symbol? (cadr fst)) 
            (error 'parse "invalid name in ~s" fst))
         (when (null? body*)
           (error 'parse "no expression in body of ~s" x))
         (f (car body*) (cdr body*) (cons (cdr fst) bind*))]
        [(and (pair? fst) (eq? (car fst) 'begin) 
              (not (assq 'begin bind*))
              (not (assq 'begin r)))
         (let ([b* (cdr fst)])
           (unless (list? b*) (error 'parse "invalid begin ~s" fst))
           (let ([body* (append b* body*)])
             (when (null? body*) 
               (error 'parse "no expression in body of ~s" x))
             (f (car body*) (cdr body*) bind*)))]
        [else
         (let ([lhs* (map car bind*)] [rhs* (map cadr bind*)])
           (let ([name* (map unique-var lhs*)])
             (let ([r (append (map cons lhs* name*) r)])
               (let ([rhs*
                      (let f ([rhs* rhs*] [ac '()])
                        (cond
                          [(null? rhs*) ac]
                          [else
                           (f (cdr rhs*) (cons (Expr (car rhs*) r) ac))]))])
                 (build-letrec (reverse name*) rhs*
                    (list->seq (Expr* (cons fst body*) r)))))))])))
   (define (build-letrec lhs* rhs* body)
     (if (null? lhs*) 
         body
         (let ([tmp* (map (lambda (x) (make-var 'tmp)) lhs*)]) 
           (make-bind lhs* (map (lambda (x) (make-primcall 'void '())) lhs*)
             (make-bind tmp* rhs* 
               (make-seq (list->seq (map make-assign lhs* tmp*)) body))))))
  (define (list->seq e*)
    (let f ([ac (car e*)] [e* (cdr e*)])
      (cond
        [(null? e*) ac]
        [else (f (make-seq ac (car e*)) (cdr e*))])))
  (define (Expr* x* r)
    (cond
       [(null? x*) '()]
       [else
        (cons (Expr (car x*) r) (Expr* (cdr x*) r))]))
  (define (Expr x r)
    (cond
      [(self-evaluating? x) (make-constant x)]
      [(symbol? x)
       (cond
         [(assq x r) => cdr]
         [(primitive? x) (make-primref x)]
         [else (error 'recordize "unbound variable ~s" x)])]
      [(not (list? x))
       (error 'recordize "invalid expression ~s" x)]
      [(and (symbol? (car x)) (assq (car x) r)) =>
       (lambda (b)
         (make-funcall (cdr b) (Expr* (cdr x) r)))]
      [(eq? (car x) 'quote)
       (unless (= (length x) 2)
         (error who "invalid syntax ~s" 'quote))
       (make-constant (cadr x))]
      [(and (>= (length x) 2) (eq? (car x) 'begin))
       (list->seq (Expr* (cdr x) r))]
      [(eq? (car x) 'if)
       (unless (= (length x) 4) 
         (error who "invalid syntax ~s" x))
       (make-conditional (Expr (cadr x) r)
         (Expr (caddr x) r)
         (Expr (cadddr x) r))]
      [(and (eq? (car x) 'let) (pair? (cdr x)) (symbol? (cadr x)))
       ;; named let
       (unless (>= (length x) 4)
         (error 'compile "invalid let ~s" x))
       (let ([name (cadr x)] [bindings (caddr x)] [body* (cdddr x)])
         (verify-proper-bindings bindings x)
         (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
           (let ([n-name (make-var name)] [nrhs* (Expr* rhs* r)])
             (let ([r (cons (cons name n-name) r)]) 
               (let ([nlhs* (map make-var lhs*)])
                 (let ([r (append (map cons lhs* nlhs*) r)])
                   (make-funcall
                     (make-bind (list n-name)
                                (list (make-primcall 'void '()))
                       (make-seq 
                         (make-assign n-name
                           (make-function nlhs* #t 
                             (Internal body* r x)))
                         n-name))
                     nrhs*)))))))]
      [(eq? (car x) 'let)
       (unless (>= (length x) 3)
         (error 'compile "invalid let ~s" x))
       (let ([bindings (cadr x)] [body* (cddr x)])
         (verify-proper-bindings bindings x)
         (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
           (let ([nlhs* (map make-var lhs*)] [nrhs* (Expr* rhs* r)])
             (let ([r (append (map cons lhs* nlhs*) r)])
               (make-bind nlhs* nrhs* 
               (Internal body* r x))))))]
      [(and (>= (length x) 3) (eq? (car x) 'let*))
       (let ([bindings (cadr x)] [body* (cddr x)])
         (verify-proper-bindings bindings x)
         (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
           (let ([nlhs* (map make-var lhs*)])
             (let f ([lhs* lhs*] [nlhs* nlhs*] [rhs* rhs*] [r r])
               (cond
                 [(null? lhs*) (Internal body* r x)]
                 [else
                  (make-bind (list (car nlhs*))
                             (list (Expr (car rhs*) r))
                    (f (cdr lhs*)
                       (cdr nlhs*)
                       (cdr rhs*)
                       (cons (cons (car lhs*) (car nlhs*)) r)))])))))]
      [(and (>= (length x) 3) (eq? (car x) 'letrec))
       (let ([bindings (cadr x)] [body* (cddr x)])
         (verify-proper-bindings bindings x)
         (cond
           [(null? bindings) (list->seq (Expr* body* r))]
           [else
            (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
              (let ([nlhs* (map make-var lhs*)]
                    [tmp* (map make-var lhs*)]) 
                (let ([r (append (map cons lhs* nlhs*) r)])
                  (make-bind nlhs* (map (lambda (x) (make-primcall 'void '())) nlhs*)
                    (make-seq
                      (make-bind tmp* (Expr* rhs* r)
                        (list->seq (map make-assign nlhs* tmp*)))
                      (Internal body* r x))))))]))]
      [(and (>= (length x) 3) (eq? (car x) 'letrec*))
       (let ([bindings (cadr x)] [body* (cddr x)])
         (verify-proper-bindings bindings x)
         (cond
           [(null? bindings) (list->seq (Expr* body* r))]
           [else
            (let ([lhs* (map car bindings)] [rhs* (map cadr bindings)])
              (let ([nlhs* (map make-var lhs*)])
                (let ([r (append (map cons lhs* nlhs*) r)])
                  (make-bind nlhs* (map (lambda (x) (make-primcall 'void '())) nlhs*)
                    (make-seq
                       (list->seq
                         (map make-assign nlhs* (Expr* rhs* r)))
                       (Internal body* r x))))))]))]
      [(and (>= (length x) 3) (eq? (car x) 'lambda))
       (let ([arg* (cadr x)] [body* (cddr x)])
          (define (new-arg* arg*)
            (cond
              [(null? arg*) '()]
              [(symbol? arg*) (list (make-var arg*))]
              [else 
               (cons (make-var (car arg*)) (new-arg* (cdr arg*)))]))
          (define (verify-proper-args args expr)
            (define (proper-args args)
              (or (null? args)
                  (symbol? args)
                  (and (pair? args)
                       (symbol? (car args))
                       (proper-args (cdr args)))))
            (unless (proper-args args)
              (error 'parse "invalid arguments in ~s" expr)))
          (define (extend-args lhs* rhs* r)
            (cond
              [(null? lhs*) r]
              [(symbol? lhs*) (cons (cons lhs* (car rhs*)) r)]
              [else
               (extend-args (cdr lhs*) (cdr rhs*)
                 (cons (cons (car lhs*) (car rhs*)) r))]))
          (verify-proper-args arg* x)
          (let ([narg* (new-arg* arg*)])
            (let ([r (extend-args arg* narg* r)])
              (make-function narg* (list? arg*)
                (Internal body* r x)))))]
      [(eq? (car x) 'and)
       (if (null? (cdr x))
           (make-constant #t)
           (let f ([a (cadr x)] [d (cddr x)])
             (cond
               [(null? d) (Expr a r)]
               [else
                (make-conditional (Expr a r)
                  (f (car d) (cdr d))
                  (make-constant #f))])))]
      [(eq? (car x) 'or)
       (if (null? (cdr x))
           (make-constant #f)
           (let f ([a (cadr x)] [d (cddr x)])
             (cond
               [(null? d) (Expr a r)]
               [else
                (let ([t (make-var 'tmp)])
                  (make-bind (list t) (list (Expr a r))
                    (make-conditional t t (f (car d) (cdr d)))))])))]
      [(and (>= (length x) 3) (eq? (car x) 'when))
       (let ([test (cadr x)] [body* (cddr x)])
         (make-conditional (Expr test r)
           (list->seq (Expr* body* r))
           (make-primcall 'void '())))]
      [(and (>= (length x) 3) (eq? (car x) 'unless))
       (let ([test (cadr x)] [body* (cddr x)])
         (make-conditional (Expr test r)
           (make-primcall 'void '())
           (list->seq (Expr* body* r))))]
      [(and (>= (length x) 2) (eq? (car x) 'cond))
       (let f ([cls (cadr x)] [cls* (cddr x)])
         (cond
           [(not (list? cls))
            (error who "malformed cond clause ~s in ~s" cls x)]
           [(not (pair? cls))
            (error who "malformed cond clause ~s in ~s" cls x)]
           [(null? cls*)
            (cond
              [(and (eq? (car cls) 'else)
                    (not (assq 'else r)))
               (unless (>= (length cls) 2)
                 (error who "malformed cond else clause ~s in ~s" cls x))
               (list->seq (Expr* (cdr cls) r))]
              [(and (>= (length cls) 2)
                    (eq? (cadr cls) '=>)
                    (not (assq '=> r)))
               (unless (= (length cls) 3)
                 (error who "malformed cond last => clause ~s in ~s" cls x))
               (let ([t (make-var 'tmp)])
                 (make-bind (list t) (list (Expr (car cls) r))
                   (make-conditional t
                     (make-funcall (Expr (caddr cls) r) (list t))
                     (make-primcall 'void '()))))]
              [(= (length cls) 1)
               (let ([t (make-var 'tmp)])
                 (make-bind (list t) (list (Expr (car cls) r))
                   (make-conditional t t (make-primcall 'void '()))))]
              [else
               (make-conditional (Expr (car cls) r)
                 (list->seq (Expr* (cdr cls) r))
                 (make-primcall 'void '()))])]
           [else
            (cond
             [(and (>= (length cls) 2)
                  (eq? (cadr cls) '=>)
                  (not (assq '=> r)))
              (unless (= (length cls) 3)
                (error who "malformed cond => clause ~s in ~s" cls x))
              (let ([t (make-var 'tmp)])
                (make-bind (list t) (list (Expr (car cls) r))
                  (make-conditional t
                    (make-funcall (Expr (caddr cls) r) (list t))
                    (f (car cls*) (cdr cls*)))))]
             [(= (length cls) 1)
              (let ([t (make-var 'tmp)])
                (make-bind (list t) (list (Expr (car cls) r))
                  (make-conditional t t 
                     (f (car cls*) (cdr cls*)))))]
             [else
              (make-conditional (Expr (car cls) r)
                (list->seq (Expr* (cdr cls) r))
                (f (car cls*) (cdr cls*)))])]))]
      [(and (= (length x) 3) (eq? (car x) 'set!))
       (let ([var (cadr x)] [val (caddr x)])
         (unless (symbol? var)
           (error who "invalid syntax in ~s" x))
         (cond
           [(assq var r) =>
            (lambda (p)
              (make-assign (cdr p) (Expr val r)))]
           [else 
            (error who "unbound variable ~s in ~s" var x)]))]
      [(and (eq? (car x) '$apply))
       (unless (>= (length (cdr x)) 2)
         (error who "insufficient arguments to $apply in ~s" x))
       (let ([rator (cadr x)] [rand* (cddr x)])
         (make-appcall (Expr rator r) (Expr* rand* r)))]
      [(eq? (car x) 'foreign-call)
       (unless (and (>= (length x) 2) (string? (cadr x)))
         (error who "invalid syntax ~s" x))
       (make-forcall (cadr x) (Expr* (cddr x) r))]
      [(eq? (car x) '$pcb-set!)
       (unless (= (length x) 3)
         (error who "incorrect number of args in ~s" x))
       (mark-pcb-set-found (cadr x))
       (make-primcall '$pcb-set! 
         (list (make-constant (pcb-index (cadr x))) (Expr (caddr x) r)))]
      [else 
       (make-funcall (Expr (car x) r) (Expr* (cdr x) r))]))
  (Expr x '()))

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
      [else (error 'unparse "invalid record ~s" x)]))
  (E x))

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
          free prog))
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
                          (let ([g (unique-var 'code)])
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
      [(and (symbol? x) (getprop x *symbol-key*)) =>
       (lambda (t) (make-primcall '$constant-ref (list t)))]
      [(symbol? x)
       (let ([t (make-var 'constant)]
             [v (symbol-convert x)])
         (set! symbols-lhs* (cons t symbols-lhs*))
         (set! symbols-rhs* (cons v symbols-rhs*))
         (putprop x *symbol-key* t)
         (make-primcall '$constant-ref (list t)))]
      [else
       (let ([t (make-var 'constant)]
             [v (convert x)])
         (set! complex-lhs* (cons t complex-lhs*))
         (set! complex-rhs* (cons v complex-rhs*))
         (make-primcall '$constant-ref (list t)))]))
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
        [(or (boolean? c) (integer? c) (char? c) (null? c)) 
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
      (let ([init-lhs (make-var 'init)]
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
      [(fixnum? boolean? char? vector? string? procedure? null? pair? not
        cons eq? vector symbol? error eof-object eof-object? void
        $unbound-object?)
       '#t]
      [($fxadd1 $fxsub1 $fxzero? $fxlognot $fxlogor $fxlogand $fx+ $fx- $fx* 
        $fx= $fx< $fx<= $fx> $fx>= $fxquotient $fxsll $fxsra $fxlogxor $exit) 
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
      [($symbol-string)
       (andmap (check op symbol?) rand*)]
      [($constant-ref $set-constant! $intern $pcb-set! $pcb-ref $make-symbol
        $symbol-value $set-symbol-value!) #t]
      [else (error 'valid-arg-types? "unhandled op ~s" op)]))
  (and (valid-arg-count? op rand*)
       (valid-arg-types? op rand*)))


(define (simplify-operands x)
  (define who 'simplify-operands)
  (define (simple? x) 
    (or (constant? x) (var? x) (primref? x)))
  (define (Expr x)
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
    (record-case x
      [(constant) x]
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
  (define (CodeExpr x)
    (record-case x 
      [(code fml* proper free* body)
       (make-code fml* proper free* (Expr body))]))
  (define (CodesExpr x)
    (record-case x 
      [(codes lhs* rhs* body)
       (make-codes lhs* (map CodeExpr rhs*) (Expr body))]))
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
  (define disp-symbol-string 0)
  (define disp-symbol-value 4)
  (define symbol-size 8)
  
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
  (define object-alignment 8)
  (define align-shift 3))


       
(define (generate-code x main-name)
  (define who 'generate-code)
  (define (argc-convention n)
    (- (fxsll n fx-shift)))
  (define (align n)
    (fxsll (fxsra (+ n object-alignment -1) align-shift) align-shift))
  (define (genlabel) (label (unique-label)))
  (define unique-label
    (let ([count 0])
      (lambda ()
        (let ([L (format "L_~a_~a" main-name count)])
          (set! count (add1 count))
          L))))
  (define (mem off val) (list 'mem off val))
  (define (int x) (list 'integer x))
  (define (movzbl src targ) (list 'movzbl src targ))
  (define (sall src targ) (list 'sall src targ))
  (define (sarl src targ) (list 'sarl src targ))
  (define (shll src targ) (list 'shll src targ))
  (define (shrl src targ) (list 'shrl src targ))
  (define (notl src) (list 'notl src))
  (define (orl src targ) (list 'orl src targ))
  (define (xorl src targ) (list 'xorl src targ))
  (define (andl src targ) (list 'andl src targ))
  (define (movl src targ) (list 'movl src targ))
  (define (movs src targ) (list 'movswl src targ))
  (define (movb src targ) (list 'movb src targ))
  (define (addl src targ) (list 'addl src targ))
  (define (imull src targ) (list 'imull src targ))
  (define (idivl src) (list 'idivl src))
  (define (subl src targ) (list 'subl src targ))
  (define (push src) (list 'push src))
  (define (pop targ) (list 'pop targ))
  (define (sete targ) (list 'sete targ))
  (define (call targ) (list 'call targ))
  (define (tail-indirect-cpr-call)
    (jmp (list 'indirect (mem (- disp-closure-code closure-tag) cpr))))
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
  (define cl '(register %cl))
  (define eax '(register %eax))
  (define ebx '(register %ebx))
  (define ecx '(register %ecx))
  (define edx '(register %edx))
  (define apr '(register %ebp))
  (define fpr '(register %esp))
  (define cpr '(register %edi))
  (define pcr '(register %esi))
  (define (constant-val x)
    (cond
      [(fixnum? x) (int (ash x fx-shift))]
      [(boolean? x) (int (if x bool-t bool-f))]
      [(null? x) (int nil)]
      [(char? x) (int (+ (ash (char->integer x) char-shift) char-tag))]
      [else (error 'constant-val "invalid immcprate ~s" x)]))
  (define (primref-loc op)
    (mem (* (pcb-index op) wordsize) pcr))
  (define (immcprate-rep x)
    (cond
      [(fixnum? x) (ash x fx-shift)]
      [(boolean? x) (if x bool-t bool-f)]
      [(null? x) nil]
      [(char? x) (+ (ash (char->integer x) char-shift) char-tag)]
      [else (error 'immcprate-rep "invalid immcprate ~s" x)]))
  (define (bool-bit-to-boolean ac)
    (list* 
      (movzbl al eax)
      (shll (int bool-shift) eax)
      (orl (int bool-tag) eax)
      ac))
  (define (var-loc x r)
    (cond
      [(assq x r) => cdr]
      [else (error 'var-loc "unbound var ~s" x)]))
  (define (label-loc x r)
    (cond
      [(assq x r) => cdr]
      [else (error 'label-loc "unbound var ~s" x)]))
  (define (cond-branch op Lt Lf ac)
    (define (opposite x)
      (cadr (assq x '([je jne] [jl jge] [jle jg] [jg jle] [jge jl]))))
    (unless (or Lt Lf)
      (error 'cond-branch "no labels"))
    (cond
      [(not Lf) (cons (list op Lt) ac)]
      [(not Lt) (cons (list (opposite op) Lf) ac)] 
      [else (list* (list op Lt) (jmp Lf) ac)]))
  (define (type-pred mask tag rand* Lt Lf si r ac)
    (cond
      [mask
       (list* 
         (movl (Simple (car rand*) r) eax)
         (andl (int mask) eax)
         (cmpl (int tag) eax)
         (cond-branch 'je Lt Lf ac))]
      [else 
       (let ([v (Simple (car rand*) r)])
         (cond
           [(memq (car v) '(mem register)) 
            (list*
              (cmpl (int tag) (Simple (car rand*) r))
              (cond-branch 'je Lt Lf ac))]
           [else
            (list*
              (movl (Simple (car rand*) r) eax)
              (cmpl (int tag) eax)
              (cond-branch 'je Lt Lf ac))]))])) 
  (define (compare-and-branch op rand* Lt Lf si r ac)
    (define (opposite x)
      (cadr (assq x '([je je] [jl jg] [jle jge] [jg jl] [jge jle]))))
    (cond
     [(constant? (cadr rand*))
      (list* 
        (cmpl (Simple (cadr rand*) r) (Simple (car rand*) r))
        (cond-branch op Lt Lf ac))]
     [(constant? (car rand*))
      (list*
        (cmpl (Simple (car rand*) r) (Simple (cadr rand*) r))
        (cond-branch (opposite op) Lt Lf ac))]
     [else
      (list* 
        (movl (Simple (car rand*) r) eax)
        (cmpl (Simple (cadr rand*) r) eax)
        (cond-branch op Lt Lf ac))]))
  (define (do-pred-prim op rand* Lt Lf si r ac)
    (case op
     [(fixnum?)    (type-pred fx-mask fx-tag rand* Lt Lf si r ac)]
     [(pair?)      (type-pred pair-mask pair-tag rand* Lt Lf si r ac)]
     [(char?)      (type-pred char-mask char-tag rand* Lt Lf si r ac)]
     [(vector?)    (type-pred vector-mask vector-tag rand* Lt Lf si r ac)]
     [(string?)    (type-pred string-mask string-tag rand* Lt Lf si r ac)]
     [(symbol?)    (type-pred symbol-mask symbol-tag rand* Lt Lf si r ac)]
     [(procedure?) (type-pred closure-mask closure-tag rand* Lt Lf si r ac)]
     [(boolean?)   (type-pred bool-mask bool-tag rand* Lt Lf si r ac)]
     [(null?)      (type-pred #f nil rand* Lt Lf si r ac)]
     [($unbound-object?) (type-pred #f unbound rand* Lt Lf si r ac)]
     [(not)        (type-pred #f bool-f rand* Lt Lf si r ac)]
     [(eof-object?) (type-pred #f eof rand* Lt Lf si r ac)]
     [($fxzero?)   (type-pred #f 0 rand* Lt Lf si r ac)]
     [($fx= $char= eq?) (compare-and-branch 'je rand* Lt Lf si r ac)]
     [($fx< $char<)     (compare-and-branch 'jl rand* Lt Lf si r ac)]
     [($fx<= $char<=)   (compare-and-branch 'jle rand* Lt Lf si r ac)]
     [($fx> $char>)     (compare-and-branch 'jg rand* Lt Lf si r ac)]
     [($fx>= $char>=)   (compare-and-branch 'jge rand* Lt Lf si r ac)]
     [($vector-ref) 
      (do-value-prim op rand* si r
        (do-simple-test eax Lt Lf ac))]
     [(cons void)
      ;;; always true
      (do-effect-prim op rand* si r
        (cond
          [(not Lt) ac]
          [else (cons (jmp Lt) ac)]))]
     [else 
      (error 'pred-prim "HERE unhandled ~s" op)]))
  (define (do-pred->value-prim op rand* si r ac)
    (case op
      [else
       (let ([Lf (genlabel)] [Lj (genlabel)])
         (do-pred-prim op rand* #f Lf si r
           (list* (movl (constant-val #t) eax)
                  (jmp Lj)
                  Lf
                  (movl (constant-val #f) eax)
                  Lj
                  ac)))]))
  (define (indirect-ref arg* off si r ac)
    (list*
      (movl (Simple (car arg*) r) eax)
      (movl (mem off eax) eax)
      ac))
  (define (do-value-prim op arg* si r ac)
    (case op
      [(eof-object) (cons (movl (int eof) eax) ac)]
      [(void)       (cons (movl (int void-object) eax) ac)]
      [($fxadd1) 
       (list* (movl (Simple (car arg*) r) eax)
              (addl (constant-val 1) eax)
              ac)]
      [($fxsub1) 
       (list* (movl (Simple (car arg*) r) eax)
              (addl (constant-val -1) eax)
              ac)]
      [($fx+) 
       (list* (movl (Simple (car arg*) r) eax)
              (addl (Simple (cadr arg*) r) eax)
              ac)]
      [($fx-) 
       (list* (movl (Simple (car arg*) r) eax)
              (subl (Simple (cadr arg*) r) eax)
              ac)]
      [($fx*)
       (cond
         [(constant? (car arg*))
          (record-case (car arg*)
            [(constant c)
             (unless (integer? c)
               (error who "invalid arg ~s to fx*" c))
             (list* (movl (Simple (cadr arg*) r) eax)
                    (imull (int c) eax)
                    ac)])]
         [(constant? (cadr arg*))
          (record-case (cadr arg*)
            [(constant c)
             (unless (integer? c)
               (error who "invalid arg ~s to fx*" c))
             (list* (movl (Simple (car arg*) r) eax)
                    (imull (int c) eax)
                    ac)])]
         [else
          (list* (movl (Simple (car arg*) r) eax)
                 (shrl (int fx-shift) eax)
                 (imull (simple (cadr arg*) r) eax)
                 ac)])]
      [($fxquotient)
       (list* (movl (Simple (car arg*) r) eax)
              (cltd)
              (idivl (Simple (cadr arg*) r))
              (sall (int fx-shift) eax)
              ac)]
      [($fxlogor) 
       (list* (movl (Simple (car arg*) r) eax)
              (orl (Simple (cadr arg*) r) eax)
              ac)]
      [($fxlogand) 
       (list* (movl (Simple (car arg*) r) eax)
              (andl (Simple (cadr arg*) r) eax)
              ac)]
      [($fxlogxor) 
       (list* (movl (Simple (car arg*) r) eax)
              (xorl (Simple (cadr arg*) r) eax)
              ac)]
      [($fxsra)
       (record-case (cadr arg*)
         [(constant i)
          (unless (fixnum? i) (error who "invalid arg to fxsra"))
          (list* (movl (Simple (car arg*) r) eax)
                 (sarl (int (+ i fx-shift)) eax)
                 (sall (int fx-shift) eax)
                 ac)]
         [else
          (list* (movl (Simple (car arg*) r) eax)
                 (movl (Simple (cadr arg*) r) ecx)
                 (sarl (int fx-shift) ecx)
                 (sarl (int fx-shift) eax)
                 (sarl cl eax)
                 (sall (int fx-shift) eax)
                 ac)])]
      [($fxsll)
       (record-case (cadr arg*)
         [(constant i)
          (unless (fixnum? i) (error who "invalid arg to fxsll"))
          (list* (movl (Simple (car arg*) r) eax)
                 (sall (int i) eax)
                 ac)]
         [else
          (list* (movl (Simple (car arg*) r) eax)
                 (movl (Simple (cadr arg*) r) ecx)
                 (sarl (int fx-shift) ecx)
                 (sall cl eax)
                 ac)])]
      [($fixnum->char)  
       (list* (movl (Simple (car arg*) r) eax)
              (shll (int (- char-shift fx-shift)) eax)
              (orl (int char-tag) eax)
              ac)]
      [($char->fixnum) 
       (list* (movl (Simple (car arg*) r) eax)
              (shrl (int (- char-shift fx-shift)) eax)
              ac)]
      [($fxlognot) 
       (list* (movl (Simple (car arg*) r) eax)
              (orl (int fx-mask) eax)
              (notl eax)
              ac)]
      [($car) (indirect-ref arg* (- disp-car pair-tag) si r ac)]
      [($cdr) (indirect-ref arg* (- disp-cdr pair-tag) si r ac)]
      [($vector-length) 
       (indirect-ref arg* (- disp-vector-length vector-tag) si r ac)]
      [($string-length) 
       (indirect-ref arg* (- disp-string-length string-tag) si r ac)]
      [($symbol-string) 
       (indirect-ref arg* (- disp-symbol-string symbol-tag) si r ac)]
      [($symbol-value) 
       (indirect-ref arg* (- disp-symbol-value symbol-tag) si r ac)]
      [($constant-ref)
       (list* (movl (Simple (car arg*) r) eax) ac)]
      [($vector-ref) 
       (list* (movl (Simple (car arg*) r) ebx)
              (addl (Simple (cadr arg*) r) ebx)
              (movl (mem (- disp-vector-data vector-tag) ebx) eax)
              ac)]
      [($string-ref) 
       (list* (movl (Simple (cadr arg*) r) ebx)
              (shrl (int fx-shift) ebx)
              (addl (Simple (car arg*) r) ebx)
              (movl (int char-tag) eax)
              (movb (mem (- disp-string-data string-tag) ebx) ah)
              ac)]
      [($string-ref-8+0) 
       (list* (movl (Simple (cadr arg*) r) ebx)
              (addl (Simple (car arg*) r) ebx)
              (movl (int 0) eax)
              (movb (mem (- disp-string-data string-tag) ebx) ah)
              (sall (int fx-shift) eax)
              ac)]
      [($string-ref-8+2) 
       (list* (movl (Simple (cadr arg*) r) ebx)
              (addl (Simple (car arg*) r) ebx)
              (movl (int 0) eax)
              (movb (mem (- (+ 16 disp-string-data) string-tag) ebx) ah)
              (sall (int fx-shift) eax)
              ac)]
      [($string-ref-16+0) 
       (list* (movl (Simple (cadr arg*) r) ebx)
              (addl (Simple (car arg*) r) ebx)
              (movs (mem (- disp-string-data string-tag) ebx) eax)
              (sall (int fx-shift) eax)
              ac)] 
      [($string-ref-16+1) 
       (list* (movl (Simple (cadr arg*) r) ebx)
              (addl (Simple (car arg*) r) ebx)
              (movs (mem (- (+ 16 disp-string-data) string-tag) ebx) eax)
              (sall (int fx-shift) eax)
              ac)]
      [($make-string) 
       (list* (movl (Simple (car arg*) r) eax)
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
       (list* (movl (Simple (car arg*) r) eax)
              (movl eax (mem disp-vector-length apr))
              (movl apr eax)
              (addl (int vector-tag) eax)
              (addl (mem disp-vector-length apr) apr)
              (addl (int (+ disp-vector-data object-alignment -1)) apr)
              (sarl (int align-shift) apr)
              (sall (int align-shift) apr)
              ac)]
      [(cons)
       (list* (movl (Simple (car arg*) r) eax)
              (movl (Simple (cadr arg*) r) ebx)
              (movl eax (mem disp-car apr))
              (movl apr eax)
              (movl ebx (mem disp-cdr apr))
              (addl (int pair-tag) eax)
              (addl (int (align pair-size)) apr)
              ac)]
      [($make-symbol)
       (list* (movl (Simple (car arg*) r) eax)
              (movl (int unbound) (mem disp-symbol-value apr))
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
            (list* (movl (Simple (car arg*) r) eax)
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
              (list* (movl (Simple (car arg*) r) ebx)
                     (movb bh (mem idx apr))
                     (f (cdr arg*) (add1 idx)))])]))]
     [($pcb-set! $set-car! $set-cdr! $vector-set! $string-set! $exit
       $set-symbol-value!) 
      (do-effect-prim op arg* si r
        (cons (movl (int void-object) eax) ac))]
     [(fixnum? $fxzero? boolean? char? pair? vector? string? symbol?
       procedure? null? not eof-object? $fx= $fx< $fx<= $fx> $fx>= eq?
       $char= $char< $char<= $char> $char>= $unbound-object?) 
      (do-pred->value-prim op arg* si r ac)]
     [else
      (error 'value-prim "unhandled ~s" op)]))
  (define (do-effect-prim op arg* si r ac)
    (case op
     [($vector-set!) 
      (list* (movl (Simple (car arg*) r) ebx)
             (addl (Simple (cadr arg*) r) ebx)
             (movl (Simple (caddr arg*) r) eax)
             (movl eax (mem (- disp-vector-data vector-tag) ebx))
             ac)]
     [($string-set!) 
      (list* (movl (Simple (cadr arg*) r) eax)
             (shrl (int fx-shift) eax)
             (addl (Simple (car arg*) r) eax)
             (movl (Simple (caddr arg*) r) ebx)
             (movb bh (mem (- disp-string-data string-tag) eax))
             ac)]
     [($set-constant!) 
      (NonTail (cadr arg*) si r
        (list* (movl eax (label-loc (car arg*) r)) ac))]
     [($pcb-set!)
      (let ([loc (car arg*)] [val (cadr arg*)])
        (record-case loc
          [(constant i) 
           (unless (fixnum? i) (error who "invalid loc ~s" loc))
           (list* (movl (Simple val r) eax)
                  (movl eax (mem (* i wordsize) pcr))
                  ac)]
         [else (error who "invalid loc ~s" loc)]))]
    [($set-car!) 
     (list* (movl (Simple (car arg*) r) eax)
            (movl (Simple (cadr arg*) r) ebx)
            (movl ebx (mem (- disp-car pair-tag) eax))
            ac)]
    [($set-cdr!) 
     (list* (movl (Simple (car arg*) r) eax)
            (movl (Simple (cadr arg*) r) ebx)
            (movl ebx (mem (- disp-cdr pair-tag) eax))
            ac)]
    [($set-symbol-value!) 
     (list* (movl (Simple (car arg*) r) eax)
            (movl (Simple (cadr arg*) r) ebx)
            (movl ebx (mem (- disp-symbol-value symbol-tag) eax))
            ac)]
    [($exit) 
     (list* (movl (Simple (car arg*) r) eax)
            (movl (mem 4 pcr) fpr)
            (addl (int (- wordsize)) fpr)
            (ret)
            ac)]
    [(cons void)
     (let f ([arg* arg*])
       (cond
         [(null? arg*) ac]
         [else 
          (Effect (car arg*) si r (f (cdr arg*)))]))]
    [else 
     (error 'do-effect-prim "unhandled op ~s" op)]))
  (define (do-foreign-call op rand* si r ac)
    (let f ([rand* (reverse rand*)] [si si])
      (cond
        [(null? rand*) 
         (list* (addl (int (+ si wordsize)) fpr)
                (call (label op))
                (subl (int (+ si wordsize)) fpr)
                ac)]
        [else
         (NonTail (car rand*) si r
           (cons (movl eax (mem si fpr))
                 (f (cdr rand*) (- si wordsize))))])))
  (define (setup-arguments rator rand* si r ac)
    (cond
      [(null? rand*)
       (NonTail rator si r ac)]
      [else
       (NonTail (car rand*) si r
         (cons
           (movl eax (mem si fpr))
           (setup-arguments rator (cdr rand*) (- si wordsize) r ac)))]))
  (define (do-nontail-call style rator rand* si r ac)
    (setup-arguments rator rand* (- si (* 2 wordsize)) r
      (list* (movl cpr (mem si fpr))
             (movl eax cpr)
             (andl (int closure-mask) eax)
             (cmpl (int closure-tag) eax)
             (jne (label "L_nonprocedure"))
             (addl (list 'integer si) fpr)
             (movl (int (argc-convention (length rand*))) eax)
             (case style
               [(funcall)
                (call (list 'indirect 
                        (mem (- disp-closure-code closure-tag) cpr)))]
               [(appcall)
                (call (label "L_apply"))]
               [else (error who "unsupported style ~s" style)])
             (subl (list 'integer si) fpr)
             (movl (mem si fpr) cpr)
             ac)))
  (define (do-tail-call style rator rand* si r ac)
    (setup-arguments rator rand* si r
      (list* (movl eax cpr)
             (andl (int closure-mask) eax)
             (cmpl (int closure-tag) eax)
             (jne (label "L_nonprocedure"))
             (let f ([di (- wordsize)] [si si] [r* rand*])
               (cond
                 [(null? r*)
                  (list* (movl (int (argc-convention (length rand*))) eax)
                         (case style
                           [(funcall)
                            (tail-indirect-cpr-call)]
                           [(appcall)
                            (jmp (label "L_apply"))]
                           [else (error who "unsupported style ~s" style)])
                         ac)]
                 [else
                  (list* (movl (mem si fpr) eax)
                         (movl eax (mem di fpr))
                         (f (- di wordsize) (- si wordsize) (cdr r*)))])))))
  (define (do-bind lhs* rhs* si r k)
    (cond
     [(null? lhs*) (k si r)]
     [else
      (NonTail (car rhs*) si r
        (cons (movl eax (mem si fpr))
          (do-bind (cdr lhs*) (cdr rhs*) (- si wordsize)
                   (cons (cons (car lhs*) (mem si fpr)) r)
                   k)))]))
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
  (define (Simple x r)
    (record-case x
     [(constant c) (constant-val c)]
     [(var) 
      (cond
        [(assq x r) => cdr]
        [else (error 'Simple "unbound var ~s" x)])]
     [(primref op) (primref-loc op)]
     [else (error 'Simple "what ~s" x)]))
  (define (NonTail x si r ac) 
    (record-case x
      [(var) 
       (cons (movl (Simple x r) eax) ac)]
      [(constant c)
       (cons (movl (constant-val c) eax) ac)]
      [(primref c)
       (cons (movl (primref-loc c) eax) ac)]
      [(primcall op rand*)
       (do-value-prim op rand* si r ac)]
      [(forcall op rand*)
       (do-foreign-call op rand* si r ac)]
      [(closure label arg*)
       (let f ([arg* arg*] [off disp-closure-data])
         (cond
           [(null? arg*)
            (list* (movl (label-loc label r) (mem 0 apr))
                   (movl apr eax)
                   (addl (int (align off)) apr)
                   (addl (int closure-tag) eax)
                   ac)]
           [else
            (list* (movl (Simple (car arg*) r) eax)
                   (movl eax (mem off apr))
                   (f (cdr arg*) (+ off wordsize)))]))]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* si r
         (lambda (si r)
           (NonTail body si r ac)))]
      [(conditional test conseq altern)
       (let ([Lj (genlabel)] [Lf (genlabel)])
         (Pred test #f Lf si r
           (NonTail conseq si r
             (list* (jmp Lj) Lf (NonTail altern si r (cons Lj ac))))))]
      [(seq e0 e1)
       (Effect e0 si r (NonTail e1 si r ac))]
      [(funcall rator rand*)
       (do-nontail-call 'funcall rator rand* si r ac)]
      [(appcall rator rand*)
       (do-nontail-call 'appcall rator rand* si r ac)]
      [else (error 'NonTail "invalid expression ~s" x)]))
  (define (Pred x Lt Lf si r ac)
    (record-case x
      [(var) 
       (do-simple-test (var-loc x r) Lt Lf ac)]
      [(constant c) 
       (if c
           (if Lt (cons (jmp Lt) ac) ac)
           (if Lf (cons (jmp Lf) ac) ac))]
      [(primcall op rand*)
       (do-pred-prim op rand* Lt Lf si r ac)]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* si r
         (lambda (si r)
           (Pred body Lt Lf si r ac)))]
      [(conditional test conseq altern)
       (cond
         [(not Lt) 
          (let ([Lj^ (genlabel)] [Lf^ (genlabel)])
            (Pred test #f Lf^ si r
              (Pred conseq Lj^ Lf si r
                (cons Lf^
                  (Pred altern #f Lf si r 
                    (cons Lj^ ac))))))]
         [(not Lf) 
          (let ([Lj^ (genlabel)] [Lf^ (genlabel)])
            (Pred test #f Lf^ si r
              (Pred conseq Lt Lj^ si r
                (cons Lf^
                  (Pred altern Lt #f si r 
                    (cons Lj^ ac))))))]
         [else 
          (let ([Lf^ (genlabel)])
            (Pred test #f Lf^ si r
              (Pred conseq Lt Lf si r
                (cons Lf^
                  (Pred altern Lt Lf si r ac)))))])] 
      [(seq e0 e1)
       (Effect e0 si r (Pred e1 Lt Lf si r ac))]
      [(forcall op rand*)
       (do-foreign-call op rand* si r 
         (do-simple-test eax Lt Lf ac))]
      [(funcall rator rand*)
       (do-nontail-call 'funcall rator rand* si r 
         (do-simple-test eax Lt Lf ac))]
      [(appcall rator rand*)
       (do-nontail-call 'appcall rator rand* si r 
         (do-simple-test eax Lt Lf ac))]
      [else (error 'Pred "invalid expression ~s" x)]))
  (define (Effect x si r ac)
   (record-case x
     [(var) ac]
     [(constant) ac]
     [(primcall op rand*)
      (do-effect-prim op rand* si r ac)]
     [(forcall op rand*)
      (do-foreign-call op rand* si r ac)]
     [(bind lhs* rhs* body)
      (do-bind lhs* rhs* si r
        (lambda (si r)
          (Effect body si r ac)))]
     [(conditional test conseq altern)
      (let ([Lf (genlabel)] [Ljoin (genlabel)])
        (Pred test #f Lf si r
          (Effect conseq si r
            (list* (jmp Ljoin) Lf (Effect altern si r (cons Ljoin ac))))))]
     [(seq e0 e1)
      (Effect e0 si r (Effect e1 si r ac))]
     [(funcall rator rand*)
      (do-nontail-call 'funcall rator rand* si r ac)]
     [(appcall rator rand*)
      (do-nontail-call 'appcall rator rand* si r ac)]
     [else (error 'Effect "invalid expression ~s" x)]))
  (define (Tail x si r ac) 
    (record-case x
     [(var)
      (NonTail x si r (cons (ret) ac))]
     [(constant)
      (NonTail x si r (cons (ret) ac))]
     [(primcall op rand*)
      (do-value-prim op rand* si r (cons (ret) ac))]
     [(forcall)
      (NonTail x si r (cons (ret) ac))]
     [(closure)
      (NonTail x si r (cons (ret) ac))]
     [(bind lhs* rhs* body)
      (do-bind lhs* rhs* si r
        (lambda (si r)
          (Tail body si r ac)))]
     [(conditional test conseq altern)
      (let ([L (genlabel)])
        (Pred test #f L si r
          (Tail conseq si r
            (cons L (Tail altern si r ac)))))]
     [(seq e0 e1)
      (Effect e0 si r (Tail e1 si r ac))]
     [(funcall rator rand*)
      (do-tail-call 'funcall rator rand* si r ac)]
     [(appcall rator rand*)
      (do-tail-call 'appcall rator rand* si r ac)]
     [else (error 'Tail "invalid expression ~s" x)]))
  (define (bind-free free* idx r)
    (cond
     [(null? free*) r]
     [else
      (bind-free
        (cdr free*)
        (+ idx wordsize)
        (cons 
          (cons
            (car free*)
            (mem (+ idx disp-closure-data (- closure-tag)) cpr))
          r))]))
  (define (bind-fml* fml* si r)
    (cond
     [(null? fml*) (values si r)]
     [else
      (bind-fml* (cdr fml*) (- si wordsize)
        (cons (cons (car fml*) (mem si fpr)) r))]))
  (define (handle-vararg fml-count ac)
    (define CONTINUE_LABEL (unique-label))
    (define DONE_LABEL (unique-label))
    (list* (movl (int nil) ebx)
           (cmpl (int (argc-convention (sub1 fml-count))) eax)
           (jg (label "L_invalid_args"))
           (je (label DONE_LABEL))
           (label CONTINUE_LABEL)
           (movl ebx (mem disp-cdr apr))
           (movl (mem fpr eax) ebx)
           (movl ebx (mem disp-car apr))
           (movl apr ebx)
           (addl (int pair-tag) ebx)
           (addl (int pair-size) apr)
           (addl (int (fxsll 1 fx-shift)) eax)
           (cmpl (int (- (fxsll fml-count fx-shift))) eax)
           (jle (label CONTINUE_LABEL))
           (label DONE_LABEL)
           (movl ebx (mem (- (fxsll fml-count fx-shift)) fpr))
           ac))
  (define (handle-procedure-entry proper fml-count ac)
    (cond
      [proper 
       (list* (cmpl (int (argc-convention fml-count)) eax)
              (jne (label "L_invalid_args"))
              ac)]
      [else (handle-vararg fml-count ac)]))
  (define (emit-code r)
    (lambda (label x)
      (record-case x 
        [(code fml* proper free* body)
         (let ([r (bind-free free* 0 r)])
           (let-values ([(si r) (bind-fml* fml* (- wordsize) r)])
             (list* 'local-function label
               (handle-procedure-entry proper (length fml*)
                 (Tail body si r '())))))])))
  (define (emit-codes prog r)
    (record-case prog
      [(codes lhs* rhs* body)
       (let ([label* (map (lambda (x) (unique-label)) lhs*)])
         (let ([r (append (map cons lhs* (map label-address label*)) r)])
           (let ([procs (map (emit-code r) label* rhs*)]
                 [main-proc 
                  (list* 'local-function "L_scheme_entry" 
                         (Tail body (- wordsize) r '()))])
             (cons main-proc procs))))]))
  (define (emit-constants prog)
    (record-case prog
      [(constants lhs* body)
       (let ([label* (map (lambda (x) (unique-label)) lhs*)])
         (let ([r (map cons lhs* (map label label*))])
           (append
             (map (lambda (x) (list 'data x)) label*)
             (emit-codes body r))))]))
  (define (emit-prog prog main-name)
    (list*
      (list 'public-function 
           (format "~a_entry" main-name)
           (movl (mem 4 esp) eax) ; pcb
           (push ebx)
           (push esi)
           (push edi)
           (push ebp)
           (movl eax pcr)
           (movl (mem 0 pcr) apr) ; allocation pointer is always first
           (movl esp (mem 8 pcr)) ; save system stack
           (movl (mem 4 pcr) esp) ; load scheme stacks
           (call (label "L_scheme_entry"))
           (movl apr (mem 0 pcr)) ; save new ap
           (movl (mem 8 pcr) esp) ; restore system stack
           (pop ebp)
           (pop edi)
           (pop esi)
           (pop ebx)
           (ret))
      (list 'local-function
            "L_apply"
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
      (list 'local-function
             "L_error_table"
             ;;;
             (label "L_nonprocedure")
             (movl cpr (mem (- wordsize) fpr)) ; first arg
             (movl (mem (* (pcb-index '$apply-nonprocedure-error-handler)
                           wordsize)
                        pcr) 
                   cpr)
             (movl (int (argc-convention 1)) eax)
             (tail-indirect-cpr-call)
            ;;;
            (label "L_invalid_args")
            (movl cpr (mem (- wordsize) fpr)) ; first arg
            (negl eax)
            (movl eax (mem (- (* 2 wordsize)) fpr))
            (movl (mem (* (pcb-index '$incorrect-args-error-handler) wordsize)
                       pcr) 
                  cpr)
            (movl (int (argc-convention 2)) eax)
            (tail-indirect-cpr-call))
      (emit-constants prog)))
  (emit-prog x main-name))


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
      [(pop movl movswl movb push call ret cltd
        cmpl je jne jl jle jg jge jb jbe ja jae
        jmp sete setl setle setg setge movzbl
        addl subl orl xorl andl notl shll shrl sall sarl imull idivl negl)
       (emit-generic x)]
      [(label) (emit "~a:" (cadr x))]
      [(comment) (emit "/* ~s */" (cadr x))]
      [else (error 'emit-instruction "unsupported instruction ~s" (car x))]))
  (define (emit-function-header x)
    (let ([t (car x)] [label (cadr x)])
      (emit ".text")
      (when (eq? t 'public-function)
       (emit ".globl ~a" label))
      (emit ".type ~a @function" label)
      (emit "~a:" label)))
  (define (emit-function x)
    (emit-function-header x)
    (for-each emit-instruction (cddr x)))
  (define (emit-data x)
    (let ([t (car x)] [label (cadr x)])
      (emit ".data")
      (emit ".align 4")        
      (emit ".type ~a, @object" label)
      (emit ".size ~a, 4" label)       
      (emit "~a:" label)                 
      (emit ".long 0")))
  (define (emit-object x)
    (case (car x)
      [(public-function local-function) (emit-function x)]
      [(data) (emit-data x)]
      [else (error who "invalid object ~s" (car x))]))
  (for-each emit-object obj*))

(define (compile-program-with-entry original-program scheme-entry)
  (let* (;;;
         [recordized (recordize original-program)]
         [pure (remove-assignments recordized)]
         [converted (convert-closures pure)]
         
         [lifted (lift-codes converted)]
         [lifted (lift-complex-constants lifted)]
         [simplified (simplify-operands lifted)]
         [linearized (generate-code simplified scheme-entry)])
    (emit-linear-code linearized)))

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
                     [inline-primitives #t]
                     [signal-error-on-undefined-pcb #t])
        (compile-program-with-entry prog "libtoplevel"))
      (close-output-port op))))

(define (generate-scheme-runtime-helpers)
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
    (close-output-port p))
  (let ([p (open-output-file "scheme.c" 'replace)])
    (fprintf p "/* automatically generated, do not edit */\n")
    (fprintf p "#include \"scheme.h\"\n")
    (fprintf p "#include <stdio.h>\n")
    (fprintf p "ptr scheme_main(pcb_t* pcb){\n")
    (for-each 
       (lambda (x)
         (let ([name (caddr x)])
          ;(fprintf p "   fprintf(stderr, \"intered ~a\\n\");\n" name)
          (fprintf p "   ~a_entry(pcb);\n" name)
          ;(fprintf p "   fprintf(stderr, \"exited ~a\\n\");\n" name)
          ))
       scheme-library-files)
    (fprintf p "   libtoplevel_entry(pcb);\n");
    (fprintf p "   return scheme_entry(pcb);\n");
    (fprintf p "}\n")
    (close-output-port p)))
    

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
                 (not (pcb-assigned? (car x))))
        (set! undefined (cons (car x) undefined))))
    pcb-table)
  (unless (null? undefined)
    (error 'compile "undefined primitives found ~s" undefined)))

  
(runtime-file 
  (string-join " " 
    (list* "scheme.c" "runtime-4.3.c" "libtoplevel.s"
       (map cadr scheme-library-files))))

(printf "Testing ...\n")

;(test-all)
;(parameterize ([inline-primitives #f]) (test-all))
;(parameterize ([inline-primitives #t]) (test-all))
;(parameterize ([inline-primitives #t]
;               [input-filter 
;                (lambda (x)
;                  `(begin
;                     (write ,x)
;                     (newline)
;                     (exit)
;                     ))])
;  (test-all))

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

(parameterize ([inline-primitives #t]
               [input-filter 
                (lambda (x)
                  `(begin 
                     (write (eval ',x))
                     (newline)
                     (exit 0)))])
  (test-all))

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
