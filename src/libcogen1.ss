#!eof
;;; input to cogen is <Program>:
;;;  <Expr> ::= (constant x)
;;;           | (var)
;;;           | (primref name)
;;;           | (bind var* <Expr>* <Expr>)
;;;           | (fix var* <FixRhs>* <Expr>)
;;;           | (conditional <Expr> <Expr> <Expr>)
;;;           | (seq <Expr> <Expr>)
;;;           | (closure <codeloc> <var>*)  ; thunk special case
;;;           | (primcall op <Expr>*)
;;;           | (forcall "name" <Expr>*)
;;;           | (funcall <Expr> <Expr>*)
;;;           | (jmpcall <label> <Expr> <Expr>*)
;;;           | (appcall <Expr> <Expr>*)
;;;           | (mvcall <Expr> <clambda>)
;;;  <codeloc> ::= (code-loc <label>)
;;;  <clambda> ::= (clambda <label> <case>* <free var>*) 
;;;  <case>    ::= (clambda-case <info> <body>)
;;;  <info>    ::= (clambda-info label <arg var>* proper)
;;;  <Program> ::= (codes <clambda>* <Expr>)


(define (verify-new-cogen-input x)
  ;;;
  (define who 'verify-new-cogen-input)
  ;;;
  (define (check-gensym x)
    (unless (gensym? x)
      (error who "invalid gensym ~s" x)))
  ;;;
  (define (check-label x)
    (record-case x
      [(code-loc label)
       (check-gensym label)]
      [else (error who "invalid label ~s" x)]))
  ;;;
  (define (check-var x)
    (record-case x 
      [(var) (void)]
      [else (error who "invalid var ~s" x)]))
  ;;;
  (define (check-closure x)
    (record-case x
      [(closure label free*)
       (check-label label)
       (for-each check-var free*)]
      [else (error who "invalid closure ~s" x)]))
  ;;;
  (define (Expr x)
    (record-case x
      [(constant) (void)]
      [(var)      (void)]
      [(primref)  (void)]
      [(bind lhs* rhs* body)
       (for-each check-var lhs*)
       (for-each Expr rhs*)
       (Expr body)]
      [(fix lhs* rhs* body)
       (for-each check-var lhs*)
       (for-each check-closure rhs*)
       (Expr body)]
      [(conditional e0 e1 e2) 
       (Expr e0) (Expr e1) (Expr e2)]
      [(seq e0 e1)
       (Expr e0) (Expr e1)]
      [(closure) (check-closure x)]
      [(primcall op arg*)
       (for-each Expr arg*)]
      [(forcall op arg*)
       (for-each Expr arg*)]
      [(funcall rator arg*)
       (Expr rator)
       (for-each Expr arg*)]
      [(jmpcall label rator arg*)
       (check-gensym label)
       (Expr rator)
       (for-each Expr arg*)]
      [(appcall rator arg*)
       (Expr rator)
       (for-each Expr arg*)]
      [(mvcall rator k)
       (Expr rator)
       (Clambda k)]
      [else (error who "invalid expr ~s" x)]))
  ;;;
  (define (check-info x)
    (record-case x
      [(case-info label args proper)
       (check-gensym label)
       (for-each check-var args)]
      [else (error who "invalid case-info ~s" x)]))
  ;;;
  (define (ClambdaCase x)
    (record-case x
      [(clambda-case info body)
       (check-info info)
       (Expr body)]
      [else (error who "invalid clambda-case ~s" x)]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (for-each check-var free*)
       (for-each ClambdaCase case*)
       (check-gensym label)]
      [else (error who "invalid clambda ~s" x)]))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (for-each Clambda code*)
       (Expr body)]
      [else (error who "invalid program ~s" x)]))
  ;;;
  (Program x))




;;; the program so far includes both primcalls and funcalls to
;;; primrefs.  This pass removes all primcalls.  Once everything
;;; works, we need to fix all previous passes to eliminate this 
;;; whole primcall business.

(define (remove-primcalls x)
  ;;;
  (define who 'remove-primcalls)
  ;;;
  (define (check-gensym x)
    (unless (gensym? x)
      (error who "invalid gensym ~s" x)))
  ;;;
  (define (check-label x)
    (record-case x
      [(code-loc label)
       (check-gensym label)]
      [else (error who "invalid label ~s" x)]))
  ;;;
  (define (check-var x)
    (record-case x 
      [(var) (void)]
      [else (error who "invalid var ~s" x)]))
  ;;;
  (define (check-closure x)
    (record-case x
      [(closure label free*)
       (check-label label)
       (for-each check-var free*)]
      [else (error who "invalid closure ~s" x)]))
  ;;;
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var)      x]
      [(primref)  x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(fix lhs* rhs* body)
       (make-fix lhs* rhs* (Expr body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Expr e0) (Expr e1) (Expr e2))]
      [(seq e0 e1)
       (make-seq (Expr e0) (Expr e1))]
      [(closure) x]
      [(primcall op arg*)
       (make-appcall (make-primref op) (map Expr arg*))]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator arg*)
       (make-funcall (Expr rator) (map Expr arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (Expr rator) (map Expr arg*))]
      [(appcall rator arg*)
       (make-appcall (Expr rator) (map Expr arg*))]
      [(mvcall rator k)
       (make-mvcall (Expr rator) (Clambda k))]
      [else (error who "invalid expr ~s" x)]))
  ;;;
  (define (ClambdaCase x)
    (record-case x
      [(clambda-case info body)
       (make-clambda-case info (Expr body))]
      [else (error who "invalid clambda-case ~s" x)]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label (map ClambdaCase case*) free*)]
      [else (error who "invalid clambda ~s" x)]))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes (map Clambda code*) (Expr body))]
      [else (error who "invalid program ~s" x)]))
  ;;;
  (Program x))

;;;
;;; So, what do we want to do here?
;;; 1. we eliminate all variables:
;;;    bind should become a sequence of (setfv i <value>) statements
;;;    variable refs should befome (fv i) where i is the index of
;;;    the var on the stack.
;;; 2. we need to keep track of the maximum stack size used; and
;;;    whether we made a procedure call so that we can insert a 
;;;    stack overflow check with an appropriate size check.
;;; 3. as we generate code, we need to keep track of which stack
;;;    locations contain pointers so that procedure calls can
;;;    set an appropriate mask.
;;; 4. primitives that allocate, as well as closures and fixes 
;;;    should insert an appropriate alloc-check marks as well as
;;;    keep the live masks for the later generation of trap handlers
;;; 5. primcalls should also keep track of liveness information
;;;    and frame size just in case the primitive needs to make a 
;;;    procedure call.


(define-record move (src dst))
(define-record alloc-check (n si live))
(define-record fvar (idx))
(define acr '%acr)
(define-record cpvar (idx))
(define-record cmp (op v0 v1))
(define false-object 'false-object)

(define (new-cogen-pass1 x)
  ;;;
  (define who 'new-cogen-pass-1)
  ;;;
  (define (fxeven? x)
    (if (fixnum? x)
        ($fxzero? ($fxlogand x 1))
        (error 'fxeven? "~s is not a fixnum" x)))
  ;;;
  (define (sum ls)
    (let f ([ls ls] [n 0])
      (cond
        [(null? ls) n]
        [else (f (cdr ls) (fx+ (car ls) n))])))
  ;;;
  (define fxmax
    (case-lambda
      [(x y) (if (fx> x y) x y)]
      [(x y z)   (fxmax x (fxmax y z))]
      [_ (error 'fxmax "unhandled")]))
  ;;;
  (define (do-seq e0 si live env e1 k)
    (let-values ([(e0 msi0 call0?) 
                  (Effect e0 si live env)])
       (let-values ([(e1 msi1 call1?)
                     (k e1 si live env)])
         (values (make-seq e0 e1) 
                 (fxmax msi0 msi1) 
                 (or call0? call1?)))))
  ;;;
  (define (do-cond e0 si live env e1 e2 k)
    (let-values ([(e0 msi0 call0?) 
                  (Pred e0 si live env)])
       (let-values ([(e1 msi1 call1?)
                     (k e1 si live env)]
                    [(e2 msi2 call2?)
                     (k e2 si live env)])
         (values (make-conditional e0 e1 e2)
                 (fxmax msi0 msi1 msi2)
                 (or call0? call1? call2?)))))
  ;;;
  (define (do-move-to-mem loc expr si live env)
    (record-case expr
      [(constant) 
       (values (make-move expr loc) 0 #f)]
      [else
       (let-values ([(x msi call?) 
                     (Value expr si live env)])
         (values (make-seq x (make-move acr loc)) msi call?))]))
  ;;;
  (define (do-bind lhs* rhs* si live env0 env body k)
    (cond
      [(null? lhs*)
       (k body si live env)]
      [else
       (let ([fv (make-fvar si)])
         (let-values ([(e0 msi0 call0?)
                       (do-move-to-mem fv (car rhs*) si live env0)])
           (let-values ([(e1 msi1 call1?)
                         (do-bind (cdr lhs*) (cdr rhs*) 
                                  (fxadd1 si)
                                  (cons si live)
                                  env0
                                  (cons (cons (car lhs*) fv) env)
                                  body
                                  k)])
             (values (make-seq e0 e1)
                     (fxmax msi0 msi1) 
                     (or call0? call1?)))))]))
  ;;;
  (define (align n)
    (cond
      [(fxeven? n) n]
      [else (fxadd1 n)]))
  ;;;
  (define (extend-env* lhs* si live env)
    (let f ([lhs* lhs*] [si si] [live live] [env env] [nlhs* '()])
      (cond
        [(null? lhs*)
         (values nlhs* si live env)]
        [else
         (let ([fv (make-fvar si)])
           (f (cdr lhs*) (fxadd1 si) (cons si live) 
              (cons (cons (car lhs*) fv) env)
              (cons fv nlhs*)))])))
  ;;;
  (define (insert-const-alloc-check n si live rest)
    (cond
      [(fxzero? n) rest]
      [else
       (make-seq 
         (make-alloc-check n si live)
         rest)]))
  ;;;
  (define (construct-closures n* lhs* nlhs* rhs* body)
    (cond
      [(null? lhs*) body]
      [else
       (make-seq 'FIXME:construct-closures body)]))
  ;;;
  (define (do-fix lhs* rhs* si live env body k)
    (define (compute-closure-size x)
      (record-case x
        [(closure label free*)
         (align (fxadd1 (length free*)))]))
    (let ([n* (map compute-closure-size rhs*)])
      (let ([n (sum n*)])
        (let-values ([(nlhs* si live env) 
                      (extend-env* lhs* si live env)])
          (let-values ([(x msi call?) 
                        (k body si live env)])
            (values (insert-const-alloc-check n si live
                       (construct-closures n* lhs* nlhs* rhs* x))
                    msi call?))))))
  ;;;
  (define (constant-rep x)
    'FIXME:constant-rep)
  ;;;
  (define (do-closure label free* si live env)
     'FIXME:do-closure)
  ;;;
  (define (do-forcall x si live env)
    (values 'FIXME:do-forcall si #f))
  ;;;
  (define (do-tail-funcall x si live env)
    (values 'FIXME:do-tail-funcall si #f))
  ;;;
  (define (do-tail-appcall x si live env)
    (values 'FIXME:do-tail-appcall si #f))
  ;;;
  (define (do-tail-jmpcall x si live env)
    (values 'FIXME:do-tail-jmpcall si #f))
  ;;;
  (define (do-effect-funcall x si live env)
    (values 'FIXME:do-effect-funcall si #f))
  ;;;
  (define (do-effect-appcall x si live env)
    (values 'FIXME:do-effect-appcall si #f))
  ;;;
  (define (do-effect-jmpcall x si live env)
    (values 'FIXME:do-effect-jmpcall si #f))
  ;;;
  (define (do-value-funcall x si live env)
    (values 'FIXME:do-value-funcall si #f))
  ;;;
  (define (do-value-appcall x si live env)
    (values 'FIXME:do-value-appcall si #f))
  ;;;
  (define (do-value-jmpcall x si live env)
    (values 'FIXME:do-value-jmpcall si #f))
  ;;;
  (define (do-mvcall x si live env k)
    (values 'FIXME:do-mvcall si #t))
  ;;;
  (define (Var x env)
    (cond
      [(assq x env) => cdr]
      [else (error who "unbound var ~s" x)]))
  ;;;
  (define (Primref x)
    'FIXME:primref)
  ;;;
  (define (Value x si live env)
    (record-case x
      [(constant t) 
       (values (make-move (constant-rep t) acr) 0 #f)]
      [(var)      
       (values (make-move (Var x env) acr) 0 #f)]
      [(primref)  
       (values (make-move (Primref x) acr) 0 #f)]
      [(bind lhs* rhs* body)  
       (do-bind lhs* rhs* si live env env body Value)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* si live env body Value)]
      [(conditional e0 e1 e2)
       (do-cond e0 si live env e1 e2 Value)]
      [(seq e0 e1)   (do-seq e0 si live env e1 Value)]
      [(closure label free*)
       (values (do-closure label free* si live env) 0 #f)]
      [(forcall)     (do-forcall x si live env)]
      [(funcall)     (do-value-funcall x si live env)]
      [(jmpcall)     (do-value-jmpcall x si live env)]
      [(appcall)     (do-value-appcall x si live env)]
      [(mvcall)      (do-mvcall x si live env Value)]
      [else (error who "invalid value expr ~s" x)]))
  ;;;
  (define (Pred x si live env)
    (record-case x
      [(constant t) 
       (values (if t #t #f) 0 #f)]
      [(var) (values (make-cmp 'neq (Var x env) false-object) 0 #f)]
      [(primref)  (values #t 0 #f)]
      [(bind lhs* rhs* body)  
       (do-bind lhs* rhs* si live env env body Pred)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* si live env body Pred)]
      [(conditional e0 e1 e2)
       (do-cond e0 si live env e1 e2 Pred)]
      [(seq e0 e1) (do-seq e0 si live env e1 Pred)]
      [(closure label free*)  (values #t 0 #f)]
      [(forcall)
       (let-values ([(x msi call?) (Value x si live env)])
         (values (make-seq x (make-cmp 'neq acr false-object)) msi call?))]
      [(funcall)
       (let-values ([(x msi call?) (Value x si live env)])
         (values (make-seq x (make-cmp 'neq acr false-object)) msi call?))]
      [(jmpcall)
       (let-values ([(x msi call?) (Value x si live env)])
         (values (make-seq x (make-cmp 'neq acr false-object)) msi call?))]
      [(appcall)
       (let-values ([(x msi call?) (Value x si live env)])
         (values (make-seq x (make-cmp 'neq acr false-object)) msi call?))]
      [(mvcall) (do-mvcall x si live env Pred)]
      [else (error who "invalid pred expr ~s" x)]))
  ;;;
  (define (Effect x si live env)
    (record-case x
      [(constant) (values 'nop 0 #f)]
      [(var)      (values 'nop 0 #f)]
      [(primref)  (values 'nop 0 #f)]
      [(bind lhs* rhs* body)  
       (do-bind lhs* rhs* si live env env body Effect)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* si live env body Effect)]
      [(conditional e0 e1 e2)
       (do-cond e0 si live env e1 e2 Effect)]
      [(seq e0 e1)   (do-seq e0 si live env e1 Effect)]
      [(closure label free*)  (values 'nop 0 #f)]
      [(forcall)     (do-forcall x si live env)]
      [(funcall)     (do-effect-funcall x si live env)]
      [(jmpcall)     (do-effect-jmpcall x si live env)]
      [(appcall)     (do-effect-appcall x si live env)]
      [(mvcall)      (do-mvcall x si live env Tail)]
      [else (error who "invalid effect expr ~s" x)]))
  ;;;
  (define (Tail x si live env)
    (record-case x
      [(constant)    (values x si #f)]
      [(var)         (values (Var x env) si #f)]
      [(primref)     (values x si #f)]
      [(bind lhs* rhs* body)  
       (do-bind lhs* rhs* si live env env body Tail)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* si live env body Tail)]
      [(conditional e0 e1 e2) 
       (do-cond e0 si live env e1 e2 Tail)]
      [(seq e0 e1)   (do-seq e0 si live env e1 Tail)]
      [(closure label free*)  
       (values (do-closure label free* si live env) si #f)]
      [(forcall)
       (let-values ([(x msi call?) (do-forcall x si live env)])
         (values x msi call?))]
      [(funcall)     (do-tail-funcall x si live env)]
      [(jmpcall)     (do-tail-jmpcall x si live env)]
      [(appcall)     (do-tail-appcall x si live env)]
      [(mvcall)      (do-mvcall x si live env Tail)]
      [else (error who "invalid tail expr ~s" x)]))
  ;;;
  (define (MainTail x si live env)
    (let-values ([(x msi call?) 
                  (Tail x si live env)])
      (if call?
          (make-seq 'FIXME:stack-overflow-check x)
          x)))
  (define (ClambdaCase free*)
    (define (bind-free ls)
      (let f ([ls ls] [i 0] [env '()])
        (cond
          [(null? ls) env]
          [else
           (f (cdr ls) (fxadd1 i)
              (cons (cons (car ls) (make-cpvar i)) env))])))
    (define (do-info x env)
      (record-case x 
        [(case-info label args proper)
         (let f ([ls args] [env env] [si 0] [live '()])
           (cond
             [(null? ls) (values env si live)]
             [else
              (let ([fv (make-fvar si)])
                (f (cdr ls)
                   (cons (cons (car ls) fv) env)
                   (fxadd1 si)
                   (cons si live)))]))]))
    (let ([env (bind-free free*)])
      (lambda (x)
        (record-case x
          [(clambda-case info body)
           (let-values ([(env si live) (do-info info env)])
             (make-clambda-case info (MainTail body si live env)))]
          [else (error who "invalid clambda-case ~s" x)]))))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label (map (ClambdaCase free*) case*) free*)]
      [else (error who "invalid clambda ~s" x)]))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes (map Clambda code*) (MainTail body 0 '() '()))]
      [else (error who "invalid program ~s" x)]))
  ;;;
  (Program x))





(define (new-cogen x)
  (verify-new-cogen-input x)
  (let ([x (remove-primcalls x)])
    (new-cogen-pass1 x)))



