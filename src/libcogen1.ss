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
       (error 'new-cogen "appcall not supported yet")
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



(define (eliminate-fix x)
  ;;;
  (define who 'eliminate-fix)
  ;;;
  (define (make-closure x)
    (record-case x
      [(closure code free*)
       (cond
         [(null? free*) x]
         [else 
          (make-prim 'make-closure 
            (list code (make-constant (length free*))))])]))
  ;;;
  (define (closure-sets var x)
    (record-case x 
      [(closure code free*)
       (let f ([i 0] [free* free*])
         (cond
           [(null? free*) (make-primcall 'void '())]
           [else
            (make-seq 
              (make-primcall 'closure-set! 
                (list var (make-constant i) (car free*)))
              (f (fxadd1 i) (cdr free*)))]))]))
  ;;;
  (define (do-fix lhs* rhs* body)
    (make-bind lhs* 
               (map make-closure rhs*)
      (let f ([lhs* lhs*] [rhs* rhs*])
        (cond
          [(null? lhs*) body]
          [else
           (make-seq
             (closure-sets (car lhs*) (car rhs*))
             (f (cdr lhs*) (cdr rhs*)))]))))
  ;;;
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var)      x]
      [(primref)  x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* (Expr body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Expr e0) (Expr e1) (Expr e2))]
      [(seq e0 e1)
       (make-seq (Expr e0) (Expr e1))]
      [(closure) 
       (let ([t (unique-var 'tmp)])
         (Expr (make-fix (list t) (list x) t)))]
      [(primcall op arg*)
       (make-appcall (make-primref op) (map Expr arg*))]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator arg*)
       (make-funcall (Expr rator) (map Expr arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (Expr rator) (map Expr arg*))]
      [(appcall rator arg*)
       (error who "appcall not supported yet")
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


(define (specify-representation x)
  (define who 'specify-representation)
  ;;;
  (define nop (make-primcall 'nop '()))
  ;;;
  (define (constant-rep x)
    (let ([c (constant-value x)])
      (cond
        [(fixnum? c) (* c fixnum-scale)]
        [(boolean? c) (if c bool-t bool-f)]
        [(void? c) void-object]
        [(bwp? c) bwp-object]
        [(char? c) (fxlogor (fxsll (char->integer c) char-shift)
                            char-tag)]
        [(null? c) nil]
        [else x])))
  ;;;
  (define (Effect x)
    (record-case x
      [(constant c) nop]
      [(var)        nop]
      [(primref)    nop]
      [(closure code free*) nop]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Value rhs*) (Effect body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Effect e1) (Effect e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Effect e1))]
      [(primcall op arg*)
       (error who "effect prim ~a not supported" op)]
      [(forcall op arg*)
       (error who "effect forcall not supported" op)]
      [(funcall rator arg*)
       (make-funcall (Value rator) (map Value arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (Value rator) (map Value arg*))]
      [(appcall rator arg*)
       (error who "appcall not supported yet")]
      [(mvcall rator x)
       (make-mvcall (Value rator) (Clambda x Effect))]
      [else (error who "invalid pred expr ~s" x)]))
  ;;;
  (define (Pred x)
    (record-case x
      [(constant c) (if c #t #f)]
      [(var) (mkprm '!= (list x bool-f))]
      [(primref)  #t]
      [(closure code free*) #t]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Value rhs*) (Pred body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Pred e1) (Pred e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Pred e1))]
      [(primcall op arg*)
       (error who "pred prim ~a not supported" op)]
      [(forcall op arg*)
       (error who "pred forcall not supported" op)]
      [(funcall rator arg*)
       (mkprm '!= 
          (list (make-funcall (Value rator) (map Value arg*))
                bool-f))]
      [(jmpcall label rator arg*)
       (mkprm '!= 
          (list (make-jmpcall label (Value rator) (map Value arg*))
                bool-f))]
      [(appcall rator arg*)
       (error who "appcall not supported yet")]
      [(mvcall rator x)
       (make-mvcall (Value rator) (Clambda x Pred))]
      [else (error who "invalid pred expr ~s" x)])) 
  ;;;
  (define (Value x)
    (record-case x
      [(constant) (constant-rep x)]
      [(var)      x]
      [(primref)  x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Value rhs*) (Value body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Value e1) (Value e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Value e1))]
      [(primcall op arg*)
       (error who "value prim ~a not supported" op)]
      [(forcall op arg*)
       (error who "value forcall not supported" op)]
      [(funcall rator arg*)
       (make-funcall (Value rator) (map Value arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (Value rator) (map Value arg*))]
      [(appcall rator arg*)
       (error who "appcall not supported yet")]
      [(mvcall rator x)
       (make-mvcall (Value rator) (Clambda x Value))]
      [else (error who "invalid value expr ~s" x)]))
  ;;;
  (define (ClambdaCase x k)
    (record-case x
      [(clambda-case info body)
       (make-clambda-case info (k body))]
      [else (error who "invalid clambda-case ~s" x)]))
  ;;;
  (define (Clambda x k)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label 
          (map (lambda (x) (ClambdaCase x k)) case*)
          free*)]
      [else (error who "invalid clambda ~s" x)]))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes 
         (map (lambda (x) (Clambda x Value)) code*) 
         (Value body))]
      [else (error who "invalid program ~s" x)]))
  ;;;
  (Program x))




(define (new-cogen x)
  (verify-new-cogen-input x)
  (let* ([x (remove-primcalls x)]
         [x (eliminate-fix x)]
         [x (specify-representation x)])
    x))



