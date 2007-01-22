
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
;;;


(define (new-cogen x)
  (verify-new-cogen-input x)
  (let ([x (remove-primcalls x)])
    x))



