
(module (alt-cogen)
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


(module (must-open-code? prim-context 
         library-primitive?)
  (define core-prims
    '([pair?             p]
      [vector?           p]
      [null?             p]
      [eof-object?       p]
      ;[eof-object        v]
      [procedure?        p]
      [symbol?           p]
      [boolean?          p]
      [string?           p]
      [char?             p]
      [fixnum?           p]
      [string?           p]
      [immediate?        p]
      [char?             p]
      [eq?               p]
      [not             not]
      [void              v]
      [cons              v]
      [$car              v]
      [$cdr              v]

      ;[$char<=           p]
      ;[$char=            p]
      ;[$char->fixnum     v]

      [$vector-ref       v]
      [$vector-set!      e]

      ;;; ports
      [output-port?      p]
      [input-port?       p]
      [port?             p]

      [$cpref            v]
      [$cpset!           e]
      [$make-cp          v]
      [$closure-code     v]
      [$code-freevars    v]
      [primitive-set!    e]

      [$fp-at-base       p]
      [$current-frame    v]
      [$seal-frame-and-call tail]
      [$frame->continuation v]

      ))
  (define library-prims
    '(vector
      list list*
      not
      car cdr
      ))
  (define (must-open-code? x)
    (and (assq x core-prims) #t))
  (define (library-primitive? x)
    (memq x library-prims))
  (define (prim-context x)
    (cond
      [(assq x core-prims) => cadr]
      [else (error 'prim-context "~s is not a core prim" x)])))


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
  (define (mkfuncall op arg*)
    (record-case op
      [(primref name)
       (cond
         [(must-open-code? name)
          (make-primcall name arg*)]
         [(library-primitive? name)
          (make-funcall op arg*)]
         [(open-codeable? name)
          (error 'chaitin-compiler "primitive ~s is not supported"
                 name)]
         [else (make-funcall op arg*)])]
      [else (make-funcall op arg*)]))
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
       (mkfuncall (make-primref op) (map Expr arg*))]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator arg*)
       (mkfuncall (Expr rator) (map Expr arg*))]
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
  (define (Expr cpvar free*)
    ;;;
    (define (Var x)
      (let f ([free* free*] [i 0])
        (cond
          [(null? free*) x]
          [(eq? x (car free*))
           (make-primcall '$cpref (list cpvar (make-constant i)))]
          [else (f (cdr free*) (fxadd1 i))])))
    ;;;
    (define (make-closure x)
      (record-case x
        [(closure code free*)
         (cond
           [(null? free*) x]
           [else 
            (make-primcall '$make-cp 
              (list code (make-constant (length free*))))])]))
    ;;;
    (define (closure-sets var x ac)
      (record-case x 
        [(closure code free*)
         (let f ([i 0] [free* free*])
           (cond
             [(null? free*) ac]
             [else
              (make-seq 
                (make-primcall '$cpset! 
                  (list var (make-constant i) 
                        (Var (car free*))))
                (f (fxadd1 i) (cdr free*)))]))]))
    ;;;
    (define (do-fix lhs* rhs* body)
      (make-bind 
         lhs* (map make-closure rhs*)
        (let f ([lhs* lhs*] [rhs* rhs*])
          (cond
            [(null? lhs*) body]
            [else
             (closure-sets (car lhs*) (car rhs*)
               (f (cdr lhs*) (cdr rhs*)))]))))
    ;;;
    (define (Expr x)
      (record-case x
        [(constant) x]
        [(var)      (Var x)]
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
         (make-primcall op (map Expr arg*))]
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
    Expr)
  ;;;
  (define (ClambdaCase free*)
    (lambda (x)
      (record-case x
        [(clambda-case info body)
         (record-case info
           [(case-info label args proper)
            (let ([cp (unique-var 'cp)])
              (make-clambda-case 
                (make-case-info label (cons cp args) proper)
                ((Expr cp free*) body)))])]
        [else (error who "invalid clambda-case ~s" x)])))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label (map (ClambdaCase free*) case*) 
                     free*)]
      [else (error who "invalid clambda ~s" x)]))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes (map Clambda code*) ((Expr #f '()) body))]
      [else (error who "invalid program ~s" x)]))
  ;;;
  (Program x))


(define (normalize-context x)
  (define who 'normalize-context)
  ;;;
  (define nop (make-primcall 'nop '()))
  ;;;
  (define (Predicafy x)
    (make-primcall 'neq?
      (list (V x) (make-constant #f))))
  (define (Unpred x)
    (make-conditional (P x) 
        (make-constant #t)
        (make-constant #f)))
  (define (mkif e0 e1 e2)
    (record-case e0
      [(constant c) (if c e1 e2)]
      [(seq p0 p1) 
       (make-seq p0 (mkif p1 e1 e2))]
      [else
       (make-conditional e0 e1 e2)]))
  (define (mkbind lhs* rhs* body)
    (if (null? lhs*)
        body
        (make-bind lhs* rhs* body)))
  (define (mkseq e0 e1)
    (if (eq? e0 nop)
        e1
        (make-seq e0 e1)))
  ;;;
  (define (P x)
    (record-case x
      [(constant v) (make-constant (not (not v)))]
      [(primref)    (make-constant #t)]
      [(closure)    (make-constant #t)]
      [(code-loc)   (make-constant #t)]
      [(seq e0 e1) 
       (mkseq (E e0) (P e1))]
      [(conditional e0 e1 e2) 
       (mkif (P e0) (P e1) (P e2))]
      [(bind lhs* rhs* body)
       (mkbind lhs* (map V rhs*) (P body))]
      [(var)     (Predicafy x)]
      [(funcall) (Predicafy x)]
      [(jmpcall) (Predicafy x)]
      [(primcall op rands)
       (case (prim-context op)
         [(v) (Predicafy x)]
         [(p) (make-primcall op (map V rands))]
         [(e) 
          (let f ([rands rands])
            (cond
              [(null? rands) (make-constant #t)]
              [else
               (mkseq (E (car rands)) (f (cdr rands)))]))]
         [(not) 
          (make-conditional 
            (P (car rands)) 
            (make-constant #f)
            (make-constant #t))]
         [else (error who "invalid context for ~s" op)])] 
      [else (error who "invalid pred ~s" x)]))
  ;;;
  (define (E x)
    (record-case x
      [(constant) nop]
      [(primref)  nop]
      [(var)      nop]
      [(closure)  nop]
      [(code-loc) nop]
      [(seq e0 e1)
       (mkseq (E e0) (E e1))]
      [(bind lhs* rhs* body)
       (mkbind lhs* (map V rhs*) (E body))]
      [(conditional e0 e1 e2) 
       (let ([e1 (E e1)] [e2 (E e2)])
         (cond
           [(and (eq? e1 nop) (eq? e2 nop))
            (E e0)]
           [else
            (mkif (P e0) e1 e2)]))]
      [(funcall rator rand*)
       (make-funcall (V rator) (map V rand*))]
      [(jmpcall label rator rand*) 
       (make-jmpcall label (V rator) (map V rand*))]
      [(primcall op rands)
       (case (prim-context op)
         [(p v not) 
          (let f ([rands rands])
            (cond
              [(null? rands) nop]
              [else
               (mkseq (f (cdr rands)) (E (car rands)))]))]
         [(e) (make-primcall op (map V rands))]
         [else (error who "invalid context for ~s" op)])] 
      [else (error who "invalid effect ~s" x)]))
  ;;;
  (define (V x) 
    (record-case x
      [(constant) x]
      [(primref)  x]
      [(var)      x]
      [(closure)  x]
      [(code-loc) x]
      [(seq e0 e1) 
       (mkseq (E e0) (V e1))]
      [(conditional e0 e1 e2)
       (mkif (P e0) (V e1) (V e2))]
      [(bind lhs* rhs* body)
       (mkbind lhs* (map V rhs*) (V body))]
      [(funcall rator rand*)
       (make-funcall (V rator) (map V rand*))]
      [(jmpcall label rator rand*) 
       (make-jmpcall label (V rator) (map V rand*))]
      [(primcall op rands)
       (case (prim-context op)
         [(v tail) (make-primcall op (map V rands))]
         [(p) (Unpred x)]
         [(e) 
          (let f ([rands rands])
            (cond
              [(null? rands) (make-constant (void))]
              [else
               (mkseq (E (car rands)) (f (cdr rands)))]))]
         [(not) 
          (make-conditional 
            (P (car rands)) 
            (make-constant #f)
            (make-constant #t))]
         [else (error who "invalid context for ~s" op)])]
      [else (error who "invalid value ~s" x)]))
  ;;;
  (define (ClambdaCase x)
    (record-case x
      [(clambda-case info body)
       (make-clambda-case info (V body))]
      [else (error who "invalid clambda-case ~s" x)]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label 
          (map ClambdaCase case*)
          free*)]
      [else (error who "invalid clambda ~s" x)]))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes 
         (map Clambda code*)
         (V body))]
      [else (error who "invalid program ~s" x)]))
  ;;;
  (Program x))



(define-syntax seq*
  (syntax-rules ()
    [(_ e) e]
    [(_ e* ... e) 
     (make-seq (seq* e* ...) e)]))

(define (specify-representation x)
  (define who 'specify-representation)
  ;;;
  (define fixnum-scale 4)
  (define fixnum-tag 0)
  (define fixnum-mask 3)
  (define pcb-dirty-vector-offset 28)
  ;;;
  (define nop (make-primcall 'nop '()))
  ;;;
  (define (constant-rep x)
    (let ([c (constant-value x)])
      (cond
        [(fixnum? c) (make-constant (* c fixnum-scale))]
        [(boolean? c) (make-constant (if c bool-t bool-f))]
        [(eq? c (void)) (make-constant void-object)]
        [(bwp-object? c) (make-constant bwp-object)]
        [(char? c) (make-constant 
                     (fxlogor char-tag
                       (fxsll (char->integer c) char-shift)))]
        [(null? c) (make-constant nil)]
        [else (make-constant (make-object c))])))
  ;;;
  (define (K x) (make-constant x))
  (define (prm op . rands) (make-primcall op rands))
  (define-syntax tbind
    (lambda (x) 
      (syntax-case x ()
        [(_ ([lhs* rhs*] ...) b b* ...)
         #'(let ([lhs* (unique-var 'lhs*)] ...)
             (make-bind (list lhs* ...)
                        (list rhs* ...)
                b b* ...))])))
  (define (Effect x)
    (define (mem-assign v x i)
      (tbind ([q v])
        (tbind ([t (prm 'int+ x (K i))])
          (make-seq 
            (prm 'mset! t (K 0) q)
            (prm 'mset! 
                 (prm 'int+
                      (prm 'mref pcr (K 28))
                      (prm 'sll (prm 'sra t (K pageshift)) (K wordshift)))
                 (K 0)
                 (K dirty-word))))))
    (record-case x
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Value rhs*) (Effect body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Effect e1) (Effect e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Effect e1))]
      [(primcall op arg*)
       (case op
         [(nop) nop]
         [($cpset!)
          (let ([x (Value (car arg*))] 
                [i (cadr arg*)]
                [v (Value (caddr arg*))])
            (record-case i
              [(constant i) 
               (unless (fixnum? i) (err x))
               (prm 'mset! x 
                  (K (+ (* i wordsize) 
                        (- disp-closure-data closure-tag)))
                  v)]
              [else (err x)]))]
         [(primitive-set!)
          (let ([x (Value (car arg*))] [v (Value (cadr arg*))])
            (mem-assign v x 
               (- disp-symbol-system-value symbol-tag)))]
         [($vector-set!)
          (let ([x (Value (car arg*))] 
                [i (cadr arg*)]
                [v (Value (caddr arg*))])
            (record-case i
              [(constant i) 
               (unless (fixnum? i) (err x))
               (mem-assign v x 
                  (+ (* i wordsize)
                     (- disp-vector-data vector-tag)))]
              [else
               (mem-assign v 
                  (prm 'int+ x (Value i))
                  (- disp-vector-data vector-tag))]))]
         [else (error who "invalid effect prim ~s" op)])]
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
  (define (tag-test x mask tag)
    (if mask
        (make-primcall '= 
          (list (make-primcall 'logand 
                  (list x (make-constant mask)))
                (make-constant tag)))
        (make-primcall '=
           (list x (make-constant tag)))))
  (define (sec-tag-test x pmask ptag smask stag)
    (let ([t (unique-var 'tmp)])
      (make-bind (list t) (list x)
        (make-conditional 
          (tag-test t pmask ptag)
          (tag-test (prm 'mref t (K (- ptag))) smask stag)
          (make-constant #f)))))
  ;;;
  (define (Pred x)
    (record-case x
      [(constant) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Value rhs*) (Pred body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Pred e1) (Pred e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Pred e1))]
      [(primcall op arg*)
       (case op
         [(eq?)  (make-primcall '= (map Value arg*))]
         [(null?) (prm '= (Value (car arg*)) (K nil))]
         [(eof-object?) (prm '= (Value (car arg*)) (K eof))]
         [(neq?) (make-primcall '!= (map Value arg*))]
         [(pair?) 
          (tag-test (Value (car arg*)) pair-mask pair-tag)]
         [(procedure?)
          (tag-test (Value (car arg*)) closure-mask closure-tag)]
         [(symbol?)
          (tag-test (Value (car arg*)) symbol-mask symbol-tag)]
         [(string?)
          (tag-test (Value (car arg*)) string-mask string-tag)]
         [(char?)
          (tag-test (Value (car arg*)) char-mask char-tag)]
         [(boolean?)
          (tag-test (Value (car arg*)) bool-mask bool-tag)]
         [(fixnum?)
          (tag-test (Value (car arg*)) fixnum-mask fixnum-tag)]
         [(vector?)
          (sec-tag-test (Value (car arg*)) 
             vector-mask vector-tag fixnum-mask fixnum-tag)]
         [(output-port?)
          (sec-tag-test (Value (car arg*))
             vector-mask vector-tag #f output-port-tag)]
         [(immediate?)
          (tbind ([t (Value (car arg*))])
            (make-conditional 
              (tag-test t fixnum-mask fixnum-tag)
              (make-constant #t)
              (tag-test t 7 7)))]
         [($fp-at-base)
          (prm '= 
               (prm 'int+
                    (prm 'mref pcr (K 12)) ;;; PCB FRAME-BASE
                    (K (- wordsize)))
               fpr)]
         [else (error who "pred prim ~a not supported" op)])]
      [(mvcall rator x)
       (make-mvcall (Value rator) (Clambda x Pred))]
      [else (error who "invalid pred expr ~s" x)])) 
  ;;;
  (define (err x)
    (error who "invalid form ~s" (unparse x)))
  ;;; value
  (define (Value x)
    (record-case x
      [(constant) (constant-rep x)]
      [(var)      x]
      [(primref name)  
       (prm 'mref
           (K (make-object name))
           (K (- disp-symbol-system-value symbol-tag)))]
      [(code-loc) (make-constant x)]
      [(closure)  (make-constant x)]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Value rhs*) (Value body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Value e1) (Value e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Value e1))]
      [(primcall op arg*)
       (case op
         [(void) (K void-object)]
         [($car) 
          (prm 'mref (Value (car arg*)) (K (- disp-car pair-tag)))]
         [($cdr) 
          (prm 'mref (Value (car arg*)) (K (- disp-cdr pair-tag)))]
         [($make-cp)
          (let ([label (car arg*)] [len (cadr arg*)])
            (record-case len
              [(constant i)
               (unless (fixnum? i) (err x))
               (tbind ([t (prm 'alloc 
                               (K (align (+ disp-closure-data
                                            (* i wordsize))))
                               (K closure-tag))])
                 (seq*
                   (prm 'mset! t 
                        (K (- disp-closure-code closure-tag))
                        (Value label))
                   t))]
              [else (err x)]))]
         [(cons)
          (tbind ([a (Value (car arg*))]
                  [d (Value (cadr arg*))])
            (tbind ([t (prm 'alloc (K pair-size) (K pair-tag))])
              (seq*
                (prm 'mset! t (K (- disp-car pair-tag)) a)
                (prm 'mset! t (K (- disp-cdr pair-tag)) d)
                t)))]
         [($current-frame)  ;; PCB NEXT-CONTINUATION
          (prm 'mref pcr (K 12))]
         [($seal-frame-and-call) 
          (tbind ([proc (Value (car arg*))])
            (tbind ([k (prm 'alloc 
                            (K disp-continuation-size)
                            (K vector-tag))])
              (tbind ([base (prm 'int+
                                 (prm 'mref pcr (K 28))
                                 (K (- wordsize)))])
                (tbind ([underflow-handler 
                         (prm 'mref base (K 0))])
                  (seq* 
                    (prm 'mset! k 
                         (K (- vector-tag))
                         (K continuation-tag))
                    (prm 'mset! k 
                         (K (- disp-continuation-top vector-tag))
                         fpr)
                    (prm 'mset! k
                         (K (- disp-continuation-next vector-tag))
                         (prm 'mref pcr (K 12))) ;;; PCB NEXT CONT
                    (prm 'mset! k
                         (K (- disp-continuation-size vector-tag))
                         (prm 'int- base fpr))
                    (prm 'mset! pcr (K 12) k)
                    (make-primcall '$call-with-underflow-handler
                       (list underflow-handler proc k)))))))]
         [($frame->continuation)
          (tbind ([arg (Value (car arg*))])
            (tbind ([t (prm 'alloc
                         (K (align (+ disp-closure-data wordsize)))
                         (K closure-tag))])
              (seq* 
                (prm 'mset! t 
                     (K (- disp-closure-code closure-tag))
                     (make-constant 
                       (make-code-loc SL_continuation_code)))
                (prm 'mset! t
                     (K (- disp-closure-data closure-tag))
                     arg)
                t)))]
         [($cpref) 
          (let ([a0 (car arg*)] [a1 (cadr arg*)])
            (record-case a1
              [(constant i) 
               (unless (fixnum? i) (err x))
               (prm 'mref (Value a0) 
                  (K (+ (- disp-closure-data closure-tag) 
                        (* i wordsize))))]
              [else (err x)]))]
         [($vector-ref) 
          (let ([a0 (car arg*)] [a1 (cadr arg*)])
            (record-case a1
              [(constant i) 
               (unless (fixnum? i) (err x))
               (make-primcall 'mref 
                  (list (Value a0) 
                        (make-constant 
                          (+ (- disp-vector-data vector-tag)
                             (* i wordsize)))))]
              [else 
               (make-primcall 'mref 
                  (list (make-primcall 'int+
                          (list (Value a0) 
                                (Value a1)))
                        (make-constant 
                          (- disp-vector-data vector-tag))))]))]
         [($closure-code)
          (prm 'int+ 
               (prm 'mref
                    (Value (car arg*)) 
                    (K (- disp-closure-code closure-tag)))
               (K (- vector-tag disp-code-data)))]
         [($code-freevars)
          (prm 'mref 
               (Value (car arg*))
               (K (- disp-code-freevars vector-tag)))]
         [else (error who "value prim ~a not supported" (unparse x))])]
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
  ;(print-code x)
  (Program x))


(define parameter-registers '(%edi)) 
(define return-value-register '%eax)
(define cp-register '%edi)
(define all-registers '(%eax %edi %ebx %edx))
(define argc-register '%eax)

(define (impose-calling-convention/evaluation-order x)
  (define who 'impose-calling-convention/evaluation-order)
  ;;;
  ;;;
  (define (S* x* k)
    (cond
      [(null? x*) (k '())]
      [else
       (S (car x*)
          (lambda (a)
            (S* (cdr x*)
                (lambda (d)
                  (k (cons a d))))))]))
  ;;;
  (define (S x k)
    (record-case x
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* (S body k))]
      [(seq e0 e1)
       (make-seq (E e0) (S e1 k))]
      [else
       (cond
         [(or (constant? x) (var? x) (symbol? x)) (k x)]
         [(or (funcall? x) (primcall? x) (jmpcall? x)
              (conditional? x))
          (let ([t (unique-var 'tmp)])
            (do-bind (list t) (list x)
              (k t)))]
         [else (error who "invalid S ~s" x)])]))
  ;;;
  (define (do-bind lhs* rhs* body)
    (cond
      [(null? lhs*) body]
      [else
       (set! locals (cons (car lhs*) locals))
       (make-seq 
         (V (car lhs*) (car rhs*))
         (do-bind (cdr lhs*) (cdr rhs*) body))]))
  ;;;
  (define (nontail-locations args)
    (let f ([regs parameter-registers] [args args])
      (cond
        [(null? args) (values '() '() '())]
        [(null? regs) (values '() '() args)]
        [else
         (let-values ([(r* rl* f*) (f (cdr regs) (cdr args))])
            (values (cons (car regs) r*)
                    (cons (car args) rl*)
                    f*))])))
  (define (do-bind-frmt* nf* v* ac)
    (cond
      [(null? nf*) ac]
      [else
       (let ([t (unique-var 't)])
         (do-bind (list t) (list (car v*))
           (make-seq
             (make-set (car nf*) t)
             (do-bind-frmt* (cdr nf*) (cdr v*) ac))))]))
  ;;;
  (define (handle-nontail-call rator rands value-dest call-targ)
    (let-values ([(reg-locs reg-args frm-args)
                  (nontail-locations (cons rator rands))])
      (let ([regt* (map (lambda (x) (unique-var 'rt)) reg-args)]
            [frmt* (map (lambda (x) (make-nfvar 'unset-conflicts #f)) frm-args)])
        (let* ([call 
                (make-ntcall call-targ value-dest 
                  (cons argc-register (append reg-locs frmt*))
                  #f #f)]
               [body
                (make-nframe frmt* #f
                  (do-bind-frmt* frmt* frm-args
                    (do-bind regt* reg-args
                      (assign* reg-locs regt*
                        (make-seq 
                          (make-set argc-register 
                             (make-constant
                               (argc-convention (length rands))))
                          call)))))])
          (if value-dest
              (make-seq body (make-set value-dest return-value-register))
              body)))))
  ;;; impose value
  (define (V d x)
    (record-case x 
      [(constant) (make-set d x)]
      [(var)      (make-set d x)]
      [(bind lhs* rhs* e)
       (do-bind lhs* rhs* (V d e))]
      [(seq e0 e1)
       (make-seq (E e0) (V d e1))]
      [(conditional e0 e1 e2)
       (make-conditional (P e0) (V d e1) (V d e2))]
      [(primcall op rands)
       (case op
         [(alloc) 
          (S (car rands)
             (lambda (size)
               (S (cadr rands)
                  (lambda (tag)
                    (make-seq
                      (make-seq 
                        (make-set d apr)
                        (make-asm-instr 'logor d tag))
                      (make-asm-instr 'int+ apr size))))))]
         [(mref) 
          (S* rands
              (lambda (rands)
                (make-set d (make-disp (car rands) (cadr rands)))))]
         [(logand int+ int-)
          (make-seq
            (V d (car rands))
            (S (cadr rands)
               (lambda (s)
                 (make-asm-instr op d s))))]
         [(sll sra)
          (let ([a (car rands)] [b (cadr rands)])
            (cond
              [(constant? b)
               (make-seq
                 (V d a)
                 (make-asm-instr op d b))]
              [else (error who "invalid shift ~s" x)]))]
         [else (error who "invalid value op ~s" op)])]
      [(funcall rator rands) 
       (handle-nontail-call rator rands d #f)]
      [(jmpcall label rator rands) 
       (handle-nontail-call rator rands d label)]
      [else (error who "invalid value ~s" x)]))
  ;;;
  (define (assign* lhs* rhs* ac)
    (cond
      [(null? lhs*) ac]
      [else
       (make-seq 
         (make-set (car lhs*) (car rhs*))
         (assign* (cdr lhs*) (cdr rhs*) ac))]))
  ;;;
  (define (VT x)
    (make-seq 
       (V return-value-register x)
       (make-primcall 'return (list return-value-register))))
  ;;; impose effect
  (define (E x)
    (record-case x
      [(seq e0 e1) (make-seq (E e0) (E e1))]
      [(conditional e0 e1 e2)
       (make-conditional (P e0) (E e1) (E e2))]
      [(bind lhs* rhs* e)
       (do-bind lhs* rhs* (E e))]
      [(primcall op rands)
       (case op
         [(mset!)
          (S* rands
              (lambda (s*)
                (make-asm-instr 'mset 
                  (make-disp (car s*) (cadr s*)) 
                  (caddr s*))))]
         [(nop) x]
         [else (error 'impose-effect "invalid instr ~s" x)])]
;       (S* rands
;           (lambda (rands)
;             (make-primcall op rands)))]
      [(funcall rator rands) 
       (handle-nontail-call rator rands #f #f)]
      [(jmpcall label rator rands) 
       (handle-nontail-call rator rands #f label)]
      [else (error who "invalid effect ~s" x)]))
  ;;; impose pred
  (define (P x)
    (record-case x
      [(constant) x]
      [(seq e0 e1) (make-seq (E e0) (P e1))]
      [(conditional e0 e1 e2)
       (make-conditional (P e0) (P e1) (P e2))]
      [(bind lhs* rhs* e)
       (do-bind lhs* rhs* (P e))]
      [(primcall op rands)
       (S* rands
           (lambda (rands)
             (make-asm-instr op (car rands) (cadr rands))))]
      [else (error who "invalid pred ~s" x)]))
  ;;;
  (define (handle-tail-call target rator rands)
    (let ([cpt (unique-var 'rator)]
          [rt* (map (lambda (x) (unique-var 't)) rands)])
      (do-bind rt* rands
        (do-bind (list cpt) (list rator)
           (let ([args (cons cpt rt*)]
                 [locs (formals-locations (cons cpt rt*))])
             (assign* (reverse locs)
                      (reverse args)
               (make-seq
                 (make-set argc-register 
                   (make-constant
                     (argc-convention (length rands))))
                 (cond
                   [target 
                    (make-primcall 'direct-jump (cons target locs))]
                   [else 
                    (make-primcall 'indirect-jump locs)]))))))))
  (define (Tail x)
    (record-case x 
      [(constant) (VT x)]
      [(var)      (VT x)]
      [(primcall op rands) 
       (case op
         [($call-with-underflow-handler) 
          (let ([handler (car rands)]
                [proc (cadr rands)]
                [k (caddr rands)])
            (seq*
              (make-set (make-fvar 1) handler)
              (make-set (make-fvar 2) k)
              (make-set cpr proc)
              (make-set argc-register 
                        (make-constant (argc-convention 1)))
              (make-asm-instr 'int- fpr
                    (make-constant wordsize))
              (make-primcall 'indirect-jump
                (list cpr (make-fvar 1) (make-fvar 2)))))]
         [else (VT x)])]
      [(bind lhs* rhs* e)
       (do-bind lhs* rhs* (Tail e))]
      [(seq e0 e1)
       (make-seq (E e0) (Tail e1))]
      [(conditional e0 e1 e2)
       (make-conditional (P e0) (Tail e1) (Tail e2))]
      [(funcall rator rands)
       (handle-tail-call #f rator rands)]
      [(jmpcall label rator rands)
       (handle-tail-call (make-code-loc label) rator rands)]
      [else (error who "invalid tail ~s" x)]))
  ;;;
  (define (formals-locations args)
    (let f ([regs parameter-registers] [args args])
      (cond
        [(null? args) '()]
        [(null? regs) 
         (let f ([i 1] [args args])
           (cond
             [(null? args) '()]
             [else
              (cons (mkfvar i)
                (f (fxadd1 i) (cdr args)))]))]
        [else
         (cons (car regs) (f (cdr regs) (cdr args)))])))
  ;;;
  (define locals '())
  ;;;
  (define (ClambdaCase x) 
    (record-case x
      [(clambda-case info body)
       (record-case info
         [(case-info label args proper)
          (set! locals args)
          (let* ([locs (formals-locations args)]
                 [body (let f ([args args] [locs locs])
                          (cond
                            [(null? args) (Tail body)]
                            [else
                             (make-seq
                               (make-set (car args) (car locs))
                               (f (cdr args) (cdr locs)))]))])
            (make-clambda-case
              (make-case-info label locs proper)
              (make-locals locals body)))])]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label (map ClambdaCase case*) free*)]))
  ;;;
  (define (Main x)
    (set! locals '())
    (let ([x (Tail x)])
      (make-locals locals x)))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes (map Clambda code*) (Main body))]))
  ;;;
  (Program x))


(module ListyGraphs 
  (empty-graph add-edge! empty-graph? print-graph node-neighbors
   delete-node!)
  ;;;
  (define-record graph (ls))
  ;;;
  (define (empty-graph) (make-graph '()))
  ;;;
  (define (empty-graph? g) 
    (andmap (lambda (x) (null? (cdr x))) (graph-ls g)))
  ;;;
  (define (add-edge! g x y)
    (let ([ls (graph-ls g)])
      (cond
        [(assq x ls) =>
         (lambda (p0)
           (unless (memq y (cdr p0))
             (set-cdr! p0 (cons y (cdr p0)))
             (cond
               [(assq y ls) => 
                (lambda (p1) 
                  (set-cdr! p1 (cons x (cdr p1))))]
               [else
                (set-graph-ls! g 
                   (cons (list y x) ls))])))]
        [(assq y ls) =>
         (lambda (p1)
           (set-cdr! p1 (cons x (cdr p1)))
           (set-graph-ls! g (cons (list x y) ls)))]
        [else 
         (set-graph-ls! g 
           (list* (list x y)
                  (list y x)
                  ls))])))
  (define (print-graph g)
    (printf "G={\n")
    (parameterize ([print-gensym 'pretty])
      (for-each (lambda (x) 
                  (let ([lhs (car x)] [rhs* (cdr x)])
                    (printf "  ~s => ~s\n" 
                            (unparse lhs)
                            (map unparse rhs*))))
        (graph-ls g)))
    (printf "}\n"))
  (define (node-neighbors x g)
    (cond
      [(assq x (graph-ls g)) => cdr]
      [else '()]))
  (define (delete-node! x g)
    (let ([ls (graph-ls g)])
      (cond
        [(assq x ls) =>
         (lambda (p)
           (for-each (lambda (y) 
                       (let ([p (assq y ls)])
                         (set-cdr! p (set-rem x (cdr p)))))
                     (cdr p))
           (set-cdr! p '()))]
        [else (void)])))
  ;;;
  #|ListyGraphs|#)

(begin
  (define (set-add x s)
    (cond
      [(memq x s) s]
      [else (cons x s)]))
             
  (define (set-rem x s)
    (cond
      [(null? s) '()]
      [(eq? x (car s)) (cdr s)]
      [else (cons (car s) (set-rem x (cdr s)))]))
  
  (define (set-difference s1 s2)
    (cond
      [(null? s2) s1]
      [else (set-difference (set-rem (car s2) s1) (cdr s2))]))
  
  (define (set-union s1 s2)
    (cond
      [(null? s1) s2]
      [(memq (car s1) s2) (set-union (cdr s1) s2)]
      [else (cons (car s1) (set-union (cdr s1) s2))])))


(module (color-by-chaitin)
  (import ListyGraphs)
  ;;;
  (define (build-graph x reg?)
    (define who 'build-graph)
    (define g (empty-graph))
    (define (R* ls)
      (cond
        [(null? ls) '()]
        [else (union (R (car ls)) (R* (cdr ls)))]))
    (define (R x)
      (record-case x
        [(constant) '()]
        [(var) (list x)]
        [(disp s0 s1) (union (R s0) (R s1))]
        [(nfvar) (list x)]
        [(fvar) (if (reg? x) (list x) '())]
        [(code-loc) '()]
        [else
         (cond
           [(symbol? x) (if (reg? x) (list x) '())]
           [else (error who "invalid R ~s" x)])]))
    ;;; build effect
    (define (E x s)
      (record-case x
        [(set x v) 
         (let ([s (set-rem x s)])
           (record-case x
             [(nfvar c i) 
              (if (list? c)
                  (set-nfvar-conf! x 
                     (set-union c s))
                  (set-nfvar-conf! x s))
              (union (R v) s)]
             [else
              (for-each (lambda (y) (add-edge! g x y)) s)
              (union (R v) s)]))]
        [(asm-instr op d v)
         (case op
           [(logand int+ int- logor sll sra)
            (let ([s (set-rem d s)])
              (record-case d
                [(nfvar c i) 
                 (if (list? c)
                     (set-nfvar-conf! d
                        (set-union c s))
                     (set-nfvar-conf! d s))
                 (union (union (R v) (R d)) s)]
                [else
                 (for-each (lambda (y) (add-edge! g d y)) s)
                 (union (union (R v) (R d)) s)]))]
           [(mset)
            (union (R v) (union (R d) s))]
           [else (error who "invalid effect ~s" x)])]
        [(seq e0 e1) (E e0 (E e1 s))]
        [(conditional e0 e1 e2)
         (let ([s1 (E e1 s)] [s2 (E e2 s)])
           (P e0 s1 s2 (set-union s1 s2)))]
        [(nframe vars live body)
         (when (reg? return-value-register)
           (for-each 
             (lambda (x)
               (for-each (lambda (r)
                           (add-edge! g x r))
                         all-registers))
             s))
         (let ([s (set-difference s all-registers)])
           (set-nframe-live! x s)
           (E body s))]
        [(ntcall targ value args mask size)
         (union (R* args) s)]
        [(primcall op arg*)
         (case op
           [(nop) s]
           [else (error who "invalid effect primcall ~s" op)])]
        [else (error who "invalid effect ~s" x)]))
    (define (P x st sf su)
      (record-case x
        [(constant c) (if c st sf)]
        [(seq e0 e1)
         (E e0 (P e1 st sf su))]
        [(conditional e0 e1 e2)
         (let ([s1 (P e1 st sf su)] [s2 (P e2 st sf su)])
           (P e0 s1 s2 (set-union s1 s2)))]
        [(asm-instr op s0 s1) 
         (union (union (R s0) (R s1)) su)]
        [else (error who "invalid pred ~s" x)]))
    (define (T x)
      (record-case x
        [(conditional e0 e1 e2)
         (let ([s1 (T e1)] [s2 (T e2)])
           (P e0 s1 s2 (set-union s1 s2)))]
        [(primcall op rands) 
         (R* rands)]
        [(seq e0 e1) (E e0 (T e1))]
        [else (error who "invalid tail ~s" x)]))
    (let ([s (T x)])
      ;(print-graph g)
      g))
  ;;;
  (define (color-graph sp* un* g)
    (define (find-low-degree ls g)
      (cond
        [(null? ls) #f]
        [(fx< (length (node-neighbors (car ls) g))
              (length all-registers))
         (car ls)]
        [else (find-low-degree (cdr ls) g)]))
    (define (find-color/maybe x confs env)
      (let ([cr (map (lambda (x)
                       (cond
                         [(symbol? x) x]
                         [(assq x env) => cdr]
                         [else #f]))
                     confs)])
        (let ([r* (set-difference all-registers cr)])
          (if (null? r*) 
              #f
              (car r*)))))
    (define (find-color x confs env)
      (or (find-color/maybe x confs env)
          (error 'find-color "cannot find color for ~s" x)))
    (cond
      [(and (null? sp*) (null? un*)) (values '() '() '())]
      [(find-low-degree un* g) =>
       (lambda (un)
         (let ([n* (node-neighbors un g)])
           (delete-node! un g)
           (let-values ([(spills sp* env) 
                         (color-graph sp* (set-rem un un*) g)])
             (let ([r (find-color un n* env)])
               (values spills sp*
                  (cons (cons un r) env))))))]
      [(find-low-degree sp* g) =>
       (lambda (sp)
         (let ([n* (node-neighbors sp g)])
           (delete-node! sp g)
           (let-values ([(spills sp* env) 
                         (color-graph (set-rem sp sp*) un* g)])
             (let ([r (find-color sp n* env)])
               (values spills (cons sp sp*)
                  (cons (cons sp r) env))))))]
      [(pair? sp*)
       (let ([sp (car sp*)])
         (let ([n* (node-neighbors sp g)])
           (delete-node! sp g)
           (let-values ([(spills sp* env) 
                         (color-graph (set-rem sp sp*) un* g)])
             (let ([r (find-color/maybe sp n* env)])
               (if r
                   (values spills (cons sp sp*)
                       (cons (cons sp r) env))
                   (values (cons sp spills) sp* env))))))]
      [else (error 'color-graph "whoaaa")]))
  ;;;
  (define (substitute env x frm-graph)
    (define who 'substitute)
    (define (max-live vars i)
      (cond
        [(null? vars) i]
        [else (max-live (cdr vars)
                (record-case (car vars)
                  [(fvar j) (max i j)]
                  [else i]))]))
    (define (actual-frame-size vars i)
      (define (conflicts? i ls)
        (and (not (null? ls))
             (or (let f ([x (car ls)])
                   (record-case x
                     [(fvar j) (eq? i j)]
                     [(var) 
                      (cond
                        [(assq x env) => (lambda (x) (f (cdr x)))]
                        [else #f])]
                     [(nfvar conf loc) (f loc)]
                     [else #f]))
                 (conflicts? i (cdr ls)))))
      (define (frame-size-ok? i vars)
        (or (null? vars)
            (and (not (conflicts? i (nfvar-conf (car vars))))
                 (frame-size-ok? (fxadd1 i) (cdr vars)))))
      (cond
        [(frame-size-ok? i vars) i]
        [else (actual-frame-size vars (fxadd1 i))]))
    (define (assign-frame-vars! vars i)
      (unless (null? vars)
        (let ([v (car vars)]
              [fv (mkfvar i)])
          (set-nfvar-loc! v fv)
          (for-each
            (lambda (x)
              (when (var? x)
                (add-edge! frm-graph x fv)))
            (nfvar-conf v)))
        (assign-frame-vars! (cdr vars) (fxadd1 i))))
    (define (Var x)
      (cond
        [(assq x env) => cdr]
        [else x]))
    (define (Rhs x)
      (record-case x
        [(var) (Var x)]
        [(primcall op rand*)
         (make-primcall op (map Rand rand*))]
        [else x]))
    (define (Rand x) 
      (record-case x
        [(var) (Var x)]
        [else x]))
    (define (Lhs x)
      (record-case x
        [(var) (Var x)]
        [(nfvar confs loc) 
         (or loc (error who "LHS not set ~s" x))]
        [else x]))
    (define (NFE idx mask x)
      (record-case x
        [(seq e0 e1) (make-seq (E e0) (NFE idx mask e1))]
        [(ntcall target value args mask^ size)
         (make-ntcall target value 
            (map (lambda (x) 
                   (if (symbol? x)
                       x
                       (Lhs x)))
                 args)
            mask idx)]
        [else (error who "invalid NF effect ~s" x)]))
    (define (make-mask n live*)
      (let ([v (make-vector (fxsra (fx+ n 7) 3) 0)])
        (for-each 
          (lambda (x)
            (record-case x
              [(fvar idx) 
               (let ([q (fxsra idx 3)]
                     [r (fxlogand idx 7)])
                 (vector-set! v q
                   (fxlogor (vector-ref v q) (fxsll 1 r))))]
              [else (void)]))
          live*)
        v))
    (define (D x)
      (record-case x
        [(constant) x]
        [(var) (Var x)]
        [(fvar) x]
        [else
         (if (symbol? x) x (error who "invalid D ~s" x))]))
    (define (R x)
      (record-case x
        [(constant) x]
        [(var) (Var x)]
        [(fvar)     x]
        [(nfvar c loc)
         (or loc (error who "unset nfvar ~s in R" x))]
        [(disp s0 s1) (make-disp (D s0) (D s1))]
        [else
         (if (symbol? x) x (error who "invalid R ~s" x))]))
    ;;; substitute effect
    (define (E x)
      (record-case x
        [(set lhs rhs) 
         (let ([lhs (R lhs)] [rhs (R rhs)])
           (cond
             [(or (eq? lhs rhs) 
                  (and (fvar? lhs) (fvar? rhs)
                       (fixnum? (fvar-idx lhs))
                       (fixnum? (fvar-idx rhs))
                       (fx= (fvar-idx lhs) (fvar-idx rhs))))
                  (make-primcall 'nop '())]
             [else (make-set lhs rhs)]))]
        [(seq e0 e1) (make-seq (E e0) (E e1))]
        [(conditional e0 e1 e2) 
         (make-conditional (P e0) (E e1) (E e2))]
        [(asm-instr op x v)
         (make-asm-instr op (R x) (R v))]
        [(primcall op rands) 
         (make-primcall op (map R rands))]
        [(nframe vars live body)
         (let ([live-fv* 
                (map (lambda (x)
                       (record-case x
                         [(var) 
                          (let ([l (Var x)])
                            (if (fvar? l)
                                l
                                (error who "unspilled live-after ~s"
                                       x)))]
                         [(nfvar c loc)
                          (or loc (error who "unspilled live-after ~s" x))]
                         [else 
                          (error who "invalid live-after ~s" x)]))
                     live)])
           (let ([i (actual-frame-size vars
                      (fx+ 2 (max-live live-fv* 0)))])
             (assign-frame-vars! vars i)
             (NFE (fxsub1 i) (make-mask i live-fv*) body)))]
        [(ntcall) x]
        [else (error who "invalid effect ~s" x)]))
    (define (P x)
      (record-case x
        [(constant) x]
        [(asm-instr op x v)
         (make-asm-instr op (R x) (R v))]
        [(conditional e0 e1 e2) 
         (make-conditional (P e0) (P e1) (P e2))]
        [(seq e0 e1) (make-seq (E e0) (P e1))]
        [else (error who "invalid pred ~s" x)])) 
    (define (T x)
      (record-case x
        [(primcall op rands) x]
        [(conditional e0 e1 e2) 
         (make-conditional (P e0) (T e1) (T e2))]
        [(seq e0 e1) (make-seq (E e0) (T e1))]
        [else (error who "invalid tail ~s" x)]))
    ;(print-code x)
    (T x))
  ;;;
  (define (do-spill sp* g)
    (define (find/set-loc x)
      (let ([ls (node-neighbors x g)])
        (define (conflicts? i ls)
          (and (pair? ls)
               (or (record-case (car ls)
                      [(fvar j)
                       (and (fixnum? j) (fx= i j))]
                      [else #f])
                   (conflicts? i (cdr ls)))))
        (let f ([i 1])
          (cond
            [(conflicts? i ls) (f (fxadd1 i))]
            [else
             (let ([fv (mkfvar i)])
               (for-each (lambda (y) (add-edge! g y fv)) ls)
               (delete-node! x g)
               (cons x fv))]))))
    (map find/set-loc sp*))
  ;;;
  (define (add-unspillables un* x)
    (define who 'add-unspillables)
    (define (mku)
      (let ([u (unique-var 'u)])
        (set! un* (cons u un*))
        u))
    (define (S x k)
      (cond
        [(or (constant? x) (var? x) (symbol? x))
         (k x)]
        [else
         (let ([u (mku)])
           (make-seq (E (make-set u x)) (k u)))]))
    (define (S* ls k)
      (cond
        [(null? ls) (k '())]
        [else
         (S (car ls) 
            (lambda (a)
              (S* (cdr ls) 
                  (lambda (d)
                    (k (cons a d))))))]))
    (define (mem? x)
      (or (disp? x) (fvar? x)))
    ;;; unspillable effect
    (define (E x)
      (record-case x
        [(set lhs rhs) 
         (cond
           [(or (constant? rhs) (var? rhs) (symbol? rhs)) x]
           [(fvar? lhs) 
            (cond
              [else 
               (let ([u (mku)])
                 (make-seq
                   (E (make-set u rhs))
                   (make-set lhs u)))])]
           [(fvar? rhs) x]
           [(disp? rhs)
            (S (disp-s0 rhs)
               (lambda (s0)
                 (S (disp-s1 rhs)
                    (lambda (s1)
                      (make-set lhs (make-disp s0 s1))))))]
           [else (error who "invalid set in ~s" x)])]
        [(seq e0 e1) (make-seq (E e0) (E e1))]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (E e1) (E e2))]
        [(asm-instr op a b) 
         (case op
           [(logor logand int+ int-)
            (cond
              [(and (mem? a) (mem? b)) 
               (let ([u (mku)])
                 (make-seq
                   (E (make-set u b))
                   (E (make-asm-instr op a u))))]
              [else x])]
           [(sll sra) 
            (unless (constant? b) (error who "invalid shift ~s" b))
            x]
           [(mset) 
            (cond
              [(mem? b) 
               (let ([u (mku)])
                 (make-seq
                   (E (make-set u b))
                   (E (make-asm-instr op a u))))]
              [else
               (let ([s1 (disp-s0 a)] [s2 (disp-s1 a)])
                 (cond
                   [(and (mem? s1) (mem? s2))
                    (let ([u (mku)])
                      (make-seq
                        (make-seq
                          (E (make-set u s1))
                          (E (make-asm-instr 'int+ u s2)))
                        (make-asm-instr 'mset 
                           (make-disp u (make-constant 0))
                           b)))]
                   [(mem? s1)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-set u s1))
                        (make-asm-instr 'mset (make-disp u s2) b)))]
                   [(mem? s2)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-set u s2))
                        (make-asm-instr 'mset (make-disp u s1) b)))]
                   [else x]))])]
           [else (error who "invalid effect ~s" op)])]
        [(primcall op rands) 
         (case op
           [(nop) x]
           [(record-effect)
            (S* rands
                (lambda (s*)
                  (make-primcall op s*)))]
           [else (error who "invalid op in ~s" x)])]
        [(ntcall) x]
        [else (error who "invalid effect ~s" x)]))
    (define (P x)
      (record-case x
        [(constant) x]
        [(primcall op rands)
         (let ([a0 (car rands)] [a1 (cadr rands)])
           (cond
             [(and (fvar? a0) (fvar? a1))
              (let ([u (mku)])
                (make-seq 
                  (make-set u a0)
                  (make-primcall op (list u a1))))]
             [else x]))]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (P e1) (P e2))]
        [(seq e0 e1) (make-seq (E e0) (P e1))]
        [(asm-instr op a b) 
         (cond
           [(and (mem? a) (mem? b)) 
            (let ([u (mku)])
              (make-seq
                (E (make-set u b))
                (make-asm-instr op a u)))]
           [else x])]
        [else (error who "invalid pred ~s" x)]))
    (define (T x)
      (record-case x
        [(primcall op rands) x]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (T e1) (T e2))]
        [(seq e0 e1) (make-seq (E e0) (T e1))]
        [else (error who "invalid tail ~s" x)]))
    (let ([x (T x)])
      (values un* x)))
  ;;;
  (define (color-program x)
    (define who 'color-program)
    (record-case x 
      [(locals sp* body)
       (let ([frame-g (build-graph body fvar?)])
         (let loop ([sp* sp*] [un* '()] [body body])
             (printf "a")
           (let ([g (build-graph body 
                        (lambda (x) 
                          (and (symbol? x)
                               (memq x all-registers))))])
                  ;  (printf "loop:\n")
                  ;  (print-code body)
             ;(print-graph g)
             (printf "b")
             (let-values ([(spills sp* env) (color-graph sp* un* g)])
             (printf "c")
               (cond
                 [(null? spills) (substitute env body frame-g)]
                 [else 
             (printf "d")
                  (let* ([env (do-spill spills frame-g)]
                         [body (substitute env body frame-g)])
             (printf "e")
                    (let-values ([(un* body)
                                  (add-unspillables un* body)])
             (printf "f")
                       (loop sp* un* body)))])))))]))
  ;;;
  (define (color-by-chaitin x)
    ;;;
    (define (ClambdaCase x) 
      (record-case x
        [(clambda-case info body)
         (make-clambda-case info (color-program body))]))
    ;;;
    (define (Clambda x)
      (record-case x
        [(clambda label case* free*)
         (make-clambda label (map ClambdaCase case*) free*)]))
    ;;;
    (define (Program x)
      (record-case x 
        [(codes code* body)
         (make-codes (map Clambda code*) (color-program body))]))
    ;;;
    (Program x))
  #|chaitin module|#)



(define (flatten-codes x)
  (define who 'flatten-codes)
  ;;;
  (define (FVar i)
    `(disp ,(* i (- wordsize)) ,fpr))
  ;;;
  (define (C x)
    (record-case x
      [(code-loc label) (label-address label)]
      [(closure label free*)
       (unless (null? free*) (error who "nonempty closure"))
       `(obj ,x)]
      [(object o)
       `(obj ,o)]
      [else 
       (if (integer? x)
           x
           (error who "invalid constant C ~s" x))]))
  (define (D x)
    (record-case x
      [(constant c) (C c)]
      [else
       (if (symbol? x) x (error who "invalid D ~s" x))]))
  (define (R x)
    (record-case x
      [(constant c) (C c)]
      [(fvar i) (FVar i)]
      [(disp s0 s1) 
       (let ([s0 (D s0)] [s1 (D s1)])
         `(disp ,s0 ,s1))]
      [else
       (if (symbol? x) x (error who "invalid R ~s" x))]))
  ;;; flatten effect
  (define (E x ac)
    (record-case x
      [(seq e0 e1) (E e0 (E e1 ac))]
      [(set lhs rhs) 
       (cons `(movl ,(R rhs) ,(R lhs)) ac)]
      [(conditional e0 e1 e2)
       (let ([lf (unique-label)] [le (unique-label)])
         (P e0 #f lf
            (E e1 
               (list* `(jmp ,le) lf
                  (E e2 (cons le ac))))))]
      [(ntcall target value args mask size) 
       (let ([LCALL (unique-label)])
         (define (rp-label value)
           (if value
               (label-address SL_multiple_values_error_rp)
               (label-address SL_multiple_values_ignore_rp)))
         (cond
           [target ;;; known call
            (list* `(subl ,(* (fxsub1 size) wordsize) ,fpr)
                   `(jmp ,LCALL)
                   `(byte-vector ,mask)
                   `(int ,(* size wordsize))
                   `(current-frame-offset)
                   (rp-label value)
                   LCALL
                   `(call (label ,target))
                   `(addl ,(* (fxsub1 size) wordsize) ,fpr)
                   ac)]
           [else
            (list* `(subl ,(* (fxsub1 size) wordsize) ,fpr)
                   `(jmp ,LCALL)
                   `(byte-vector ,mask)
                   `(int ,(* size wordsize))
                   `(current-frame-offset)
                   (rp-label value)
                   '(byte 0)
                   '(byte 0)
                   LCALL
                   `(call (disp ,(fx- disp-closure-code closure-tag) ,cp-register))
                   `(addl ,(* (fxsub1 size) wordsize) ,fpr)
                   ac)]))]
      [(asm-instr op d s)
       (case op
         [(logand) (cons `(andl ,(R s) ,(R d)) ac)]
         [(int+) (cons `(addl ,(R s) ,(R d)) ac)]
         [(int-) (cons `(subl ,(R s) ,(R d)) ac)]
         [(logor)  (cons `(orl ,(R s) ,(R d)) ac)]
         [(mset) (cons `(movl ,(R s) ,(R d)) ac)]
         [(sll)  (cons `(sall ,(R s) ,(R d)) ac)]
         [(sra)  (cons `(sarl ,(R s) ,(R d)) ac)]
         [else (error who "invalid instr ~s" x)])]
      [(primcall op rands)
       (case op
         [(nop) ac]
         [(record-effect) 
          (let ([a (car rands)])
            (unless (symbol? a) 
              (error who "invalid arg to record-effect ~s" a))
            (list* `(shrl ,pageshift ,a)
                   `(sall ,wordshift ,a)
                   `(addl ,(pcb-ref 'dirty-vector) ,a)
                   `(movl ,dirty-word (disp 0 ,a))
                   ac))]
         [else (error who "invalid effect ~s" x)])]
      [else (error who "invalid effect ~s" x)]))
  ;;;
  (define (unique-label)
    (label (gensym)))
  ;;;
  (define (P x lt lf ac)
    (record-case x
      [(constant c) 
       (if c
           (if lt (cons `(jmp ,lt) ac) ac)
           (if lf (cons `(jmp ,lf) ac) ac))]
      [(seq e0 e1)
       (E e0 (P e1 lt lf ac))]
      [(conditional e0 e1 e2)
       (cond
         [(and lt lf) 
          (let ([l (unique-label)])
            (P e0 #f l
               (P e1 lt lf
                  (cons l (P e2 lt lf ac)))))]
         [lt
          (let ([lf (unique-label)] [l (unique-label)])
            (P e0 #f l
               (P e1 lt lf
                  (cons l (P e2 lt #f (cons lf ac))))))]
         [lf 
          (let ([lt (unique-label)] [l (unique-label)])
            (P e0 #f l
               (P e1 lt lf
                  (cons l (P e2 #f lf (cons lt ac))))))]
         [else
          (let ([lf (unique-label)] [l (unique-label)])
            (P e0 #f l
               (P e1 #f #f
                  (cons `(jmp ,lf)
                    (cons l (P e2 #f #f (cons lf ac)))))))])]
      [(asm-instr op a0 a1)
       (let ()
         (define (notop x)
           (cond
             [(assq x '([= !=] [!= =] [< >=] [<= >] [> <=] [>= <]))
              => cadr]
             [else (error who "invalid op ~s" x)]))
         (define (jmpname x)
           (cond
             [(assq x '([= je] [!= jne] [< jl] [<= jle] [> jg] [>= jge]))
              => cadr]
             [else (error who "invalid jmpname ~s" x)]))
         (define (revjmpname x)
           (cond
             [(assq x '([= je] [!= jne] [< jg] [<= jge] [> jl] [>= jle]))
              => cadr]
             [else (error who "invalid jmpname ~s" x)]))
         (define (cmp op a0 a1 lab ac)
           (cond
             [(or (symbol? a0) (constant? a1))
              (list* `(cmpl ,(R a1) ,(R a0))
                     `(,(jmpname op) ,lab)
                     ac)]
             [(or (symbol? a1) (constant? a0))
              (list* `(cmpl ,(R a0) ,(R a1))
                     `(,(revjmpname op) ,lab)
                     ac)]
             [else (error who "invalid ops ~s ~s" a0 a1)]))
         (cond
           [(and lt lf)
            (cmp op a0 a1 lt
                (cons `(jmp ,lf) ac))]
           [lt 
            (cmp op a0 a1 lt ac)]
           [lf 
            (cmp (notop op) a0 a1 lf ac)]
           [else ac]))]
      [else (error who "invalid pred ~s" x)]))
  ;;;
  (define (T x ac)
    (record-case x
      [(seq e0 e1) (E e0 (T e1 ac))]
      [(conditional e0 e1 e2)
       (let ([L (unique-label)])
         (P e0 #f L (T e1 (cons L (T e2 ac)))))]
      [(primcall op rands)
       (case op
        [(return) (cons '(ret) ac)]
        [(indirect-jump) 
         (cons `(jmp (disp ,(fx- disp-closure-code closure-tag) ,cp-register))
               ac)]
        [(direct-jump)
         (cons `(jmp (label ,(code-loc-label (car rands)))) ac)]
        [else (error who "invalid tail ~s" x)])]
      [else (error who "invalid tail ~s" x)]))
  ;;;
  (define (handle-vararg fml-count ac)
    (define CONTINUE_LABEL (unique-label))
    (define DONE_LABEL (unique-label))
    (define CONS_LABEL (unique-label))
    (define LOOP_HEAD (unique-label))
    (define L_CALL (unique-label))
    (list* (cmpl (int (argc-convention (fxsub1 fml-count))) eax)
           (jg (label SL_invalid_args))
           (jl CONS_LABEL)
           (movl (int nil) ebx)
           (jmp DONE_LABEL)
           CONS_LABEL
           (movl (pcb-ref 'allocation-redline) ebx)
           (addl eax ebx)
           (addl eax ebx)
           (cmpl ebx apr)
           (jle LOOP_HEAD)
           ; overflow
           (addl eax esp) ; advance esp to cover args
           (pushl cpr)    ; push current cp
           (pushl eax)    ; push argc
           (negl eax)     ; make argc positive
           (addl (int (fx* 4 wordsize)) eax) ; add 4 words to adjust frame size
           (pushl eax)    ; push frame size
           (addl eax eax) ; double the number of args
           (movl eax (mem (fx* -2 wordsize) fpr)) ; pass it as first arg
           (movl (int (argc-convention 1)) eax) ; setup argc
           (movl (primref-loc 'do-vararg-overflow) cpr) ; load handler
           (jmp L_CALL)   ; go to overflow handler
           ; NEW FRAME
           '(int 0)        ; if the framesize=0, then the framesize is dynamic
           '(current-frame-offset)
           '(int 0)        ; multiarg rp
           (byte 0)
           (byte 0)
           L_CALL
           (indirect-cpr-call)
           (popl eax)     ; pop framesize and drop it
           (popl eax)     ; reload argc
           (popl cpr)     ; reload cp
           (subl eax fpr) ; readjust fp
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
           (cmpl (int (fx- 0 (fxsll fml-count fx-shift))) eax)
           (jle CONTINUE_LABEL)
           DONE_LABEL
           (movl ebx (mem (fx- 0 (fxsll fml-count fx-shift)) fpr))
           ac))
  ;;;
  (define (properize args proper ac)
    (cond
      [proper ac]
      [else
       (handle-vararg (length (cdr args)) ac)]))
  ;;;
  (define (ClambdaCase x ac) 
    (record-case x
      [(clambda-case info body)
       (record-case info
         [(case-info L args proper)
          (let ([lothers (unique-label)])
            (list* `(cmpl ,(argc-convention 
                             (if proper 
                                 (length (cdr args))
                                 (length (cddr args))))
                          ,argc-register)
                   (cond
                     [proper `(jne ,lothers)]
                     [(> (argc-convention 0) (argc-convention 1))
                      `(jle ,lothers)]
                     [else
                      `(jge ,lothers)])
               (properize args proper
                  (cons (label L) 
                        (T body (cons lothers ac))))))])]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda L case* free*)
       (list* (length free*) 
              (label L)
          (let f ([case* case*])
            (cond
              [(null? case*) (invalid-args-error)]
              [else
               (ClambdaCase (car case*) (f (cdr case*)))])))]))
  (define (invalid-args-error)
    `((jmp (label ,SL_invalid_args))))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (cons (list* 0 
                    (label (gensym))
                    (T body '()))
             (map Clambda code*))]))
  ;;;
  (Program x))

(define (print-code x)
  (parameterize ([print-gensym '#t])
    (pretty-print (unparse x))))

(define (alt-cogen x)
  (verify-new-cogen-input x)
  (let* (
         ;[foo (print-code x)]
         [x (remove-primcalls x)]
         ;[foo (printf "1")]
         [x (eliminate-fix x)]
         ;[foo (printf "2")]
         [x (normalize-context x)]
         ;[foo (printf "3")]
         ;[foo (print-code x)]
         [x (specify-representation x)]
         [foo (printf "4")]
         [x (impose-calling-convention/evaluation-order x)]
         [foo (printf "5")]
         ;[foo (print-code x)]
         [x (color-by-chaitin x)]
         [foo (printf "6")]
         ;[foo (print-code x)]
         [ls (flatten-codes x)])
    (when #t
      (parameterize ([gensym-prefix "L"]
                     [print-gensym #f])
        (for-each 
          (lambda (ls)
            (newline)
            (for-each (lambda (x) (printf "    ~s\n" x)) ls))
          ls)))
    ls))
  
#|module alt-cogen|#)


