
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
  (define (check-jmp-target x)
    (unless (or (gensym? x)
                (and (pair? x)
                     (eq? (car x) 'symbol-code)
                     (symbol? (cdr x))))
      (error who "invalid jmp target")))
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
       (check-jmp-target label)
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


(module (must-open-code? prim-context)
  (define core-prims
    ;;;ctxt: p=predicate v=value vt=true-value e=effect
    '([pair?             p]
      [vector?           p]
      [null?             p]
      [bwp-object?       p]
      [eof-object?       p]
      [eof-object        vt]
      [$unbound-object?  p]
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
      [not              pv]
      [void              vt]
      [$fx+              vt]
      [$fx-              vt]
      [$fx*              vt]
      [$fxadd1           vt]
      [$fxsub1           vt]
      [$fxsll            vt]
      [$fxsra            vt]
      [$fxlogand         vt]
      [$fxlogor          vt]
      [$fxlogxor         vt]
      [$fxlognot         vt]
      [$fxmodulo         vt]
      [$fxquotient       vt]
      [$fxzero?          p]
      [$fx>              p]
      [$fx>=             p]
      [$fx<              p]
      [$fx<=             p]
      [$fx=              p]
      [-                 v]
      [+                 v]
      [=                 p]
      [<                 p]
      [<=                p]
      [>                 p]
      [>=                p]
      [zero?             p]


      [$char=            p]
      [$char<            p]
      [$char<=           p]
      [$char>            p]
      [$char>=           p]
      [$char->fixnum     vt]
      [$fixnum->char     vt]

      [cons              vt]
      [list              vt]
      [list*             pv]
      [car               v]
      [cdr               v]
      [$car              v]
      [$cdr              v]
      [set-car!          e]
      [set-cdr!          e]
      [$set-car!         e]
      [$set-cdr!         e]


      [vector            vt]
      [$make-vector      vt]
      [$vector-length    vt]
      [vector-length     vt]
      [$vector-ref       v]
      [vector-ref        v]
      [vector-set!       e]
      [$vector-set!      e]

      [$make-string      vt]
      [$string-length    vt]
      [$string-ref       vt]
      [string-ref        vt]
      [$string-set!      e]

      [$make-symbol               vt]
      [$set-symbol-value!         e]
      [$symbol-string             v]
      [$symbol-unique-string      v]
      [$set-symbol-unique-string! e]
      [$symbol-plist              vt]
      [$set-symbol-plist!         e]
      [$set-symbol-string!        e]
      [top-level-value            v]
      [$symbol-value              v]

      [$memq                     pv]
      [$procedure-check           v]

      [$record           vt]
      [$record/rtd?      p]
      [$record-ref       v]
      [$record-set!      e]
      [$record?          p]
      [$record-rtd       vt]
      [$make-record      vt]

      ;;; ports
      [output-port?             p]
      [input-port?              p]
      [port?                    p]
      [$make-port/input         vt]
      [$make-port/output        vt]
      [$make-port/both          vt]
      [$port-handler            vt]
      [$port-input-buffer       vt]
      [$port-input-index        vt]
      [$port-input-size         vt]
      [$port-output-buffer      vt]
      [$port-output-index       vt]
      [$port-output-size        vt]
      [$set-port-input-index!   e]
      [$set-port-input-size!    e]
      [$set-port-output-index!  e]
      [$set-port-output-size!   e]

      [$code?             p]
      [$code-size         vt]
      [$code-reloc-vector vt]
      [$code-freevars     vt]
      [$code-ref          vt]
      [$code-set!         e]
      [$code->closure     vt]
      [$closure-code      vt]
      
      [$make-tcbucket         vt]
      [$tcbucket-key           v]
      [$tcbucket-val           v]
      [$tcbucket-next         vt]
      [$set-tcbucket-tconc!    e]
      [$set-tcbucket-val!      e]
      [$set-tcbucket-next!     e]
      
      [$cpref                  v]
      [primitive-set!          e]
      [primitive-ref           v]

      [pointer-value          vt]
      [$fp-at-base             p]
      [$current-frame         vt]
      [$seal-frame-and-call tail]
      [$frame->continuation   vt]
      [$forward-ptr?           p]

      [$make-call-with-values-procedure vt]
      [$make-values-procedure vt]
      [$arg-list vt]
      [$interrupted?       p]
      [$unset-interrupted! e]

      ))
  (define (must-open-code? x)
    (and (assq x core-prims) #t))
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
    (define (do-fix lhs* rhs* body)
      (define (handle-closure x)
        (record-case x
          [(closure code free*) 
           (make-closure code (map Var free*))]))
      (make-fix lhs* (map handle-closure rhs*) body))
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
      [(forcall) (Predicafy x)]
      [(fix lhs* rhs* body) 
       (make-fix lhs* rhs* (P body))]
      [(primcall op rands)
       (case (prim-context op)
         [(v) (Predicafy x)]
         [(p) (make-primcall op (map V rands))]
         [(vt e) (make-seq (E x) (make-constant #t))]
         [(pv) 
          (case op
            [(list*) 
             (case (length rands)
               [(1) (P (car rands))]
               [else (make-seq (E x) (make-constant #t))])]
            [($memq) 
             (record-case (cadr rands)
               [(constant ls)
                (unless (list? ls) (error who "invalid call to $memq"))
                (cond
                  [(null? ls) 
                   (make-seq (E (car rands)) (make-constant #f))]
                  [else
                   (let ([t (unique-var 'tmp)])
                     (make-bind (list t) (list (V (car rands)))
                        (let f ([ls ls])
                          (cond
                            [(null? (cdr ls)) 
                             (make-primcall 'eq? (list t (make-constant (car ls))))]
                            [else
                             (make-conditional 
                               (make-primcall 'eq? (list t (make-constant (car ls))))
                               (make-constant #t)
                               (f (cdr ls)))]))))])]
               [else (Predicafy x)])]
            [(not)
             (make-conditional 
               (P (car rands)) 
               (make-constant #f)
               (make-constant #t))]
            [else (error who "unhandled pv prim ~s" op)])]
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
      [(fix lhs* rhs* body) 
       (make-fix lhs* rhs* (E body))]
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
      [(forcall op rands) (make-forcall op (map V rands))]
      [(primcall op rands)
       (case (prim-context op)
         [(p v pv vt) 
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
      [(fix lhs* rhs* body) 
       (make-fix lhs* rhs* (V body))]
      [(funcall rator rand*)
       (make-funcall (V rator) (map V rand*))]
      [(jmpcall label rator rand*) 
       (make-jmpcall label (V rator) (map V rand*))]
      [(forcall op rands) (make-forcall op (map V rands))]
      [(primcall op rands)
       (case (prim-context op)
         [(v vt tail) (make-primcall op (map V rands))]
         [(p) (Unpred x)]
         [(e) (make-seq (E x) (make-constant (void)))]
         [(pv)
          (case op
            [($memq) 
             (record-case (cadr rands)
               [(constant ls)
                (unless (list? ls) (error who "invalid call to $memq"))
                (cond
                  [(null? ls) 
                   (make-seq (E (car rands)) (make-constant #f))]
                  [else
                   (let ([t (unique-var 'tmp)])
                     (make-bind (list t) (list (V (car rands)))
                        (let f ([ls ls])
                          (cond
                            [(null? ls) 
                             (make-constant #f)]
                            [else
                             (make-conditional 
                               (make-primcall 'eq? (list t (make-constant (car ls))))
                               (make-constant ls)
                               (f (cdr ls)))]))))])]
               [else (make-funcall (make-primref '$memq) (map V rands))])]
            [(list*) 
             (case (length rands)
               [(0) (make-funcall (make-primref 'list*) '())]
               [(1) (V (car rands))]
               [else (make-primcall 'list* (map V rands))])]
            [(not)
             (make-conditional 
               (P (car rands)) 
               (make-constant #f)
               (make-constant #t))]
            [else (error who "unhandled pv ~s" op)])]
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



(define (remove-complex-operands x)
  (define who 'remove-complex-operands)
  (define (mkbind lhs* rhs* body)
    (if (null? lhs*) body (make-bind lhs* rhs* body)))
  (define (simplify* arg* op)
    (define (partition arg*)
      (if (null? arg*)
          (values '() '() '())
          (let ([a (car arg*)])
            (let-values ([(lhs* rhs* arg*) (partition (cdr arg*))])
              (record-case a
                [(constant) (values lhs* rhs* (cons a arg*))]
                [(var)      (values lhs* rhs* (cons a arg*))]
                [(code-loc) (values lhs* rhs* (cons a arg*))]
                [(closure)  (values lhs* rhs* (cons a arg*))]
                [else 
                 (let ([t (unique-var 'tmp)])
                   (values (cons t lhs*)
                           (cons a rhs*)
                           (cons t arg*)))])))))
    (let ([arg* (map V arg*)])
      (let-values ([(lhs* rhs* arg*) (partition arg*)])
        (mkbind lhs* rhs* (make-primcall op arg*)))))
  (define (E x)
    (record-case x
      [(bind lhs* rhs* body)
       (mkbind lhs* (map V rhs*) (E body))]
      [(fix lhs* rhs* body) 
       (make-fix lhs* rhs* (E body))]
      [(conditional e0 e1 e2) 
       (make-conditional (P e0) (E e1) (E e2))]
      [(seq e0 e1)
       (make-seq (E e0) (E e1))]
      [(primcall op arg*) (simplify* arg* op)]
      [(forcall op arg*)
       (make-forcall op (map V arg*))]
      [(funcall rator arg*)
       (make-funcall (V rator) (map V arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (V rator) (map V arg*))]
      [else (error who "invalid effect expr ~s" x)]))  
  (define (P x)
    (record-case x
      [(constant) x]
      [(bind lhs* rhs* body)
       (mkbind lhs* (map V rhs*) (P body))]
      [(fix lhs* rhs* body) 
       (make-fix lhs* rhs* (P body))]
      [(conditional e0 e1 e2) 
       (make-conditional (P e0) (P e1) (P e2))]
      [(seq e0 e1)
       (make-seq (E e0) (P e1))]
      [(primcall op arg*) (simplify* arg* op)]
      [else (error who "invalid pred expr ~s" x)])) 
  (define (V x)
    (record-case x
      [(constant) x]
      [(var)      x]
      [(primref name) x] 
      [(code-loc) x]
      [(closure)  x]
      [(bind lhs* rhs* body)
       (mkbind lhs* (map V rhs*) (V body))]
      [(fix lhs* rhs* body) 
       (make-fix lhs* rhs* (V body))]
      [(conditional e0 e1 e2) 
       (make-conditional (P e0) (V e1) (V e2))]
      [(seq e0 e1)
       (make-seq (E e0) (V e1))]
      [(primcall op arg*) (simplify* arg* op)]
      [(forcall op arg*)
       (make-forcall op (map V arg*))]
      [(funcall rator arg*)
       (make-funcall (V rator) (map V arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (V rator) (map V arg*))]
      [else (error who "invalid value expr ~s" x)])) 
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
  (Program x))


(define-syntax seq*
  (syntax-rules ()
    [(_ e) e]
    [(_ e* ... e) 
     (make-seq (seq* e* ...) e)]))


(include "pass-specify-rep.ss")

#;(define (specify-representation x)
  (define who 'specify-representation)
  ;;;
  (define fixnum-scale 4)
  (define fixnum-shift 2)
  (define fixnum-tag 0)
  (define fixnum-mask 3)
  (define pcb-dirty-vector-offset 28)
  ;;;
  (define nop (make-primcall 'nop '()))
  ;;;
  (import primops)
  (define (handle-fix lhs* rhs* body)
    (define (closure-size x)
      (record-case x
        [(closure code free*) 
         (if (null? free*) 
             0
             (align (+ disp-closure-data
                       (* (length free*) wordsize))))]))
    (define (partition p? lhs* rhs*)
      (cond
        [(null? lhs*) (values '() '() '() '())]
        [else
         (let-values ([(a* b* c* d*)
                       (partition p? (cdr lhs*) (cdr rhs*))]
                      [(x y) (values (car lhs*) (car rhs*))])
           (cond
             [(p? x y)
              (values (cons x a*) (cons y b*) c* d*)]
             [else 
              (values a* b* (cons x c*) (cons y d*))]))]))
    (define (combinator? lhs rhs)
      (record-case rhs
        [(closure code free*) (null? free*)]))
    (define (sum n* n)
      (cond
        [(null? n*) n]
        [else (sum (cdr n*) (+ n (car n*)))]))
    (define (adders lhs n n*)
      (cond
        [(null? n*) '()]
        [else
         (cons (prm 'int+ lhs (K n))
               (adders lhs (+ n (car n*)) (cdr n*)))]))
    (define (build-closures lhs* rhs* body)
      (let ([lhs (car lhs*)] [rhs (car rhs*)]
            [lhs* (cdr lhs*)] [rhs* (cdr rhs*)])
        (let ([n (closure-size rhs)] 
              [n* (map closure-size rhs*)])
          (make-bind (list lhs) 
                     (list (prm 'alloc 
                                (K (sum n* n))
                                (K closure-tag)))
            (make-bind lhs* (adders lhs n n*)
              body)))))
    (define (build-setters lhs* rhs* body)
      (define (build-setter lhs rhs body)
        (record-case rhs
          [(closure code free*) 
           (make-seq
             (prm 'mset lhs 
                  (K (- disp-closure-code closure-tag))
                  (Value code))
             (let f ([ls free*] 
                     [i (- disp-closure-data closure-tag)])
               (cond
                 [(null? ls) body]
                 [else
                  (make-seq
                    (prm 'mset lhs (K i) (Value (car ls)))
                    (f (cdr ls) (+ i wordsize)))])))]))
      (cond
        [(null? lhs*) body]
        [else
         (build-setter (car lhs*) (car rhs*)
           (build-setters (cdr lhs*) (cdr rhs*) body))]))
    (let-values ([(flhs* frhs* clhs* crhs*)
                  (partition combinator? lhs* rhs*)])
      (cond
        [(null? clhs*) (make-bind flhs* (map Value frhs*) body)]
        [(null? flhs*)
         (build-closures clhs* crhs*
            (build-setters clhs* crhs* body))]
        [else
         (make-bind flhs* (map Value frhs*)
           (build-closures clhs* crhs*
             (build-setters clhs* crhs* body)))])))
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
         #'(let ([ls (list rhs* ...)])
             (let ([lhs* (unique-var 'lhs*)] ...)
               (make-bind (list lhs* ...) ls
                  (begin b b* ...))))])))
  (define (Effect x)
    (define (dirty-vector-set address)
      (prm 'mset 
         (prm 'int+
              (prm 'mref pcr (K 28)) ;;; FIXME: make srl
              (prm 'sll (prm 'sra address (K pageshift)) (K wordshift)))
         (K 0)
         (K dirty-word)))
    (define (smart-dirty-vector-set addr what)
      (record-case what
        [(constant t) 
         (if (or (fixnum? t) (immediate? t))
             (prm 'nop)
             (dirty-vector-set addr))]
        [else (dirty-vector-set addr)]))
    (define (mem-assign v x i)
      (tbind ([q v])
        (tbind ([t (prm 'int+ x (K i))])
          (make-seq 
            (prm 'mset t (K 0) q)
            (dirty-vector-set t)))))
    (define (smart-mem-assign what v x i)
      (record-case what
        [(constant t) 
         (if (or (fixnum? t) (immediate? t))
             (prm 'mset x (K i) v)
             (mem-assign v x i))]
        [else (mem-assign v x i)]))
    (record-case x
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Value rhs*) (Effect body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Effect e1) (Effect e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Effect e1))]
      [(fix lhs* rhs* body) 
       (handle-fix lhs* rhs* (Effect body))]
      [(primcall op arg*)
       (case op
         [(nop) nop]
         [($set-symbol-value!)
          (tbind ([x (Value (car arg*))] [v (Value (cadr arg*))])
            (seq*
              (prm 'mset x (K (- disp-symbol-value symbol-tag)) v)
              (prm 'mset x (K (- disp-symbol-function symbol-tag))
                   (prm 'mref x (K (- disp-symbol-error-function symbol-tag))))
              (dirty-vector-set x)))]
         [($init-symbol-function!)
          (tbind ([x (Value (car arg*))] [v (Value (cadr arg*))])
            (seq*
              (prm 'mset x (K (- disp-symbol-function symbol-tag)) v)
              (prm 'mset x (K (- disp-symbol-error-function symbol-tag)) v)
              (dirty-vector-set x)))]
         [(primitive-set! $set-symbol-value! $set-symbol-string!
           $set-symbol-unique-string! $set-symbol-plist!)
          (let ([off
                 (case op
                   [(primitive-set!) disp-symbol-system-value]
                   [($set-symbol-value!) disp-symbol-value]
                   [($set-symbol-string!) disp-symbol-string]
                   [($set-symbol-unique-string!) disp-symbol-unique-string]
                   [($set-symbol-plist!) disp-symbol-plist]
                   [else (err x)])])
            (tbind ([x (Value (car arg*))] [v (Value (cadr arg*))])
              (mem-assign v x (- off symbol-tag))))]
         [($vector-set! $record-set!)
          (tbind ([x (Value (car arg*))] 
                  [v (Value (caddr arg*))])
            (let ([i (cadr arg*)])
              (record-case i
                [(constant i) 
                 (unless (fixnum? i) 
                   (error who "invalid arg ~s to ~s" i op))
                 (mem-assign v x 
                    (+ (* i wordsize)
                       (- disp-vector-data vector-tag)))]
                [else
                 (tbind ([i (Value i)])
                   (mem-assign v 
                      (prm 'int+ x i)
                      (- disp-vector-data vector-tag)))])))]
         [(vector-set!)
          (tbind ([a0 (Value (car arg*))]
                  [val (Value (caddr arg*))])
            (let ([a1 (cadr arg*)])
              (record-case a1
                [(constant i)
                 (if (and (fixnum? i) (fx>= i 0))
                     (make-shortcut
                       (seq*
                         (make-conditional
                           (tag-test a0 vector-mask vector-tag)
                           (prm 'nop)
                           (prm 'interrupt))
                         (tbind ([t (prm 'mref a0
                                      (K (- disp-vector-length vector-tag)))])
                           (seq* 
                              (make-conditional
                                (prm '< (K (* i fixnum-scale)) t)
                                (prm 'nop)
                                (prm 'interrupt))
                              (make-conditional
                                (tag-test t fixnum-mask fixnum-tag)
                                (prm 'nop)
                                (prm 'interrupt))
                              (smart-mem-assign (caddr arg*) val a0 
                                (+ (* i wordsize)
                                   (- disp-vector-data vector-tag))))))
                       (Effect
                         (make-funcall (make-primref 'vector-set!)
                           (list a0 (Value a1) val))))
                     (Effect 
                       (make-funcall (make-primref 'vector-set!)
                         (list a0 (Value a1) val))))]
                [else
                 (tbind ([a1 (Value a1)])
                   (make-shortcut
                      (seq*
                        (make-conditional
                          (tag-test a0 vector-mask vector-tag)
                          (prm 'nop)
                          (prm 'interrupt))
                        (tbind ([t (prm 'mref a0
                                       (K (- disp-vector-length vector-tag)))])
                          (seq* 
                             (make-conditional
                               (prm 'u< a1 t)
                               (prm 'nop)
                               (prm 'interrupt))
                             (make-conditional
                               (tag-test (prm 'logor t a1) fixnum-mask fixnum-tag)
                               (prm 'nop)
                               (prm 'interrupt))
                             (mem-assign val 
                               (prm 'int+ a0 a1)
                               (- disp-vector-data vector-tag)))))
                      (Effect
                        (make-funcall (make-primref 'vector-set!)
                          (list a0 a1 val)))))])))] 
         [($set-car! $set-cdr!)
          (let ([off (if (eq? op '$set-car!) 
                         (- disp-car pair-tag)
                         (- disp-cdr pair-tag))])
            (tbind ([x (Value (car arg*))]
                    [v (Value (cadr arg*))])
              (seq* ;;; car/cdr addresses are in the same 
                    ;;; card as the pair address, so no
                    ;;; adjustment is necessary as was the
                    ;;; case with vectors and records.
                (prm 'mset x (K off) v)
                (dirty-vector-set x))))]
         [(set-car! set-cdr!)
          (let ([off (if (eq? op 'set-car!) 
                         (- disp-car pair-tag)
                         (- disp-cdr pair-tag))])
            (tbind ([x (Value (car arg*))]
                    [v (Value (cadr arg*))])
              (seq* ;;; car/cdr addresses are in the same 
                    ;;; card as the pair address, so no
                    ;;; adjustment is necessary as was the
                    ;;; case with vectors and records.
                (make-shortcut
                  (seq*
                    (make-conditional
                      (tag-test x pair-mask pair-tag)
                      (prm 'nop)
                      (prm 'interrupt))
                    (prm 'mset x (K off) v)
                    (smart-dirty-vector-set x (cadr arg*)))
                  (Effect 
                    (make-funcall (make-primref 'error)
                      (list (K op) (K "~s is not a pair") x)))))))]
         [($string-set!)
          (tbind ([x (Value (car arg*))]) 
            (let ([i (cadr arg*)]
                  [c (caddr arg*)])
              (record-case i
                [(constant i) 
                 (unless (fixnum? i) 
                   (error who "invalid arg ~s to ~s" i op))
                 (record-case c
                   [(constant c)
                    (unless (char? c) (err x))
                    (prm 'bset/c x 
                         (K (+ i (- disp-string-data string-tag)))
                         (K (char->integer c)))]
                   [else
                    (unless (= char-shift 8)
                      (error who "assumption about char-shift"))
                    (tbind ([c (Value c)])
                     (prm 'bset/h x
                          (K (+ i (- disp-string-data string-tag)))
                          c))])]
                [else
                 (tbind ([i (Value i)])
                   (record-case c
                    [(constant c)
                     (unless (char? c) (err x))
                     (prm 'bset/c x 
                          (prm 'sra i (K fixnum-shift))
                          (K (char->integer c)))]
                    [else
                     (unless (= char-shift 8)
                       (error who "assumption about char-shift"))
                     (tbind ([c (Value c)])
                      (prm 'bset/h x
                           (prm 'int+ 
                                (prm 'sra i (K fixnum-shift))
                                (K (- disp-string-data string-tag)))
                           c))]))])))]
         [($code-set!) 
          (tbind ([x (Value (car arg*))]
                  [i (Value (cadr arg*))]
                  [v (Value (caddr arg*))])
            (prm 'bset/h x
                 (prm 'int+ 
                      (prm 'sra i (K fixnum-shift))
                      (K (- disp-code-data vector-tag)))
                 (prm 'sll v (K (- 8 fixnum-shift)))))]
         [($unset-interrupted!) ;;; PCB INTERRUPT
          (prm 'mset pcr (K 40) (K 0))]
         [($set-port-input-index!  $set-port-output-index!)
          (let ([off (case op
                       [($set-port-input-index!)  disp-port-input-index]
                       [($set-port-output-index!) disp-port-output-index]
                       [else (err x)])])
            (tbind ([x (Value (car arg*))]
                    [v (Value (cadr arg*))])
               (prm 'mset x (K (- off vector-tag)) v)))]
         [($set-port-input-size!   $set-port-output-size!)
          (let-values ([(sz-off idx-off)
                        (case op
                          [($set-port-input-size!) 
                           (values disp-port-input-size
                                   disp-port-input-index)]
                          [($set-port-output-size!) 
                           (values disp-port-output-size
                                   disp-port-output-index)]
                          [else (err x)])])
            (tbind ([x (Value (car arg*))]
                    [v (Value (cadr arg*))])
               (seq*
                 (prm 'mset x (K (- idx-off vector-tag)) (K 0))
                 (prm 'mset x (K (- sz-off vector-tag)) v))))]
         [($set-tcbucket-next! $set-tcbucket-val! $set-tcbucket-tconc!)
          (tbind ([x (Value (car arg*))]
                  [v (Value (cadr arg*))])
            (mem-assign v x
              (- (case op
                   [($set-tcbucket-tconc!) disp-tcbucket-tconc]
                   [($set-tcbucket-next!)  disp-tcbucket-next]
                   [($set-tcbucket-val!)   disp-tcbucket-val]
                   [else (err 'tcbucket!)])
                 vector-tag)))]
         [else
          (if (primop? op)
              (cogen-primop op 'E arg*)
              (error who "invalid effect prim ~s" op))])]
      [(forcall op arg*)
       (make-forcall op (map Value arg*))]
      [(funcall rator arg*)
       (make-funcall (Function rator) (map Value arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (Value rator) (map Value arg*))]
      [(mvcall rator x)
       (make-mvcall (Value rator) (Clambda x Effect))]
      [else (error who "invalid effect expr ~s" x)]))
  ;;;
  (define (tag-test x mask tag)
    (tbind ([x x])
      (if mask
          (prm '= 
             (prm 'logand x (K mask))
             (K tag))
          (prm '= x (K tag)))))
  (define (sec-tag-test x pmask ptag smask stag)
    (tbind ([t x])
      (make-conditional 
        (tag-test t pmask ptag)
        (tag-test (prm 'mref t (K (- ptag))) smask stag)
        (make-constant #f))))
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
      [(fix lhs* rhs* body) 
       (handle-fix lhs* rhs* (Pred body))]
      [(primcall op arg*)
       (case op
         [(eq?)  (make-primcall '= (map Value arg*))]
         [(null?) (prm '= (Value (car arg*)) (K nil))]
         [(eof-object?) (prm '= (Value (car arg*)) (K eof))]
         [(bwp-object?) (prm '= (Value (car arg*)) (K bwp-object))]
         [(neq?) (make-primcall '!= (map Value arg*))]
         [($fxzero?) (prm '= (Value (car arg*)) (K 0))]
         [(zero?) 
          (tbind ([x (Value (car arg*))])
            (make-conditional
              (tag-test x fixnum-mask fixnum-tag)
              (prm '= x (K 0))
              (prm '!=
                   (make-funcall (Value (make-primref 'zero?)) (list x))
                   (Value (K #f)))))]
         [($unbound-object?) (prm '= (Value (car arg*)) (K unbound))]
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
         [($forward-ptr?)
          (tbind ([x (Value (car arg*))]) (prm '= x (K -1)))]
         [($record?)
          (sec-tag-test (Value (car arg*))
             vector-mask vector-tag vector-mask vector-tag)]
         [($code?)
          (sec-tag-test (Value (car arg*))
             vector-mask vector-tag #f code-tag)]
         [(input-port?)
          (sec-tag-test (Value (car arg*)) 
             vector-mask vector-tag #f input-port-tag)]
         [(output-port?)
          (sec-tag-test (Value (car arg*)) 
             vector-mask vector-tag #f output-port-tag)]
         [(port?)
          (sec-tag-test (Value (car arg*))
             vector-mask vector-tag port-mask port-tag)]
         [($record/rtd?)
          (tbind ([t (Value (car arg*))]
                  [v (Value (cadr arg*))])
            (make-conditional 
              (tag-test t vector-mask vector-tag)
              (prm '= (prm 'mref t (K (- vector-tag))) v)
              (make-constant #f)))]
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
         [($interrupted?)
          (prm '!= (prm 'mref pcr (K 40)) (K 0))]
         [($fx= $char=) 
          (prm '= (Value (car arg*)) (Value (cadr arg*)))]
         [($fx< $char<) 
          (prm '< (Value (car arg*)) (Value (cadr arg*)))]
         [($fx> $char>) 
          (prm '> (Value (car arg*)) (Value (cadr arg*)))]
         [($fx<= $char<=) 
          (prm '<= (Value (car arg*)) (Value (cadr arg*)))]
         [($fx>= $char>=) 
          (prm '>= (Value (car arg*)) (Value (cadr arg*)))]
         [(= < <= > >=) 
          (unless (= (length arg*) 2)
            (error who "only binary ~s for now" op))
          (let ([cmp?
                 (case op
                   [(=)  =]
                   [(<)  <]
                   [(<=) <=]
                   [(>)  >]
                   [(>=) >=]
                   [else (error who "unhandled op ~s" op)])])
            (let ([a (car arg*)] [b (cadr arg*)])
              (define (call a b)
                (prm '!= (Value (K #f))
                     (make-funcall 
                       (Value (make-primref op))
                       (list a b))))
              (record-case a
                [(constant i) 
                 (cond
                   [(fixnum? i) 
                    (record-case b
                      [(constant j) 
                       (if (fixnum? j)
                           (make-constant (cmp? i j))
                           (call (Value a) (Value b)))]
                      [else
                       (tbind ([b (Value b)])
                         (make-conditional
                           (tag-test b fixnum-mask fixnum-tag)
                           (prm op (Value a) b)
                           (call (Value a) b)))])]
                   [else
                    (call (Value a) (Value b))])]
                [else
                 (record-case b
                   [(constant j)
                    (if (fixnum? j)
                        (tbind ([a (Value a)])
                          (make-shortcut
                            (make-seq
                              (make-conditional
                                (tag-test a fixnum-mask fixnum-tag)
                                (make-primcall 'nop '())
                                (make-primcall 'interrupt '()))
                              (prm op a (Value b)))
                            (call a (Value b))))
                        (call (Value a) (Value b)))]
                   [else 
                    (tbind ([a (Value a)] [b (Value b)])
                      (make-conditional 
                        (tag-test (prm 'logor a b) fixnum-mask fixnum-tag)
                        (prm op a b)
                        (call a b)))])])))]
         [else (error who "pred prim ~a not supported" op)])]
      [(mvcall rator x)
       (make-mvcall (Value rator) (Clambda x Pred))]
      [else (error who "invalid pred expr ~s" x)])) 
  ;;;
  (define (err x)
    (error who "invalid form ~s" (unparse x)))
  ;;;
  (define (align-code unknown-amt known-amt)
    (prm 'sll 
       (prm 'sra
            (prm 'int+ unknown-amt
                 (K (+ known-amt (sub1 object-alignment))))
            (K align-shift))
       (K align-shift)))
  (define (remove-complex* ls k)
    (let-values ([(lhs* rhs* arg*) 
                  (let f ([ls ls])
                    (cond
                      [(null? ls) (values '() '() '())]
                      [else
                       (let-values ([(lhs* rhs* arg*)
                                     (f (cdr ls))])
                         (let ([a (car ls)])
                           (cond
                             [(or (var? a) (complex? a))
                              (values lhs* rhs* (cons a arg*))]
                             [else
                              (let ([t (unique-var 'tmp)])
                                (values
                                  (cons t lhs*)
                                  (cons (Value a) rhs*)
                                  (cons t arg*)))])))]))])
       (cond
         [(null? lhs*) 
          (k arg*)]
         [else
          (make-bind lhs* rhs*
            (k arg*))])))
  (define encountered-symbol-calls '())
  (define (Function x)
    (define (nonproc x)
      (tbind ([t (Value x)])
        (make-shortcut
          (make-seq
            (make-conditional
              (tag-test t closure-mask closure-tag)
              (prm 'nop)
              (prm 'interrupt))
            t)
          (Value 
            (make-funcall (make-primref 'error)
              (list (K 'apply) (K "~s is not a procedure") t))))))
    (record-case x
       [(primcall op args)
        (cond
          [(and (eq? op 'top-level-value)
                (= (length args) 1)
                (record-case (car args)
                  [(constant t) 
                   (and (symbol? t) t)]
                  [else #f])) =>
           (lambda (sym)
             (unless (memq sym encountered-symbol-calls)
               (set! encountered-symbol-calls 
                 (cons sym encountered-symbol-calls)))
             (prm 'mref (Value (K sym))
                  (K (- disp-symbol-function symbol-tag))))]
          [else 
           (nonproc x)])]
       [(primref op)  (Value x)]
       [else (nonproc x)]))
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
      [(fix lhs* rhs* body) 
       (handle-fix lhs* rhs* (Value body))]
      [(conditional e0 e1 e2) 
       (make-conditional (Pred e0) (Value e1) (Value e2))]
      [(seq e0 e1)
       (make-seq (Effect e0) (Value e1))]
      [(primcall op arg*)
       (case op
         [(void) (K void-object)]
         [(eof-object) (K eof)]
         [($car) 
          (tbind ([x (Value (car arg*))])
            (prm 'mref x (K (- disp-car pair-tag))))]
         [($cdr) 
          (tbind ([x (Value (car arg*))])
            (prm 'mref x (K (- disp-cdr pair-tag))))]
         [(car cdr) 
          (tbind ([x (Value (car arg*))])
            (make-shortcut
              (make-seq
                 (make-conditional 
                   (tag-test x pair-mask pair-tag)
                   (prm 'nop)
                   (prm 'interrupt))
                 (prm 'mref x (K (- (if (eq? op 'car) disp-car disp-cdr)
                                    pair-tag))))
              (Value 
                (make-funcall (make-primref 'error)
                  (list (K op) (K "~s is not a pair") x)))))] 
         [(primitive-ref) 
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
               (K (- disp-symbol-system-value symbol-tag))))]
         [($symbol-string) 
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
               (K (- disp-symbol-string symbol-tag))))] 
         [($symbol-plist) 
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
                 (K (- disp-symbol-plist symbol-tag))))]
         [($symbol-value) 
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
               (K (- disp-symbol-value symbol-tag))))]
         [($symbol-unique-string) 
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
                 (K (- disp-symbol-unique-string symbol-tag))))]
         [($make-symbol)
          (tbind ([str (Value (car arg*))])
            (tbind ([x (prm 'alloc 
                            (K (align symbol-size))
                            (K symbol-tag))])
              (seq*
                (prm 'mset x 
                     (K (- disp-symbol-string symbol-tag))
                     str)
                (prm 'mset x 
                     (K (- disp-symbol-unique-string symbol-tag))
                     (K 0))
                (prm 'mset x 
                     (K (- disp-symbol-value symbol-tag))
                     (K unbound))
                (prm 'mset x 
                     (K (- disp-symbol-plist symbol-tag))
                     (K nil))
                (prm 'mset x 
                     (K (- disp-symbol-system-value symbol-tag))
                     (K unbound))
                (prm 'mset x 
                     (K (- disp-symbol-function symbol-tag))
                     (K 0))
                (prm 'mset x 
                     (K (- disp-symbol-error-function symbol-tag))
                     (K 0)) 
                (prm 'mset x 
                     (K (- disp-symbol-unused symbol-tag))
                     (K 0))
                x)))]
         [(list)
          (cond
            [(null? arg*) (K nil)]
            [else
             (let ([t* (map (lambda (x) (unique-var 't)) arg*)]
                   [n (length arg*)])
               (make-bind t* (map Value arg*)
                 (tbind ([v (prm 'alloc
                                 (K (align (* n pair-size)))
                                 (K pair-tag))])
                   (seq*
                     (prm 'mset v (K (- disp-car pair-tag)) (car t*))
                     (prm 'mset v
                          (K (- (+ disp-cdr (* (sub1 n) pair-size)) pair-tag))
                          (K nil))
                     (let f ([t* (cdr t*)] [i pair-size])
                       (cond
                         [(null? t*) v]
                         [else
                          (make-seq
                            (tbind ([tmp (prm 'int+ v (K i))])
                              (make-seq
                                (prm 'mset tmp 
                                     (K (- disp-car pair-tag))
                                     (car t*))
                                (prm 'mset tmp
                                     (K (+ disp-cdr (- pair-size) (- pair-tag)))
                                     tmp)))
                            (f (cdr t*) (+ i pair-size)))]))))))])]
         [(list*)
          (let ([result
            (let ([t* (map (lambda (x) (unique-var 't)) arg*)]
                  [n (length arg*)])
              (make-bind t* (map Value arg*)
                (tbind ([v (prm 'alloc
                                (K (* (sub1 n) pair-size))
                                (K pair-tag))])
                  (seq*
                    (prm 'mset v (K (- disp-car pair-tag)) (car t*))
                    (prm 'mset v
                         (K (- (+ disp-cdr (* (- n 2) pair-size)) pair-tag))
                         (car (last-pair t*)))
                    (let f ([t* (cdr t*)] [i pair-size])
                      (cond
                        [(null? (cdr t*)) v]
                        [else
                         (make-seq
                           (tbind ([tmp (prm 'int+ v (K i))])
                             (make-seq
                               (prm 'mset tmp 
                                    (K (- disp-car pair-tag))
                                    (car t*))
                               (prm 'mset tmp
                                    (K (- (- disp-cdr pair-tag) pair-size))
                                    tmp)))
                           (f (cdr t*) (+ i pair-size)))]))))))])
            result)]
         [(vector)
          (let ([t* (map (lambda (x) (unique-var 't)) arg*)])
            (make-bind t* (map Value arg*)
              (tbind ([v (prm 'alloc
                              (K (align (+ disp-vector-data
                                           (* (length t*)
                                              wordsize))))
                              (K vector-tag))])
                (seq*
                  (prm 'mset v (K (- disp-vector-length vector-tag))
                       (K (* (length t*) wordsize)))
                  (let f ([t* t*] [i (- disp-vector-data vector-tag)])
                    (cond
                      [(null? t*) v]
                      [else
                       (make-seq
                         (prm 'mset v (K i) (car t*))
                         (f (cdr t*) (+ i wordsize)))]))))))]
         [($record) 
          (let ([rtd (car arg*)] [v* (map Value (cdr arg*))])
            (tbind ([rtd (Value rtd)])
              (let ([t* (map (lambda (x) (unique-var 'v)) v*)])
                (make-bind t* v*
                  (tbind ([t (prm 'alloc 
                                  (K (align
                                       (+ disp-record-data
                                         (* (length v*) wordsize))))
                                  (K vector-tag))])
                    (seq*
                      (prm 'mset t 
                           (K (- disp-record-rtd vector-tag))
                           rtd)
                      (let f ([t* t*] 
                              [i (- disp-record-data vector-tag)])
                        (cond
                          [(null? t*) t]
                          [else
                           (make-seq 
                             (prm 'mset t (K i) (car t*))
                             (f (cdr t*) (+ i wordsize)))]))))))))]
         [($vector-length)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
                 (K (- disp-vector-length vector-tag))))]
         [($make-vector)
          (unless (= (length arg*) 1)
            (error who "incorrect args to $make-vector"))
          (let ([len (car arg*)])
            (record-case len
              [(constant i)
               (unless (fixnum? i) (error who "invalid ~s" x))
               (tbind ([v (prm 'alloc
                               (K (align (+ (* i wordsize)
                                            disp-vector-data)))
                               (K vector-tag))])
                 (seq*
                   (prm 'mset v 
                        (K (- disp-vector-length vector-tag))
                        (K (make-constant (* i fixnum-scale))))
                   v))]
              [else
               (tbind ([len (Value len)])
                 (tbind ([alen (align-code len disp-vector-data)])
                   (tbind ([v (prm 'alloc alen (K vector-tag))])
                     (seq*
                       (prm 'mset v 
                            (K (- disp-vector-length vector-tag))
                            len)
                       v))))]))]
         [($string-length)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
                 (K (- disp-string-length string-tag))))]
         [($string-ref)
          (tbind ([s (Value (car arg*))])
            (let ([i (cadr arg*)])
              (record-case i
                [(constant i)
                 (unless (fixnum? i) (err x))
                 (prm 'logor
                   (prm 'sll
                     (prm 'logand 
                        (prm 'mref s
                          (K (+ i (- disp-string-data string-tag))))
                        (K 255))
                     (K char-shift))
                   (K char-tag))]
                [else
                 (tbind ([i (Value i)])
                   (prm 'logor
                     (prm 'sll
                       (prm 'srl ;;; FIXME: bref
                          (prm 'mref s
                               (prm 'int+
                                  (prm 'sra i (K fixnum-shift));
                                  ;;; ENDIANNESS DEPENDENCY
                                  (K (- disp-string-data 
                                        (- wordsize 1) 
                                        string-tag))))
                          (K (* (- wordsize 1) 8)))
                       (K char-shift))
                     (K char-tag)))])))]
         [(string-ref)
          (tbind ([s (Value (car arg*))])
            (let ([idx (cadr arg*)])
              (record-case idx
                [(constant i)
                 (cond
                   [(and (fixnum? i) (fx>= i 0))
                    (make-shortcut
                      (seq*
                        (make-conditional
                          (tag-test s string-mask string-tag)
                          (prm 'nop)
                          (prm 'interrupt))
                        (tbind ([len 
                                 (prm 'mref s
                                    (K (- disp-string-length string-tag)))])
                          (make-conditional
                            (prm 'u< (K (* i fixnum-scale)) len)
                            (prm 'nop)
                            (prm 'interrupt)))
                        (Value (prm '$string-ref s idx)))
                      (Value
                        (make-funcall (make-primref 'string-ref) 
                          (list s idx))))]
                   [else 
                    (Value 
                      (make-funcall (make-primref 'string-ref) 
                        (list s idx)))])]
                [else
                 (tbind ([i (Value idx)])
                   (make-shortcut
                     (seq*
                       (make-conditional
                         (tag-test i fixnum-mask fixnum-tag)
                         (prm 'nop)
                         (prm 'interrupt)) 
                       (make-conditional
                         (tag-test s string-mask string-tag)
                         (prm 'nop)
                         (prm 'interrupt))
                       (tbind ([len 
                                (prm 'mref s
                                   (K (- disp-string-length string-tag)))])
                         (make-conditional
                           (prm 'u< i len)
                           (prm 'nop)
                           (prm 'interrupt)))
                       (Value (prm '$string-ref s i)))
                     (Value
                       (make-funcall (make-primref 'string-ref) 
                         (list s i)))))])))]
         [($make-string)
          (unless (= (length arg*) 1) (err x))
          (let ([n (car arg*)])
            (record-case n
              [(constant n)
               (unless (fixnum? n) (err x))
               (tbind ([s (prm 'alloc 
                            (K (align (+ n 1 disp-string-data)))
                            (K string-tag))])
                (seq*
                  (prm 'mset s
                      (K (- disp-string-length string-tag))
                      (K (* n fixnum-scale)))
                  (prm 'bset/c s
                      (K (+ n (- disp-string-data string-tag)))
                      (K 0))
                  s))]
              [else
               (tbind ([n (Value n)])
                 (tbind ([s (prm 'alloc 
                              (align-code 
                                (prm 'sra n (K fixnum-shift))
                                (+ disp-string-data 1))
                              (K string-tag))])
                   (seq*
                     (prm 'mset s
                       (K (- disp-string-length string-tag))
                       n)
                     (prm 'bset/c s
                          (prm 'int+ 
                               (prm 'sra n (K fixnum-shift))
                               (K (- disp-string-data string-tag)))
                          (K 0))
                     s)))]))]
         [($make-record)
          (let ([rtd (car arg*)] [len (cadr arg*)])
            (tbind ([rtd (Value rtd)])
              (record-case len
                [(constant i) 
                 (unless (fixnum? i)
                   (error who "invalid make-rec ~s" len))
                 (tbind ([t (prm 'alloc
                                 (K (align (+ (* i wordsize)
                                              disp-record-data)))
                                 (K vector-tag))])
                   (seq* 
                     (prm 'mset t
                          (K (- disp-record-rtd vector-tag))
                          rtd)
                     t))]
                [else 
                 (tbind ([len (Value len)])
                   (tbind ([ln (align-code len disp-record-data)])
                     (tbind ([t (prm 'alloc ln (K vector-tag))])
                       (seq* 
                         (prm 'mset t 
                              (K (- disp-record-rtd vector-tag))
                              rtd)
                          t))))])))]
         [($record-rtd)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
              (K (- disp-record-rtd vector-tag))))]
         [(cons)
          (tbind ([a (Value (car arg*))]
                  [d (Value (cadr arg*))])
            (tbind ([t (prm 'alloc (K pair-size) (K pair-tag))])
              (seq*
                (prm 'mset t (K (- disp-car pair-tag)) a)
                (prm 'mset t (K (- disp-cdr pair-tag)) d)
                t)))]
         [($fxadd1)
          (prm 'int+ (Value (car arg*)) (K (* 1 fixnum-scale)))]
         [($fxsub1)
          (prm 'int+ (Value (car arg*)) (K (* -1 fixnum-scale)))]
         [($fx+)
          (prm 'int+ (Value (car arg*)) (Value (cadr arg*)))]
         [($fx-)
          (prm 'int- (Value (car arg*)) (Value (cadr arg*)))]
         [($fx*)
          (let ([a (car arg*)] [b (cadr arg*)])
            (record-case a
              [(constant a)
               (unless (fixnum? a) (err x))
               (tbind ([b (Value b)])
                 (prm 'int* b (K a)))]
              [else
               (record-case b
                 [(constant b)
                  (unless (fixnum? b) (err x))
                  (tbind ([a (Value a)])
                    (prm 'int* a (K b)))]
                 [else
                  (tbind ([a (Value a)] [b (Value b)])
                    (prm 'int* a (prm 'sra b (K fixnum-shift))))])]))]
         [($fxquotient) 
          (tbind ([a (Value (car arg*))] [b (Value (cadr arg*))])
            (prm 'sll (prm 'remainder a b) (K fixnum-shift)))]
         [($fxmodulo)
          (tbind ([a (Value (car arg*))]
                  [b (Value (cadr arg*))])
            (tbind ([c (prm 'logand b 
                          (prm 'sra 
                             (prm 'logxor b a)
                             (K (sub1 (* 8 wordsize)))))])
              (prm 'int+ c (prm 'quotient a b))))]
         [($fxsll)
          (let ([a (car arg*)] [c (cadr arg*)])
            (record-case c
              [(constant i) 
               (if (fixnum? i)
                   (tbind ([a (Value a)])
                     (prm 'sll a (K i)))
                   (error who "invalid arg to fxsll ~s" i))]
              [else 
               (tbind ([a (Value a)] [c (Value c)])
                 (prm 'sll a (prm 'sra c (K fixnum-shift))))]))]
         [($fxsra)
          (let ([a (car arg*)] [c (cadr arg*)])
            (record-case c
              [(constant i) 
               (if (fixnum? i)
                   (tbind ([a (Value a)])
                     (prm 'sra a (K i)))
                   (error who "invalid arg to fxsra ~s" i))]
              [else 
               (tbind ([a (Value a)] [c (Value c)])
                 (prm 'logand 
                   (prm 'sra a
                     (prm 'sra c (K fixnum-shift)))
                   (K (* -1 fixnum-scale))))]))]
         [($fxlogand)
          (prm 'logand (Value (car arg*)) (Value (cadr arg*)))]
         [(pointer-value)
          (prm 'logand (Value (car arg*)) (K (* -1 fixnum-scale)))]
         [($fxlogxor)
          (prm 'logxor (Value (car arg*)) (Value (cadr arg*)))]
         [($fxlogor)
          (prm 'logor (Value (car arg*)) (Value (cadr arg*)))]
         [($fxlognot)
          (Value (prm '$fxlogxor (car arg*) (K -1)))]
         [(+)
          (let ([primname 
                 (case op
                   [(+) 'int+/overflow]
                   [else (error who "invalid op ~s" op)])]
                [ID 
                 (case op
                   [(+) 0]
                   [else (error who "invalid op ~s" op)])])
            (define (handle-binary a b)
              (record-case a
                [(constant i)
                 (if (fixnum? i)
                     (tbind ([b (Value b)])
                       (make-shortcut
                         (make-seq
                           (make-conditional
                             (tag-test b fixnum-mask fixnum-tag)
                             (make-primcall 'nop '())
                             (make-primcall 'interrupt '()))
                           (prm primname (Value a) b))
                         (make-funcall (Value (make-primref op))
                            (list (Value a) b))))
                     (make-funcall (Value (make-primref op)) 
                        (list (Value a) b)))]
                [else
                 (record-case b
                   [(constant i)
                    (if (fixnum? i)
                        (tbind ([a (Value a)])
                          (make-shortcut
                            (make-seq
                              (make-conditional
                                (tag-test a fixnum-mask fixnum-tag)
                                (make-primcall 'nop '())
                                (make-primcall 'interrupt '()))
                              (prm primname a (Value b)))
                            (make-funcall (Value (make-primref op))
                               (list a (Value b)))))
                        (make-funcall (Value (make-primref op))
                           (list a (Value b))))]
                   [else
                    (tbind ([a (Value a)]
                            [b (Value b)])
                      (make-shortcut
                        (make-seq
                          (make-conditional
                            (tag-test (prm 'logor a b) fixnum-mask fixnum-tag)
                            (make-primcall 'nop '())
                            (make-primcall 'interrupt '()))
                          (prm primname a b))
                        (make-funcall (Value (make-primref op))
                           (list a b))))])]))
            (cond
              [(null? arg*) (K 0)]
              [(ormap (lambda (x)
                        (record-case x 
                          [(constant i) (not (number? i))]
                          [else #f])) arg*)
               (make-funcall (Value (make-primref op)) (map Value arg*))]
              [(= (length arg*) 1) ;;; FIXME: do something better
               (handle-binary (K ID) (car arg*))]
              [(= (length arg*) 2)
               (handle-binary (car arg*) (cadr arg*))]
              [else
               (remove-complex* arg*
                 (lambda (arg*)
                   (Value 
                     (let f ([a (car arg*)] [d (cdr arg*)])
                        (cond
                          [(null? d) a]
                          [else (f (prm op a (car d)) (cdr d))])))))]))]
         [(-)
          (let ()
            (define (handle-binary a b)
              (record-case a
                [(constant i)
                 (if (fixnum? i)
                     (tbind ([b (Value b)])
                       (make-shortcut
                         (make-seq
                           (make-conditional
                             (tag-test b fixnum-mask fixnum-tag)
                             (make-primcall 'nop '())
                             (make-primcall 'interrupt '()))
                           (prm 'int-/overflow (Value a) b))
                         (make-funcall (Value (make-primref '-))
                            (list (Value a) b))))
                     (make-funcall (Value (make-primref '-)) 
                        (list (Value a) b)))]
                [else
                 (record-case b
                   [(constant i)
                    (if (fixnum? i)
                        (tbind ([a (Value a)])
                          (make-shortcut
                            (make-seq
                              (make-conditional
                                (tag-test a fixnum-mask fixnum-tag)
                                (make-primcall 'nop '())
                                (make-primcall 'interrupt '()))
                              (prm 'int-/overflow a (Value b)))
                            (make-funcall (Value (make-primref '-))
                               (list a (Value b)))))
                        (make-funcall (Value (make-primref '-))
                           (list a (Value b))))]
                   [else
                    (tbind ([a (Value a)]
                            [b (Value b)])
                      (make-shortcut
                        (make-seq
                          (make-conditional
                            (tag-test (prm 'logor a b) fixnum-mask fixnum-tag)
                            (make-primcall 'nop '())
                            (make-primcall 'interrupt '()))
                          (prm 'int-/overflow a b))
                        (make-funcall (Value (make-primref '-))
                           (list a b))))])]))
            (cond
              [(or (null? arg*)
                   (ormap 
                     (lambda (x)
                       (record-case x 
                         [(constant i) (not (number? i))]
                         [else #f])) arg*))
               (make-funcall (Value (make-primref '-)) (map Value arg*))]
              [(= (length arg*) 1)
               (handle-binary (K 0) (car arg*))]
              [(= (length arg*) 2)
               (handle-binary (car arg*) (cadr arg*))]
              [else
               (remove-complex* arg*
                 (lambda (arg*)
                   (Value 
                     (let f ([a (car arg*)] [d (cdr arg*)])
                        (cond
                          [(null? d) a]
                          [else (f (prm '- a (car d)) (cdr d))])))))]))]
         [($char->fixnum)
          (tbind ([x (Value (car arg*))])
            (prm 'sra x
              (K (- char-shift fixnum-shift))))]
         [($fixnum->char)
          (tbind ([x (Value (car arg*))])
            (prm 'logor
                 (prm 'sll x (K (- char-shift fixnum-shift)))
                 (K char-tag)))]
         [($current-frame)  ;; PCB NEXT-CONTINUATION
          (prm 'mref pcr (K 20))]
         [($arg-list)  ;; PCB ARGS-LIST
          (prm 'mref pcr (K 32))]
         [($seal-frame-and-call) 
          (tbind ([proc (Value (car arg*))])
            (tbind ([k (prm 'alloc 
                            (K continuation-size)
                            (K vector-tag))])
              (tbind ([base (prm 'int+ ;;; PCB BASE
                                 (prm 'mref pcr (K 12))
                                 (K (- wordsize)))])
                (tbind ([underflow-handler 
                         (prm 'mref base (K 0))])
                  (seq* 
                    (prm 'mset k 
                         (K (- vector-tag))
                         (K continuation-tag))
                    (prm 'mset k 
                         (K (- disp-continuation-top vector-tag))
                         fpr)
                    (prm 'mset k
                         (K (- disp-continuation-next vector-tag))
                         (prm 'mref pcr (K 20))) ;;; PCB NEXT CONT
                    (prm 'mset k
                         (K (- disp-continuation-size vector-tag))
                         (prm 'int- base fpr))
                    (prm 'mset pcr (K 20) k)
                    (prm 'mset pcr (K 12) fpr)
                    (make-primcall '$call-with-underflow-handler
                       (list underflow-handler proc k)))))))]
         [($frame->continuation)
          (tbind ([arg (Value (car arg*))])
            (tbind ([t (prm 'alloc
                         (K (align (+ disp-closure-data wordsize)))
                         (K closure-tag))])
              (seq* 
                (prm 'mset t 
                     (K (- disp-closure-code closure-tag))
                     (K (make-code-loc SL_continuation_code)))
                (prm 'mset t
                     (K (- disp-closure-data closure-tag))
                     arg)
                t)))]
         [($make-call-with-values-procedure)
          (K (make-closure (make-code-loc SL_call_with_values) '()))]
         [($make-values-procedure)
          (K (make-closure (make-code-loc SL_values) '()))]
         [($cpref) 
          (let ([a0 (car arg*)] [a1 (cadr arg*)])
            (record-case a1
              [(constant i) 
               (unless (fixnum? i) (err x))
               (tbind ([a0 (Value a0)])
                 (prm 'mref a0
                    (K (+ (- disp-closure-data closure-tag) 
                          (* i wordsize)))))]
              [else (err x)]))]
         [($vector-ref $record-ref) 
          (let ([a0 (car arg*)] [a1 (cadr arg*)])
            (record-case a1
              [(constant i) 
               (unless (fixnum? i) (err x))
               (tbind ([a0 (Value a0)])
                 (prm 'mref a0
                    (K (+ (- disp-vector-data vector-tag)
                          (* i wordsize)))))]
              [else 
               (tbind ([a0 (Value a0)] [a1 (Value a1)])
                 (prm 'mref (prm 'int+ a0 a1)
                    (K (- disp-vector-data vector-tag))))]))]
         [($closure-code)
          (tbind ([x (Value (car arg*))])
            (prm 'int+ 
                 (prm 'mref x
                      (K (- disp-closure-code closure-tag)))
                 (K (- vector-tag disp-code-data))))]
         [($code-freevars)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x
               (K (- disp-code-freevars vector-tag))))]
         [(top-level-value)
          (let ([sym 
                 (record-case (car arg*)
                   [(constant c)
                    (if (symbol? c) c #f)]
                   [else #f])])
            (cond
              [sym
               (tbind ([v (Value (prm '$symbol-value (car arg*)))])
                  (make-shortcut
                    (make-seq
                      (make-conditional
                        (Pred (prm '$unbound-object? v))
                        (prm 'interrupt)
                        (prm 'nop))
                      v)
                    (Value 
                      (make-funcall 
                        (make-primref 'top-level-value-error)
                        (list (car arg*))))))]
              [sym
               (Value
                 (tbind ([v (prm '$symbol-value (car arg*))])
                   (make-conditional
                     (make-primcall '$unbound-object? (list v))
                     (make-funcall 
                       (make-primref 'top-level-value-error)
                       (list (car arg*)))
                     v)))]
              [else
               (Value
                 (tbind ([sym (car arg*)])
                   (make-conditional
                     (make-primcall 'symbol? (list sym))
                     (tbind ([v (make-primcall 
                                  '$symbol-value (list sym))])
                        (make-conditional
                          (make-primcall '$unbound-object? (list v))
                          (make-funcall 
                            (make-primref 'top-level-value-error)
                            (list sym))
                          v))
                     (make-funcall 
                       (make-primref 'top-level-value-error)
                       (list sym)))))]))]
         [($make-port/input $make-port/output $make-port/both)
          (unless (= (length arg*) 7) (err x))
          (let ([tag
                 (case op
                   [($make-port/input)  input-port-tag]
                   [($make-port/output) output-port-tag]
                   [($make-port/both)   input/output-port-tag]
                   [else (err x)])]
                [t* (map (lambda (x) (unique-var 'tmp)) arg*)])
            (make-bind t* (map Value arg*)
              (apply
                 (lambda (handler buf/i idx/i sz/i buf/o idx/o sz/o)
                   (tbind ([p (prm 'alloc 
                                (K (align port-size))
                                (K vector-tag))])
                     (seq*
                       (prm 'mset p 
                            (K (- vector-tag))
                            (K tag))
                       (prm 'mset p
                            (K (- disp-port-handler vector-tag)) 
                            handler)
                       (prm 'mset p
                            (K (- disp-port-input-buffer vector-tag)) 
                            buf/i)
                       (prm 'mset p
                            (K (- disp-port-input-index vector-tag)) 
                            idx/i)
                       (prm 'mset p
                            (K (- disp-port-input-size vector-tag)) 
                            sz/i)
                       (prm 'mset p
                            (K (- disp-port-output-buffer vector-tag)) 
                            buf/o)
                       (prm 'mset p
                            (K (- disp-port-output-index vector-tag)) 
                            idx/o)
                       (prm 'mset p
                            (K (- disp-port-output-size vector-tag)) 
                            sz/o)
                       p)))
                 t*)))]
         [($port-handler
           $port-input-buffer $port-output-buffer
           $port-input-index  $port-output-index
           $port-input-size   $port-output-size)
          (let ([off (case op
                       [($port-handler)       disp-port-handler]
                       [($port-input-buffer)  disp-port-input-buffer]
                       [($port-input-index)   disp-port-input-index]
                       [($port-input-size)    disp-port-input-size]
                       [($port-output-buffer) disp-port-output-buffer]
                       [($port-output-index)  disp-port-output-index]
                       [($port-output-size)   disp-port-output-size]
                       [else (err x)])])
            (tbind ([p (Value (car arg*))])
               (prm 'mref p (K (- off vector-tag)))))]
         [($code-reloc-vector)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x (K (- disp-code-relocsize vector-tag))))]
         [($code-size)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x (K (- disp-code-instrsize vector-tag))))]
         [($code->closure)
          (tbind ([x (Value (car arg*))])
            (tbind ([v (prm 'alloc
                            (K (align (+ 0 disp-closure-data)))
                            (K closure-tag))])
              (seq*
                (prm 'mset v 
                     (K (- disp-closure-code closure-tag))
                     (prm 'int+ x 
                       (K (- disp-code-data vector-tag))))
                v)))]
         [($code-ref)
          (tbind ([x (Value (car arg*))]
                  [i (Value (cadr arg*))])
            (prm 'sll
              (prm 'logand
                   (prm 'mref x
                        (prm 'int+
                             (prm 'sra i (K fixnum-shift))
                             (K (- disp-code-data vector-tag))))
                   (K 255))
              (K fixnum-shift)))]
         [($make-tcbucket)
          (tbind ([tconc (Value (car arg*))]
                  [key (Value (cadr arg*))]
                  [val (Value (caddr arg*))]
                  [next (Value (cadddr arg*))])
            (tbind ([x (prm 'alloc
                            (K (align tcbucket-size))
                            (K vector-tag))])
              (seq*
                (prm 'mset x 
                     (K (- disp-tcbucket-tconc vector-tag))
                     tconc)
                (prm 'mset x 
                     (K (- disp-tcbucket-key vector-tag))
                     key)
                (prm 'mset x 
                     (K (- disp-tcbucket-val vector-tag))
                     val)
                (prm 'mset x 
                     (K (- disp-tcbucket-next vector-tag))
                     next)
                x)))]
         [($tcbucket-key)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x (K (- disp-tcbucket-key vector-tag))))]
         [($tcbucket-val)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x (K (- disp-tcbucket-val vector-tag))))]
         [($tcbucket-next)
          (tbind ([x (Value (car arg*))])
            (prm 'mref x (K (- disp-tcbucket-next vector-tag))))]
         [($procedure-check)
          (tbind ([x (Value (car arg*))])
            (make-shortcut
              (make-seq
                (make-conditional 
                  (tag-test x closure-mask closure-tag)
                  (prm 'nop)
                  (prm 'interrupt))
                x)
              (Value 
                (make-funcall (make-primref 'error)
                  (list (make-constant 'apply)
                        (make-constant "~s is not a procedure")
                        x)))))]
         [else
          (if (primop? op)
              (cogen-primop op 'V arg*)
              (error who "invalid value prim ~s" op))])]
      [(forcall op arg*)
       (make-forcall op (map Value arg*))]
      [(funcall rator arg*)
       (make-funcall (Function rator) (map Value arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (Value rator) (map Value arg*))]
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
  (define (error-codes)
    (define (code-list symbol)
      (define L1 (gensym))
      (define L2 (gensym))
      `(0
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) ,cp-register]
        [andl ,closure-mask ,cp-register]
        [cmpl ,closure-tag ,cp-register]
        [jne (label ,L1)]
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) ,cp-register]
        [movl ,cp-register (disp ,(- disp-symbol-function symbol-tag) (obj ,symbol))]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]
        [label ,L1]
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) %eax]
        [cmpl ,unbound %eax]
        [je (label ,L2)]
        [movl (obj apply) (disp -4 %esp)]
        [movl (obj "~s is not a procedure") (disp -8 %esp)]
        [movl %eax (disp -12 %esp)]
        [movl (obj error) ,cp-register]
        [movl (disp ,(- disp-symbol-system-value symbol-tag)
                    ,cp-register) ,cp-register]
        [movl ,(argc-convention 3) %eax]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]
        [label ,L2]
        [movl (obj ,symbol) (disp -4 %esp)]
        [movl (obj top-level-value) ,cp-register]
        [movl (disp ,(- disp-symbol-system-value symbol-tag)
                    ,cp-register) ,cp-register]
        [movl ,(argc-convention 1) %eax]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]))
    (let ([ls encountered-symbol-calls])
      (let ([c* (map code-list ls)])
        (let ([c* (list*->code* (lambda (x) #f) c*)])
          (let ([p* (map (lambda (x) ($code->closure x)) c*)])
            (let f ([ls ls] [p* p*])
              (cond
                [(null? ls) (prm 'nop)]
                [else 
                 (make-seq
                   (tbind ([p (Value (K (car p*)))] [s (Value (K (car ls)))])
                       (Effect (prm '$init-symbol-function! s p)))
                   (f (cdr ls) (cdr p*)))])))))))
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (let ([code* (map (lambda (x) (Clambda x Value)) code*)]
             [body (Value body)])
         (make-codes code*
           (make-seq (error-codes) body)))]
      [else (error who "invalid program ~s" x)]))
  ;;;
  ;(print-code x)
  (Program x))





(define parameter-registers '(%edi)) 
(define return-value-register '%eax)
(define cp-register '%edi)
(define all-registers '(%eax %edi %ebx %edx))
(define argc-register '%eax)

(define non-8bit-registers '(%edi))

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
              (forcall? x) (shortcut? x)
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
  (define (make-set lhs rhs)
    (make-asm-instr 'move lhs rhs))
  (define (do-bind-frmt* nf* v* ac)
    (cond
      [(null? nf*) ac]
      [else
       (make-seq
         (V (car nf*) (car v*))
         (do-bind-frmt* (cdr nf*) (cdr v*) ac))]))
  ;;;
  (define (handle-nontail-call rator rands value-dest call-targ)
    (let-values ([(reg-locs reg-args frm-args)
                  (nontail-locations (cons rator rands))])
      (let ([regt* (map (lambda (x) (unique-var 'rt)) reg-args)]
            [frmt* (map (lambda (x) (make-nfv 'unset-conflicts #f #f #f #f))
                        frm-args)])
        (let* ([call 
                (make-ntcall call-targ value-dest 
                  (list* argc-register 
                         pcr esp apr
                         (append reg-locs frmt*))
                  #f #f)]
               [body
                (make-nframe frmt* #f
                  (do-bind-frmt* frmt* frm-args
                    (do-bind (cdr regt*) (cdr reg-args)
                      ;;; evaluate cpt last
                      (do-bind (list (car regt*)) (list (car reg-args))
                        (assign* reg-locs regt*
                          (make-seq 
                            (make-set argc-register 
                               (make-constant
                                 (argc-convention (length rands))))
                            call))))))])
          (if value-dest
              (make-seq body (make-set value-dest return-value-register))
              body)))))
  (define (alloc-check size)
    (E (make-conditional ;;; PCB ALLOC-REDLINE
         (make-primcall '<= 
           (list (make-primcall 'int+ (list apr size)) 
                 (make-primcall 'mref (list pcr (make-constant 4)))))
         (make-primcall 'nop '())
         (make-funcall 
           (make-primcall 'mref
              (list (make-constant (make-object 'do-overflow))
                    (make-constant (- disp-symbol-system-value
                                      symbol-tag))))
           (list size)))))
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
               (make-seq
                 (alloc-check size)
                 (S (cadr rands)
                    (lambda (tag)
                      (make-seq
                        (make-seq 
                          (make-set d apr)
                          (make-asm-instr 'logor d tag))
                        (make-asm-instr 'int+ apr size)))))))]
         [(mref) 
          (S* rands
              (lambda (rands)
                (make-set d (make-disp (car rands) (cadr rands)))))]
         [(logand logxor logor int+ int- int*
                  int-/overflow int+/overflow int*/overflow)
          (make-seq
            (V d (car rands))
            (S (cadr rands)
               (lambda (s)
                 (make-asm-instr op d s))))]
         [(remainder)
          (S* rands
              (lambda (rands)
                (seq* 
                  (make-set eax (car rands))
                  (make-asm-instr 'cltd edx eax)
                  (make-asm-instr 'idiv eax (cadr rands))
                  (make-set d eax))))]
         [(quotient) 
          (S* rands
              (lambda (rands)
                (seq* 
                  (make-set eax (car rands))
                  (make-asm-instr 'cltd edx eax)
                  (make-asm-instr 'idiv edx (cadr rands))
                  (make-set d edx))))]
         [(sll sra srl)
          (let ([a (car rands)] [b (cadr rands)])
            (cond
              [(constant? b)
               (make-seq
                 (V d a)
                 (make-asm-instr op d b))]
              [else 
               (S b
                  (lambda (b)
                    (seq*
                      (V d a)
                      (make-set ecx b)
                      (make-asm-instr op d ecx))))]))]
         [else (error who "invalid value op ~s" op)])]
      [(funcall rator rands) 
       (handle-nontail-call rator rands d #f)]
      [(jmpcall label rator rands) 
       (handle-nontail-call rator rands d label)]
      [(forcall op rands)
       (handle-nontail-call 
         (make-constant (make-foreign-label op))
         rands d op)] 
      [(shortcut body handler)
       (make-shortcut 
          (V d body)
          (V d handler))] 
      [else 
       (if (symbol? x) 
           (make-set d x)
           (error who "invalid value ~s" (unparse x)))]))
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
         [(mset bset/c bset/h)
          (S* rands
              (lambda (s*)
                (make-asm-instr op
                  (make-disp (car s*) (cadr s*)) 
                  (caddr s*))))]
         [(nop interrupt) x]
         [else (error 'impose-effect "invalid instr ~s" x)])]
      [(funcall rator rands)
       (handle-nontail-call rator rands #f #f)]
      [(jmpcall label rator rands) 
       (handle-nontail-call rator rands #f label)]
      [(forcall op rands)
       (handle-nontail-call 
         (make-constant (make-foreign-label op))
         rands #f op)]
      [(shortcut body handler)
       (make-shortcut (E body) (E handler))]
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
       (let ([a (car rands)] [b (cadr rands)])
         (cond
           [(and (constant? a) (constant? b))
            (let ([t (unique-var 'tmp)])
              (P (make-bind (list t) (list a)
                    (make-primcall op (list t b)))))]
           [else
            (S* rands
                (lambda (rands)
                  (let ([a (car rands)] [b (cadr rands)])
                    (make-asm-instr op a b))))]))]
        [(shortcut body handler)
         (make-shortcut (P body) (P handler))]
      [else (error who "invalid pred ~s" x)]))
  ;;;
  (define (Tail env)
    (define (handle-tail-call target rator rands)
      (let* ([args (cons rator rands)]
             [locs (formals-locations args)]
             [rest 
              (make-seq
                (make-set argc-register 
                  (make-constant
                    (argc-convention (length rands))))
                (cond
                  [target 
                   (make-primcall 'direct-jump 
                     (cons target 
                      (list* argc-register
                             pcr esp apr
                             locs)))]
                  [else 
                   (make-primcall 'indirect-jump 
                     (list* argc-register 
                            pcr esp apr
                            locs))]))])
         (let f ([args (reverse args)] 
                 [locs (reverse locs)] 
                 [targs '()]
                 [tlocs '()])
           (cond
             [(null? args) (assign* tlocs targs rest)]
             [(constant? (car args))
              (f (cdr args) (cdr locs)
                 (cons (car args) targs) 
                 (cons (car locs) tlocs))]
             [(and (fvar? (car locs)) 
                   (cond
                     [(and (var? (car args)) (assq (car args) env))
                      => (lambda (p) (eq? (cdr p) (car locs)))]
                     [else #f]))
              (f (cdr args) (cdr locs) targs tlocs)]
             [else
              (let ([t (unique-var 'tmp)])
                (set! locals (cons t locals))
                (make-seq
                  (V t (car args))
                  (f (cdr args) (cdr locs) 
                     (cons t targs) (cons (car locs) tlocs))))]))))
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
                (make-set (mkfvar 1) handler)
                (make-set (mkfvar 2) k)
                (make-set cpr proc)
                (make-set argc-register (make-constant (argc-convention 1)))
                (make-asm-instr 'int- fpr (make-constant wordsize))
                (make-primcall 'indirect-jump
                  (list argc-register cpr pcr esp apr
                        (mkfvar 1) (mkfvar 2)))))]
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
        [(forcall) (VT x)]
        [(shortcut body handler)
         (make-shortcut  (Tail body) (Tail handler))]
        [else (error who "invalid tail ~s" x)]))
    Tail)
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
                 [env (map cons args locs)]
                 [body (let f ([args args] [locs locs])
                          (cond
                            [(null? args) ((Tail env) body)]
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
    (let ([x ((Tail '()) x)])
      (make-locals locals x)))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes (map Clambda code*) (Main body))]))
  ;;;
;  (print-code x)
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
  (define empty-set '())
  (define (set-member? x s) (memq x s))

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



(module (assign-frame-sizes)
  ;;; assign-frame-sizes module
  (define (has-nontail-call? x)
    (define who 'has-nontail-call?)
    (define (E x)
      (record-case x
        [(seq e0 e1) (or (E e0) (E e1))]
        [(conditional e0 e1 e2)
         (or (P e0) (E e1) (E e2))]
        [(nframe) #t]
        [(asm-instr) #f]
        [(primcall op args)
         (case op
           [(interrupt nop) #f]
           [else (error who "invalid effect ~s" (unparse x))])]
        [(shortcut body handler) 
         (or (E body) (E handler))]
        [else (error who "invalid effect ~s" x)]))
    (define (P x)
      (record-case x
        [(seq e0 e1) (or (E e0) (P e1))]
        [(conditional e0 e1 e2)
         (or (P e0) (P e1) (P e2))]
        [(asm-instr) #f]
        [(constant) #f]
        [(shortcut body handler) 
         (or (P body) (P handler))]
        [else (error who "invalid pred ~s" x)]))
    (define (T x)
      (record-case x
        [(seq e0 e1) 
         (or (E e0) (T e1))]
        [(conditional e0 e1 e2)
         (or (P e0) (T e1) (T e2))]
        [(primcall) #f]
        [(shortcut body handler) 
         (or (T body) (T handler))]
        [else (error who "invalid tail ~s" x)]))
    (T x))
  ;;;
  (begin
    (define (init-var! x)
      (set-var-var-move! x (empty-var-set))
      (set-var-reg-move! x (empty-reg-set))
      (set-var-frm-move! x (empty-frm-set))
      (set-var-var-conf! x (empty-var-set))
      (set-var-reg-conf! x (empty-reg-set))
      (set-var-frm-conf! x (empty-frm-set)))
    (define (init-nfv! x)
      (set-nfv-frm-conf! x (empty-frm-set))
      (set-nfv-nfv-conf! x (empty-nfv-set))
      (set-nfv-var-conf! x (empty-var-set)))
    (define (reg? x) (symbol? x))
    (define (empty-frm-set) empty-set)
    (define (empty-nfv-set) empty-set)
    (define (empty-var-set) empty-set)
    (define (add-var x s) (set-add x s))
    (define (mem-var? x s) (set-member? x s))
    (define (rem-var x s) (set-rem x s))
    (define (union-vars s1 s2) (union s1 s2))
    (define (empty-reg-set) empty-set)
    (define (add-reg x s) (set-add x s))
    (define (rem-reg x s) (set-rem x s))
    (define (mem-reg? x s) (set-member? x s))
    (define (union-regs s1 s2) (union s1 s2))
    (define (add-frm x s) (set-add x s))
    (define (mem-frm? x s) (set-member? x s))
    (define (rem-frm x s) (set-rem x s))
    (define (union-frms s1 s2) (union s1 s2))
    (define (for-each-var s f) (for-each f s))
    (define (add-nfv x s) (set-add x s))
    (define (rem-nfv x s) (set-rem x s))
    (define (mem-nfv? x s) (set-member? x s))
    (define (union-nfvs s1 s2) (union s1 s2))
    (define (for-each-nfv s f) (for-each f s)))
  ;;;
  (define (uncover-frame-conflicts x)
    (define who 'uncover-frame-conflicts)
    (define spill-set '())
    (define-syntax assert
      (syntax-rules ()
        [(_ p0 p1 v0 v1)
         (unless (and (p0 v0)
                      (andmap p1 v1))
           (error 'assert "failed in ~s" '(assert p0 p1 v0 v1)))]))
    (define (mark-reg/vars-conf! r vs)
      (assert reg? var? r vs)
      (for-each-var vs
        (lambda (v)
          (set-var-reg-conf! v 
            (add-reg r (var-reg-conf v))))))
    (define (mark-frm/vars-conf! f vs) 
      (assert fvar? var? f vs)
      (for-each-var vs
        (lambda (v)
          (set-var-frm-conf! v 
            (add-frm f (var-frm-conf v))))))
    (define (mark-frm/nfvs-conf! f ns) 
      (assert fvar? nfv? f ns)
      (for-each-nfv ns
        (lambda (n)
          (set-nfv-frm-conf! n
            (add-frm f (nfv-frm-conf n))))))
    (define (mark-var/vars-conf! v vs) 
      (assert var? var? v vs)
      (for-each-var vs
        (lambda (w)
          (set-var-var-conf! w
            (add-var v (var-var-conf w)))))
      (set-var-var-conf! v
        (union-vars vs (var-var-conf v))))
    (define (mark-var/frms-conf! v fs) 
      (assert var? fvar? v fs)
      (set-var-frm-conf! v
        (union-frms fs (var-frm-conf v))))
    (define (mark-var/regs-conf! v rs) 
      (assert var? reg? v rs)
      (set-var-reg-conf! v
        (union-regs rs (var-reg-conf v))))
    (define (mark-var/nfvs-conf! v ns) 
      (assert var? nfv? v ns)
      (for-each-nfv ns
        (lambda (n)
          (set-nfv-var-conf! n
            (add-var v (nfv-var-conf n))))))
    (define (mark-nfv/vars-conf! n vs)
      (assert nfv? var? n vs)
      (set-nfv-var-conf! n
        (union-vars vs (nfv-var-conf n))))
    (define (mark-nfv/frms-conf! n fs)
      (assert nfv? fvar? n fs)
      (set-nfv-frm-conf! n
        (union-frms fs (nfv-frm-conf n))))
    (define (mark-nfv/nfvs-conf! n ns)
      (assert nfv? nfv? n ns)
      (set-nfv-nfv-conf! n
        (union-nfvs ns (nfv-nfv-conf n)))
      (for-each-nfv ns
        (lambda (m)
          (set-nfv-nfv-conf! m
            (add-nfv n (nfv-nfv-conf m))))))
    (define (mark-var/var-move! x y)
      (set-var-var-move! x 
        (add-var y (var-var-move x)))
      (set-var-var-move! y 
        (add-var x (var-var-move y))))
    (define (mark-var/frm-move! x y)
      (set-var-frm-move! x 
        (add-frm y (var-frm-move x))))
    (define (mark-var/reg-move! x y)
      (set-var-reg-move! x 
        (add-reg y (var-reg-move x))))
    (define (const? x)
      (or (constant? x)
          (code-loc? x)))
    (define (R x vs rs fs ns)
      (cond
        [(const? x) (values vs rs fs ns)]
        [(reg? x) 
         (values vs (add-reg x rs) fs ns)]
        [(fvar? x)
         (values vs rs (add-frm x fs) ns)]
        [(var? x)
         (values (add-var x vs) rs fs ns)]
        [(nfv? x)
         (values vs rs fs (add-nfv x ns))]
        [(disp? x)
         (let-values ([(vs rs fs ns) (R (disp-s0 x) vs rs fs ns)])
            (R (disp-s1 x) vs rs fs ns))]
        [else (error who "invalid R ~s" x)]))
    (define (R* ls vs rs fs ns)
      (cond
        [(null? ls) (values vs rs fs ns)]
        [else 
         (let-values ([(vs rs fs ns) (R (car ls) vs rs fs ns)])
            (R* (cdr ls) vs rs fs ns))]))
    (define (E x vs rs fs ns)
      (record-case x
        [(seq e0 e1) 
         (let-values ([(vs rs fs ns) (E e1 vs rs fs ns)])
           (E e0 vs rs fs ns))]
        [(conditional e0 e1 e2)
         (let-values ([(vs1 rs1 fs1 ns1) (E e1 vs rs fs ns)]
                      [(vs2 rs2 fs2 ns2) (E e2 vs rs fs ns)])
           (P e0
              vs1 rs1 fs1 ns1
              vs2 rs2 fs2 ns2
              (union-vars vs1 vs2)
              (union-regs rs1 rs2)
              (union-frms fs1 fs2)
              (union-nfvs ns1 ns2)))]
        [(asm-instr op d s)
         (case op
           [(move)
            (cond
              [(reg? d)
               (cond
                 [(not (mem-reg? d rs)) 
                  (set-asm-instr-op! x 'nop)
                  (values vs rs fs ns)]
                 [(or (const? s) (disp? s) (reg? s))
                  (let ([rs (rem-reg d rs)])
                    (mark-reg/vars-conf! d vs)
                    (R s vs rs fs ns))]
                 [(var? s)
                  (let ([rs (rem-reg d rs)]
                        [vs (rem-var s vs)])
                    (mark-var/reg-move! s d)
                    (mark-reg/vars-conf! d vs)
                    (values (add-var s vs) rs fs ns))]
                 [else (error who "invalid rs ~s" (unparse x))])]
              [(fvar? d) 
               (cond
                 [(not (mem-frm? d fs)) 
                  (set-asm-instr-op! x 'nop)
                  (values vs rs fs ns)]
                 [(or (const? s) (disp? s) (reg? s))
                  (let ([fs (rem-frm d fs)])
                    (mark-frm/vars-conf! d vs)
                    (mark-frm/nfvs-conf! d ns)
                    (R s vs rs fs ns))]
                 [(var? s) 
                  (let ([fs (rem-frm d fs)]
                        [vs (rem-var s vs)])
                    (mark-var/frm-move! s d)
                    (mark-frm/vars-conf! d vs)
                    (mark-frm/nfvs-conf! d ns)
                    (values (add-var s vs) rs fs ns))]
                 [else (error who "invalid fs ~s" s)])]
              [(var? d)
               (cond
                 [(not (mem-var? d vs)) 
                  (set-asm-instr-op! x 'nop)
                  (values vs rs fs ns)]
                 [(or (disp? s) (constant? s))
                  (let ([vs (rem-var d vs)])
                    (mark-var/vars-conf! d vs)
                    (mark-var/frms-conf! d fs)
                    (mark-var/regs-conf! d rs)
                    (mark-var/nfvs-conf! d ns)
                    (R s vs rs fs ns))]
                 [(reg? s) 
                  (let ([vs (rem-var d vs)]
                        [rs (rem-reg s rs)])
                    (mark-var/reg-move! d s)
                    (mark-var/vars-conf! d vs)
                    (mark-var/frms-conf! d fs)
                    (mark-var/regs-conf! d rs)
                    (mark-var/nfvs-conf! d ns)
                    (values vs (add-reg s rs) fs ns))]
                 [(var? s) 
                  (let ([vs (rem-var d (rem-var s vs))])
                    (mark-var/var-move! d s)
                    (mark-var/vars-conf! d vs)
                    (mark-var/frms-conf! d fs)
                    (mark-var/regs-conf! d rs)
                    (mark-var/nfvs-conf! d ns)
                    (values (add-var s vs) rs fs ns))]
                 [(fvar? s) 
                  (let ([vs (rem-var d vs)]
                        [fs (rem-frm s fs)])
                    (mark-var/frm-move! d s)
                    (mark-var/vars-conf! d vs)
                    (mark-var/frms-conf! d fs)
                    (mark-var/regs-conf! d rs)
                    (mark-var/nfvs-conf! d ns)
                    (values vs rs (add-var s fs) ns))]
                 [else (error who "invalid vs ~s" s)])]
              [(nfv? d)
               (cond
                 [(not (mem-nfv? d ns)) (error who "dead nfv")]
                 [(or (disp? s) (constant? s) (reg? s))
                  (let ([ns (rem-nfv d ns)])
                    (mark-nfv/vars-conf! d vs)
                    (mark-nfv/frms-conf! d fs)
                    (R s vs rs fs ns))]
                 [(var? s)
                  (let ([ns (rem-nfv d ns)]
                        [vs (rem-var s vs)])
                    (mark-nfv/vars-conf! d vs)
                    (mark-nfv/frms-conf! d fs)
                    (values (add-var s vs) rs fs ns))]
                 [else (error who "invalid ns ~s" s)])]
              [else (error who "invalid d ~s" d)])]
           [(int-/overflow int+/overflow int*/overflow)
            (let ([v (exception-live-set)])
              (unless (vector? v)
                (error who "unbound exception"))
              (let ([vs (union-vars vs (vector-ref v 0))]
                    [rs (union-regs rs (vector-ref v 1))]
                    [fs (union-frms fs (vector-ref v 2))]
                    [ns (union-nfvs ns (vector-ref v 3))])
                (cond
                  [(var? d) 
                   (cond
                     [(not (mem-var? d vs)) 
                      (set-asm-instr-op! x 'nop)
                      (values vs rs fs ns)]
                     [else
                      (let ([vs (rem-var d vs)])
                        (mark-var/vars-conf! d vs)
                        (mark-var/frms-conf! d fs)
                        (mark-var/nfvs-conf! d ns)
                        (mark-var/regs-conf! d rs)
                        (R s (set-add d vs) rs fs ns))])]
                  [(reg? d)
                   (cond
                     [(not (mem-reg? d rs))
                      (values vs rs fs ns)]
                     [else
                      (let ([rs (rem-reg d rs)])
                        (mark-reg/vars-conf! d vs)
                        (R s vs (set-add d rs) fs ns))])]
                  [(nfv? d) 
                   (cond
                     [(not (mem-nfv? d ns)) (error who "dead nfv")]
                     [else
                      (let ([ns (rem-nfv d ns)])
                        (mark-nfv/vars-conf! d vs)
                        (mark-nfv/frms-conf! d fs)
                        (R s vs rs fs (add-nfv d ns)))])]
                  [else (error who "invalid op d ~s" (unparse x))])))] 
           [(logand logor logxor sll sra srl int+ int- int*) 
            (cond
              [(var? d) 
               (cond
                 [(not (mem-var? d vs)) 
                  (set-asm-instr-op! x 'nop)
                  (values vs rs fs ns)]
                 [else
                  (let ([vs (rem-var d vs)])
                    (mark-var/vars-conf! d vs)
                    (mark-var/frms-conf! d fs)
                    (mark-var/nfvs-conf! d ns)
                    (mark-var/regs-conf! d rs)
                    (R s (set-add d vs) rs fs ns))])]
              [(reg? d) 
               (cond
                 [(not (mem-reg? d rs))
                  (values vs rs fs ns)]
                 [else
                  (let ([rs (rem-reg d rs)])
                    (mark-reg/vars-conf! d vs)
                    (R s vs (set-add d rs) fs ns))])]
              [(nfv? d) 
               (cond
                 [(not (mem-nfv? d ns)) (error who "dead nfv")]
                 [else
                  (let ([ns (rem-nfv d ns)])
                    (mark-nfv/vars-conf! d vs)
                    (mark-nfv/frms-conf! d fs)
                    (R s vs rs fs (add-nfv d ns)))])]
              [else (error who "invalid op d ~s" (unparse x))])]
           [(idiv) 
            (mark-reg/vars-conf! eax vs)
            (mark-reg/vars-conf! edx vs)
            (R s vs (add-reg eax (add-reg edx rs)) fs ns)]
           [(cltd) 
            (mark-reg/vars-conf! edx vs)
            (R s vs (rem-reg edx rs) fs ns)]
           [(mset bset/c bset/h) 
            (R* (list s d) vs rs fs ns)]
           [else (error who "invalid effect op ~s" (unparse x))])]
        [(ntcall target value args mask size)
         (set! spill-set (union-vars vs spill-set))
         (R* args vs (empty-reg-set) fs ns)]
        [(nframe nfvs live body)
         (for-each init-nfv! nfvs)
         (set-nframe-live! x (vector vs fs ns))
         (E body vs rs fs ns)]
        [(primcall op args)
         (case op
           [(nop) (values vs rs fs ns)]
           [(interrupt) 
            (let ([v (exception-live-set)])
              (unless (vector? v)
                (error who "unbound exception"))
              (values (vector-ref v 0)
                      (vector-ref v 1)
                      (vector-ref v 2)
                      (vector-ref v 3)))]
           [else (error who "invalid effect op ~s" op)])]
        [(shortcut body handler)
         (let-values ([(vsh rsh fsh nsh) (E handler vs rs fs ns)])
            (parameterize ([exception-live-set
                            (vector vsh rsh fsh nsh)])
              (E body vs rs fs ns)))]
        [else (error who "invalid effect ~s" (unparse x))]))
    (define (P x vst rst fst nst 
                 vsf rsf fsf nsf
                 vsu rsu fsu nsu)
      (record-case x
        [(seq e0 e1) 
         (let-values ([(vs rs fs ns) 
                       (P e1 vst rst fst nst 
                          vsf rsf fsf nsf
                          vsu rsu fsu nsu)])
           (E e0 vs rs fs ns))]
        [(conditional e0 e1 e2)
         (let-values ([(vs1 rs1 fs1 ns1)
                       (P e1 vst rst fst nst 
                          vsf rsf fsf nsf
                          vsu rsu fsu nsu)]
                      [(vs2 rs2 fs2 ns2) 
                       (P e2 vst rst fst nst 
                          vsf rsf fsf nsf
                          vsu rsu fsu nsu)])
           (P e0
              vs1 rs1 fs1 ns1
              vs2 rs2 fs2 ns2
              (union-vars vs1 vs2)
              (union-regs rs1 rs2)
              (union-frms fs1 fs2)
              (union-nfvs ns1 ns2)))] 
        [(constant t)
         (if t
             (values vst rst fst nst)
             (values vsf rsf fsf nsf))]
        [(asm-instr op d s)
         (R* (list d s) vsu rsu fsu nsu)]
        [(shortcut body handler)
         (let-values ([(vsh rsh fsh nsh)
                       (P handler vst rst fst nst 
                          vsf rsf fsf nsf
                          vsu rsu fsu nsu)])
            (parameterize ([exception-live-set
                            (vector vsh rsh fsh nsh)])
              (P body vst rst fst nst 
                 vsf rsf fsf nsf
                 vsu rsu fsu nsu)))]
        [else (error who "invalid pred ~s" (unparse x))]))
    (define (T x)
      (record-case x
        [(seq e0 e1) 
         (let-values ([(vs rs fs ns) (T e1)])
           (E e0 vs rs fs ns))]
        [(conditional e0 e1 e2)
         (let-values ([(vs1 rs1 fs1 ns1) (T e1)]
                      [(vs2 rs2 fs2 ns2) (T e2)])
           (P e0
              vs1 rs1 fs1 ns1
              vs2 rs2 fs2 ns2
              (union-vars vs1 vs2)
              (union-regs rs1 rs2)
              (union-frms fs1 fs2)
              (union-nfvs ns1 ns2)))]
        [(primcall op arg*) 
         (case op
           [(return indirect-jump direct-jump) 
            (R* arg* (empty-var-set)
                (empty-reg-set) 
                (empty-frm-set)
                (empty-nfv-set))]
           [else (error who "invalid tail op ~s" x)])]
        [(shortcut body handler)
         (let-values ([(vsh rsh fsh nsh) (T handler)])
            (parameterize ([exception-live-set
                            (vector vsh rsh fsh nsh)])
               (T body)))]
        [else (error who "invalid tail ~s" x)]))
    (define exception-live-set 
      (make-parameter #f))
    (T x)
    spill-set)
  (define-syntax frm-loc
    (syntax-rules ()
      [(_ x)
       (let ([t x])
         (if (fvar? t)
             (fvar-idx t)
             (error 'frm-loc "in ~s ~s" (unparse t) '(frm-loc x))))]))
  (define (frame-conflict? i vs fs)
    (define (frm-conf x)
      (unless (fvar? x) (error 'here3 "herea"))
      (fx= i (frm-loc x)))
    (define (var-conf x)
      (let ([loc (var-loc x)])
        (and (fvar? loc)
             (fx= i (frm-loc loc)))))
    (unless (andmap fvar? fs) (error 'frame-conflict? "nonfvars"))
    (or (ormap frm-conf fs)
        (ormap var-conf vs)))
  ;;;
  (define (assign-locations! ls)
    (for-each (lambda (x) (set-var-loc! x #t)) ls))
  (define (rewrite x)
    (define who 'rewrite)
    (define (assign x)
      (define (assign-any)
        (let ([frms (var-frm-conf x)]
              [vars (var-var-conf x)])
          (let f ([i 1])
            (cond
              [(frame-conflict? i vars frms) (f (fxadd1 i))]
              [else 
               (let ([fv (mkfvar i)])
                 (set-var-loc! x fv)
                 (for-each
                   (lambda (var)
                     (set-var-var-conf! var
                       (rem-var x (var-var-conf var)))
                     (set-var-frm-conf! var
                       (add-frm fv (var-frm-conf var))))
                   vars)
                 fv)]))))
      (define (assign-move x)
        (let ([mr (set-difference 
                    (var-frm-move x) 
                    (var-frm-conf x))])
          (cond
            [(null? mr) #f]
            [else 
             (let ([fv (car mr)])
               (set-var-loc! x fv)
               (for-each
                   (lambda (var)
                     (set-var-var-conf! var
                       (rem-var x (var-var-conf var)))
                     (set-var-frm-conf! var
                       (add-frm fv (var-frm-conf var))))
                   (var-var-conf x))
               (for-each
                 (lambda (var)
                   (set-var-var-move! var
                      (rem-var x (var-var-move var)))
                   (set-var-frm-move! var
                      (add-frm fv (var-frm-move var)))
                   (let ([loc (var-loc var)])
                     (when (and loc (not (fvar? loc)))
                       (assign-move var))))
                 (var-var-move x))
               fv)])))
      (or (assign-move x)
          (assign-any)))
    (define (NFE idx mask x)
      (record-case x
        [(seq e0 e1) 
         (let ([e0 (E e0)])
           (make-seq e0 (NFE idx mask e1)))]
        [(ntcall target value args mask^ size)
         (make-ntcall target value 
            (map (lambda (x) 
                   (cond
                     [(symbol? x) x]
                     [(nfv? x) (nfv-loc x)]
                     [else (error who "invalid arg")]))
                 args)
            mask idx)]
        [else (error who "invalid NF effect ~s" x)]))
    (define (Var x)
      (cond
        [(var-loc x) =>
         (lambda (loc)
           (if (fvar? loc)
               loc
               (assign x)))]
        [else x]))
    (define (R x)
      (cond
        [(or (constant? x) (reg? x) (fvar? x)) x]
        [(nfv? x)
         (or (nfv-loc x)
             (error who "unassigned nfv"))]
        [(var? x) (Var x)]
        [(disp? x)
         (make-disp (R (disp-s0 x)) (R (disp-s1 x)))]
        [else (error who "invalid R ~s" (unparse x))]))
    (define (E x)
      (record-case x
        [(seq e0 e1)
         (let ([e0 (E e0)])
           (make-seq e0 (E e1)))]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (E e1) (E e2))]
        [(asm-instr op d s)
         (case op
           [(move) 
            (let ([d (R d)] [s (R s)])
              (cond
                [(eq? d s) 
                 (make-primcall 'nop '())]
                [else
                 (make-asm-instr 'move d s)]))]
           [(logand logor logxor int+ int- int* mset bset/c bset/h 
              sll sra srl
              cltd idiv int-/overflow int+/overflow int*/overflow)
            (make-asm-instr op (R d) (R s))]
           [(nop) (make-primcall 'nop '())]
           [else (error who "invalid op ~s" op)])]
        [(nframe vars live body)
         (let ([live-frms1 (map Var (vector-ref live 0))]
               [live-frms2 (vector-ref live 1)]
               [live-nfvs (vector-ref live 2)])
           (define (max-frm ls i)
             (cond
               [(null? ls) i]
               [else 
                (max-frm (cdr ls) 
                  (max i (fvar-idx (car ls))))]))
           (define (max-nfv ls i)
             (cond
               [(null? ls) i]
               [else
                (let ([loc (nfv-loc (car ls))])
                  (unless (fvar? loc) (error 'max-nfv "not assigned"))
                  (max-nfv (cdr ls) (max i (frm-loc loc))))]))
           (define (actual-frame-size vars i)
             (define (frame-size-ok? i vars)
               (or (null? vars)
                   (and (let ([x (car vars)])
                          (not (frame-conflict? i 
                                 (nfv-var-conf x)
                                 (nfv-frm-conf x))))
                        (frame-size-ok? (fxadd1 i) (cdr vars)))))
              (cond
                [(frame-size-ok? i vars) i]
                [else (actual-frame-size vars (fxadd1 i))]))
           (define (assign-frame-vars! vars i)
             (unless (null? vars)
               (let ([v (car vars)] [fv (mkfvar i)])
                 (set-nfv-loc! v fv)
                 (for-each
                   (lambda (x) 
                     (when (fx= (frm-loc x) i)
                       (error who "invalid assignment")))
                   (nfv-frm-conf v))
                 (for-each
                   (lambda (x)
                     (let ([loc (nfv-loc x)])
                       (cond
                         [loc
                          (when (fx= (frm-loc loc) i)
                            (error who "invalid assignment"))]
                         [else
                          (set-nfv-nfv-conf! x
                            (rem-nfv v (nfv-nfv-conf x)))
                          (set-nfv-frm-conf! x
                            (add-frm fv (nfv-frm-conf x)))])))
                   (nfv-nfv-conf v))
                 (for-each
                   (lambda (x)
                     (let ([loc (var-loc x)])
                       (cond
                         [(fvar? loc)
                          (when (fx= (frm-loc loc) i)
                            (error who "invalid assignment"))]
                         [else
                          (set-var-frm-conf! x
                            (add-frm fv (var-frm-conf x)))])))
                   (nfv-var-conf v)))
               (assign-frame-vars! (cdr vars) (fxadd1 i))))
           (define (make-mask n)
             (let ([v (make-vector (fxsra (fx+ n 7) 3) 0)])
               (define (set-bit idx)
                 (let ([q (fxsra idx 3)]
                       [r (fxlogand idx 7)])
                   (vector-set! v q
                     (fxlogor (vector-ref v q) (fxsll 1 r)))))
               (for-each (lambda (x) (set-bit (fvar-idx x))) live-frms1)
               (for-each (lambda (x) (set-bit (fvar-idx x))) live-frms2)
               (for-each (lambda (x) 
                           (let ([loc (nfv-loc x)])
                             (when loc 
                               (set-bit (fvar-idx loc))))) 
                         live-nfvs) v))
           (let ([i (actual-frame-size vars
                      (fx+ 2 
                        (max-frm live-frms1
                          (max-nfv live-nfvs
                            (max-frm live-frms2 0)))))])
             (assign-frame-vars! vars i)
             (NFE (fxsub1 i) (make-mask (fxsub1 i)) body)))]
        [(primcall op args)
         (case op
           [(nop interrupt) x]
           [else (error who "invalid effect prim ~s" op)])]
        [(shortcut body handler)
         (make-shortcut (E body) (E handler))]
        [else (error who "invalid effect ~s" (unparse x))]))
    (define (P x)
      (record-case x
        [(seq e0 e1)
         (let ([e0 (E e0)])
           (make-seq e0 (P e1)))]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (P e1) (P e2))]
        [(asm-instr op d s) (make-asm-instr op (R d) (R s))]
        [(constant) x]
        [(shortcut body handler)
         (make-shortcut (P body) (P handler))]
        [else (error who "invalid pred ~s" (unparse x))]))
    (define (T x)
      (record-case x
        [(seq e0 e1)
         (let ([e0 (E e0)])
           (make-seq e0 (T e1)))]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (T e1) (T e2))]
        [(primcall op args) x]
        [(shortcut body handler)
         (make-shortcut (T body) (T handler))]
        [else (error who "invalid tail ~s" (unparse x))]))
    (T x))
  ;;;
  (define (Main x)
    (record-case x 
      [(locals vars body)
       (cond
         [(has-nontail-call? body)
          (for-each init-var! vars)
          (let ([call-live* (uncover-frame-conflicts body)])
            (assign-locations! call-live*)
            (let ([body (rewrite body)])
              (make-locals (set-difference vars call-live*) body)))]
         [else x])]
      [else (error 'assign-frame-sizes "invalid main ~s" x)]))
  ;;;
  (define (ClambdaCase x) 
    (record-case x
      [(clambda-case info body)
       (make-clambda-case info (Main body))]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label (map ClambdaCase case*) free*)]))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (make-codes (map Clambda code*) (Main body))]))
  ;;;
  (define (assign-frame-sizes x)
    (Program x)))




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
        [(nfv) (list x)]
        [(fvar) (if (reg? x) (list x) '())]
        [(code-loc) '()]
        [else
         (cond
           [(symbol? x) (if (reg? x) (list x) '())]
           [else (error who "invalid R ~s" x)])]))
    ;;; build effect
    (define (E x s)
      (record-case x
        [(asm-instr op d v)
         (case op
           [(move)
            (let ([s (set-rem d s)])
              (record-case d
                [(nfv c i) 
                 (if (list? c)
                     (set-nfv-conf! d
                        (set-union c s))
                     (set-nfv-conf! d s))
                 (union (R v) s)]
                [else
                 (for-each (lambda (y) (add-edge! g d y)) s)
                 (union (R v) s)]))] 
           [(int-/overflow int+/overflow int*/overflow)
            (unless (exception-live-set)
              (error who "uninitialized live set"))
            (let ([s (set-rem d (set-union s (exception-live-set)))])
              (record-case d
                [(nfv c i)
                 (if (list? c)
                     (set-nfv-conf! d (set-union c s))
                     (set-nfv-conf! d s))
                 (union (union (R v) (R d)) s)]
                [else
                 (for-each (lambda (y) (add-edge! g d y)) s)
                 (union (union (R v) (R d)) s)]))] 
           [(logand logxor int+ int- int* logor sll sra srl)
            (let ([s (set-rem d s)])
              (record-case d
                [(nfv c i)
                 (if (list? c)
                     (set-nfv-conf! d (set-union c s))
                     (set-nfv-conf! d s))
                 (union (union (R v) (R d)) s)]
                [else
                 (for-each (lambda (y) (add-edge! g d y)) s)
                 (union (union (R v) (R d)) s)]))]
           [(bset/c)
            (union (union (R v) (R d)) s)]
           [(bset/h)
            (when (register? eax)
              (when (var? v)
                (for-each (lambda (r) (add-edge! g v r))
                  non-8bit-registers)))
              (union (union (R v) (R d)) s)]
           [(cltd)
            (let ([s (set-rem edx s)])
              (when (register? edx)
                (for-each (lambda (y) 
                            (add-edge! g edx y))
                          s))
              (union (R eax) s))]
           [(idiv) 
            (let ([s (set-rem eax (set-rem edx s))])
              (when (register? eax)
                (for-each (lambda (y) 
                            (add-edge! g eax y)
                            (add-edge! g edx y))
                          s))
              (union (union (R eax) (R edx))
                     (union (R v) s)))]
           [(mset)
            (union (R v) (union (R d) s))]
           [else (error who "invalid effect ~s" x)])]
        [(seq e0 e1) (E e0 (E e1 s))]
        [(conditional e0 e1 e2)
         (let ([s1 (E e1 s)] [s2 (E e2 s)])
           (P e0 s1 s2 (set-union s1 s2)))]
        [(ntcall targ value args mask size)
         (union (R* args) s)]
        [(primcall op arg*)
         (case op
           [(nop) s]
           [(interrupt) 
            (or (exception-live-set) (error who "uninitialized exception"))]
           [else (error who "invalid effect primcall ~s" op)])]
        [(shortcut body handler)
         (let ([s2 (E handler s)])
           (parameterize ([exception-live-set s2])
             (E body s)))]
        [else (error who "invalid effect ~s" (unparse x))]))
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
        [(shortcut body handler)
         (let ([s2 (P handler st sf su)])
           (parameterize ([exception-live-set s2])
             (P body st sf su)))]
        [else (error who "invalid pred ~s" (unparse x))]))
    (define (T x)
      (record-case x
        [(conditional e0 e1 e2)
         (let ([s1 (T e1)] [s2 (T e2)])
           (P e0 s1 s2 (set-union s1 s2)))]
        [(primcall op rands) 
         (R* rands)]
        [(seq e0 e1) (E e0 (T e1))]
        [(shortcut body handler)
         (let ([s2 (T handler)])
           (parameterize ([exception-live-set s2])
              (T body)))]
        [else (error who "invalid tail ~s" (unparse x))]))
    (define exception-live-set (make-parameter #f))
    (let ([s (T x)])
      ;(pretty-print (unparse x))
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
        [(nfv confs loc) 
         (or loc (error who "LHS not set ~s" x))]
        [else x]))
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
        [(nfv c loc)
         (or loc (error who "unset nfv ~s in R" x))]
        [(disp s0 s1) (make-disp (D s0) (D s1))]
        [else
         (if (symbol? x) x (error who "invalid R ~s" x))]))
    ;;; substitute effect
    (define (E x)
      (record-case x
        [(seq e0 e1) (make-seq (E e0) (E e1))]
        [(conditional e0 e1 e2) 
         (make-conditional (P e0) (E e1) (E e2))]
        [(asm-instr op x v)
         (make-asm-instr op (R x) (R v))]
        [(primcall op rands) 
         (make-primcall op (map R rands))]
        [(ntcall) x]
        [(shortcut body handler)
         (make-shortcut (E body) (E handler))]
        [else (error who "invalid effect ~s" (unparse x))]))
    (define (P x)
      (record-case x
        [(constant) x]
        [(asm-instr op x v)
         (make-asm-instr op (R x) (R v))]
        [(conditional e0 e1 e2) 
         (make-conditional (P e0) (P e1) (P e2))]
        [(seq e0 e1) (make-seq (E e0) (P e1))]
        [(shortcut body handler)
         (make-shortcut (P body) (P handler))]
        [else (error who "invalid pred ~s" (unparse x))])) 
    (define (T x)
      (record-case x
        [(primcall op rands) x]
        [(conditional e0 e1 e2) 
         (make-conditional (P e0) (T e1) (T e2))]
        [(seq e0 e1) (make-seq (E e0) (T e1))]
        [(shortcut body handler)
         (make-shortcut (T body) (T handler))]
        [else (error who "invalid tail ~s" (unparse x))]))
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
           (make-seq (E (make-asm-instr 'move u x)) (k u)))]))
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
        [(seq e0 e1) (make-seq (E e0) (E e1))]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (E e1) (E e2))]
        [(asm-instr op a b) 
         (case op
           [(logor logxor logand int+ int- int* move
                   int-/overflow int+/overflow int*/overflow)
            (cond
              [(and (eq? op 'move) (eq? a b)) 
               (make-primcall 'nop '())]
              [(and (mem? a) (mem? b)) 
               (let ([u (mku)])
                 (make-seq
                   (E (make-asm-instr 'move u b))
                   (E (make-asm-instr op a u))))]
              [(disp? a) 
               (let ([s0 (disp-s0 a)] [s1 (disp-s1 a)])
                 (cond
                   [(mem? s0)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-asm-instr 'move u s0))
                        (E (make-asm-instr op (make-disp u s1) b))))]
                   [(mem? s1)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-asm-instr 'move u s1))
                        (E (make-asm-instr op (make-disp s0 u) b))))]
                   [else x]))]
              [(disp? b) 
               (let ([s0 (disp-s0 b)] [s1 (disp-s1 b)])
                 (cond
                   [(mem? s0)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-asm-instr 'move u s0))
                        (E (make-asm-instr op a (make-disp u s1)))))]
                   [(mem? s1)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-asm-instr 'move u s1))
                        (E (make-asm-instr op a (make-disp s0 u)))))]
                   [else x]))] 
              [else x])]
           [(cltd) 
            (unless (and (symbol? a) (symbol? b)) 
              (error who "invalid args to cltd"))
            x]
           [(idiv) 
            (unless (symbol? a) 
              (error who "invalid arg to idiv"))
            (cond
              [(disp? b)
               (error who "invalid arg to idiv ~s" b)]
              [else x])]
           [(sll sra srl)
            (unless (or (constant? b)
                        (eq? b ecx))
              (error who "invalid shift ~s" b))
            x]
           [(mset bset/c bset/h) 
            (cond
              [(mem? b) 
               (let ([u (mku)])
                 (make-seq
                   (E (make-asm-instr 'move u b))
                   (E (make-asm-instr op a u))))]
              [else
               (let ([s1 (disp-s0 a)] [s2 (disp-s1 a)])
                 (cond
                   [(and (mem? s1) (mem? s2))
                    (let ([u (mku)])
                      (make-seq
                        (make-seq
                          (E (make-asm-instr 'move u s1))
                          (E (make-asm-instr 'int+ u s2)))
                        (make-asm-instr op 
                           (make-disp u (make-constant 0))
                           b)))]
                   [(mem? s1)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-asm-instr 'move u s1))
                        (E (make-asm-instr op (make-disp u s2) b))))]
                   [(mem? s2)
                    (let ([u (mku)])
                      (make-seq
                        (E (make-asm-instr 'move u s2))
                        (E (make-asm-instr op (make-disp u s1) b))))]
                   [else x]))])]
           [else (error who "invalid effect ~s" op)])]
        [(primcall op rands) 
         (case op
           [(nop interrupt) x]
           [else (error who "invalid op in ~s" (unparse x))])]
        [(ntcall) x]
        [(shortcut body handler)
         (let ([body (E body)])
           (make-shortcut body (E handler)))]
        [else (error who "invalid effect ~s" (unparse x))]))
    (define (P x)
      (record-case x
        [(constant) x]
        [(primcall op rands)
         (let ([a0 (car rands)] [a1 (cadr rands)])
           (cond
             [(and (fvar? a0) (fvar? a1))
              (let ([u (mku)])
                (make-seq 
                  (make-asm-instr 'move  u a0)
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
                (E (make-asm-instr 'move u b))
                (make-asm-instr op a u)))]
           [else x])]
        [(shortcut body handler)
         (let ([body (P body)])
           (make-shortcut body (P handler)))]
        [else (error who "invalid pred ~s" (unparse x))]))
    (define (T x)
      (record-case x
        [(primcall op rands) x]
        [(conditional e0 e1 e2)
         (make-conditional (P e0) (T e1) (T e2))]
        [(seq e0 e1) (make-seq (E e0) (T e1))]
        [(shortcut body handler)
         (make-shortcut (T body) (T handler))]
        [else (error who "invalid tail ~s" (unparse x))]))
    (let ([x (T x)])
      (values un* x)))
  ;;;
  (define (color-program x)
    (define who 'color-program)
    (record-case x 
      [(locals sp* body)
       (let ([frame-g (build-graph body fvar?)])
         (let loop ([sp* sp*] [un* '()] [body body])
           (let-values ([(un* body) (add-unspillables un* body)])
             (let ([g (build-graph body 
                          (lambda (x) 
                            (and (symbol? x)
                                 (memq x all-registers))))])
               (let-values ([(spills sp* env) (color-graph sp* un* g)])
                 (cond
                   [(null? spills) (substitute env body frame-g)]
                   [else 
                    (let* ([env (do-spill spills frame-g)]
                           [body (substitute env body frame-g)])
                      (loop sp* un* body))]))))))]))
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
      [(foreign-label L) `(foreign-label ,L)]
      [(closure label free*)
       (unless (null? free*) (error who "nonempty closure"))
       `(obj ,x)]
      [(object o)
       `(obj ,o)]
      [else 
       (if (integer? x)
           x
           (error who "invalid constant C ~s" x))]))
  (define (BYTE x)
    (record-case x
      [(constant x)
       (unless (and (integer? x) (fx<= x 255) (fx<= 0 x))
         (error who "invalid byte ~s" x))
       x]
      [else (error who "invalid byte ~s" x)]))
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
  (define (reg/h x)
    (cond
      [(assq x '([%eax %ah] [%ebx %bh] [%ecx %ch] [%edx %dh]))
       => cadr]
      [else (error who "invalid reg/h ~s" x)]))
  (define (R/cl x)
    (record-case x
      [(constant i) 
       (unless (fixnum? i)
         (error who "invalid R/cl ~s" x))
       (fxlogand i 31)]
      [else
       (if (eq? x ecx)
           '%cl
           (error who "invalid R/cl ~s" x))]))
  (define (interrupt? x)
    (record-case x
      [(primcall op args) (eq? op 'interrupt)]
      [else #f]))
  ;;; flatten effect
  (define (E x ac)
    (record-case x
      [(seq e0 e1) (E e0 (E e1 ac))]
      [(conditional e0 e1 e2)
       (cond
         [(interrupt? e1)
          (let ([L (or (exception-label)
                       (error who "no exception label"))])
            (P e0 L #f (E e2 ac)))]
         [(interrupt? e2)
          (let ([L (or (exception-label)
                       (error who "no exception label"))])
            (P e0 #f L (E e1 ac)))]
         [else
          (let ([lf (unique-label)] [le (unique-label)])
            (P e0 #f lf
               (E e1 
                  (list* `(jmp ,le) lf
                     (E e2 (cons le ac))))))])]
      [(ntcall target value args mask size) 
       (let ([LCALL (unique-label)])
         (define (rp-label value)
           (if value
               (label-address SL_multiple_values_error_rp)
               (label-address SL_multiple_values_ignore_rp)))
         (cond
           [(string? target) ;; foreign call
            (list* `(subl ,(* (fxsub1 size) wordsize) ,fpr)
                   `(movl (foreign-label "ik_foreign_call") %ebx)
                   `(jmp ,LCALL)
                   `(byte-vector ,mask)
                   `(int ,(* size wordsize))
                   `(current-frame-offset)
                   (rp-label value)
                   '(byte 0)
                   '(byte 0)
                   '(byte 0)
                   LCALL
                   `(call %ebx)
                   `(addl ,(* (fxsub1 size) wordsize) ,fpr)
                   ac)]
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
         [(int*) (cons `(imull ,(R s) ,(R d)) ac)]
         [(int-) (cons `(subl ,(R s) ,(R d)) ac)]
         [(logor)  (cons `(orl ,(R s) ,(R d)) ac)]
         [(logxor) (cons `(xorl ,(R s) ,(R d)) ac)]
         [(mset) (cons `(movl ,(R s) ,(R d)) ac)]
         [(move) 
          (if (eq? d s)
              ac
              (cons `(movl ,(R s) ,(R d)) ac))]
         [(bset/c) (cons `(movb ,(BYTE s) ,(R d)) ac)]
         [(bset/h) (cons `(movb ,(reg/h s) ,(R d)) ac)]
         [(sll)  (cons `(sall ,(R/cl s) ,(R d)) ac)]
         [(sra)  (cons `(sarl ,(R/cl s) ,(R d)) ac)]
         [(srl)  (cons `(shrl ,(R/cl s) ,(R d)) ac)]
         [(idiv) (cons `(idivl ,(R s)) ac)]
         [(cltd) (cons `(cltd) ac)]
         [(int-/overflow)
          (let ([L (or (exception-label) 
                       (error who "no exception label"))])
            (list* `(subl ,(R s) ,(R d)) 
                   `(jo ,L)
                   ac))]
         [(int*/overflow)
          (let ([L (or (exception-label) 
                       (error who "no exception label"))])
            (list* `(imull ,(R s) ,(R d)) 
                   `(jo ,L)
                   ac))] 
         [(int+/overflow)
          (let ([L (or (exception-label) 
                       (error who "no exception label"))])
            (list* `(addl ,(R s) ,(R d)) 
                   `(jo ,L)
                   ac))]
         [else (error who "invalid instr ~s" x)])]
      [(primcall op rands)
       (case op
         [(nop) ac]
         [(interrupt) 
          (let ([l (or (exception-label)
                       (error who "no exception label"))])
            (cons `(jmp ,l) ac))]
         [else (error who "invalid effect ~s" (unparse x))])]
      [(shortcut body handler)
       (let ([L (unique-interrupt-label)] [L2 (unique-label)])
         (let ([hand (cons L (E handler `((jmp ,L2))))])
           (let ([tc (exceptions-conc)])
             (set-cdr! tc (append hand (cdr tc)))))
         (parameterize ([exception-label L])
           (E body (cons L2 ac))))]
      ;[(shortcut body handler) 
      ; (let ([L (unique-label)] [L2 (unique-label)])
      ;   (let ([ac (cons L (E handler (cons L2 ac)))])
      ;     (parameterize ([exception-label L])
      ;        (E body (cons `(jmp ,L2) ac)))))]
      [else (error who "invalid effect ~s" (unparse x))]))
  ;;;
  (define (unique-interrupt-label)
    (label (gensym "ERROR")))
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
             [(assq x '([= !=] [!= =] [< >=] [<= >] [> <=] [>= <]
                        [u< u>=] [u<= u>] [u> u<=] [u>= u<]))
              => cadr]
             [else (error who "invalid op ~s" x)]))
         (define (jmpname x)
           (cond
             [(assq x '([= je] [!= jne] [< jl] [<= jle] [> jg] [>= jge]
                        [u< jb] [u<= jbe] [u> ja] [u>= jae]))
              => cadr]
             [else (error who "invalid jmpname ~s" x)]))
         (define (revjmpname x)
           (cond
             [(assq x '([= je] [!= jne] [< jg] [<= jge] [> jl] [>= jle]
                        [u< ja] [u<= jae] [u> jb] [u>= jbe]))
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
      [(shortcut body handler)
       (let ([L (unique-interrupt-label)] [lj (unique-label)])
         (let ([ac (if (and lt lf) ac (cons lj ac))])
           (let ([hand (cons L (P handler (or lt lj) (or lf lj) '()))])
             (let ([tc (exceptions-conc)])
               (set-cdr! tc (append hand (cdr tc)))))
           (parameterize ([exception-label L])
             (P body lt lf ac))))]
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
      [(shortcut body handler) 
       (let ([L (unique-interrupt-label)])
         (let ([hand (cons L (T handler '()))])
           (let ([tc (exceptions-conc)])
             (set-cdr! tc (append hand (cdr tc)))))
         (parameterize ([exception-label L])
           (T body ac)))]
      [else (error who "invalid tail ~s" x)]))
  (define exception-label (make-parameter #f))
  ;;;
  (define (handle-vararg fml-count ac)
    (define CONTINUE_LABEL (unique-label))
    (define DONE_LABEL (unique-label))
    (define CONS_LABEL (unique-label))
    (define LOOP_HEAD (unique-label))
    (define L_CALL (unique-label))
    (list* (cmpl (int (argc-convention (fxsub1 fml-count))) eax)
           ;(jg (label SL_invalid_args))
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
                      `(jg ,lothers)]
                     [else
                      `(jl ,lothers)])
               (properize args proper
                  (cons (label L) 
                        (T body (cons lothers ac))))))])]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda L case* free*)
       (list* (length free*) 
              (label L)
          (let ([ac (list '(nop))])
            (parameterize ([exceptions-conc ac])
              (let f ([case* case*])
                (cond
                  [(null? case*) 
                   (cons `(jmp (label ,SL_invalid_args)) ac)]
                  [else
                   (ClambdaCase (car case*) (f (cdr case*)))])))))]))
  ;;;
  (define exceptions-conc (make-parameter #f))
  ;;;
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (cons (list* 0 
                    (label (gensym))
                    (let ([ac (list '(nop))])
                      (parameterize ([exceptions-conc ac])
                        (T body ac))))
             (map Clambda code*))]))
  ;;;
  ;;; (print-code x)
  (Program x))

(define (print-code x)
  (parameterize ([print-gensym '#t])
    (pretty-print (unparse x))))

(define (alt-cogen x)
  (verify-new-cogen-input x)
  (let* (
         ;[foo (printf "0")]
         [x (remove-primcalls x)]
         ;[foo (printf "1")]
         [x (eliminate-fix x)]
         ;[foo (printf "2")]
         [x (normalize-context x)]
         ;[foo (printf "3")]
         [x (remove-complex-operands x)]
         [x (specify-representation x)]
         ;[foo (printf "4")]
         [x (impose-calling-convention/evaluation-order x)]
         ;[foo (printf "5")]
         [x (assign-frame-sizes x)]
         ;[foo (printf "6")]
         [x (color-by-chaitin x)]
         ;[foo (printf "7")]
         [ls (flatten-codes x)]
         ;[foo (printf "8")]
         )
    (when #f
      (parameterize ([gensym-prefix "L"]
                     [print-gensym #f])
        (for-each 
          (lambda (ls)
            (newline)
            (for-each (lambda (x) (printf "    ~s\n" x)) ls))
          ls)))
    ls))
  

#|module alt-cogen|#)


