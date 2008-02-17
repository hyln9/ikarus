
;;; Work in progress



;;; Oscar Waddell. "Extending the Scope of Syntactic Abstraction". PhD.
;;; Thesis. Indiana University Computer Science Department. August 1999.
;;; Available online: 
;;;   http://www.cs.indiana.edu/~owaddell/papers/thesis.ps.gz

(module (source-optimize optimize-level)
  (define who 'source-optimize)
  ;;; this define-structure definition for compatibility with the
  ;;; notation used in Oscar's thesis.
  (define-syntax define-structure
    (lambda (stx) 
      (define (fmt ctxt)
        (lambda (str . args) 
          (datum->syntax ctxt 
            (string->symbol 
              (apply format str (map syntax->datum args))))))
      (syntax-case stx ()
        [(_ (name fields ...)) 
         #'(define-struct name (fields ...))]
        [(_ (name fields ...) ([others defaults] ...))
         (with-syntax ([(pred maker (getters ...) (setters ...))
                        (let ([fmt (fmt #'name)])
                          (list (fmt "~s?" #'name)
                                (fmt "make-~s" #'name)
                                (map (lambda (x) (fmt "~s-~s" #'name x))
                                     #'(fields ... others ...))
                                (map (lambda (x) (fmt "set-~s-~s!" #'name x))
                                     #'(fields ... others ...))))])
           #'(module (name pred getters ... setters ... maker)
               (module P (name pred getters ... setters ... maker)
                 (define-struct name (fields ... others ...)))
               (module (maker)
                 (define (maker fields ...)
                   (import P)
                   (maker fields ... defaults ...)))
               (module (name pred getters ... setters ...)
                 (import P))))])))
  ;;;
  (define-structure (prelex name operand)
    ([source-referenced?   #f]
     [source-assigned?     #f]
     [residual-referenced? #f]
     [residual-assigned?   #f]))
  ;;;
  (define-structure (app rand* ctxt)
    ([inlined #f]))
  ;;;
  (define-structure (operand expr env)
    ([value                  #f]
     [residualize-for-effect #f]))
  ;;;
  (module (init-var! var-info)
    (define (init-var! x)
      (set-var-index! x (make-prelex #f #f)))
    (define (var-info x)
      (let ([v (var-index x)])
        (if (prelex? v)
            v
            (error 'var-info "not initialized" x)))))
  (module (with-extended-env)
    (define (extend e args rands) 
      (error 'extend "not yet"))
    (define-syntax with-extended-env
      (syntax-rules ()
        [(_ ((e2 args2) (e1 args1 rands)) b b* ...)
         (let-values ([(e2 args2) (extend e1 args1 rands)])
           b b* ...)])))
  ;;; purpose of prepare:
  ;;; 1. attach an info struct to every bound variable
  ;;; 2. set the plref and plset flags to indicate whether
  ;;;    there is a reference/assignment to the variable.
  ;;; 3. verify well-formness of the input.
  (define (prepare x)
    (define (L x)
      (for-each 
        (lambda (x)
          (struct-case x
            [(clambda-case info body)
             (for-each init-var! (case-info-args info))
             (E body)]))
        (clambda-cases x)))
    (define (E x) 
      (struct-case x 
        [(constant) (void)]
        [(var) (set-prelex-source-referenced?! (var-info x) #t)]
        [(primref)  (void)]
        [(clambda)  (L x)]
        [(seq e0 e1) (E e0) (E e1)]
        [(conditional e0 e1 e2) 
         (E e0) (E e1) (E e2)]
        [(assign x val) 
         (set-prelex-source-assigned?! (var-info x) #t)
         (E val)]
        [(bind lhs* rhs* body)
         (for-each E rhs*)
         (for-each init-var! lhs*)
         (E body)]
        [(fix lhs* rhs* body)
         (for-each init-var! lhs*)
         (for-each L rhs*)
         (E body)
         (for-each ;;; sanity check
           (lambda (x)
             (assert (not (prelex-source-assigned? (var-info x))))) 
           lhs*)]
        [(funcall rator rand*) 
         (for-each E rand*)
         (E rator)]
        [(forcall name rand*)
         (for-each E rand*)]
        [else (error who "invalid expr in prepare" x)]))
    (E x))
  ;;; business
  (define-syntax ctxt-case
    (lambda (stx)
      (define (test x)
        (case (syntax->datum x)
          [(p)   #'(eq? t 'p)]
          [(v)   #'(eq? t 'v)]
          [(e)   #'(eq? t 'e)]
          [(app) #'(app? t)]
          [else (syntax-violation stx "invalid ctxt" x)]))
      (define (extract cls*)
        (syntax-case cls* (else)
          [([else e e* ...]) #'(begin e e* ...)]
          [([(t* ...) e e* ...] rest ...) 
           (with-syntax ([(t* ...) (map test #'(t* ...))]
                         [body (extract #'(rest ...))])
             #'(if (or t* ...) 
                   (begin e e* ...)
                   body))]))
      (syntax-case stx ()
        [(_ expr cls* ...)
         (with-syntax ([body (extract #'(cls* ...))])
           #'(let ([t expr])
               body))])))
  (define (mkseq e0 e1)
    ;;; returns a (seq e0 e1) with a seq-less e1 if both 
    ;;; e0 and e1 are constructed properly.
    (if (simple? e0)
        e1
        (let ([e0 (struct-case e0
                    [(seq e0a e0b) (if (simple? e0b) e0a e0)]
                    [else e0])])
          (struct-case e1
            [(seq e1a e1b) (make-seq (make-seq e0 e1a) e1b)]
            [else (make-seq e0 e1)]))))
  ;;; simple?: check quickly whether something is effect-free
  (define (simple? x) 
    (struct-case x
      [(constant) #t]
      [(var)      #t]
      [(primref)  #t]
      [(clambda)  #t]
      [else       #f]))
  ;;; result returns the "last" value of an expression
  (define (result-expr x)
    (struct-case x
      [(seq e0 e1) e1]
      [else        x]))
  ;;;
  (define (records-equal? x y ctxt)
    (struct-case x
      [(constant kx)
       (struct-case y
         [(constant ky)
          (ctxt-case ctxt
            [(e) #t]
            [(p) (if kx ky (not ky))]
            [else (eq? kx ky)])]
         [else #f])]
      [else #f]))
  ;;;
  (module (E-call value-visit-operand!)
    (define (residualize-operand x)
      (or (operand-value x)
          (E (operand-expr x) (operand-env x) 'e)))
    (define (residualize-operands e rand*)
      (cond
        [(null? rand*) e]
        [(not (operand-residualize-for-effect (car rand*)))
         (residualize-operands e (cdr rand*))]
        [else
         (mkseq 
           (residualize-operand (car rand*))
           (residualize-operands e (cdr rand*)))]))
    (define (value-visit-operand! rand)
      (or (operand-value rand) 
          (let ([e (E (operand-expr rand) (operand-env rand) 'v)])
            (set-operand-value! rand e)
            e)))
    (define (E-call rator rand* env ctxt)
      (let ([ctxt (make-app rand* ctxt)])
        (let ([rator (E rator env ctxt)])
          (if (app-inlined ctxt)
              (residualize-operands rator rand*)
              (make-funcall rator 
                (map value-visit-operand! rand*)))))))
  ;;;
  (define (E-var x env ctxt)
    (ctxt-case ctxt
      [(e) (make-constant (void))]
      [else 
       (let ([x (lookup x env)])
         (let ([opnd (prelex-operand x)])
           (if opnd
               (begin
                 (value-visit-operand! opnd)
                 (if (prelex-source-assigned? x) 
                     (residualize-ref x)
                     (copy x opnd ctxt)))
               (residualize-ref x))))]))
  ;;;
  (define (copy x opnd ctxt)
    (let ([rhs (result-expr (operand-value opnd))])
      (struct-case rhs
        [(constant) rhs]
        [(var) 
         (if (prelex-source-assigned? rhs)
             (residualize-ref x)
             (let ([opnd (prelex-operand rhs)])
               (if (and opnd (operand-value opnd))
                   (copy2 rhs opnd ctxt)
                   (residualize-ref rhs))))]
        [else (copy2 x opnd ctxt)])))
  (define (copy2 x opnd ctxt)
    (let ([rhs (result-expr (operand-value opnd))])
      (struct-case rhs
        [(clambda) 
         (ctxt-case ctxt
           [(v) (residualize-ref x)]
           [(p) (make-constant #t)]
           [(e) (make-constant (void))]
           [(app) (inline rhs ctxt empty-env)]
           [else (error 'copy2 "cannot happen")])]
        [(primref p) 
         (ctxt-case ctxt
           [(v) rhs]
           [(p) (make-constant #t)]
           [(e) (make-constant (void))]
           [(app) (fold-prim p ctxt)]
           [else (error 'copy2 "cannot happen")])]
        [else (residualize-ref x)])))
  ;;;
  (define (inline proc ctxt env)
    (struct-case proc
      [(clambda g cases cp free name)
       (let ([rand* (app-rand* ctxt)])
         (struct-case (get-case cases rand*)
           [(clambda-case info body) 
            (struct-case info
              [(case-info label args proper) 
               (with-extended-env ((env args) (env args rand*))
                 (let ([body (E body (app-ctxt ctxt) env)])
                   (let ([result (make-let-binding args rand* body)])
                     (set-app-inlined! ctxt #t)
                     result)))])]
           [else proc]))]))
  ;;;
  (define (make-let-binding args rand* body) 
    (error 'make-let-binding "not yet"))
  (define (get-case . args)
    (error 'get-case "not yet"))
  ;;;
  (define (fold-prim p ctxt)
    (make-primref p))
  ;;;
  (define (residualize-ref x)
    (set-prelex-residual-referenced?! (var-info x) #t)
    x)
  ;;;
  (define (E x env ctxt)
    (struct-case x
      [(constant) x]
      [(var) (E-var x env ctxt)]
      [(seq e0 e1)
       (mkseq (E e0 env 'e) (E e1 env ctxt))]
      [(conditional e0 e1 e2)
       (let ([e0 (E e0 env 'p)])
         (struct-case (result-expr e0)
           [(constant k) 
            (mkseq e0 (E (if k e1 e2) env ctxt))]
           [else 
            (let ([ctxt (ctxt-case ctxt [(app) 'v] [else ctxt])])
              (let ([e1 (E e1 env ctxt)]
                    [e2 (E e2 env ctxt)])
                (if (records-equal? e1 e2 ctxt)
                    (mkseq e0 e1)
                    (make-conditional e0 e1 e2))))]))]
      [(assign x v)
       (mkseq
         (let ([xi (var-info x)])
           (cond
             [(not (prelex-source-referenced? xi))
              ;;; dead on arrival
              (E v env 'e)]
             [else
              (set-prelex-residual-assigned?! xi #t)
              (make-assign x (E v env 'v))]))
         (make-constant (void)))]
      [(funcall rator rand*)
       (E-call rator (map (make-operand env) rand*) env ctxt)]
      [(primref name)
       (ctxt-case ctxt
         [(app) (fold-prim env ctxt)]
         [(v) x]
         [else (make-constant (void))])]
      [(clambda g cases cp free name) 
       (ctxt-case ctxt
         [(app) (inline x env ctxt)]
         [(p e) (make-constant (void))]
         [else
          (make-clambda g
            (map 
              (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (struct-case info
                     [(case-info label args proper) 
                      (with-extended-env ((env args) (env args #f))
                        (make-clambda-case 
                          (make-case-info label args proper)
                          (E body env 'v)))])]))
              cases)
            cp free name)])]
      [else (error who "invalid expression" x)]))
  (define empty-env '())
  (define (lookup . args)
    (error 'lookup "not yet"))
  (define optimize-level 
    (make-parameter #f))
  (define (source-optimize expr)
    (cond
      [(equal? (optimize-level) 17)
       (prepare expr)
       (pretty-print (unparse expr))
       (let ([expr (E expr empty-env 'v)])
         (pretty-print (unparse expr))
         expr)]
      [else expr])))




