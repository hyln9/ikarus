
;;; Work in progress
(define (source-optimize x) x)

#!eof


;;; Oscar Waddell. "Extending the Scope of Syntactic Abstraction". PhD.
;;; Thesis. Indiana University Computer Science Department. August 1999.
;;; Available online: 
;;;   http://www.cs.indiana.edu/~owaddell/papers/thesis.ps.gz

(module (source-optimize)
  (define who 'source-optimize)
  (define-struct info (source-referenced source-assigned 
                       residual-referenced residual-assigned))
  (module (init-var! var-info)
    (define (init-var! x) 
      (set-var-index! x (make-info #f #f #f #f)))
    (define (var-info x)
      (let ([v (var-index x)])
        (if (info? v) 
            v
            (error 'var-info "not initialized" x)))))
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
        [(var) (set-info-source-refereced! (var-info x) #t)]
        [(primref)  (void)]
        [(clambda)  (L x)]
        [(seq e0 e1) (E e0) (E e1)]
        [(conditional e0 e1 e2) 
         (E e0) (E e1) (E e2)]
        [(assign x val) 
         (set-info-source-assigned! (var-info x) #t) 
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
             (assert (not (info-source-assigned (var-info x))))) 
           lhs*)]
        [(funcall rator rand*) 
         (for-each E rand*)
         (E rator)]
        [(forcall name rand*)
         (for-each E rand*)]
        [else (error who "invalid expr in prepare" x)]))
    (E x))
  ;;; business
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
  (define (result x)
    (struct-case x
      [(seq e0 e1) e1]
      [else        x]))
  (define (records-equal? x y)
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
  (module (E-call)
    (define (residualize-operands e rand*)
      (cond
        [(null? rand*) e]
        [(not (rand-residualize-for-effect (car rand*)))
         (residualize-operands e (cdr rand*))]
        [else
         (let ([e1 (process-rands-for-effect rand*)])
           (mkseq e1 (residualize-operands e (cdr rand*))))])) 
    (define (value-visit-operand! rand)
      (or (rand-value rand) 
          (let ([e (E (rand-expr rand) (rand-env rand) 'v)])
            (set-rand-value! rand e)
            e)))
    (define (E-call rator rand* env ctxt)
      (let ([ctxt (make-app-ctxt rand* ctxt)])
        (let ([rator (E rator env ctxt)])
          (if (app-inlined ctxt) 
              (residualize-operand rator rand*)
              (make-funcall rator 
                (map value-visit-operand! rand*)))))))
  (define (E x env ctxt)
    (struct-case x
      [(constant) x]
      [(var) (E-var x env ctxt)]
      [(seq e0 e1)
       (mkseq (E e0 env 'e) (E e1 env ctxt))]
      [(conditional e0 e1 e2)
       (let ([e0 (E e0 env 'p)])
         (struct-case (result e0)
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
             [(not (info-source-referenced xi))
              ;;; dead on arrival
              (E v env 'e)]
             [else
              (set-info-residual-assigned! i #t)
              (make-assign x (E v env 'v))]))
         (make-constant (void)))]
      [(funcall rator rand*) 
       (E-call rator (map (mkoperand env) rand*) env ctxt)]
      [(primref name)
       (ctxt-case ctxt
         [(app) (E-fold-prim name env ctxt)]
         [(value) x]
         [else (make-constant (void))])]
      [(clambda g cases cp free name) 
       (ctxt-case ctxt
         [(app) (E-inline x env ctxt)]
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
                          (make-info label args proper)
                          (E body env 'v)))])]))
              cases)
            cp free name)])]
      [else (error who "invalid expression" x)]))
  (define (source-optimize expr)
    (prepare expr)
    (E expr 'v)))



