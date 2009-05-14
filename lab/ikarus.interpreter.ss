
(import (ikarus))

(define (make-annotated-procedure ann proc)
  (import (ikarus system $codes))
  ($make-annotated-procedure ann proc))


(module (click-ring unclick-ring get-traces mark-reduction!
                    last-trace reset-ring) 

  (define outer-ring-size 5)
  (define inner-ring-size 5)

  (define-struct cell (prev next content))
  
  (define (make-ring n f)
    (cond
      [(= n 1) 
       (let ([cell (make-cell #f #f (f))])
         (set-cell-prev! cell cell)
         (set-cell-next! cell cell)
         cell)]
      [else 
       (let ([ring (make-ring (- n 1) f)])
         (let ([next (cell-next ring)])
           (let ([cell (make-cell ring next (f))])
             (set-cell-prev! next cell)
             (set-cell-next! ring cell)
             cell)))]))
  
  (define (make-double-ring n m)
    (make-ring n (lambda () (make-ring m (lambda () #f)))))

  (define (ring->list x0)
    (let f ([x x0])
      (cons (cell-content x)
        (let ([x (cell-prev x)])
          (if (eq? x x0)
              '()
              (f x))))))
   
  (define (get-traces)
    (map ring->list (ring->list step-ring)))

  (define step-ring #f)

  (define (reset-ring)
    (set! step-ring (make-double-ring outer-ring-size inner-ring-size)))

  (define (last-trace)
    (cell-content (cell-content step-ring)))

  (define (click-ring)
    (let ([x (cell-next step-ring)])
      (set! step-ring x)
      (let ([y (cell-content x)])
        (set-cell-content! y #f))))

  (define (unclick-ring)
    (set-cell-content! (cell-content step-ring) #f)
    (set! step-ring (cell-prev step-ring)))

  (define (mark-reduction! x)
    (let ([y (cell-next (cell-content step-ring))])
      (set-cell-content! y x)
      (set-cell-content! step-ring y)))
)

(define (primitive-value x)
  (import (ikarus system $codes))
  (import (ikarus system $structs))
  (import (ikarus system $flonums))
  (import (ikarus system $bignums))
  (case x
    [($code-reloc-vector) (lambda (x) ($code-reloc-vector x))]
    [($code-freevars) (lambda (x) ($code-freevars x))]
    [($code-size) (lambda (x) ($code-size x))]
    [($code-annotation) (lambda (x) ($code-annotation x))]
    [($code-ref) (lambda (x i) ($code-ref x i))]
    [($code-set!) (lambda (x i v) ($code-set! x i v))]
    [($code->closure) (lambda (x) ($code->closure x))]
    [($closure-code) (lambda (x) ($closure-code x))]
    [($struct)
     (case-lambda
       [(x0)
($struct x0)]
       [(x0 x1) 
($struct x0 x1)]
       [(x0 x1 x2)
($struct x0 x1 x2)]
       [(x0 x1 x2 x3)
($struct x0 x1 x2 x3)]
       [(x0 x1 x2 x3 x4)
($struct x0 x1 x2 x3 x4)]
       [(x0 x1 x2 x3 x4 x5)
($struct x0 x1 x2 x3 x4 x5)]
       [(x0 x1 x2 x3 x4 x5 x6) 
($struct x0 x1 x2 x3 x4 x5 x6)]
       [(x0 x1 x2 x3 x4 x5 x6 x7)
($struct x0 x1 x2 x3 x4 x5 x6 x7)]
       [(x0 x1 x2 x3 x4 x5 x6 x7 x8)
($struct x0 x1 x2 x3 x4 x5 x6 x7 x8)]
       [(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
($struct x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)]
       [(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
($struct x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)]
       [(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
($struct x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)]
       )]
    [($struct-set!) struct-set!]
    [($struct-ref) struct-ref]
    [($annotated-procedure-annotation) 
     (lambda (x) ($annotated-procedure-annotation x))]
    [($char<=) char<=?]
    [($string-ref) string-ref]
    [($flonum-u8-ref) 
     (lambda (x i) 
       (case i
         [(0) ($flonum-u8-ref x 0)]
         [(1) ($flonum-u8-ref x 1)]
         [(2) ($flonum-u8-ref x 2)]
         [(3) ($flonum-u8-ref x 3)]
         [(4) ($flonum-u8-ref x 4)]
         [(5) ($flonum-u8-ref x 5)]
         [(6) ($flonum-u8-ref x 6)]
         [(7) ($flonum-u8-ref x 7)]
         [else (error 'flonum-u8-ref "invalid index" i)]))]
    [($bignum-positive?) positive?]
    [($bignum-byte-ref) (lambda (x i) ($bignum-byte-ref x i))]
    [($bignum-size) (lambda (x) ($bignum-size x))]
    [else (system-value x)]))


(define core-interpret
  (let ()
    (define who 'interpret)
    (define (lexical-set! env i j v)
      (vector-set! (list-ref env i) j v))
    (define (lexical-ref env i j)
      (vector-ref (list-ref env i) j))
    (define (global-set! loc v)
      (set-symbol-value! loc v))
    (define (global-ref loc)
      (top-level-value loc))
    (define (extend-env ls env)
      (define (properize x)
        (cond
          [(null? x) '()]
          [(pair? x) (cons (car x) (properize (cdr x)))]
          [else (list x)]))
      (let ([v (list->vector (properize ls))])
        (values (cons v env) (vector-length v))))
    (define (lookup x env)
      (let f ([i 0] [env env])
        (cond
          [(null? env) `(global ,x)]
          [else
           (let g ([j 0] [rib (car env)])
             (cond
               [(= j (vector-length rib)) 
                (f (+ i 1) (cdr env))]
               [(eq? x (vector-ref rib j)) `(lexical ,i ,j)]
               [else (g (+ j 1) rib)]))])))
    (define (get-fmls x args)
      (define (matching? fmls args)
        (cond
          [(null? fmls) (null? args)]
          [(pair? fmls) (and (pair? args) (matching? (cdr fmls) (cdr args)))]
          [else #t]))
      (define (get-cls* x)
        (if (pair? x)
            (case (car x)
              [(case-lambda) (cdr x)]
              [(annotated-case-lambda) (cddr x)]
              [else '()])
            '()))
      (let f ([cls* (get-cls* x)])
        (cond
          [(null? cls*) #f]
          [(matching? (caar cls*) args)
           (caar cls*)]
          [else (f (cdr cls*))])))
    (define (compile-letrec* binding* body env ctxt tail?)
      (let ([lhs* (map car binding*)]
            [rhs* (map cadr binding*)])
        (let-values ([(env n) (extend-env lhs* env)])
          (let ([rhs* (map (lambda (lhs rhs) 
                             (compile-expr rhs env lhs #f))
                           lhs* rhs*)]
                [body (compile-expr body env ctxt tail?)])
            (lambda (env)
              (let ([vec (make-vector n)])
                (let ([env (cons vec env)])
                  (let f ([i 0] [rhs* rhs*])
                    (if (null? rhs*)
                        (body env)
                        (begin 
                          (vector-set! vec i ((car rhs*) env))
                          (f (+ i 1) (cdr rhs*))))))))))))
    (define (compile-library-letrec* binding* body env ctxt tail?)
      (let ([lhs* (map car binding*)]
            [loc* (map cadr binding*)]
            [rhs* (map caddr binding*)])
        (let-values ([(env n) (extend-env lhs* env)])
          (let ([rhs* (map (lambda (lhs rhs)
                             (compile-expr rhs env lhs #f))
                           lhs* rhs*)]
                [body (compile-expr body env ctxt tail?)])
            (lambda (env)
              (let ([vec (make-vector n)])
                (let ([env (cons vec env)])
                  (let f ([i 0] [rhs* rhs*] [loc* loc*])
                    (if (null? rhs*)
                        (body env)
                        (let ([v ((car rhs*) env)]) 
                          (vector-set! vec i v)
                          (global-set! (car loc*) v)
                          (f (+ i 1) (cdr rhs*) (cdr loc*))))))))))))
    (define (compile-case-lambda ae binding* env ctxt)
      (define (compile-clause fmls expr k)
        (let-values ([(env n) (extend-env fmls env)])
          (let ([expr (compile-expr expr env
                        (if (pair? ctxt) (car ctxt) #f)
                        #t)])
            (cond
              [(list? fmls) 
               (lambda (env args argcount)
                 (if (= n argcount) 
                     (expr (cons (list->vector args) env))
                     (k env args argcount)))]
              [else
               (lambda (env args argcount)
                 (let ([n1 (- n 1)])
                   (if (>= argcount n1)
                       (let ([vec (make-vector n)])
                         (let f ([ls args] [i 0])
                           (cond
                             [(= i n1)
                              (vector-set! vec i ls)]
                             [else
                              (vector-set! vec i (car ls))
                              (f (cdr ls) (+ i 1))]))
                         (expr (cons vec env)))
                       (k env args argcount))))]))))
      (let ([proc 
             (let f ([binding* binding*])
               (cond
                 [(null? binding*) 
                  (lambda (env args argcount)
                    (assertion-violation 'apply 
                      "incorrect number of arguments" 
                      args))]
                 [else
                  (let ([b (car binding*)])
                    (compile-clause (car b) (cadr b)
                      (f (cdr binding*))))]))])
        (lambda (env)
          (make-annotated-procedure
            (cons 
              (and (symbol? ctxt) ctxt)
              (and (annotation? ae) (annotation-source ae)))
            (lambda args
              (proc env args (length args)))))))
    (define (compile-var expr env)
      (let ([x (lookup expr env)])
        (case (car x)
          [(lexical) 
           (let ([i (cadr x)] [j (caddr x)])
             (lambda (env) 
               (lexical-ref env i j)))]
          [(global) 
           (let ([loc (cadr x)])
             (lambda (env)
               (global-ref loc)))]
          [else (die who "invalid value" x)])))
    (define (compile-set! lhs rhs env)
      (let ([rhs (compile-expr rhs env lhs #f)]
            [x (lookup lhs env)])
        (case (car x)
          [(lexical)
           (let ([i (cadr x)] [j (caddr x)])
             (lambda (env)
               (lexical-set! env i j (rhs env))))]
          [(global)
           (let ([loc (cadr x)])
             (lambda (env)
               (global-set! loc (rhs env))))]
          [else (die who "invalid set! target" lhs)])))
    (define (compile-if e0 e1 e2 env ctxt tail?)
      (let ([e0 (compile-expr e0 env #f #f)]
            [e1 (compile-expr e1 env ctxt tail?)]
            [e2 (compile-expr e2 env ctxt tail?)])
        (lambda (env)
          (if (e0 env) (e1 env) (e2 env)))))
    (define (compile-begin e e* env ctxt tail?)
      (cond
        [(null? e*) (compile-expr e env ctxt tail?)]
        [else
         (let ([e0 (compile-expr e env #f #f)]
               [e* (compile-begin (car e*) (cdr e*) env ctxt tail?)])
           (lambda (env) (e0 env) (e* env)))]))
    (define (get-src/expr ae)
      (if (annotation? ae)
          (values (annotation-source ae) (annotation-stripped ae))
          (values #f (syntax->datum ae))))
    (define (compile-tail-call ae e e* env ctxt)
      (let ([e (compile-expr e env (list ctxt) #f)]
            [fmls (get-fmls e e*)])
        (let ([e* 
               (let f ([e* e*] [fmls fmls])
                 (cond
                   [(pair? fmls)
                    (cons
                      (compile-expr (car e*) env (car fmls) #f)
                      (f (cdr e*) (cdr fmls)))]
                   [else
                    (map (lambda (x) (compile-expr x env #f #f)) e*)]))])
          (if (or #t ae)
              (let-values ([(src expr) (get-src/expr ae)])
                (lambda (env)
                  (let ([e (e env)] [e* (map (lambda (e) (e env)) e*)])
                    (let ([tr (last-trace)])
                      (mark-reduction!
                        (make-trace src expr env e e* 
                          (if tr (+ 1 (trace-depth tr)) 0))))
                    (apply e e*))))
              (lambda (env)
                (let ([e (e env)] [e* (map (lambda (e) (e env)) e*)])
                  (apply e e*)))))))
    (define (compile-nontail-call ae e e* env ctxt)
      (let ([proc (compile-tail-call ae e e* env ctxt)])
        (lambda (env)
          (dynamic-wind
            click-ring
            (lambda () (proc env))
            unclick-ring))))
    (define (compile-call ae e e* env ctxt tail?)
      (define (let-pattern? e e*)
        (let ([ls (get-fmls e e*)])
          (and (list? ls) (= (length ls) (length e*)))))
      (define (compile-let e e* env ctxt tail?)
        (let ([ls (get-fmls e e*)])
          (let ([e (compile-expr e env ctxt env)]
                [e* (map (lambda (x e) 
                           (compile-expr e env x env))
                         ls e*)])
            (lambda (env)
              (apply (e env)
                (map (lambda (e) (e env)) e*))))))
      (if (let-pattern? e e*)
          (compile-let e e* env ctxt tail?)
          (if tail?
              (compile-tail-call ae e e* env ctxt)
              (compile-nontail-call ae e e* env ctxt))))
    (define (compile-expr expr env ctxt tail?)
      (cond
        [(symbol? expr) (compile-var expr env)]
        [(and (pair? expr) (list? expr))
         (let ([a (car expr)] [d (cdr expr)])
           (case a
             [(quote) (let ([v (car d)]) (lambda (env) v))]
             [(set!) (compile-set! (car d) (cadr d) env)]
             [(if)
              (compile-if (car d) (cadr d) (caddr d) env ctxt tail?)]
             [(begin) 
              (compile-begin (car d) (cdr d) env ctxt tail?)]
             [(lambda)
              (compile-case-lambda #f (list d) env ctxt)]
             [(case-lambda)
              (compile-case-lambda #f d env ctxt)]
             [(annotated-case-lambda)
              (compile-case-lambda (car d) (cdr d) env ctxt)]
             [(letrec letrec*)
              (compile-letrec* (car d) (cadr d) env ctxt tail?)]
             [(library-letrec*)
              (compile-library-letrec* (car d) (cadr d) env ctxt tail?)]
             [(primitive)
              (let ([v (primitive-value (car d))])
                (lambda (env) v))]
             [(annotated-call) 
              (compile-call (car d) (cadr d) (cddr d) env ctxt tail?)]
             [(foreign-call) 
              (let ([name (car d)] [args (cdr d)])
                (let ([ls (map (lambda (x) (gensym)) args)])
                  (let ([code 
                         (real-eval
                            `(lambda ,ls (foreign-call ,name . ,ls)))])
                    (compile-expr `(',code . ,args) env ctxt tail?))))]
             [else
              (compile-call #f a d env ctxt tail?)]))]
        [else
         (die who "invalid expression" expr)]))
    (lambda (expr)
      ((compile-expr expr '() #f #t) '()))))

(define-struct trace (src expr env v v* depth))

(define (print-trace x)
  (printf "  [~a] ~s\n" (trace-depth x) (trace-expr x))
  (let ([src (trace-src x)])
    (when (pair? src)
      (printf "     source: char ~a of ~a\n" (cdr src) (car src))))
  (printf "     operator: ~s\n" (trace-v x))
  (printf "     operands: ~s\n" (trace-v* x)))

(define (print-step ls)
  (let ([ls (let f ([ls ls])
              (if (or (null? ls) (not (car ls)))
                  '()
                  (cons (car ls) (f (cdr ls)))))])
    (unless (null? ls)
     (printf "FRAME:\n")
     (for-each print-trace (reverse ls)))))

(define (print-all-traces)
  (for-each print-step (get-traces)))


(define (start-repl)
  (display "Ikarus Interpreter\n\n")
  (new-cafe
    (lambda (x)
      (with-exception-handler
        (lambda (con)
          (print-all-traces)
          (raise-continuable con))
        (lambda ()
          (reset-ring)
          (eval x (interaction-environment)))))))

(define (start-script script-name args)
  (command-line-arguments (cons script-name args))
  (with-exception-handler
    (lambda (con)
      (print-all-traces)
      (raise-continuable con))
    (lambda ()
      (reset-ring)
      (load-r6rs-script script-name #f #t))))
 
(define original-eval (current-core-eval))

(define (real-eval x)
  (parameterize ([current-core-eval original-eval])
    (eval x (environment '(ikarus)))))

(current-core-eval core-interpret)
(apply
  (case-lambda
    [(interpreter flag script-name . rest) 
     (if (string=? flag "--r6rs-script") 
         (start-script script-name rest)
         (error interpreter "invalid args" (cons* flag script-name rest)))]
    [(interpreter) (start-repl)]
    [(interpreter . rest)
     (error interpreter "invalid args" rest)])
  (command-line-arguments))


 


#!eof

(print-graph #t)

;(write (make-double-rib 5 5))
(write (make-ring 10 (lambda () #f)))
(newline)
