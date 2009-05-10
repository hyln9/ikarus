
(import (ikarus) (match))

(define (make-annotated-procedure ann proc)
  (import (ikarus system $codes))
  ($make-annotated-procedure ann proc))


(define (compile-expr expr env)
  (define who 'compile-expr)
  (define (do-letrec lhs* rhs* body env)
    (let-values ([(env n) (extend-env lhs* env)])
      (let ([rhs* (map (lambda (x) (compile-expr x env)) rhs*)]
            [body (compile-expr body env)])
        (lambda (env)
          (let ([vec (make-vector n)])
            (let ([env (cons vec env)])
              (let f ([i 0] [rhs* rhs*])
                (if (null? rhs*)
                    (body env)
                    (begin 
                      (vector-set! vec i ((car rhs*) env))
                      (f (+ i 1) (cdr rhs*)))))))))))
  (define (do-library-letrec lhs* loc* rhs* body env)
    (let-values ([(env n) (extend-env lhs* env)])
      (let ([rhs* (map (lambda (x) (compile-expr x env)) rhs*)]
            [body (compile-expr body env)])
        (lambda (env)
          (let ([vec (make-vector n)])
            (let ([env (cons vec env)])
              (let f ([i 0] [rhs* rhs*] [loc* loc*])
                (if (null? rhs*)
                    (body env)
                    (let ([v ((car rhs*) env)]) 
                      (vector-set! vec i v)
                      (global-set! (car loc*) v)
                      (f (+ i 1) (cdr rhs*) (cdr loc*)))))))))))
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
  (define (compile-case-lambda ae fml* expr* env k)
    (define (compile-clause fmls expr k)
      (cond
        [(list? fmls) 
         (let-values ([(env n) (extend-env fmls env)])
           (let ([expr (compile-expr expr env)])
             (lambda (env args argcount)
               (if (= n argcount) 
                   (expr (cons (list->vector args) env))
                   (k env args argcount)))))]
        [else
         (let-values ([(env n) (extend-env fmls env)])
           (let ([expr (compile-expr expr env)])
             (lambda (env args argcount)
               (if (>= argcount n)
                   (let ([vec (make-vector n)])
                     (let f ([ls args] [i 0])
                       (cond
                         [(= i (- n 1)) 
                          (vector-set! vec i ls)]
                         [else
                          (vector-set! vec i (car ls))
                          (f (cdr ls) (+ i 1))]))
                     (expr (cons vec env)))
                   (k env args argcount)))))]))
    (let ([proc 
           (let f ([fml* fml*] [expr* expr*])
             (cond
               [(null? fml*) k]
               [else
                (compile-clause (car fml*) (car expr*)
                  (f (cdr fml*) (cdr expr*)))]))])
      (lambda (env)
        (make-annotated-procedure
          (cons #f
               (if (annotation? ae)
                   (annotation-source ae)
                   '#f))
          (lambda args
            (proc env args (length args)))))))
  (match expr
    [(quote ,x) (lambda (env) x)]
    [,var (guard (gensym? var))
     (let ([var (lookup var env)])
       (match var
         [(lexical ,i ,j)
          (lambda (env) 
            (lexical-ref env i j))]
         [(global ,loc) 
          (lambda (env)
            (global-ref loc))]
         [,_ (die who "invalid variable" var)]))]
    [(set! ,lhs ,[rhs])
     (let ([lhs (lookup lhs env)])
       (match lhs
         [(lexical ,i ,j) 
          (lambda (env) 
            (lexical-set! env i j (rhs env)))]
         [(global ,loc)
          (lambda (env)
            (global-set! loc (rhs env)))]
         [,_ (die who "invalid set! target" lhs)]))]
    [(if ,[e0] ,[e1] ,[e2])
     (lambda (env) 
       (if (e0 env) (e1 env) (e2 env)))]
    [(begin ,[e] ,[e*] ...) 
     (let ([e* (cons e e*)])
       (lambda (env)
         (for-each (lambda (e) (e env)) e*)))]
    [(annotated-case-lambda ,ae [,fml* ,expr*] ...)
     (compile-case-lambda ae fml* expr* env 
       (lambda (env args argcount)
         (assertion-violation 'apply 
           "incorrect number of arguments" 
           args)))]
    [(case-lambda [,fml* ,expr*] ...)
     (compile-case-lambda #f fml* expr* env 
       (lambda (env args argcount)
         (assertion-violation 'apply 
           "incorrect number of arguments" 
           args)))] 
    [(lambda ,fmls ,body) 
     (compile-expr `(case-lambda [,fmls ,body]) env)]
    [(letrec ([,lhs* ,rhs*] ...) ,body)
     (do-letrec lhs* rhs* body env)]
    [(letrec* ([,lhs* ,rhs*] ...) ,body)
     (do-letrec lhs* rhs* body env)]
    [(library-letrec* ([,lhs* ,loc* ,rhs*] ...) ,body)
     (do-library-letrec lhs* loc* rhs* body env)]
    [(primitive ,x)
     (let ([v (system-value x)])
       (lambda (env) v))]
    [(,[e] ,[e*] ...)
     (lambda (env)
       (apply (e env) (map (lambda (e) (e env)) e*)))]
    [,x (die who "invalid expression" x)]))


(parameterize ([current-core-eval
                (lambda (expr) 
                  ;(display "=================\n")
                  ;(pretty-print expr)
                  ;(display "=================\n")
                  (let ([proc (compile-expr expr '())])
                    (proc '())))])
  (display "Ikarus Interpreter\n\n")
  (new-cafe))


     


#!eof

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
       (let ([prev (cell-prev ring)]
             [next (cell-next ring)])
         (let ([cell (make-cell ring next (f))])
           (set-cell-next! prev cell)
           (set-cell-prev! ring cell)
           cell)))]))

(define (make-double-ring n m)
  (make-ring n (lambda () (make-ring m (lambda () #f)))))

(print-graph #t)

;(write (make-double-rib 5 5))
(write (make-ring 10 (lambda () #f)))
(newline)
