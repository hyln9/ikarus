
(import (ikarus))

(define (make-annotated-procedure ann proc)
  (import (ikarus system $codes))
  ($make-annotated-procedure ann proc))

(define id
  (let ([id -1])
    (lambda () 
      (set! id (+ id 1))
      id)))

(module (with-click get-traces mark-reduction!
                    reset-ring) 

  (define outer-ring-size 20)
  (define inner-ring-size 20)

  (define-struct cell (prev next num content))
  
  (define (make-ring n f)
    (cond
      [(= n 1) 
       (let ([cell (make-cell #f #f -1 (f))])
         (set-cell-prev! cell cell)
         (set-cell-next! cell cell)
         cell)]
      [else 
       (let ([ring (make-ring (- n 1) f)])
         (let ([next (cell-next ring)])
           (let ([cell (make-cell ring next -1 (f))])
             (set-cell-prev! next cell)
             (set-cell-next! ring cell)
             cell)))]))
  
  (define (make-double-ring n m)
    (make-ring n (lambda () (make-ring m (lambda () #f)))))

  (define (ring->list x)
    (let f ([x x] [orig #f])
      (if (or (eq? x orig) (eqv? (cell-num x) -1))
          '()
           (cons (cons (cell-num x) (cell-content x))
             (f (cell-prev x) (or orig x))))))
   
  (define (get-traces)
    (map (lambda (x) 
           (cons (car x) (ring->list (cdr x))))
         (ring->list step-ring)))

  (define step-ring #f)

  (define (reset-ring)
    (printf "RESET!\n")
    (set! step-ring (make-double-ring outer-ring-size inner-ring-size))
    (set-cell-num! step-ring 0))

  (define (with-click p)
    (dynamic-wind
      (lambda () 
        (let ([prev step-ring])
          (let ([next (cell-next prev)])
            (set-cell-num! next (+ (cell-num prev) 1))
            (set-cell-num! (cell-content next) -1)
            (set! step-ring next))))
      p
      (lambda ()
        (let ([next step-ring])
          (let ([prev (cell-prev next)])
            (set-cell-num! prev (- (cell-num next) 1))
            (set-cell-num! next -1)
            (set-cell-num! (cell-content next) -1)
            (set! step-ring prev))))))

  (define (mark-reduction! x)
    (let ([prev (cell-content step-ring)])
      (let ([next (cell-next prev)])
        (set-cell-content! next x)
        (set-cell-num! next (+ (cell-num prev) 1))
        (set-cell-content! step-ring next))))

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
            [e* (map (lambda (x) (compile-expr x env #f #f)) e*)])
        (if ae
            (let-values ([(src expr) (get-src/expr ae)])
              (lambda (env)
                (let ([e (e env)] [e* (map (lambda (e) (e env)) e*)])
                  (mark-reduction! (make-trace src expr env e e* (id)))
                  (apply e e*))))
            (lambda (env)
              (let ([e (e env)] [e* (map (lambda (e) (e env)) e*)])
                (mark-reduction! (make-trace #f #f env e e* (id)))
                (apply e e*))))))
    (define (compile-nontail-call ae e e* env ctxt)
      (let ([proc (compile-tail-call ae e e* env ctxt)])
        (lambda (env)
          (with-click (lambda () (proc env))))))
    (define (compile-let e e* env ctxt tail?)
      (define (build-let fmls e* body)
        (let ([e*
               (list->vector
                 (map (lambda (x e) (compile-expr e env x #f))
                      fmls e*))])
          (let-values ([(env _) (extend-env fmls env)])
            (let ([body (compile-expr body env ctxt tail?)])
              (lambda (env)
                (body (cons (vector-map (lambda (x) (x env)) e*) env)))))))
      (define (dispatch cls*)
        (define (try cls)
          (let ([fmls (car cls)] [body (cadr cls)])
            (and (list? fmls)
                 (= (length fmls) (length e*))
                 (build-let fmls e* body))))
        (and (pair? cls*) (or (try (car cls*)) (dispatch (cdr cls*)))))
      (and (pair? e)
           (case (car e)
             [(lambda) (dispatch (list (cdr e)))]
             [(case-lambda) (dispatch (cdr e))]
             [(annotated-case-lambda) (dispatch (cddr e))]
             [else #f])))
    (define (compile-call ae e e* env ctxt tail?)
      (or (compile-let e e* env ctxt tail?)
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

(define-struct trace (src expr env v v* id))

(define (print-trace x)
  (define (chop x)
    (if (> (string-length x) 60)
        (format "~a#..." (substring x 0 56))
        x))
  (let ([n (car x)] [x (cdr x)])
    (printf " [~a] ~s\n" n (trace-expr x))
    (let ([src (trace-src x)])
      (when (pair? src)
        (printf "     source: char ~a of ~a\n" (cdr src) (car src))))
    (printf "     operator: ~s\n" (trace-v x))
    (printf "     operands: ")
    (let ([ls (map (lambda (x) (format "~s" x)) (trace-v* x))])
      (if (< (apply + 1 (length ls) (map string-length ls)) 60)
          (write (trace-v* x))
          (begin
            (display "(")
            (let f ([a (car ls)] [ls (cdr ls)])
              (display (chop a))
              (if (null? ls)
                  (display ")")
                  (begin 
                    (display "\n                ") 
                    (f (car ls) (cdr ls))))))))
    (newline)))

(define (print-step x)
  (let ([n (car x)] [ls (cdr x)])
    (unless (null? ls)
      (printf "FRAME ~s:\n" n)
      (for-each print-trace (reverse ls)))))

(define (print-all-traces)
  (newline)
  (for-each print-step (reverse (get-traces))))

(define (guarded-start proc)
  (with-exception-handler
    (lambda (con)
      (print-all-traces)
      (let f ()
        (printf "continue? (y/n) ")
        (case (read)
          [(y) (void)]
          [(n) (raise-continuable con)]
          [else (f)])))
    (lambda () 
      (reset-ring)
      (proc))))

(define (start-repl)
  (display "Ikarus Interpreter\n\n")
  (new-cafe
    (lambda (x)
      (guarded-start
        (lambda ()
          (eval x (interaction-environment)))))))

(define (start-script script-name args)
  (command-line-arguments (cons script-name args))
  (guarded-start
    (lambda ()
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
