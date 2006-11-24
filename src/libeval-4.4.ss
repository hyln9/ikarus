
(let ()
  (define verify-proper-lambda-bindings
    (lambda (fml*)
      (void)))
  (define keyword?
    (lambda (x)
      (memq x 
        '(lambda let let* letrec letrec* if quote when unless
          set! begin define or and cond))))
  (define self-evaluating?
    (lambda (x)
      (or (fixnum? x) (null? x) (boolean? x) (char? x) (string? x))))
  (define extend-r
    (lambda (fml* r)
      (cons fml* r)))
  (define classify
    (lambda (fml* k)
      (let f ([fml* fml*] [i 0])
        (cond
          [(null? fml*) (k i #t)]
          [(pair? fml*) (f (cdr fml*) (fx+ i 1))]
          [else (k i #f)]))))
  (define compile-lambda-binder
    (lambda (fml*)
      (classify fml*
        (lambda (len proper?)
          (if proper?
              (lambda (args)
                (let ([v (make-vector len)])
                  (let f ([i 0] [args args])
                    (cond
                      [(fx= i len) 
                       (if (null? args)
                           v
                           (error 'apply
                             "incorrect number of args to procedure"))]
                      [(pair? args)
                       (vector-set! v i (car args))
                       (f (fx+ i 1) (cdr args))]
                      [else (error 'apply "insufficient arguments")]))))
              (lambda (args)
                (let ([v (make-vector (fx+ len 1))])
                  (let f ([i 0] [args args])
                    (cond
                      [(fx= i len) 
                       (vector-set! v i args)
                       v]
                      [(pair? args)
                       (vector-set! v i (car args))
                       (f (fx+ i 1) (cdr args))]
                      [else
                       (error 'apply "insufficient arguments")])))))))))
  (define compile-lambda
    (lambda (body r x)
      (unless (fx>= (length body) 2)
        (error 'eval "invalid function definition ~s" x))
      (let ([fml* (car body)] [body* (cdr body)])
        (verify-proper-lambda-bindings fml*)
        (let ([r (extend-r fml* r)]
              [ext (compile-lambda-binder fml*)])
          (let ([body (compile-internal body* r x)])
            (lambda (env)
              (lambda args
                (body (cons (ext args) env)))))))))
  (define compile-if
    (lambda (body r x)
      (unless (fx= (length body) 3)
        (error 'eval "invalid syntax ~s" x))
      (let ([test (compile-expr (car body) r)]
            [conseq (compile-expr (cadr body) r)]
            [altern (compile-expr (caddr body) r)])
        (lambda (env)
          (if (test env)
              (conseq env)
              (altern env))))))
  (define compile-when
    (lambda (body r x)
      (unless (fx>= (length body) 2)
        (error 'eval "invalid syntax ~s" x))
      (let ([test (compile-expr (car body) r)]
            [conseq (compile-expr*->last (cdr body) r)])
        (lambda (env)
          (when (test env)
            (conseq env))))))
  (define compile-unless
    (lambda (body r x)
      (unless (fx>= (length body) 2)
        (error 'eval "invalid syntax ~s" x))
      (let ([test (compile-expr (car body) r)]
            [altern (compile-expr*->last (cdr body) r)])
        (lambda (env)
          (unless (test env)
            (altern env))))))
  (define compile-quote
    (lambda (body x)
      (unless (fx= (length body) 1)
        (error 'eval "invalid quote expression ~s" x))
      (let ([v (car body)])
        (lambda (env) v))))
  (define compile-form
    (lambda (k body r x)
      (cond
        [(eq? k 'quote) (compile-quote body x)]
        [(eq? k 'lambda) (compile-lambda body r x)]
        [(eq? k 'let) (compile-let body r x)]
        [(eq? k 'if) (compile-if body r x)]
        [(eq? k 'let*) (compile-let* body r x)]
        [(eq? k 'letrec) (compile-letrec body r x)]
        [(eq? k 'letrec*) (compile-letrec* body r x)]
        [(eq? k 'set!) (compile-assign body r x)]
        [(eq? k 'begin) (compile-begin body r x)]
        [(eq? k 'or) (compile-or body r x)]
        [(eq? k 'and) (compile-and body r x)]
        [(eq? k 'cond) (compile-cond body r x)]
        [(eq? k 'when) (compile-when body r x)]
        [(eq? k 'unless) (compile-unless body r x)]
        [(eq? k 'define) 
         (error 'eval "invalid definition in expression context in ~s" x)]
        [else (error 'eval "unhandled keyword ~s" k)])))
  (define compile-one-clause
    (lambda (cls r x rest)
      (unless (and (pair? cls) (list? cls))
        (error 'eval "invalid cond clause ~s" cls))
      (let ([len (length cls)])
        (cond
          [(fx= len 1)
           (let ([q (compile-expr (car cls) r)])
             (lambda (env)
               (let ([t (q env)])
                 (if t t (rest env)))))]
          [(and (fx= len 3) (eq? (cadr cls) '=>) (special? '=> r))
           (let ([q (compile-expr (car cls) r)]
                 [f (compile-expr (caddr cls) r)])
             (lambda (env)
               (let ([t (q env)])
                 (if t ((f env) t) (rest env)))))]
          [else
           (let ([q (compile-expr (car cls) r)]
                 [d (compile-expr*->last (cdr cls) r)])
             (lambda (env)
               (if (q env)
                   (d env)
                   (rest env))))]))))
  (define compile-last-cond-clause
    (lambda (cls r x)
      (unless (and (pair? cls) (list? cls))
        (error 'eval "invalid syntax ~s" x))
      (cond
        [(and (eq? (car cls) 'else) (special? 'else r))
         (when (null? (cdr cls))
            (error 'eval "invalid syntax ~s" x))
         (compile-expr*->last (cdr cls) r)]
        [else
         (compile-one-clause cls r x
           (lambda (env) #f))])))
  (define compile-cond
    (lambda (cls* r x)
      (cond
        [(null? cls*) (lambda (env) #f)]
        [(null? (cdr cls*))
         (compile-last-cond-clause (car cls*) r x)]
        [else
         (compile-one-clause (car cls*) r x
           (compile-cond (cdr cls*) r x))])))
  (define compile-and
    (lambda (ls r x)
      (cond
        [(null? ls) (lambda (env) #t)]
        [(null? (cdr ls)) (compile-expr (car ls) r)]
        [else
         (let ([a (compile-expr (car ls) r)]
               [d (compile-and (cdr ls) r x)])
           (lambda (env)
             (and (a env) (d env))))])))
  (define compile-or
    (lambda (ls r x)
      (cond
        [(null? ls) (lambda (env) #f)]
        [(null? (cdr ls)) (compile-expr (car ls) r)]
        [else
         (let ([a (compile-expr (car ls) r)]
               [d (compile-or (cdr ls) r x)])
           (lambda (env)
             (or (a env) (d env))))])))
  (define compile-begin
    (lambda (body r x)
      (unless (pair? body) (error 'eval "invalid expression ~s" x))
      (compile-expr*->last body r)))
  (define compile-expr*->last 
    (lambda (body* r)
      (let f ([a (car body*)] [d (cdr body*)])
        (cond
          [(null? d) (compile-expr a r)]
          [else
           (let ([a (compile-expr a r)])
            (let ([d (compile-expr*->last d r)])
              (lambda (env) (a env) (d env))))]))))
  (define compile-expr*->assign 
    (lambda (body* r)
      (let f ([i 0] [a (car body*)] [d (cdr body*)])
        (cond
          [(null? d)
           (let ([v (compile-expr a r)])
             (lambda (env)
               (vector-set! (car env) i (v env))))]
          [else
           (let ([v (compile-expr a r)]
                 [d (f (fxadd1 i) (car d) (cdr d))])
             (lambda (env)
               (vector-set! (car env) i (v env))
               (d env)))]))))
  (define vector-assign!
    (lambda (v i ls)
      (unless (null? ls)
        (vector-set! v i (car ls))
        (vector-assign! v (fxadd1 i) (cdr ls)))))
  (define build-letrec
    (lambda (lhs* rhs* r body*)
      (cond
        [(null? lhs*) (compile-expr*->last body* r)]
        [else
         (let ([r (extend-r lhs* r)])
           (let ([rhs* (compile-expr*->list rhs* r)]
                 [body (compile-expr*->last body* r)]
                 [n (length lhs*)]) ;?
             (lambda (env) 
               (let ([v (make-vector n #f)])
                 (let ([env (cons v env)])
                   (vector-assign! v 0 (rhs* env))
                   (body env))))))])))
  (define verify-bindings
    (lambda (bind* x)
      (unless (and (list? bind*)
                   (andmap
                     (lambda (x) 
                       (and (list? x) (fx= (length x) 2) (symbol? (car x))))
                     bind*))
        (error 'eval "invalid bindings in ~s" x))))
  (define compile-letrec
    (lambda (body r x)
      (unless (fx>= (length body) 2)
        (error 'eval "invalid syntax ~s" x))
      (let ([bind* (car body)] [body* (cdr body)])
        (build-letrec (map car bind*) (map cadr bind*) r body*))))
  (define compile-letrec*
    (lambda (body r x)
      (unless (fx>= (length body) 2)
        (error 'eval "invalid syntax ~s" x))
      (let ([bind* (car body)] [body* (cdr body)])
        (verify-bindings bind* x)
        (if (null? bind*)
            (compile-internal body* r x)
            (let ([r (extend-r (map car bind*) r)])
              (let ([rhs* (compile-expr*->assign (map cadr bind*) r)])
                (let ([body (compile-internal body* r x)]
                      [n (length bind*)])
                  (lambda (env)
                    (let ([env (cons (make-vector n #f) env)])
                      (rhs* env)
                      (body env))))))))))
  (define compile-let
    (lambda (body r x)
      (unless (fx>= (length body) 2)
        (error 'eval "invalid syntax ~s" x))
      (let ([bind* (car body)] [body* (cdr body)])
        (verify-bindings bind* x)
        (if (null? bind*)
            (compile-internal body* r x)
            (let ([rhs* (compile-expr*->list (map cadr bind*) r)])
              (let ([r (extend-r (map car bind*) r)])
                (let ([body (compile-internal body* r x)])
                  (lambda (env)
                    (body (cons (list->vector (rhs* env)) env))))))))))
  (define compile-let*
    (lambda (body r x)
      (unless (fx>= (length body) 2)
        (error 'eval "invalid syntax ~s" x))
      (let ([bind* (car body)] [body* (cdr body)])
        (verify-bindings bind* x)
        (let f ([bind* bind*] [r r])
          (cond
            [(null? bind*) (compile-internal body* r x)]
            [else
             (let ([b (car bind*)])
               (let ([lhs (car b)] [rhs (cadr b)])
                 (let ([rhs (compile-expr rhs r)])
                   (let ([r (extend-r (list lhs) r)])
                     (let ([rest (f (cdr bind* r))])
                       (lambda (env)
                         (let ([env (cons (vector (rhs env)) env)])
                           (rest env))))))))])))))
  (define compile-expr*->list 
    (lambda (expr* r)
      (when (null? expr*) 
        (error 'eval "this should nto happen"))
      (let f ([a (car expr*)] [d (cdr expr*)])
        (cond
          [(null? d) 
           (let ([a (compile-expr a r)])
             (lambda (env)
               (cons (a env) '())))]
          [else
           (let ([a (compile-expr a r)]
                 [d (f (car d) (cdr d))])
             (lambda (env)
               (cons (a env) (d env))))]))))
  (define compile-internal-aux
    (lambda (x* r x lhs* rhs*)
      (when (null? x*)
        (error 'eval "no body in ~s" x))
      (let ([a (car x*)] [d (cdr x*)])
        (cond
          [(and (pair? a)
                (eq? (car a) 'define)
                (special? 'define r)
                (not (memq 'define lhs*)))
           (unless (and (list? a) (fx= (length a) 3))
             (error 'eval "invalid syntax ~s" a))
           (let ([lhs (cadr a)] [rhs (caddr a)])
             (unless (symbol? lhs) 
               (error 'eval "invalid id ~s in ~s" lhs x))
             (when (memq lhs lhs*)
               (error 'eval "duplicate definition for ~s in ~s ~s" lhs lhs* x))
             (compile-internal-aux d r x 
               (cons lhs lhs*) (cons rhs rhs*)))]
          [(and (pair? a) 
                (eq? (car a) 'begin)
                (special? 'begin r)
                (not (memq 'begin lhs*)))
           (let ([rest (cdr a)])
             (unless (list? rest)
               (error 'eval "invalid begin syntax ~s" a))
             (compile-internal-aux (append rest d) r x lhs* rhs*))]
          [else
           (build-letrec (reverse lhs*) (reverse rhs*) r x*)]))))
  (define special? 
    (lambda (x r)
      (cond
        [(top-level-bound? x) #f]
        [(lookup x r) #f]
        [else #t])))
  (define compile-internal 
    (lambda (x* r x)
      (compile-internal-aux x* r x '() '())))
  (define lookup
    (lambda (x r)
      (let f ([r r] [i 0])
        (cond
          [(null? r) #f]
          [else
           (or (let f ([ls (car r)] [j 0])
                 (cond
                   [(null? ls) #f]
                   [(pair? ls)
                    (if (eq? (car ls) x)
                        (cons i j)
                        (f (cdr ls) (fx+ j 1)))]
                   [(eq? ls x) (cons i j)]
                   [else #f]))
             (f (cdr r) (fx+ i 1)))]))))
  (define compile-assign
    (lambda (body r x)
      (unless (fx= (length body) 2)
        (error 'eval "invalid assignment ~s" x))
      (unless (symbol? (car body))
        (error 'eval "invalid syntax ~s" x))
      (let ([val (compile-expr (cadr body) r)]
            [var (car body)])
        (cond
          [(lookup var r) =>
           (lambda (p)
             (build-lexical-assignment p val))]
          [(top-level-bound? var)
           (lambda (env)
             (set-top-level-value! var (val env)))]
          [(keyword? var)
           (error 'eval "invalid assignment to keyword in ~s" x)]
          [else
           (lambda (env)
             (set-top-level-value! var (val env)))]))))
  (define list-ref
    (lambda (ls i)
      (cond
        [(null? ls) (error 'list-ref "index out of range")]
        [(fxzero? i) (car ls)]
        [else (list-ref (cdr ls) (fx- i 1))])))
  (define build-lexical-assignment
    (lambda (p val)
      (lambda (env)
        (vector-set! (list-ref env (car p)) (cdr p) (val env)))))
  (define build-lexical-reference
    (lambda (p)
      (lambda (env)
        (vector-ref (list-ref env (car p)) (cdr p)))))
  (define compile-expr
    (lambda (x r)
      (cond
        [(self-evaluating? x) (lambda (env) x)]
        [(symbol? x) 
         (cond
           [(lookup x r) => build-lexical-reference]
           [(top-level-bound? x) 
            (lambda (env) (top-level-value x))]
           [(keyword? x) (error 'eval "invalid reference to keyword ~s" x)]
           [else
            (lambda (env)
              (if (top-level-bound? x)
                  (top-level-value x)
                  (error 'eval "reference to unbound variable ~s" x)))])]
        [(not (list? x)) (error 'eval "invalid expression ~s" x)]
        [(and (symbol? (car x)) (keyword? (car x)) (special? (car x) r))
         (compile-form (car x) (cdr x) r x)]
        [else
         (let ([op (compile-expr (car x) r)]
               [rand* (cdr x)]
               [n (length (cdr x))])
           (cond
             [(fx= n 0)
              (lambda (env) ((op env)))]
             [(fx= n 1)
              (let ([r1 (compile-expr (car rand*) r)])
                (lambda (env)
                  ((op env) (r1 env))))]
             [(fx= n 2)
              (let ([r1 (compile-expr (car rand*) r)]
                    [r2 (compile-expr (cadr rand*) r)])
                (lambda (env)
                  ((op env) (r1 env) (r2 env))))] 
             [(fx= n 3)
              (let ([r1 (compile-expr (car rand*) r)]
                    [r2 (compile-expr (cadr rand*) r)]
                    [r3 (compile-expr (caddr rand*) r)])
                (lambda (env)
                  ((op env) (r1 env) (r2 env) (r3 env))))] 
             [(fx= n 4)
              (let ([r1 (compile-expr (car rand*) r)]
                    [r2 (compile-expr (cadr rand*) r)]
                    [r3 (compile-expr (caddr rand*) r)]
                    [r4 (compile-expr (cadddr rand*) r)])
                (lambda (env)
                  ((op env) (r1 env) (r2 env) (r3 env) (r4 env))))]
             [else 
              (let ([r1 (compile-expr (car rand*) r)]
                    [r2 (compile-expr (cadr rand*) r)]
                    [r3 (compile-expr (caddr rand*) r)]
                    [r4 (compile-expr (cadddr rand*) r)]
                    [r* (compile-expr*->list (cddddr rand*) r)])
                (lambda (env)
                  ($apply (op env) (r1 env) (r2 env) (r3 env) (r4 env) 
                          (r* env))))]))])))
  (define eval-top
    (lambda (x)
      (cond
       [(and (pair? x) (eq? (car x) 'define) (not (top-level-bound? 'define)))
        (unless (and (list? x) (fx= (length x) 3))
          (error 'eval "invalid syntax ~s" x))
        (let ([var (cadr x)] [val (caddr x)])
          (unless (symbol? var) (error 'eval "invalid syntax ~s" x))
          (let ([val (compile-expr val '())])
            (set-top-level-value! var (val '()))))]
       [(and (pair? x) (eq? (car x) 'begin) (not (top-level-bound? 'begin)))
        (unless (list? x) 
          (error 'eval "invalid syntax ~s" x))
        (letrec ([f
                  (lambda (x x*)
                    (if (null? x*)
                        (eval-top x)
                        (begin
                          (eval-top x)
                          (f (car x*) (cdr x*)))))])
          (let ([d (cdr x)])
            (unless (null? d)
              (f (car d) (cdr d)))))]
       [else
        ((compile-expr x '()) '())])))
  ($pcb-set! eval eval-top))


($pcb-set! current-eval 
  (make-parameter eval
    (lambda (f)
      (unless (procedure? f)
        (error 'current-eval "not a procedure ~s" f))
      f)))

(let ()
  (define read-and-eval
    (lambda (p)
      (let ([x (read p)])
        (unless (eof-object? x)
          ((current-eval) x)
          (read-and-eval p)))))
  ($pcb-set! load
    (lambda (x)
      (unless (string? x)
        (error 'load "~s is not a string" x))
      (let ([p (open-input-file x)])
        (read-and-eval p)
        (close-input-port p)))))

#!eof
        
(define test-suite
  (lambda (x)
    (printf "performing ~a tests\n" (car x))
    (for-each
      (lambda (t)
        (define x (car t))
        (write x)
        (newline)
        (unless (equal? (caddr t) "")
          (let ([v (eval x)] [w (interpret x)])
            (unless (equal? v w)
              (error #f "got ~s, should be ~s" v w)))))
      (cdr x))))
        
(define test-file
  (lambda (x)
    (with-input-from-file x
      (lambda ()
        (let f ()
          (let ([x (read)])
            (unless (eof-object? x)
              (test-suite (cdr x))
              (f))))))))
              
(define fxadd1 (lambda (n) (fx+ n 1)))
(define fxsub1 (lambda (n) (fx- n 1)))
(define fixnum->char integer->char)
(define char->fixnum char->integer)
(define $apply apply)
(define char= char=?)
(define char< char<?)
(define char<= char<=?)
(define char> char>?)
(define char>= char>=?)

  
(for-each 
   test-file
   '("tests-1.1-req.scm"
     "tests-1.2-req.scm"
     "tests-1.3-req.scm"
     "tests-1.4-req.scm"
     "tests-1.5-req.scm"
     "tests-1.6-req.scm"
     "tests-1.7-req.scm"
     "tests-1.8-req.scm"
     "tests-1.9-req.scm"
     "tests-2.1-req.scm"
     "tests-2.2-req.scm"
     "tests-2.3-req.scm"
     "tests-2.4-req.scm"
     "tests-2.6-req.scm"
     "tests-2.8-req.scm"
     "tests-2.9-req.scm"
     "tests-3.1-req.scm"
     "tests-3.2-req.scm"
     "tests-3.3-req.scm"
     "tests-3.4-req.scm"
     "tests-4.1-req.scm"
     "tests-4.2-req.scm"
     "tests-4.3-req.scm"))
