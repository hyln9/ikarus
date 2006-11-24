
;;; Expand : Scheme -> Core Scheme 
;;;
;;; <CS> ::= (quote datum)
;;;        | <gensym>
;;;        | (if <CS> <CS> <CS>)
;;;        | (set! <gensym> <CS>)
;;;        | (begin <CS> <CS> ...)
;;;        | (lambda <FMLS> <CS> <CS> ...)
;;;        | (<prim> <CS> <CS> ...)
;;;        | (primref <primname>)
;;;        | (<CS> <CS> ...)
;;; <FML> ::= ()
;;;         | <gensym>
;;;         | (<gensym> . <FML>)
;;; <prim> ::= void | memv | top-level-value | set-top-level-value!
;;;


(let ()
  (define syntax-error
    (lambda (x)
      (error 'interpret "invalid syntax ~s" x)))
  ;;;
  (define C*->last
    (lambda (a d env)
      (cond
        [(null? d) (C a env)]
        [else
         (let ([a (C a env)]
               [d (C*->last (car d) (cdr d) env)])
           (lambda (renv)
             (a renv)
             (d renv)))])))
  ;;;
  (define C*->list
    (lambda (a d env)
      (cond
        [(null? d) 
         (let ([a (C a env)])
           (lambda (renv)
             (list (a renv))))]
        [else
         (let ([a (C a env)]
               [d (C*->list (car d) (cdr d) env)])
           (lambda (renv)
             (cons (a renv) (d renv))))])))
  ;;;
  (define extend-env
    (lambda (fml* env)
      (cons fml* env)))
  ;;;
  (define fml-length
    (lambda (fml* x)
      (cond
        [(pair? fml*) (fxadd1 (fml-length (cdr fml*) x))]
        [(null? fml*) 0]
        [(symbol? fml*) 1]
        [else (syntax-error x)])))
  ;;;
  (define whack-proper
    (lambda (v ls i j)
      (cond
        [(null? ls) 
         (if (fx= i j)
             v
             (error 'apply "incorrect number of arguments to procedure"))]
        [(fx= i j)
         (error 'apply "incorrect number of arguments to procedure")]
        [else
         (vector-set! v i (car ls))
         (whack-proper v (cdr ls) (fxadd1 i) j)])))
  ;;;
  (define whack-improper
    (lambda (v ls i j)
      (cond
        [(fx= i j) (vector-set! v i ls) v]
        [(null? ls) 
         (error 'apply "incorrect number of arguments to procedure")]
        [else
         (vector-set! v i (car ls))
         (whack-improper v (cdr ls) (fxadd1 i) j)])))
  ;;;
  (define lookup
    (lambda (x env)
      (define Lj
        (lambda (x fml* j)
          (cond
            [(pair? fml*) 
             (if (eq? (car fml*) x)
                 j
                 (Lj x (cdr fml*) (fxadd1 j)))]
            [(eq? x fml*) j]
            [else #f])))
      (define Li
        (lambda (x env i)
          (cond
            [(null? env) #f]
            [(Lj x (car env) 0) =>
             (lambda (j)
               (cons i j))]
            [else (Li x (cdr env) (fxadd1 i))])))
      (Li x env 0)))
  ;;;
  (define C
    (lambda (x env)
      (cond
        [(gensym? x) 
         (cond
           [(lookup x env) =>
            (lambda (b)
              (let ([i (car b)] [j (cdr b)])
                (lambda (renv)
                  (vector-ref (list-ref renv i) j))))]
           [else (syntax-error x)])]
        [(pair? x)
         (let ([a (car x)] [d (cdr x)])
           (unless (list? d) (syntax-error x))
           (cond 
             [(eq? a 'quote) 
              (unless (fx= (length d) 1) (syntax-error x))
              (let ([v (car d)])
                (lambda (renv) v))]
             [(eq? a 'if) 
              (unless (fx= (length d) 3) (syntax-error x))
              (let ([test   (C (car d) env)] 
                    [conseq (C (cadr d) env)]
                    [altern (C (caddr d) env)])
                (lambda (renv)
                  (if (test renv)
                      (conseq renv)
                      (altern renv))))]
             [(eq? a 'set!)
              (unless (fx= (length d) 2) (syntax-error x))
              (let ([var (car d)] [val (C (cadr d) env)])
                (cond
                  [(lookup var env) =>
                   (lambda (b)
                     (let ([i (car b)] [j (cdr b)])
                       (lambda (renv)
                         (vector-set! (list-ref renv i) j (val renv)))))]
                  [else (syntax-error x)]))]
             [(eq? a 'begin)
              (unless (fx>= (length d) 1) (syntax-error x))
              (C*->last (car d) (cdr d) env)]
             [(eq? a 'lambda)
              (unless (fx>= (length d) 2) (syntax-error x))
              (let ([fml* (car d)] [body* (cdr d)])
                (let ([env (extend-env fml* env)]
                      [n (fml-length fml* x)])
                  (let ([body* (C*->last (car body*) (cdr body*) env)])
                    (if (list? fml*)
                        (lambda (renv)
                          (lambda args
                            (body* 
                              (cons (whack-proper (make-vector n) args 0 n)
                                    renv))))
                        (lambda (renv)
                          (lambda args
                            (body*
                              (cons 
                                (whack-improper (make-vector n) args 0 (fxsub1 n))
                                renv))))))))]
             [(eq? a 'void)
              (unless (fx= (length d) 0) (syntax-error x))
              (lambda (renv) (void))]
             [(eq? a 'memv)
              (unless (fx= (length d) 2) (syntax-error x))
              (let ([val (C (car d) env)] [list (C (cadr d) env)])
                (lambda (renv)
                  (memq (val renv) (list renv))))]
             [(eq? a 'top-level-value)
              (unless (fx= (length d) 1) (syntax-error x))
              (let ([qsym (car d)])
                (unless (and (pair? qsym)
                             (fx= (length qsym) 2)
                             (eq? (car qsym) 'quote)
                             (symbol? (cadr qsym)))
                  (syntax-error x))
                (let ([sym (cadr qsym)])
                  (if (top-level-bound? sym)
                      (lambda (renv) 
                        (top-level-value sym))
                      (lambda (renv)
                        (if (top-level-bound? sym)
                            (top-level-value sym)
                            (error #f "~s is unbound" sym))))))]
             [(eq? a 'set-top-level-value!)
              (unless (fx= (length d) 2) (syntax-error x))
              (let ([qsym (car d)] [val (C (cadr d) env)])
                (unless (and (pair? qsym)
                             (fx= (length qsym) 2)
                             (eq? (car qsym) 'quote)
                             (symbol? (cadr qsym)))
                  (syntax-error x))
                (let ([sym (cadr qsym)])
                  (lambda (renv) 
                    (set-top-level-value! sym (val renv)))))]
             [(eq? a '|#primitive|)
              (unless (fx= (length d) 1) (syntax-error x))
              (let ([sym (car d)])
                (let ([prim (primitive sym)])
                  (if (procedure? prim)
                      (lambda (renv) prim)
                      (syntax-error x))))]
             [(memq a '(foreign-call $apply $pcb-set!))
              (error 'interpret "~a form is not supported" a)]
             [else
              (let ([rator (C a env)] [n (length d)])
                (cond 
                  [(fx= n 0) 
                   (lambda (renv)
                     ((rator renv)))]
                  [(fx= n 1) 
                   (let ([arg1 (C (car d) env)])
                     (lambda (renv)
                       ((rator renv) (arg1 renv))))]
                  [(fx= n 2)
                   (let ([arg1 (C (car d) env)]
                         [arg2 (C (cadr d) env)])
                     (lambda (renv)
                       ((rator renv) (arg1 renv) (arg2 renv))))]
                  [else
                   (let ([arg* (C*->list (car d) (cdr d) env)])
                     (lambda (renv)
                       (apply (rator renv) (arg* renv))))]))]))]
        [else (syntax-error x)])))
  ;;;
  ($pcb-set! interpret
    (lambda (x)
      (let ([x (expand x)])
        (let ([p (C x '())])
          (p '())))))
  ;;;
  ($pcb-set! current-eval
    (make-parameter 
      interpret
      (lambda (f)
        (unless (procedure? f)
          (error 'current-eval "~s is not a procedure" f))
        f)))
  ;;;
  ($pcb-set! eval
    (lambda (x)
      ((current-eval) x))))

