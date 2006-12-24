
;;; Changes:
;;; 6.5: handles letrec
;;; 6.1: adding case-lambda, dropping lambda
;;; 6.0: basic version working
;;;

;;; Expand : Scheme -> Core Scheme 
;;;
;;; <CS> ::= (quote datum)
;;;        | <gensym>
;;;        | (if <CS> <CS> <CS>)
;;;        | (set! <gensym> <CS>)
;;;        | (begin <CS> <CS> ...)
;;;        | (case-lambda (<FML> <CS>) (<FML> <CS>) ...)
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
             (error 'apply1 "incorrect number of arguments to procedure"))]
        [(fx= i j)
         (error 'apply2 "incorrect number of arguments to procedure")]
        [else
         (vector-set! v i (car ls))
         (whack-proper v (cdr ls) (fxadd1 i) j)])))
  ;;;
  (define whack-improper
    (lambda (v ls i j)
      (cond
        [(fx= i j) (vector-set! v i ls) v]
        [(null? ls) 
         (error 'apply3 "incorrect number of arguments to procedure")]
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
             [(eq? a 'letrec)
              (let ([bind* (car d)] [body* (cdr d)])
                (if (null? bind*)
                    (C*->last (car body*) (cdr body*) env)
                    (let ([lhs* (map car bind*)] [rhs* (map cadr bind*)])
                      (let ([env (extend-env lhs* env)])
                        (let ([body* (C*->last (car body*) (cdr body*) env)]
                              [rhs* (C*->list (car rhs*) (cdr rhs*) env)]
                              [n (length lhs*)])
                          (lambda (renv)
                            (let ([v (make-vector n)])
                              (let ([renv (cons v renv)])
                                (let f ([i 0] [ls (rhs* renv)])
                                  (if (null? ls)
                                      (body* renv)
                                      (begin
                                        (vector-set! v i (car ls))
                                        (f (fxadd1 i) (cdr ls))))))))
                          )))))]
             [(eq? a 'case-lambda)
              (unless (fx>= (length d) 1) (syntax-error x))
              (let ()
                (define generate
                  (lambda (d)
                    (cond
                      [(null? d) 
                       (lambda (n args renv)
                         (error 'apply 
                                "incorrect number of arguments ~s to procedure"
                                n))]
                      [else
                       (let ([k (generate (cdr d))]
                             [a (car d)])
                         (let ([fml (car a)] [body* (cdr a)])
                           (let ([env (extend-env fml env)]
                                 [n (fml-length fml x)])
                             (let ([body*
                                    (C*->last (car body*) (cdr body*) env)])
                               (if (list? fml)
                                   (lambda (m args renv)
                                     (if (fx= n m)
                                         (body* (cons (list->vector args) renv))
                                         (k m args renv)))
                                   (let ([q (fxsub1 n)])
                                     (lambda (m args renv)
                                       (if (fx>= m q)
                                         (let ([v (make-vector n)])
                                           (let f ([i 0] [args args])
                                             (cond
                                               [(fx= i q)
                                                (vector-set! v q args)]
                                               [else
                                                (vector-set! v i (car args))
                                                (f (fxadd1 i) (cdr args))]))
                                           (body* (cons v renv)))
                                         (k m args renv)))))))))])))
                (let ([dispatch (generate d)])
                  (lambda (renv)
                    (lambda args
                      (dispatch (length args) args renv)))))]
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
             [(memq a '(set-top-level-value!))
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
             ;;; [(eq? a '$pcb-set!)
             ;;;  (unless (fx= (length d) 2) (syntax-error x))
             ;;;  (let ([sym (car d)] [val (C (cadr d) env)])
             ;;;    (unless (symbol? sym) (syntax-error x))
             ;;;    (lambda (renv) 
             ;;;      (set-top-level-value! sym (val renv))))] 
             [(eq? a '|#primitive|)
              (unless (fx= (length d) 1) (syntax-error x))
              (let ([sym (car d)])
                (let ([prim (primitive-ref sym)])
                  (if (procedure? prim)
                      (lambda (renv) prim)
                      (syntax-error x))))]
             [(memq a '(foreign-call $apply))
              (error 'interpret "~a form is not supported" a)]
         ;;; [else
         ;;;  (let ([rator (C a env)] [n (length d)])
         ;;;    (cond 
         ;;;      [(fx= n 0) 
         ;;;       (lambda (renv)
         ;;;         (let ([p (rator renv)])
         ;;;           (p)))]
         ;;;      [(fx= n 1) 
         ;;;       (let ([arg1 (C (car d) env)])
         ;;;         (lambda (renv)
         ;;;           (let ([p (rator renv)])
         ;;;             (p (arg1 renv)))))]
         ;;;      [(fx= n 2)
         ;;;       (let ([arg1 (C (car d) env)]
         ;;;             [arg2 (C (cadr d) env)])
         ;;;         (lambda (renv)
         ;;;           (let ([p (rator renv)])
         ;;;             (p (arg1 renv) (arg2 renv)))))]
         ;;;      [else
         ;;;       (let ([arg* (C*->list (car d) (cdr d) env)])
         ;;;         (lambda (renv)
         ;;;           (apply (rator renv) (arg* renv))))]))]
             [else
              (let ([rator (C a env)] [n (length d)])
                (cond 
                  [(fx= n 0) 
                   (lambda (renv)
                     (apply (rator renv) '()))]
                  ;[(fx= n 1) 
                  ; (let ([arg1 (C (car d) env)])
                  ;   (lambda (renv)
                  ;     ((rator renv) (arg1 renv))))]
                  ;[(fx= n 2)
                  ; (let ([arg1 (C (car d) env)]
                  ;       [arg2 (C (cadr d) env)])
                  ;   (lambda (renv)
                  ;     ((rator renv) (arg1 renv) (arg2 renv))))]
                  [else
                   (let ([arg* (C*->list (car d) (cdr d) env)])
                     (lambda (renv)
                       (apply (rator renv) (arg* renv))))]))]
             
             ))]
        [else (syntax-error x)])))
  ;;;
  ;(primitive-set! 'interpret
  ;  (lambda (x)
  ;    (let ([x (expand x)])
  ;      (let ([p (C x '())])
  ;        (p '())))))
  ;;;
  (primitive-set! 'current-eval
    (make-parameter 
      interpret
      (lambda (f)
        (unless (procedure? f)
          (error 'current-eval "~s is not a procedure" f))
        f)))
  ;;;
  (primitive-set! 'eval
    (lambda (x)
      ((current-eval) x))))

