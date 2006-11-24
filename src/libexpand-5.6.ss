
;;; Extended: cond case  

;;;
;;;
;;; Expand : Scheme -> Core Scheme 
;;;
;;; <CS> ::= (quote datum)
;;;        | <gensym>
;;;        | (if <CS> <CS> <CS>)
;;;        | (set! <gensym> <CS>)
;;;        | (begin <CS> <CS> ...)
;;;        | (letrec ([<gensym> <CS>] ...) <CS> <CS> ...)
;;;        | (lambda <FMLS> <CS> <CS> ...)
;;;        | (<prim> <CS> <CS> ...)
;;;        | (<CS> <CS> ...)
;;; <FML> ::= ()
;;;         | <gensym>
;;;         | (<gensym> . <FML>)
;;; <prim> ::= void | memv | top-level-value | set-top-level-value! 
;;;          | $pcb-set! | foreign-call | $apply
;;; 
;;;
;;; Handled keywords:
;;; Core:     lambda set! if quote begin define
;;; Extended: let let* letrec letrec* when unless or and cond case  



(let ()
  (define *keyword* (gensym "*keyword*"))
  (define build-void
    (lambda ()
      '(void)))
  (define build-primref
    (lambda (x)
      x))
  (define build-global-assignment
    (lambda (x val)
      (list 'set-top-level-value! (build-constant x) val)))
  (define build-pcb-set!
    (lambda (x val)
      (list '$pcb-set! x val)))
  (define build-foreign-call
    (lambda (name rand*)
      (cons 'foreign-call
            (cons name rand*))))
  (define build-apply
    (lambda (proc rand*)
      (cons '$apply
            (cons proc rand*))))
  (define build-global-reference
    (lambda (x)
      (list 'top-level-value (build-constant x))))
  (define build-memv
    (lambda (x ls)
      (list 'memv x ls)))
  (define build-application
    (lambda (fun arg*)
      (cons fun arg*)))
  (define build-sequence
    (lambda (a b)
      (let ([a* 
             (if (and (pair? a) (eq? (car a) 'begin))
                 (cdr a)
                 (list a))]
            [b*
             (if (and (pair? b) (eq? (car b) 'begin))
                 (cdr b)
                 (list b))])
        (cons 'begin (append a* b*)))))
  (define build-constant
    (lambda (x) (list 'quote x)))
  (define build-lexical-reference
    (lambda (x) x))
  (define build-lexical-assignment
    (lambda (lhs rhs)
      (list 'set! lhs rhs)))
  (define build-conditional
    (lambda (test conseq altern)
      (list 'if test conseq altern)))
  (define build-function
    (lambda (fml* body)
      (list 'lambda fml* body)))
  (define build-assignments
    (lambda (lhs* rhs* body)
      (cond
        [(null? lhs*) body]
        [else
         (build-sequence 
           (build-lexical-assignment (car lhs*) (car rhs*))
           (build-assignments (cdr lhs*) (cdr rhs*) body))])))
  (define build-letrec
    (lambda (lhs* rhs* body)
      (if (null? lhs*)
          body
          (let ([g* (map (lambda (x) (gensym)) lhs*)])
            (build-let lhs* (map (lambda (x) (build-void)) lhs*)
              (build-let g* rhs*
                (build-assignments lhs* g* body)))))))
  (define build-letrec*
    (lambda (lhs* rhs* body)
      (if (null? lhs*)
          body
          (build-let lhs* (map (lambda (x) (build-void)) lhs*)
            (build-assignments lhs* rhs* body)))))
  (define build-let
    (lambda (lhs* rhs* body)
      (build-application
        (build-function lhs* body)
        rhs*)))
  (define build-let*
    (lambda (lhs* rhs* body)
      (cond
        [(null? lhs*) body]
        [else
         (build-let (list (car lhs*)) (list (car rhs*))
           (build-let* (cdr lhs*) (cdr rhs*) body))])))
  ;;; builds
  (define keyword?
    (lambda (x) (getprop x *keyword*)))
  (define self-evaluating?
    (lambda (x)
      (or (immediate? x) (string? x))))
  (define syntax-error 
    (lambda (x)
      (error 'expand "invalid syntax ~s" x)))
  (define empty-env '())
  (define E*
    (lambda (x* env)
      (cond
        [(null? x*) '()]
        [else
         (cons (E (car x*) env) (E* (cdr x*) env))])))
  (define lookup
    (lambda (x env)
      (cond
        [(assq x env) => cdr]
        [else #f])))
  (define bug
    (lambda (str . args)
      (error 'bug "~a ~a" str args)))
  ;;;
  (define E-quote
    (lambda (d env x)
      (unless (fx= (length d) 1) 
        (syntax-error x))
      (build-constant (car d))))
  ;;;
  (define E-if
    (lambda (d env x)
      (let ([n (length d)])
        (cond
          [(fx= n 2) 
           (build-conditional
             (E (car d) env)
             (E (cadr d) env)
             (build-void))]
          [(fx= n 3)
           (build-conditional
             (E (car d) env)
             (E (cadr d) env)
             (E (caddr d) env))]
          [else (syntax-error x)]))))
  ;;;
  (define E-set!
    (lambda (d env x)
      (unless (fx= (length d) 2) (syntax-error x))
      (let ([lhs (car d)] [rhs (cadr d)])
        (unless (symbol? lhs) (syntax-error x))
        (cond
          [(lookup lhs env) =>
           (lambda (b)
             (build-lexical-assignment b (E rhs env)))]
          [(keyword? lhs) (syntax-error x)]
          [else 
           (build-global-assignment lhs (E rhs env))]))))
  ;;;
  (define E-begin
    (lambda (d env x)
      (unless (fx>= (length d) 1)
        (syntax-error x))
      (E-begin^ (car d) (cdr d) env)))
  (define E-begin^
    (lambda (a d env)
      (cond
        [(null? d) (E a env)]
        [else
         (build-sequence
           (E a env)
           (E-begin^ (car d) (cdr d) env))])))
  ;;;
  (define E-named-let
    (lambda (name d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([bindings (car d)] [body* (cdr d)])
        (verify-bindings bindings x)
        (let ([lhs* (map car bindings)] 
              [rhs* (map cadr bindings)])
          (verify-fml* lhs* x)
          (let ([rator
                 (let ([gname (gensym)]
                       [nlhs* (map (lambda (x) (gensym)) lhs*)])
                   (let ([env
                          (extend-env-fml* lhs* nlhs* 
                              (cons (cons name gname) env))])
                     (let ([body (E-internal body* env x)])
                       (let ([fun (build-function nlhs* body)])
                         (build-letrec 
                           (list gname)
                           (list fun)
                           (build-lexical-reference gname))))))]
                [rand* (map (lambda (x) (E x env)) rhs*)])
            (build-application rator rand*))))))
  ;;;
  (define E-let
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([bindings (car d)] [body* (cdr d)])
        (cond
          [(symbol? bindings)
           (E-named-let bindings body* env x)]
          [else
           (verify-bindings bindings x)
           (let ([lhs* (map car bindings)]
                 [rhs* (map cadr bindings)])
             (verify-fml* lhs* x)
             (let ([nlhs* (map (lambda (x) (gensym)) lhs*)])
               (let ([nrhs* (map (lambda (x) (E x env)) rhs*)])
                 (let ([env (extend-env-fml* lhs* nlhs* env)])
                   (build-let nlhs* nrhs* (E-internal body* env x))))))]))))
  (define verify-bindings
    (lambda (b* x)
      (unless (list? b*) (syntax-error x))
      (for-each
        (lambda (b)
          (unless (and (list? b) 
                       (fx= (length b) 2)
                       (symbol? (car b)))
            (syntax-error x)))
        b*)))
  ;;;
  (define E-let*
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([bindings (car d)] [body* (cdr d)])
        (verify-bindings bindings x)
        (let ([lhs* (map car bindings)]
              [rhs* (map cadr bindings)])
          (let ([nlhs* (map (lambda (x) (gensym)) lhs*)])
            (let f ([lhs* lhs*] [nlhs* nlhs*] [rhs* rhs*] [env env])
              (cond
                [(null? lhs*) (E-internal body* env x)]
                [else
                 (build-let (list (car nlhs*))
                            (list (E (car rhs*) env))
                   (f (cdr lhs*) (cdr nlhs*) (cdr rhs*)
                      (cons (cons (car lhs*) (car nlhs*)) env)))])))))))
  ;;;
  (define E-letrec
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([bindings (car d)] [body* (cdr d)])
        (verify-bindings bindings x)
        (let ([lhs* (map car bindings)]
              [rhs* (map cadr bindings)])
          (verify-fml* lhs* x)
          (let ([nlhs* (map (lambda (x) (gensym)) lhs*)])
            (let ([env (extend-env-fml* lhs* nlhs* env)])
              (let ([nrhs* (map (lambda (x) (E x env)) rhs*)])
                (build-letrec nlhs* nrhs* (E-internal body* env x)))))))))
  ;;;
  (define E-letrec*
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([bindings (car d)] [body* (cdr d)])
        (verify-bindings bindings x)
        (let ([lhs* (map car bindings)]
              [rhs* (map cadr bindings)])
          (verify-fml* lhs* x)
          (let ([nlhs* (map (lambda (x) (gensym)) lhs*)])
            (let ([env (extend-env-fml* lhs* nlhs* env)])
              (let ([nrhs* (map (lambda (x) (E x env)) rhs*)])
                (build-letrec* nlhs* nrhs* (E-internal body* env x)))))))))
  ;;;
  (define E-lambda
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([fml* (car d)] [body* (cdr d)])
        (verify-fml* fml* x)
        (let ([nfml* (gen-fml* fml*)])
          (let ([env (extend-env-fml* fml* nfml* env)])
            (build-function 
              nfml*
              (E-internal body* env x)))))))
  (define verify-fml*
    (lambda (fml* x)
      (let ([g (gensym)])
        (let f ([fml* fml*])
          (cond
            [(pair? fml*)
             (let ([a (car fml*)])
               (unless (symbol? a) (syntax-error x))
               (when (getprop a g) (syntax-error x))
               (putprop a g a)
               (f (cdr fml*))
               (remprop a g))]
            [(symbol? fml*)
             (when (getprop fml* g) (syntax-error x))]
            [(null? fml*) (void)]
            [else (syntax-error x)])))))
  (define gen-fml*
    (lambda (fml*)
      (cond
        [(pair? fml*) 
         (cons (gensym) (gen-fml* (cdr fml*)))]
        [(symbol? fml*) (gensym)]
        [else '()])))
  (define extend-env-fml*
    (lambda (fml* nfml* env)
      (cond
        [(pair? fml*) 
         (cons (cons (car fml*) (car nfml*)) 
               (extend-env-fml* (cdr fml*) (cdr nfml*) env))]
        [(symbol? fml*)
         (cons (cons fml* nfml*) env)]
        [else env])))
  ;;;
  (define E-internal
    (lambda (body* env x)
      (let f ([a (car body*)] [body* (cdr body*)] [lhs* '()] [rhs* '()])
        (cond
          [(and (pair? a) (symbol? (car a)))
           (let ([fst (car a)])
             (cond
               [(or (memq fst lhs*) (lookup a env))
                (E-internal-done a body* lhs* rhs* env)]
               [(keyword? fst)
                (cond
                  [(eq? fst 'begin) 
                   (let ([d (cdr a)])
                     (unless (list? d) (syntax-error x))
                     (let ([body* (append d body*)])
                       (if (null? body*)
                           (syntax-error x)
                           (f (car body*) (cdr body*) lhs* rhs*))))]
                  [(eq? fst 'define)
                   (let ([def (parse-define (cdr a) env fst)])
                     (f (car body*) (cdr body*)
                        (cons (car def) lhs*)
                        (cons (cdr def) rhs*)))]
                  [else (E-internal-done a body* lhs* rhs* env)])]
               [else (E-internal-done a body* lhs* rhs* env)]))]
          [else (E-internal-done a body* lhs* rhs* env)]))))
  (define parse-define
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([fst (car d)] [rest (cdr d)])
        (cond
          [(symbol? fst) 
           (unless (fx= (length rest) 1) (syntax-error x))
           (list fst 'expr (car rest))]
          [(pair? fst)
           (unless (list? fst) (syntax-error x))
           (unless (andmap symbol? fst) (syntax-error x))
           (list (car fst) 'defun (cdr fst) rest)]
          [else (syntax-error x)]))))
  (define E-def
    (lambda (x env)
      (let ([type (car x)])
        (cond
          [(eq? type 'expr) (E (cadr x) env)]
          [(eq? type 'defun)
           (let ([fml* (cadr x)] [body* (caddr x)])
             (let ([nfml* (map (lambda (x) (gensym)) fml*)])
               (let ([env (extend-env-fml* fml* nfml* env)])
                 (build-function nfml*
                    (E-internal body* env x)))))]
          [else (bug "invalid type" x)]))))
  (define E-internal-done
    (lambda (a d lhs* rhs* env)
      (if (null? lhs*) 
          (E-begin^ a d env)
          (let ([nlhs* (map (lambda (x) (gensym)) lhs*)])
            (let ([env (append (map cons lhs* nlhs*) env)])
              (let ([nrhs* (map (lambda (x) (E-def x env)) rhs*)]) 
                (build-letrec nlhs* nrhs* (E-begin^ a d env))))))))
  ;;;
  (define E-when
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([test (car d)] [body* (cdr d)])
        (build-conditional
          (E test env)
          (E-begin^ (car body*) (cdr body*) env)
          (build-void)))))
  ;;;
  (define E-unless
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([test (car d)] [body* (cdr d)])
        (build-conditional
          (E test env)
          (build-void)
          (E-begin^ (car body*) (cdr body*) env)))))
  ;;;
  (define E-or
    (lambda (d env x)
      (cond
        [(null? d) (build-constant #f)]
        [(null? (cdr d)) (E (car d) env)]
        [else
         (let ([t (gensym)])
           (build-let (list t) (list (E (car d) env))
             (build-conditional
               (build-lexical-reference t)
               (build-lexical-reference t)
               (E-or (cdr d) env x))))])))
  ;;;
  (define E-and
    (lambda (d env x)
      (cond
        [(null? d) (build-constant #t)]
        [(null? (cdr d)) (E (car d) env)]
        [else
         (build-conditional
           (E (car d) env)
           (E-and (cdr d) env x)
           (build-constant #f))])))
  ;;;
  (define E-case
    (lambda (d env x)
      (unless (fx>= (length d) 2) (syntax-error x))
      (let ([val (car d)] [cls* (cdr d)])
        (let ([g (gensym)])
          (build-let (list g)
                     (list (E val env))
            (E-case-cls* g (car cls*) (cdr cls*) env x))))))
  (define E-case-cls*
    (lambda (g cls cls* env x)
      (cond
        [(null? cls*) (E-case-cls-last g cls env x)]
        [else
         (unless (and (list? cls) (fx>= (length cls) 2))
           (syntax-error x))
         (let ([ls (car cls)] [b* (cdr cls)])
           (unless (list? ls) (syntax-error x))
           (build-conditional
             (build-memv (build-lexical-reference g)
                         (build-constant ls))
             (E-begin^ (car b*) (cdr b*) env)
             (E-case-cls* g (car cls*) (cdr cls*) env x)))])))
  (define E-case-cls-last
    (lambda (g cls env x)
      (unless (and (list? cls) (fx>= (length cls) 2))
        (syntax-error x))
      (let ([fst (car cls)] [b* (cdr cls)])
        (cond
          [(and (eq? fst 'else) 
                (not (lookup fst env)))
           (E-begin^ (car b*) (cdr b*) env)]
          [(list? fst)
           (build-conditional
             (build-memv (build-lexical-reference g)
                         (build-constant fst))
             (E-begin^ (car b*) (cdr b*) env)
             (build-void))]
          [else (syntax-error x)]))))
  ;;;
  (define E-cond
    (lambda (d env x)
      (unless (fx>= (length d) 1) (syntax-error x))
      (E-cond-cls* (car d) (cdr d) env x)))
  (define E-cond-cls*
    (lambda (cls cls* env x)
      (cond
        [(null? cls*) (E-cond-cls-last cls env x)]
        [else
         (E-cond-cls cls env x
           (E-cond-cls* (car cls*) (cdr cls*) env x))])))
  (define E-cond-cls
    (lambda (cls env x k)
      (unless (list? cls) (syntax-error x))
      (let ([n (length cls)])
        (unless (fx>= n 1) (syntax-error x))
        (cond
          [(fx= n 1) 
           (let ([g (gensym)])
             (build-let (list g) 
                        (list (E (car cls) env))
                (build-conditional
                  (build-lexical-reference g)
                  (build-lexical-reference g)
                  k)))]
          [(and (fx= n 3)
                (eq? (cadr cls) '=>)
                (not (lookup '=> env)))
           (let ([g (gensym)])
             (build-let (list g) 
                        (list (E (car cls) env))
                (build-conditional
                  (build-lexical-reference g)
                  (build-application
                    (E (caddr cls) env)
                    (list (build-lexical-reference g)))
                  k)))]
          [else
           (let ([test (car cls)] [body* (cdr cls)])
             (build-conditional
               (E test env)
               (E-begin^ (car body*) (cdr body*) env)
               k))]))))
  (define E-cond-cls-last
    (lambda (cls env x)
      (unless (list? cls) (syntax-error x))
      (cond
        [(and (fx>= (length cls) 2)
              (eq? (car cls) 'else)
              (not (lookup 'else env)))
         (let ([body* (cdr cls)])
           (E-begin^ (car body*) (cdr body*) env))]
        [else (E-cond-cls cls env x (build-void))])))
  ;;;
  (define E-pcb-set!
    (lambda (d env x)
      (unless (fx= (length d) 2)  (syntax-error x))
      (let ([name (car d)] [val (cadr d)])
        (unless (symbol? name) (syntax-error x))
        (build-pcb-set! (build-constant name) (E val env)))))
  ;;;
  (define E-foreign-call
    (lambda (d env x)
      (unless (fx>= (length d) 1) (syntax-error x))
      (build-foreign-call
        (E (car d) env)
        (map (lambda (x) (E x env)) (cdr d)))))
  ;;;
  (define E-apply
    (lambda (d env x)
      (unless (fx>= (length d) 1) (syntax-error x))
      (build-apply
        (E (car d) env)
        (map (lambda (x) (E x env)) (cdr d)))))
  ;;;
  (define E
    (lambda (x env)
      (cond
        [(self-evaluating? x) (build-constant x)]
        [(symbol? x)
         (cond
           [(lookup x env) =>
            (lambda (b)
              (build-lexical-reference b))]
           [(keyword? x) 
            (syntax-error x)]
           [else 
            (build-global-reference x)])]
        [(pair? x)
         (let ([a (car x)] [d (cdr x)])
           (unless (list? d) (syntax-error x))
           (cond
             [(symbol? a)
              (cond
                [(lookup a env) =>
                 (lambda (b)
                   (build-application 
                     (build-lexical-reference b) 
                     (E* d env)))]
                [(keyword? a)
                 (cond
                   [(eq? a 'quote)   (E-quote d env x)]
                   [(eq? a 'if)      (E-if d env x)]
                   [(eq? a 'set!)    (E-set! d env x)]
                   [(eq? a 'begin)   (E-begin d env x)]
                   [(eq? a 'lambda)  (E-lambda d env x)]
                   [(eq? a 'let)     (E-let d env x)]
                   [(eq? a 'letrec)  (E-letrec d env x)]
                   [(eq? a 'let*)    (E-let* d env x)]
                   [(eq? a 'letrec*) (E-letrec* d env x)]
                   [(eq? a 'when)    (E-when d env x)]
                   [(eq? a 'unless)  (E-unless d env x)]
                   [(eq? a 'or)      (E-or d env x)]
                   [(eq? a 'and)     (E-and d env x)]
                   [(eq? a 'case)    (E-case d env x)]
                   [(eq? a 'cond)    (E-cond d env x)]
                   [(eq? a 'foreign-call)
                    (E-foreign-call d env x)]
                   [(eq? a '$pcb-set!) (E-pcb-set! d env x)]
                   [(eq? a '$apply) (E-apply d env x)]
                   [(eq? a 'define)  (syntax-error x)]
                   [else (bug "unhandled keyword" x)])]
                [else
                 (build-application
                   (build-global-reference a)
                   (E* d env))])]
             [else
              (build-application
                (E a env)
                (E* d env))]))]
        [else (syntax-error x)])))
  ;;;
  (define E*-top
    (lambda (x x*)
      (cond
        [(null? x*) (E-top x)]
        [else
         (let ([x (E-top x)])
           (build-sequence x (E*-top (car x*) (cdr x*))))])))
  ;;;
  (define E-top-level-define
    (lambda (d ctxt)
      (let ([def (parse-define d empty-env ctxt)])
        (let ([lhs (car def)] [rhs (cdr def)])
          (remprop lhs *keyword*)
          (build-global-assignment lhs
            (E-def rhs empty-env))))))
  ;;;
  (define E-top
    (lambda (x)
      (cond
        [(self-evaluating? x) 
         (build-constant x)]
        [(symbol? x)
         (when (keyword? x) (syntax-error x))
         (build-global-reference x)]
        [(pair? x)
         (let ([a (car x)] [d (cdr x)])
           (unless (list? d) (syntax-error x))
           (cond
             [(and (symbol? a) (keyword? a))
              (cond 
                [(eq? a 'begin) 
                 (if (null? d)
                     (build-void)
                     (E*-top (car d) (cdr d)))]
                [(eq? a 'define) (E-top-level-define d x)]
                [else (E x empty-env)])]
             [else
              (build-application
                (E a empty-env)
                (E* d empty-env))]))]
        [else (syntax-error x)])))
  ;;;
  ($pcb-set! core-expand E-top)
  ;;;
  ($pcb-set! current-expand
     (make-parameter
       core-expand
       (lambda (x)
         (unless (procedure? x)
           (error 'current-expand "~s is not a procedure" x))
         x)))
  ;;;
  ($pcb-set! expand
     (lambda (x)
       ((current-expand) x)))
  ;;;
  (for-each
    (lambda (x)
      (putprop x *keyword* x))
    '(lambda set! let let* letrec letrec* if quote when unless set! begin 
      define or and cond case $pcb-set! foreign-call $apply)))

