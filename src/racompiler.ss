#!/usr/bin/env ikarus --script
(import scheme)
(define (racompile x)
  ;;;
  (define-syntax record-case
    (lambda (x)
      (define (enumerate fld* i)
        (syntax-case fld* ()
          [() #'()]
          [(x . x*) 
           (with-syntax ([i i] [i* (enumerate #'x* (fx+ i 1))])
             #'(i . i*))]))
      (define (generate-body ctxt cls*)
        (syntax-case cls* (else)
          [() (with-syntax ([x x]) #'(error #f "unmatched ~s in ~s" v 'x))]
          [([else b b* ...])  #'(begin b b* ...)]
          [([(rec-name rec-field* ...) b b* ...] . rest) (identifier? #'rec-name)
           (with-syntax ([altern (generate-body ctxt #'rest)]
                         [(id* ...) (enumerate #'(rec-field* ...) 0)]
                         [rtd #'(type-descriptor rec-name)])
            #'(if (#%$record/rtd? v rtd)
                  (let ([rec-field* (#%$record-ref v id*)] ...)
                    b b* ...)
                  altern))]))
      (syntax-case x ()
        [(_ expr cls* ...)
         (with-syntax ([body (generate-body #'_ #'(cls* ...))])
           #'(let ([v expr]) body))])))
  ;;;
  (define-record constant (val))
  (define (mkconst v) (make-constant v))
  (define-record int (val))
  (define (mkint v) (make-int v))
  (define-record set (lhs rhs))
  (define (mkset x v) (make-set x v))
  (define-record reg (name))
  (define (mkreg x) (make-reg x))
  (define-record primcall (op rand*))
  (define (mkprm op . rand*) (make-primcall op rand*))
  (define-record seq (e0 e1))
  (define (mkseq e0 e1) (make-seq e0 e1))
  (define-record conditional (e0 e1 e2))
  (define (mkif e0 e1 e2) (make-conditional e0 e1 e2))
  (define-record app (rator rand*))
  (define (mkapp rator . rands) (make-app rator rands))
  (define-record clambda (free cases))
  (define-record clambda-case (fml* proper body))
  (define-record var (name index))
  (define-record bind (lhs* rhs* body))
  (define (mkbind lhs* rhs* body)
    (if (null? lhs*)
        body
        (make-bind lhs* rhs* body)))
  ;;;
  (define (unparse x)
    (define (flat x ac)
      (record-case x
        [(seq e0 e1)
         (flat e0 (flat e1 ac))]
        [else
         (cons (E x) ac)]))
    (define (E x)
      (record-case x
        [(constant c) `(const ,c)]
        [(int i)      `(int ,i)]
        [(var name) `(var ,name)]
        [(set lhs rhs) `(set ,(E lhs) ,(E rhs))]
        [(reg r)      `(reg ,r)]
        [(primcall op rands) `(,op . ,(map E rands))]
        [(seq e0 e1) 
         `(seq . ,(flat e0 (flat e1 '())))]
        [(conditional e0 e1 e2)
         `(if ,(E e0) ,(E e1) ,(E e2))]
        [else (error 'unparse "invalid ~s" x)]))
    (E x))
  ;;;
  (define (pretty-code x)
    (parameterize ([print-gensym 'pretty])
      (pretty-print (unparse x))))
  ;;;
  (module (primitive? arg-count-ok? primitive-context)
    (define primitives 
      '([$fxadd1           1  v]
        [$fxsub1           1  v]
        [$fxlognot         1  v]
        [$fixnum->char     1  v]
        [$char->fixnum     1  v]
        [fixnum?           1  p]
        [null?             1  p]
        [$fxzero?          1  p]
        [boolean?          1  p]
        [char?             1  p]
        [not               1  not]
        [$fx+              2  v]
        [$fx-              2  v]
        [$fx*              2  v]
        [$fxlogor          2  v]
        [$fxlogand         2  v]
        [$fx=              2  p]
        [$fx<              2  p]
        [$fx<=             2  p]
        [$fx>              2  p]
        [$fx>=             2  p]
        ))
    ;;;
    (define (primitive? x)
      (and (assq x primitives) #t))
    ;;;
    (define (arg-count-ok? prim n)
      (cond
        [(assq prim primitives) =>
         (lambda (p)
           (let ([m (cadr p)])
             (cond
               [(= n m) #t]
               [else #f])))]
        [else (error 'arg-count-ok? "~s is not a primitive" prim)]))
    ;;;
    (define (primitive-context prim)
      (cond
        [(assq prim primitives) => caddr]
        [else (error 'arg-count-ok? "~s is not a primitive" prim)]))
    #|module|#)
  ;;;
  (define (recordize x)
    (define who 'recordize)
    ;;;
    (define (E* x* r)
      (map (lambda (x) (E x r)) x*))
    ;;;
    (define (list->seq ls)
      (let f ([a (car ls)] [ls (cdr ls)])
        (cond
          [(null? ls) a]
          [else
           (f (make-seq a (car ls)) (cdr ls))])))
    ;;;
    (define (lookup x r)
      (cond
        [(null? r) #f]
        [(assq x (car r)) => cdr]
        [else (lookup x (cdr r))]))
    ;;;
    (define (E x r)
      (cond
        [(symbol? x)
         (or (lookup x r)
             (error who "unbound variable ~s" x))]
        [(and (pair? x) (symbol? (car x)))
         (case (car x)
           [(quote) (mkconst (cadr x))]
           [(if) 
            (mkif (E (cadr x) r) 
                  (E (caddr x) r)
                  (E (cadddr x) r))]
           [(case-lambda) 
            (make-clambda #f 
              (map (lambda (x)
                     (define (parse-fml* fml*)
                       (cond
                         [(null? fml*) 
                          (values '() '() #t)]
                         [(symbol? fml*)
                          (let ([f (make-var fml* #f)])
                            (values (list f) 
                                    (list (cons fml* f))
                                    #f))]
                         [else
                          (let-values ([(f* r p) 
                                        (parse-fml* (cdr fml*))])
                            (let ([f (make-var (car fml*) #f)])
                              (values (cons f f*) 
                                      (cons (cons (car fml*) f) r)
                                      p)))]))
                     (let ([fml* (car x)]
                           [body* (cdr x)])
                       (let-values ([(fml* nr proper) 
                                     (parse-fml* fml*)])
                         (make-clambda-case fml* proper
                            (list->seq (E* body* (cons nr r)))))))
                   (cdr x)))]
           [else (make-app (E (car x) r) (E* (cdr x) r))])]
        [(pair? x)
         (let ([a (car x)])
           (cond 
             [(and (pair? a) (eq? (car a) '|#primitive|))
              (let ([op (cadr a)])
                (cond
                  [(not (primitive? op))
                   (error who "invalid primitive ~s" op)]
                  [(not (arg-count-ok? op (length (cdr x))))
                   (error who "incorrect args in ~s" x)]
                  [else
                   (make-primcall op (E* (cdr x) r))]))]
             [else 
              (make-app (E a r) (E* (cdr x) r))]))]
        [else (error who "invalid expression ~s" x)]))
    ;;;
    (E x '()))
  ;;;
  (define (optimize-direct-calls x)
    (define who 'optimize-direct-call)
    (define (optimize rator rands)
      (define (args-match fml* proper rands)
        (if proper
            (= (length fml*) (length rands))
            (error who "unhandled improper list")))
      (define (bindem fml* proper rands body)
        (if proper
            (mkbind fml* rands body)
            (error who "unhandled improper list")))
      (record-case rator
        [(clambda free cases)
         (let f ([ls cases])
           (cond
             [(null? ls) (make-app rator rands)]
             [(record-case (car ls)
                [(clambda-case fml* proper body)
                 (if (args-match fml* proper rands)
                     (bindem fml* proper rands body)
                     #f)])]
             [else (f (cdr ls))]))]
        [else (make-app rator rands)]))
    (define (E x)
      (record-case x
        [(constant) x]
        [(var) x]
        [(conditional e0 e1 e2)
         (mkif (E e0) (E e1) (E e2))]
        [(clambda free cases)
         (make-clambda free
           (map (lambda (c)
                  (record-case c
                    [(clambda-case fml* proper body)
                     (make-clambda-case fml* proper (E body))]))
                cases))]
        [(primcall op rands)
         (make-primcall op (map E rands))]
        [(app rator rands)
         (optimize (E rator) (map E rands))]
        [else (error who "invalid expression ~s" x)]))
    (E x))
  ;;;
  (define (normalize-context x)
    (define who 'normalize-context)
    ;;;
    (define (P x)
      (define (predicafy x)
        (mkif (mkprm 'eq? x (make-constant #f))
          (make-constant #f)
          (make-constant #t)))
      (record-case x
        [(constant c) (make-constant (if c #t #f))]
        [(var x) (predicafy x)]
        [(conditional e0 e1 e2)
         (mkif (P e0) (P e1) (P e2))]
        [(bind lhs* rhs* body)
         (make-bind lhs* (map V rhs*) (P body))]
        [(primcall op rands)
         (case (primitive-context op)
           [(v) (predicafy (V x))]
           [(p) (make-primcall op (map V rands))]
           [(not) (mkif (P (car rands)) (mkconst #f) (mkconst #t))]
           [else (error who "unhandled pred context")])]
        [else (error who "invalid expression ~s" x)]))
    ;;;
    (define (V x)
      (record-case x
        [(constant) x]
        [(var)      x]
        [(conditional e0 e1 e2)
         (mkif (P e0) (V e1) (V e2))]
        [(bind lhs* rhs* body)
         (make-bind lhs* (map V rhs*) (V body))]
        [(primcall op rands)
         (case (primitive-context op)
           [(v) (make-primcall op (map V rands))]
           [(p) (mkif (P x) (mkconst #t) (mkconst #f))]
           [(not) (mkif (P (car rands)) (mkconst #f) (mkconst #t))]
           [else (error who "unhandled value context")])]
        [else (error who "invalid expression ~s" x)]))
    ;;;
    (V x))
  ;;;
  (define (specify-representation x)
    (define who 'specify-representation)
    ;;;
    (define fixnum-scale 4)
    (define fixnum-shift 2)
    (define fixnum-mask 3)
    (define fixnum-tag 0)
    (define boolean-mask #xEF)
    (define boolean-tag #x2F)
    (define true-object #x3F)
    (define false-object #x2F)
    (define void-object #x7F)
    (define bwp-object #x8F)
    (define eof-object #x5F)
    (define null-object #x4F)
    (define char-shift 8)
    (define char-tag #x0F)
    (define char-mask #xFF)
    ;;;
    (define (immediate? c)
      (or (fixnum? c)
          (boolean? c)
          (char? c)
          (null? c)
          (eq? c (void))
          (eof-object? c)
          (bwp-object? c)))
    ;;;
    (define (immediate-rep c)
      (cond
        [(fixnum? c) (mkint (* c fixnum-scale))]
        [(boolean? c) (mkint (if c true-object false-object))]
        [(char? c) 
         (mkint (fxlogor char-tag (fxsll (char->integer c) char-shift)))]
        [(null? c) (mkint null-object)]
        [(eof-object? c) (mkint eof-object)]
        [(eq? c (void)) (mkint void-object)]
        [(bwp-object? c) (mkint bwp-object)]
        [else (error 'immediate-rep "invalid ~s" c)]))
    ;;;
    (define (P x)
      (define (tagcmp rands mask tag)
        (mkprm 'int= 
            (mkprm 'intand (V (car rands)) (mkint mask))
            (mkint tag)))
      (record-case x
        [(constant) x]
        [(conditional e0 e1 e2)
         (mkif (P e0) (P e1) (P e2))]
        [(bind lhs* rhs* body)
         (make-bind lhs* (map V rhs*) (P body))]
        [(primcall op rands)
         (case op
           [(fixnum?)  (tagcmp rands fixnum-mask fixnum-tag)] 
           [(boolean?) (tagcmp rands boolean-mask boolean-tag)] 
           [(char?)    (tagcmp rands char-mask char-tag)] 
           [($fxzero?)
            (mkprm 'int= (V (car rands)) (immediate-rep 0))]
           [(null?)
            (mkprm 'int= (V (car rands)) (immediate-rep '()))]
           [(eq? $fx=) 
            (mkprm 'int= (V (car rands)) (V (cadr rands)))]
           [(eq? $fx<) 
            (mkprm 'int< (V (car rands)) (V (cadr rands)))]
           [(eq? $fx<=) 
            (mkprm 'int<= (V (car rands)) (V (cadr rands)))]
           [(eq? $fx>) 
            (mkprm 'int> (V (car rands)) (V (cadr rands)))]
           [(eq? $fx>=) 
            (mkprm 'int>= (V (car rands)) (V (cadr rands)))]
           [else (error who "invalid value prim ~s" op)])]
        [else (error who "invalid value ~s" x)])) 
    (define (V x)
      (record-case x
        [(constant c)
         (if (immediate? c)
             (immediate-rep c)
             x)]
        [(conditional e0 e1 e2)
         (mkif (P e0) (V e1) (V e2))]
        [(var) x]
        [(bind lhs* rhs* body)
         (make-bind lhs* (map V rhs*) (V body))]
        [(primcall op rands)
         (case op
           [($fxadd1) 
            (mkprm 'int+ (V (car rands)) (immediate-rep 1))]
           [($fxsub1) 
            (mkprm 'int+ (V (car rands)) (immediate-rep -1))]
           [($fx+)
            (mkprm 'int+ (V (car rands)) (V (cadr rands)))]
           [($fxlogor)
            (mkprm 'intor (V (car rands)) (V (cadr rands)))]
           [($fxlogand)
            (mkprm 'intand (V (car rands)) (V (cadr rands)))]
           [($fx-)
            (mkprm 'int- (V (car rands)) (V (cadr rands)))]
           [($fx*)
            (let ([a (car rands)] [b (cadr rands)])
              (let ([ai (record-case a
                          [(constant i) 
                           (if (fixnum? i) i #f)]
                          [else #f])]
                    [bi (record-case b
                          [(constant i) 
                           (if (fixnum? i) i #f)]
                          [else #f])])
                (cond
                  [ai
                   (mkprm 'int* (V b) (mkint ai))]
                  [bi
                   (mkprm 'int* (V a) (mkint bi))]
                  [else
                   (mkprm 'int* ;;; FIXME GC problem
                          (mkprm 'intsra (V a) (mkint fixnum-shift))
                          (V b))])))]
           [($fxlognot) 
            (mkprm 'intxor (V (car rands)) (immediate-rep -1))]
           [($char->fixnum)
            (mkprm 'intsra (V (car rands))
                   (mkint (- char-shift fixnum-shift)))]
           [($fixnum->char)
            (mkprm 'intor 
                (mkprm 'intsll (V (car rands)) 
                       (mkint (- char-shift fixnum-shift)))
                (mkint char-tag))]
           [else (error who "invalid value prim ~s" op)])]
        [else (error who "invalid value ~s" x)]))
    ;;;
    (V x))
  ;;;
  (define (impose-calling-convention x)
    (define who 'impose-calling-convention)
    ;;;
    (define rv-register (mkreg '%eax))
    ;;;
    (define (simple? x)
      (record-case x
        [(constant) #t]
        [(int)      #t]
        [else       #f]))
    ;;;
    (define (do-bind lhs* rhs* body)
      (cond
        [(null? lhs*) body]
        [else
         (mkseq (D (car lhs*) (car rhs*))
           (do-bind (cdr lhs*) (cdr rhs*) body))]))
    ;;;
    (define (D d x)
      (define (assoc op a b)
        (cond
          [(simple? a)
           (let ([t (new-uvar)])
             (mkseq (D t b)
               (mkseq (mkset t (mkprm op t a))
                 (mkset d t))))]
          [(simple? b)
           (let ([t (new-uvar)])
             (mkseq (D t a)
               (mkseq (mkset t (mkprm op t b))
                 (mkset d t))))]
          [else (error who "two complex operands ~s ~s" a b)]))
      (record-case x
        [(constant) (mkset d x)]
        [(int)      (mkset d x)]
        [(var)      (mkset d x)]
        [(conditional e0 e1 e2)
         (mkif (P e0) (D d e1) (D d e2))]
        [(primcall op rands) 
         (case op
           [(int+)
            (assoc 'int+ (car rands) (cadr rands))]
           [(int*)
            (assoc 'int* (car rands) (cadr rands))]
           [(intxor)
            (assoc 'intxor (car rands) (cadr rands))]
           [(intor)
            (assoc 'intor (car rands) (cadr rands))]
           [(intand)
            (assoc 'intand (car rands) (cadr rands))]
           [(int-)
            (let ([a (car rands)] [b (cadr rands)])
              (cond
               [(simple? b)
                (let ([t (new-uvar)])
                  (mkseq (D t a)
                    (mkseq (mkset t (mkprm 'int- t b))
                      (mkset d t))))]
               [(simple? a)
                (let ([t (new-uvar)])
                  (mkseq (D t b)
                    (mkseq (D d a)
                      (mkset d (mkprm 'int- d t)))))]
               [else (error who "two complex operands ~s ~s" a b)]))]
           [(intsll intsra)
            (let ([a (car rands)] [b (cadr rands)])
              (record-case b
                [(int) 
                 (let ([t (new-uvar)])
                   (mkseq (D t a)
                     (mkseq (mkset t (mkprm op t b))
                       (mkset d t))))]
                [else 
                 (error who "unhandled intsll ~s" b)]))]
           [else (error who "invalid value prim ~s" op)])]
        [else (error who "invalid value value ~s" x)]))
    ;;;
    (define (P x)
      (define (prim op op^ a b)
        (cond
          [(simple? a) 
           (mkseq (V b) (mkprm op^ rv-register a))]
          [(simple? b)
           (mkseq (V a) (mkprm op rv-register b))]
          [else (error who "two complex operands ~s ~s" a b)]))
      (record-case x
        [(constant) x]
        [(conditional e0 e1 e2)
         (mkif (P e0) (P e1) (P e2))]
        [(primcall op rands)
         (case op
           [(int=)
            (prim 'int= 'int= (car rands) (cadr rands))]
           [(int<)
            (prim 'int< 'int> (car rands) (cadr rands))]
           [(int<=)
            (prim 'int<= 'int>= (car rands) (cadr rands))]
           [(int>)
            (prim 'int> 'int< (car rands) (cadr rands))]
           [(int>=)
            (prim 'int>= 'int<= (car rands) (cadr rands))]
           [else (error who "invalid pred prim ~s" op)])]
        [else (error who "invalid pred value ~s" x)])) 
    (define (V x)
      (define (assoc op a b)
        (cond
          [(simple? a) 
           (mkseq (V b)
                  (mkset rv-register (mkprm op rv-register a)))]
          [(simple? b)
           (mkseq (V a)
                  (mkset rv-register (mkprm op rv-register b)))]
          [else (error who "two complex operands ~s ~s" a b)]))
      (record-case x
        [(constant) (mkset rv-register x)]
        [(int)      (mkset rv-register x)]
        [(var)      (mkset rv-register x)]
        [(conditional e0 e1 e2)
         (mkif (P e0) (V e1) (V e2))]
        [(primcall op rands) 
         (case op
           [(int+)
            (assoc 'int+ (car rands) (cadr rands))]
           [(int*)
            (assoc 'int* (car rands) (cadr rands))]
           [(intxor)
            (assoc 'intxor (car rands) (cadr rands))]
           [(intor)
            (assoc 'intor (car rands) (cadr rands))]
           [(intand)
            (assoc 'intand (car rands) (cadr rands))]
           [(int-)
            (let ([a (car rands)] [b (cadr rands)])
              (cond
               [(simple? b)
                (mkseq (V a)
                       (mkset rv-register (mkprm 'int- rv-register b)))]
               [(simple? a)
                (mkseq (mkseq (V b) 
                         (mkset rv-register (mkprm 'intneg rv-register)))
                       (mkset rv-register (mkprm 'int+ rv-register a)))]
               [else (error who "two complex operands ~s ~s" a b)]))]
           [(intsll intsra)
            (let ([a (car rands)] [b (cadr rands)])
              (record-case b
                [(int) 
                 (mkseq (V a)
                        (mkset rv-register (mkprm op rv-register b)))]
                [else 
                 (error who "unhandled intsll ~s" b)]))]
           [else (error who "invalid value prim ~s" op)])]
        [else (error who "invalid value value ~s" x)]))
    ;;;
    (define (Tail x)
      (define (return x)
        (mkseq x (mkprm 'return rv-register)))
      (record-case x
        [(constant) (return (V x))]
        [(int)      (return (V x))]
        [(var)      (return (V x))]
        [(primcall) (return (V x))]
        [(bind lhs* rhs* body)
         (do-bind lhs* rhs* (Tail body))]
        [(conditional e0 e1 e2)
         (mkif (P e0) (Tail e1) (Tail e2))]
        [else (error who "invalid tail ~s" x)]))
    ;;;
    (Tail x))
  ;;;
  (define (linearize x)
    (define who 'linearize)
    ;;;
    (define (op x)
      (record-case x
        [(reg r) r]
        [(constant c) `(obj ,c)]
        [(int i) i]
        [else (error who "invalid op ~s" x)]))
    ;;;
    (define (same? x y)
      (record-case x
        [(reg rx) 
         (record-case y
           [(reg ry) (eq? rx ry)]
           [else #f])]
        [else (error 'same? "invalid arg ~s" x)]))
    ;;;
    (define (indep? x y)
      (record-case x
        [(reg rx)
         (let f ([y y])
           (record-case y
             [(int) #t]
             [(constant) #t]
             [(reg ry) (not (eq? rx ry))]
             [(primcall op rands)
              (andmap f rands)]
             [else (error 'indep? "unhandled ~s" y)]))]
        [else (error 'indep? "invalid arg ~s" x)]))
    ;;;
    (define (Pred x lt lf ac)
      (define (revcmp x)
        (case x
          [(int=) 'int=]
          [(int<) 'int>]
          [(int<=) 'int>=]
          [(int>) 'int<]
          [(int>=) 'int<=]
          [else (errot 'revcmp "invalid cmp ~s" x)]))
      (define (CJump cnd lt lf ac)
        (define (cjumpop x)
          (case x
            [(int=) 'je]
            [(int<) 'jl]
            [(int<=) 'jle]
            [(int>) 'jg]
            [(int>=) 'jge]))
        (define (cjumpop^ x)
          (case x
            [(int=) 'jne]
            [(int<) 'jnl]
            [(int<=) 'jnle]
            [(int>) 'jng]
            [(int>=) 'jnge]))
        (cond
          [(and lt lf) 
           (list* `(,(cjumpop cnd) (label ,lt))
                  `(jmp (label ,lf))
                  ac)]
          [lt
           (list* `(,(cjumpop cnd) (label ,lt))
                  ac)]
          [lf
           (list* `(,(cjumpop^ cnd) (label ,lf))
                  ac)]
          [else ac]))
      (record-case x
        [(constant c)
         (if c
             (if lt (cons `(jmp (label ,lt)) ac) ac)
             (if lf (cons `(jmp (label ,lf)) ac) ac))]
        [(seq e0 e1)
         (Effect e0 (Pred e1 lt lf ac))]
        [(conditional e0 e1 e2)
         (cond
           [(and lt lf)
            (let ([g (gensym)])
              (Pred e0 #f g
                 (Pred e1 lt lf 
                    (cons `(label ,g) 
                          (Pred e2 lt lf ac)))))]
           [lt
            (let ([g (gensym)] [lf (gensym)])
              (Pred e0 #f g
                 (Pred e1 lt lf
                    (cons `(label ,g)
                          (Pred e2 lt #f
                             (cons `(label ,lf) ac))))))]
           [lf
            (let ([g (gensym)] [lt (gensym)])
              (Pred e0 #f g
                 (Pred e1 lt lf
                    (cons `(label ,g)
                          (Pred e2 #f lf
                             (cons `(label ,lt) ac))))))]
           [else
            (let ([g (gensym)] [lt (gensym)])
              (Pred e0 #f g
                 (Pred e1 lt lt
                    (cons `(label ,g)
                          (Pred e2 #f #f
                             (cons `(label ,lt) ac))))))])]
        [(primcall prim rands)
         (let ([a (car rands)] [b (cadr rands)])
           (record-case a
             [(reg ra) 
              (cons `(cmpl ,(op b) ,(op a))
                    (CJump prim lt lf ac))]
             [(reg rb) 
              (cons `(cmpl ,(op a) ,(op b))
                    (CJump (revcmp prim) lt lf ac))]
             [else (error who "invalid operands in pred ~s ~s" a b)]))]
        [else (error who "invalid pred ~s" x)]))
    ;;;
    (define (Effect x ac)
      (define (primname x)
        (case x
          [(int+) 'addl]
          [(int*) 'imull]
          [(intor) 'orl]
          [(intxor) 'xorl]
          [(intand) 'andl]
          [(intsll) 'sall]
          [(intsra) 'sarl]
          [else (error who "invalid primname ~s" x)]))
      (record-case x
        [(seq e0 e1)
         (Effect e0 (Effect e1 ac))]
        [(conditional e0 e1 e2)
         (let ([g (gensym)] [elabel (gensym)])
           (Pred e0 #f g
              (Effect e1 
                (list* `(jmp (label ,elabel))
                       `(label ,g)
                       (Effect e2 
                          (cons `(label ,elabel) ac))))))]
        [(set targ v)
         (record-case v
           [(int i) (cons `(movl ,i ,(op targ)) ac)]
           [(constant c) (cons `(movl (obj ,c) ,(op targ)) ac)]
           [(primcall prim rands)
            (case prim
              [(int+ intor intxor intand int*) 
               (let ([asmprm (primname prim)])
                 (let ([a (car rands)] [b (cadr rands)])
                   (cond
                     [(and (same? targ a) (indep? targ b))
                      (cons `(,asmprm ,(op b) ,(op a)) ac)]
                     [(and (same? targ b) (indep? targ b))
                      (cons `(,asmprm ,(op a) ,(op b)) ac)]
                     [(indep? targ b)
                      (list* `(movl ,(op a) ,(op targ))
                             `(,asmprm ,(op b) ,(op targ))
                             ac)]
                     [(indep? targ a)
                      (list* `(movl ,(op b) ,(op targ))
                             `(,asmprm ,(op a) ,(op targ))
                             ac)]
                     [else (error who "invalid ops")])))]
              [(int-)
               (let ([a (car rands)] [b (cadr rands)])
                 (cond
                   [(and (same? targ a) (indep? targ b))
                    (cons `(subl ,(op b) ,(op a)) ac)]
                   [else (error who "invalid ops int-")]))]
              [(intneg)
               (let ([a (car rands)])
                 (cond
                   [(same? targ a)
                    (cons `(negl ,(op a)) ac)]
                   [else (error who "invalid ops intneg")]))]
              [(intsll intsra) 
               (let ([asmprm (primname prim)])
                 (let ([a (car rands)] [b (cadr rands)])
                   (cond
                     [(and (same? targ a) (indep? targ b))
                      (cons `(,asmprm ,(op b) ,(op a)) ac)]
                     [(indep? targ b)
                      (list* `(movl ,(op a) ,(op targ))
                             `(,asmprm ,(op b) ,(op targ))
                             ac)]
                     [else (error who "invalid ops")])))]
              [else (error who "invalid op ~s" prim)])]
           [else (error who "invalid rhs ~s" v)])]
        [else (error who "invalid effect ~s" x)]))
    ;;;
    (define (Tail x ac)
      (record-case x
        [(seq e0 e1)
         (Effect e0 (Tail e1 ac))]
        [(conditional e0 e1 e2)
         (let ([g (gensym)])
           (Pred e0 #f g
              (Tail e1 
                (cons `(label ,g)
                      (Tail e2 ac)))))]
        [(primcall op rands) 
         (case op
           [(return) 
            (cons '(ret) ac)]
           [else (error who "invalid tail prim ~s" op)])]
        [else (error who "invalid tail ~s" x)]))
    ;;;
    (printf "linearing:\n")
    (pretty-code x)
    (list (list* 0
                 (Tail x '()))))
  ;;;
  (define (compile x)
    (let* ([x (parameterize ([expand-mode 'bootstrap]
                             [interaction-environment 
                               ($make-environment '|#system| #t)])
                (expand x))]
           [x (recordize x)]
           [x (optimize-direct-calls x)]
           [x (normalize-context x)]
           [x (specify-representation x)]
           [x (impose-calling-convention x)]
           [x* (linearize x)]
           [foo (parameterize ([print-gensym 'pretty])
                  (for-each 
                    (lambda (ls)
                      (for-each (lambda (x) 
                                  (printf "     ~s\n" x))
                                ls))
                    x*))]
           [code (car (#%list*->code* 
                         (lambda (x) #f)
                         x*))])
      ((#%$code->closure code))))
  (compile x))



(define-syntax add-tests-with-string-output 
  (syntax-rules (=>)
    [(_ name [expr* => str*] ...)
     (begin
       (printf "SECTION ~a ...\n" 'name)
       (let ([str str*]
             [expr 'expr*])
         (fprintf (console-output-port) "testing ~s\n" expr)
         (let ([r (let ([v (racompile expr)])
                    (fprintf (console-output-port) ".")
                    (with-output-to-string 
                      (lambda ()
                        (write v)
                        (newline))))])
           (fprintf (console-output-port) ".")
           (unless (string=? r str)
             (error #f "expected ~s, got ~s\n" str r)))) 
       ...)]))

(load "tests/tests-1.1-req.scm")
(load "tests/tests-1.2-req.scm")
(load "tests/tests-1.3-req.scm")
(load "tests/tests-1.4-req.scm")
(load "tests/tests-1.5-req.scm")
(load "tests/tests-1.6-req.scm")

(printf "ALL IS GOOD :-)\n")
