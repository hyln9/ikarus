
;(module primops (primop? cogen-primop)
;  (define (primop? x) #f)
;  (define cogen-primop (lambda args (error 'cogen-primop "not yet"))))
;
;#!eof

(define-syntax export-all-module
  (syntax-rules (define)
    [(_ M (define name* v*) ...)
     (module M (name* ...)
       (define name* v*) ...)]))

(export-all-module object-representation
  (define fixnum-scale 4)
  (define fixnum-shift 2)
  (define fixnum-tag 0)
  (define fixnum-mask 3))

(module primops (primop? get-primop set-primop!)

  (define cookie (gensym))
  (define (primop? x)
    (and (getprop x cookie) #t))
  (define (get-primop x)
    (or (getprop x cookie)
        (error 'getprimop "~s is not a primitive" x)))
  (define (set-primop! x v)
    (putprop x cookie v))
  )


(module (specify-representation)
  (import object-representation)
  (import primops)
   (define-record PH
    (interruptable? p-handler p-handled? v-handler v-handled? e-handler e-handled?))
  (define interrupt-handler
    (make-parameter (lambda () (error 'interrupt-handler "uninitialized"))))
  (define (interrupt) 
    ((interrupt-handler))
    (prm 'interrupt))
  (define (with-interrupt-handler p x ctxt args k)
    (cond
      [(not (PH-interruptable? p))
       (parameterize ([interrupt-handler 
                       (lambda ()
                         (error 'cogen "~s ~s is uninterruptable in ~s" 
                                x args ctxt))])
          (k))]
      [else
       (let ([interrupted? #f])
         (let ([body 
                (parameterize ([interrupt-handler
                                (lambda () (set! interrupted? #t))])
                   (k))])
           (cond
             [(not interrupted?) body]
             [(eq? ctxt 'V)
              (let ([h (make-funcall (V (make-primref x)) args)])
                (if (record-case body
                      [(primcall op) (eq? op 'interrupt)]
                      [else #f])
                     h
                     (make-shortcut body h)))]
             [(eq? ctxt 'E)
              (let ([h (make-funcall (V (make-primref x)) args)])
                (if (record-case body
                      [(primcall op) (eq? op 'interrupt)]
                      [else #f])
                     h
                     (make-shortcut body h)))]
             [(eq? ctxt 'P)
              (let ([h (prm '!= (make-funcall (V (make-primref x)) args)
                            (K bool-f))])
                (if (record-case body
                      [(primcall op) (eq? op 'interrupt)]
                      [else #f])
                     h
                     (make-shortcut body h)))]
             [else (error 'with-interrupt-handler "invalid context ~s" ctxt)])))]))
  (define-syntax with-tmp
    (lambda (x)
      (syntax-case x ()
        [(_ ([lhs* rhs*] ...) b b* ...)
         (with-syntax ([(n* ...) (generate-temporaries #'(lhs* ...))])
           #'(let ([lhs* rhs*] ...)
               (let ([n* (unique-var 'lhs*)] ...)
                 (make-bind (list n* ...) (list lhs* ...)
                    (let ([lhs* n*] ...)
                      (seq* b b* ...))))))])))
  ;;; if ctxt is V:
  ;;;   if cogen-value, then V
  ;;;   if cogen-pred, then (if P #f #t)
  ;;;   if cogen-effect, then (seq E (void))
  ;;;
  ;;; if ctxt is P:
  ;;;   if cogen-pred, then P
  ;;;   if cogen-value, then (!= V #f) 
  ;;;   if cogen-effect, then (seq E #t)
  ;;;
  ;;; if ctxt is E:
  ;;;   if cogen-effect, then E
  ;;;   if cogen-value, then (let ([tmp V]) (nop))
  ;;;   if cogen-pred, then (if P (nop) (nop))
  (define (simplify* args k)
    (define (S* ls)
      (cond
        [(null? ls) (values '() '() '())]
        [else
         (let-values ([(lhs* rhs* arg*) (S* (cdr ls))])
           (let ([a (car ls)])
             (cond
               [(constant? a) 
                (values lhs* rhs* (cons a arg*))]
               ;[(var? a)
               ; (values lhs* rhs* (cons a arg*))]
               [else
                (let ([t (unique-var 'tmp)])
                  (values (cons t lhs*) (cons (V a) rhs*) (cons t arg*)))])))]))
    (let-values ([(lhs* rhs* args) (S* args)])
      (cond
        [(null? lhs*) (k args)]
        [else
         (make-bind lhs* rhs* (k args))])))
  (define (cogen-primop x ctxt args)
    (define (interrupt? x)
      (record-case x
        [(primcall x) (eq? x 'interrupt)]
        [else #f]))
    (let ([p (get-primop x)])
       (simplify* args
         (lambda (args)
           (with-interrupt-handler p x ctxt (map T args)
             (lambda ()
               (case ctxt
                 [(P) 
                  (cond
                    [(PH-p-handled? p) 
                     (apply (PH-p-handler p) args)]
                    [(PH-v-handled? p)
                     (let ([e (apply (PH-v-handler p) args)])
                       (if (interrupt? e) e (prm '!= e (K bool-f))))]
                    [(PH-e-handled? p)
                     (let ([e (apply (PH-e-handler p) args)])
                       (if (interrupt? e) e (make-seq e (K #t))))]
                    [else (error 'cogen-primop "~s is not handled" x)])]
                 [(V) 
                  (cond
                    [(PH-v-handled? p) 
                     (apply (PH-v-handler p) args)]
                    [(PH-p-handled? p) 
                     (let ([e (apply (PH-p-handler p) args)])
                       (if (interrupt? e)
                           e 
                           (make-conditional e (K bool-t) (K bool-f))))]
                    [(PH-e-handled? p)
                     (let ([e (apply (PH-e-handler p) args)])
                       (if (interrupt? e) e (make-seq e (K void-object))))]
                    [else (error 'cogen-primop "~s is not handled" x)])]
                 [(E) 
                  (cond
                    [(PH-e-handled? p) 
                     (apply (PH-e-handler p) args)]
                    [(PH-p-handled? p) 
                     (let ([e (apply (PH-p-handler p) args)])
                       (if (interrupt? e)
                           e 
                           (make-conditional e (prm 'nop) (prm 'nop))))]
                    [(PH-v-handled? p)
                     (let ([e (apply (PH-v-handler p) args)])
                       (if (interrupt? e)
                           e
                           (with-tmp ([t e]) (prm 'nop))))]
                    [else (error 'cogen-primop "~s is not handled" x)])]
                 [else 
                  (error 'cogen-primop "invalid context ~s" ctxt)])))))))
  
  (define-syntax define-primop
    (lambda (x)
      (define (cogen-name stx name suffix)
        (datum->syntax-object stx
          (string->symbol
            (format "cogen-~a-~a" suffix 
                    (syntax-object->datum name)))))
      (define (generate-handler name ctxt case*)
        (define (filter-cases case*)
          (syntax-case case* ()
            [() '()]
            [([(c . arg*) b b* ...] . rest)
             (free-identifier=? #'c ctxt)
             (cons #'[arg* b b* ...] (filter-cases #'rest))]
            [(c . rest) (filter-cases #'rest)]))
        (let ([case* (filter-cases case*)])
          (with-syntax ([ctxt ctxt] [name name]
                        [(case* ...) case*]
                        [handled? (not (null? case*))])
            #'[(case-lambda 
                 case* ...
                 [args (interrupt)])
               handled?])))
      (syntax-case x ()
        [(_ name int? case* ...) 
         (with-syntax ([cogen-p (cogen-name #'_ #'name "pred")]
                       [cogen-e (cogen-name #'_ #'name "effect")]
                       [cogen-v (cogen-name #'_ #'name "value")]
                       [interruptable?
                        (syntax-case #'int? (safe unsafe)
                          [safe   #t] [unsafe #f])]
                       [(p-handler phandled?) 
                        (generate-handler #'name #'P #'(case* ...))]
                       [(v-handler vhandled?)
                        (generate-handler #'name #'V #'(case* ...))]
                       [(e-handler ehandled?) 
                        (generate-handler #'name #'E #'(case* ...))])
           #'(begin
               (define cogen-p p-handler)
               (define cogen-v v-handler)
               (define cogen-e e-handler)
               (module ()
                 (set-primop! 'name
                    (make-PH interruptable? 
                       cogen-p phandled? 
                       cogen-v vhandled?
                       cogen-e ehandled?)))))])))


  (define (handle-fix lhs* rhs* body)
    (define (closure-size x)
      (record-case x
        [(closure code free*) 
         (if (null? free*) 
             0
             (align (+ disp-closure-data
                       (* (length free*) wordsize))))]))
    (define (partition p? lhs* rhs*)
      (cond
        [(null? lhs*) (values '() '() '() '())]
        [else
         (let-values ([(a* b* c* d*)
                       (partition p? (cdr lhs*) (cdr rhs*))]
                      [(x y) (values (car lhs*) (car rhs*))])
           (cond
             [(p? x y)
              (values (cons x a*) (cons y b*) c* d*)]
             [else 
              (values a* b* (cons x c*) (cons y d*))]))]))
    (define (combinator? lhs rhs)
      (record-case rhs
        [(closure code free*) (null? free*)]))
    (define (sum n* n)
      (cond
        [(null? n*) n]
        [else (sum (cdr n*) (+ n (car n*)))]))
    (define (adders lhs n n*)
      (cond
        [(null? n*) '()]
        [else
         (cons (prm 'int+ lhs (K n))
               (adders lhs (+ n (car n*)) (cdr n*)))]))
    (define (build-closures lhs* rhs* body)
      (let ([lhs (car lhs*)] [rhs (car rhs*)]
            [lhs* (cdr lhs*)] [rhs* (cdr rhs*)])
        (let ([n (closure-size rhs)] 
              [n* (map closure-size rhs*)])
          (make-bind (list lhs) 
                     (list (prm 'alloc 
                                (K (sum n* n))
                                (K closure-tag)))
            (make-bind lhs* (adders lhs n n*)
              body)))))
    (define (build-setters lhs* rhs* body)
      (define (build-setter lhs rhs body)
        (record-case rhs
          [(closure code free*) 
           (make-seq
             (prm 'mset lhs 
                  (K (- disp-closure-code closure-tag))
                  (V code))
             (let f ([ls free*] 
                     [i (- disp-closure-data closure-tag)])
               (cond
                 [(null? ls) body]
                 [else
                  (make-seq
                    (prm 'mset lhs (K i) (V (car ls)))
                    (f (cdr ls) (+ i wordsize)))])))]))
      (cond
        [(null? lhs*) body]
        [else
         (build-setter (car lhs*) (car rhs*)
           (build-setters (cdr lhs*) (cdr rhs*) body))]))
    (let-values ([(flhs* frhs* clhs* crhs*)
                  (partition combinator? lhs* rhs*)])
      (cond
        [(null? clhs*) (make-bind flhs* (map V frhs*) body)]
        [(null? flhs*)
         (build-closures clhs* crhs*
            (build-setters clhs* crhs* body))]
        [else
         (make-bind flhs* (map V frhs*)
           (build-closures clhs* crhs*
             (build-setters clhs* crhs* body)))])))

  (define (constant-rep x)
    (let ([c (constant-value x)])
      (cond
        [(fixnum? c) (make-constant (* c fixnum-scale))]
        [(boolean? c) (make-constant (if c bool-t bool-f))]
        [(eq? c (void)) (make-constant void-object)]
        [(bwp-object? c) (make-constant bwp-object)]
        [(char? c) (make-constant 
                     (fxlogor char-tag
                       (fxsll (char->integer c) char-shift)))]
        [(null? c) (make-constant nil)]
        [(object? c) (error 'constant-rep "double-wrap")]
        [else (make-constant (make-object c))])))

  (define (V x)
    (record-case x
      [(constant) (constant-rep x)]
      [(var)      x]
      [(primref name)  
       (prm 'mref
             (K (make-object name))
             (K (- disp-symbol-system-value symbol-tag)))]
      [(code-loc) (make-constant x)]
      [(closure)  (make-constant x)]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map V rhs*) (V body))]
      [(fix lhs* rhs* body) 
       (handle-fix lhs* rhs* (V body))]
      [(conditional e0 e1 e2) 
       (make-conditional (P e0) (V e1) (V e2))]
      [(seq e0 e1)
       (make-seq (E e0) (V e1))]
      [(primcall op arg*)
       (cogen-primop op 'V arg*)]
      [(forcall op arg*)
       (make-forcall op (map V arg*))]
      [(funcall rator arg*)
       (make-funcall (Function rator) (map V arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (V rator) (map V arg*))]
      [else (error 'cogen-V "invalid value expr ~s" x)])) 

  (define (P x)
    (record-case x
      [(constant c) (if c (K #t) (K #f))]
      [(primref)  (K #t)]
      [(code-loc) (K #t)]
      [(closure)  (K #t)]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map V rhs*) (P body))]
      [(conditional e0 e1 e2) 
       (make-conditional (P e0) (P e1) (P e2))]
      [(seq e0 e1)
       (make-seq (E e0) (P e1))]
      [(fix lhs* rhs* body) 
       (handle-fix lhs* rhs* (P body))]
      [(primcall op arg*)
       (cogen-primop op 'P arg*)]
      [(var)     (prm '!= (V x) (V (K #f)))]
      [(funcall) (prm '!= (V x) (V (K #f)))]
      [(jmpcall) (prm '!= (V x) (V (K #f)))]
      [(forcall) (prm '!= (V x) (V (K #f)))]
      [else (error 'cogen-P "invalid pred expr ~s" x)])) 
  
  (define (E x)
    (record-case x
      [(constant) (nop)]
      [(var)      (nop)]
      [(primref)  (nop)]
      [(code-loc) (nop)]
      [(closure)  (nop)]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map V rhs*) (E body))]
      [(conditional e0 e1 e2) 
       (make-conditional (P e0) (E e1) (E e2))]
      [(seq e0 e1)
       (make-seq (E e0) (E e1))]
      [(fix lhs* rhs* body) 
       (handle-fix lhs* rhs* (E body))]
      [(primcall op arg*)
       (cogen-primop op 'E arg*)]
      [(forcall op arg*)
       (make-forcall op (map V arg*))]
      [(funcall rator arg*)
       (make-funcall (Function rator) (map V arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (V rator) (map V arg*))]
      [else (error 'cogen-E "invalid effect expr ~s" x)]))

  (define (Function x)
    (define (nonproc x)
      (with-tmp ([x (V x)])
        (make-shortcut
          (make-seq
            (make-conditional
              (tag-test x closure-mask closure-tag)
              (prm 'nop)
              (prm 'interrupt))
            x)
          (V (make-funcall (make-primref 'error)
               (list (K 'apply) (K "~s is not a procedure") x))))))
    (record-case x
       [(primcall op args)
        (cond
          [(and (eq? op 'top-level-value)
                (= (length args) 1)
                (record-case (car args)
                  [(constant t) 
                   (and (symbol? t) t)]
                  [else #f])) =>
           (lambda (sym)
             (record-symbol-call! sym)
             (prm 'mref (T (K sym))
                  (K (- disp-symbol-function symbol-tag))))]
          [else (nonproc x)])]
       [(primref op) (V x)]
       [else (nonproc x)]))


  (define encountered-symbol-calls '())
  (define (record-symbol-call! x)
    (unless (memq x encountered-symbol-calls)
      (set! encountered-symbol-calls 
        (cons x encountered-symbol-calls))))


  ;;;========================================================================
  ;;;
  (define (interrupt-unless x)
    (make-conditional x (prm 'nop) (interrupt))) 
  (define (interrupt-when x)
    (make-conditional x (interrupt) (prm 'nop))) 
  (define (interrupt-unless-fixnum x)
    (interrupt-unless (tag-test x fixnum-mask fixnum-tag)))


  (define (T x)
    (record-case x
      [(var) x]
      [(constant i) (constant-rep x)]
      [else (error 'cogen-T "invalid ~s" (unparse x))]))

  (define (ClambdaCase x)
    (record-case x
      [(clambda-case info body)
       (make-clambda-case info (V body))]
      [else (error 'specify-rep "invalid clambda-case ~s" x)]))
  ;;;
  (define (Clambda x)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label 
          (map ClambdaCase case*)
          free*)]
      [else (error 'specify-rep "invalid clambda ~s" x)]))
  ;;;
  (define (error-codes)
    (define (code-list symbol)
      (define L1 (gensym))
      (define L2 (gensym))
      `(0
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) ,cp-register]
        [andl ,closure-mask ,cp-register]
        [cmpl ,closure-tag ,cp-register]
        [jne (label ,L1)]
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) ,cp-register]
        [movl ,cp-register (disp ,(- disp-symbol-function symbol-tag) (obj ,symbol))]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]
        [label ,L1]
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) %eax]
        [cmpl ,unbound %eax]
        [je (label ,L2)]
        [movl (obj apply) (disp -4 %esp)]
        [movl (obj "~s is not a procedure") (disp -8 %esp)]
        [movl %eax (disp -12 %esp)]
        [movl (obj error) ,cp-register]
        [movl (disp ,(- disp-symbol-system-value symbol-tag)
                    ,cp-register) ,cp-register]
        [movl ,(argc-convention 3) %eax]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]
        [label ,L2]
        [movl (obj ,symbol) (disp -4 %esp)]
        [movl (obj top-level-value) ,cp-register]
        [movl (disp ,(- disp-symbol-system-value symbol-tag)
                    ,cp-register) ,cp-register]
        [movl ,(argc-convention 1) %eax]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]))
    (let ([ls encountered-symbol-calls])
      (let ([c* (map code-list ls)])
        (let ([c* (list*->code* (lambda (x) #f) c*)])
          (let ([p* (map (lambda (x) ($code->closure x)) c*)])
            (let f ([ls ls] [p* p*])
              (cond
                [(null? ls) (prm 'nop)]
                [else 
                 (make-seq
                   (with-tmp ([p (V (K (car p*)))] [s (V (K (car ls)))])
                     (E (prm '$init-symbol-function! s p)))
                   (f (cdr ls) (cdr p*)))])))))))
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (let ([code* (map Clambda code*)]
             [body (V body)])
         (make-codes code*
           (make-seq (error-codes) body)))]
      [else (error 'specify-rep "invalid program ~s" x)]))

  (define (specify-representation x)
    (let ([x (Program x)])
      x))

  (include "pass-specify-rep-primops.ss"))