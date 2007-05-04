

;;; 9.0: * calls (gensym <symbol>) instead of 
;;;        (gensym (symbol->string <symbol>)) in order to avoid incrementing
;;;        gensym-count.
;;; 6.7: * open-coded top-level-value, car, cdr
;;; 6.2: * side-effects now modify the dirty-vector
;;;      * added bwp-object?
;;;      * added pointer-value
;;;      * added tcbuckets
;;; 6.1: * added case-lambda, dropped lambda
;;; 6.0: * basic compiler

(library (ikarus compiler)
  (export)
  (import (scheme))


(let ()

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
          #'(if ($record/rtd? v rtd)
                (let ([rec-field* ($record-ref v id*)] ...)
                  b b* ...)
                altern))]))
    (syntax-case x ()
      [(_ expr cls* ...)
       (with-syntax ([body (generate-body #'_ #'(cls* ...))])
         #'(let ([v expr]) body))])))

(include "set-operations.ss")


(define open-coded-primitives
;;; these primitives, when found in operator position with the correct
;;; number of arguments, will be open-coded by the generator.  If an 
;;; incorrect number of args is detected, or if they appear in non-operator
;;; position, then they cannot be open-coded, and the pcb-primitives table
;;; is consulted for a reference of the pcb slot containing the primitive.
;;; If it's not found there, an error is signalled.
;;;
;;; prim-name          args
  '([$constant-ref      1   value]
    [$constant-set!     2   effect]
    [$pcb-ref           1   value]
    [$pcb-set!          2   effect]
    ;;; type predicates
    [fixnum?            1   pred]
    [bignum?            1   pred]
    [flonum?            1   pred]
    [immediate?         1   pred]
    [boolean?           1   pred]
    [char?              1   pred]
    [pair?              1   pred]
    [symbol?            1   pred]
    [vector?            1   pred]
    [string?            1   pred]
    [procedure?         1   pred]
    [null?              1   pred]
    [eof-object?        1   pred]
    [bwp-object?        1   pred]
    [$unbound-object?   1   pred]
    [$forward-ptr?      1   pred]
    [not                1   pred]
    [pointer-value      1  value]
    [eq?                2   pred]
    ;;; fixnum primitives
    [$fxadd1            1   value]
    [$fxsub1            1   value]
    [$fx+               2   value]
    [$fx-               2   value]
    [$fx*               2   value]
    [$fxsll             2   value]
    [$fxsra             2   value]
    [$fxlogand          2   value]
    [$fxlogor           2   value]
    [$fxlogxor          2   value]
    [$fxlognot          1   value]
    [$fxquotient        2   value]
    [$fxmodulo          2   value]
    ;;; fixnum predicates
    [$fxzero?           1   pred]
    [$fx=               2   pred]
    [$fx<               2   pred]
    [$fx<=              2   pred]
    [$fx>               2   pred]
    [$fx>=              2   pred]
    ;;; character predicates
    [$char=             2   pred]
    [$char<             2   pred]
    [$char<=            2   pred]
    [$char>             2   pred]
    [$char>=            2   pred]
    ;;; character conversion
    [$fixnum->char      1   value]
    [$char->fixnum      1   value]
    ;;; lists/pairs
    [cons               2   value]
    [list*         positive value]
    [list             any   value]
    [car                1   value]
    [cdr                1   value]
    [$car               1   value]
    [$cdr               1   value]
    [$set-car!          2   effect]
    [$set-cdr!          2   effect]
    ;;; vectors
    [$make-vector       1   value]
    [vector           any   value]
    [$vector-length     1   value]
    [$vector-ref        2   value]
    [$vector-set!       3   effect]
    ;;; strings
    [$make-string       1   value]
    [$string          any   value]
    [$string-length     1   value]
    [$string-ref        2   value]
    [$string-set!       3   effect]
    ;;; symbols
    [$make-symbol       1   value]
    [$symbol-value      1   value]
    [$symbol-string     1   value]
    [$symbol-unique-string     1   value]
    [$set-symbol-value! 2   effect]
    [$set-symbol-function! 2   effect]
    [$set-symbol-string! 2   effect]
    [$set-symbol-unique-string! 2   effect]
    [$symbol-plist      1   value]
    [$set-symbol-plist! 2   effect]
    [primitive-ref 1 value]
    [primitive-set! 2 effect]
    [top-level-value      1   value]
    ;;; ports
    [port?                    1 pred]
    [input-port?              1 pred]
    [output-port?             1 pred]
    [$make-port/input         7 value]
    [$make-port/output        7 value]
    [$make-port/both          7 value]
    [$port-handler            1 value]
    [$port-input-buffer       1 value]
    [$port-input-index        1 value]
    [$port-input-size         1 value]
    [$port-output-buffer      1 value]
    [$port-output-index       1 value]
    [$port-output-size        1 value]
    [$set-port-input-index!   2 effect]
    [$set-port-input-size!    2 effect]
    [$set-port-output-index!  2 effect]
    [$set-port-output-size!   2 effect]
    ;;; tcbuckets
    [$make-tcbucket       4   value]
    [$tcbucket-key        1   value]
    [$tcbucket-val        1   value]
    [$tcbucket-next       1   value]
    [$set-tcbucket-val!   2  effect]
    [$set-tcbucket-next!  2  effect]
    [$set-tcbucket-tconc! 2  effect]
    ;;; misc
    [eof-object         0   value]
    [void               0   value]
    [$exit              1   effect]
    [$fp-at-base        0   pred]
    [$current-frame     0   value]
    [$arg-list          0   value]
    [$seal-frame-and-call 1  tail]
    [$frame->continuation 1 value]
    [$interrupted?       0   pred]
    [$unset-interrupted! 0 effect]
    ;;; 
    ;;; records
    ;;;
    [$make-record       2   value]
    [$record?           1    pred]
    [$record/rtd?       2    pred]
    [$record-rtd        1   value]
    [$record-ref        2   value]
    [$record-set!       3  effect]
    [$record          any   value]
    ;;;
    ;;; asm
    ;;;
    [$code?              1    pred]
    [$code-size          1    value]
    [$code-reloc-vector  1    value]
    [$code-freevars      1    value]
    [$code-ref           2    value]
    [$code-set!          3    value]
    [$code->closure      1    value]
    [$closure-code       1    value]
    ;;;
    [$make-call-with-values-procedure 0 value]
    [$make-values-procedure 0 value]
   ))

(define (primitive-context x)
  (cond
    [(assq x open-coded-primitives) => caddr]
    [else (error 'primitive-context "unknown prim ~s" x)]))

(define (open-codeable? x)
  (cond
    [(assq x open-coded-primitives) #t]
    [else  #f]))

(define (open-coded-primitive-args x)
  (cond
    [(assq x open-coded-primitives) => cadr]
    [else (error 'open-coded-primitive-args "invalid ~s" x)]))

;;; end of primitives table section

  
(define-record constant (value))
(define-record code-loc (label))
(define-record foreign-label (label))
(define-record var 
   (name assigned referenced 
         reg-conf frm-conf var-conf reg-move frm-move var-move
         loc index))
(define-record cp-var (idx))
(define-record frame-var (idx))
(define-record new-frame (base-idx size body))
(define-record save-cp (loc))
(define-record eval-cp (check body))
(define-record return (value))
(define-record call-cp
  (call-convention label save-cp? rp-convention base-idx arg-count live-mask))
(define-record tailcall-cp (convention label arg-count))
(define-record primcall (op arg*))
(define-record primref (name))
(define-record conditional (test conseq altern))
(define-record interrupt-call (test handler))
(define-record bind (lhs* rhs* body))
(define-record recbind (lhs* rhs* body))
(define-record fix (lhs* rhs* body))

(define-record seq (e0 e1))
(define-record case-info (label args proper))
(define-record clambda-case (info body))
(define-record clambda (label cases free))
(define-record closure (code free*))
(define-record funcall (op rand*))
(define-record jmpcall (label op rand*))
(define-record forcall (op rand*))
(define-record codes (list body))
(define-record assign (lhs rhs))
(define-record mvcall (producer consumer))



(define-record shortcut (body handler))

(define-record fvar (idx))
(define-record object (val))
(define-record locals (vars body))
(define-record nframe (vars live body))
(define-record nfv (conf loc var-conf frm-conf nfv-conf))
(define-record ntcall (target value args mask size))
(define-record asm-instr (op dst src))
(define-record disp (s0 s1))

(define mkfvar
  (let ([cache '()])
    (lambda (i)
      (cond
        [(fixnum? i)
         (cond
           [(assv i cache) => cdr]
           [else
            (let ([fv (make-fvar i)])
              (set! cache (cons (cons i fv) cache))
              fv)])]
        [else (error 'mkfvar "~s is not a fixnum" i)]))))

(define (unique-var x)
  (make-var (gensym x) #f #f #f #f #f #f #f #f #f #f))

(define (recordize x)
  (define *cookie* (gensym))
  (define (gen-fml* fml*)
    (cond
      [(pair? fml*)
       (let ([v (unique-var (car fml*))])
         (putprop (car fml*) *cookie* v)
         (cons v (gen-fml* (cdr fml*))))]
      [(symbol? fml*)
       (let ([v (unique-var fml*)])
         (putprop fml* *cookie* v)
         v)]
      [else '()]))
  (define (ungen-fml* fml*)
    (cond
      [(pair? fml*)
       (remprop (car fml*) *cookie*)
       (ungen-fml* (cdr fml*))]
      [(symbol? fml*)
       (remprop fml* *cookie*)]))
  (define (properize fml*)
    (cond
      [(pair? fml*)
       (cons (car fml*) (properize (cdr fml*)))]
      [(null? fml*) '()]
      [else (list fml*)]))
  (define (quoted-sym x)
    (if (and (list? x)
             (fx= (length x) 2)
             (eq? 'quote (car x))
             (symbol? (cadr x)))
        (cadr x)
        (error 'quoted-sym "not a quoted symbol ~s" x)))
  (define (quoted-string x)
    (if (and (list? x)
             (fx= (length x) 2)
             (eq? 'quote (car x))
             (string? (cadr x)))
        (cadr x)
        (error 'quoted-string "not a quoted string ~s" x)))
  (define (Var x)
    (or (getprop x *cookie*) 
        (error 'recordize "unbound ~s" x)))
  (define (E x)
    (cond
      [(pair? x)
       (case (car x)
         [(quote) (make-constant (cadr x))]
         [(if) 
          (make-conditional 
            (E (cadr x))
            (E (caddr x))
            (E (cadddr x)))]
         [(set!)
          (let ([lhs (cadr x)] [rhs (caddr x)])
            (make-assign (Var lhs) (E rhs)))]
         [(begin)
          (let f ([a (E (cadr x))] [d (cddr x)])
            (cond
              [(null? d) a]
              [else
               (f (make-seq a (E (car d))) (cdr d))]))]
         [(letrec)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [rhs* (map cadr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (let ([expr (make-recbind nlhs* (map E rhs*) (E body ))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(case-lambda)
          (let ([cls*
                 (map
                   (lambda (cls)
                     (let ([fml* (car cls)] [body (cadr cls)])
                       (let ([nfml* (gen-fml* fml*)])
                         (let ([body (E body)])
                           (ungen-fml* fml*)
                           (make-clambda-case 
                             (make-case-info
                               (gensym)
                               (properize nfml*) 
                               (list? fml*)) 
                             body)))))
                   (cdr x))])
            (make-clambda (gensym) cls* #f))]
         [(foreign-call)
          (let ([name (quoted-string (cadr x))] [arg* (cddr x)])
            (make-forcall name (map E arg*)))]
         [(|#primitive|)
          (let ([var (cadr x)])
            (make-primref var))]
         [(top-level-value)
          (let ([var (quoted-sym (cadr x))])
             (make-funcall
               (make-primref 'top-level-value)
               (list (make-constant var))))]
         [(set-top-level-value!)
          (make-funcall (make-primref 'set-top-level-value!)
                        (map E (cdr x)))]
         [(void) 
          (make-constant (void))]
         [else
          (make-funcall (E (car x)) (map E (cdr x)))])]
      [(symbol? x) (Var x)]
      [else (error 'recordize "invalid expression ~s" x)]))
  (E x))

(define (unparse x)
  (define (E-args proper x)
    (if proper 
        (map E x)
        (let f ([a (car x)] [d (cdr x)])
          (cond
            [(null? d) (E a)]
            [else (cons (E a) (f (car d) (cdr d)))]))))
  (define (E x)
    (record-case x
      [(constant c) `(quote ,c)]
      [(code-loc x) `(code-loc ,x)]
      [(var x) (string->symbol (format "v:~a" x))]
      [(primref x) x]
      [(conditional test conseq altern) 
       `(if ,(E test) ,(E conseq) ,(E altern))]
      [(interrupt-call e0 e1)
       `(interrupt-call ,(E e0) ,(E e1))]
      [(primcall op arg*) `(,op . ,(map E arg*))]
      [(bind lhs* rhs* body) 
       `(let ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(recbind lhs* rhs* body) 
       `(letrec ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(fix lhs* rhs* body) 
       `(fix ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(seq e0 e1) 
       (let ()
         (define (f x ac)
           (record-case x
             [(seq e0 e1) (f e0 (f e1 ac))]
             [else (cons (E x) ac)]))
         (cons 'begin (f e0 (f e1 '()))))]
      [(clambda-case info body)
       `(,(E-args (case-info-proper info)
                               (case-info-args info))
            ,(E body))]
      [(clambda g cls* free)
       `(,g (case-lambda . ,(map E cls*)))]
      [(clambda label clauses free)
       `(code ,label . ,(map E clauses))]
      [(closure code free*)
       `(closure ,(E code) ,(map E free*))]
      [(codes list body)
       `(codes ,(map E list)
          ,(E body))]
      [(funcall rator rand*) `(funcall ,(E rator) . ,(map E rand*))]
      [(jmpcall label rator rand*)
       `(jmpcall ,label ,(E rator) . ,(map E rand*))]
      [(forcall rator rand*) `(foreign-call ,rator . ,(map E rand*))]
      [(assign lhs rhs) `(set! ,(E lhs) ,(E rhs))]
      [(return x) `(return ,(E x))]
      [(new-frame base-idx size body)
       `(new-frame [base: ,base-idx]
                   [size: ,size]
          ,(E body))]
      [(frame-var idx) 
       (string->symbol (format "fv.~a" idx))]
      [(cp-var idx) 
       (string->symbol (format "cp.~a" idx))]
      [(save-cp expr)
       `(save-cp ,(E expr))]
      [(eval-cp check body)
       `(eval-cp ,check ,(E body))]
      [(call-cp call-convention label save-cp? rp-convention base-idx arg-count live-mask)
       `(call-cp [conv: ,call-convention]
                 [label: ,label]
                 [rpconv: ,(if (symbol? rp-convention)
                               rp-convention
                               (E rp-convention))]
                 [base-idx: ,base-idx]
                 [arg-count: ,arg-count]
                 [live-mask: ,live-mask])]
      [(tailcall-cp convention label arg-count)
       `(tailcall-cp ,convention ,label ,arg-count)]
      [(foreign-label x) `(foreign-label ,x)]
      [(mvcall prod cons) `(mvcall ,(E prod) ,(E cons))]
      [(fvar idx) (string->symbol (format "fv.~a" idx))]
      [(nfv idx) 'nfv]
      [(locals vars body) `(locals ,(map E vars) ,(E body))]
      [(asm-instr op d s)
       `(asm ,op ,(E d) ,(E s))]
      [(disp s0 s1)
       `(disp ,(E s0) ,(E s1))]
      [(nframe vars live body) `(nframe ;[vars: ,(map E vars)]
                                        ;[live: ,(map E live)]
                                  ,(E body))]
      [(shortcut body handler)
       `(shortcut ,(E body) ,(E handler))]
      [(ntcall target valuw args mask size)
       `(ntcall ,target ,size)]
      [else
       (if (symbol? x) 
           x
           "#<unknown>")]))
  (E x))

(define open-mvcalls (make-parameter #t))

(define (optimize-direct-calls x)
  (define who 'optimize-direct-calls)
  (define (make-conses ls)
    (cond
      [(null? ls) (make-constant '())]
      [else 
       (make-primcall 'cons 
         (list (car ls) (make-conses (cdr ls))))]))      
  (define (properize lhs* rhs*)
    (cond
      [(null? lhs*) (error who "improper improper")]
      [(null? (cdr lhs*)) 
       (list (make-conses rhs*))]
      [else (cons (car rhs*) (properize (cdr lhs*) (cdr rhs*)))]))
  (define (inline-case cls rand*)
    (record-case cls
      [(clambda-case info body)
       (record-case info
         [(case-info label fml* proper)
          (if proper
              (and (fx= (length fml*) (length rand*))
                   (make-bind fml* rand* body))
              (and (fx<= (length fml*) (length rand*))
                   (make-bind fml* (properize fml* rand*) body)))])]))
  (define (try-inline cls* rand* default)
    (cond
      [(null? cls*) default]
      [(inline-case (car cls*) rand*)]
      [else (try-inline (cdr cls*) rand* default)]))
  (define (inline rator rand*)
    (define (valid-mv-consumer? x)
      (record-case x
        [(clambda L cases F)
         (and (fx= (length cases) 1)
              (record-case (car cases)
                [(clambda-case info body)
                 (record-case info
                   [(case-info L args proper) proper])]))]
        [else #f]))
    (define (single-value-consumer? x)
      (record-case x
        [(clambda L cases F)
         (and (fx= (length cases) 1)
              (record-case (car cases)
                [(clambda-case info body)
                 (record-case info
                   [(case-info L args proper)
                    (and proper (fx= (length args) 1))])]))]
        [else #f])) 
    (define (valid-mv-producer? x)
      (record-case x
        [(funcall) #t]
        [(conditional) #f]
        [(bind lhs* rhs* body) (valid-mv-producer? body)]
        [else #f] ;; FIXME BUG
       ; [else (error 'valid-mv-producer? "unhandles ~s"
       ;              (unparse x))]
        ))
    (record-case rator
      [(clambda g cls*)
       (try-inline cls* rand*
          (make-funcall rator rand*))]
      [(primref op)
       (case op
         ;;; FIXME HERE
         [(call-with-values)
          (cond
            [(and (open-mvcalls) (fx= (length rand*) 2))
             (let ([producer (inline (car rand*) '())] 
                   [consumer (cadr rand*)])
               (cond
                 [(single-value-consumer? consumer)
                  (inline consumer (list producer))]
                 [(and (valid-mv-consumer? consumer)
                       (valid-mv-producer? producer))
                  (make-mvcall producer consumer)]
                 [else 
                  (make-funcall rator rand*)]))]
            [else
             (make-funcall rator rand*)])]
         [else
          (make-funcall rator rand*)])]
      [else (make-funcall rator rand*)]))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(recbind lhs* rhs* body)
       (make-recbind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test)
         (Expr conseq)
         (Expr altern))]
      [(seq e0 e1) 
       (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls*)
       (make-clambda g
         (map (lambda (x)
                (record-case x
                  [(clambda-case info body)
                   (make-clambda-case info (Expr body))]))
              cls*)
         #f)]
      [(primcall rator rand*) 
       (make-primcall rator (map Expr rand*))]
      [(funcall rator rand*)
       (inline (Expr rator) (map Expr rand*))]
      [(forcall rator rand*) 
       (make-forcall rator (map Expr rand*))]
      [(assign lhs rhs)
       (make-assign lhs (Expr rhs))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x))

(define lambda-both 0)
(define lambda-producer 0)
(define lambda-consumer 0)
(define lambda-none 0)
(define branching-producer 0)

(define (analyze-cwv x)
  (define who 'analyze-cwv)
  (define (lambda? x)
    (record-case x
      [(clambda) #t]
      [else      #f]))
  (define (branching-producer? x)
    (define (bt? x)
      (record-case x
        [(bind lhs* rhs* body) (bt? body)]
        [(recbind lhs* rhs* body) (bt? body)]
        [(conditional test conseq altern) #t]
        [(seq e0 e1) (bt? e1)]
        [else #f]))
    (define (branching-clause? x)
      (record-case x 
        [(clambda-case info body) 
         (bt? body)]))
    (record-case x
      [(clambda g cls*)
       (ormap branching-clause? cls*)]
      [else #f]))
  (define (analyze producer consumer)
    (cond
      [(and (lambda? producer) (lambda? consumer))
       (set! lambda-both (fxadd1 lambda-both))]
      [(lambda? producer) 
       (set! lambda-producer (fxadd1 lambda-producer))]
      [(lambda? consumer)
       (set! lambda-consumer (fxadd1 lambda-consumer))]
      [else 
       (set! lambda-none (fxadd1 lambda-none))])
    (when (branching-producer? producer)
      (set! branching-producer (fxadd1 branching-producer)))
    (printf "both=~s p=~s c=~s none=~s branching-prod=~s\n"
            lambda-both lambda-producer lambda-consumer lambda-none
            branching-producer))
  (define (E x)
    (record-case x
      [(constant) (void)]
      [(var) (void)]
      [(primref) (void)]
      [(bind lhs* rhs* body)
       (for-each E rhs*) (E body)]
      [(recbind lhs* rhs* body)
       (for-each E rhs*) (E body)]
      [(conditional test conseq altern)
       (E test)
       (E conseq)
       (E altern)]
      [(seq e0 e1) (E e0) (E e1)]
      [(clambda g cls*)
       (for-each 
         (lambda (x)
           (record-case x
             [(clambda-case info body) (E body)]))
         cls*)]
      [(primcall rator rand*) 
       (for-each E rand*)
       (when (and (eq? rator 'call-with-values) (fx= (length rand*) 2))
         (analyze (car rand*) (cadr rand*)))]
      [(funcall rator rand*)
       (E rator) (for-each E rand*)
       (when (and (record-case rator
                    [(primref op) (eq? op 'call-with-values)]
                    [else #f])
                  (fx= (length rand*) 2))
         (analyze (car rand*) (cadr rand*)))]
      [(forcall rator rand*) 
       (for-each E rand*)]
      [(assign lhs rhs)
       (E rhs)]
      [else (error who "invalid expression ~s" (unparse x))]))
  (E x))

(define (optimize-letrec x)
  (define who 'optimize-letrec)
  (define (extend-hash lhs* h ref)
    (for-each (lambda (lhs) (put-hash-table! h lhs #t)) lhs*)
    (lambda (x)
      (unless (get-hash-table h x #f)
        (put-hash-table! h x #t)
        (ref x))))
  (define (E* x* ref comp)
    (cond
      [(null? x*) '()]
      [else
       (cons (E (car x*) ref comp)
             (E* (cdr x*) ref comp))]))  
  (define (do-rhs* i lhs* rhs* ref comp vref vcomp)
    (cond
      [(null? rhs*) '()]
      [else
       (let ([h (make-hash-table)])
         (let ([ref
                (lambda (x)
                  (unless (get-hash-table h x #f)
                    (put-hash-table! h x #t)
                    (ref x)
                    (when (memq x lhs*)
                      (vector-set! vref i #t))))]
               [comp
                (lambda ()
                  (vector-set! vcomp i #t)
                  (comp))])
           (cons (E (car rhs*) ref comp)
                 (do-rhs* (fxadd1 i) lhs* (cdr rhs*) ref comp vref vcomp))))]))
  (define (partition-rhs* i lhs* rhs* vref vcomp)
    (cond
      [(null? lhs*) (values '() '() '() '() '() '())]
      [else
       (let-values 
         ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
           (partition-rhs* (fxadd1 i) (cdr lhs*) (cdr rhs*) vref vcomp)]
          [(lhs rhs) (values (car lhs*) (car rhs*))])
         (cond
           [(var-assigned lhs) 
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [(clambda? rhs)
            (values slhs* srhs* (cons lhs llhs*) (cons rhs lrhs*) clhs* crhs*)]
           [(or (vector-ref vref i) (vector-ref vcomp i))
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [else
            (values (cons lhs slhs*) (cons rhs srhs*) llhs* lrhs* clhs* crhs*)]
           ))]))
  (define (do-recbind lhs* rhs* body ref comp)
    (let ([h (make-hash-table)]
          [vref (make-vector (length lhs*) #f)]
          [vcomp (make-vector (length lhs*) #f)])
      (let* ([ref (extend-hash lhs* h ref)]
             [body (E body ref comp)])
        (let ([rhs* (do-rhs* 0 lhs* rhs* ref comp vref vcomp)])
          (let-values ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
                        (partition-rhs* 0 lhs* rhs* vref vcomp)])
            (let ([v* (map (lambda (x) (make-primcall 'void '())) clhs*)]
                  [t* (map (lambda (x) (unique-var 'tmp)) clhs*)])
              (make-bind slhs* srhs*
                (make-bind clhs* v*
                  (make-fix llhs* lrhs*
                    (make-bind t* crhs*
                      (build-assign* clhs* t* body)))))))))))
  (define (build-assign* lhs* rhs* body)
    (cond
      [(null? lhs*)  body]
      [else
       (make-seq
         (make-assign (car lhs*) (car rhs*))
         (build-assign* (cdr lhs*) (cdr rhs*) body))]))
  (define (E x ref comp)
    (record-case x
      [(constant) x]
      [(var) (ref x) x]
      [(assign lhs rhs)
       (set-var-assigned! lhs #t)
       (ref lhs)
       (make-assign lhs (E rhs ref comp))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let ([rhs* (E* rhs* ref comp)])
         (let ([h (make-hash-table)])
           (let ([body (E body (extend-hash lhs* h ref) comp)])
             (make-bind lhs* rhs* body))))]
      [(recbind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp))]
      [(conditional e0 e1 e2)
       (make-conditional (E e0 ref comp) (E e1 ref comp) (E e2 ref comp))]
      [(seq e0 e1) (make-seq (E e0 ref comp) (E e1 ref comp))]
      [(clambda g cls*)
       (make-clambda g
         (map (lambda (x)
                (record-case x
                  [(clambda-case info body)
                   (let ([h (make-hash-table)])
                     (let ([body (E body (extend-hash (case-info-args info) h ref) void)])
                       (make-clambda-case info body)))]))
              cls*)
         #f)]
      [(primcall rator rand*) 
       (when (memq rator '(call/cc call/cf))
         (comp))
       (make-primcall rator (E* rand* ref comp))]
      [(funcall rator rand*)
       (let ([rator (E rator ref comp)] [rand* (E* rand* ref comp)])
         (record-case rator
           [(primref op)
            (when (memq op '(call/cc call/cf))
              (comp))]
           [else
            (comp)])
         (make-funcall rator rand*))]
      [(mvcall p c)
       (let ([p (E p ref comp)] [c (E c ref comp)])
         (comp)
         (make-mvcall p c))]
      [(forcall rator rand*) 
       (make-forcall rator (E* rand* ref comp))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (E x (lambda (x) (error who "free var ~s found" x))
       void))

;;; This pass was here before optimize-letrec was implemented.
(define (remove-letrec x)
  (define who 'remove-letrec)
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(recbind lhs* rhs* body)
       (let ([t* (map (lambda (lhs) (unique-var 'tmp)) lhs*)]
             [v* (map (lambda (lhs) (make-primcall 'void '())) lhs*)])
         (make-bind lhs* v*
           (make-bind t* (map Expr rhs*)
             (let f ([lhs* lhs*] [t* t*])
               (cond
                 [(null? lhs*) (Expr body)]
                 [else
                  (make-seq
                    (make-assign (car lhs*) (car t*))
                    (f (cdr lhs*) (cdr t*)))])))))]
      ;[(fix lhs* rhs* body)
      ; (Expr (make-recbind lhs* rhs* body))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test)
         (Expr conseq)
         (Expr altern))]
      [(seq e0 e1) 
       (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls*)
       (make-clambda g
         (map (lambda (x)
                (record-case x
                  [(clambda-case info body)
                   (make-clambda-case info (Expr body))]))
              cls*)
         #f)]
      [(primcall rator rand*) 
       (make-primcall rator (map Expr rand*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(forcall rator rand*) 
       (make-forcall rator (map Expr rand*))]
      [(assign lhs rhs)
       (make-assign lhs (Expr rhs))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x))

(define (uncover-assigned/referenced x)
  (define who 'uncover-assigned/referenced)
  (define (Expr* x*)
    (for-each Expr x*))
  (define (init-var x)
    (set-var-assigned! x #f)
    (set-var-referenced! x #f))
  (define (Expr x)
    (record-case x
      [(constant) (void)]
      [(var) (set-var-referenced! x #t)]
      [(primref) (void)]
      [(bind lhs* rhs* body)
       (for-each init-var lhs*)
       (begin (Expr body) (Expr* rhs*))]
      [(fix lhs* rhs* body)
       (for-each init-var lhs*)
       (Expr* rhs*)
       (Expr body)
       (when (ormap var-assigned lhs*)
         (error who "a fix lhs is assigned"))]
      [(conditional test conseq altern)
       (begin (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (begin (Expr e0) (Expr e1))]
      [(clambda g cls*)
       (for-each
         (lambda (cls)
           (record-case cls
             [(clambda-case info body)
              (for-each init-var (case-info-args info))
              (Expr body)]))
         cls*)]
      [(primcall rator rand*) (Expr* rand*)]
      [(funcall rator rand*)
       (begin (Expr rator) (Expr* rand*))]
      [(mvcall p c) (begin (Expr p) (Expr c))]
      [(forcall rator rand*) (Expr* rand*)]
      [(assign lhs rhs)
       (set-var-assigned! lhs #t)
       (Expr rhs)]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x)
  x)

(module (tally-giveup)
  (define giveup-list '())
  (define (tally-giveup op)
    (cond
      [(getprop op '*compiler-giveup-tally*) =>
       (lambda (n)
         (putprop op '*compiler-giveup-tally* (add1 n)))]
      [else 
       (set! giveup-list (cons op giveup-list))
       (putprop op '*compiler-giveup-tally* 1)]))
  (define (print-tally)
    (for-each 
      (lambda (x)
        (let ([n (getprop x '*compiler-giveup-tally*)])
          (when (>= n 30)
            (printf "~s ~s\n" n x))))
      giveup-list))
  (primitive-set! 'compiler-giveup-tally print-tally))

#|FIXME:missing-optimizations
  128 list*
  111 cadr
  464 $record/rtd?
  404 memq
  249 map
  114 not
  451 car
  224 syntax-error
  248 $syntax-dispatch
  237 pair?
  125 length
  165 $cdr
  137 $car
  805 $record-ref
  181 fixnum?
  328 null?
  136 fx-
  207 eq?
  153 call-with-values
  165 values
  336 apply
  384 cdr
  898 cons
  747 error
  331 primitive-set!
  555 void
  645 list
|#

(module (optimize-primcall)
  (define (optimize-primcall ctxt op rand*)
    (cond
      [(getprop op *cookie*) =>
       (lambda (proc)
         (proc ctxt op rand* 
               (lambda () 
                 (make-funcall (make-primref op) rand*))))]
      [else
       (tally-giveup op)
       (make-funcall (make-primref op) rand*)]))
  (define (constant-value x k) 
    (record-case x 
      [(constant t) (k t)] ; known
      [(bind lhs* rhs* body) (constant-value body k)]
      [(fix lhs* rhs* body) (constant-value body k)]
      [(seq e0 e1) (constant-value e1 k)]
      [else #f]))
  (define (mk-seq e0 e1)  ;;; keep e1 seq-free.
    (cond
      [(and (primcall? e0) (eq? (primcall-op e0) 'void)) e1]
      [(seq? e1)
       (make-seq (make-seq e0 (seq-e0 e1)) (seq-e1 e1))]
      [else
       (make-seq e0 e1)]))
  (define (equable? x)
    (if (number? x) (fixnum? x) #t))
  (define *cookie* (gensym "optimizer-cookie"))
  (define-syntax set-cases
    (syntax-rules ()
      [(_ ctxt op rand* giveup 
          [(op** ...) b* b** ...] ...)
       (begin
         (let ([p (lambda (ctxt op rand* giveup) b* b** ...)])
           (putprop 'op** *cookie* p) ...
           (void)) ...)]))
  (set-cases ctxt op rand* giveup
    [(eq?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or
                  (constant-value a0
                    (lambda (x0)
                      (constant-value a1
                        (lambda (x1)
                          (mk-seq (mk-seq a0 a1)
                            (make-constant (eq? x0 x1)))))
                  (and (eq? ctxt 'e)
                       (mk-seq a0 a1)))))))
         (giveup))] 
    [(eqv?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or
                  (constant-value a0
                    (lambda (x0)
                      (or (constant-value a1
                            (lambda (x1)
                              (mk-seq (mk-seq a0 a1)
                                (make-constant (eqv? x0 x1)))))
                          (and (equable? x0)
                               (optimize-primcall ctxt 'eq? rand*)))))
                  (constant-value a1
                    (lambda (x1)
                      (and (equable? x1)
                           (optimize-primcall ctxt 'eq? rand*))))
                  (and (eq? ctxt 'e)
                       (mk-seq a0 a1)))))
         (giveup))]
    [(memv)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (constant-value a1
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'e) (mk-seq a0 a1)]
                      [(constant-value a0
                         (lambda (x)
                           (mk-seq (mk-seq a0 a1)
                             (case ctxt
                               [(v) (make-constant (memv x ls))]
                               [else (make-constant
                                       (if (memv x ls) #t #f))]))))]
                      [(andmap equable? ls)
                       (optimize-primcall ctxt 'memq rand*)]
                      [(fx= (length ls) 1)
                       (mk-seq a1
                         (optimize-primcall ctxt 'eqv?
                            (list a0 (make-constant (car ls)))))]
                      [else #f])))))
         (giveup))]
    [(memq) 
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (constant-value a1
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'e) (make-seq a0 a1)]
                      [(constant-value a0
                         (lambda (x)
                           (mk-seq (mk-seq a0 a1)
                             (case ctxt
                               [(v) (make-constant (memq x ls))]
                               [else (make-constant
                                       (if (memq x ls) #t #f))]))))]
                      [(fx= (length ls) 1)
                       (mk-seq a1
                         (optimize-primcall ctxt 'eq?
                           (list a0 (make-constant (car ls)))))]
                      [else (make-primcall '$memq rand*)])))))
         (giveup))]
    [(list)
     (case ctxt
       [(v) (if (null? rand*) (make-constant '()) (giveup))]
       [else
        (if (null? rand*)
            (make-constant #t)
            (let f ([a (car rand*)] [d (cdr rand*)])
              (cond
                [(null? d) (make-seq a (make-constant #t))]
                [else
                 (f (make-seq a (car d)) (cdr d))])))])]
    [(list*)
     (case ctxt
       [(e) 
        (cond
          [(null? rand*) (giveup)]
          [else
           (let f ([a (car rand*)] [d (cdr rand*)])
             (cond
               [(null? d) a]
               [else (f (mk-seq a (car d)) (cdr d))]))])]
       [(p) 
        (cond
          [(null? rand*) (giveup)]
          [(null? (cdr rand*)) 
           (let ([a (car rand*)])
             (or (constant-value a
                   (lambda (v)
                     (mk-seq a (make-constant (if v #t #f)))))
                 a))]
          [else 
           (let f ([a (car rand*)] [d (cdr rand*)])
             (cond
               [(null? d) (mk-seq a (make-constant #t))]
               [else (f (mk-seq a (car d)) (cdr d))]))])]
       [else
        (cond
          [(null? rand*) (giveup)]
          [(null? (cdr rand*)) (car rand*)]
          [else (giveup)])])]
    [(cons)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (case ctxt
                  [(e) (mk-seq a0 a1)]
                  [(p) (mk-seq (mk-seq a0 a1) (make-constant #t))]
                  [else (giveup)])))
         (giveup))]
    [($record-ref $record/rtd?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (case ctxt
                  [(e) (mk-seq a0 a1)]
                  [else 
                   (or (constant-value a1
                         (lambda (n1)
                           (mk-seq a1
                             (make-primcall op
                                (list a0 (make-constant n1))))))
                       (make-primcall op rand*))])))
         (error 'optimize "~s rands to ~s" (map unparse rand*) op))]
    [(void)
     (or (and (null? rand*)
              (case ctxt
                [(p) (make-constant #t)]
                [else (make-constant (void))]))
         (giveup))]
    [(car cdr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (constant-value a
                  (lambda (v)
                    (and (pair? v)
                         (mk-seq a
                           (make-constant
                             (case op
                               [(car) (car v)]
                               [else  (cdr v)]))))))))
         (giveup))]
    [(cadr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (and (pair? v)
                             (pair? (cdr v))
                             (mk-seq a
                               (make-constant
                                 (cadr v))))))
                    (make-primcall op rand*))))
         (giveup))] 
    [(not null? pair? fixnum? vector? string? char? symbol?
      eof-object?)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (case ctxt
                  [(e) a]
                  [else
                   (or (constant-value a
                         (lambda (v)
                           (mk-seq a
                             (make-constant
                               (case op
                                 [(not) (not v)]
                                 [(null?) (null? v)]
                                 [(pair?) (pair? v)]
                                 [(fixnum?) (fixnum? v)]
                                 [(vector?) (vector? v)]
                                 [(string?) (string? v)]
                                 [(char?) (char? v)]
                                 [(symbol?) (symbol? v)]
                                 [(eof-object?) (eof-object? v)]
                                 [else 
                                  (error 'optimize
                                    "huh ~s" op)])))))
                       (make-primcall op rand*))])))
         (giveup))]
    [($car $cdr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (if (pair? v)
                            (make-seq a
                              (make-constant
                                (case op
                                  [($car) (car v)]
                                  [else   (cdr v)])))
                            (error 'optimize
                                   "incorrect arg ~s to ~s"
                                   v op))))
                    (giveup))))
         (error 'optimize "incorrect args ~s to ~s"
                (map unparse rand*) op))]
    [(fxadd1 fxsub1)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (and (fixnum? v)
                             (let ([t 
                                    (case op
                                      [(fxadd1) (add1 v)]
                                      [else     (sub1 v)])])
                                (and (fixnum? t)
                                     (mk-seq a 
                                       (make-constant t)))))))
                    (make-primcall op rand*))))
         (giveup))]
    [(fx+)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or (constant-value a1
                      (lambda (v1)
                        (and (fixnum? v1)
                             (or (constant-value a0
                                   (lambda (v0)
                                     (and (fixnum? v0)
                                          (let ([r (+ v0 v1)])
                                            (and (fixnum? r)
                                                 (mk-seq (mk-seq a0 a1) 
                                                   (make-constant r)))))))
                                 (mk-seq a1 
                                   (make-primcall op 
                                     (list a0 (make-constant v1))))))))
                    (constant-value a0
                      (lambda (v0)
                        (and (fixnum? v0)
                             (mk-seq a0 
                               (make-primcall op
                                  (list (make-constant v0) a1))))))
                    (make-primcall op rand*))))
         (giveup))]
    ;X; [(fx- fx+ fx*)
    ;X;  (or (and (fx= (length rand*) 2)
    ;X;           (let ([a0 (car rand*)] [a1 (cadr rand*)])
    ;X;             (or (constant-value a1
    ;X;                   (lambda (v1)
    ;X;                     (and (fixnum? v1)
    ;X;                          (or (constant-value a0
    ;X;                                (lambda (v0)
    ;X;                                  (and (fixnum? v0)
    ;X;                                       (let ([r (case op
    ;X;                                                  [(fx+) (+ v0 v1)]
    ;X;                                                  [(fx-) (- v0 v1)]
    ;X;                                                  [(fx*) (* v0 v1)]
    ;X;                                                  [else (error 'compile "BOO")])])
    ;X;                                         (and (fixnum? r)
    ;X;                                               (mk-seq (mk-seq a0 a1)
    ;X;                                                 (make-constant r)))))))
    ;X;                              (mk-seq a1 (make-primcall op (list a0 v1)))))))
    ;X;                 (constant-value a0
    ;X;                   (lambda (v0)
    ;X;                     (and (fixnum? v0)
    ;X;                          (mk-seq a0 (make-primcall op (list v0 a1))))))
    ;X;                 (make-primcall op (list a0 a1)))))
    ;X;      (giveup))]
    ;;; unoptimizables
    [(error syntax-error $syntax-dispatch $sc-put-cte 
      primitive-set!  apply) 
     (giveup)]
    ))


(define (mk-mvcall p c)
  (record-case p
    [(funcall) (make-mvcall p c)]
    [(seq e0 e1)
     (make-seq e0 (mk-mvcall e1 c))]
    [(bind lhs* rhs* body)
     (make-bind lhs* rhs* (mk-mvcall body c))]
    [else (error 'mk-mvcall "invalid producer ~s" (unparse p))]))


(define (copy-propagate x)
  (define who 'copy-propagate)
  (define the-void (make-primcall 'void '()))
  (define (known-value x) 
    (record-case x 
      [(constant) x] ; known
      [(primref)  x] ; known
      [(bind lhs* rhs* body) (known-value body)]
      [(fix lhs* rhs* body) (known-value body)]
      [(seq e0 e1) (known-value e1)]
      [else #f]))
  (define (same-values? x y)
    (cond
      [(constant? x)
       (and (constant? y) 
            (eq? (constant-value x) 
                 (constant-value y)))]
      [(primref? x)
       (and (primref? y)
            (eq? (primref-name x)
                 (primref-name y)))]
      [else #f]))
  (define (predicate-value x)
    (record-case x
      [(constant t) (if t 't 'f)]
      [(bind lhs rhs body) (predicate-value body)]
      [(fix lhs rhs body) (predicate-value body)]
      [(seq e0 e1) (predicate-value e1)]
      [else #f]))
  (define (do-conditional e0 e1 e2 k)
    (let ([e0 (Pred e0)])
      (cond
        [(predicate-value e0) =>
         (lambda (v)
           (if (eq? v 't) (k e1) (k e2)))]
        [else
         (make-conditional e0 (k e1) (k e2))])))
  (define (partition-referenced lhs* rhs*)
    (cond
      [(null? lhs*) (values '() '() the-void)]
      [else
       (let ([lhs (car lhs*)] [rhs (car rhs*)])
         (let-values ([(lhs* rhs* eff*) 
                       (partition-referenced
                           (cdr lhs*) (cdr rhs*))])
           (cond
             [(var-referenced lhs)
              (values (cons lhs lhs*) (cons rhs rhs*) eff*)]
             [else
              (values lhs* rhs* (mk-seq eff* (Effect rhs)))])))]))
  (define (partition/assign-known lhs* rhs*)
    (cond
      [(null? lhs*) (values '() '() the-void)]
      [else
       (let ([lhs (car lhs*)] [rhs (car rhs*)])
         (let-values ([(lhs* rhs* eff*) 
                       (partition/assign-known
                           (cdr lhs*) (cdr rhs*))])
           (cond
             [(and (not (var-assigned lhs)) (known-value rhs)) =>
              (lambda (v)
                (set-var-referenced! lhs v)
                (values lhs* rhs* (mk-seq eff* rhs)))]
             [else
              (values (cons lhs lhs*) (cons rhs rhs*) eff*)])))]))
  (define (do-bind lhs* rhs* body k) 
    (let-values ([(lhs* rhs* eff0)
                  (partition-referenced lhs* rhs*)])
      (let ([rhs* (map Value rhs*)])
        (let-values ([(lhs* rhs* eff1)
                      (partition/assign-known lhs* rhs*)])
          (let ([body
                 (cond
                   [(null? lhs*) (k body)]
                   [else
                    (make-bind lhs* rhs* (k body))])])
            (mk-seq (mk-seq eff0 eff1) body))))))
  (define (do-fix lhs* rhs* body k) 
    (let-values ([(lhs* rhs* eff*) 
                  (partition-referenced lhs* rhs*)])
      (cond
        [(null? lhs*) (k body)]
        [else
         (make-fix lhs* (map Value rhs*) (k body))])))
  (define (mk-seq e0 e1)  ;;; keep e1 seq-free.
    (cond
      [(and (primcall? e0) (eq? (primcall-op e0) 'void)) e1]
      [(primref? e0) e1]
      [(seq? e1)
       (make-seq (make-seq e0 (seq-e0 e1)) (seq-e1 e1))]
      [else
       (make-seq e0 e1)]))
  (define (do-clambda g cls*)
    (make-clambda g
      (map (lambda (cls)
             (record-case cls
               [(clambda-case info body)
                (make-clambda-case info (Value body))]))
           cls*)
      #f))
  (define (Effect x)
    (record-case x
      [(constant) the-void]
      [(var)      the-void]
      [(primref)  the-void]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body Effect)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* body Effect)]
      [(conditional e0 e1 e2)
       (let ([e0 (Pred e0)])
         (cond
           [(predicate-value e0) =>
            (lambda (v)
              (mk-seq e0 (if (eq? v 't) (Effect e1) (Effect e2))))]
           [else
            (make-conditional e0 (Effect e1) (Effect e2))]))]
      [(seq e0 e1) (mk-seq (Effect e0) (Effect e1))]
      [(clambda g cls*) the-void]
      [(primcall rator rand*) 
       (optimize-primcall 'e rator (map Value rand*))]
      [(funcall rator rand*)
       (let ([rator (Value rator)])
         (cond
           [(known-value rator) =>
            (lambda (v)
              (record-case v
                [(primref op)
                 (mk-seq rator
                    (optimize-primcall 'e op (map Value rand*)))]
                [else
                 (make-funcall rator (map Value rand*))]))]
           [else (make-funcall rator (map Value rand*))]))]
      [(forcall rator rand*) 
       (make-forcall rator (map Value rand*))]
      [(mvcall p c)
       (mk-mvcall (Value p) (Value c))]
      [(assign lhs rhs)
       (unless (var-assigned lhs)
         (error who "var ~s is not assigned" lhs))
       (if (var-referenced lhs)
           (make-assign lhs (Value rhs))
           (Effect rhs))]
      [else (error who "invalid effect expression ~s" (unparse x))]))
  (define (Pred x)
    (record-case x
      [(constant) x]
      [(var) 
       (let ([r (var-referenced x)])
         (if (constant? r) r x))]
      [(primref) (make-constant #t)]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body Pred)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* body Pred)]
      [(conditional e0 e1 e2)
       (let ([e0 (Pred e0)])
         (cond
           [(predicate-value e0) =>
            (lambda (t0)
              (mk-seq e0 (if (eq? t0 't) (Pred e1) (Pred e2))))]
           [else
            (let ([e1 (Pred e1)] [e2 (Pred e2)])
              (cond
                [(predicate-value e1) =>
                 (lambda (t1)
                   (cond
                     [(predicate-value e2) =>
                      (lambda (t2)
                        (if (eq? t1 t2)
                            (mk-seq (make-conditional e0 e1 e2) 
                                    (make-constant (if (eq? t1 't) #t #f)))
                            (make-conditional e0 e1 e2)))]
                     [else (make-conditional e0 e1 e2)]))]
                [else (make-conditional e0 e1 e2)]))]))]
      [(seq e0 e1) (mk-seq (Effect e0) (Pred e1))]
      [(clambda g cls*) (make-constant #t)]
      [(primcall rator rand*) 
       (optimize-primcall 'p rator (map Value rand*))]
      [(funcall rator rand*)
       (let ([rator (Value rator)])
         (cond
           [(known-value rator) =>
            (lambda (v)
              (record-case v
                [(primref op)
                 (mk-seq rator
                    (optimize-primcall 'p op (map Value rand*)))]
                [else
                 (make-funcall rator (map Value rand*))]))]
           [else (make-funcall rator (map Value rand*))]))]
      [(forcall rator rand*) 
       (make-forcall rator (map Value rand*))]
      [(assign lhs rhs)
       (mk-seq (Effect x) (make-constant #t))]
      [(mvcall p c)
       (mk-mvcall (Value p) (Value c))]
      [else (error who "invalid pred expression ~s" (unparse x))]))
  (define (Value x)
    (record-case x
      [(constant) x]
      [(var) 
       (let ([r (var-referenced x)])
         (case r
           [(#t) x]
           [(#f) (error who "Reference to a var ~s that should not be" x)]
           [else r]))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body Value)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* body Value)]
      [(conditional e0 e1 e2)
       (let ([e0 (Pred e0)])
         (cond
           [(predicate-value e0) =>
            (lambda (t0)
              (mk-seq e0 (if (eq? t0 't) (Value e1) (Value e2))))]
           [else
            (let ([e1 (Value e1)] [e2 (Value e2)])
              (let ([t1 (known-value e1)] [t2 (known-value e2)])
                (cond
                  [(and t1 t2) 
                   (if (same-values? t1 t2)
                       (mk-seq (make-conditional e0 e1 e2) t1)
                       (make-conditional e0 e1 e2))]
                  [else (make-conditional e0 e1 e2)])))]))]
      [(seq e0 e1) (mk-seq (Effect e0) (Value e1))]
      [(clambda g cls*) (do-clambda g cls*)]
      [(primcall rator rand*) 
       (optimize-primcall 'v rator (map Value rand*))]
      [(funcall rator rand*)
       (let ([rator (Value rator)])
         (cond
           [(known-value rator) =>
            (lambda (v)
              (record-case v
                [(primref op)
                 (mk-seq rator
                    (optimize-primcall 'v op (map Value rand*)))]
                [else
                 (make-funcall rator (map Value rand*))]))]
           [else (make-funcall rator (map Value rand*))]))]
      [(forcall rator rand*) 
       (make-forcall rator (map Value rand*))]
      [(assign lhs rhs)
       (mk-seq (Effect x) the-void)]
      [(mvcall p c)
       (mk-mvcall (Value p) (Value c))]
      [else (error who "invalid value expression ~s" (unparse x))]))
  (let ([x (Value x)])
    ;;; since we messed up the references and assignments here, we
    ;;; redo them
    (uncover-assigned/referenced x)))


(define (rewrite-assignments x)
  (define who 'rewrite-assignments)
  (define (fix-lhs* lhs*)
    (cond
      [(null? lhs*) (values '() '() '())]
      [else
       (let ([x (car lhs*)])
         (let-values ([(lhs* a-lhs* a-rhs*) (fix-lhs* (cdr lhs*))])
           (cond
             [(var-assigned x)
              (let ([t (unique-var 'assignment-tmp)])
                (values (cons t lhs*) (cons x a-lhs*) (cons t a-rhs*)))]
             [else
              (values (cons x lhs*) a-lhs* a-rhs*)])))]))
  (define (bind-assigned lhs* rhs* body)
    (cond
      [(null? lhs*) body]
      [else
       (make-bind lhs*
         (map (lambda (rhs) (make-primcall 'vector (list rhs))) rhs*)
         body)]))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) 
       (cond
         [(var-assigned x) 
          (make-primcall '$vector-ref (list x (make-constant 0)))]
         [else x])]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let-values ([(lhs* a-lhs* a-rhs*) (fix-lhs* lhs*)]) 
         (make-bind lhs* (map Expr rhs*) 
           (bind-assigned a-lhs* a-rhs* (Expr body))))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls*) 
       (make-clambda g
         (map (lambda (cls)
                (record-case cls
                  [(clambda-case info body)
                   (record-case info
                     [(case-info label fml* proper)
                      (let-values ([(fml* a-lhs* a-rhs*) (fix-lhs* fml*)])
                        (make-clambda-case 
                          (make-case-info label fml* proper)
                          (bind-assigned a-lhs* a-rhs* (Expr body))))])]))
              cls*)
         #f)]
      [(primcall op rand*)
       (make-primcall op (map Expr rand*))]
      [(forcall op rand*)
       (make-forcall op (map Expr rand*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(assign lhs rhs)
       (unless (var-assigned lhs)
         (error 'rewrite-assignments "not assigned ~s in ~s" lhs x))
       (make-primcall '$vector-set! (list lhs (make-constant 0) (Expr rhs)))]
      [(mvcall p c) (make-mvcall (Expr p) (Expr c))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x))





(define (optimize-for-direct-jumps x)
  (define who 'optimize-for-direct-jumps)
  (define (init-var x)
    (set-var-referenced! x #f))
  (define (set-var x v)
    (record-case v
      [(clambda) (set-var-referenced! x v)]
      [(var) 
       (cond
         [(bound-var v) => (lambda (v) (set-var-referenced! x v))]
         [else (void)])]
      [else (void)]))
  (define (bound-var x)
    (var-referenced x))
  (define (optimize c rator rand*)
    (let ([n (length rand*)])
      (record-case c
        [(clambda main-label cls*)
         (let f ([cls* cls*])
           (cond
             [(null? cls*) 
              ;;; none matching?
              (make-funcall rator rand*)]
             [else
              (record-case (clambda-case-info (car cls*))
                [(case-info label fml* proper)
                 (cond
                   [proper
                    (if (fx= n (length fml*))
                        (make-jmpcall label rator rand*)
                        (f (cdr cls*)))]
                   [else
                    (if (fx<= (length (cdr fml*)) n)
                        (make-jmpcall label rator
                           (let f ([fml* (cdr fml*)] [rand* rand*])
                             (cond
                               [(null? fml*) 
                                (list (make-primcall 'list rand*))]
                               [else
                                (cons (car rand*)
                                      (f (cdr fml*) (cdr rand*)))])))
                        (f (cdr cls*)))])])]))])))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var)      x]
      [(primref)  x]
      [(bind lhs* rhs* body)
       (for-each init-var lhs*)
       (let ([rhs* (map Expr rhs*)])
         (for-each set-var lhs* rhs*)
         (make-bind lhs* rhs* (Expr body)))]
      [(fix lhs* rhs* body)
       (for-each set-var lhs* rhs*)
       (make-fix lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls*) 
       (make-clambda g
         (map (lambda (cls)
                (record-case cls
                  [(clambda-case info body)
                   (for-each init-var (case-info-args info))
                   (make-clambda-case info (Expr body))]))
              cls*)
         #f)]
      [(primcall op rand*)
       (make-primcall op (map Expr rand*))]
      [(forcall op rand*)
       (make-forcall op (map Expr rand*))]
      [(funcall rator rand*)
       (let ([rator (Expr rator)])
         (cond
           [(and (var? rator) (bound-var rator)) =>
            (lambda (c)
              (optimize c rator (map Expr rand*)))]
           [(and (primref? rator)
                 (eq? (primref-name rator) '$$apply))
            (make-jmpcall SL_apply 
                          (Expr (car rand*))
                          (map Expr (cdr rand*)))]
           [else
            (make-funcall rator (map Expr rand*))]))]
      [(mvcall p c) (make-mvcall (Expr p) (Expr c))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (Expr x))


 

(define (convert-closures prog)
  (define who 'convert-closures)
  (define (Expr* x*)
    (cond
      [(null? x*) (values '() '())]
      [else
       (let-values ([(a a-free) (Expr (car x*))]
                    [(d d-free) (Expr* (cdr x*))])
         (values (cons a d) (union a-free d-free)))]))
   (define (do-clambda* x*)
    (cond
      [(null? x*) (values '() '())]
      [else
       (let-values ([(a a-free) (do-clambda (car x*))]
                    [(d d-free) (do-clambda* (cdr x*))])
         (values (cons a d) (union a-free d-free)))]))
  (define (do-clambda x)
    (record-case x 
      [(clambda g cls*)
       (let-values ([(cls* free) 
                     (let f ([cls* cls*])
                       (cond
                         [(null? cls*) (values '() '())]
                         [else
                          (record-case (car cls*)
                            [(clambda-case info body)
                             (let-values ([(body body-free) (Expr body)]
                                          [(cls* cls*-free) (f (cdr cls*))])
                               (values
                                 (cons (make-clambda-case info body) cls*)
                                 (union (difference body-free (case-info-args info))
                                        cls*-free)))])]))])
          (values (make-closure (make-clambda g cls* free) free)
                  free))]))
  (define (Expr ex)
    (record-case ex
      [(constant) (values ex '())]
      [(var) (values ex (singleton ex))]
      [(primref) (values ex '())]
      [(bind lhs* rhs* body)
       (let-values ([(rhs* rhs-free) (Expr* rhs*)] 
                    [(body body-free) (Expr body)])
          (values (make-bind lhs* rhs* body)
                  (union rhs-free (difference body-free lhs*))))]
      [(fix lhs* rhs* body)
       (let-values ([(rhs* rfree) (do-clambda* rhs*)]
                    [(body bfree) (Expr body)])
          (values (make-fix lhs* rhs* body)
                  (difference (union bfree rfree) lhs*)))]
      [(conditional test conseq altern)
       (let-values ([(test test-free) (Expr test)]
                    [(conseq conseq-free) (Expr conseq)]
                    [(altern altern-free) (Expr altern)])
         (values (make-conditional test conseq altern)
                 (union test-free (union conseq-free altern-free))))]
      [(seq e0 e1) 
       (let-values ([(e0 e0-free) (Expr e0)]
                    [(e1 e1-free) (Expr e1)])
         (values (make-seq e0 e1) (union e0-free e1-free)))]
      [(clambda)
       (do-clambda ex)]
      [(primcall op rand*)
       (let-values ([(rand* rand*-free) (Expr* rand*)])
         (values (make-primcall op rand*) rand*-free))]
      [(forcall op rand*)
       (let-values ([(rand* rand*-free) (Expr* rand*)])
         (values (make-forcall op rand*)  rand*-free))]
      [(funcall rator rand*)
       (let-values ([(rator rat-free) (Expr rator)]
                    [(rand* rand*-free) (Expr* rand*)])
         (values (make-funcall rator rand*) 
                 (union rat-free rand*-free)))]
      [(jmpcall label rator rand*)
       (let-values ([(rator rat-free) (Expr rator)]
                    [(rand* rand*-free) (Expr* rand*)])
         (values (make-jmpcall label rator rand*) 
                 (union rat-free rand*-free)))] 
      [(mvcall p c)
       (let-values ([(p p-free) (Expr p)]
                    [(c c-free) (Expr c)])
         (record-case c
           [(closure code free^) 
            (values (make-mvcall p code)
                    (union p-free c-free))]
           [else (error who "invalid mvcall consumer ~s" 
                        (unparse c))]))]
      [else (error who "invalid expression ~s" (unparse ex))]))
  (let-values ([(prog free) (Expr prog)])
    (unless (null? free) 
      (error 'convert-closures "free vars ~s encountered in ~a"
          free (unparse prog)))
   prog))


 




(define (optimize-closures/lift-codes x)
  (define who 'optimize-closures/lift-codes)
  (define all-codes '())
  (define (init-non-thunk var)
    (set-var-assigned! var #f)
    (set-var-referenced! var #f))
  (define (var-thunk var)
    (var-referenced var))
  (define (make-thunk-var var thunk)
    (set-var-referenced! var thunk))
  (define (thunk? x)
    (record-case x
      [(closure code free*)
       (null? free*)]
      [else #f]))
  (define (trim/lift-code code free*)
    (record-case code
      [(clambda label cls* free*/dropped)
       (let ([cls* (map
                     (lambda (x)
                       (record-case x 
                         [(clambda-case info body)
                          (for-each init-non-thunk
                              (case-info-args info))
                          (make-clambda-case info (E body))]))
                     cls*)])
         (let ([g (make-code-loc label)])
           (set! all-codes
             (cons (make-clambda label cls* free*) all-codes))
           g))]))
  (define (optimize-one-closure code free)
    (let ([free (trim-vars free)])
      (make-closure (trim/lift-code code free) free)))
  (define (trim p? ls)
    (cond
      [(null? ls) '()]
      [(p? (car ls)) (trim p? (cdr ls))]
      [else
       (cons (car ls) (trim p? (cdr ls)))]))
  (define (trim-vars ls)
    (trim var-thunk ls))
  (define (trim-thunks ls)
    (trim thunk? ls))
  (define (do-bind lhs* rhs* body)
    (for-each init-non-thunk lhs*)
    (let ([rhs* (map E rhs*)])
      (for-each (lambda (x v) 
                  (when (thunk? v) (make-thunk-var x v)))
                lhs* rhs*)
      (make-bind (trim-vars lhs*) (trim-thunks rhs*) (E body))))
  (define (do-fix lhs* rhs* body)
    (for-each init-non-thunk lhs*)
    (let ([free** ;;; trim the free lists first; after init.
           (map (lambda (x) (trim-vars (closure-free* x))) rhs*)])
      (define-record node (name code deps whacked free))
      (let ([node* (map (lambda (lhs rhs) 
                          (let ([n (make-node lhs (closure-code rhs) '() #f '())])
                            (make-thunk-var lhs n)
                            n))
                        lhs* rhs*)])
        ;;; if x is free in y, then whenever x becomes a non-thunk,
        ;;; y also becomes a non-thunk.  Here, we mark these
        ;;; dependencies.
        (for-each 
          (lambda (my-node free*)
            (for-each (lambda (fvar)
                        (cond
                          [(var-thunk fvar) => ;;; one of ours
                           (lambda (her-node)
                             (set-node-deps! her-node 
                               (cons my-node (node-deps her-node))))]
                          [else ;;; not one of ours
                           (set-node-free! my-node
                             (cons fvar (node-free my-node)))]))
                      free*))
           node* free**)
        ;;; Next, we go over the list of nodes, and if we find one
        ;;; that has any free variables, we know it's a non-thunk,
        ;;; so we whack it and add it to all of its dependents.
        (let ()
          (define (process-node x)
            (unless (null? (node-free x))
              (unless (node-whacked x)
                (set-node-whacked! x #t)
                (for-each 
                  (lambda (y)
                    (set-node-free! y 
                      (cons (node-name x) (node-free y)))
                    (process-node y))
                  (node-deps x)))))
          (for-each process-node node*))
        ;;; Now those that have free variables are actual closures.
        ;;; Those with no free variables are actual thunks.
        (let ([rhs*
               (map
                 (lambda (node)
                   (let ([free (node-free node)])
                     (let ([closure
                            (make-closure (node-code node) free)])
                       (if (null? free)
                           (make-thunk-var (node-name node) closure)
                           (init-non-thunk (node-name node)))
                       closure)))
                 node*)])
          (for-each 
            (lambda (x)
              (set-closure-code! x
                (trim/lift-code 
                  (closure-code x)
                  (closure-free* x))))
            rhs*)
          ;;;
          (make-fix (trim-vars lhs*)
                    (trim-thunks rhs*)
                    (E body))))))
  (define (E x)
    (record-case x
      [(constant) x]
      [(var)      (or (var-thunk x) x)]
      [(primref)  x]
      [(bind lhs* rhs* body) (do-bind lhs* rhs* body)]
      [(fix lhs* rhs* body) (do-fix lhs* rhs* body)]
      [(conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern))]
      [(seq e0 e1)           (make-seq (E e0) (E e1))]
      [(closure c free)      (optimize-one-closure c free)]
      [(primcall op rand*)   (make-primcall op (map E rand*))]
      [(forcall op rand*)    (make-forcall op (map E rand*))]
      [(funcall rator rand*) (make-funcall (E rator) (map E rand*))]
      [(jmpcall label rator rand*) (make-jmpcall label (E rator) (map E rand*))]
      [(mvcall p c)
       (record-case c 
         [(clambda label cases free)
          (make-mvcall (E p) 
            (make-clambda label
              (map (lambda (x)
                     (record-case x
                       [(clambda-case info body)
                        (make-clambda-case info (E body))]))
                   cases)
              free))])]
      [else (error who "invalid expression ~s" (unparse x))]))
  (let ([x (E x)])
    (make-codes all-codes x)))


(include "libcogen1.ss")




(define (lift-codes x) ;;; NOT USED
  (define who 'lift-codes)
  (define all-codes '())
  (define (do-code x)
    (record-case x
      [(clambda label cls* free) 
       (let ([cls* (map 
                     (lambda (x)
                       (record-case x
                         [(clambda-case info body)
                          (make-clambda-case info (E body))]))
                     cls*)])
         (let ([g (make-code-loc label)])
           (set! all-codes
             (cons (make-clambda label cls* free) all-codes))
           g))]))
  (define (E x)
    (record-case x
      [(constant) x]
      [(var)      x]
      [(primref)  x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map E rhs*) (E body))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map E rhs*) (E body))]
      [(conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern))]
      [(seq e0 e1)           (make-seq (E e0) (E e1))]
      [(closure c free)      (make-closure (do-code c) free)]
      [(primcall op rand*)   (make-primcall op (map E rand*))]
      [(forcall op rand*)    (make-forcall op (map E rand*))]
      [(funcall rator rand*) (make-funcall (E rator) (map E rand*))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (let ([x (E x)])
    (make-codes all-codes x)))

(define (syntactically-valid? op rand*)
  (define (valid-arg-count? op rand*)
    (let ([n (open-coded-primitive-args op)] [m (length rand*)])
      (cond
        [(eq? n 'any) #t]
        [(eq? n 'positive) (fx> m 1)]
        [(eq? n 'no-code) 
         (error 'syntactically-valid
           "should not primcall non codable prim ~s" op)]
        [(fixnum? n) 
         (cond
          [(fx= n m) #t]
          [else
           (error 'compile 
             "Possible incorrect number of args in ~s"
              (cons op (map unparse rand*)))
           #f])]
        [else (error 'do-primcall "BUG: what ~s" n)])))
  (define (check op pred?)
    (lambda (arg)
      (record-case arg
        [(constant c)
         (cond
           [(pred? c) #t]
           [else
            (error 'compile "Possible argument error to primitive ~s" op)
            #f])]
        [(primref) 
         (cond
           [(pred? (lambda (x) x)) #t]
           [else
            (error 'compile "Possible argument error to primitive ~s" op)
            #f])]
        [else #t])))
  (define (nonnegative-fixnum? n)
    (and (fixnum? n) (fx>= n 0)))
  (define (byte? n)
    (and (fixnum? n) (fx<= 0 n) (fx<= n 127)))
  (define (valid-arg-types? op rand*)
    (case op
      [(fixnum? flonum? bignum? immediate? boolean? char? vector? string? procedure?
        null? pair? not cons eq? vector symbol? error eof-object eof-object? 
        void $unbound-object? $code? $forward-ptr? bwp-object?
        pointer-value top-level-value car cdr list* list $record
        port? input-port? output-port?
        $make-port/input $make-port/output $make-port/both
        $port-handler 
        $port-input-buffer $port-input-index $port-input-size
        $port-output-buffer $port-output-index $port-output-size 
        $set-port-input-index! $set-port-input-size! 
        $set-port-output-index! $set-port-output-size! ) 
       '#t]
      [($fxadd1 $fxsub1 $fxzero? $fxlognot $fxlogor $fxlogand $fx+ $fx- $fx* 
        $fx= $fx< $fx<= $fx> $fx>= $fxquotient $fxmodulo $fxsll $fxsra $fxlogxor $exit) 
       (andmap (check op fixnum?) rand*)]
      [($fixnum->char) 
       (andmap (check op byte?) rand*)]
      [($char->fixnum $char= $char< $char<= $char> $char>= $string)
       (andmap (check op char?) rand*)]
      [($make-vector $make-string)
       (andmap (check op nonnegative-fixnum?) rand*)]
      [($car $cdr) 
       (andmap (check op pair?) rand*)]
      [($vector-length) 
       (andmap (check op vector?) rand*)]
      [($string-length) 
       (andmap (check op string?) rand*)]
      [($set-car! $set-cdr!)
       ((check op pair?) (car rand*))]
      [($vector-ref $vector-set!)
       (and ((check op vector?) (car rand*))
            ((check op nonnegative-fixnum?) (cadr rand*)))]
      [($string-ref $string-set!
        $string-ref-16+0 $string-ref-16+1 $string-ref-8+0 $string-ref-8+2)
       (and ((check op string?) (car rand*))
            ((check op nonnegative-fixnum?) (cadr rand*)))]
      [($symbol-string $symbol-unique-string)
       (andmap (check op symbol?) rand*)]
      [($constant-ref $set-constant! $intern $pcb-set! $pcb-ref $make-symbol
        $symbol-value $set-symbol-value! $set-symbol-function! $symbol-plist $set-symbol-plist!
        $set-symbol-system-value! $set-symbol-system-value!
        $set-symbol-unique-string!
        $set-symbol-string!
        $seal-frame-and-call  $frame->continuation $code->closure
        $closure-code
        $code-size $code-reloc-vector $code-freevars 
        $code-ref $code-set!
        $make-record $record? $record/rtd? $record-rtd $record-ref $record-set!
        primitive-set! primitive-ref
        $make-tcbucket $tcbucket-key $tcbucket-val $tcbucket-next
        $set-tcbucket-val! 
        $set-tcbucket-next! $set-tcbucket-tconc!)
       #t]
      [else (error 'valid-arg-types? "unhandled op ~s" op)]))
  (and (valid-arg-count? op rand*)
       (or (null? rand*)
           (valid-arg-types? op rand*))))

(begin ;;; UNINLINED ANALYSIS
  ;;; the output of simplify-operands differs from the input in that the 
  ;;; operands to primcalls are all simple (variables, primrefs, or constants).
  ;;; funcalls to open-codable primrefs whos arguments are "ok" are converted to
  ;;; primcalls.  
  (define uninlined '())
  (define (mark-uninlined x)
    (cond
      [(assq x uninlined) =>
       (lambda (p) (set-cdr! p (fxadd1 (cdr p))))]
      [else (set! uninlined (cons (cons x 1) uninlined))]))
  (module ()
    (primitive-set! 'uninlined-stats 
      (lambda () 
        (let f ([ls uninlined] [ac '()])
          (cond
            [(null? ls) ac]
            [(fx> (cdar ls) 15)
             (f (cdr ls) (cons (car ls) ac))]
            [else (f (cdr ls) ac)]))))))

(define (introduce-primcalls x)
  (define who 'introduce-primcalls)
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(closure) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(primcall op arg*)
       (case op
         ;[(values) 
         ; (if (fx= (length arg*) 1)
         ;     (Expr (car arg*))
         ;     (begin
         ;       (warning 'compile "possible incorrect number of values")
         ;       (make-funcall (make-primref 'values) (map Expr arg*))))]
         [else 
          (make-primcall op (map Expr arg*))])]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator rand*)
       (cond
         [(and (primref? rator) 
               (open-codeable? (primref-name rator))
               (syntactically-valid? (primref-name rator) rand*))
          (Expr (make-primcall (primref-name rator) rand*))]
         [else
          (when (primref? rator)
            (mark-uninlined (primref-name rator)))
          (make-funcall (Expr rator) (map Expr rand*))])]
      [(jmpcall label op arg*)
       (make-jmpcall label (Expr op) (map Expr arg*))]
      [(mvcall p c)
       (record-case c
         [(clambda label cases free)
          (make-mvcall (Expr p)
            (make-clambda label 
              (map (lambda (x)
                     (record-case x
                       [(clambda-case info body)
                        (make-clambda-case info (Expr body))]))
                   cases)
              free))])]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(constant) (make-return x)]
      [(var) (make-return x)]
      [(primref) (make-return x)]
      [(closure) (make-return x)]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Tail body))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Tail body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Tail e1))]
      [(primcall op arg*)
       (case op
         ;[(values) 
         ; (if (fx= (length arg*) 1)
         ;     (make-return (Expr (car arg*)))
         ;     (make-return* (map Expr arg*)))]
         [else
          (make-return (make-primcall op (map Expr arg*)))])]
      [(forcall op arg*)
       (make-return (make-forcall op (map Expr arg*)))]
      [(funcall rator rand*)
       (cond
         [(and (primref? rator)
               (open-codeable? (primref-name rator))
               (syntactically-valid? (primref-name rator) rand*))
          (Tail (make-primcall (primref-name rator) rand*))]
         [else
          (make-funcall (Expr rator) (map Expr rand*))])]
      [(jmpcall label op arg*)
       (make-jmpcall label (Expr op) (map Expr arg*))]
      [(mvcall p c)
       (record-case c
         [(clambda label cases free)
          (make-mvcall (Expr p)
            (make-clambda label 
              (map (lambda (x)
                     (record-case x
                       [(clambda-case info body)
                        (make-clambda-case info (Tail body))]))
                   cases)
              free))])]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (CaseExpr x)
    (record-case x
      [(clambda-case info body)
       (make-clambda-case info (Tail body))]))
  (define (CodeExpr x)
    (record-case x
      [(clambda L cases free) 
       (make-clambda L (map CaseExpr cases) free)]))
  (define (CodesExpr x)
    (record-case x 
      [(codes list body)
       (make-codes (map CodeExpr list) (Tail body))]))
  (CodesExpr x))

(define (simplify-operands x)
  (define who 'simplify-operands)
  (define (simple? x) 
    (or (constant? x) (var? x) (primref? x)
        (and (closure? x) (null? (closure-free* x)))))
  (define (simplify arg lhs* rhs* k)
    (if (simple? arg)
        (k arg lhs* rhs*)
        (let ([v (unique-var 'tp)])
          (k v (cons v lhs*) (cons (Expr arg) rhs*)))))
  (define (simplify* arg* lhs* rhs* k)
    (cond
      [(null? arg*) (k '() lhs* rhs*)]
      [else 
       (simplify (car arg*) lhs* rhs*
         (lambda (a lhs* rhs*)
           (simplify* (cdr arg*) lhs* rhs*
             (lambda (d lhs* rhs*)
               (k (cons a d) lhs* rhs*)))))]))
  (define (make-bind^ lhs* rhs* body)
    (if (null? lhs*)
        body
        (make-bind lhs* rhs* body)))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(closure) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(primcall op arg*)
       (cond
         [(memq op '(not car cdr cadr fxadd1 fxsub1
                     null? pair? fixnum? vector? string?
                     char? symbol? eof-object?
                     )) ;;; SIMPLIFY
          (make-primcall op (map Expr arg*))]
         [(memq op '(fx+))
          (cond
            [(simple? (cadr arg*))
             (make-primcall op 
                (list (Expr (car arg*)) (cadr arg*)))]
            [(simple? (car arg*))
             (make-primcall op
                (list (car arg*) (Expr (cadr arg*))))]
            [else 
             (simplify* (cdr arg*) '() '()
               (lambda (a* lhs* rhs*)
                 (make-bind^ lhs* rhs*
                   (make-primcall op
                     (cons (Expr (car arg*)) a*)))))])]
         [else
          (simplify* arg* '() '()
            (lambda (arg* lhs* rhs*)
              (make-bind^ lhs* rhs* 
                (make-primcall op arg*))))])]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(jmpcall label op arg*)
       (make-jmpcall label (Expr op) (map Expr arg*))]
      [(mvcall p c)
       (make-mvcall (Expr p) (CodeExpr c))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(return v) (make-return (Expr v))]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Tail body))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Tail body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Tail e1))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(jmpcall label op arg*)
       (make-jmpcall label (Expr op) (map Expr arg*))]
      [(mvcall p c)
       (make-mvcall (Expr p) (CodeExpr c))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (CaseExpr x)
    (record-case x 
      [(clambda-case info body)
       (make-clambda-case info (Tail body))]))
  (define (CodeExpr x)
    (record-case x
      [(clambda L clauses free)
       (make-clambda L (map CaseExpr clauses) free)]))
  (define (CodesExpr x)
    (record-case x 
      [(codes list body)
       (make-codes (map CodeExpr list) (Tail body))]))
  (CodesExpr x))

(define (insert-stack-overflow-checks x)
  (define who 'insert-stack-overflow-checks)
  (define (insert-check body)
    (make-seq
      (make-interrupt-call 
        (make-primcall '$fp-overflow '())
        (make-funcall (make-primref 'do-stack-overflow) '()))
      body))
  (define (Expr x)
    (record-case x
      [(constant) #f]
      [(var) #f]
      [(primref) #f]
      [(closure code free*) #f]
      [(bind lhs* rhs* body)
       (or (ormap Expr rhs*) (Expr body))]
      [(fix lhs* rhs* body) (Expr body)]
      [(conditional test conseq altern)
       (or (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (or (Expr e0) (Expr e1))]
      [(primcall op arg*) (ormap Expr arg*)]
      [(forcall op arg*)  (ormap Expr arg*)]
      [(funcall rator arg*) #t] 
      [(jmpcall label rator arg*)    #t]
      [(mvcall p c) #t]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(return v) (Expr v)]
      [(bind lhs* rhs* body)
       (or (ormap Expr rhs*) (Tail body))]
      [(fix lhs* rhs* body) (Tail body)]
      [(conditional test conseq altern)
       (or (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (or (Expr e0) (Tail e1))]
      [(funcall rator arg*) (or (Expr rator) (ormap Expr arg*))] 
      [(jmpcall label rator arg*) (or (Expr rator) (ormap Expr arg*))]
      [(mvcall p c) #t]
      [else (error who "invalid tail expression ~s" (unparse x))]))
  (define (CaseExpr x)
    (record-case x 
      [(clambda-case info body)
       (if (Tail body)
           (make-clambda-case info (insert-check body))
           x)]))
  (define (CodeExpr x)
    (record-case x
      [(clambda L cases free)
       (make-clambda L (map CaseExpr cases) free)]))
  (define (CodesExpr x)
    (record-case x 
      [(codes list body)
       (make-codes (map CodeExpr list)
                   (if (Tail body)
                       (insert-check body)
                       body))]))
  (CodesExpr x))

(define (insert-allocation-checks x)
  (define who 'insert-allocation-checks)
  (define (check-bytes n var body)
    (make-seq
      (make-interrupt-call 
        (make-primcall '$ap-check-bytes
          (list (make-constant n) var))
        (make-forcall "ik_collect" 
                      (list 
                        (make-primcall '$fx+ 
                          (list (make-constant (fx+ n 4096)) var)))))
      body))
  (define (check-words n var body)
    (make-seq
      (make-interrupt-call 
        (make-primcall '$ap-check-words
          (list (make-constant n) var))
        (make-forcall "ik_collect" ; (make-primref 'do-overflow-words)
                      (list
                        (make-primcall '$fx+ 
                          (list (make-constant (fx+ n 4096)) var)))))
      body))
  (define (check-const n body)
    (cond
      [(fxzero? n) body]
      [else
       (make-seq
         (make-interrupt-call 
           (make-primcall '$ap-check-const
             (list (make-constant n)))
           (make-forcall "ik_collect" ;(make-primref 'do-overflow) 
                         (list (make-constant (fx+ n 4096)))))
         body)]))
  (define (closure-size x)
    (record-case x 
      [(closure code free*)
       (if (null? free*)
           0
           (align (fx+ disp-closure-data (fx* (length free*) wordsize))))]
      [else (error 'closure-size "~s is not a closure" x)]))
  (define (sum ac ls)
    (cond
      [(null? ls) ac]
      [else (sum (fx+ ac (car ls)) (cdr ls))]))
  (define (Expr x)
    (record-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(closure) 
       (check-const (closure-size x) x)]
      [(fix lhs* rhs* body)
       (if (null? lhs*)
           (Expr body)
           (check-const (sum 0 (map closure-size rhs*))
             (make-fix lhs* rhs* 
               (Expr body))))]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(primcall op arg*)
       (let ([x (make-primcall op (map Expr arg*))])
         (case op
           [(cons) (check-const pair-size x)]
           [($make-symbol) (check-const symbol-size x)]
           [($make-tcbucket) (check-const tcbucket-size x)]
           [($frame->continuation $code->closure) 
            (check-const 
              (fx+ disp-closure-data (fx* (length arg*) wordsize)) x)]
           [($make-string) 
            (record-case (car arg*)
              [(constant i)
               (check-const (fx+ i (fx+ disp-string-data 1)) x)]
              [else
               (check-bytes (fxadd1 disp-string-data) (car arg*) x)])]
           [($string)
            (check-const (fx+ (length arg*) (fx+ disp-string-data 1)) x)]
           [($make-port/input $make-port/output $make-port/both)
            (check-const port-size x)]
           [($make-vector) 
            (record-case (car arg*)
              [(constant i)
               (check-const (fx+ (fx* i wordsize) disp-vector-data) x)]
              [else
               (check-words (fxadd1 disp-vector-data) (car arg*) x)])]
           [($make-record) 
            (record-case (cadr arg*)
              [(constant i)
               (check-const (fx+ (fx* i wordsize) disp-record-data) x)]
              [else
               (check-words (fxadd1 disp-record-data) (cadr arg*) x)])]
           [(list*)
            (check-const (fx* (fxsub1 (length arg*)) pair-size) x)]
           [(list)
            (check-const (fx* (length arg*) pair-size) x)]
           [(vector $record)
            (check-const (fx+ (fx* (length arg*) wordsize) disp-vector-data) x)]
           [else x]))]
      [(forcall op arg*)
       (make-forcall op (map Expr arg*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(jmpcall label op arg*)
       (make-jmpcall label (Expr op) (map Expr arg*))]
      [(interrupt-call e0 e1)
       (make-interrupt-call (Expr e0) (Expr e1))]
      [(mvcall p c)
       (make-mvcall (Expr p) (CodeExpr c))]
      [else (error who "invalid expression ~s" (unparse x))]))
  (define (Tail x)
    (record-case x
      [(return v) (make-return (Expr v))]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Tail body))]
      [(fix lhs* rhs* body)
       (if (null? lhs*)
           (Tail body)
           (check-const (sum 0 (map closure-size rhs*))
             (make-fix lhs* rhs* 
               (Tail body))))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Tail conseq) (Tail altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Tail e1))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(jmpcall label op arg*)
       (make-jmpcall label (Expr op) (map Expr arg*))]
      [(mvcall p c)
       (make-mvcall (Expr p) (CodeExpr c))]
      [else (error who "invalid tail expression ~s" (unparse x))]))
  (define (CaseExpr x)
    (record-case x 
      [(clambda-case info body)
       (make-clambda-case info (Tail body))]))
  (define (CodeExpr x)
    (record-case x
      [(clambda L cases free)
       (make-clambda L (map CaseExpr cases) free)]))
  (define (CodesExpr x)
    (record-case x 
      [(codes list body)
       (make-codes (map CodeExpr list) (Tail body))]))
  (CodesExpr x))


(define (insert-engine-checks x)
  (define (Tail x)
    (make-seq
      (make-interrupt-call 
        (make-primcall '$engine-check '())
        (make-funcall (make-primref '$do-event) '()))
      x))
  (define (CaseExpr x)
    (record-case x 
      [(clambda-case info body)
       (make-clambda-case info (Tail body))]))
  (define (CodeExpr x)
    (record-case x
      [(clambda L cases free)
       (make-clambda L (map CaseExpr cases) free)]))
  (define (CodesExpr x)
    (record-case x 
      [(codes list body)
       (make-codes (map CodeExpr list) (Tail body))]))
  (CodesExpr x))

(define (remove-local-variables x)
  (define who 'remove-local-variables)
  (define (Body orig-x orig-si orig-r orig-live save-cp?)
    (define (simple* x* r)
      (map (lambda (x) 
             (cond
               [(assq x r) => cdr]
               [else
                 (when (var? x) (error who "unbound var ~s" x))
                 x]))
            x*))
    (define (env->live-mask r sz)
      (let ([s (make-vector (fxsra (fx+ sz 7) 3) 0)])
        (for-each 
          (lambda (idx) 
             (let ([q (fxsra idx 3)]
                   [r (fxlogand idx 7)])
               (vector-set! s q 
                 (fxlogor (vector-ref s q) (fxsll 1 r)))))
          r)
        s))
    (define (check? x)
      (cond
        [(primref? x) #f]  ;;;; PRIMREF CHECK
        [(closure? x) #f]
        [else         #t]))
    (define (do-new-frame label op rand* si r 
              call-convention rp-convention orig-live)
      (let ([start-si (if save-cp? (fxadd1 si) si)])
        (make-new-frame start-si (fx+ (length rand*) 2)
          (let f ([r* rand*] [nsi (fxadd1 start-si)] [live orig-live])
            (cond
              [(null? r*) 
               (make-seq
                 (make-seq 
                   (if save-cp?
                       (make-save-cp (make-frame-var si))
                       (nop))
                   (case call-convention
                     [(normal apply)
                      (make-eval-cp (check? op)
                        (Expr op nsi r 
                          (if save-cp? 
                              (cons si live)
                              live)))]
                     [(direct)
                      (if (closure? op)
                          (nop)
                          (make-eval-cp #f
                            (Expr op nsi r 
                              (if save-cp?
                                  (cons si live)
                                  live))))]
                     [(foreign)
                      (make-eval-cp #f (make-foreign-label op))]
                     [else (error who "invalid convention ~s" call-convention)]))
                 (make-call-cp call-convention label save-cp?
                    rp-convention
                    start-si    ; frame size
                    (length rand*) ; argc
                    (env->live-mask 
                      (if save-cp? (cons si orig-live) orig-live)
                      start-si)))] ; mask-size ~~ frame size
              [else
               (make-seq 
                 (make-assign (make-frame-var nsi)
                   (Expr (car r*) nsi r live))
                 (f (cdr r*) (fxadd1 nsi) (cons nsi live)))])))))
    (define (nop) (make-primcall 'void '()))
    (define (do-bind lhs* rhs* body si r live k)
      (let f ([lhs* lhs*] [rhs* rhs*] [si si] [nr r] [live live])
        (cond
          [(null? lhs*) (k body si nr live)]
          [else
           (let ([v (make-frame-var si)])
             (make-seq
               (make-assign v (Expr (car rhs*) si r live))
               (f (cdr lhs*) (cdr rhs*) (fxadd1 si)
                  (cons (cons (car lhs*) v) nr)
                  (cons si live))))])))
    (define (do-closure r)
      (lambda (x)
        (record-case x
          [(closure code free*)
           (make-closure code (simple* free* r))])))
    (define (do-fix lhs* rhs* body si r live k)
      (let f ([l* lhs*] [nlhs* '()] [si si] [r r] [live live])
        (cond
          [(null? l*) 
           (make-fix (reverse nlhs*) 
                     (map (do-closure r) rhs*)
                     (k body si r live))]
          [else
           (let ([v (make-frame-var si)])
             (f (cdr l*) (cons v nlhs*) (fxadd1 si) 
                (cons (cons (car l*) v) r)
                (cons si live)))])))
    (define (do-tail-frame-unoptimized label op rand* si r call-conv live)
      (let f ([i si] [r* rand*] [live live])
        (cond
          [(null? r*)
           (make-seq
             (make-eval-cp (check? op) (Expr op i r live))
             (let f ([i 1] [j si] [r* rand*])
               (cond
                 [(null? r*) 
                  (make-tailcall-cp call-conv label (length rand*))]
                 [else
                  (make-seq
                    (make-assign (make-frame-var i)
                                 (make-frame-var j))
                    (f (fxadd1 i) (fxadd1 j) (cdr r*)))])))]
          [else
           (let ([fv (make-frame-var i)]
                 [rhs (Expr (car r*) i r live)])
             (make-seq
               (make-assign fv rhs)
               (f (fxadd1 i) (cdr r*) (cons i live))))])))
    (define (do-tail-frame label op rand* si r call-conv live)
      (define (const? x)
        (record-case x
          [(constant) #t]
          [(primref)  #t]
          [else       #f]))
      (define (evalrand* rand* i si r live ac)
        (cond
          [(null? rand*) 
           ;;; evaluate operator after all operands
           (make-seq
             (make-eval-cp (check? op) (Expr op si r live))
             ac)]
          [(const? (car rand*))
           ;;; constants are not live since they can be assigned
           ;;; after all args are evaluated
           (evalrand* (cdr rand*) (fxadd1 i) (fxadd1 si) r live
             (make-seq ac
               (make-assign (make-frame-var i) (car rand*))))]
          [else
           (let ([vsi (make-frame-var si)]
                 [rhs (Expr (car rand*) si r live)])
             (cond
               [(and (frame-var? rhs)
                     (fx= (frame-var-idx rhs) i))
                ;;; value of rhs is already in f[i]
                ;;; just mark it live
                (evalrand* (cdr rand*) (fx+ i 1) (fx+ si 1) r live ac)]
               [(fx= i si)
                (make-seq 
                  (make-assign vsi rhs)
                  (evalrand* (cdr rand*) (fx+ i 1) (fx+ si 1) r
                             (cons si live) ac))]
               [else
                (make-seq
                  (make-assign vsi rhs)
                  (evalrand* (cdr rand*) (fxadd1 i) (fxadd1 si) r (cons si live)
                    (make-seq ac
                      (make-assign (make-frame-var i) vsi))))]))]))
      (make-seq
        (evalrand* rand* 1 si r live (make-primcall 'void '()))
        (make-tailcall-cp call-conv label (length rand*))))
    (define (Tail x si r live)
      (record-case x
        [(return v) (make-return (Expr v si r live))]
        [(bind lhs* rhs* body)
         (do-bind lhs* rhs* body si r live Tail)]
        [(fix lhs* rhs* body)
         (do-fix lhs* rhs* body si r live Tail)]
        [(conditional test conseq altern)
         (make-conditional 
           (Expr test si r live)
           (Tail conseq si r live) 
           (Tail altern si r live))]
        [(seq e0 e1) (make-seq (Effect e0 si r live) (Tail e1 si r live))]
        [(primcall op arg*)
         (make-return 
           (make-primcall op 
             (map (lambda (x) (Expr x si r live)) arg*)))]
        [(funcall op rand*)
         (do-tail-frame #f op rand* si r 'normal live)]
        [(jmpcall label op rand*)
         (do-tail-frame label op rand* si r 'direct live)]
        [(mvcall p c)
         (do-mvcall p c x si r live Tail)]
        [else (error who "invalid expression ~s" (unparse x))]))
    (define (do-mvcall p c x si r live k)
      (define (make-mv-rp c si r live k)
        (define (do-clambda-case x)
          (record-case x 
            [(clambda-case info body)
             (record-case info
               [(case-info label fml* proper)
                (let-values ([(fml* si r live) 
                              (bind-fml* fml* 
                                (if save-cp? (fx+ si 2) (fx+ si 1)) 
                                r live)])
                  (make-clambda-case
                    (make-case-info label fml* proper)
                    (k body si r live)))])]))
        (record-case c
          [(clambda L cases free)
           (make-clambda L (map do-clambda-case cases) free)]))
      (record-case p
        [(funcall op rand*) 
         (do-new-frame #f op rand* si r 'normal 
           (make-mv-rp c si r live k)
           live)]
        [(jmpcall label op rand*) 
         (do-new-frame label op rand* si r 'direct
           (make-mv-rp c si r live k)
           live)]
        [else (error who "invalid mvcall producer ~s" 
                (unparse p))]))
    (define (Effect x si r live)
      (record-case x
        [(constant) (nop)]
        [(var) (nop)] 
        [(primref) (nop)]
        [(closure code free*) (nop)]
        [(bind lhs* rhs* body)
         (do-bind lhs* rhs* body si r live Effect)]
        [(fix lhs* rhs* body)
         (do-fix lhs* rhs* body si r live Effect)]
        [(conditional test conseq altern)
         (make-conditional 
           (Expr test si r live)
           (Effect conseq si r live) 
           (Effect altern si r live))]
        [(seq e0 e1) (make-seq (Effect e0 si r live) (Effect e1 si r live))]
        [(primcall op arg*)
         (make-primcall op 
           (map (lambda (x) (Expr x si r live)) arg*))]
        [(forcall op rand*)
         (do-new-frame #f op rand* si r 'foreign 'effect live)]
        [(funcall op rand*)
         (do-new-frame #f op rand* si r 'normal 'effect live)]
        [(mvcall p c)
         (do-mvcall p c x si r live Effect)]
        [(jmpcall label op rand*)
         (do-new-frame label op rand* si r 'direct 'effect live)]
        [(interrupt-call e0 e1)
         (make-interrupt-call 
           (Expr e0 si r live)
           (Effect e1 si r live))]
        [else (error who "invalid effect expression ~s" (unparse x))]))
    (define (Expr x si r live)
      (record-case x
        [(constant) x]
        [(var) 
         (cond
           [(assq x r) => cdr]
           [else (error who "unbound var ~s" x)])]
        [(primref) x]
        [(closure code free*) 
         (make-closure code (simple* free* r))]
        [(bind lhs* rhs* body)
         (do-bind lhs* rhs* body si r live Expr)]
        [(fix lhs* rhs* body)
         (do-fix lhs* rhs* body si r live Expr)]
        [(conditional test conseq altern)
         (make-conditional 
           (Expr test si r live)
           (Expr conseq si r live) 
           (Expr altern si r live))]
        [(seq e0 e1) (make-seq (Effect e0 si r live) (Expr e1 si r live))]
        [(primcall op arg*)
         (make-primcall op 
           (map (lambda (x) (Expr x si r live)) arg*))]
        [(forcall op rand*)
         (do-new-frame #f op rand* si r 'foreign 'value live)]
        [(funcall op rand*)
         (do-new-frame #f op rand* si r 'normal 'value live)]
        [(jmpcall label op rand*)
         (do-new-frame label op rand* si r 'direct 'value live)]
        [(mvcall p c)
         (do-mvcall p c x si r live Expr)]
        [else (error who "invalid expression ~s" (unparse x))]))
    (Tail orig-x orig-si orig-r orig-live))
  (define (bind-fml* fml* si r live)
    (let f ([si si] [fml* fml*])
      (cond
        [(null? fml*) (values '() si r live)]
        [else
          (let-values ([(nfml* nsi r live)
                        (f (fxadd1 si) (cdr fml*))])
            (let ([v (make-frame-var si)])
              (values (cons v nfml*)
                      nsi
                      (cons (cons (car fml*) v) r)
                      (cons si live))))])))
  (define (bind-free* free*)
    (let f ([free* free*] [idx 0] [r '()])
      (cond
        [(null? free*) r]
        [else
         (f (cdr free*) (fxadd1 idx)
            (cons (cons (car free*) (make-cp-var idx)) r))])))
  (define CaseExpr 
    (lambda (r save-cp?)
      (lambda (x)
        (record-case x 
          [(clambda-case info body)
           (record-case info
             [(case-info label fml* proper)
              (let-values ([(fml* si r live) (bind-fml* fml* 1 r '())])
                (make-clambda-case 
                  (make-case-info label fml* proper)
                  (Body body si r live save-cp?)))])]))))
  (define (CodeExpr x)
    (record-case x
      [(clambda L cases free)
       (let ([r (bind-free* free)])
         (make-clambda L (map (CaseExpr r (not (null? free))) cases) free))]))
  (define (CodesExpr x)
    (record-case x 
      [(codes list body)
       (make-codes (map CodeExpr list) 
                   (Body body 1 '() '() #f))]))
  (CodesExpr x))

(define (optimize-ap-check x)
  (define who 'optimize-ap-check)
  (define (min x y)
    (if (fx< x y) x y))
  (define (Tail x f)
    (record-case x
      [(return v)
       (let-values ([(v f) (NonTail v f)])
         (make-return v))]
      [(fix lhs* rhs* body)
       (make-fix lhs* rhs* (Tail body f))]
      [(conditional test conseq altern)
       (let-values ([(test f) (NonTail test f)])
         (make-conditional 
           test
           (Tail conseq f)
           (Tail altern f)))]
      [(seq e0 e1) 
       (let-values ([(e0 f) (NonTail e0 f)])
         (make-seq e0 (Tail e1 f)))]
      [(tailcall-cp) x]
      [(new-frame base-idx size body)
       (make-new-frame base-idx size (Tail body f))]
      [(call-cp call-conv)
       (let-values ([(x f) 
                     (do-cp-call x f 
                       (lambda (x f) 
                         (values (Tail x f) 0)))])
         x)]
      [else (error who "invalid tail expression ~s" (unparse x))]))
  (define (do-primcall op arg* f)
    (case op
      [($ap-check-const)
       (let ([n (constant-value (car arg*))])
         (cond
           [(fx< n f) 
            (values (make-constant #f) (fx- f n))]
           [(fx<= n 4096)
            (values (make-primcall '$ap-check-const 
                                   (list (make-constant 4096)))
                    (fx- 4096 n))]
           [else
            (values (make-primcall '$ap-check-const 
                                   (list (make-constant (fx+ n 4096))))
                    4096)]))]
      [($ap-check-bytes $ap-check-words)
       (values (make-primcall op 
                 (list (make-constant (fx+ (constant-value (car arg*)) 
                                           4096))
                       (cadr arg*)))
               4096)]
      [else (values (make-primcall op arg*) f)]))
  (define (do-cp-call x f k)
    (record-case x
      [(call-cp call-conv label save-cp? rp-conv si argc mask)
       (record-case rp-conv
         [(clambda L cases free)
          (let-values ([(cases f)
                        (let g ([cases cases])
                          (cond
                            [(null? cases) 
                             (values '() f)]
                            [else
                             (let-values ([(c* f) (g (cdr cases))])
                               (record-case (car cases)
                                [(clambda-case info body)
                                 (let-values ([(c f0) (k body f)])
                                   (values 
                                     (cons (make-clambda-case info c) c*)
                                     (min f f0)))]))]))])
            (values 
              (make-call-cp call-conv label save-cp? 
                (make-clambda L cases free) si argc mask)
              f))]
         [else 
          (values x f)])]))
  (define (NonTail x f)
    (record-case x
      [(constant)         (values x f)]
      [(frame-var)        (values x f)]
      [(cp-var)           (values x f)]
      [(save-cp)          (values x f)]
      [(foreign-label)    (values x f)]
      [(primref)          (values x f)]
      [(closure)          (values x f)]
      [(call-cp call-conv)
       (let-values ([(x f) (do-cp-call x f NonTail)])
         (cond
           [(eq? call-conv 'foreign)
            (values x f)]
           [else
            (values x 0)]))]
      [(primcall op arg*) 
       (let loop ([arg* arg*] [ls '()] [f f])
         (cond
           [(null? arg*) 
            (do-primcall op (reverse ls) f)]
           [else
            (let-values ([(a f) (NonTail (car arg*) f)])
              (loop (cdr arg*) (cons a ls) f))]))]
      [(fix lhs* rhs* body)
       (let-values ([(body f) (NonTail body f)])
         (values (make-fix lhs* rhs* body) f))]
      [(conditional test conseq altern)
       (let-values ([(test f) (NonTail test f)])
         (if (constant? test)
             (if (constant-value test)
                 (NonTail conseq f)
                 (NonTail altern f))
             (let-values ([(conseq f0) (NonTail conseq f)]
                          [(altern f1) (NonTail altern f)])
               (values (make-conditional test conseq altern)
                       (min f0 f1)))))]
      [(seq e0 e1) 
       (let-values ([(e0 f) (NonTail e0 f)])
         (let-values ([(e1 f) (NonTail e1 f)])
           (values (make-seq e0 e1) f)))]
      [(assign lhs rhs)
       (let-values ([(rhs f) (NonTail rhs f)])
         (values (make-assign lhs rhs) f))]
      [(eval-cp check body)
       (let-values ([(body f) (NonTail body f)])
         (values (make-eval-cp check body) f))]
      [(new-frame base-idx size body)
       (let-values ([(body f) (NonTail body f)])
         (values (make-new-frame base-idx size body) f))]
      [(interrupt-call e0 e1) ;;; FIXME: suboptimal
       (let-values ([(e0 f0) (NonTail e0 f)])
         (let-values ([(e1 f1) (NonTail e1 f0)])
           (values (make-interrupt-call e0 e1) 
                   (min f0 f1))))]
      [else (error who "invalid nontail expression ~s" (unparse x))]))
  (define CaseExpr 
    (lambda (x)
      (record-case x 
        [(clambda-case info body)
         (make-clambda-case info (Tail body 0))])))
  (define (CodeExpr x)
    (record-case x
      [(clambda L cases free)
       (make-clambda L (map CaseExpr cases) free)]))
  (define (CodesExpr x)
    (record-case x 
      [(codes list body)
       (make-codes (map CodeExpr list) 
                   (Tail body 0))]))
  (CodesExpr x))

(begin ;;; DEFINITIONS
  (define fx-shift 2)
  (define fx-mask #x03)
  (define fx-tag 0)
  (define bool-f #x2F)
  (define bool-t #x3F)
  (define bool-mask #xEF)
  (define bool-tag #x2F)
  (define bool-shift 4)
  (define nil     #x4F)
  (define eof     #x5F) ; double check
  (define unbound #x6F) ; double check
  (define void-object #x7F) ; double check
  (define bwp-object  #x8F) ; double check
  (define char-shift 8)
  (define char-tag #x0F)
  (define char-mask #xFF)
  (define pair-mask 7)
  (define pair-tag  1)
  (define disp-car  0)
  (define disp-cdr  4)
  (define pair-size 8)

  (define flonum-tag    #x17)
  (define flonum-size     16)
  (define disp-flonum-data 8)

  (define bignum-mask        #b111)
  (define bignum-tag         #b011)
  (define bignum-sign-mask  #b1000)
  (define bignum-sign-shift   3)
  (define bignum-length-shift 4) 
  (define disp-bignum-data    4)

  (define pagesize 4096)
  (define pageshift 12)
  (define wordsize  4)
  (define wordshift 2)
  
  (define symbol-mask 7)
  (define symbol-tag 2)
  (define disp-symbol-string          0)
  (define disp-symbol-unique-string   4)
  (define disp-symbol-value           8)
  (define disp-symbol-plist          12)
  (define disp-symbol-system-value   16)
  (define disp-symbol-function       20)
  (define disp-symbol-error-function 24)
  (define disp-symbol-unused         28)
  (define symbol-size                32)
  (define vector-tag 5)
  (define vector-mask 7)
  (define disp-vector-length 0)
  (define disp-vector-data 4)
  (define string-mask 7)
  (define string-tag 6)
  (define disp-string-length 0)
  (define disp-string-data 4)
  (define closure-mask 7)
  (define closure-tag 3)
  (define disp-closure-data 4)
  (define disp-closure-code 0)
  (define continuation-size       16)
  (define continuation-tag      #x1F)
  (define disp-continuation-top    4)
  (define disp-continuation-size   8)
  (define disp-continuation-next  12)
  (define code-tag              #x2F)
  (define disp-code-instrsize      4)
  (define disp-code-relocsize      8)
  (define disp-code-freevars      12)
  (define disp-code-data          16)
  (define port-tag              #x3F)
  (define input-port-tag        #x7F)
  (define output-port-tag       #xBF)
  (define input/output-port-tag #xFF)
  (define port-mask             #x3F)
  (define disp-port-handler        4)
  (define disp-port-input-buffer   8)
  (define disp-port-input-index   12)
  (define disp-port-input-size    16)
  (define disp-port-output-buffer 20)
  (define disp-port-output-index  24)
  (define disp-port-output-size   28)
  (define port-size               32)
  (define disp-tcbucket-tconc 0)
  (define disp-tcbucket-key   4)
  (define disp-tcbucket-val   8)
  (define disp-tcbucket-next 12)
  (define tcbucket-size      16)
  (define record-ptag  5)
  (define record-pmask 7)
  (define disp-record-rtd     0)
  (define disp-record-data    4)
  (define disp-frame-size -17)
  (define disp-frame-offset -13)
  (define disp-multivalue-rp -9)
  (define object-alignment 8)
  (define align-shift 3)
  (define dirty-word -1))

(begin ;;; COGEN HELERS
  (define (align n)
    (fxsll (fxsra (fx+ n (fxsub1 object-alignment)) align-shift) align-shift))
  (define (mem off val)
    (cond
      [(fixnum? off) (list 'disp (int off) val)]
      [(register? off) (list 'disp off val)]
      [else (error 'mem "invalid disp ~s" off)]))
  (define-syntax int
    (syntax-rules ()
      [(_ x) x]))
  (define (obj x) (list 'obj x))
  (define (byte x) (list 'byte x))
  (define (byte-vector x) (list 'byte-vector x))
  (define (movzbl src targ) (list 'movzbl src targ))
  (define (sall src targ) (list 'sall src targ))
  (define (sarl src targ) (list 'sarl src targ))
  (define (shrl src targ) (list 'shrl src targ))
  (define (notl src) (list 'notl src))
  (define (pushl src) (list 'pushl src))
  (define (popl src) (list 'popl src))
  (define (orl src targ) (list 'orl src targ))
  (define (xorl src targ) (list 'xorl src targ))
  (define (andl src targ) (list 'andl src targ))
  (define (movl src targ) (list 'movl src targ))
  (define (leal src targ) (list 'leal src targ))
  (define (movb src targ) (list 'movb src targ))
  (define (addl src targ) (list 'addl src targ))
  (define (imull src targ) (list 'imull src targ))
  (define (idivl src) (list 'idivl src))
  (define (subl src targ) (list 'subl src targ))
  (define (push src) (list 'push src))
  (define (pop targ) (list 'pop targ))
  (define (sete targ) (list 'sete targ))
  (define (call targ) (list 'call targ))
  (define (tail-indirect-cpr-call)
    (jmp (mem (fx- disp-closure-code closure-tag) cpr)))
  (define (indirect-cpr-call)
    (call (mem (fx- disp-closure-code closure-tag) cpr)))
  (define (negl targ) (list 'negl targ))
  (define (label x) (list 'label x))
  (define (label-address x) (list 'label-address x))
  (define (ret) '(ret))
  (define (cltd) '(cltd))
  (define (cmpl arg1 arg2) (list 'cmpl arg1 arg2))
  (define (je label) (list 'je label))
  (define (jne label) (list 'jne label))
  (define (jle label) (list 'jle label))
  (define (jge label) (list 'jge label))
  (define (jg label) (list 'jg label))
  (define (jl label) (list 'jl label))
  (define (jb label) (list 'jb label))
  (define (ja label) (list 'ja label))
  (define (jo label) (list 'jo label))
  (define (jmp label) (list 'jmp label))
 ; (define edi '%edx) ; closure pointer
 ; (define esi '%esi) ; pcb
 ; (define ebp '%ebp) ; allocation pointer
  (define esp '%esp) ; stack base pointer 
  (define al '%al)
  (define ah '%ah)
  (define bh '%bh)
  (define cl '%cl)
  (define eax '%eax)
  (define ebx '%ebx)
  (define ecx '%ecx)
  (define edx '%edx)
  (define apr '%ebp) ; allocation pointer
  (define fpr '%esp) ; frame pointer
  (define cpr '%edi) ; closure pointer
  (define pcr '%esi) ; pcb pointer
  (define register? symbol?)
  (define (argc-convention n)
    (fx- 0 (fxsll n fx-shift))))

(define pcb-ref
  (lambda (x)
    (case x
      [(allocation-pointer) (mem  0 pcr)]
      [(allocation-redline) (mem  4 pcr)]
      [(frame-pointer)      (mem  8 pcr)]
      [(frame-base)         (mem 12 pcr)]
      [(frame-redline)      (mem 16 pcr)]
      [(next-continuation)  (mem 20 pcr)]
      [(system-stack)       (mem 24 pcr)]
      [(dirty-vector)       (mem 28 pcr)]
      [(arg-list)           (mem 32 pcr)]
      [(engine-counter)     (mem 36 pcr)]
      [(interrupted)        (mem 40 pcr)]
      [else (error 'pcb-ref "invalid arg ~s" x)])))

(define (primref-loc op)
  (unless (symbol? op) (error 'primref-loc "not a symbol ~s" op))
  (mem (fx- disp-symbol-system-value symbol-tag)
        (obj op)))


(define (generate-code x)
  (define who 'generate-code)
  (define (rp-label x L_multi)
    (case x
      [(value) (label-address SL_multiple_values_error_rp)]
      [(effect) (label-address SL_multiple_values_ignore_rp)]
      [else 
       (if (clambda? x)
           (label-address L_multi)
           (error who "invalid rp-convention ~s" x))]))
  (define unique-label
    (lambda ()
      (label (gensym))))
  (define handlers '())
  (define (add-handler! ls)
    (set-cdr! handlers (append ls (cdr handlers))))
  (define (constant-val x)
    (cond
      [(fixnum? x) (obj x)]
      [(boolean? x) (int (if x bool-t bool-f))]
      [(null? x) (int nil)]
      [(char? x) (int (fx+ (fxsll (char->integer x) char-shift) char-tag))]
      [(eq? x (void)) (int void-object)]
      [else (obj x)]))
  (define (cond-branch op Lt Lf ac)
    (define (opposite x)
      (cond
        [(assq x '([je jne] [jne je] [jl jge] [jle jg] [jg jle] [jge jl]))
         => cadr]
        [else (error who "BUG: no opposite of ~s" x)]))
    (unless (or Lt Lf)
      (error 'cond-branch "no labels"))
    (cond
      [(not Lf) (cons (list op Lt) ac)]
      [(not Lt) (cons (list (opposite op) Lf) ac)] 
      [else (list* (list op Lt) (jmp Lf) ac)]))
  (define (indirect-type-pred pri-mask pri-tag sec-mask sec-tag rand* Lt Lf ac)
    (NonTail (car rand*)
      (cond
        [(and Lt Lf)
         (list* (movl eax ebx)
                (andl (int pri-mask) ebx)
                (cmpl (int pri-tag) ebx)
                (jne Lf)
                (movl (mem (fx- 0 pri-tag) eax) ebx)
                (if sec-mask
                    (andl (int sec-mask) ebx)
                    '(nop))
                (cmpl (int sec-tag) ebx)
                (jne Lf)
                (jmp Lt)
                ac)]
        [Lf
         (list* (movl eax ebx)
                (andl (int pri-mask) ebx)
                (cmpl (int pri-tag) ebx)
                (jne Lf)
                (movl (mem (fx- 0 pri-tag) eax) ebx)
                (if sec-mask
                    (andl (int sec-mask) ebx)
                    '(nop))
                (cmpl (int sec-tag) ebx)
                (jne Lf)
                ac)]
        [Lt
         (let ([L_END (unique-label)])
           (list* (movl eax ebx)
                  (andl (int pri-mask) ebx)
                  (cmpl (int pri-tag) ebx)
                  (jne L_END)
                  (movl (mem (fx- 0 pri-tag) eax) ebx)
                  (if sec-mask
                      (andl (int sec-mask) ebx)
                      '(nop))
                  (cmpl (int sec-tag) ebx)
                  (je Lt)
                  L_END
                  ac))]
        [else ac])))
  (define (type-pred mask tag rand* Lt Lf ac)
    (let* ([ac (cond-branch 'je Lt Lf ac)]
           [ac (cons (cmpl (int tag) eax) ac)]
           [ac (if mask
                   (cons (andl (int mask) eax) ac)
                   ac)])
      (NonTail (car rand*) ac)))
  (define (compare-and-branch op rand* Lt Lf ac)
    (define (opposite x)
      (cadr (assq x '([je je] [jl jg] [jle jge] [jg jl] [jge jle]))))
    (cond
      [(and (constant? (car rand*)) (constant? (cadr rand*)))
       (list* 
         (movl (Simple (car rand*)) eax)
         (cmpl (Simple (cadr rand*)) eax)
         (cond-branch op Lt Lf ac))]
      [(constant? (cadr rand*))
       (list* 
         (cmpl (Simple (cadr rand*)) (Simple (car rand*)))
         (cond-branch op Lt Lf ac))]
      [(constant? (car rand*))
       (list*
         (cmpl (Simple (car rand*)) (Simple (cadr rand*)))
         (cond-branch (opposite op) Lt Lf ac))]
      [else
       (list* 
         (movl (Simple (car rand*)) eax)
         (cmpl (Simple (cadr rand*)) eax)
         (cond-branch op Lt Lf ac))]))
  (define (do-pred-prim op rand* Lt Lf ac)
    (case op
     [(fixnum?)    (type-pred fx-mask fx-tag rand* Lt Lf ac)]
     [(pair?)      (type-pred pair-mask pair-tag rand* Lt Lf ac)]
     [(char?)      (type-pred char-mask char-tag rand* Lt Lf ac)]
     [(string?)    (type-pred string-mask string-tag rand* Lt Lf ac)]
     [(symbol?)    (type-pred symbol-mask symbol-tag rand* Lt Lf ac)]
     [(procedure?) (type-pred closure-mask closure-tag rand* Lt Lf ac)]
     [(boolean?)   (type-pred bool-mask bool-tag rand* Lt Lf ac)]
     [(null?)      (type-pred #f nil rand* Lt Lf ac)]
     [($unbound-object?) (type-pred #f unbound rand* Lt Lf ac)]
     [($forward-ptr?) (type-pred #f -1 rand* Lt Lf ac)]
     [(not)        (Pred (car rand*) Lf Lt ac)]
     [(eof-object?) (type-pred #f eof rand* Lt Lf ac)]
     [(bwp-object?) (type-pred #f bwp-object rand* Lt Lf ac)]
     [($code?) 
      (indirect-type-pred vector-mask vector-tag #f code-tag 
         rand* Lt Lf ac)]
     [($fxzero?)   (type-pred #f 0 rand* Lt Lf ac)]
     [($fx= $char= eq?) (compare-and-branch 'je rand* Lt Lf ac)]
     [($fx< $char<)     (compare-and-branch 'jl rand* Lt Lf ac)]
     [($fx<= $char<=)   (compare-and-branch 'jle rand* Lt Lf ac)]
     [($fx> $char>)     (compare-and-branch 'jg rand* Lt Lf ac)]
     [($fx>= $char>=)   (compare-and-branch 'jge rand* Lt Lf ac)]
     [(vector?) 
      (indirect-type-pred vector-mask vector-tag fx-mask fx-tag 
         rand* Lt Lf ac)]
     [($record?) 
      (indirect-type-pred record-pmask record-ptag record-pmask record-ptag
         rand* Lt Lf ac)]
     [(output-port?)
      (indirect-type-pred 
        vector-mask vector-tag #f output-port-tag rand* Lt Lf ac)] 
     [(input-port?)
      (indirect-type-pred 
        vector-mask vector-tag #f input-port-tag rand* Lt Lf ac)] 
     [(port?)
      (indirect-type-pred 
        vector-mask vector-tag port-mask port-tag rand* Lt Lf ac)] 
     [(bignum?)
      (indirect-type-pred 
        vector-mask vector-tag bignum-mask bignum-tag rand* Lt Lf ac)] 
     [(flonum?)
      (indirect-type-pred 
        vector-mask vector-tag #f flonum-tag rand* Lt Lf ac)] 
     [($record/rtd?)
      (cond
        [Lf
         (list* (movl (Simple (car rand*)) eax)
                (movl eax ebx)
                (andl (int vector-mask) eax)
                (cmpl (int vector-tag) eax)
                (jne Lf)
                (movl (Simple (cadr rand*)) eax)
                (cmpl (mem (fx- disp-record-rtd vector-tag) ebx) eax)
                (jne Lf)
                (if Lt
                    (cons (jmp Lt) ac)
                    ac))]
        [Lt
         (let ([Ljoin (unique-label)])
           (list* (movl (Simple (car rand*)) eax)
                  (movl eax ebx)
                  (andl (int vector-mask) eax)
                  (cmpl (int vector-tag) eax)
                  (jne Ljoin)
                  (movl (Simple (cadr rand*)) eax)
                  (cmpl (mem (fx- disp-record-rtd vector-tag) ebx) eax)
                  (je Lt)
                  Ljoin
                  ac))]
        [else ac])]
     [(immediate?)
      (cond
        [(and Lt Lf)
         (list* (movl (Simple (car rand*)) eax)
             (movl eax ebx)
             (andl (int fx-mask) ebx)
             (je Lt)
             (andl (int 7) eax)
             (cmpl (int 7) eax)
             (je Lt)
             (jmp Lf)
             ac)]
        [Lt
         (list* (movl (Simple (car rand*)) eax)
             (movl eax ebx)
             (andl (int fx-mask) ebx)
             (je Lt)
             (andl (int 7) eax)
             (cmpl (int 7) eax)
             (je Lt)
             ac)]
        [Lf
         (let ([Ljoin (unique-label)])
           (list* 
             (movl (Simple (car rand*)) eax)
             (movl eax ebx)
             (andl (int fx-mask) ebx)
             (je Ljoin)
             (andl (int 7) eax)
             (cmpl (int 7) eax)
             (jne Lf)
             Ljoin
             ac))]
        [else ac])]
     [($ap-check-words)
      (record-case (car rand*)
        [(constant i)
         (list* (movl (pcb-ref 'allocation-redline) eax)
                (subl (Simple (cadr rand*)) eax)
                (subl (int i) eax)
                (cmpl eax apr)
                (cond-branch 'jge Lt Lf ac))]
        [else (error who "ap-check-words")])]
     [($ap-check-bytes)
      (record-case (car rand*)
        [(constant i)
         (list* (movl (Simple (cadr rand*)) eax)
                (negl eax)
                (addl (pcb-ref 'allocation-redline) eax)
                (subl (int i) eax)
                (cmpl eax apr)
                (cond-branch 'jge Lt Lf ac))]
        [else (error who "ap-check-bytes")])]
     [($ap-check-const) 
      (record-case (car rand*)
        [(constant i) 
         (if (fx<= i pagesize)
             (list* 
               (cmpl (pcb-ref 'allocation-redline) apr)
               (cond-branch 'jge Lt Lf ac))
             (list* 
               (movl (pcb-ref 'allocation-redline) eax)
               (subl (int i) eax)
               (cmpl eax apr)
               (cond-branch 'jge Lt Lf ac)))]
        [else (error who "ap-check-const")])]
     [($fp-at-base) 
      (list* 
        (movl (pcb-ref 'frame-base) eax)
        (subl (int wordsize) eax)
        (cmpl eax fpr)
        (cond-branch 'je Lt Lf ac))]
     [($interrupted?)
      (list* (movl (pcb-ref 'interrupted) eax)
             (cmpl (int 0) eax)
             (cond-branch 'jne Lt Lf ac))]
     [($fp-overflow) 
      (list* (cmpl (pcb-ref 'frame-redline) fpr)
             (cond-branch 'jle Lt Lf ac))]
     [($memq)
      (record-case (cadr rand*)
        [(constant ls)
         (let-values ([(Lt ac)
                       (if Lt 
                           (values Lt ac)
                           (let ([L (unique-label)])
                             (values L (cons L ac))))])
           (NonTail (car rand*)
             (let f ([ls ls])
               (cond
                 [(null? ls)
                  (if Lf (list* (jmp Lf) ac) ac)]
                 [else
                  (list* (cmpl (Simple (make-constant (car ls))) eax)
                         (je Lt)
                         (f (cdr ls)))]))))]
        [else
         (error 'compile 
           "BUG: second arg to $memq should be constant")])]
     [($engine-check)
      (list* (addl (int 1) (pcb-ref 'engine-counter))
             (cond-branch 'je Lt Lf ac))]
     [($vector-ref top-level-value car cdr $record-ref) 
      (do-value-prim op rand*
        (do-simple-test eax Lt Lf ac))]
     [(cons void $fxadd1 $fxsub1)
      ;;; always true
      (do-effect-prim op rand*
        (cond
          [(not Lt) ac]
          [else (cons (jmp Lt) ac)]))]
     [else 
      (error 'pred-prim "HERE unhandled ~s" op)]))
  (define (do-pred->value-prim op rand* ac)
    (case op
      [else
       (let ([Lf (unique-label)] [Lj (unique-label)])
         (do-pred-prim op rand* #f Lf
           (list* (movl (constant-val #t) eax)
                  (jmp Lj)
                  Lf
                  (movl (constant-val #f) eax)
                  Lj
                  ac)))]))
  (define (indirect-ref arg* off ac)
    (list*
      (movl (Simple (car arg*)) eax)
      (movl (mem off eax) eax)
      ac))
  (define (do-make-port tag args ac)
    (let f ([args args] [idx disp-vector-data])
      (cond
        [(null? args)
         (if (fx= idx port-size)
             (list*
               (movl (int tag) (mem 0 apr))
               (movl apr eax)
               (addl (int port-size) apr)
               (addl (int vector-tag) eax)
               ac)
             (error 'do-make-port "BUG"))]
         [else
          (list* 
            (movl (Simple (car args)) eax)
            (movl eax (mem idx apr))
            (f (cdr args) (fx+ idx wordsize)))])))
  (define (do-value-prim op arg* ac)
    (case op
      [(eof-object) (cons (movl (int eof) eax) ac)]
      [(void)       (cons (movl (int void-object) eax) ac)]
      [($fxadd1) 
       (list* (movl (Simple (car arg*)) eax)
              (addl (constant-val 1) eax)
              ac)]
      [(fxadd1)
       (NonTail (car arg*)
         (list* (movl eax ebx)
                (andl (int fx-mask) ebx)
                (jne (label SL_fxadd1_error))
                (addl (int (fxsll 1 fx-shift)) eax)
                (jo (label SL_fxadd1_error))
                ac))]
      [(fx+)
       (let foo ([a0 (car arg*)] [a1 (cadr arg*)])
         (cond
           [(simple? a1)
            (cond
              [(constant? a1)
               (let ([v (constant-value a1)])
                 (cond
                   [(fixnum? v)
                    (let ([L 
                           (let ([L (unique-label)])
                             (add-handler!
                               (list L
                                     (movl (Simple a1) ebx)
                                     (jmp (label SL_fx+_overflow))))
                             L)])
                      (NonTail a0
                        (list* 
                          (movl eax ebx)
                          (andl (int fx-mask) ebx)
                          ;;; arg in eax
                          (jne (label SL_fx+_type))
                          (addl (Simple a1) eax)
                          (jo L)
                          ac)))]
                   [else
                    (NonTail a0
                      (list*
                        (movl (Simple a1) eax)
                        ;;; arg in eax
                        (jmp (label SL_fx+_type))
                        ac))]))]
              [else
               (NonTail a0
                 (list* 
                   (movl eax ecx)
                   (movl (Simple a1) ebx)
                   (orl ebx ecx)
                   (andl (int fx-mask) ecx)
                   ;;; args in eax, ebx
                   (jne (label SL_fx+_types))
                   (addl ebx eax)
                   ;;; args in eax (ac),ebx
                   (jo (label SL_fx+_overflow))
                   ac))])]
           [else (foo a1 a0)]))]
      [(fxsub1)
       (NonTail (car arg*)
         (list* (movl eax ebx)
                (andl (int fx-mask) ebx)
                (jne (label SL_fxsub1_error))
                (subl (int (fxsll 1 fx-shift)) eax)
                (jo (label SL_fxsub1_error))
                ac))] 
      [($fxsub1) 
       (list* (movl (Simple (car arg*)) eax)
              (addl (constant-val -1) eax)
              ac)]
      [($fx+) 
       (list* (movl (Simple (car arg*)) eax)
              (addl (Simple (cadr arg*)) eax)
              ac)]
      [($fx-) 
       (list* (movl (Simple (car arg*)) eax)
              (subl (Simple (cadr arg*)) eax)
              ac)]
      ;X; [(fx-)
      ;X;  (let ([a0 (car arg*)] [a1 (cadr arg*)])
      ;X;    (cond
      ;X;      [(simple? a1)
      ;X;       (cond
      ;X;         [(and (constant? a1) (fixnum? (constant-value a1)))
      ;X;          (NonTail a0
      ;X;             (movl eax ebx)
      ;X;             (andl (int fx-mask) ebx)
      ;X;             (jne (label SL_fx-_eax_error))
      ;X;             (subl (Simple a1) eax)
      ;X;             (jo (label SL_fx-_overflow))
      ;X;             ac)]
      ;X;         [else
      ;X;          (NonTail a0
      ;X;             (movl eax ebx)
      ;X;             (movl (Simple a1) ecx)
      ;X;             (orl ecx ebx)
      ;X;             (andl (int fx-mask) ebx)
      ;X;             (jne (label SL_fx-_eax/ecx_error))
      ;X;             (subl ecx eax)
      ;X;             (jo (label SL_fx-_overflow))
      ;X;             ac)])]
      ;X;      ljfhjdhfkjdhfjk]
      [($fx*)
       (cond
         [(constant? (car arg*))
          (record-case (car arg*)
            [(constant c)
             (unless (fixnum? c)
               (error who "invalid arg ~s to fx*" c))
             (list* (movl (Simple (cadr arg*)) eax)
                    (imull (int c) eax)
                    ac)])]
         [(constant? (cadr arg*))
          (record-case (cadr arg*)
            [(constant c)
             (unless (fixnum? c)
               (error who "invalid arg ~s to fx*" c))
             (list* (movl (Simple (car arg*)) eax)
                    (imull (int c) eax)
                    ac)])]
         [else
          (list* (movl (Simple (car arg*)) eax)
                 (sarl (int fx-shift) eax)
                 (imull (Simple (cadr arg*)) eax)
                 ac)])]
      [($fxquotient)
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ecx)
              (cltd)
              (idivl ecx)
              (sall (int fx-shift) eax)
              ac)]
      [($fxmodulo)
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl eax ecx)
              (xorl ebx ecx)
              (sarl (int (fxsub1 (fx* wordsize 8))) ecx)
              (andl ebx ecx)
              (cltd) ;;; sign extend eax into edx:eax
              (idivl ebx) ;;; divide edx:eax by ebx 
                          ;;; quotient goes to eax
                          ;;; remainder to edx
              (movl edx eax)
              (addl ecx eax)
              ac)]
      [($fxlogor) 
       (list* (movl (Simple (car arg*)) eax)
              (orl (Simple (cadr arg*)) eax)
              ac)]
      [($fxlogand) 
       (list* (movl (Simple (car arg*)) eax)
              (andl (Simple (cadr arg*)) eax)
              ac)]
      [($fxlogxor) 
       (list* (movl (Simple (car arg*)) eax)
              (xorl (Simple (cadr arg*)) eax)
              ac)]
      [($fxsra)
       (record-case (cadr arg*)
         [(constant i)
          (unless (fixnum? i) (error who "invalid arg to fxsra"))
          (list* (movl (Simple (car arg*)) eax)
                 (sarl (int (fx+ i fx-shift)) eax)
                 (sall (int fx-shift) eax)
                 ac)]
         [else
          (list* (movl (Simple (car arg*)) eax)
                 (movl (Simple (cadr arg*)) ecx)
                 (sarl (int fx-shift) ecx)
                 (sarl (int fx-shift) eax)
                 (sarl cl eax)
                 (sall (int fx-shift) eax)
                 ac)])]
      [($fxsll)
       (record-case (cadr arg*)
         [(constant i)
          (unless (fixnum? i) (error who "invalid arg to fxsll"))
          (list* (movl (Simple (car arg*)) eax)
                 (sall (int i) eax)
                 ac)]
         [else
          (list* (movl (Simple (car arg*)) eax)
                 (movl (Simple (cadr arg*)) ecx)
                 (sarl (int fx-shift) ecx)
                 (sall cl eax)
                 ac)])]
      [($fixnum->char)  
       (list* (movl (Simple (car arg*)) eax)
              (sall (int (fx- char-shift fx-shift)) eax)
              (orl (int char-tag) eax)
              ac)]
      [($char->fixnum) 
       (list* (movl (Simple (car arg*)) eax)
              (sarl (int (fx- char-shift fx-shift)) eax)
              ac)]
      [($fxlognot) 
       (list* (movl (Simple (car arg*)) eax)
              (orl (int fx-mask) eax)
              (notl eax)
              ac)]
      [($car) (indirect-ref arg* (fx- disp-car pair-tag) ac)]
      [($cdr) (indirect-ref arg* (fx- disp-cdr pair-tag) ac)]
      [($vector-length) 
       (indirect-ref arg* (fx- disp-vector-length vector-tag) ac)]
      [($string-length) 
       (indirect-ref arg* (fx- disp-string-length string-tag) ac)]
      [($symbol-string) 
       (indirect-ref arg* (fx- disp-symbol-string symbol-tag) ac)]
      [($symbol-unique-string) 
       (indirect-ref arg* (fx- disp-symbol-unique-string symbol-tag) ac)]
      [($symbol-value) 
       (indirect-ref arg* (fx- disp-symbol-value symbol-tag) ac)]
      [(primitive-ref) 
       (indirect-ref arg* (fx- disp-symbol-system-value symbol-tag) ac)]
      [($tcbucket-key) 
       (indirect-ref arg* (fx- disp-tcbucket-key vector-tag) ac)]
      [($tcbucket-val) 
       (indirect-ref arg* (fx- disp-tcbucket-val vector-tag) ac)]
      [($tcbucket-next) 
       (indirect-ref arg* (fx- disp-tcbucket-next vector-tag) ac)]
      [($port-handler) 
       (indirect-ref arg* (fx- disp-port-handler vector-tag) ac)]
      [($port-input-buffer) 
       (indirect-ref arg* (fx- disp-port-input-buffer vector-tag) ac)]
      [($port-input-index) 
       (indirect-ref arg* (fx- disp-port-input-index vector-tag) ac)]
      [($port-input-size) 
       (indirect-ref arg* (fx- disp-port-input-size vector-tag) ac)]
      [($port-output-buffer) 
       (indirect-ref arg* (fx- disp-port-output-buffer vector-tag) ac)]
      [($port-output-index) 
       (indirect-ref arg* (fx- disp-port-output-index vector-tag) ac)]
      [($port-output-size) 
       (indirect-ref arg* (fx- disp-port-output-size vector-tag) ac)]
      [(pointer-value)
       (list*
         (movl (Simple (car arg*)) eax)
         (sarl (int fx-shift) eax)
         (sall (int fx-shift) eax)
         ac)]
      [($symbol-plist) 
       (indirect-ref arg* (fx- disp-symbol-plist symbol-tag) ac)]
      [($record-rtd) 
       (indirect-ref arg* (fx- disp-record-rtd record-ptag) ac)]
      [($constant-ref)
       (list* (movl (Simple (car arg*)) eax) ac)]
      [(car cdr)
       (let ([x (car arg*)])
         (NonTail x
           (list* 
             (movl eax ebx)
             (andl (int pair-mask) eax)
             (cmpl (int pair-tag) eax)
             (if (eq? op 'car)
                 (list* 
                   (jne (label SL_car_error))
                   (movl (mem (fx- disp-car pair-tag) ebx) eax)
                   ac)
                 (list* 
                   (jne (label SL_cdr_error))
                   (movl (mem (fx- disp-cdr pair-tag) ebx) eax)
                   ac)))))]
      [(cadr)
       (NonTail (car arg*)
          (list* 
            (movl eax ebx)
            (andl (int pair-mask) eax)
            (cmpl (int pair-tag) eax)
            (jne (label SL_cadr_error))
            (movl (mem (fx- disp-cdr pair-tag) ebx) eax)
            (movl eax ecx)
            (andl (int pair-mask) eax)
            (cmpl (int pair-tag) eax)
            (jne (label SL_cadr_error))
            (movl (mem (fx- disp-car pair-tag) ecx) eax)
            ac))]
      [(top-level-value)
       (let ([x (car arg*)])
         (cond
           [(constant? x)
            (let ([v (constant-value x)])
              (cond
                [(symbol? v)
                 (list* 
                   (movl (mem (fx- disp-symbol-value symbol-tag) (obj v)) eax)
                   (movl (obj v) ebx)
                   (cmpl (int unbound) eax)
                   (je (label SL_top_level_value_error))
                   ac)]
                [else
                 (list* 
                   (movl (obj v) ebx)
                   (jmp (label SL_top_level_value_error))
                   ac)]))]
           [else
            (NonTail x
              (list* 
                (movl eax ebx)
                (andl (int symbol-mask) eax)
                (cmpl (int symbol-tag) eax)
                (jne (label SL_top_level_value_error))
                (movl (mem (fx- disp-symbol-value symbol-tag) ebx) eax)
                (cmpl (int unbound) eax)
                (je (label SL_top_level_value_error))
                ac))]))]
      [($vector-ref) 
       (list* (movl (Simple (car arg*)) ebx)
              (addl (Simple (cadr arg*)) ebx)
              (movl (mem (fx- disp-vector-data vector-tag) ebx) eax)
              ac)]
      [($record-ref) 
       (list* (movl (Simple (car arg*)) ebx)
              (addl (Simple (cadr arg*)) ebx)
              (movl (mem (fx- disp-record-data record-ptag) ebx) eax)
              ac)]
      [($code-ref) 
       (list* (movl (Simple (cadr arg*)) ebx)
              (sarl (int fx-shift) ebx)
              (addl (Simple (car arg*)) ebx)
              (movl (int 0) eax)
              (movb (mem (fx- disp-code-data vector-tag) ebx) ah)
              (sarl (int (fx- 8 fx-shift)) eax)
              ac)]
      [($string-ref) 
       (list* (movl (Simple (cadr arg*)) ebx)
              (sarl (int fx-shift) ebx)
              (addl (Simple (car arg*)) ebx)
              (movl (int char-tag) eax)
              (movb (mem (fx- disp-string-data string-tag) ebx) ah)
              ac)]
      [($make-string) 
       (list* (movl (Simple (car arg*)) ebx)
              (movl ebx (mem disp-string-length apr))
              (movl apr eax)
              (addl (int string-tag) eax)
              (sarl (int fx-shift) ebx)
              (addl ebx apr)
              (movb (int 0) (mem disp-string-data apr))
              (addl (int (fx+ disp-string-data object-alignment)) apr)
              (sarl (int align-shift) apr)
              (sall (int align-shift) apr)
              ac)]
      [($make-vector) 
       (list* (movl (Simple (car arg*)) ebx)
              (movl ebx (mem disp-vector-length apr))
              (movl apr eax)
              (addl (int vector-tag) eax)
              (addl ebx apr)
              (addl (int (fx+ disp-vector-data (fxsub1 object-alignment))) apr)
              (sarl (int align-shift) apr)
              (sall (int align-shift) apr)
              ac)]
      [($make-record) 
       (list* (movl (Simple (car arg*)) eax)
              (movl eax (mem disp-record-rtd apr))
              (movl apr eax)
              (addl (int record-ptag) eax)
              (addl (Simple (cadr arg*)) apr)
              (addl (int (fx+ disp-record-data (fxsub1 object-alignment))) apr)
              (sarl (int align-shift) apr)
              (sall (int align-shift) apr)
              ac)]
      [(cons)
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl eax (mem disp-car apr))
              (movl ebx (mem disp-cdr apr))
              (leal (mem pair-tag apr) eax)
              (addl (int (align pair-size)) apr)
              ac)]
      [(list)
       (cond
         [(null? arg*) (NonTail (make-constant '()) ac)]
         [else
          (list*
            (addl (int pair-tag) apr)
            (movl apr eax)
            (let f ([a (car arg*)] [d (cdr arg*)])
              (list*
                (movl (Simple a) ebx)
                (movl ebx (mem (fx- disp-car pair-tag) apr))
                (if (null? d)
                    (list*
                      (movl (int nil) (mem (fx- disp-cdr pair-tag) apr))
                      (addl (int (fx- pair-size pair-tag)) apr)
                      ac)
                    (list*
                      (addl (int pair-size) apr)
                      (movl apr 
                          (mem (fx- disp-cdr (fx+ pair-tag pair-size)) apr))
                      (f (car d) (cdr d)))))))])]
      [(list*)
       (cond
         [(fx= (length arg*) 1) (NonTail (car arg*) ac)]
         [(fx= (length arg*) 2) (NonTail (make-primcall 'cons arg*) ac)]
         [else
          (list*
            (addl (int pair-tag) apr)
            (movl apr eax)
            (let f ([a (car arg*)] [b (cadr arg*)] [d (cddr arg*)])
              (list*
                (movl (Simple a) ebx)
                (movl ebx (mem (fx- disp-car pair-tag) apr))
                (if (null? d)
                    (list*
                      (movl (Simple b) ebx)
                      (movl ebx (mem (fx- disp-cdr pair-tag) apr))
                      (addl (int (fx- pair-size pair-tag)) apr)
                      ac)
                    (list*
                      (addl (int pair-size) apr)
                      (movl apr 
                          (mem (fx- disp-cdr (fx+ pair-tag pair-size)) apr))
                      (f b (car d) (cdr d)))))))])]
      [($make-symbol)
       (list* (movl (Simple (car arg*)) eax)
              (movl eax (mem disp-symbol-string apr))
              (movl (int 0) (mem disp-symbol-unique-string apr))
              (movl (int unbound) (mem disp-symbol-value apr))
              (movl (int nil) (mem disp-symbol-plist apr))
              (movl (int unbound) (mem disp-symbol-system-value apr))
              (movl (int 0) (mem disp-symbol-function apr))
              (movl (int 0) (mem disp-symbol-error-function apr))
              (movl (int 0) (mem disp-symbol-unused apr))
              (movl apr eax)
              (addl (int symbol-tag) eax)
              (addl (int (align symbol-size)) apr)
              ac)]
      [($make-port/input)  (do-make-port input-port-tag arg* ac)]
      [($make-port/output) (do-make-port output-port-tag arg* ac)]
      [($make-port/both)   (do-make-port input/output-port-tag arg* ac)]
      [($make-tcbucket)
       (list* (movl (Simple (car arg*)) eax)
              (movl eax (mem disp-tcbucket-tconc apr))
              (movl (Simple (cadr arg*)) eax)
              (movl eax (mem disp-tcbucket-key apr))
              (movl (Simple (caddr arg*)) eax)
              (movl eax (mem disp-tcbucket-val apr))
              (movl (Simple (cadddr arg*)) eax)
              (movl eax (mem disp-tcbucket-next apr))
              (movl apr eax)
              (addl (int vector-tag) eax)
              (addl (int (align tcbucket-size)) apr)
              ac)]
      [($record) 
       (let ([rtd (car arg*)]
             [ac 
              (let f ([arg* (cdr arg*)] [idx disp-record-data])
                (cond
                  [(null? arg*) 
                   (list* (movl apr eax)
                          (addl (int vector-tag) eax)
                          (addl (int (align idx)) apr)
                          ac)]
                  [else 
                   (list* (movl (Simple (car arg*)) eax)
                          (movl eax (mem idx apr))
                          (f (cdr arg*) (fx+ idx wordsize)))]))])
         (cond
           [(constant? rtd)
            (list* (movl (Simple rtd) (mem 0 apr)) ac)]
           [else
            (list* (movl (Simple rtd) eax) (movl eax (mem 0 apr)) ac)]))]
      [(vector) 
       (let f ([arg* arg*] [idx disp-vector-data])
         (cond
           [(null? arg*) 
            (list* (movl apr eax)
                   (addl (int vector-tag) eax)
                   (movl (int (fx- idx disp-vector-data))
                         (mem disp-vector-length apr))
                   (addl (int (align idx)) apr)
                   ac)]
           [else 
            (list* (movl (Simple (car arg*)) eax)
                   (movl eax (mem idx apr))
                   (f (cdr arg*) (fx+ idx wordsize)))]))]
     [($string)
      (let f ([arg* arg*] [idx disp-string-data])
        (cond
          [(null? arg*) 
           (list* (movb (int 0) (mem idx apr))
                  (movl apr eax)
                  (addl (int string-tag) eax)
                  (movl (int (fx* (fx- idx disp-string-data) wordsize))
                        (mem disp-string-length apr))
                  (addl (int (align (fxadd1 idx))) apr)
                  ac)]
          [else
           (record-case (car arg*)
             [(constant c) 
              (unless (char? c) (error who  "invalid arg to string ~s" x))
              (list* (movb (int (char->integer c)) (mem idx apr))
                     (f (cdr arg*) (fxadd1 idx)))]
             [else
              (list* (movl (Simple (car arg*)) ebx)
                     (movb bh (mem idx apr))
                     (f (cdr arg*) (fxadd1 idx)))])]))]
     [($current-frame)
      (list* (movl (pcb-ref 'next-continuation) eax) ac)]
     [($arg-list)
      (list* (movl (pcb-ref 'arg-list) eax) ac)]
     [($seal-frame-and-call) 
      (list* (movl (Simple (car arg*)) cpr) ; proc
             (movl (pcb-ref 'frame-base) eax)
             ; eax=baseofstack
             (movl (mem (fx- 0 wordsize) eax) ebx) ; underflow handler
             (movl ebx (mem (fx- 0 wordsize) fpr)) ; set
             ; create a new cont record
             (movl (int continuation-tag) (mem 0 apr))
             (movl fpr (mem disp-continuation-top apr))
             ; compute the size of the captured frame
             (movl eax ebx) 
             (subl fpr ebx) 
             (subl (int wordsize) ebx)
             ; and store it
             (movl ebx (mem disp-continuation-size apr))
             ; load next cont     (K 20)
             (movl (pcb-ref 'next-continuation) ebx)
             ; and store it
             (movl ebx (mem disp-continuation-next apr))
             ; adjust ap
             (movl apr eax)
             (addl (int vector-tag) eax)
             (addl (int continuation-size) apr)
             ; store new cont in current-cont
             (movl eax (pcb-ref 'next-continuation))
             ; adjust fp
             (movl fpr (pcb-ref 'frame-base))
             (subl (int wordsize) fpr)
             ; tail-call f
             (movl eax (mem (fx- 0 wordsize) fpr))
             (movl (int (argc-convention 1)) eax)
             (tail-indirect-cpr-call)
             ac)]
     [($code-size) 
      (indirect-ref arg* (fx- disp-code-instrsize vector-tag)  ac)]
     [($code-reloc-vector) 
       (indirect-ref arg* (fx- disp-code-relocsize vector-tag) ac)]
     [($code-freevars) 
       (indirect-ref arg* (fx- disp-code-freevars vector-tag) ac)]
     [($closure-code)
      (indirect-ref arg* (fx- disp-closure-code closure-tag)
        (list* (addl (int (fx- vector-tag disp-code-data)) eax)
               ac))]
     [($set-car! $set-cdr! $vector-set! $string-set! $exit
       $set-symbol-value! $set-symbol-plist! 
       $code-set!  primitive-set! 
       $set-code-object! $set-code-object+offset! $set-code-object+offset/rel!
       $record-set!
       $set-port-input-index! $set-port-input-size!
       $set-port-output-index! $set-port-output-size!) 
      (do-effect-prim op arg*
        (cons (movl (int void-object) eax) ac))]
     [(fixnum? bignum? flonum? immediate? $fxzero? boolean? char? pair? 
       vector? string? symbol?
       procedure? null? not eof-object? $fx= $fx< $fx<= $fx> $fx>= eq?
       $char= $char< $char<= $char> $char>= $unbound-object? $code? 
       $record? $record/rtd? bwp-object? port? input-port? output-port?) 
      (do-pred->value-prim op arg* ac)]
     [($code->closure)
      (list* 
        (movl (Simple (car arg*)) eax)
        (addl (int (fx- disp-code-data vector-tag)) eax)
        (movl eax (mem 0 apr))
        (movl apr eax)
        (addl (int closure-tag) eax)
        (addl (int (align disp-closure-data)) apr)
        ac)]
     [($frame->continuation)
      (NonTail 
        (make-closure (make-code-loc SL_continuation_code) arg*)
        ac)]
     [($make-call-with-values-procedure)
      (NonTail 
        (make-closure (make-code-loc SL_call_with_values) arg*)
        ac)]
     [($make-values-procedure)
      (NonTail 
        (make-closure (make-code-loc SL_values) arg*)
        ac)]
     [($memq)
      (record-case (cadr arg*)
        [(constant ls)
         (let-values ([(Lt ac)
                       (let ([L (unique-label)])
                         (values L (cons L ac)))])
           (NonTail (car arg*)
             (list*
               (movl eax ebx)
               (let f ([ls ls])
                 (cond
                   [(null? ls)
                    (list* (movl (int bool-f) eax) ac)]
                   [else
                    (list* 
                      (movl (obj ls) eax)
                      (cmpl (Simple (make-constant (car ls))) ebx)
                      (je Lt)
                      (f (cdr ls)))])))))]
        [else
         (error 'compile 
           "BUG: second arg to $memq should be constant")])]
     [else
      (error 'value-prim "unhandled ~s" op)]))
  (define (indirect-assignment arg* offset ac)
    (list*
      (movl (Simple (car arg*)) eax)
      (movl (Simple (cadr arg*)) ebx)
      (movl ebx (mem offset eax))
       ;;; record side effect
      (addl (int offset) eax)
      (shrl (int pageshift) eax)
      (sall (int wordshift) eax)
      (addl (pcb-ref 'dirty-vector) eax)
      (movl (int dirty-word) (mem 0 eax))
      ac))
  (define (do-effect-prim op arg* ac)
    (case op
      [($vector-set!) 
       (list* (movl (Simple (car arg*)) ebx)
              (addl (Simple (cadr arg*)) ebx)
              (addl (int (fx- disp-vector-data vector-tag)) ebx)
              (movl (Simple (caddr arg*)) eax)
              (movl eax (mem 0 ebx))
              ;;; record side effect
              (shrl (int pageshift) ebx)
              (sall (int wordshift) ebx)
              (addl (pcb-ref 'dirty-vector) ebx)
              (movl (int dirty-word) (mem 0 ebx))
              ac)]
      [($code-set!) 
       (list* (movl (Simple (cadr arg*)) eax)   ;;; index
              (sarl (int fx-shift) eax)         ;;; unfixed
              (addl (Simple (car arg*)) eax)    ;;; + code
              (movl (Simple (caddr arg*)) ebx)  ;;; value (fixnum)
              (sall (int (fx- 8 fx-shift)) ebx) ;;; move to high byte
              (movb bh (mem (fx- disp-code-data vector-tag) eax))
              ac)]
      [($string-set!) 
       (list* (movl (Simple (cadr arg*)) eax)
              (sarl (int fx-shift) eax)
              (addl (Simple (car arg*)) eax)
              (movl (Simple (caddr arg*)) ebx)
              (movb bh (mem (fx- disp-string-data string-tag) eax))
              ac)]
      [($set-car!) 
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl ebx (mem (fx- disp-car pair-tag) eax))
               ;;; record side effect
               (shrl (int pageshift) eax)
               (sall (int wordshift) eax)
               (addl (pcb-ref 'dirty-vector) eax)
               (movl (int dirty-word) (mem 0 eax))
              ac)]
      [($set-cdr!) 
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl ebx (mem (fx- disp-cdr pair-tag) eax))
               ;;; record side effect
               (shrl (int pageshift) eax)
               (sall (int wordshift) eax)
               (addl (pcb-ref 'dirty-vector) eax)
               (movl (int dirty-word) (mem 0 eax))
              ac)]
      [($set-tcbucket-key!)
       (indirect-assignment arg* (fx- disp-tcbucket-key vector-tag) ac)]
      [($set-tcbucket-val!)
       (indirect-assignment arg* (fx- disp-tcbucket-val vector-tag) ac)]
      [($set-tcbucket-next!)
       (indirect-assignment arg* (fx- disp-tcbucket-next vector-tag) ac)]
      [($set-tcbucket-tconc!)
       (indirect-assignment arg* (fx- disp-tcbucket-tconc vector-tag) ac)]
      [($set-port-input-index!)
       (list*
         (movl (Simple (car arg*)) eax)
         (movl (Simple (cadr arg*)) ebx)
         (movl ebx (mem (fx- disp-port-input-index vector-tag) eax))
         ac)]
      [($set-port-input-size!)
       (list*
         (movl (Simple (car arg*)) eax)
         (movl (Simple (cadr arg*)) ebx)
         (movl (int 0) (mem (fx- disp-port-input-index vector-tag) eax))
         (movl ebx (mem (fx- disp-port-input-size vector-tag) eax))
         ac)]
      [($set-port-output-index!)
       (list*
         (movl (Simple (car arg*)) eax)
         (movl (Simple (cadr arg*)) ebx)
         (movl ebx (mem (fx- disp-port-output-index vector-tag) eax))
         ac)]
      [($set-port-output-size!)
       (list*
         (movl (Simple (car arg*)) eax)
         (movl (Simple (cadr arg*)) ebx)
         (movl (int 0) (mem (fx- disp-port-output-index vector-tag) eax))
         (movl ebx (mem (fx- disp-port-output-size vector-tag) eax))
         ac)]
      [($set-symbol-value!) 
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl ebx (mem (fx- disp-symbol-value symbol-tag) eax))
              (movl (mem (fx- disp-symbol-error-function symbol-tag) eax) ebx)
              (movl ebx (mem (fx- disp-symbol-function symbol-tag) eax))
               ;;; record side effect
               (addl (int (fx- disp-symbol-value symbol-tag)) eax)
               (shrl (int pageshift) eax)
               (sall (int wordshift) eax)
               (addl (pcb-ref 'dirty-vector) eax)
               (movl (int dirty-word) (mem 0 eax))
              ac)]
      [(primitive-set!) 
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl ebx (mem (fx- disp-symbol-system-value symbol-tag) eax))
               ;;; record side effect
               (addl (int (fx- disp-symbol-system-value symbol-tag)) eax)
               (shrl (int pageshift) eax)
               (sall (int wordshift) eax)
               (addl (pcb-ref 'dirty-vector) eax)
               (movl (int dirty-word) (mem 0 eax))
              ac)]
      [($set-symbol-plist!) 
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl ebx (mem (fx- disp-symbol-plist symbol-tag) eax))
               ;;; record side effect
               (addl (int (fx- disp-symbol-plist symbol-tag)) eax)
               (shrl (int pageshift) eax)
               (sall (int wordshift) eax)
               (addl (pcb-ref 'dirty-vector) eax)
               (movl (int dirty-word) (mem 0 eax))
              ac)]
      [($set-symbol-unique-string!) 
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl ebx (mem (fx- disp-symbol-unique-string symbol-tag) eax))
               ;;; record side effect
               (addl (int (fx- disp-symbol-unique-string symbol-tag)) eax)
               (shrl (int pageshift) eax)
               (sall (int wordshift) eax)
               (addl (pcb-ref 'dirty-vector) eax)
               (movl (int dirty-word) (mem 0 eax))
              ac)]
      [($set-symbol-string!) 
       (list* (movl (Simple (car arg*)) eax)
              (movl (Simple (cadr arg*)) ebx)
              (movl ebx (mem (fx- disp-symbol-string symbol-tag) eax))
               ;;; record side effect
               (addl (int (fx- disp-symbol-string symbol-tag)) eax)
               (shrl (int pageshift) eax)
               (sall (int wordshift) eax)
               (addl (pcb-ref 'dirty-vector) eax)
               (movl (int dirty-word) (mem 0 eax))
              ac)] 
      [($record-set!) 
       (list* (movl (Simple (car arg*)) ebx)
              (addl (Simple (cadr arg*)) ebx)
              (movl (Simple (caddr arg*)) eax)
              (addl (int (fx- disp-record-data record-ptag)) ebx)
              (movl eax (mem 0 ebx))
               ;;; record side effect
               (shrl (int pageshift) ebx)
               (sall (int wordshift) ebx)
               (addl (pcb-ref 'dirty-vector) ebx)
               (movl (int dirty-word) (mem 0 ebx))
              ac)]
      [($unset-interrupted!)
       (list* (movl (int 0) (pcb-ref 'interrupted))
              ac)]
      [(cons pair? void $fxadd1 $fxsub1 $record-ref $fx=
             symbol? eq?)
       (let f ([arg* arg*])
         (cond
           [(null? arg*) ac]
           [else 
            (Effect (car arg*) (f (cdr arg*)))]))]
      [(car cdr top-level-value) ;;; may signal an error
       (do-value-prim op arg* ac)]
      [else 
       (error 'do-effect-prim "unhandled op ~s" op)]))
 (define (do-simple-test x Lt Lf ac)
    (unless (or Lt Lf)
      (error 'Pred "no labels"))
    (cond
      [(not Lt)
       (list* (cmpl (int bool-f) x) (je Lf) ac)]
      [(not Lf)
       (list* (cmpl (int bool-f) x) (jne Lt) ac)]
      [else
       (list* (cmpl (int bool-f) x) (je Lf) (jmp Lt) ac)]))
  (define (simple? x) 
    (record-case x
      [(cp-var)    #t]
      [(frame-var) #t]
      [(constant)  #t]
      [(code-loc)  #t]
      [(primref)   #t]
      [(closure)   #t]
      [else        #f]))
  (define (Simple x)
    (record-case x
      [(cp-var i) 
       (mem (fx+ (fx* i wordsize) (fx- disp-closure-data closure-tag)) cpr)]
      [(frame-var i) (mem (fx* i (fx- 0 wordsize)) fpr)]
      [(constant c) (constant-val c)]
      [(code-loc label) (label-address label)]
      [(primref op) (primref-loc op)]
      [(closure label free) 
       (cond
         [(null? free) (obj x)]
         [else (error 'Simple "BUG: not a thunk ~s" x)])]
      [else (error 'Simple "what ~s" x)]))
  (define (do-fix lhs* rhs* ac)
    ;;; 1. first, set the code pointers in the right places
    ;;; 2. next, for every variable appearing in the rhs* but is not in
    ;;;    the lhs*, load it once and set it everywhere it occurs.
    ;;; 3. next, compute the values of the lhs*, and for every computed
    ;;;    value, store it on the stack, and set it everywhere it occurs
    ;;;    in the rhs*
    ;;; 4. that's it.
    (define (closure-size x)
      (align (fx+ disp-closure-data
                  (fx* wordsize (length (closure-free* x))))))
    (define (assign-codes rhs* n* i ac)
      (cond
        [(null? rhs*) ac]
        [else
         (record-case (car rhs*)
           [(closure label free*)
            (cons (movl (Simple label) (mem i apr))
                  (assign-codes 
                    (cdr rhs*) (cdr n*) (fx+ i (car n*)) ac))])]))
    (define (whack-free x i n* rhs* ac)
      (cond
        [(null? rhs*) ac]
        [else
         (let ([free (closure-free* (car rhs*))])
           (let f ([free free] [j (fx+ i disp-closure-data)])
             (cond
               [(null? free) 
                (whack-free x (fx+ i (car n*)) (cdr n*) (cdr rhs*) ac)]
               [(eq? (car free) x)
                (cons
                  (movl eax (mem j apr))
                  (f (cdr free) (fx+ j wordsize)))]
               [else (f (cdr free) (fx+ j wordsize))])))]))
    (define (assign-nonrec-free* rhs* all-rhs* n* seen ac)
      (cond
        [(null? rhs*) ac]
        [else
         (let f ([ls (closure-free* (car rhs*))] [seen seen])
           (cond
             [(null? ls) 
              (assign-nonrec-free* (cdr rhs*) all-rhs* n* seen ac)]
             [(memq (car ls) seen) (f (cdr ls) seen)]
             [else
              (cons
                (movl (Simple (car ls)) eax)
                (whack-free (car ls) 0 n* all-rhs*
                  (f (cdr ls) (cons (car ls) seen))))]))]))
    (define (assign-rec-free* lhs* rhs* all-n* ac)
      (list* (movl apr eax)
             (addl (int closure-tag) eax)
             (let f ([lhs* lhs*] [n* all-n*])
               (cond
                 [(null? (cdr lhs*)) 
                  (cons
                    (movl eax (Simple (car lhs*)))
                    (whack-free (car lhs*) 0 all-n* rhs* ac))]
                 [else
                  (cons
                    (movl eax (Simple (car lhs*)))
                    (whack-free (car lhs*) 0 all-n* rhs*
                      (cons
                        (addl (int (car n*)) eax)
                        (f (cdr lhs*) (cdr n*)))))]))))
    (define (sum ac ls)
      (cond
        [(null? ls) ac]
        [else (sum (fx+ ac (car ls)) (cdr ls))]))
    (define partition
      (lambda (lhs* rhs*)
        (let f ([lhs* lhs*] [rhs* rhs*] 
                [tlhs* '()] [trhs* '()]
                [clhs* '()] [crhs* '()])
          (cond
            [(null? lhs*)
             (values tlhs* trhs* clhs* crhs*)]
            [(null? (closure-free* (car rhs*)))
             (f (cdr lhs*) (cdr rhs*) 
                (cons (car lhs*) tlhs*) (cons (car rhs*) trhs*)
                clhs* crhs*)]
            [else
             (f (cdr lhs*) (cdr rhs*) 
                tlhs* trhs*
                (cons (car lhs*) clhs*) 
                (cons (car rhs*) crhs*))]))))
    (define do-closures
      (lambda (lhs* rhs* ac)
        (let* ([n* (map closure-size rhs*)])
          (assign-codes rhs* n* 0
            (assign-nonrec-free* rhs* rhs* n* lhs*
              (assign-rec-free* lhs* rhs* n*
                (cons (addl (int (sum 0 n*)) apr) ac)))))))
    (define do-thunks
      (lambda (lhs* rhs* ac)
        (cond
          [(null? lhs*) ac]
          [else
           (do-thunks (cdr lhs*) (cdr rhs*)
              (cons (movl (obj (car rhs*)) 
                          (idx->frame-loc
                            (frame-var-idx (car lhs*))))
                    ac))])))
    (let-values ([(tlhs* trhs* clhs* crhs*)
                  (partition lhs* rhs*)])
      (cond
        [(null? clhs*)
         (do-thunks tlhs* trhs* ac)]
        [(null? tlhs*)
         (do-closures clhs* crhs* ac)]
        [else
         (do-thunks tlhs* trhs* 
           (do-closures clhs* crhs* ac))])))
  (define (frame-adjustment offset)
    (fx* (fxsub1 offset) (fx- 0 wordsize)))
  (define (NonTail x ac) 
    (record-case x
      [(constant c)
       (cons (movl (constant-val c) eax) ac)]
      [(frame-var)
       (cons (movl (Simple x) eax) ac)]
      [(cp-var)
       (cons (movl (Simple x) eax) ac)]
      [(foreign-label L) 
       (cons (movl (list 'foreign-label L) eax) ac)]
      [(primref c)
       (cons (movl (primref-loc c) eax) ac)]
      [(closure label arg*)
       (cond
         [(null? arg*)
          (cons (movl (obj x) eax) ac)]
         [else
          (let f ([arg* arg*] [off disp-closure-data])
            (cond
              [(null? arg*)
               (list* (movl (Simple label) (mem 0 apr))
                      (movl apr eax)
                      (addl (int (align off)) apr)
                      (addl (int closure-tag) eax)
                      ac)]
              [else
               (list* (movl (Simple (car arg*)) eax)
                      (movl eax (mem off apr))
                      (f (cdr arg*) (fx+ off wordsize)))]))])]
      [(conditional test conseq altern)
       (let ([Lj (unique-label)] [Lf (unique-label)])
         (Pred test #f Lf
           (NonTail conseq
             (list* (jmp Lj) Lf (NonTail altern (cons Lj ac))))))]
      [(seq e0 e1)
       (Effect e0 (NonTail e1 ac))]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* (NonTail body ac))]
      [(primcall op rand*)
       (do-value-prim op rand* ac)]
      [(new-frame base-idx size body)
       (NonTail body ac)]
      [(call-cp)
       (handle-call-cp x ac NonTail)]
      [else (error 'NonTail "invalid expression ~s" x)]))
  (define (Pred x Lt Lf ac)
    (record-case x
      [(frame-var i) 
       (do-simple-test (idx->frame-loc i) Lt Lf ac)]
      [(cp-var i) 
       (do-simple-test (Simple x) Lt Lf ac)]
      [(constant c) 
       (if c
           (if Lt (cons (jmp Lt) ac) ac)
           (if Lf (cons (jmp Lf) ac) ac))]
      [(closure) 
       (if Lt (cons (jmp Lt) ac) ac)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* (Pred body Lt Lf ac))]
      [(primcall op rand*)
       (do-pred-prim op rand* Lt Lf ac)]
      [(conditional test conseq altern)
       (cond
         [(not Lt) 
          (let ([Lj^ (unique-label)] [Lf^ (unique-label)])
            (Pred test #f Lf^
              (Pred conseq Lj^ Lf
                (cons Lf^
                  (Pred altern #f Lf 
                    (cons Lj^ ac))))))]
         [(not Lf) 
          (let ([Lj^ (unique-label)] [Lf^ (unique-label)])
            (Pred test #f Lf^
              (Pred conseq Lt Lj^
                (cons Lf^
                  (Pred altern Lt #f 
                    (cons Lj^ ac))))))]
         [else 
          (let ([Lf^ (unique-label)])
            (Pred test #f Lf^
              (Pred conseq Lt Lf
                (cons Lf^
                  (Pred altern Lt Lf ac)))))])] 
      [(seq e0 e1)
       (Effect e0 (Pred e1 Lt Lf ac))]
      [(new-frame)
       (NonTail x (do-simple-test eax Lt Lf ac))]
      [else (error 'Pred "invalid expression ~s" x)]))
  (define (idx->frame-loc i)
    (mem (fx* i (fx- 0 wordsize)) fpr))
  (define (Effect x ac)
   (record-case x
     [(constant) ac]
     [(primcall op rand*)
      (do-effect-prim op rand* ac)]
     [(conditional test conseq altern)
      (let* ([Ljoin (unique-label)]
             [ac (cons Ljoin ac)]
             [altern-ac (Effect altern ac)])
        (cond
          [(eq? altern-ac ac) ;; altern is nop
           (let* ([conseq-ac (Effect conseq ac)])
             (cond
               [(eq? conseq-ac ac) ;; conseq is nop too!
                (Effect test ac)]
               [else  ; "when" pattern
                (Pred test #f Ljoin conseq-ac)]))]
          [else 
           (let* ([Lf (unique-label)]
                  [nac (list* (jmp Ljoin) Lf altern-ac)]
                  [conseq-ac (Effect conseq nac)])
             (cond
               [(eq? conseq-ac nac) ;; "unless" pattern"
                (Pred test Ljoin #f altern-ac)]
               [else
                (Pred test #f Lf conseq-ac)]))]))]
     [(interrupt-call test handler)
      (let ([Ljoin (unique-label)]
            [Lint (unique-label)])
        (let ([handler 
               (Effect handler 
                 (list* (jmp Ljoin) '()))])
          (add-handler! (cons Lint handler))
          (Pred test Lint #f 
             (cons Ljoin ac))))]
     [(seq e0 e1)
      (Effect e0 (Effect e1 ac))]
     [(fix lhs* rhs* body)
      (do-fix lhs* rhs* (Effect body ac))]
     [(assign loc val)
      (record-case loc
        [(frame-var i)
         (record-case val
           [(constant c) 
            (cons (movl (constant-val c) (idx->frame-loc i)) ac)]
           [else
            (NonTail val
              (cons (movl eax (idx->frame-loc i)) ac))])]
        [else (error who "invalid assign loc ~s" loc)])]
     [(eval-cp check body)
      (cond
        [check
         (NonTail body
           (list* 
             (movl eax cpr)
             (andl (int closure-mask) eax)
             (cmpl (int closure-tag) eax)
             (jne (label SL_nonprocedure))
             ac))]
        [(primref? body)
         (list* (movl (primref-loc (primref-name body)) cpr) ac)]
        [else
         (NonTail body (list* (movl eax cpr) ac))])]
     [(save-cp loc)
      (record-case loc
        [(frame-var i)
         (cons (movl cpr (idx->frame-loc i)) ac)]
        [else (error who "invalid cpr loc ~s" x)])]
     [(new-frame) (NonTail x ac)]
     [(frame-var) ac]
     [else (error 'Effect "invalid expression ~s" x)]))
  (define (Tail x ac) 
    (record-case x
     [(return x) 
      (NonTail x (cons (ret) ac))]
     [(conditional test conseq altern)
      (let ([L (unique-label)])
        (Pred test #f L
          (Tail conseq 
            (cons L (Tail altern ac)))))]
     [(seq e0 e1)
      (Effect e0 (Tail e1 ac))]
     [(fix lhs* rhs* body)
      (do-fix lhs* rhs* (Tail body ac))]
     [(new-frame idx size body)
      (Tail body ac)]
     [(tailcall-cp call-convention direct-label argc)
      (case call-convention
        [(normal) 
         (list*
           (movl (int (argc-convention argc)) eax)
           (tail-indirect-cpr-call)
           ac)]
        [(apply)  
         (list*
           (movl (int (argc-convention argc)) eax)
           (jmp (label SL_apply))
           ac)]
        [(direct) 
         (list* 
           (movl (int (argc-convention argc)) eax)
           (jmp (label direct-label)) 
           ac)]
        [else 
         (error who "invalid tail-call convention ~s"
                call-convention)])]
      [(call-cp)
       (handle-call-cp x ac Tail)]
      [else (error 'GenTail "invalid expression ~s" (unparse x))]))
  (define (handle-call-cp x ac k)
    (record-case x
      [(call-cp call-convention direct-label save-cp?
                rp-convention offset size mask)
       (let* ([L_multi (gensym "L_multi")]
              [ac
               (record-case rp-convention
                 [(clambda L cases F)
                  (record-case (car cases)
                    [(clambda-case info body)
                     (record-case info
                       [(case-info L args proper)
                        (when (or (fx= (length args) 1)
                                  (not proper))
                          (error who "BUG: unhandles single rv"))
                        (list*
                          (subl (int (frame-adjustment offset)) fpr)
                          (movl (int (argc-convention 1)) eax)
                          (jmp (label SL_invalid_args))
                          (label L_multi)
                          (if save-cp? (movl (mem wordsize fpr) cpr) '(nop))
                          (subl (int (frame-adjustment (fxadd1 offset))) fpr)
                          (cmpl (int (argc-convention (length args))) eax)
                          (jne (label SL_invalid_args))
                          (k body ac))])])]
                 [else 
                  (list* 
                    (if save-cp? (movl (mem 0 fpr) cpr) '(nop))
                    (subl (int (frame-adjustment offset)) fpr)
                    ac)])]
              [L_CALL (unique-label)])
         (case call-convention
           [(normal)
            (list* (addl (int (frame-adjustment offset)) fpr)
                   (movl (int (argc-convention size)) eax)
                   (jmp L_CALL)
                   ; NEW FRAME
                   `(byte-vector ,mask)
                   `(int ,(fx* offset wordsize))
                   `(current-frame-offset)
                   (rp-label rp-convention L_multi)
                   `(byte 0) ; padding for indirect calls only
                   `(byte 0) ; direct calls are ok
                   L_CALL
                   (indirect-cpr-call)
                   ac)]
           [(direct)
            (list* (addl (int (frame-adjustment offset)) fpr)
                   ;(movl (int (argc-convention size)) eax)
                   (jmp L_CALL)
                   ; NEW FRAME
                   `(byte-vector ,mask)
                   `(int ,(fx* offset wordsize))
                   `(current-frame-offset)
                   (rp-label rp-convention L_multi)
                   ;;; no padding for direct calls
                   L_CALL
                   (call (label direct-label))
                   ac)] 
           [(foreign) 
            (list* (addl (int (frame-adjustment offset)) fpr)
                   (movl (int (argc-convention size)) eax)
                   (movl '(foreign-label "ik_foreign_call") ebx)
                   (jmp L_CALL)
                   ; NEW FRAME
                   (byte-vector mask)
                   `(int ,(fx* offset wordsize))
                   `(current-frame-offset)
                   (rp-label rp-convention L_multi) ; should be 0, since C has 1 rv
                   '(byte 0)
                   '(byte 0)
                   '(byte 0)
                   L_CALL
                   (call ebx)
                   ac)]
           [else
            (error who "invalid convention ~s for call-cp"
                   call-convention)]))]))
  (define (handle-vararg fml-count ac)
    (define CONTINUE_LABEL (unique-label))
    (define DONE_LABEL (unique-label))
    (define CONS_LABEL (unique-label))
    (define LOOP_HEAD (unique-label))
    (define L_CALL (unique-label))
    (list* (cmpl (int (argc-convention (fxsub1 fml-count))) eax)
           (jg (label SL_invalid_args))
           (jl CONS_LABEL)
           (movl (int nil) ebx)
           (jmp DONE_LABEL)
           CONS_LABEL
           (movl (pcb-ref 'allocation-redline) ebx)
           (addl eax ebx)
           (addl eax ebx)
           (cmpl ebx apr)
           (jle LOOP_HEAD)
           ; overflow
           (addl eax esp) ; advance esp to cover args
           (pushl cpr)    ; push current cp
           (pushl eax)    ; push argc
           (negl eax)     ; make argc positive
           (addl (int (fx* 4 wordsize)) eax) ; add 4 words to adjust frame size
           (pushl eax)    ; push frame size
           (addl eax eax) ; double the number of args
           (movl eax (mem (fx* -2 wordsize) fpr)) ; pass it as first arg
           (movl (int (argc-convention 1)) eax) ; setup argc
           (movl (primref-loc 'do-vararg-overflow) cpr) ; load handler
           (jmp L_CALL)   ; go to overflow handler
           ; NEW FRAME
           '(int 0)        ; if the framesize=0, then the framesize is dynamic
           '(current-frame-offset)
           '(int 0)        ; multiarg rp
           (byte 0)
           (byte 0)
           L_CALL
           (indirect-cpr-call)
           (popl eax)     ; pop framesize and drop it
           (popl eax)     ; reload argc
           (popl cpr)     ; reload cp
           (subl eax fpr) ; readjust fp
           LOOP_HEAD
           (movl (int nil) ebx)
           CONTINUE_LABEL
           (movl ebx (mem disp-cdr apr))
           (movl (mem fpr eax) ebx)
           (movl ebx (mem disp-car apr))
           (movl apr ebx)
           (addl (int pair-tag) ebx)
           (addl (int pair-size) apr)
           (addl (int (fxsll 1 fx-shift)) eax)
           (cmpl (int (fx- 0 (fxsll fml-count fx-shift))) eax)
           (jle CONTINUE_LABEL)
           DONE_LABEL
           (movl ebx (mem (fx- 0 (fxsll fml-count fx-shift)) fpr))
           ac))
  (define (Entry check? x ac)
    (record-case x
      [(clambda-case info body)
       (record-case info
         [(case-info L fml* proper)
          (let ([ac
                 (cons (label L) (Tail body ac))])
            (cond
              [(and proper check?)
               (list* (cmpl (int (argc-convention (length fml*))) eax)
                      (jne (label SL_invalid_args))
                      ac)]
              [proper ac]
              [else
               (handle-vararg (length fml*) ac)]))])]))
  (define make-dispatcher
    (lambda (j? L L* x x* ac)
      (cond
        [(null? L*) (if j? (cons (jmp (label L)) ac) ac)]
        [else
         (record-case x
           [(clambda-case info _)
            (record-case info
              [(case-info _ fml* proper)
               (cond
                 [proper
                  (list* (cmpl (int (argc-convention (length fml*))) eax)
                         (je (label L))
                         (make-dispatcher #t 
                           (car L*) (cdr L*) (car x*) (cdr x*) ac))]
                 [else
                  (list* (cmpl (int (argc-convention (fxsub1 (length fml*)))) eax)
                         (jle (label L))
                         (make-dispatcher #t
                           (car L*) (cdr L*) (car x*) (cdr x*) ac))])])])])))
  (define (handle-cases x x* ac)
    (let ([L* (map (lambda (_) (gensym)) x*)]
          [L (gensym)])
      (make-dispatcher #f L L* x x*
        (let f ([x x] [x* x*] [L L] [L* L*])
          (cond
            [(null? x*) 
             (cons (label L) (Entry 'check x ac))]
            [else
             (cons (label L)
               (Entry #f x
                 (f (car x*) (cdr x*) (car L*) (cdr L*))))])))))
  (define (CodeExpr x)
    (record-case x
      [(clambda L cases free)
       (set! handlers (list '(nop)))
       (list*
         (length free)
         (label L)
         (handle-cases (car cases) (cdr cases) handlers))]))
  (record-case x
    [(codes ls body)
     (let ([body
            (begin
              (set! handlers (list '(nop)))
              (Tail body handlers))])
       (cons (list* 0
                    (label (gensym))
                    body)
             (map CodeExpr ls)))]))

(begin ;;; ASSEMBLY HELPERS
  (define SL_fx+_type (gensym "SL_fx+_type"))
  (define SL_fx+_types (gensym "SL_fx+_types"))
  (define SL_fx+_overflow (gensym "SL_fx+_overflow"))
  (define SL_fxadd1_error (gensym "SL_fxadd1_error"))
  (define SL_fxsub1_error (gensym "SL_fxsub1_error"))
  (define SL_nonprocedure (gensym "SL_nonprocedure"))
  (define SL_top_level_value_error (gensym "SL_top_level_value_error"))
  (define SL_car_error (gensym "SL_car_error"))
  (define SL_cdr_error (gensym "SL_cdr_error"))
  (define SL_cadr_error (gensym "SL_cadr_error"))
  (define SL_invalid_args (gensym "SL_invalid_args"))
  (define SL_foreign_call (gensym "SL_foreign_call"))
  (define SL_continuation_code (gensym "SL_continuation_code"))
  (define SL_multiple_values_error_rp (gensym "SL_multiple_values_error_rp"))
  (define SL_multiple_values_ignore_rp (gensym "SL_multiple_ignore_error_rp"))
  (define SL_underflow_multiple_values (gensym "SL_underflow_multiple_values"))
  (define SL_underflow_handler (gensym "SL_underflow_handler"))
  (define SL_scheme_exit (gensym "SL_scheme_exit"))
  (define SL_apply (gensym "SL_apply"))
  (define SL_values (gensym "SL_values"))
  (define SL_call_with_values (gensym "SL_call_with_values"))
  (module ()
    (list*->code* (lambda (x) #f)
      (list
        (list 0
              (label SL_fxadd1_error)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'fxadd1-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))
        (list 0
              (label SL_fx+_type)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'fx+-type-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call)) 
        (list 0
              (label SL_fx+_overflow)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl ebx (mem (fx- wordsize wordsize) fpr))
              (movl (primref-loc 'fx+-overflow-error) cpr)
              (movl (int (argc-convention 2)) eax)
              (tail-indirect-cpr-call)) 
        (list 0
              (label SL_fx+_types)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl ebx (mem (fx- wordsize wordsize) fpr))
              (movl (primref-loc 'fx+-types-error) cpr)
              (movl (int (argc-convention 2)) eax)
              (tail-indirect-cpr-call))
        (list 0
              (label SL_fxsub1_error)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'fxsub1-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call)) 
        (list 0
              (label SL_car_error)
              (movl ebx (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'car-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))
        
        (list 0
              (label SL_cdr_error)
              (movl ebx (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'cdr-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))
        (list 0
              (label SL_cadr_error)
              (movl ebx (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'cadr-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))
        (list 0
              (label SL_top_level_value_error)
              (movl ebx (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'top-level-value-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))
        
        (let ([L_cwv_done (gensym)]
              [L_cwv_loop (gensym)]
              [L_cwv_multi_rp (gensym)]
              [L_cwv_call (gensym)])
          (list 
              0 ; no free vars
              (label SL_call_with_values)
              (cmpl (int (argc-convention 2)) eax)
              (jne (label SL_invalid_args))
              (movl (mem (fx- 0 wordsize) fpr) ebx) ; producer
              (movl ebx cpr)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label SL_nonprocedure))
              (movl (int (argc-convention 0)) eax)
              (subl (int (fx* wordsize 2)) fpr)
              (jmp (label L_cwv_call))
              ; MV NEW FRAME
              (byte-vector '#(#b110))
              `(int ,(fx* wordsize 3))
               '(current-frame-offset)
              (label-address L_cwv_multi_rp)
              (byte 0)
              (byte 0)
              (label L_cwv_call)
              (indirect-cpr-call)
              ;;; one value returned
              (addl (int (fx* wordsize 2)) fpr)
              (movl (mem (fx* -2 wordsize) fpr) ebx) ; consumer
              (movl ebx cpr)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (int (argc-convention 1)) eax)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label SL_nonprocedure))
              (tail-indirect-cpr-call)
              ;;; multiple values returned
              (label L_cwv_multi_rp)
              ; because values does not pop the return point
              ; we have to adjust fp one more word here
              (addl (int (fx* wordsize 3)) fpr) 
              (movl (mem (fx* -2 wordsize) fpr) cpr) ; consumer
              (cmpl (int (argc-convention 0)) eax)
              (je (label L_cwv_done))
              (movl (int (fx* -4 wordsize)) ebx)
              (addl fpr ebx)  ; ebx points to first value
              (movl ebx ecx)
              (addl eax ecx)  ; ecx points to the last value
              (label L_cwv_loop)
              (movl (mem 0 ebx) edx)
              (movl edx (mem (fx* 3 wordsize) ebx))
              (subl (int wordsize) ebx)
              (cmpl ecx ebx)
              (jge (label L_cwv_loop))
              (label L_cwv_done)
              (movl cpr ebx)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label SL_nonprocedure))
              (tail-indirect-cpr-call)))
        
        (let ([L_values_one_value (gensym)]
              [L_values_many_values (gensym)])
          (list 0 ; no freevars
              (label SL_values)
              (cmpl (int (argc-convention 1)) eax)
              (je (label L_values_one_value))
              (label L_values_many_values)
              (movl (mem 0 fpr) ebx) ; return point
              (jmp (mem disp-multivalue-rp ebx))     ; go
              (label L_values_one_value)
              (movl (mem (fx- 0 wordsize) fpr) eax)
              (ret)))
        
        (let ([L_apply_done (gensym)]
              [L_apply_loop (gensym)])
          (list 0 
              (label SL_apply)
              (movl (mem fpr eax) ebx)
              (cmpl (int nil) ebx)
              (je (label L_apply_done))
              (label L_apply_loop)
              (movl (mem (fx- disp-car pair-tag) ebx) ecx)
              (movl (mem (fx- disp-cdr pair-tag) ebx) ebx)
              (movl ecx (mem fpr eax))
              (subl (int wordsize) eax)
              (cmpl (int nil) ebx)
              (jne (label L_apply_loop))
              (label L_apply_done)
              (addl (int wordsize) eax)
              (tail-indirect-cpr-call)))
        
        (list 0
              (label SL_nonprocedure)
              (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
              (movl (primref-loc '$apply-nonprocedure-error-handler) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))
        
        (list 0
              (label SL_multiple_values_error_rp)
              (movl (primref-loc '$multiple-values-error) cpr)
              (tail-indirect-cpr-call))
        
        (list 0
              (label SL_multiple_values_ignore_rp)
              (ret))
        
        (list 0
              (label SL_invalid_args)
              ;;;
              (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
              (negl eax)
              (movl eax (mem (fx- 0 (fx* 2 wordsize)) fpr))
              (movl (primref-loc '$incorrect-args-error-handler) cpr)
              (movl (int (argc-convention 2)) eax)
              (tail-indirect-cpr-call))
        
        (let ([Lset (gensym)] [Lloop (gensym)])
          (list 0
              (label SL_foreign_call)
              (movl fpr (pcb-ref 'frame-pointer))
              (movl apr (pcb-ref 'allocation-pointer))
              (movl fpr ebx)
              (movl (pcb-ref 'system-stack) esp)
              (pushl pcr)
              (cmpl (int 0) eax)
              (je (label Lset))
              (label Lloop)
              (movl (mem ebx eax) ecx)
              (pushl ecx)
              (addl (int 4) eax)
              (cmpl (int 0) eax)
              (jne (label Lloop))
              (label Lset)
              ; FOREIGN NEW FRAME
              (call cpr)
              (movl (pcb-ref 'frame-pointer) fpr)
              (movl (pcb-ref 'allocation-pointer) apr)
              (ret)))
        
        (let ([L_cont_zero_args      (gensym)] 
              [L_cont_mult_args      (gensym)]
              [L_cont_one_arg        (gensym)]
              [L_cont_mult_move_args (gensym)]
              [L_cont_mult_copy_loop (gensym)])
          (list  1 ; freevars
              (label SL_continuation_code)
              (movl (mem (fx- disp-closure-data closure-tag) cpr) ebx) ; captured-k
              (movl ebx (pcb-ref 'next-continuation)) ; set
              (movl (pcb-ref 'frame-base) ebx)
              (cmpl (int (argc-convention 1)) eax)
              (jg (label L_cont_zero_args))
              (jl (label L_cont_mult_args))
              (label L_cont_one_arg)
              (movl (mem (fx- 0 wordsize) fpr) eax)
              (movl ebx fpr)
              (subl (int wordsize) fpr)
              (ret)
              (label L_cont_zero_args)
              (subl (int wordsize) ebx)
              (movl ebx fpr)
              (movl (mem 0 ebx) ebx) ; return point
              (jmp (mem disp-multivalue-rp ebx))  ; go
              (label L_cont_mult_args)
              (subl (int wordsize) ebx)
              (cmpl ebx fpr)
              (jne (label L_cont_mult_move_args))
              (movl (mem 0 ebx) ebx)
              (jmp (mem disp-multivalue-rp ebx))
              (label L_cont_mult_move_args)
              ; move args from fpr to ebx
              (movl (int 0) ecx)
              (label L_cont_mult_copy_loop)
              (subl (int wordsize) ecx)
              (movl (mem fpr ecx) edx)
              (movl edx (mem ebx ecx))
              (cmpl ecx eax)
              (jne (label L_cont_mult_copy_loop))
              (movl ebx fpr)
              (movl (mem 0 ebx) ebx)
              (jmp (mem disp-multivalue-rp ebx))
              ))
        ))))

(define (compile-core-expr->code p)
  (let* ([p (recordize p)]
         [p (optimize-direct-calls p)]
         [p (optimize-letrec p)]
         [p (uncover-assigned/referenced p)]
         [p (copy-propagate p)]
         [p (rewrite-assignments p)]
         [p (optimize-for-direct-jumps p)]
         [p (convert-closures p)]
         [p (optimize-closures/lift-codes p)] 
         [p (introduce-primcalls p)]
         [p (simplify-operands p)]
         [p (insert-stack-overflow-checks p)]
         [p (insert-allocation-checks p)]
         [p (insert-engine-checks p)]
         [p (remove-local-variables p)]
         [p (optimize-ap-check p)])
    (let ([ls* (generate-code p)])
      (when (assembler-output)
        (parameterize ([gensym-prefix "L"]
                       [print-gensym #f])
          (for-each 
            (lambda (ls)
              (newline)
              (for-each (lambda (x) (printf "    ~s\n" x)) ls))
            ls*)))
      (let ([code* 
             (list*->code* 
               (lambda (x)
                 (if (closure? x)
                     (if (null? (closure-free* x))
                         (code-loc-label (closure-code x))
                         (error 'compile "BUG: non-thunk escaped: ~s" x))
                     #f))
               ls*)])
        (car code*)))))


(define compile-core-expr-to-port
  (lambda (expr port)
    (fasl-write (compile-core-expr->code expr) port)))

(define (compile-core-expr x)
  (let ([code (compile-core-expr->code x)])
    ($code->closure code)))

(primitive-set! 'compile-core-expr-to-port compile-core-expr-to-port)

(primitive-set! 'assembler-output (make-parameter #f))

(primitive-set! 'eval-core
  (lambda (x) ((compile-core-expr x))))

(primitive-set! 'current-primitive-locations
  (make-parameter '()))

))

#!eof junk


(define (compile-expr->code x)
  (compile-core-expr->code
    (parameterize ([assembler-output #f])
      (expand x))))

(primitive-set! 'compile
  (lambda (x)
    (let ([code 
           (if (code? x)
               x
               (compile-expr->code x))])
      (let ([proc ($code->closure code)])
        (proc)))))

(define compile-file
  (lambda (input-file output-file . rest)
    (let ([ip (open-input-file input-file)]
          [op (apply open-output-file output-file rest)])
      (let f ()
        (let ([x (read ip)])
          (unless (eof-object? x)
            (fasl-write (compile-expr->code x) op)
            (f))))
      (close-input-port ip)
      (close-output-port op))))
(primitive-set! 'compile-file compile-file)

;(include "libaltcogen.ss")
(define alt-cogen
  (lambda args
    (error 'alt-cogen "disabled for now")))

(define alt-compile-file
  (lambda (input-file output-file . rest)
    (let ([ip (open-input-file input-file)]
          [op (apply open-output-file output-file rest)])
      (let f ()
        (let ([x (read ip)])
          (unless (eof-object? x)
            (fasl-write (alt-compile-expr x) op)
            (f))))
      (close-input-port ip)
      (close-output-port op))))


(primitive-set! 'alt-compile-file alt-compile-file)

(primitive-set! 'alt-compile
  (lambda (x)
    (let ([code 
           (if (code? x)
               x
               (alt-compile-expr x))])
      (let ([proc ($code->closure code)])
        (proc)))))

(define (alt-compile-expr expr)
  (let* ([p (parameterize ([assembler-output #f])
              (expand expr))]
         [p (recordize p)]
         [p (parameterize ([open-mvcalls #f])
              (optimize-direct-calls p))]
         [p (optimize-letrec p)]
         [p (uncover-assigned/referenced p)]
         [p (copy-propagate p)]
         [p (rewrite-assignments p)]
         [p (optimize-for-direct-jumps p)]
         [p (convert-closures p)]
         [p (optimize-closures/lift-codes p)])
    (let ([ls* (alt-cogen p)])
      (when (assembler-output)
        (parameterize ([gensym-prefix "L"]
                       [print-gensym #f])
          (for-each 
            (lambda (ls)
              (newline)
              (for-each (lambda (x) (printf "    ~s\n" x)) ls))
            ls*)))
      (let ([code* 
             (list*->code* 
               (lambda (x)
                 (if (closure? x)
                     (if (null? (closure-free* x))
                         (code-loc-label (closure-code x))
                         (error 'compile "BUG: non-thunk escaped: ~s" x))
                     #f))
               ls*)])
        (car code*)))))
