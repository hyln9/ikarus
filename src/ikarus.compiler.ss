
(library (ikarus compiler)
  (export compile-core-expr-to-port 
          assembler-output
          current-primitive-locations eval-core)
  (import 
    (ikarus system $fx)
    (ikarus system $pairs)
    (only (ikarus system $codes) $code->closure)
    (only (ikarus system $records) $record-ref $record/rtd?)
    (except (ikarus)
        compile-core-expr-to-port assembler-output
        current-primitive-locations eval-core)
    (ikarus intel-assembler)
    (ikarus fasl write))


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
    ;;; bytevectors
    [bytevector?        1   pred]
    [$make-bytevector   1   value]
    [$bytevector-length 1   value]
    [$bytevector-u8-ref    2   value]
    [$bytevector-s8-ref    2   value]
    [$bytevector-set!   3   effect]
    ;;; bignums
    [$make-bignum       2   value]
    [$bignum-positive?  1   pred]
    [$bignum-size       1   value]
    [$bignum-byte-ref   2   value]
    [$bignum-byte-set!  3   effect]
    ;;; ratnums
    [$make-ratnum       2   value]
    [ratnum?            1   pred]
    [$ratnum-n          1   value]
    [$ratnum-d          1   value]
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
    [base-rtd           0   value]
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
    [code?              1    pred]
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
(define-record rec*bind (lhs* rhs* body))
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
                (let ([expr (make-recbind nlhs* (map E rhs*) (E body))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(letrec*)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [rhs* (map cadr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (let ([expr (make-rec*bind nlhs* (map E rhs*) (E body))])
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
      [(rec*bind lhs* rhs* body) 
       `(letrec* ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
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
      [(rec*bind lhs* rhs* body)
       (make-rec*bind lhs* (map Expr rhs*) (Expr body))]
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


(define simple-primitives 
  ;;; primitives that are side-effect-free
  ;;; FIXME: surely something must go here, no?
  '())

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
  (define (do-recbind lhs* rhs* body ref comp letrec?)
    (let ([h (make-hash-table)]
          [vref (make-vector (length lhs*) #f)]
          [vcomp (make-vector (length lhs*) #f)])
      (let* ([ref (extend-hash lhs* h ref)]
             [body (E body ref comp)])
        (let ([rhs* (do-rhs* 0 lhs* rhs* ref comp vref vcomp)])
          (let-values ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
                        (partition-rhs* 0 lhs* rhs* vref vcomp)])
           (let ([v* (map (lambda (x) (make-primcall 'void '())) clhs*)])
             (make-bind slhs* srhs*
               (make-bind clhs* v*
                 (make-fix llhs* lrhs*
                   (if letrec?
                       (let ([t* (map (lambda (x) (unique-var 'tmp)) clhs*)])
                         (make-bind t* crhs*
                           (build-assign* clhs* t* body)))
                       (build-assign* clhs* crhs* body)))))))))))
  (define (build-assign* lhs* rhs* body)
    (cond
      [(null? lhs*) body]
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
           (do-recbind lhs* rhs* body ref comp #t))]
      [(rec*bind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp #f))] 
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
       (unless (memq rator simple-primitives)
         (comp))
       (make-primcall rator (E* rand* ref comp))]
      [(funcall rator rand*)
       (let ([rator (E rator ref comp)] [rand* (E* rand* ref comp)])
         (record-case rator
           [(primref op)
            (unless (memq op simple-primitives)
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
  555 void
  645 list
|#

;;; FIXME URGENT: should handle (+ x k), (- x k) where k is a fixnum
;;;               also fx+, fx-
(module (optimize-primcall)
  (define (optimize-primcall ctxt op rand*)
    (cond
      [(getprop op *cookie*) =>
       (lambda (proc)
         (proc ctxt op rand* 
               (lambda () 
                 (make-funcall (make-primref op) rand*))))]
      [else
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
      apply) 
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
            (make-jmpcall (sl-apply-label) 
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

  (define ratnum-tag    #x27)
  (define disp-ratnum-num  4)
  (define disp-ratnum-den  8)
  (define ratnum-size     16)

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
  
  ;(define symbol-mask 7)
  ;(define symbol-tag 2)
  ;(define disp-symbol-string          0)
  ;(define disp-symbol-unique-string   4)
  ;(define disp-symbol-value           8)
  ;(define disp-symbol-plist          12)
  ;(define disp-symbol-system-value   16)
  ;(define disp-symbol-function       20)
  ;(define disp-symbol-error-function 24)
  ;(define disp-symbol-unused         28)
  ;(define symbol-size                32)

  (define bytevector-mask 7)
  (define bytevector-tag 2)
  (define disp-bytevector-length 0)
  (define disp-bytevector-data 4)

  (define ptag-mask 7)
  (define symbol-ptag 5)
  (define symbol-record-tag #x5F)
  (define disp-symbol-record-string  4)
  (define disp-symbol-record-ustring 8)
  (define disp-symbol-record-value  12)
  (define disp-symbol-record-proc   16)
  (define disp-symbol-record-plist  20)
  (define symbol-record-size  24)
  
  (define record-tag  5)
  (define record-mask 7)

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
      [(base-rtd)           (mem 44 pcr)]
      [else (error 'pcb-ref "invalid arg ~s" x)])))


(define (primref->symbol op)
  (unless (symbol? op) (error 'primref->symbol "not a symbol ~s" op))
  (cond
    [((current-primitive-locations) op) =>
     (lambda (x)
       (unless (symbol? x) 
         (error 'primitive-location 
            "~s is not a valid location for ~s" x op))
       x)]
    [else
     (error 'compile "cannot find location of primitive ~s" op)]))

(define (primref-loc op)
  (mem (fx- disp-symbol-record-value record-tag) 
       (obj (primref->symbol op))))



(module ;assembly-labels
  (refresh-cached-labels!
   sl-apply-label sl-fx+-type-label sl-fx+-types-label
   sl-continuation-code-label sl-invalid-args-label
   sl-mv-ignore-rp-label sl-mv-error-rp-label sl-values-label 
   sl-cwv-label sl-top-level-value-error-label sl-cadr-error-label
   sl-cdr-error-label sl-car-error-label sl-nonprocedure-error-label
   sl-fxsub1-error-label sl-fxadd1-error-label sl-fx+-overflow-label)
  (define-syntax define-cached
    (lambda (x)
      (syntax-case x ()
        [(_ refresh [(name*) b* b** ...] ...)
         (with-syntax ([(v* ...) (generate-temporaries #'(name* ...))])
           #'(begin
               (define v* #f) ...
               (define (name*)
                 (or v* (error 'name* "uninitialized label"))) ...
               (define (refresh)
                 (define-syntax name* 
                   (lambda (stx) (syntax-error stx "cannot use label before it is defined")))
                 ...
                 (let* ([name* (let ([label (let () b* b** ...)])
                                 (set! v* label)
                                 (lambda () label))] ...)
                   (void)))))])))
  (define-cached refresh-cached-labels!
   [(sl-apply-label)
    (let ([SL_apply (gensym "SL_apply")]
          [L_apply_done (gensym)]
          [L_apply_loop (gensym)])
      (assemble-sources (lambda (x) #f)
        (list
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
              (tail-indirect-cpr-call))))
      SL_apply)]
   [(sl-fx+-type-label)
    (define SL_fx+_type (gensym "SL_fx+_type"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
              (label SL_fx+_type)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'fx+-type-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))))
    SL_fx+_type]
   [(sl-fx+-types-label)
    (define SL_fx+_types (gensym "SL_fx+_types"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fx+_types)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl ebx (mem (fx- wordsize wordsize) fpr))
          (movl (primref-loc 'fx+-types-error) cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
    SL_fx+_types]
   [(sl-continuation-code-label)
    (define SL_continuation_code (gensym "SL_continuation_code"))
    (assemble-sources (lambda (x) #f)
      (list
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
              (jmp (mem disp-multivalue-rp ebx))))))
    SL_continuation_code]
   [(sl-invalid-args-label)
    (define SL_invalid_args (gensym "SL_invalid_args"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_invalid_args)
          ;;;
          (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
          (negl eax)
          (movl eax (mem (fx- 0 (fx* 2 wordsize)) fpr))
          (movl (primref-loc '$incorrect-args-error-handler) cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
    SL_invalid_args]
   [(sl-mv-ignore-rp-label)
    (define SL_multiple_values_ignore_rp (gensym "SL_multiple_ignore_error_rp"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
           (label SL_multiple_values_ignore_rp)
           (ret))))
    SL_multiple_values_ignore_rp]
   [(sl-mv-error-rp-label)
    (define SL_multiple_values_error_rp (gensym "SL_multiple_values_error_rp"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_multiple_values_error_rp)
          (movl (primref-loc '$multiple-values-error) cpr)
          (tail-indirect-cpr-call))))
    SL_multiple_values_error_rp]
   [(sl-values-label)
    (define SL_values (gensym "SL_values"))
    (assemble-sources (lambda (x) #f)
      (list
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
              (ret)))))
    SL_values]
   [(sl-nonprocedure-error-label)
    (define SL_nonprocedure (gensym "SL_nonprocedure"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_nonprocedure)
          (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
          (movl (primref-loc '$apply-nonprocedure-error-handler) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_nonprocedure]
   [(sl-cwv-label)
    (define SL_call_with_values (gensym "SL_call_with_values"))
    (assemble-sources (lambda (x) #f)
      (list
        (let ([L_cwv_done (gensym)]
              [L_cwv_loop (gensym)]
              [L_cwv_multi_rp (gensym)]
              [L_cwv_call (gensym)])
          (list 
              0 ; no free vars
              (label SL_call_with_values)
              (cmpl (int (argc-convention 2)) eax)
              (jne (label (sl-invalid-args-label)))
              (movl (mem (fx- 0 wordsize) fpr) ebx) ; producer
              (movl ebx cpr)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label (sl-nonprocedure-error-label)))
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
              (jne (label (sl-nonprocedure-error-label)))
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
              (jne (label (sl-nonprocedure-error-label)))
              (tail-indirect-cpr-call)))))
    SL_call_with_values]
   [(sl-top-level-value-error-label)
    (define SL_top_level_value_error (gensym "SL_top_level_value_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_top_level_value_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'top-level-value-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_top_level_value_error]
   [(sl-cadr-error-label)
    (define SL_cadr_error (gensym "SL_cadr_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_cadr_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'cadr-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_cadr_error]
   [(sl-cdr-error-label)
    (define SL_cdr_error (gensym "SL_cdr_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_cdr_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'cdr-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_cdr_error]
   [(sl-car-error-label)
    (define SL_car_error (gensym "SL_car_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_car_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'car-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_car_error]
   [(sl-fxsub1-error-label)
    (define SL_fxsub1_error (gensym "SL_fxsub1_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fxsub1_error)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'fxsub1-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_fxsub1_error]
   [(sl-fxadd1-error-label)
    (define SL_fxadd1_error (gensym "SL_fxadd1_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fxadd1_error)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'fxadd1-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_fxadd1_error]
   [(sl-fx+-overflow-label)
    (define SL_fx+_overflow (gensym "SL_fx+_overflow"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fx+_overflow)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl ebx (mem (fx- wordsize wordsize) fpr))
          (movl (primref-loc 'fx+-overflow-error) cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
    SL_fx+_overflow]))

(define (alt-compile-core-expr->code p)
  (let* ([p (recordize p)]
         [p (optimize-direct-calls p)]
         [p (optimize-letrec p)]
         [p (uncover-assigned/referenced p)]
         [p (copy-propagate p)]
         [p (rewrite-assignments p)]
         [p (optimize-for-direct-jumps p)]
         [p (convert-closures p)]
         [p (optimize-closures/lift-codes p)])
    p ))

(define (compile-core-expr->code p)
  (let* ([p (recordize p)]
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
             (assemble-sources 
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

(define assembler-output (make-parameter #f))


(define eval-core
  (lambda (x) ((compile-core-expr x))))

(include "libaltcogen.ss")

(define current-primitive-locations
  (let ([plocs (lambda (x) #f)])
    (case-lambda
      [() plocs]
      [(p)
       (if (procedure? p)
           (begin 
             (set! plocs p) 
             (refresh-cached-labels!))
           (error 'current-primitive-locations "~s is not a procedure" p))])))

)


