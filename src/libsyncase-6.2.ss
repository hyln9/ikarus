
;;; 6.2: initial syncase implementation
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
;;;        | (#primitive| <primname>)
;;;        | (<CS> <CS> ...)
;;; <FML> ::= ()
;;;         | <gensym>
;;;         | (<gensym> . <FML>)
;;; <prim> ::= void | memv | top-level-value | set-top-level-value! 
;;;          | primitive-set! | foreign-call | $apply

(let ([*stx* (make-record-type "*stx*" '(e marks ribcage))]
      [*rib* (make-record-type "*rib*" '(sym* marks* lab*))]
      [*top* (make-record-type "*top*" '())])

  (define stx? (record-predicate *stx*))
  (define make-stx (record-constructor *stx*))
  (define stx-e (record-field-accessor *stx* 'e))
  (define stx-marks (record-field-accessor *stx* 'marks))
  (define stx-ribcage (record-field-accessor *stx* 'ribcage))
  (define make-rib (record-constructor *rib*))
  (define rib-sym* (record-field-accessor *rib* 'sym*))
  (define rib-marks* (record-field-accessor *rib* 'marks*))
  (define rib-lab* (record-field-accessor *rib* 'lab*))
  (define *top-ribcage* ((record-constructor *top*)))
  (define (top? x) (eq? x *top-ribcage*))
  (define *syncase-macro* (gensym "*syncase-macro*"))

  (define (build-data x)            `(quote ,x))
  (define (build-global-ref x)      `(top-level-value ',x))
  (define (build-lexical-ref x)     x)
  (define (build-app a d)           `(,a . ,d))
  (define (build-lambda fml* body)
    (cond
      [(and (pair? body) (eq? (car body) 'begin))
       `(lambda ,fml* . ,(cdr body))]
      [else 
       `(lambda ,fml* ,body)]))
  (define (build-begin body*)       `(begin . ,body*))


  (define (build-void)              `(void))
  (define (build-if e0 e1 e2)       `(if ,e0 ,e1 ,e2))
  (define (build-foreign-call e e*) `(foreign-call ,e ,e*))



  (define (id? x) 
    (and (stx? x)
         (symbol? (stx-e x))))

  (define (stx->datum x) ;;;; use strip
    (cond
      [(stx? x) (stx-e x)]
      [else     x]))

  (define (stx-pair? x)
    (and (stx? x)
         (pair? (stx-e x))))

  (define (strip x)
    (cond
      [(stx? x) (stx-e x)]
      [else x]))

  (define label? string?)

  (define (eqmarks? m1* m2*)
    (cond
      [(null? m1*) (null? m2*)]
      [(memq (car m1*) m2*) (eqmarks? (cdr m1*) (remq (car m1*) m2*))]
      [else #f]))

  (define (rib-lookup sym m* sym* m** lab*)
    (and (pair? sym*)
        (if (and (eq? sym (car sym*))
                 (eqmarks? m* (car m**)))
            (car lab*)
            (rib-lookup sym m* (cdr sym*) (cdr m**) (cdr lab*)))))

  (define (ribcage-lookup sym m* rc)
    (cond
      [(pair? rc)
       (let ([r (car rc)])
         (cond
           [(eq? r 'shift) 
            (ribcage-lookup sym (cdr m*) (cdr rc))]
           [else
            (or (rib-lookup sym m* (rib-sym* r) (rib-marks* r) (rib-lab* r))
                (ribcage-lookup sym m* (cdr rc)))]))]
      [(top? rc) #f]
      [else (error "BUG1")]))
  
  (define (resolve x)
    (unless (id? x) (error "BUG2"))
    (let ([sym (stx-e x)]
          [m* (stx-marks x)]
          [rc (stx-ribcage x)])
      (or (ribcage-lookup sym m* rc)    ; bound            -> label
          (getprop sym *syncase-macro*) ; top-level-macros -> pair
          sym                           ; global           -> symbol
          )))

  (define (remove-last ls)
    (let ([d (cdr ls)])
      (cond
        [(null? d) '()]
        [else (cons (car ls) (remove-last d))])))

  (define (unshift rc)
    (cond
      [(pair? rc)
       (if (eq? (car rc) 'shift)
           (cdr rc)
           (cons (car rc) (unshift (cdr rc))))]
      [else (error "BUG3: missing shift")]))

  (define (push-wrap m r x)
    (cond
      [(stx? x)
       (let ([xm (stx-marks x)])
         (cond
           [(and (pair? xm) (eq? (car xm) #f))
            (make-stx (stx-e x)
                      (append (remove-last m) (cdr xm))
                      (unshift (append r (stx-ribcage x))))]
           [else
            (make-stx (stx-e x)
                      (append m xm)
                      (append r (stx-ribcage x)))]))]
      [else (make-stx x m r)]))

  (define (push-subst sym* marks* lab* x)
    (cond
      [(stx? x)
       (make-stx (stx-e x)
                 (stx-marks x)
                 (cons (make-rib sym* marks* lab*) (stx-ribcage x)))]
      [else
       (make-stx x 
                 '()
                 (cons (make-rib sym* marks* lab*) '()))]))

  (define (push-antimark x)
    (cond
      [(stx? x)
       (make-stx (stx-e x)
                 (cons #f (stx-marks x))
                 (stx-ribcage x))]
      [else (make-stx x (cons #f '()) '())]))

  (define (push-mark m x)
    (cond
      [(stx? x)
       (let ([m* (stx-marks x)])
         (cond
           [(and (pair? m*) (eq? (car m*) #f))
            (make-stx (stx-e x) (cdr m*) (stx-ribcage x))]
           [else
            (make-stx (stx-e x) (cons m m*) (cons 'shift (stx-ribcage x)))]))]
      [else
       (make-stx x (list m) '(shift))]))

  (define (push-rib rib x)
    (cond
      [(stx? x) 
       (make-stx (stx-e x) (stx-marks x) (cons rib (stx-ribcage x)))]
      [else (make-stx x '() (list rib))]))

  (define (expose-stx x)
    (let ([e (stx-e x)])
      (cond
        [(pair? e)
         (let ([m (stx-marks x)]
               [r (stx-ribcage x)])
           (cons
             (push-wrap m r (car e))
             (push-wrap m r (cdr e))))]
        [(vector? e)
         (let ([m (stx-marks x)]
               [r (stx-ribcage x)])
           (list->vector
             (map (lambda (x) (push-wrap m r x)) 
                  (vector->list e))))]
        [(null? e) e]
        [else x])))

  (define (expose x)
    (cond
      [(stx? x) (expose-stx x)]
      [else x]))

  (define (expose-ls ox)
    (let loop ([x (expose ox)])
      (cond
        [(pair? x) (cons (car x) (loop (expose (cdr x))))]
        [(null? x) '()]
        [else (error 'expose-ls "BUG: not a list: ~s" x)])))

  (define (expose* x)
    (cond
      [(id? x) x]
      [(stx? x) (expose* (expose x))]
      [(pair? x) (cons (expose* (car x)) (expose* (cdr x)))]
      [(vector? x) 
       (list->vector (map expose* (vector->list x)))]
      [else x]))

  (define (lookup lab r)
    (define (lookup1 lab lab* g*)
      (cond
        [(null? lab*) #f]
        [(eq? lab (car lab*)) (car g*)]
        [else (lookup1 lab (cdr lab*) (cdr g*))]))
    (cond
      [(null? r) #f]
      [(eq? (car r) 'lexical-barrier)
       (let ([v (lookup lab (cdr r))])
         (cond
           [(not (symbol? v)) v]
           [else             #f]))]
      [else
       (or (lookup1 lab (caar r) (cdar r))
           (lookup lab (cdr r)))]))

  (define (genmark) (gensym "M"))
  (define (newsym x) 
    (gensym))
    ;(gensym (symbol->string x)))

  (define (apply-macro proc x r)
    (expand-ctx (push-mark (genmark) (proc (push-antimark x))) r))
      
  (define (identifier-macro? x r)
    (and (id? x)
         (let ([a (resolve x)])
           (or (and (label? a)
                    (let ([a (lookup a r)])
                      (and (procedure? a) a)))
               (and (pair? a)
                    (eq? (car a) '*user-macro*)
                    (cdr a))))))

  (define (macro-call? x r)
    (if (id? x)
        (identifier-macro? x r)
        (let ([x (expose x)])
          (and (pair? x)
               (identifier-macro? (car x) r)))))

  (define (core? x)
    (and (pair? x) (eq? (car x) '*core-macro*)))

  (define (apply-core-form a d ctx r)
    (unless (core? a) (syntax-error ctx))
    ((cdr a) a d ctx r))

  (define (E* d r ctx)
    (let ([d (expose-ls d)])
      (map (lambda (x) (E x r)) d)))

  (define (extend-core name proc)
    (putprop name *syncase-macro* (cons '*core-macro* proc)))

  (define (extend-user-macro name proc)
    (putprop name *syncase-macro* (cons '*user-macro* proc)))

  (define (E ctx r)
    (let ([x (expose ctx)])
      (cond
        [(macro-call? x r) =>
         (lambda (proc)
           (apply-macro proc ctx r))]
        [(pair? x)
         (let ([a (car x)] [d (cdr x)])
           (cond
             [(id? a)
              (let ([a (resolve a)])
                (cond
                  [(label? a)
                   (cond
                     [(lookup a r) =>
                      (lambda (g)
                        (cond
                          [(symbol? g)
                           (build-app (build-lexical-ref g) 
                                      (E* d r ctx))]
                          [(and (pair? g) (eq? (car g) 'pat))
                           (syntax-error ctx)]
                          [else (error 'expand "BUG4")]))]
                     [else (syntax-error ctx)])]
                  [(core? a)
                   (apply-core-form a d ctx r)]
                  [(symbol? a)
                   (build-app (build-global-ref a) 
                              (E* d r ctx))]
                  [else (syntax-error ctx)]))]
             [else
              (build-app 
                (E a r)
                (E* d r ctx))]))]
        [(id? x)
         (let ([a (resolve x)])
           (cond
             [(label? a)
              (cond
                [(lookup a r) =>
                 (lambda (g)
                   (cond
                     [(symbol? g) (build-lexical-ref g)]
                     [(and (pair? g) (eq? (car g) 'pat))
                      (syntax-error ctx)]
                     [else (error 'expand "BUG5")]))]
                [else (syntax-error ctx)])]
             [(core? a) (syntax-error ctx)]
             [(symbol? a)
              (build-global-ref a)]
             [else (syntax-error ctx)]))]
        [else (build-data (strip x))])))

  (define (core-expand x)
    (E (make-stx x '() *top-ribcage*) '()))

  (define (process-fml* bind* ctx)
    (define (assert-no-dups s m* s* m**)
      (unless (null? s*)
        (when (and (eq? s (car s*))
                   (eqmarks? m* (car m**)))
          (syntax-error ctx))
        (assert-no-dups s m* (cdr s*) (cdr m*))))
    (let loop ([bind* (expose bind*)])
      (cond
        [(null? bind*) (values '() '() '() '() '())]
        [(pair? bind*)
         (let ([b (car bind*)])
           (unless (id? b) (syntax-error ctx))
           (let-values ([(fml* s* m** g* lab*) 
                         (loop (expose (cdr bind*)))])
             (let ([s (stx-e b)] [m* (stx-marks b)])
               (assert-no-dups s m* s* m**)
               (let ([lab (string #\i)] [g (newsym s)])
                 (values (cons g fml*)
                         (cons s s*)
                         (cons m* m**)
                         (cons g g*)
                         (cons lab lab*))))))]
        [else (syntax-error ctx)])))

  (define (top-level-macro? x r sym)
    (let ([x (expose x)])
      (and (pair? x)
           (id? (car x))
           (let ([loc (resolve (car x))])
             (and (or (and (pair? loc) 
                           (eq? (car loc) '*core-macro*))
                      (symbol? loc))
                  (eq? (stx->datum (car x)) sym))))))

  (define (define? x r)
    (top-level-macro? x r 'define))

  (define (begin? x r)
    (top-level-macro? x r 'begin))

  (define (begin-e* x ctx)
    (let ([x (expose x)])
      (let loop ([x (expose (cdr x))])
        (cond
          [(null? x) '()]
          [(pair? x) (cons (car x) (loop (expose (cdr x))))]
          [else (syntax-error ctx)]))))

  (define (expand-body* body* ctx r)
    (let ([rib (make-rib '() '() '())])
      (let loop ([body* (expose (push-rib rib body*))]
                 [r r]
                 [lab* '()] [sym* '()] [marks* '()] [vrhs* '()])
        (cond
          [(null? body*) (syntax-error ctx)]
          [(pair? body*)
           (let ([a (car body*)] [d (cdr body*)])
             (cond
               [(macro-call? a r) =>
                (lambda (proc)
                  (loop (cons (push-mark (genmark) (proc (push-antimark a))) d)
                        r lab* sym* marks* vrhs*))]
               [(define? a r)
                (let-values ([(lhs rhs) (extract-define a ctx)])
                  (loop (expose d) 
                        r
                        (cons (string #\p) lab*)
                        (cons (stx-e lhs) sym*)
                        (cons (stx-marks lhs) marks*)
                        (cons rhs vrhs*)))]
               [(begin? a r)
                (loop (expose (append (begin-e* a ctx) d))
                      r lab* sym* marks* vrhs*)]
               [else
                ;;; done
                (cond
                  [(null? sym*) 
                   (let ([body* (E* body* r ctx)])
                     (build-begin body*))]
                  [else
                   (let ([g* (map newsym sym*)])
                     (let* ([r (cons (cons lab* g*) r)]
                            [rhs* 
                             (E* (push-subst sym* marks* lab* vrhs*)
                                          r ctx)]
                            [body* 
                             (E* (push-subst sym* marks* lab* body*)
                                          r ctx)])
                       (build-letrec g* rhs* (build-begin body*))))])]))]
          [else (syntax-error ctx)]))))

  (define (extract-bindings bind* ctx)
    (let ([bind* (expose bind*)])
      (cond
        [(null? bind*) (values '() '())]
        [(not (pair? bind*)) (syntax-error ctx)]
        [else
         (let ([a (car bind*)] [d (cdr bind*)])
           (let ([a (expose-ls a)])
             (cond
               [(fx= (length a) 2)
                (let-values ([(lhs* rhs*) 
                              (extract-bindings d ctx)])
                  (values (cons (car a) lhs*)
                          (cons (cadr a) rhs*)))]
               [else (syntax-error ctx)])))])))

  (define (core-stx x)
    (make-stx x '() *top-ribcage*))

  (extend-core 'quote
    (lambda (a d ctx r)
      (let ([d (expose-ls d)])
        (cond
          [(and (list? d) (fx= (length d) 1))
           (build-data (strip (car d)))]
          [else (syntax-error ctx)]))))

  (extend-core 'lambda
    (lambda (a d ctx r)
      (let ([d (expose d)])
        (cond
          [(pair? d)
           (let ([fml* (car d)] [body* (cdr d)])
             (let-values ([(fml* s* m** g* lab*)
                           (process-fml* fml* ctx)])
               (let ([body* (push-subst s* m** lab* body*)])
                 (let ([r (cons (cons lab* g*) r)])
                   (build-lambda fml* 
                     (expand-body* body* ctx r))))))]
          [else (syntax-error ctx)]))))

  (extend-core 'if
    (lambda (a d ctx r)
      (let ([d (expose d)])
        (unless (pair? d) (syntax-error ctx))
        (let ([test (car d)] [d (expose (cdr d))])
          (unless (pair? d) (syntax-error ctx))
          (let ([conseq (car d)] [d (expose (cdr d))])
            (let ([altern
                   (cond
                     [(null? d) (build-void)]
                     [(pair? d)
                      (let ([altern (car d)] [d (expose (cdr d))])
                        (cond
                          [(null? d) (E altern r)]
                          [else (syntax-error ctx)]))]
                     [else (syntax-error ctx)])])
              (build-if (E test r) (E conseq r) altern)))))))

  (extend-core 'begin
    (lambda (a d ctx r)
      (let ([d (expose-ls d)])
        (when (null? d) (syntax-error ctx))
        (build-begin (E* d r ctx)))))


  (extend-core 'define 
    (lambda (a d ctx r) (syntax-error ctx)))

  (extend-core 'foreign-call
    (lambda (a d ctx r)
      (let ([d (expose-ls d)])
        (unless (fx>= (length d) 1) (syntax-error ctx))
        (build-foreign-call
          (E (car d) r)
          (E* (cdr d) r ctx)))))

  (extend-core 'let
    (lambda (a d ctx r)
      (let ([d (expose d)])
        (unless (pair? d) (syntax-error ctx))
        (let ([bind* (car d)] [body* (cdr d)])
          (let-values ([(lhs* rhs*) 
                        (extract-bindings bind* ctx)])
            (let ([lambda^ (core-stx 'lambda)])
              (E `((,lambda^ ,lhs* . ,body*) . ,rhs*) r)))))))

  (extend-core 'let*
    (lambda (a d ctx r)
      (let ([d (expose d)])
        (unless (pair? d) (syntax-error ctx))
        (let ([bind* (car d)] [body* (cdr d)])
          (let-values ([(lhs* rhs*)
                        (extract-bindings bind* ctx)])
            (let ([lambda^ (core-stx 'lambda)])
              (E (let f ([lhs* lhs*] [rhs* rhs*])
                   (cond
                     [(null? lhs*) 
                      `((,lambda^ () . ,body*))]
                     [else
                      `((,lambda^ (,(car lhs*)) 
                          ,(f (cdr lhs*) (cdr rhs*)))
                        ,(car rhs*))]))
                 r)))))))

  (set! expand core-expand)
)
