
(library (ikarus syntax)
  (export)
  (import (scheme))
  (define who 'chi-top-library)
  (define noexpand "noexpand")
  (define-syntax no-source 
    (lambda (x) #f))
  (begin ;;; GOOD ONES
    (define-syntax build-application
      (syntax-rules ()
        ((_ ae fun-exp arg-exps)
         `(,fun-exp . ,arg-exps))))
    (define-syntax build-conditional
      (syntax-rules ()
        ((_ ae test-exp then-exp else-exp)
         `(if ,test-exp ,then-exp ,else-exp))))
    (define-syntax build-lexical-reference
      (syntax-rules ()
        ((_ ae var) var)
        ((_ type ae var) var)))
    (define-syntax build-lexical-assignment
      (syntax-rules ()
        ((_ ae var exp) `(set! ,var ,exp))))
    (define-syntax build-global-reference
      (syntax-rules ()
        [(_ ae var) `(top-level-value ',var)]))
    (define-syntax build-global-assignment
      (syntax-rules ()
        [(_ ae var exp) `(set-top-level-value! ',var ,exp)]))
    (define-syntax build-global-definition
      (syntax-rules ()
        [(_ ae var exp) (build-global-assignment ae var exp)]))
    (define-syntax build-lambda
      (syntax-rules ()
        [(_ ae vars exp) `(case-lambda [,vars ,exp])]))
    (define build-case-lambda
      (lambda (ae vars* exp*)
        `(case-lambda . ,(map list vars* exp*))))
    (define build-let
      (lambda (ae lhs* rhs* body)
        `((case-lambda [,lhs* ,body]) . ,rhs*)))
    (define-syntax build-primref
      (syntax-rules ()
        [(_ ae name)   (build-primref ae 1 name)]
        [(_ ae level name) `(|#primitive| ,name)]))
    (define-syntax build-foreign-call
      (syntax-rules ()
        [(_ ae name arg*) `(foreign-call ,name . ,arg*)]))
    (define-syntax build-data
      (syntax-rules ()
        ((_ ae exp) `',exp)))
    (define build-sequence
      (lambda (ae exps)
        (let loop ((exps exps))
          (if (null? (cdr exps))
              (car exps)
              (if (equal? (car exps) '(#%void))
                  (loop (cdr exps))
                  `(begin ,@exps))))))
    (define build-void
      (lambda () '(#%void)))
    (define build-letrec
      (lambda (ae vars val-exps body-exp)
        (if (null? vars) body-exp `(letrec ,(map list vars val-exps) ,body-exp)))))
  (define top-mark* '(top))
  (define top-marked?
    (lambda (m*) (memq 'top m*)))
  (define gen-lexical 
    (lambda (sym)
      (cond
        [(symbol? sym) 
         (gensym (symbol->string sym))]
        [(stx? sym) (gen-lexical (id->sym sym))]
        [else (error 'gen-lexical "invalid arg ~s" sym)])))
  (define gen-label
    (lambda (_) (gensym)))
  (define make-rib
    (lambda (sym* mark** label*)
      (vector 'rib sym* mark** label*)))
  (define make-full-rib
    (lambda (id* label*)
      (make-rib (map id->sym id*) (map stx-mark* id*) label*)))
  (define make-empty-rib
    (lambda ()
      (make-rib '() '() '())))
  (define extend-rib!
    (lambda (rib id label)
      (if (rib? rib)
          (let ([sym (id->sym id)] [mark* (stx-mark* id)])
            (vector-set! rib 1 (cons sym (vector-ref rib 1)))
            (vector-set! rib 2 (cons mark* (vector-ref rib 2)))
            (vector-set! rib 3 (cons label (vector-ref rib 3))))
          (error 'extend-rib! "~s is not a rib" rib))))
  (define rib?
    (lambda (x) 
      (and (vector? x)
           (= (vector-length x) 4)
           (eq? (vector-ref x 0) 'rib))))
  (define rib-sym* 
    (lambda (x)
      (if (rib? x)
          (vector-ref x 1)
          (error 'rib-sym* "~s is not a rib" x))))
  (define rib-mark** 
    (lambda (x)
      (if (rib? x)
          (vector-ref x 2)
          (error 'rib-mark** "~s is not a rib" x))))
  (define rib-label* 
    (lambda (x)
      (if (rib? x)
          (vector-ref x 3)
          (error 'rib-label* "~s is not a rib" x))))
  #;(module (make-stx stx? stx-expr stx-mark* stx-subst*)
    (define-record stx (expr mark* subst*)))
  (module (make-stx stx? stx-expr stx-mark* stx-subst*)
    (define make-stx
      (lambda (e m* s*)
        (vector 'stx e m* s*)))
    (define stx?
      (lambda (x)
        (and (vector? x) 
             (= (vector-length x) 4)
             (eq? (vector-ref x 0) 'stx))))
    (define stx-expr 
      (lambda (x)
        (if (stx? x)
            (vector-ref x 1)
            (error 'stx-expr "~s is not a syntax object" x))))
    (define stx-mark* 
      (lambda (x)
        (if (stx? x)
            (vector-ref x 2)
            (error 'stx-mark* "~s is not a syntax object" x))))
    (define stx-subst* 
      (lambda (x)
        (if (stx? x)
            (vector-ref x 3)
            (error 'stx-subst* "~s is not a syntax object" x)))))
  (define datum->stx
    (lambda (id datum)
      (make-stx datum (stx-mark* id) (stx-subst* id))))
  (define join-wraps
    (lambda (m1* s1* e)
      (define cancel 
        (lambda (ls1 ls2)
          (let f ((x (car ls1)) (ls1 (cdr ls1)))
            (if (null? ls1)
              (cdr ls2)
              (cons x (f (car ls1) (cdr ls1)))))))
      (let ((m2* (stx-mark* e)) (s2* (stx-subst* e)))
        (if (and (not (null? m1*))
                 (not (null?  m2*))
                 (eq? (car m2*) anti-mark))
            ; cancel mark, anti-mark, and corresponding shifts
            (values (cancel m1* m2*) (cancel s1* s2*))
            (values (append m1* m2*) (append s1* s2*))))))
  (define stx
    (lambda (e m* s*)
      (if (stx? e)
          (let-values ([(m* s*) (join-wraps m* s* e)])
            (make-stx (stx-expr e) m* s*))
          (make-stx e m* s*))))
  (define add-subst
    (lambda (subst e)
      (if subst
          (stx e '() (list subst))
          e)))
  (define gen-mark
    (lambda () (string #\m)))
  (define add-mark
    (lambda (m e)
      (stx e (list m) '(shift))))
  (define anti-mark #f)
  (define syntax-kind? 
    (lambda (x p?)
      (if (stx? x)
          (syntax-kind? (stx-expr x) p?)
          (p? x))))
  (define syntax-vector->list
    (lambda (x)
      (cond
        [(stx? x) 
         (let ([ls (syntax-vector->list (stx-expr x))]
               [m* (stx-mark* x)] [s* (stx-subst* x)])
           (map (lambda (x) (stx x m* s*)) ls))]
        [(vector? x) (vector->list x)]
        [else (error 'syntax-vector->list "not a syntax vector ~s" x)])))
  (define syntax-pair?
    (lambda (x) (syntax-kind? x pair?)))
  (define syntax-vector? 
    (lambda (x) (syntax-kind? x vector?)))
  (define syntax-null?
    (lambda (x) (syntax-kind? x null?)))
  (define syntax-list?
    (lambda (x)
      (or (syntax-null? x)
          (and (syntax-pair? x) (syntax-list? (syntax-cdr x))))))
  (define syntax-car
    (lambda (x)
      (if (stx? x)
          (stx (syntax-car (stx-expr x)) (stx-mark* x) (stx-subst* x))
          (if (pair? x)
              (car x)
              (error 'syntax-car "~s is not a pair" x)))))
  (define syntax->list
    (lambda (x)
      (if (syntax-pair? x)
          (cons (syntax-car x) (syntax->list (syntax-cdr x)))
          (if (syntax-null? x)
              '()
              (error 'syntax->list "invalid ~s" x)))))
  (define syntax-cdr
    (lambda (x)
      (if (stx? x)
          (stx (syntax-cdr (stx-expr x)) (stx-mark* x) (stx-subst* x))
          (if (pair? x)
              (cdr x)
              (error 'syntax-cdr "~s is not a pair" x))))) 
  (define id?
    (lambda (x) (syntax-kind? x symbol?)))
  (define id->sym
    (lambda (x)
      (if (stx? x)
          (id->sym (stx-expr x))
          (if (symbol? x)
              x
              (error 'id->sym "~s is not an id" x)))))
  (define same-marks?
    (lambda (x y)
      (or (eq? x y)
          (and (pair? x) (pair? y)
               (eq? (car x) (car y))
               (same-marks? (cdr x) (cdr y))))))
  (define bound-id=?
    (lambda (x y)
      (and (eq? (id->sym x) (id->sym y))
           (same-marks? (stx-mark* x) (stx-mark* y)))))
  (define free-id=?
    (lambda (i j)
      (let ((t0 (id->label i)) (t1 (id->label j)))
        (if (or t0 t1)
            (eq? t0 t1)
            (eq? (id->sym i) (id->sym j))))))
  (define valid-bound-ids?
    (lambda (id*)
      (and (andmap id? id*)
           (distinct-bound-ids? id*))))
  (define distinct-bound-ids?
    (lambda (id*)
      (or (null? id*)
          (and (not (bound-id-member? (car id*) (cdr id*)))
               (distinct-bound-ids? (cdr id*))))))
  (define bound-id-member?
    (lambda (id id*)
      (and (pair? id*)
           (or (bound-id=? id (car id*))
               (bound-id-member? id (cdr id*))))))
  (define self-evaluating?
    (lambda (x) 
      (or (number? x) (string? x) (char? x) (boolean? x))))
  (define stx->datum
    (lambda (x)
      (strip x '())))
  (define extend-env
    (lambda (lab b r) 
      (cons (cons lab b) r)))
  (define extend-env* 
    (lambda (lab* b* r)
      (append (map cons lab* b*) r)))
  (define cons-id
    (lambda (kwd kwd*)
      (if (id? kwd)
          (cons kwd kwd*)
          kwd*)))
  (define strip
    (lambda (x m*)
      (if (top-marked? m*) 
          x
          (let f ([x x])
            (cond
              [(stx? x) (strip (stx-expr x) (stx-mark* x))]
              [(pair? x)
               (let ([a (f (car x))] [d (f (cdr x))])
                 (if (and (eq? a (car x)) (eq? d (cdr x)))
                     x
                     (cons a d)))]
              [(vector? x)
               (let ([old (vector->list x)])
                 (let ([new (map f old)])
                   (if (andmap eq? old new) 
                       x 
                       (list->vector new))))]
              [else x])))))
  (define id->label
    (lambda (id)
      (let ([sym (id->sym id)])
        (let search ([subst* (stx-subst* id)] [mark* (stx-mark* id)])
          (cond
            [(null? subst*) #f]
            [(eq? (car subst*) 'shift) 
             (search (cdr subst*) (cdr mark*))]
            [else
             (let ([rib (car subst*)])
               (let f ([sym* (rib-sym* rib)] 
                       [mark** (rib-mark** rib)]
                       [label* (rib-label* rib)])
                 (cond
                   [(null? sym*) (search (cdr subst*) mark*)]
                   [(and (eq? (car sym*) sym)
                         (same-marks? (car mark**) mark*))
                    (car label*)]
                   [else (f (cdr sym*) (cdr mark**) (cdr label*))])))])))))
  (define label->binding
    (lambda (x r)
      (cond
        [(not x) (cons 'unbound #f)]
        [(assq x r) => cdr]
        [else (cons 'displaced-lexical #f)])))
  (define make-binding cons)
  (define binding-type car)
  (define binding-value cdr)
  (define syntax-type
    (lambda (e r)
      (cond
        [(id? e) 
         (let ([id e])
           (let* ([label (id->label id)]
                  [b (label->binding label r)]
                  [type (binding-type b)])
             (unless label 
               (stx-error e "unbound identifier"))
             (case type
               [(lexical core-prim macro)
                (values type (binding-value b) id)]
               [else (values 'other #f #f)])))]
        [(syntax-pair? e)
         (let ([id (syntax-car e)])
           (if (id? id) 
               (let* ([label (id->label id)]
                      [b (label->binding label r)]
                      [type (binding-type b)])
                 (case type
                   [(define define-syntax core-macro begin macro
                      module set!)
                    (values type (binding-value b) id)]
                   [else 
                    (values 'call #f #f)]))
               (values 'call #f #f)))]
        [else (let ([d (strip e '())])
                (if (self-evaluating? d)
                    (values 'constant d #f)
                    (values 'other #f #f)))])))
  (define parse-library 
    (lambda (e)
      (syntax-match e ()
        [(_ (name name* ...)
            (export exp* ...)
            (import (scheme))
            b* ...)
         (if (and (eq? export 'export)
                  (eq? import 'import)
                  (eq? scheme 'scheme)
                  (symbol? name)
                  (andmap symbol? name*)
                  (andmap symbol? exp*))
             (values (cons name name*) exp* b*)
             (error who "malformed library ~s" e))]
        [_ (error who "malformed library ~s" e)])))
  (define-syntax stx-error 
    (lambda (x)
      (syntax-case x ()
        [(_ stx) #'(error 'chi "invalid syntax ~s" (strip stx '()))]
        [(_ stx msg) #'(error 'chi "~a: ~s" msg (strip stx '()))])))
  (define sanitize-binding
    (lambda (x)
      (cond
        [(procedure? x) (cons 'macro x)]
        [(and (pair? x) (eq? (car x) 'macro!) (procedure? (cdr x)))
         x]
        [(and (pair? x) (eq? (car x) '$rtd)) x]
        [else (error 'expand "invalid transformer ~s" x)])))
  (define compile-time-eval-hook
    (lambda (x)
      (eval `(,noexpand ,x))))
  (define make-eval-transformer
    (lambda (x)
      (sanitize-binding (compile-time-eval-hook x))))
  (module (syntax-match) 
    (define-syntax syntax-match-test
      (lambda (ctx)
        (define dots?
          (lambda (x)
            (and (identifier? x) 
                 (free-identifier=? x #'(... ...)))))
        (define free-identifier-member?
          (lambda (x ls)
            (and (ormap (lambda (y) (free-identifier=? x y)) ls) #t)))
        (define f
          (lambda (ctx lits)
            (syntax-case ctx ()
              [id (identifier? #'id) 
               (if (free-identifier-member? #'id lits)
                   #'(lambda (x) (and (id? x) (free-id=? x (scheme-stx 'id))))
                   #'(lambda (x) #t))]
              [(pat dots) (dots? #'dots)
               (with-syntax ([p (f #'pat lits)])
                 #'(lambda (x) 
                     (and (syntax-list? x)
                          (andmap p (syntax->list x)))))]
              [(pat dots . last) (dots? #'dots)
               (with-syntax ([p (f #'pat lits)] [l (f #'last lits)])
                 #'(lambda (x)
                     (let loop ([x x])
                       (cond
                         [(syntax-pair? x) 
                          (and (p (syntax-car x)) 
                               (loop (syntax-cdr x)))]
                         [else (l x)]))))]
              [(a . d) 
               (with-syntax ([pa (f #'a lits)] [pd (f #'d lits)])
                 #'(lambda (x) 
                     (and (syntax-pair? x)
                          (pa (syntax-car x))
                          (pd (syntax-cdr x)))))]
              [#(pats ...)
               (with-syntax ([p (f #'(pats ...) lits)])
                 #'(lambda (x) 
                     (and (syntax-vector? x) 
                          (p (syntax-vector->list x)))))]
              [datum 
               #'(lambda (x) 
                   (equal? (strip x '()) 'datum))])))
        (syntax-case ctx ()
          [(_ x (lits ...) [pat code code* ...])
           (with-syntax ([pat-code (f #'pat #'(lits ...))])
             #'(pat-code x))])))
    (define-syntax syntax-match-conseq
      (lambda (ctx) 
        (define free-identifier-member?
          (lambda (x ls)
            (and (ormap (lambda (y) (free-identifier=? x y)) ls) #t)))
        (define dots?
          (lambda (x)
            (and (identifier? x) 
                 (free-identifier=? x #'(... ...))))) 
        (define f
          (lambda (stx lits)
            (syntax-case stx ()
              [id (identifier? #'id) 
               (if (free-identifier-member? #'id lits)
                   (values '() #'(lambda (x) (dont-call-me)))
                  (values (list #'id) #'(lambda (x) x)))]
              [(pat dots) (dots? #'dots)
               (let-values ([(vars extractor) (f #'pat lits)])
                  (cond
                    [(null? vars) 
                     (values '() #'(lambda (x) (dont-call-me)))]
                    [else
                     (values vars
                       (with-syntax ([(vars ...) vars]
                                     [ext extractor]
                                     [(t* ...) (generate-temporaries vars)])
                         #'(lambda (x) 
                             (let f ([x x] [vars '()] ...)
                               (cond
                                 [(syntax-null? x) 
                                  (values (reverse vars) ...)]
                                 [else
                                  (let-values ([(t* ...) (ext (syntax-car x))])
                                    (f (syntax-cdr x) 
                                       (cons t* vars)
                                       ...))])))))]))]
              [(pat dots . last) (dots? #'dots)
               (let-values ([(pvars pext) (f #'pat lits)])
                 (let-values ([(lvars lext) (f #'last lits)])
                   (cond
                     [(and (null? pvars) (null? lvars)) 
                      (values '() #'(lambda (x) (dont-call-me)))]
                     [(null? lvars)
                      (values pvars
                        (with-syntax ([(pvars ...) pvars]
                                      [(t* ...) (generate-temporaries pvars)]
                                      [pext pext])
                          #'(lambda (x)
                              (let loop ([x x] [pvars '()] ...)
                                (cond
                                  [(syntax-pair? x)
                                   (let-values ([(t* ...) (pext (syntax-car x))])
                                     (loop (syntax-cdr x) 
                                           (cons t* pvars) ...))]
                                  [else
                                   (values (reverse pvars) ...)])))))]
                     [(null? pvars)
                      (values lvars
                        (with-syntax ([lext lext])
                          #'(let loop ([x x])
                              (cond
                                [(syntax-pair? x) (loop (syntax-cdr x))]
                                [else (lext x)]))))]
                     [else
                      (values (append pvars lvars)
                        (with-syntax ([(pvars ...) pvars]
                                      [(t* ...) (generate-temporaries pvars)]
                                      [(lvars ...) lvars]
                                      [lext lext]
                                      [pext pext])
                          #'(lambda (x)
                              (let loop ([x x] [pvars '()] ...)
                                (cond
                                  [(syntax-pair? x)
                                   (let-values ([(t* ...) (pext (syntax-car x))])
                                     (loop (syntax-cdr x) 
                                           (cons t* pvars) ...))]
                                  [else
                                   (let-values ([(lvars ...) (lext x)])
                                     (values (reverse pvars) ...
                                             lvars ...))])))))])))]
              [(a . d) 
               (let-values ([(avars aextractor) (f #'a lits)])
                 (let-values ([(dvars dextractor) (f #'d lits)])
                   (cond
                     [(and (null? avars) (null? dvars))
                      (values '() #'(lambda (x) (dot-call-me)))]
                     [(null? avars) 
                      (values dvars
                        (with-syntax ([d dextractor])
                          #'(lambda (x) (d (syntax-cdr x)))))]
                     [(null? dvars) 
                      (values avars
                        (with-syntax ([a aextractor])
                          #'(lambda (x) (a (syntax-car x)))))]
                     [else
                      (values (append avars dvars)
                        (with-syntax ([(avars ...) avars]
                                      [(dvars ...) dvars]
                                      [a aextractor]
                                      [d dextractor])
                          #'(lambda (x)
                              (let-values ([(avars ...) (a (syntax-car x))])
                                (let-values ([(dvars ...) (d (syntax-cdr x))])
                                  (values avars ... dvars ...))))))])))]
              [#(pats ...) 
               (let-values ([(vars extractor) (f #'(pats ...) lits)])
                 (cond
                   [(null? vars) (values '() #f)]
                   [else 
                    (values vars
                      (with-syntax ([extractor extractor])
                        #'(lambda (x) 
                            (extractor (syntax-vector->list x)))))]))]
              [datum 
               (values '() #'(lambda (x) (dot-call-me)))])))
        (syntax-case ctx ()
          [(_ x (lits ...) [pat code code* ...])
           (let-values ([(vars extractor) 
                         (f #'pat #'(lits ...))])
             (with-syntax ([e extractor] [(vs ...) vars])
               (case (length vars)
                 [(0) #'(begin code code* ...)]
                 [(1) #'(let ([vs ... (e x)]) code code* ...)]
                 [else #'(let-values ([(vs ...) (e x)]) code code* ...)])))])))
    (define-syntax syntax-match
      (lambda (x)
        (syntax-case x ()
          [(_ expr (lits ...)) #'(stx-error expr)]
          [(_ expr (lits ...) cls cls* ...)
           #'(let ([t expr])
               (if (syntax-match-test t (lits ...) cls)
                   (syntax-match-conseq t (lits ...) cls)
                   (syntax-match t (lits ...) cls* ...)))]))))
  (define parse-define
    (lambda (x)
      (syntax-match x ()
        [(_ (id . fmls) b b* ...) 
         (if (id? id) 
             (values id
              (cons 'defun (cons fmls (cons b b*))))
             (stx-error x))]
        [(_ id val)
         (if (id? id) 
             (values id (cons 'expr val))
             (stx-error x))])))
  (define parse-define-syntax
    (lambda (x)
      (syntax-match x ()
        [(_ id val) 
         (if (id? id) 
             (values id val)
             (stx-error x))])))
  (define scheme-stx
    (lambda (sym)
      (cond
        [(assq sym scheme-env) =>
         (lambda (x)
           (let ([name (car x)] [label (cadr x)])
             (add-subst 
               (make-rib (list name) (list top-mark*) (list label))
               (stx sym top-mark* '()))))]
        [else (stx sym top-mark* '())])))
  (define scheme-env ; the-env
    '([define        define-label        (define)]
      [define-syntax define-syntax-label (define-syntax)]
      [module        module-label        (module)]
      [begin         begin-label         (begin)]
      [set!          set!-label          (set!)]
      [define-record define-record-label (macro . define-record)]
      [include       include-label       (macro . include)]
      [syntax-rules  syntax-rules-macro  (macro . syntax-rules)]
      [quasiquote    quasiquote-macro    (macro . quasiquote)]
      [with-syntax   with-syntax-label   (macro . with-syntax)]
      [let           let-label           (macro . let)]
      [let*          let*-label          (macro . let*)]
      [cond          cond-label          (macro . cond)]
      [and           and-label           (macro . and)]
      [or            or-label            (macro . or)]
      [case          case-label          (core-macro .  case)]
      [foreign-call  foreign-call-label  (core-macro .  foreign-call)]
      [quote         quote-label         (core-macro . quote)]
      [syntax-case   syntax-case-label   (core-macro . syntax-case)]
      [syntax        syntax-label        (core-macro . syntax)]
      [lambda        lambda-label        (core-macro . lambda)]
      [case-lambda   case-lambda-label   (core-macro . case-lambda)]
      [let-values    let-values-label    (core-macro . let-values)]
      [type-descriptor           type-descriptor-label           (core-macro . type-descriptor)]
      [letrec        letrec-label        (core-macro . letrec)]
      [if            if-label            (core-macro . if)]
      [when          when-label          (core-macro . when)]
      [unless        unless-label        (core-macro . unless)]
      [parameterize  parameterize-label  (core-macro . parameterize)]
      ;;; prims
      [void         void-label        (core-prim . void)]
      [not          not-label         (core-prim . not)]
      [boolean?     boolean-label     (core-prim . boolean?)]
      [null?        null?-label       (core-prim . null?)]
      [procedure?   procedure?-label  (core-prim . procedure?)]
      [eof-object?  eof-object?-label (core-prim . eof-object?)]
      [eof-object   eof-object-label  (core-prim . eof-object)]
      ;;; comparison
      [eq?        eq?-label        (core-prim . eq?)]
      [eqv?       eqv?-label       (core-prim . eqv?)]
      [equal?     equal?-label     (core-prim . equal?)]
      ;;; pairs/lists
      [cons       cons-label       (core-prim . cons)]
      [pair?      pair?-label      (core-prim . pair?)]
      [car        car-label        (core-prim . car)]
      [cdr        cdr-label        (core-prim . cdr)]
      [set-car!   set-car!-label   (core-prim . set-car!)]
      [set-cdr!   set-cdr!-label   (core-prim . set-cdr!)]
      [caar       caar-label       (core-prim . caar)]
      [cdar       cdar-label       (core-prim . cdar)]
      [cadr       cadr-label       (core-prim . cadr)]
      [cddr       cddr-label       (core-prim . cddr)]
      [caaar       caaar-label       (core-prim . caaar)]
      [cdaar       cdaar-label       (core-prim . cdaar)]
      [cadar       cadar-label       (core-prim . cadar)]
      [cddar       cddar-label       (core-prim . cddar)]
      [caadr       caadr-label       (core-prim . caadr)]
      [cdadr       cdadr-label       (core-prim . cdadr)]
      [caddr       caddr-label       (core-prim . caddr)]
      [cdddr       cdddr-label       (core-prim . cdddr)]
      [caaaar       caaaar-label       (core-prim . caaaar)]
      [cdaaar       cdaaar-label       (core-prim . cdaaar)]
      [cadaar       cadaar-label       (core-prim . cadaar)]
      [cddaar       cddaar-label       (core-prim . cddaar)]
      [caadar       caadar-label       (core-prim . caadar)]
      [cdadar       cdadar-label       (core-prim . cdadar)]
      [caddar       caddar-label       (core-prim . caddar)]
      [cdddar       cdddar-label       (core-prim . cdddar)]
      [caaadr       caaadr-label       (core-prim . caaadr)]
      [cdaadr       cdaadr-label       (core-prim . cdaadr)]
      [cadadr       cadadr-label       (core-prim . cadadr)]
      [cddadr       cddadr-label       (core-prim . cddadr)]
      [caaddr       caaddr-label       (core-prim . caaddr)]
      [cdaddr       cdaddr-label       (core-prim . cdaddr)]
      [cadddr       cadddr-label       (core-prim . cadddr)]
      [cddddr       cddddr-label       (core-prim . cddddr)]
      [list       list-label       (core-prim . list)]
      [list-ref       list-ref-label       (core-prim . list-ref)]
      [make-list make-list-label (core-prim . make-list)]
      [list*       list*-label       (core-prim . list*)]
      [list?      list?-label      (core-prim . list?)]
      [append     append-label     (core-prim . append)]
      [last-pair     last-pair-label     (core-prim . last-pair)]
      [reverse     reverse-label     (core-prim . reverse)]
      [length     length-label     (core-prim . length)]
      [assq       assq-label       (core-prim . assq)]
      [assv       assv-label       (core-prim . assv)]
      [assoc      assoc-label      (core-prim . assoc)]
      [memq       memq-label       (core-prim . memq)]
      [memv       memv-label       (core-prim . memv)]
      [member     member-label     (core-prim . member)]
      [$car       $car-label       (core-prim . $car)]
      [$cdr       $cdr-label       (core-prim . $cdr)]
      [$set-car!       $set-car!-label       (core-prim . $set-car!)]
      [$set-cdr!       $set-cdr!-label       (core-prim . $set-cdr!)]
      [$memq       $memq-label       (core-prim . $memq)]
      [$memv       $memv-label       (core-prim . $memv)]
      ;;; weak conses
      [bwp-object?  bwp-object?-label (core-prim . bwp-object?)]
      [weak-cons       weak-cons-label       (core-prim . weak-cons)]
      [weak-pair?       weak-pair?-label       (core-prim . weak-pair?)]
      ;;; chars
      [char?     char?-label     (core-prim . char?)]
      [char=?     char=?-label     (core-prim . char=?)]
      [char<?     char<?-label     (core-prim . char<?)]
      [char>?     char>?-label     (core-prim . char>?)]
      [char<=?     char<=?-label     (core-prim . char<=?)]
      [char>=?     char>=?-label     (core-prim . char>=?)]
      [integer->char integer->char-label (core-prim . integer->char)]
      [char->integer char->integer-label (core-prim . char->integer)]
      [char-whitespace? char-whitespace?-label (core-prim . char-whitespace?)]
      [$char?     $char?-label     (core-prim . $char?)]
      [$char=    $char=-label    (core-prim . $char=)]
      [$char<    $char<-label    (core-prim . $char<)]
      [$char>    $char>-label    (core-prim . $char>)]
      [$char<=    $char<=-label    (core-prim . $char<=)]
      [$char>=    $char>=-label    (core-prim . $char>=)]
      [$char->fixnum $char->fixnum-label (core-prim . $char->fixnum)]
      [$fixnum->char $fixnum->char-label (core-prim . $fixnum->char)]
      ;;; strings
      [string?    string?-label    (core-prim . string?)]
      [string    string-label    (core-prim . string)]
      [make-string make-string-label (core-prim . make-string)]
      [string-ref string-ref-label (core-prim . string-ref)]
      [string-set! string-set!-label (core-prim . string-set!)]
      [string-length string-length-label (core-prim . string-length)]
      [string=?   string=?-label   (core-prim . string=?)]
      [substring  substring-label  (core-prim . substring)]
      [string-append string-append-label (core-prim . string-append)]
      [string->list string->list-label (core-prim . string->list)]
      [list->string list->string-label (core-prim . list->string)]
      [uuid    uuid-label    (core-prim . uuid)]
      [date-string    date-string-label    (core-prim . date-string)]
      [$make-string $make-string-label (core-prim . $make-string)]
      [$string-ref $string-ref-label (core-prim . $string-ref)]
      [$string-set! $string-set!-label (core-prim . $string-set!)]
      [$string-length $string-length-label (core-prim . $string-length)]
      ;;; vectors
      [vector      vector-label      (core-prim . vector)]
      [make-vector make-vector-label (core-prim . make-vector)]
      [vector-ref  vector-ref-label  (core-prim . vector-ref)]
      [vector-set! vector-set!-label (core-prim . vector-set!)]
      [vector?     vector?-label     (core-prim . vector?)]
      [vector-length vector-length-label (core-prim . vector-length)]
      [list->vector list->vector-label (core-prim . list->vector)]
      [vector->list vector->list-label (core-prim . vector->list)]
      [$make-vector $make-vector-label (core-prim . $make-vector)]
      [$vector-length $vector-length-label (core-prim . $vector-length)]
      [$vector-ref $vector-ref-label (core-prim . $vector-ref)]
      [$vector-set! $vector-set!-label (core-prim . $vector-set!)]
      ;;; iterators
      [for-each   for-each-label   (core-prim . for-each)]
      [map        map-label        (core-prim . map)]
      [andmap     andmap-label     (core-prim . andmap)]
      [ormap      ormap-label      (core-prim . ormap)]
      ;;; fixnums
      [fixnum?    fixnum-label     (core-prim . fixnum?)]
      [fx<        fx<-label        (core-prim . fx<)]
      [fx<=       fx<=-label       (core-prim . fx<=)]
      [fx>        fx>-label        (core-prim . fx>)]
      [fx>=       fx>=-label       (core-prim . fx>=)]
      [fx=        fx=-label        (core-prim . fx=)]
      [fx-        fx--label        (core-prim . fx-)]
      [fx+        fx+-label        (core-prim . fx+)]
      [fx*        fx*-label        (core-prim . fx*)]
      [fxzero?    fxzero?-label    (core-prim . fxzero?)]
      [fxadd1     fxadd1-label     (core-prim . fxadd1)]
      [fxsub1     fxsub1-label     (core-prim . fxsub1)]
      [fxquotient fxquotient-label (core-prim . fxquotient)]
      [fxremainder fxremainder-label (core-prim . fxremainder)]
      [fxmodulo fxmodulo-label (core-prim . fxmodulo)]
      [fxsll       fxsll-label       (core-prim . fxsll)]
      [fxsra       fxsra-label       (core-prim . fxsra)]
      [fxlogand    fxlogand-label    (core-prim . fxlogand)]
      [fxlogxor    fxlogxor-label    (core-prim . fxlogxor)]
      [fxlogor    fxlogor-label    (core-prim . fxlogor)]
      [fxlognot    fxlognot-label    (core-prim . fxlognot)]
      [fixnum->string fixnum->string-label (core-prim . fixnum->string)]
      [$fxzero?    $fxzero?-label    (core-prim . $fxzero?)]
      [$fxadd1        $fxadd1-label        (core-prim . $fxadd1)]
      [$fxsub1        $fxsub1-label        (core-prim . $fxsub1)]
      [$fx>=       $fx>=-label       (core-prim . $fx>=)]
      [$fx<=       $fx<=-label       (core-prim . $fx<=)]
      [$fx>       $fx>-label       (core-prim . $fx>)]
      [$fx<       $fx<-label       (core-prim . $fx<)]
      [$fx=        $fx=-label        (core-prim . $fx=)]
      [$fxsll        $fxsll-label        (core-prim . $fxsll)]
      [$fxsra        $fxsra-label        (core-prim . $fxsra)]
      [$fxquotient $fxquotient-label (core-prim . $fxquotient)]
      [$fxmodulo $fxmodulo-label (core-prim . $fxmodulo)]
      [$fxlogxor        $fxlogxor-label        (core-prim . $fxlogxor)]
      [$fxlogor        $fxlogor-label        (core-prim . $fxlogor)]
      [$fxlognot        $fxlognot-label        (core-prim . $fxlognot)]
      [$fxlogand        $fxlogand-label        (core-prim . $fxlogand)]
      [$fx+        $fx+-label        (core-prim . $fx+)]
      [$fx*        $fx*-label        (core-prim . $fx*)]
      [$fx-        $fx--label        (core-prim . $fx-)]
      ;;; flonum
      [string->flonum string->flonum-label (core-prim . string->flonum)]
      ;;; generic arithmetic
      [-          minus-label    (core-prim . -)]
      [=          =-label        (core-prim . =)]
      [<          <-label        (core-prim . <)]
      [>          >-label        (core-prim . >)]
      [<=          <=-label      (core-prim . <=)]
      [>=          >=-label      (core-prim . >=)]
      [*          *-label        (core-prim . *)]
      [+          plus-label     (core-prim . +)]
      [add1        add1-label    (core-prim . add1)]
      [sub1        sub1-label    (core-prim . sub1)]
      [number?   number?-label   (core-prim . number?)]
      [bignum?   bignum?-label   (core-prim . bignum?)]
      [integer?   integer?-label   (core-prim . integer?)]
      [flonum?   flonum?-label   (core-prim . flonum?)]
      [quotient   quotient-label   (core-prim . quotient)]
      [remainder   remainder-label   (core-prim . remainder)]
      [quotient+remainder   quotient+remainder-label   (core-prim . quotient+remainder)]
      [number->string   number->string-label   (core-prim . number->string)]
      [string->number   string->number-label   (core-prim . string->number)]
      ;;; other numerics
      [flonum->string   flonum->string-label   (core-prim . flonum->string)]
      [string->flonum   string->flonum-label   (core-prim . string->flonum)]
      ;;; symbols/gensyms
      [symbol?    symbol?-label    (core-prim . symbol?)]
      [gensym?    gensym?-label    (core-prim . gensym?)]
      [gensym     gensym-label     (core-prim . gensym)]
      [getprop    getprop-label    (core-prim . getprop)]
      [putprop    putprop-label    (core-prim . putprop)]
      [remprop    remprop-label    (core-prim . remprop)]
      [property-list    property-list-label    (core-prim . property-list)]
      [string->symbol string->symbol-label (core-prim . string->symbol)]
      [symbol->string symbol->string-label (core-prim . symbol->string)]
      [gensym->unique-string gensym->unique-string-label (core-prim . gensym->unique-string)]
      [$make-symbol     $make-symbol-label     (core-prim . $make-symbol)]
      [$symbol-unique-string $symbol-unique-string-label (core-prim . $symbol-unique-string)]
      [$symbol-value $symbol-value-label (core-prim . $symbol-value)]
      [$symbol-string $symbol-string-label (core-prim . $symbol-string)]
      [$symbol-plist $symbol-plist-label (core-prim . $symbol-plist)]
      [$set-symbol-value! $set-symbol-value!-label (core-prim . $set-symbol-value!)]
      [$set-symbol-string! $set-symbol-string!-label (core-prim . $set-symbol-string!)]
      [$set-symbol-unique-string! $set-symbol-unique-string!-label (core-prim . $set-symbol-unique-string!)]
      [$set-symbol-plist! $set-symbol-plist!-label (core-prim . $set-symbol-plist!)]
      ;;; top-level
      [top-level-bound?     top-level-bound-label      (core-prim . top-level-bound?)]
      [top-level-value      top-level-value-label      (core-prim .  top-level-value)]
      [set-top-level-value! set-top-level-value!-label (core-prim .  set-top-level-value!)]
      ;;; guardians
      [make-guardian     make-guardian-label     (core-prim . make-guardian)]
      ;;; IO/low-level
      [$make-port/input        $make-port/input-label        (core-prim .  $make-port/input)]
      [$make-port/output        $make-port/output-label        (core-prim .  $make-port/output)]
      [$make-port/both        $make-port/both-label        (core-prim .  $make-port/both)]
      [$port-handler        $port-handler-label        (core-prim .  $port-handler)]
      [$port-input-buffer        $port-input-buffer-label        (core-prim .  $port-input-buffer)]
      [$port-input-index        $port-input-index-label        (core-prim .  $port-input-index)]
      [$port-input-size        $port-input-size-label        (core-prim .  $port-input-size)]
      [$port-output-buffer        $port-output-buffer-label        (core-prim .  $port-output-buffer)]
      [$port-output-index        $port-output-index-label        (core-prim .  $port-output-index)]
      [$port-output-size        $port-output-size-label        (core-prim .  $port-output-size)]
      [$set-port-input-index!        $set-port-input-index!-label        (core-prim .  $set-port-input-index!)]
      [$set-port-input-size!        $set-port-input-size!-label        (core-prim .  $set-port-input-size!)]
      [$set-port-output-index!        $set-port-output-index!-label        (core-prim .  $set-port-output-index!)]
      [$set-port-output-size!        $set-port-output-size!-label        (core-prim .  $set-port-output-size!)]
      [make-input-port        make-input-port-label        (core-prim .  make-input-port)]
      [make-output-port        make-output-port-label        (core-prim .  make-output-port)]
      [make-input/output-port        make-input/output-port-label        (core-prim .  make-input/output-port)]
      [$make-input-port        $make-input-port-label        (core-prim .  $make-input-port)]
      [$make-output-port        $make-output-port-label        (core-prim .  $make-output-port)]
      [$make-input/output-port        $make-input/output-port-label        (core-prim .  $make-input/output-port)]
      [port-output-index        port-output-index-label        (core-prim .  port-output-index)]
      [port-output-size        port-output-size-label        (core-prim .  port-output-size)]
      [port-output-buffer        port-output-buffer-label        (core-prim .  port-output-buffer)]
      [set-port-output-index!        set-port-output-index!-label        (core-prim .  set-port-output-index!)]
      [set-port-output-size!        set-port-output-size!-label        (core-prim .  set-port-output-size!)]
      [port-input-buffer        port-input-buffer-label        (core-prim .  port-input-buffer)]
      [port-input-index        port-input-index-label        (core-prim .  port-input-index)]
      [port-input-size        port-input-size-label        (core-prim .  port-input-size)]
      [set-port-input-index!        set-port-input-index!-label        (core-prim .  set-port-input-index!)]
      [set-port-input-size!        set-port-input-size!-label        (core-prim .  set-port-input-size!)]
      [*standard-input-port*        *standard-input-port*-label        (core-prim .  *standard-input-port*)]
      [*standard-output-port*        *standard-output-port*-label        (core-prim .  *standard-output-port*)]
      [*standard-error-port*        *standard-error-port*-label        (core-prim .  *standard-error-port*)]
      [*current-input-port*        *current-input-port*-label        (core-prim .  *current-input-port*)]
      [*current-output-port*        *current-output-port*-label        (core-prim .  *current-output-port*)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;[port        port-label        (core-prim .  port)]
      ;;; IO/ports
      [output-port?        output-port?-label        (core-prim .  output-port?)]
      [input-port?        input-port?-label        (core-prim .  input-port?)]
      [port?        port?-label        (core-prim .  port?)]
      [port-name        port-name-label        (core-prim .  port-name)]
      [input-port-name        input-port-name-label        (core-prim .  input-port-name)]
      [output-port-name        output-port-name-label        (core-prim .  output-port-name)]
      [open-input-file  open-input-file-label  (core-prim .  open-input-file)]
      [with-input-from-file  with-input-from-file-label  (core-prim .  with-input-from-file)]
      [with-output-to-file  with-output-to-file-label  (core-prim .  with-output-to-file)]
      [open-output-file  open-output-file-label  (core-prim .  open-output-file)]
      [open-output-string  open-output-string-label  (core-prim .  open-output-string)]
      [get-output-string  get-output-string-label  (core-prim .  get-output-string)]
      [close-input-port  close-input-port-label  (core-prim .  close-input-port)]
      [close-output-port  close-output-port-label  (core-prim .  close-output-port)]
      [console-input-port  console-input-port-label  (core-prim .  console-input-port)]
      [console-output-port console-output-port-label (core-prim .  console-output-port)]
      [current-input-port  current-input-port-label  (core-prim .  current-input-port)]
      [current-output-port current-output-port-label (core-prim .  current-output-port)]
      [standard-input-port  standard-input-port-label  (core-prim .  standard-input-port)]
      [standard-output-port standard-output-port-label (core-prim .  standard-output-port)]
      [standard-error-port  standard-error-port-label  (core-prim .  standard-error-port)]
      [flush-output-port   flush-output-port-label   (core-prim .  flush-output-port)]
      [reset-input-port!   reset-input-port!-label   (core-prim .  reset-input-port!)]
      [$flush-output-port   $flush-output-port-label   (core-prim .  $flush-output-port)]
      [$reset-input-port!   $reset-input-port!-label   (core-prim .  $reset-input-port!)]
      [$close-input-port  $close-input-port-label  (core-prim .  $close-input-port)]
      [$close-output-port  $close-output-port-label  (core-prim .  $close-output-port)]
      ;;; IO/high-level
      [display    display-label    (core-prim . display)]
      [write      write-label      (core-prim . write)]
      [write-char write-char-label (core-prim . write-char)]
      [read       read-label       (core-prim . read)]
      [read-char  read-char-label  (core-prim . read-char)]
      [read-token  read-token-label  (core-prim . read-token)]
      [peek-char  peek-char-label  (core-prim . peek-char)]
      [unread-char  unread-char-label  (core-prim . unread-char)]
      [newline    newline-label    (core-prim . newline)]
      [printf     printf-label     (core-prim . printf)]
      [format     format-label     (core-prim . format)]
      [pretty-print pretty-print-label (core-prim . pretty-print)]
      [comment-handler comment-handler-label (core-prim . comment-handler)]
      [print-gensym print-gensym-label (core-prim . print-gensym)]
      [gensym-count       gensym-count-label       (core-prim . gensym-count)]
      [gensym-prefix       gensym-prefix-label       (core-prim . gensym-prefix)]
      [$write-char $write-char-label (core-prim . $write-char)]
      [$read-char  $read-char-label  (core-prim . $read-char)]
      [$peek-char  $peek-char-label  (core-prim . $peek-char)]
      [$unread-char  $unread-char-label  (core-prim . $unread-char)]
      ;;; hash tables
      [make-hash-table make-hash-table-label (core-prim . make-hash-table)]
      [hash-table?      hash-table?-label      (core-prim . hash-table?)]
      [get-hash-table get-hash-table-label (core-prim . get-hash-table)]
      [put-hash-table! put-hash-table!-label (core-prim . put-hash-table!)]
      ;;; evaluation / control
      [make-parameter      make-parameter-label      (core-prim . make-parameter)]
      [apply      apply-label      (core-prim . apply)]
      [values     values-label     (core-prim . values)]
      [call-with-values cwv-label  (core-prim . call-with-values)]
      [current-eval current-eval-label (core-prim . current-eval)]
      [call/cc     call/cc-label   (core-prim . call/cc)]
      [call/cf     call/cf-label   (core-prim . call/cf)]
      [dynamic-wind dynamic-wind-label (core-prim . dynamic-wind)]
      [error      error-label      (core-prim . error)]
      [print-error print-error-label (core-prim . print-error)]
      [error-handler error-handler-label (core-prim .  error-handler)]
      [interrupt-handler interrupt-handler-label (core-prim .  interrupt-handler)]
      [exit       exit-label       (core-prim . exit)]
      [compile    compile-label    (core-prim . compile)]
      [compile-core-expr-to-port    compile-core-expr-to-port-label    (core-prim . compile-core-expr-to-port)]
      [eval       eval-label       (core-prim . eval)]
      [load       load-label       (core-prim . load)]
      [expand-mode       expand-mode-label       (core-prim . expand-mode)]
      [assembler-output       assembler-output-label       (core-prim . assembler-output)]
      [current-expand       current-expand-label       (core-prim . current-expand)]
      [expand       expand-label       (core-prim . expand)]
      [fasl-write       fasl-write-label       (core-prim . fasl-write)]
      [new-cafe   new-cafe-label   (core-prim . new-cafe)]
      [command-line-arguments command-line-arguments-label (core-prim .  command-line-arguments)]
      [list*->code*       list*->code*-label       (core-prim . list*->code*)]
      ;;; record/mid-level
      [record?                 record?-label                 (core-prim . record?)]
      [make-record-type        make-record-type-label        (core-prim . make-record-type)]
      [record-type-descriptor  record-type-descriptor-label  (core-prim . record-type-descriptor)]
      [record-type-field-names record-type-field-names-label (core-prim . record-type-field-names)]
      [record-type-symbol      record-type-symbol-label      (core-prim . record-type-symbol)]
      [record-type-name        record-type-name-label        (core-prim . record-type-name)]
      [record-name             record-name-label             (core-prim . record-name)]
      [record-constructor      record-constructor-label      (core-prim . record-constructor)]
      [record-predicate        record-predicate-labe         (core-prim . record-predicate)]
      [record-length           record-length-label           (core-prim . record-length)]
      [record-printer          record-printer-label          (core-prim . record-printer)]
      [record-ref              record-ref-label              (core-prim . record-ref)]
      [record-field-accessor   record-field-accessor-label   (core-prim . record-field-accessor)]
      [record-field-mutator    record-field-mutator-label    (core-prim . record-field-mutator)]
      ;;; records/low-level 
      [$base-rtd    $base-rtd-label    (core-prim . $base-rtd)]
      [$record-set! $record-set!-label (core-prim . $record-set!)]
      [$record-ref  $record-ref-label  (core-prim . $record-ref)]
      [$record-rtd  $record-rtd-label  (core-prim . $record-rtd)]
      [$record      $record-label      (core-prim . $record)]
      [$make-record $make-record-label (core-prim . $make-record)]
      [$record?     $record?-label     (core-prim . $record?)]
      [$record/rtd? $record/rtd?-label (core-prim . $record/rtd?)]
      ;;; syntax-case
      [identifier?          identifier?-label          (core-prim . identifier?)]
      [syntax-error             syntax-error-label             (core-prim . syntax-error)]
      [generate-temporaries generate-temporaries-label (core-prim . generate-temporaries)]
      [free-identifier=?    free-identifier=?-label    (core-prim . free-identifier=?)]
      [chi-top-library    chi-top-library-label    (core-prim . chi-top-library)]
      ;;; codes
      [$closure-code  $closure-code-label (core-prim . $closure-code)]
      [$code? $code?-label (core-prim . $code?)]
      [$code->closure $code->closure-label (core-prim . $code->closure)]
      [$code-reloc-vector $code-reloc-vector-label (core-prim . $code-reloc-vector)]
      [$code-freevars $code-freevars-label (core-prim . $code-freevars)]
      [$code-size $code-size-label (core-prim . $code-size)]
      [$code-ref $code-ref-label (core-prim . $code-ref)]
      [$code-set! $code-set!-label (core-prim . $code-set!)]
      [code? code?-label (core-prim . code?)]
      [make-code make-code-label (core-prim . make-code)]
      [code-reloc-vector code-reloc-vector-label (core-prim . code-reloc-vector)]
      [set-code-reloc-vector! set-code-reloc-vector!-label (core-prim . set-code-reloc-vector!)]
      [code-size code-size-label (core-prim . code-size)]
      [code-freevars code-freevars-label (core-prim . code-freevars)]
      [code-ref code-ref-label (core-prim . code-ref)]
      [code-set! code-set!-label (core-prim . code-set!)]
      ;;; tcbuckets
      [$make-tcbucket     $make-tcbucket-label    (core-prim . $make-tcbucket)]
      [$tcbucket-key     $tcbucket-key-label    (core-prim . $tcbucket-key)]
      [$tcbucket-val     $tcbucket-val-label    (core-prim . $tcbucket-val)]
      [$tcbucket-next     $tcbucket-next-label    (core-prim . $tcbucket-next)]
      [$set-tcbucket-val! $set-tcbucket-val!-label (core-prim . $set-tcbucket-val!)]
      [$set-tcbucket-next! $set-tcbucket-next!-label (core-prim . $set-tcbucket-next!)]
      [$set-tcbucket-tconc! $set-tcbucket-tconc!-label (core-prim . $set-tcbucket-tconc!)]
      ;;; misc
      [immediate?     immediate?-label    (core-prim . immediate?)]
      [pointer-value     pointer-value-label    (core-prim . pointer-value)]
      [$forward-ptr?     $forward-ptr?-label    (core-prim . $forward-ptr?)]
      ;;; junk that should go away
      [$unbound-object?     $unbound-object?-label    (core-prim . $unbound-object?)]
      [$make-call-with-values-procedure $make-cwv-procedure (core-prim . $make-call-with-values-procedure)]
      [$make-values-procedure $make-values-procedure (core-prim . $make-values-procedure)]
      [primitive-set! primitive-set!-label (core-prim . primitive-set!)]
      [primitive? primitive?-label (core-prim .  primitive?)]
      [primitive-ref primitive-ref-label (core-prim .  primitive-ref)]
      [$$apply     $$apply-label    (core-prim . $$apply)]
      [$arg-list     $arg-list-label    (core-prim . $arg-list)]
      [$interrupted? $interrupted?-label (core-prim .  $interrupted?)]
      [$unset-interrupted!     $unset-interrupted!-label    (core-prim . $unset-interrupted!)]
      [$fp-at-base     $fp-at-base-label    (core-prim . $fp-at-base)]
      [$primitive-call/cc     $primitive-call/cc-label    (core-prim . $primitive-call/cc)]
      [$frame->continuation     $frame->continuation-label    (core-prim . $frame->continuation)]
      [$current-frame     $current-frame-label    (core-prim . $current-frame)]
      [$seal-frame-and-call     $seal-frame-and-call-label    (core-prim . $seal-frame-and-call)]
      ))
  (define make-scheme-rib
    (lambda ()
      (let ([rib (make-empty-rib)])
        (for-each 
          (lambda (x)
            (let ([name (car x)] [label (cadr x)])
              (extend-rib! rib (stx name top-mark* '()) label)))
          scheme-env)
        rib)))
  (define make-scheme-env
    (lambda ()
      (map 
        (lambda (x) 
           (let ([name (car x)] [label (cadr x)] [binding (caddr x)])
             (cons label binding)))
         scheme-env)))
  ;;; macros
  (define add-lexicals
    (lambda (lab* lex* r)
      (append (map (lambda (lab lex)
                     (cons lab (cons 'lexical lex)))
                   lab* lex*)
              r)))
  (define let-values-transformer ;;; go away
    (lambda (e r mr) 
      (syntax-match e ()
        [(_ ([(fml** ...) rhs*] ...) b b* ...)
         (let ([rhs* (chi-expr* rhs* r mr)])
           (let ([lex** (map (lambda (ls) (map gen-lexical ls)) fml**)]
                 [lab** (map (lambda (ls) (map gen-label ls)) fml**)])
             (let ([fml* (apply append fml**)]
                   [lab* (apply append lab**)]
                   [lex* (apply append lex**)])
               (let f ([lex** lex**] [rhs* rhs*])
                  (cond
                    [(null? lex**) 
                     (chi-internal 
                       (add-subst 
                         (make-full-rib fml* lab*)
                         (cons b b*))
                       (add-lexicals lab* lex* r)
                       mr)]
                    [else
                     (build-application no-source 
                       (build-primref no-source 'call-with-values)
                       (list 
                         (build-lambda no-source '() (car rhs*))
                         (build-lambda no-source (car lex**) 
                           (f (cdr lex**) (cdr rhs*)))))])))))])))
  (define letrec-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ ([lhs* rhs*] ...) b b* ...) 
         (if (not (valid-bound-ids? lhs*))
             (stx-error e)
             (let ([lex* (map gen-lexical lhs*)]
                   [lab* (map gen-label lhs*)])
               (let ([rib (make-full-rib lhs* lab*)]
                     [r (add-lexicals lab* lex* r)])
                 (let ([body (chi-internal 
                               (add-subst rib (cons b b*))
                               r mr)]
                       [rhs* (chi-expr* 
                               (map (lambda (x) 
                                      (add-subst rib x))
                                    rhs*)
                               r mr)])
                   (build-letrec no-source 
                     lex* rhs* body)))))])))
  (define type-descriptor-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ id) 
         (unless (id? id) (stx-error e))
         (let* ([lab (id->label id)]
                [b (label->binding lab r)]
                [type (binding-type b)])
           (unless lab (stx-error e "unbound identifier"))
           (case type
             [($rtd)
              (build-data no-source (binding-value b))]
             [else (stx-error e "invalid type")]))])))
  (define when-transformer ;;; go away
    (lambda (e r mr)
      (syntax-match e ()
        [(_ test e e* ...) 
         (build-conditional no-source
           (chi-expr test r mr)
           (build-sequence no-source
             (chi-expr* (cons e e*) r mr))
           (build-void))])))
  (define unless-transformer ;;; go away
    (lambda (e r mr)
      (syntax-match e ()
        [(_ test e e* ...) 
         (build-conditional no-source
           (chi-expr test r mr)
           (build-void) 
           (build-sequence no-source
             (chi-expr* (cons e e*) r mr)))])))
  (define if-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ e0 e1 e2)
         (build-conditional no-source
           (chi-expr e0 r mr)
           (chi-expr e1 r mr)
           (chi-expr e2 r mr))]
        [(_ e0 e1)
         (build-conditional no-source
           (chi-expr e0 r mr)
           (chi-expr e1 r mr)
           (build-void))])))
  (define case-transformer ;;; go away
    (lambda (e r mr)
      (define build-one 
        (lambda (t cls rest)
          (syntax-match cls ()
            [((d* ...) e e* ...)
             (build-conditional no-source
               (build-application no-source
                 (build-primref no-source 'memv)
                 (list t (build-data no-source (strip d* '()))))
               (build-sequence no-source
                 (chi-expr* (cons e e*) r mr))
               rest)]
            [else (stx-error e)])))
      (define build-last 
        (lambda (t cls)
          (syntax-match cls ()
            [((d* ...) e e* ...)
             (build-one t cls (build-void))]
            [(else-kwd x x* ...)
             (if (and (id? else-kwd)
                      (free-id=? else-kwd (scheme-stx 'else)))
                 (build-sequence no-source
                   (chi-expr* (cons x x*) r mr))
                 (stx-error e))]
            [else (stx-error e)])))
      (syntax-match e ()
        [(_ expr) 
         (build-sequence no-source 
           (list (chi-expr expr r mr) (build-void)))]
        [(_ expr cls cls* ...)
         (let ([t (gen-lexical 't)])
           (build-let no-source 
              (list t) (list (chi-expr expr r mr))
              (let f ([cls cls] [cls* cls*])
                (cond
                  [(null? cls*) (build-last t cls)]
                  [else
                   (build-one t cls
                     (f (car cls*) (cdr cls*)))]))))])))
  (define quote-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ datum) (build-data no-source (strip datum '()))])))
  (define case-lambda-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ [fmls* b* b** ...] ...)
         (let-values ([(fmls* body*)
                       (chi-lambda-clause* fmls*
                         (map cons b* b**) r mr)])
           (build-case-lambda no-source fmls* body*))])))
  (define lambda-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ fmls b b* ...)
         (let-values ([(fmls body) 
                       (chi-lambda-clause fmls 
                          (cons b b*) r mr)])
           (build-lambda no-source fmls body))])))
  (define bless
    (lambda (x)
      (stx 
        (let f ([x x])
          (cond
            [(pair? x) (cons (f (car x)) (f (cdr x)))]
            [(symbol? x) (scheme-stx x)]
            [else x]))
        '() '())))
  (define with-syntax-macro
    (lambda (e)
      (syntax-match e ()
        [(_ ([fml* expr*] ...) b b* ...) 
         (bless
           `(syntax-case (list . ,expr*) ()
              [,fml* (begin ,b . ,b*)]))])))
  (define let-macro
    (lambda (stx)
      (syntax-match stx ()
        [(_ ([lhs* rhs*] ...) b b* ...)
         (if (valid-bound-ids? lhs*)
             (bless `((lambda ,lhs* ,b . ,b*) . ,rhs*))
             (stx-error stx "invalid syntax"))]
        [(_ f ([lhs* rhs*] ...) b b* ...)
         (if (and (id? f) (valid-bound-ids? lhs*))
             (bless `(letrec ([,f (lambda ,lhs* ,b . ,b*)])
                        (,f . ,rhs*)))
             (stx-error stx "invalid syntax"))])))
  (define let*-macro
    (lambda (stx)
      (syntax-match stx ()
        [(_ ([lhs* rhs*] ...) b b* ...)
         (if (andmap id? lhs*)
             (bless
               (let f ([x* (map list lhs* rhs*)])
                 (cond
                   [(null? x*) `(let () ,b . ,b*)]
                   [else `(let (,(car x*)) ,(f (cdr x*)))])))
             (stx-error stx "invalid bindings"))])))
  (define or-macro
    (lambda (stx)
      (syntax-match stx ()
        [(_) #f]
        [(_ e e* ...)
         (bless 
           (let f ([e e] [e* e*])
             (cond
               [(null? e*) `(begin #f ,e)]
               [else
                `(let ([t ,e])
                   (if t t ,(f (car e*) (cdr e*))))])))])))
  (define and-macro
    (lambda (stx)
      (syntax-match stx ()
        [(_) #t]
        [(_ e e* ...)
         (bless
           (let f ([e e] [e* e*])
             (cond
               [(null? e*) `(begin #f ,e)]
               [else `(if ,e ,(f (car e*) (cdr e*)) #f)])))])))
  (define cond-macro
    (lambda (stx)
      (syntax-match stx ()
        [(_ cls cls* ...)
         (bless
           (let f ([cls cls] [cls* cls*])
             (cond
               [(null? cls*)
                (syntax-match cls (else =>)
                  [(else e e* ...) `(begin ,e . ,e*)]
                  [(e => p) `(let ([t ,e]) (if t (,p t) (void)))]
                  [(e) `(or ,e (void))]
                  [(e e* ...) `(if ,e (begin . ,e*) (void))]
                  [_ (stx-error stx "invalid last clause")])]
               [else
                (syntax-match cls (else =>)
                  [(else e e* ...) (stx-error stx "incorrect position of keyword else")]
                  [(e => p) `(let ([t ,e]) (if t (,p t) ,(f (car cls*) (cdr cls*))))]
                  [(e) `(or ,e ,(f (car cls*) (cdr cls*)))]
                  [(e e* ...) `(if ,e (begin . ,e*) ,(f (car cls*) (cdr cls*)))]
                  [_ (stx-error stx "invalid last clause")])])))])))
  (define include-macro
    (lambda (e)
      (syntax-match e ()
        [(id filename)
         (let ([filename (stx->datum filename)])
           (unless (string? filename) (stx-error e))
           (with-input-from-file filename
             (lambda ()
               (let f ([ls '()])
                 (let ([x (read)])
                   (cond
                     [(eof-object? x) 
                      (cons (bless 'begin) 
                        (datum->stx id (reverse ls)))]
                     [else (f (cons x ls))]))))))])))
  (define syntax-rules-macro
    (lambda (e)
      (syntax-match e ()
        [(_ (lits ...) 
            [pat* tmp*] ...)
         (unless (andmap
                   (lambda (x) 
                     (and (id? x) 
                          (not (free-id=? x (scheme-stx '...)))
                          (not (free-id=? x (scheme-stx '_)))))
                   lits)
           (stx-error e "invalid literals"))
         (bless `(lambda (x) 
                   (syntax-case x ,lits
                     ,@(map (lambda (pat tmp)
                              `[,pat (syntax ,tmp)])
                            pat* tmp*))))])))
  (define quasiquote-macro
    (let ()
      (define-syntax app
        (syntax-rules (quote)
          [(_ 'x arg* ...) 
           (list (scheme-stx 'x) arg* ...)]))
      (define-syntax app*
        (syntax-rules (quote)
          [(_ 'x arg* ... last) 
           (list* (scheme-stx 'x) arg* ... last)]))
      (define quasilist*
        (lambda (x y)
          (let f ((x x))
            (if (null? x) y (quasicons (car x) (f (cdr x)))))))
      (define quasicons
        (lambda (x y)
          (syntax-match y (quote list)
            [(quote dy)
             (syntax-match x (quote)
               [(quote dx) (app 'quote (cons dx dy))]
               [_
                (syntax-match dy ()
                  [() (app 'list x)]
                  [_  (app 'cons x y)])])]
            [(list stuff ...)
             (app* 'list x stuff)]
            [_ (app 'cons x y)])))
      (define quasiappend
        (lambda (x y)
          (let ([ls (let f ((x x))
                      (if (null? x)
                          (syntax-match y (quote)
                            [(quote ()) '()]
                            [_ (list y)])
                          (syntax-match (car x) (quote)
                            [(quote ()) (f (cdr x))]
                            [_ (cons (car x) (f (cdr x)))])))])
            (cond
              [(null? ls) (app 'quote '())]
              [(null? (cdr ls)) (car ls)]
              [else (app* 'append ls)]))))
      (define quasivector
        (lambda (x)
          (let ((pat-x x))
            (syntax-match pat-x (quote)
              [(quote (x* ...)) (app 'quote (list->vector x*))]
              [_ (let f ((x x) (k (lambda (ls) (app* 'vector ls))))
                   (syntax-match x (quote list cons)
                     [(quote (x* ...))
                      (k (map (lambda (x) (app 'quote x)) x*))]
                     [(list x* ...)
                      (k x*)]
                     [(cons x y)
                      (f y (lambda (ls) (k (cons x ls))))]
                     [_ (app 'list->vector pat-x)]))]))))
      (define vquasi
        (lambda (p lev)
          (syntax-match p ()
            [(p . q)
             (syntax-match p (unquote unquote-splicing)
               [(unquote p ...)
                (if (= lev 0)
                    (quasilist* p (vquasi q lev))
                    (quasicons
                      (quasicons (app 'quote 'unquote)
                                 (quasi p (- lev 1)))
                      (vquasi q lev)))]
               [(unquote-splicing p ...)
                (if (= lev 0)
                    (quasiappend p (vquasi q lev))
                    (quasicons
                      (quasicons
                        (app 'quote 'unquote-splicing)
                        (quasi p (- lev 1)))
                      (vquasi q lev)))]
               [p (quasicons (quasi p lev) (vquasi q lev))])]
            [() (app 'quote '())])))
      (define quasi
        (lambda (p lev)
          (syntax-match p (unquote unquote-splicing quasiquote)
            [(unquote p)
             (if (= lev 0)
                 p
                 (quasicons (app 'quote 'unquote) (quasi (list p) (- lev 1))))]
            [((unquote p ...) . q)
             (if (= lev 0)
                 (quasilist* p (quasi q lev))
                 (quasicons
                   (quasicons (app 'quote 'unquote) (quasi p (- lev 1)))
                   (quasi q lev)))]
            [((unquote-splicing p ...) . q)
             (if (= lev 0)
                 (quasiappend p (quasi q lev))
                 (quasicons
                   (quasicons
                     (app 'quote 'unquote-splicing)
                     (quasi p (- lev 1)))
                   (quasi q lev)))]
            [(quasiquote p)
             (quasicons (app 'quote 'quasiquote) (quasi (list p) (+ lev 1)))]
            [(p . q) (quasicons (quasi p lev) (quasi q lev))]
            [#(x ...) (quasivector (vquasi x lev))]
            [p (app 'quote p)])))
      (lambda (x)
        (syntax-match x ()
          [(_ e) (quasi e 0)]))))
  (define define-record-macro
    (lambda (e)
      (define enumerate
        (lambda (ls)
          (let f ([i 0] [ls ls])
            (cond
              [(null? ls) '()]
              [else (cons i (f (add1 i) (cdr ls)))]))))
      (define mkid
        (lambda (id str)
          (datum->stx id (string->symbol str))))
      (syntax-match e ()
        [(_ name (field* ...))
         (let* ([namestr (symbol->string (id->sym name))]
                [fields (map id->sym field*)]
                [fieldstr* (map symbol->string fields)]
                [rtd (datum->stx name (make-record-type namestr fields))]
                [constr (mkid name (format "make-~a" namestr))]
                [pred (mkid name (format "~a?" namestr))]
                [i* (enumerate field*)]
                [getters
                 (map (lambda (x)
                        (mkid name (format "~a-~a" namestr x)))
                      fieldstr*)]
                [setters
                 (map (lambda (x)
                        (mkid name (format "set-~a-~a!" namestr x)))
                      fieldstr*)])
           (bless
             `(begin
                (define-syntax ,name (cons '$rtd ',rtd))
                (define ,constr 
                  (lambda ,field*
                    ($record ',rtd ,@field*)))
                (define ,pred
                  (lambda (x) ($record/rtd? x ',rtd)))
                ,@(map (lambda (getter i)
                          `(define ,getter
                             (lambda (x)
                               (if ($record/rtd? x ',rtd) 
                                   ($record-ref x ,i)
                                   (error ',getter
                                          "~s is not a record of type ~s"
                                          x ',rtd)))))
                       getters i*)
                ,@(map (lambda (setter i)
                          `(define ,setter
                             (lambda (x v)
                               (if ($record/rtd? x ',rtd) 
                                   ($record-set! x ,i v)
                                   (error ',setter
                                          "~s is not a record of type ~s"
                                          x ',rtd)))))
                       setters i*))))])))
  (define parameterize-transformer ;;; go away
    (lambda (e r mr)
      (syntax-match e ()
        [(_ () b b* ...) 
         (chi-internal (cons b b*) r mr)]
        [(_ ([olhs* orhs*] ...) b b* ...)
         (let ([lhs* (map (lambda (x) (gen-lexical 'lhs)) olhs*)]
               [rhs* (map (lambda (x) (gen-lexical 'rhs)) olhs*)]
               [t*   (map (lambda (x) (gen-lexical 't)) olhs*)]
               [swap (gen-lexical 'swap)])
           (build-let no-source 
             (append lhs* rhs*)
             (append (chi-expr* olhs* r mr) (chi-expr* orhs* r mr))
             (build-let no-source 
               (list swap)
               (list (build-lambda no-source '() 
                       (build-sequence no-source 
                         (map (lambda (t lhs rhs) 
                                (build-let no-source 
                                  (list t) 
                                  (list (build-application no-source
                                          (build-lexical-reference no-source lhs)
                                          '()))
                                  (build-sequence no-source 
                                    (list (build-application no-source 
                                            (build-lexical-reference no-source lhs)
                                            (list (build-lexical-reference no-source rhs)))
                                          (build-lexical-assignment no-source rhs 
                                            (build-lexical-reference no-source t))))))
                              t* lhs* rhs*))))
               (build-application no-source
                 (build-primref no-source 'dynamic-wind)
                 (list (build-lexical-reference no-source swap)
                       (build-lambda no-source '() 
                         (chi-internal (cons b b*) r mr))
                       (build-lexical-reference no-source swap))))))])))
  (define foreign-call-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ name arg* ...)
         (build-foreign-call no-source
           (chi-expr name r mr)
           (chi-expr* arg* r mr))])))
  ;; p in pattern:                        matches:
  ;;   ()                                 empty list
  ;;   _                                  anything (no binding created)
  ;;   any                                anything
  ;;   (p1 . p2)                          pair
  ;;   #(free-id <key>)                   <key> with free-identifier=?
  ;;   each-any                           any proper list
  ;;   #(each p)                          (p*)
  ;;   #(each+ p1 (p2_1 ... p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
  ;;   #(vector p)                        #(x ...) if p matches (x ...)
  ;;   #(atom <object>)                   <object> with "equal?"
  (define convert-pattern
   ; returns syntax-dispatch pattern & ids
    (lambda (pattern keys)
      (define cvt*
        (lambda (p* n ids)
          (if (null? p*)
              (values '() ids)
              (let-values (((y ids) (cvt* (cdr p*) n ids)))
                (let-values (((x ids) (cvt (car p*) n ids)))
                  (values (cons x y) ids))))))
      (define id-dots?
        (lambda (x) 
          (and (syntax-pair? x)
               (let ([d (syntax-cdr x)])
                 (and (syntax-pair? d) 
                      (syntax-null? (syntax-cdr d))
                      (ellipsis? (syntax-car d)))))))
      (define id-dots-id
        (lambda (x) (syntax-car x)))
      (define syntax-foo?
        (lambda (x)
          (and (syntax-pair? x)
               (let ((d (syntax-cdr x)))
                 (and (syntax-pair? d)
                      (ellipsis? (syntax-car d)))))))
      (define syntax-foo-z
        (lambda (x)
          (let f ([x (syntax-cdr (syntax-cdr x))])
            (cond
              ((syntax-pair? x) (f (syntax-cdr x)))
              (else x)))))
      (define syntax-foo-ys
        (lambda (x)
          (let f ([x (syntax-cdr (syntax-cdr x))])
            (cond
              [(syntax-pair? x)
               (cons (syntax-car x) (f (syntax-cdr x)))]
              [else '()]))))
      (define syntax-foo-x
        (lambda (x) (syntax-car x)))
      (define cvt
        (lambda (p n ids)
          (cond
            [(not (id? p))
             (cond
               [(id-dots? p)
                (let-values ([(p ids) (cvt (id-dots-id p) (+ n 1) ids)])
                  (values
                    (if (eq? p 'any) 'each-any (vector 'each p))
                    ids))]
               [(syntax-foo? p) ; (x dots y ... . z)
                (let-values ([(z ids) (cvt (syntax-foo-z p) n ids)])
                  (let-values ([(y ids) (cvt* (syntax-foo-ys p) n ids)])
                    (let-values ([(x ids) (cvt (syntax-foo-x p) (+ n 1) ids)])
                      (values (vector 'each+ x (reverse y) z) ids))))]
               [(syntax-pair? p)
                (let-values ([(y ids) (cvt (syntax-cdr p) n ids)])
                  (let-values ([(x ids) (cvt (syntax-car p) n ids)])
                    (values (cons x y) ids)))]
               [(syntax-null? p) (values '() ids)]
               [(syntax-vector? p)
                (let-values ([(p ids) (cvt (syntax-vector->list p) n ids)])
                  (values (vector 'vector p) ids))]
               [else (values (vector 'atom (strip p '())) ids)])]
            [(bound-id-member? p keys)
             (values (vector 'free-id p) ids)]
            [(free-id=? p (scheme-stx '_))
             (values '_ ids)]
            [else (values 'any (cons (cons p n) ids))])))
      (cvt pattern 0 '())))
  (define syntax-dispatch
    (lambda (e p)
      (define match-each
        (lambda (e p m* s*)
          (cond
            ((pair? e)
             (let ((first (match (car e) p m* s* '())))
               (and first
                    (let ((rest (match-each (cdr e) p m* s*)))
                      (and rest (cons first rest))))))
            ((null? e) '())
            ((stx? e)
             (let-values (((m* s*) (join-wraps m* s* e)))
               (match-each (stx-expr e) p m* s*)))
            (else #f))))
      (define match-each+
        (lambda (e x-pat y-pat z-pat m* s* r)
          (let f ((e e) (m* m*) (s* s*))
            (cond
              ((pair? e)
               (let-values (((xr* y-pat r) (f (cdr e) m* s*)))
                 (if r
                     (if (null? y-pat)
                         (let ((xr (match (car e) x-pat m* s* '())))
                           (if xr
                               (values (cons xr xr*) y-pat r)
                               (values #f #f #f)))
                         (values
                           '()
                           (cdr y-pat)
                           (match (car e) (car y-pat) m* s* r)))
                     (values #f #f #f))))
              ((stx? e)
               (let-values (((m* s*) (join-wraps m* s* e)))
                 (f (stx-expr e) m* s*)))
              (else (values '() y-pat (match e z-pat m* s* r)))))))
      (define match-each-any
        (lambda (e m* s*)
          (cond
            ((pair? e)
             (let ((l (match-each-any (cdr e) m* s*)))
               (and l (cons (stx (car e) m* s*) l))))
            ((null? e) '())
            ((stx? e)
             (let-values (((m* s*) (join-wraps m* s* e)))
               (match-each-any (stx-expr e) m* s*)))
            (else #f))))
      (define match-empty
        (lambda (p r)
          (cond
            ((null? p) r)
            ((eq? p '_) r)
            ((eq? p 'any) (cons '() r))
            ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
            ((eq? p 'each-any) (cons '() r))
            (else
             (case (vector-ref p 0)
               ((each) (match-empty (vector-ref p 1) r))
               ((each+)
                (match-empty
                  (vector-ref p 1)
                  (match-empty
                    (reverse (vector-ref p 2))
                    (match-empty (vector-ref p 3) r))))
               ((free-id atom) r)
               ((vector) (match-empty (vector-ref p 1) r))
               (else (error 'syntax-dispatch "invalid pattern" p)))))))
      (define combine
        (lambda (r* r)
          (if (null? (car r*))
              r
              (cons (map car r*) (combine (map cdr r*) r)))))
      (define match*
        (lambda (e p m* s* r)
          (cond
            ((null? p) (and (null? e) r))
            ((pair? p)
             (and (pair? e)
                  (match (car e) (car p) m* s*
                    (match (cdr e) (cdr p) m* s* r))))
            ((eq? p 'each-any)
             (let ((l (match-each-any e m* s*))) (and l (cons l r))))
            (else
             (case (vector-ref p 0)
               ((each)
                (if (null? e)
                    (match-empty (vector-ref p 1) r)
                    (let ((r* (match-each e (vector-ref p 1) m* s*)))
                      (and r* (combine r* r)))))
               ((free-id)
                (and (symbol? e)
                     (free-id=? (stx e m* s*) (vector-ref p 1))
                     r))
               ((each+)
                (let-values (((xr* y-pat r)
                              (match-each+ e (vector-ref p 1)
                                (vector-ref p 2) (vector-ref p 3) m* s* r)))
                  (and r
                       (null? y-pat)
                       (if (null? xr*)
                           (match-empty (vector-ref p 1) r)
                           (combine xr* r)))))
               ((atom) (and (equal? (vector-ref p 1) (strip e m*)) r))
               ((vector)
                (and (vector? e)
                     (match (vector->list e) (vector-ref p 1) m* s* r)))
               (else (error 'syntax-dispatch "invalid pattern" p)))))))
      (define match
        (lambda (e p m* s* r)
          (cond
            ((not r) #f)
            ((eq? p '_) r)
            ((eq? p 'any) (cons (stx e m* s*) r))
            ((stx? e)
             (let-values (((m* s*) (join-wraps m* s* e)))
               (match (stx-expr e) p m* s* r)))
            (else (match* e p m* s* r)))))
      (match e p '() '() '())))
  (define ellipsis?
    (lambda (x) 
      (and (id? x) (free-id=? x (scheme-stx '...)))))
  (define syntax-case-transformer
    (let ()
      (define build-dispatch-call
        (lambda (pvars expr y r mr)
          (let ([ids (map car pvars)] 
                [levels (map cdr pvars)])
            (let ([labels (map gen-label ids)]
                  [new-vars (map gen-lexical ids)])
              (let ([body
                     (chi-expr
                       (add-subst (make-full-rib ids labels) expr)
                       (extend-env*
                         labels
                         (map (lambda (var level)
                                (make-binding 'syntax (cons var level)))
                              new-vars
                              (map cdr pvars))
                         r)
                       mr)])
                (build-application no-source
                  (build-primref no-source 'apply)
                  (list (build-lambda no-source new-vars body) y)))))))
      (define invalid-ids-error
        (lambda (id* e class)
          (let find ((id* id*) (ok* '()))
            (if (null? id*)
                (stx-error e) ; shouldn't happen
                (if (id? (car id*))
                    (if (bound-id-member? (car id*) ok*)
                        (syntax-error (car id*) "duplicate " class)
                        (find (cdr id*) (cons (car id*) ok*)))
                    (syntax-error (car id*) "invalid " class))))))
      (define gen-clause
        (lambda (x keys clauses r mr pat fender expr)
          (let-values (((p pvars) (convert-pattern pat keys)))
            (cond
              ((not (distinct-bound-ids? (map car pvars)))
               (invalid-ids-error (map car pvars) pat "pattern variable"))
              ((not (andmap (lambda (x) (not (ellipsis? (car x)))) pvars))
               (stx-error pat "3misplaced ellipsis in syntax-case pattern"))
              (else
               (let ((y (gen-lexical 'tmp)))
                 (let ([test 
                        (cond
                          [(eq? fender #t) y]
                          [else
                           (let ([call
                                  (build-dispatch-call
                                     pvars fender y r mr)])
                             (build-conditional no-source
                                (build-lexical-reference no-source y)
                                call
                                (build-data no-source #f)))])])
                    (let ([conseq
                           (build-dispatch-call pvars expr
                             (build-lexical-reference no-source y)
                             r mr)])
                      (let ([altern
                             (gen-syntax-case x keys clauses r mr)])
                        (build-application no-source
                          (build-lambda no-source (list y) 
                            (build-conditional no-source test conseq altern))
                          (list
                            (build-application no-source
                              (build-primref no-source 'syntax-dispatch)
                              (list
                                (build-lexical-reference no-source x)
                                (build-data no-source p))))))))))))))
      (define gen-syntax-case
        (lambda (x keys clauses r mr)
          (if (null? clauses)
              (build-application no-source
                (build-primref no-source 'syntax-error)
                (list (build-lexical-reference no-source x)))
              (syntax-match (car clauses) ()
                [(pat expr)
                 (if (and (id? pat)
                          (not (bound-id-member? pat keys))
                          (not (ellipsis? pat)))
                     (if (free-id=? pat (scheme-stx '_))
                         (chi-expr expr r mr)
                         (let ([lab (gen-label pat)]
                               [lex (gen-lexical pat)])
                           (let ([body
                                  (chi-expr
                                    (add-subst
                                       (make-full-rib (list pat) (list lab))
                                       expr)
                                     (extend-env lab 
                                       (make-binding 'syntax (cons lex 0))
                                       r)
                                     mr)])
                              (build-application no-source
                                (build-lambda no-source (list lex) body)
                                (list (build-lexical-reference no-source x))))))
                     (gen-clause x keys (cdr clauses) r mr pat #t expr))]
                [(pat fender expr)
                 (gen-clause x keys (cdr clauses) r mr pat fender expr)]))))
      (lambda (e r mr)
        (syntax-match e ()
          [(_ expr (keys ...) clauses ...)
           (unless (andmap (lambda (x) (and (id? x) (not (ellipsis?  x)))) keys)
             (stx-error e))
           (let ((x (gen-lexical 'tmp)))
             (let ([body (gen-syntax-case x keys clauses r mr)])
               (build-application no-source
                 (build-lambda no-source (list x) body)
                 (list (chi-expr expr r mr)))))]))))
    (define syntax-transformer
      (let ()
        (define match2
          (lambda (e f? sk fk)
            (if (syntax-list? e)
                (let ((e (syntax->list e)))
                  (if (= (length e) 2)
                      (let ((e0 (car e)) (e1 (cadr e)))
                        (if (or (eq? f? #t) (f? e0 e1))
                            (sk e0 e1)
                            (fk)))
                      (fk)))
                (fk))))
        (define gen-syntax
          (lambda (src e r maps ellipsis? vec?)
            (if (id? e)
                (let ((label (id->label e)))
                  (let ((b (label->binding label r)))
                    (if (eq? (binding-type b) 'syntax)
                        (let-values (((var maps)
                                      (let ((var.lev (binding-value b)))
                                        (gen-ref src (car var.lev) (cdr var.lev) maps))))
                          (values (list 'ref var) maps))
                        (if (ellipsis? e)
                            (syntax-error src "1misplaced ellipsis in syntax form")
                            (begin
                              (values (list 'quote e) maps))))))
                (match2 e (lambda (dots e) (ellipsis? dots))
                  (lambda (dots e)
                    (if vec?
                       (syntax-error src "2misplaced ellipsis in syntax form")
                       (gen-syntax src e r maps (lambda (x) #f) #f)))
                  (lambda ()
                    (cond
                      ((and (syntax-pair? e)   ;(x dots . y)
                            (let ((t (syntax-cdr e)))
                              (and (syntax-pair? t)
                                   (ellipsis? (syntax-car t)))))
                       (let f ((y (syntax-cdr (syntax-cdr e)))
                               (k (lambda (maps)
                                    (let-values (((x maps)
                                                  (gen-syntax src (syntax-car e) r 
                                                    (cons '() maps) ellipsis? #f)))
                                      (if (null? (car maps))
                                          (syntax-error src
                                            "extra ellipsis in syntax form")
                                          (values (gen-map x (car maps)) (cdr maps)))))))
                         (cond
                           ((syntax-null? y) (k maps))
                           ((and (syntax-pair? y) (ellipsis? (syntax-car y)))
                            ; (dots . y)
                            (f (syntax-cdr y)
                               (lambda (maps)
                                 (let-values (((x maps) (k (cons '() maps))))
                                   (if (null? (car maps))
                                       (syntax-error src "extra ellipsis in syntax form")
                                       (values (gen-mappend x (car maps)) (cdr maps)))))))
                           (else 
                            (let-values (((y maps) 
                                          (gen-syntax src y r maps ellipsis? vec?)))
                              (let-values (((x maps) (k maps)))
                                (values (gen-append x y) maps)))))))
                      ((syntax-pair? e) ;(x . y)
                       (let-values (((xnew maps)
                                     (gen-syntax src (syntax-car e) r 
                                                 maps ellipsis? #f)))
                         (let-values (((ynew maps)
                                       (gen-syntax src (syntax-cdr e) r 
                                                   maps ellipsis? vec?)))
                           (values (gen-cons e (syntax-car e) (syntax-cdr e) xnew ynew)
                                   maps))))
                      ((syntax-vector? e) ;#(x1 x2 ...)
                       (let ((ls (syntax-vector->list e)))
                         (let-values (((lsnew maps)
                                       (gen-syntax src ls r maps ellipsis? #t)))
                           (values (gen-vector e ls lsnew) maps))))
                      ((and (syntax-null? e) vec?) (values '(quote ()) maps))
                      (else (values `(quote ,e) maps))))))))
        (define gen-ref
          (lambda (src var level maps)
            (if (= level 0)
                (values var maps)
                (if (null? maps)
                    (syntax-error src "missing ellipsis in syntax form")
                    (let-values (((outer-var outer-maps)
                                  (gen-ref src var (- level 1) (cdr maps))))
                      (cond
                        ((assq outer-var (car maps)) =>
                         (lambda (b) (values (cdr b) maps)))
                        (else
                         (let ((inner-var (gen-lexical 'tmp)))
                           (values
                             inner-var
                             (cons
                               (cons (cons outer-var inner-var) (car maps))
                               outer-maps))))))))))
        (define gen-append
          (lambda (x y)
            (if (equal? y '(quote ())) x (list 'append x y))))
        (define gen-mappend
          (lambda (e map-env)
            (list 'apply '(primitive append) (gen-map e map-env))))
        (define gen-map
          (lambda (e map-env)
            (let ((formals (map cdr map-env))
                  (actuals (map (lambda (x) (list 'ref (car x))) map-env)))
              (cond
               ; identity map equivalence:
               ; (map (lambda (x) x) y) == y
                ((eq? (car e) 'ref)
                 (car actuals))
               ; eta map equivalence:
               ; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
                ((andmap
                   (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                   (cdr e))
                 (list* 'map (list 'primitive (car e))
                       (map (let ((r (map cons formals actuals)))
                              (lambda (x) (cdr (assq (cadr x) r))))
                            (cdr e))))
                (else (list* 'map (list 'lambda formals e) actuals))))))
        (define gen-cons
          (lambda (e x y xnew ynew)
            (case (car ynew)
              ((quote)
               (if (eq? (car xnew) 'quote)
                   (let ((xnew (cadr xnew)) (ynew (cadr ynew)))
                     (if (and (eq? xnew x) (eq? ynew y))
                         (list 'quote e)
                         (list 'quote (cons xnew ynew))))
                   (if (eq? (cadr ynew) '())
                       (list 'list xnew)
                       (list 'cons xnew ynew))))
              ((list) (list* 'list xnew (cdr ynew)))
              (else (list 'cons xnew ynew)))))
        (define gen-vector
          (lambda (e ls lsnew)
            (cond
              ((eq? (car lsnew) 'quote)
               (if (eq? (cadr lsnew) ls)
                   (list 'quote e)
                   (list 'quote (list->vector (cadr lsnew)))))
                   ;`(quote #(,@(cadr lsnew)))))
              ((eq? (car lsnew) 'list) 
               (cons 'vector (cdr lsnew)))
              (else (list 'list->vector lsnew)))))
        (define regen
          (lambda (x)
            (case (car x)
              ((ref) (build-lexical-reference no-source (cadr x)))
              ((primitive) (build-primref no-source (cadr x)))
              ((quote) (build-data no-source (cadr x)))
              ((lambda) (build-lambda no-source (cadr x) (regen (caddr x))))
              ((map)
               (let ((ls (map regen (cdr x))))
                 (build-application no-source
                   (build-primref no-source 'map)
                   ls)))
              (else
               (build-application no-source
                 (build-primref no-source (car x))
                 (map regen (cdr x)))))))
        (lambda (e r mr)
          (match2 e #t
            (lambda (_ x)
              (let-values (((e maps) (gen-syntax e x r '() ellipsis? #f)))
                (regen e)))
            (lambda () (syntax-error e))))))
  (define core-macro-transformer
    (lambda (name)
      (case name
        [(quote)         quote-transformer]
        [(lambda)        lambda-transformer]
        [(case-lambda)   case-lambda-transformer]
        [(let-values)    let-values-transformer]
        [(letrec)        letrec-transformer]
        [(case)          case-transformer]
        [(if)            if-transformer]
        [(when)          when-transformer]
        [(unless)        unless-transformer]
        [(parameterize)  parameterize-transformer]
        [(foreign-call)  foreign-call-transformer]
        [(syntax-case)   syntax-case-transformer]
        [(syntax)        syntax-transformer]
        [(type-descriptor) type-descriptor-transformer]
        [else (error 'macro-transformer "cannot find ~s" name)])))
  (define macro-transformer
    (lambda (x)
      (cond
        [(procedure? x) x]
        [(symbol? x)
         (case x
           [(define-record) define-record-macro]
           [(include)       include-macro]
           [(cond)          cond-macro]
           [(let)           let-macro]
           [(or)            or-macro]
           [(and)           and-macro]
           [(let*)          let*-macro]
           [(syntax-rules)  syntax-rules-macro]
           [(quasiquote)    quasiquote-macro]
           [(with-syntax)   with-syntax-macro]
           [else (error 'macro-transformer "invalid macro ~s" x)])]
        [else (error 'core-macro-transformer "invalid macro ~s" x)])))
  ;;; chi procedures
  (define chi-macro 
    (lambda (p e)
      (let ([s ((macro-transformer p) (add-mark anti-mark e))])
        (add-mark (gen-mark) s))))
  (define chi-expr*
    (lambda (e* r mr)
      ;;; expand left to right
      (cond
        [(null? e*) '()]
        [else
         (let ([e (chi-expr (car e*) r mr)])
           (cons e (chi-expr* (cdr e*) r mr)))])))
  (define chi-application
    (lambda (e r mr)
      (syntax-match e  ()
        [(rator rands ...) 
         (let ([rator (chi-expr rator r mr)])
           (build-application no-source 
             rator
             (chi-expr* rands r mr)))])))
  (define chi-expr
    (lambda (e r mr)
      (let-values ([(type value kwd) (syntax-type e r)])
        (case type
          [(core-macro)
           (let ([transformer (core-macro-transformer value)])
             (transformer e r mr))]
          [(core-prim) 
           (let ([name value])
             (build-primref no-source name))]
          [(call) (chi-application e r mr)]
          [(lexical)
           (let ([lex value])
             (build-lexical-reference no-source lex))]
          [(macro) (chi-expr (chi-macro value e) r mr)]
          [(constant)
           (let ([datum value])
             (build-data no-source datum))]
          [(set!) (chi-set! e r mr)]
          [(begin) 
           (syntax-match e ()
             [(_ x x* ...)
              (build-sequence no-source
                (chi-expr* (cons x x*) r mr))])]
          [else (error 'chi-expr "invalid type ~s for ~s" type
                       (strip e '())) (stx-error e)]))))
  (define chi-set!
    (lambda (e r mr)
      (syntax-match e ()
        [(_ x v)
         (if (id? x)
             (let-values ([(type value kwd) (syntax-type x r)])
               (case type
                 [(lexical) 
                  (build-lexical-assignment no-source 
                    value 
                    (chi-expr v r mr))]
                 [else (stx-error e)]))
             (stx-error e))])))
  (define chi-lambda-clause
    (lambda (fmls body* r mr)
      (syntax-match fmls ()
        [(x* ...) 
         (if (valid-bound-ids? x*) 
             (let ([lex* (map gen-lexical x*)]
                   [lab* (map gen-label x*)])
               (values
                 lex*
                 (chi-internal 
                   (add-subst 
                     (make-full-rib x* lab*)
                     body*)
                   (add-lexicals lab* lex* r)
                   mr)))
             (stx-error fmls "invalid fmls"))]
        [(x* ... . x)
         (if (valid-bound-ids? (cons x x*)) 
             (let ([lex* (map gen-lexical x*)]
                   [lab* (map gen-label x*)]
                   [lex (gen-lexical x)]
                   [lab (gen-label x)])
               (values
                 (append lex* lex)
                 (chi-internal 
                   (add-subst 
                     (make-full-rib (cons x x*) (cons lab lab*))
                     body*)
                   (add-lexicals (cons lab lab*)
                                 (cons lex lex*)
                                 r)
                   mr)))
             (stx-error fmls "invalid fmls"))]
        [_ (stx-error fmls "invalid fmls")])))
  (define chi-lambda-clause*
    (lambda (fmls* body** r mr)
      (cond
        [(null? fmls*) (values '() '())]
        [else
         (let-values ([(a b)
                       (chi-lambda-clause (car fmls*) (car body**) r mr)])
           (let-values ([(a* b*)
                         (chi-lambda-clause* (cdr fmls*) (cdr body**) r mr)])
             (values (cons a a*) (cons b b*))))])))
  (define chi-rhs*
    (lambda (rhs* r mr)
      (define chi-rhs
        (lambda (rhs) 
          (case (car rhs)
            [(defun) 
             (let ([x (cdr rhs)])
               (let ([fmls (car x)] [body* (cdr x)])
                 (let-values ([(fmls body) 
                               (chi-lambda-clause fmls body* r mr)])
                   (build-lambda no-source fmls body))))]
            [(expr) 
             (let ([expr (cdr rhs)])
               (chi-expr expr r mr))]
            [else (error 'chi-rhs "invalid rhs ~s" rhs)])))
      (let f ([ls rhs*])
        (cond ;;; chi in order
          [(null? ls) '()]
          [else
           (let ([a (chi-rhs (car ls))])
             (cons a (f (cdr ls))))]))))
  (define find-bound=?
    (lambda (x lhs* rhs*)
      (cond
        [(null? lhs*) #f]
        [(bound-id=? x (car lhs*)) (car rhs*)]
        [else (find-bound=? x (cdr lhs*) (cdr rhs*))])))
  (define (find-dups ls)
    (let f ([ls ls] [dups '()])
      (cond
        [(null? ls) dups]
        [(find-bound=? (car ls) (cdr ls) (cdr ls)) =>
         (lambda (x) (f (cdr ls) (cons (list (car ls) x) dups)))]
        [else (f (cdr ls) dups)])))
  (define chi-internal
    (lambda (e* r mr)
      (define return
        (lambda (init* module-init** r mr lhs* lex* rhs*)
          (let ([mod-init* (apply append (reverse module-init**))])
            (unless (valid-bound-ids? lhs*)
              (stx-error (find-dups lhs*) "multiple definitions in internal"))
            (let ([rhs* (chi-rhs* rhs* r mr)]
                  [init* (chi-expr* (append mod-init* init*) r mr)])
              (build-letrec no-source 
                 (reverse lex*) (reverse rhs*) 
                 (build-sequence no-source init*))))))
      (let* ([rib (make-empty-rib)]
             [e* (map (lambda (x) (add-subst rib x)) 
                      (syntax->list e*))])
        (let f ([e* e*] [module-init** '()] [r r] [mr r] [lhs* '()] [lex* '()] [rhs* '()] [kwd* '()])
          (cond
            [(null? e*) (error 'chi-internal "empty body")]
            [else
             (let ([e (car e*)])
               (let-values ([(type value kwd) (syntax-type e r)])
                 (let ([kwd* (cons-id kwd kwd*)])
                   (case type
                     [(define) 
                      (let-values ([(id rhs) (parse-define e)])
                        (when (bound-id-member? id kwd*)
                          (stx-error id "undefined identifier"))
                        (let ([lex (gen-lexical id)]
                              [lab  (gen-label id)])
                          (extend-rib! rib id lab)
                          (f (cdr e*)
                             module-init**
                             (cons (cons lab (cons 'lexical lex)) r)
                             mr 
                             (cons id lhs*)
                             (cons lex lex*)
                             (cons rhs rhs*)
                             kwd*)))]
                     [(define-syntax)
                      (let-values ([(id rhs) (parse-define-syntax e)])
                        (when (bound-id-member? id kwd*)
                          (syntax-error id "undefined identifier"))
                        (let ([lab (gen-label id)])
                          (let ([expanded-rhs (chi-expr rhs mr mr)])
                            (extend-rib! rib id lab)
                            (let ([b (make-eval-transformer expanded-rhs)])
                              (f (cdr e*)
                                 module-init**
                                 (cons (cons lab b) r)
                                 (cons (cons lab b) mr) 
                                 (cons id lhs*) lex* rhs* kwd*)))))]
                     [(begin)
                      (syntax-match e ()
                        [(_ x* ...)
                         (f (append x* (cdr e*)) module-init** 
                            r mr lhs* lex* rhs* kwd*)])]
                     [(macro)
                      (f (cons (add-subst rib (chi-macro value e)) (cdr e*))
                         module-init** r mr lhs* lex* rhs* kwd*)]
                     [(module)
                      (let-values ([(m-lhs* m-lex* m-rhs* m-init* m-exp-id* m-exp-lab* r mr kwd*)
                                    (chi-internal-module e r mr kwd*)])
                        (for-each
                          (lambda (id lab) (extend-rib! rib id lab))
                          m-exp-id* m-exp-lab*)
                        (f (cdr e*)
                           (cons m-init* module-init**)
                           r mr 
                           (append m-exp-id* lhs*)
                           (append m-lex* lex*)
                           (append m-rhs* rhs*)
                           kwd*))]
                     [else 
                      (return e* module-init** r mr lhs* lex* rhs*)]))))])))))
  (define chi-internal-module
    (lambda (e r mr kwd*)
      (define parse-module
        (lambda (e)
          (syntax-match e ()
            [(_ (export* ...) b* ...)
             (unless (andmap id? export*) (stx-error e))
             (values #f export* b*)]
            [(_ name (export* ...) b* ...)
             (unless (and (id? name) (andmap id? export*)) (stx-error e))
             (values name export* b*)])))
      (let-values ([(name exp-id* e*) (parse-module e)])
        (let* ([rib (make-empty-rib)]
               [e* (map (lambda (x) (add-subst rib x))
                        (syntax->list e*))])
          (define return
            (lambda (init* r mr lhs* lex* rhs* kwd*)
              (unless (valid-bound-ids? lhs*)
                (stx-error (find-dups lhs*) "multiple definitions in module"))
              (let ([exp-lab* 
                     (map (lambda (x) 
                             (or (id->label (add-subst rib x))
                                 (stx-error x "cannot find export")))
                          exp-id*)])
                (if (not name) ;;; explicit export
                    (values lhs* lex* rhs* init* exp-id* exp-lab* r mr kwd*)
                    (let ([lab (gen-label 'module)]
                          [iface (cons exp-id* exp-lab*)])
                      (values lhs* lex* rhs* init* 
                              (list name) ;;; FIXME: module cannot
                              (list lab)  ;;;  export itself yet
                              (cons (cons lab (cons '$module iface)) r)
                              (cons (cons lab (cons '$module iface)) mr)
                              kwd*))))))
          (let f ([e* e*] [r r] [mr mr] [lhs* '()] [lex* '()] [rhs* '()] [kwd* kwd*])
            (cond
              [(null? e*) (return '() r mr lhs* lex* rhs* kwd*)]
              [else 
               (let ([e (car e*)])
                 (let-values ([(type value kwd) (syntax-type e r)])
                   (let ([kwd* (cons-id kwd kwd*)])
                     (case type
                       [(define) 
                        (let-values ([(id rhs) (parse-define e)])
                          (when (bound-id-member? id kwd*) 
                            (stx-error id "undefined identifier"))
                          (let ([lex (gen-lexical id)]
                                [lab (gen-label id)])
                            (extend-rib! rib id lab)
                            (f (cdr e*)
                               (cons (cons lab (cons 'lexical lex)) r)
                               mr 
                               (cons id lhs*)
                               (cons lex lex*)
                               (cons rhs rhs*)
                               kwd*)))]
                       [(define-syntax)
                        (let-values ([(id rhs) (parse-define-syntax e)])
                          (when (bound-id-member? id kwd*)
                            (syntax-error id "undefined identifier"))
                          (let ([lab (gen-label id)])
                            (let ([expanded-rhs (chi-expr rhs mr mr)])
                              (extend-rib! rib id lab)
                              (let ([b (make-eval-transformer expanded-rhs)])
                                (f (cdr e*)
                                   (cons (cons lab b) r)
                                   (cons (cons lab b) mr) 
                                   (cons id lhs*)
                                   lex* rhs* kwd*)))))]
                       [(begin)
                        (syntax-match e ()
                          [(_ x* ...)
                           (f (append x* (cdr e*)) r mr lhs* lex* rhs* kwd*)])]
                       [(macro)
                        (f (cons (add-subst rib (chi-macro value e)) (cdr e*))
                           r mr lhs* lex* rhs* kwd*)]
                       [else (return e* r mr lhs* lex* rhs* kwd*)]))))]))))))
  (define chi-library-internal 
    (lambda (e* r rib kwd*)
      (define return
        (lambda (init* module-init** r mr lhs* lex* rhs*)
          (let ([module-init* (apply append (reverse module-init**))])
            (values (append module-init* init*)
              r mr (reverse lhs*) (reverse lex*) (reverse rhs*)))))
      (let f ([e* e*] [module-init** '()] [r r] [mr r]
              [lhs* '()] [lex* '()] [rhs* '()] [kwd* kwd*])
        (cond
          [(null? e*) (return e* module-init** r mr lhs* lex* rhs*)]
          [else
           (let ([e (car e*)])
             ;(printf "chi ~s\n" e)
             (let-values ([(type value kwd) (syntax-type e r)])
               (let ([kwd* (cons-id kwd kwd*)])
                 (case type
                   [(define) 
                    (let-values ([(id rhs) (parse-define e)])
                      (when (bound-id-member? id kwd*) 
                        (stx-error id "cannt redefine identifier"))
                      (when (bound-id-member? id lhs*) 
                        (stx-error id "multiple definition"))
                      (let ([lex (gen-lexical id)]
                            [lab (gen-label id)])
                        (extend-rib! rib id lab)
                        (f (cdr e*)
                           module-init**
                           (cons (cons lab (cons 'lexical lex)) r)
                           mr 
                           (cons id lhs*) (cons lex lex*) (cons rhs rhs*)
                           kwd*)))]
                   [(define-syntax)
                    (let-values ([(id rhs) (parse-define-syntax e)])
                      (when (bound-id-member? id kwd*) 
                        (syntax-error id "undefined identifier"))
                      (let ([lab (gen-label id)])
                        (let ([expanded-rhs (chi-expr rhs mr mr)])
                          (extend-rib! rib id lab)
                          (let ([b (make-eval-transformer expanded-rhs)])
                            (f (cdr e*) 
                               module-init**
                               (cons (cons lab b) r)
                               (cons (cons lab b) mr) 
                               (cons id lhs*) lex* rhs* kwd*)))))] 
                   [(module)
                    (let-values ([(m-lhs* m-lex* m-rhs* m-init* m-exp-id* m-exp-lab* r mr kwd*)
                                  (chi-internal-module e r mr kwd*)])
                      (for-each
                        (lambda (id lab) (extend-rib! rib id lab))
                        m-exp-id* m-exp-lab*)
                      (f (cdr e*)
                         (cons m-init* module-init**)
                         r mr 
                         (append m-exp-id* lhs*)
                         (append m-lex* lex*)
                         (append m-rhs* rhs*)
                         kwd*))]
                   [(begin)
                    (syntax-match e ()
                      [(_ x* ...)
                       (f (append x* (cdr e*)) module-init** r mr lhs* lex* rhs*
                          kwd*)])]
                   [(macro)
                    (f (cons (add-subst rib (chi-macro value e)) (cdr e*))
                       module-init**
                       r mr lhs* lex* rhs* kwd*)]
                   [else 
                    (return e* module-init** r mr lhs* lex* rhs*)]))))]))))
  (define library-expander^
    (lambda (e)
      (let-values ([(name exp* b*) (parse-library e)])
        (let ([rib (make-scheme-rib)]
              [r (make-scheme-env)])
          (let ([b* (map (lambda (x) (stx x top-mark* (list rib))) b*)]
                [kwd* (map (lambda (sym mark*) (stx sym mark* (list rib)))
                           (rib-sym* rib) (rib-mark** rib))])
            (let-values ([(init* r mr lhs* lex* rhs*)
                          (chi-library-internal b* r rib kwd*)])
              (build-letrec no-source
                lex* 
                (chi-rhs* rhs* r mr)
                (if (null? init*) 
                    (build-void)
                    (build-sequence no-source 
                      (chi-expr* init* r mr))))))))))
  (define library-expander
    (lambda (x)
      (let ([v (library-expander^ x)])
        ;(pretty-print v)
        v)))
  (primitive-set! 'identifier? id?)
  (primitive-set! 'generate-temporaries
    (lambda (ls)
      (unless (list? ls) 
        (error 'generate-temporaries "~s is not a list"))
      (map (lambda (x) (stx (gensym 't) top-mark* '())) ls)))
  (primitive-set! 'free-identifier=? 
    (lambda (x y)
      (if (id? x)
          (if (id? y)
              (free-id=? x y)
              (error 'free-identifier=? "~s is not an identifier" y))
          (error 'free-identifier=? "~s is not an identifier" x))))
  (primitive-set! 'syntax-error
    (lambda (x . args)
      (unless (andmap string? args)
        (error 'syntax-error "invalid argument ~s" args))
      (error #f "~a: ~s" 
             (apply string-append args) 
             (strip x '()))))
  (primitive-set! 'syntax-dispatch syntax-dispatch)
  (primitive-set! 'chi-top-library library-expander))

