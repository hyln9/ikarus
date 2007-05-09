
;;; FIXME: teach the compiler about (apply append ls) since I
;;;        used it a zillion times in this file.
;;;        other primitives that are usually apply'd include the
;;;        arithmetic operations (+, =, min, max, ...).
;;;        (apply (lambda ---) ls) is also common in this file.

(library (ikarus syntax)
  (export identifier? syntax-dispatch
          generate-temporaries free-identifier=? syntax-error
          eval-r6rs-top-level boot-library-expand eval-top-level)
  (import
    (except (ikarus library-manager) installed-libraries)
    (only (ikarus compiler) eval-core)
    (only (ikarus) error)
    (chez modules)
    (only (ikarus) ormap andmap gensym fxadd1 fx= fxsub1 sub1 list*
          add1 format make-record-type symbol-value parameterize
          void make-parameter set-symbol-value!)
    (rename (r6rs)
      (free-identifier=? sys:free-identifier=?)
      (identifier? sys:identifier?)
      (syntax-error sys:syntax-error)
      ;(syntax->datum sys:syntax->datum)
      (generate-temporaries sys:generate-temporaries)))
  (define who 'expander)
  (define-syntax no-source
    (lambda (x) #f))
  (begin ;;; builders
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
        [(_ ae var exp) `(#%set-top-level-value! ',var ,exp)]))
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
        (if (null? vars) body-exp `(letrec ,(map list vars val-exps) ,body-exp))))
    (define build-letrec*
      (lambda (ae vars val-exps body-exp)
        (if (null? vars) body-exp `(letrec* ,(map list vars val-exps) ,body-exp)))))
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
  (define-record rib (sym* mark** label* sealed/freq))
  (define make-full-rib
    (lambda (id* label*)
      (make-rib (map id->sym id*) (map stx-mark* id*) label* #f)))
  (define make-empty-rib
    (lambda ()
      (make-rib '() '() '() #f)))
  (define extend-rib!
    (lambda (rib id label)
      (if (rib? rib)
          (let ([sym (id->sym id)] [mark* (stx-mark* id)])
            (when (rib-sealed/freq rib)
              (error 'extend-rib! "rib ~s is sealed" rib))
            (set-rib-sym*! rib (cons sym (rib-sym* rib)))
            (set-rib-mark**! rib (cons mark* (rib-mark** rib)))
            (set-rib-label*! rib (cons label (rib-label* rib))))
          (error 'extend-rib! "~s is not a rib" rib))))
  (define (extend-rib/check! rib id label)
    (cond
      [(rib? rib)
       (when (rib-sealed/freq rib)
         (error 'extend-rib/check! "rib ~s is sealed" rib))
       (let ([sym (id->sym id)] [mark* (stx-mark* id)])
         (let ([sym* (rib-sym* rib)])
           (when (and (memq sym (rib-sym* rib))
                      (bound-id=? id
                        (stx sym mark* (list rib))))
             (stx-error id "cannot redefine"))
           (set-rib-sym*! rib (cons sym sym*))
           (set-rib-mark**! rib (cons mark* (rib-mark** rib)))
           (set-rib-label*! rib (cons label (rib-label* rib)))))]
      [else (error 'extend-rib/check! "~s is not a rib" rib)]))
  (module (make-stx stx? stx-expr stx-mark* stx-subst*)
    (define-record stx (expr mark* subst*)))
  (define (seal-rib! rib)
    (let ([sym* (rib-sym* rib)])
      (unless (null? sym*)
        ;;; only seal if rib is not empty.
        (let ([sym* (list->vector sym*)])
          (set-rib-sym*! rib sym*)
          (set-rib-mark**! rib
            (list->vector (rib-mark** rib)))
          (set-rib-label*! rib
            (list->vector (rib-label* rib)))
          (set-rib-sealed/freq! rib
            (make-vector (vector-length sym*) 0))))))
  (define (unseal-rib! rib)
    (when (rib-sealed/freq rib)
      (set-rib-sealed/freq! rib #f)
      (set-rib-sym*! rib (vector->list (rib-sym* rib)))
      (set-rib-mark**! rib (vector->list (rib-mark** rib)))
      (set-rib-label*! rib (vector->list (rib-label* rib)))))
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
  (define (increment-rib-frequency! rib idx)
    (let ([freq* (rib-sealed/freq rib)])
      (let ([freq (vector-ref freq* idx)])
        (let ([i
               (let f ([i idx])
                 (cond
                   [(fx= i 0) 0]
                   [else
                    (let ([j (fxsub1 i)])
                      (cond
                        [(fx= freq (vector-ref freq* j)) (f j)]
                        [else i]))]))])
          (vector-set! freq* i (fxadd1 freq))
          (unless (fx= i idx)
            (let ([sym* (rib-sym* rib)]
                  [mark** (rib-mark** rib)]
                  [label* (rib-label* rib)])
              (let ([sym (vector-ref sym* idx)])
                (vector-set! sym* idx (vector-ref sym* i))
                (vector-set! sym* i sym))
              (let ([mark* (vector-ref mark** idx)])
                (vector-set! mark** idx (vector-ref mark** i))
                (vector-set! mark** i mark*))
              (let ([label (vector-ref label* idx)])
                (vector-set! label* idx (vector-ref label* i))
                (vector-set! label* i label))))))))
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
               (cond
                 [(rib-sealed/freq rib)
                  (let ([sym* (rib-sym* rib)])
                    (let f ([i 0] [n (sub1 (vector-length sym*))])
                      (cond
                        [(and (eq? (vector-ref sym* i) sym)
                              (same-marks? mark*
                                (vector-ref (rib-mark** rib) i)))
                          (let ([label (vector-ref (rib-label* rib) i)])
                            (increment-rib-frequency! rib i)
                            label)]
                        [(fx= i n) (search (cdr subst*) mark*)]
                        [else (f (fxadd1 i) n)])))]
                 [else
                  (let f ([sym* (rib-sym* rib)]
                          [mark** (rib-mark** rib)]
                          [label* (rib-label* rib)])
                    (cond
                      [(null? sym*) (search (cdr subst*) mark*)]
                      [(and (eq? (car sym*) sym)
                            (same-marks? (car mark**) mark*))
                       (car label*)]
                      [else (f (cdr sym*) (cdr mark**) (cdr label*))]))]))])))))
  (define label->binding
    (lambda (x r)
      (cond
        [(imported-label->binding x)]
        [(assq x r) => cdr]
        [else (cons 'displaced-lexical #f)])))
  (define make-binding cons)
  (define binding-type car)
  (define binding-value cdr)
  (define local-binding-value cadr)
  (define local-macro-src cddr)
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
               [(lexical core-prim macro global local-macro
                   global-macro displaced-lexical)
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
                      local-macro global-macro module set!)
                    (values type (binding-value b) id)]
                   [else
                    (values 'call #f #f)]))
               (values 'call #f #f)))]
        [else (let ([d (strip e '())])
                (if (self-evaluating? d)
                    (values 'constant d #f)
                    (values 'other #f #f)))])))
  (define-syntax stx-error
    (lambda (x)
      (syntax-case x ()
        [(_ stx) #'(error #f "invalid syntax ~s" (strip stx '()))]
        [(_ stx msg) #'(error #f "~a: ~s" msg (strip stx '()))])))
  (define sanitize-binding
    (lambda (x src)
      (cond
        [(procedure? x) (list* 'local-macro x src)]
        [(and (pair? x) (eq? (car x) 'macro!) (procedure? (cdr x)))
         (list* 'local-macro! (cdr x) src)]
        [(and (pair? x) (eq? (car x) '$rtd)) x]
        [else (error 'expand "invalid transformer ~s" x)])))
  (define make-eval-transformer
    (lambda (x)
      (sanitize-binding (eval-core x) x)))
  (define-syntax syntax-match
    (lambda (ctx)
      (define dots?
        (lambda (x)
          (and (sys:identifier? x)
               (sys:free-identifier=? x #'(... ...)))))
      (define free-identifier-member?
        (lambda (x ls)
          (and (ormap (lambda (y) (sys:free-identifier=? x y)) ls) #t)))
      (define (parse-clause lits cls)
        (define (parse-pat pat)
          (syntax-case pat ()
            [id (sys:identifier? #'id)
             (cond
               [(free-identifier-member? #'id lits)
                (values '()
                  #'(lambda (x)
                       (and (id? x)
                         (free-id=? x (scheme-stx 'id))
                         '())))]
               [(sys:free-identifier=? #'id #'_)
                (values '() #'(lambda (x) '()))]
               [else
                (values (list #'id) #'(lambda (x) (list x)))])]
            [(pat dots) (dots? #'dots)
             (let-values ([(pvars decon) (parse-pat #'pat)])
               (with-syntax ([(v* ...) pvars] [decon decon])
                 (values pvars
                   #'(letrec ([f (lambda (x)
                                   (cond
                                     [(syntax-pair? x)
                                      (let ([cars/f (decon (syntax-car x))])
                                        (and cars/f
                                          (let ([cdrs/f (f (syntax-cdr x))])
                                            (and cdrs/f
                                              (map cons cars/f cdrs/f)))))]
                                     [(syntax-null? x)
                                      (list (begin 'v* '()) ...)]
                                     [else #f]))])
                       f))))]
            [(pat dots . last) (dots? #'dots)
             (let-values ([(p1 d1) (parse-pat #'pat)]
                          [(p2 d2) (parse-pat #'last)])
               (with-syntax ([(v* ...) (append p1 p2)]
                             [(v1* ...) p1]
                             [(v2* ...) p2]
                             [d1 d1] [d2 d2])
                 (values (append p1 p2)
                   #'(letrec ([f (lambda (x)
                                   (cond
                                     [(syntax-pair? x)
                                      (let ([cars/f (d1 (syntax-car x))])
                                        (and cars/f
                                          (let ([d/f (f (syntax-cdr x))])
                                            (and d/f
                                              (cons (map cons cars/f (car d/f))
                                                    (cdr d/f))))))]
                                     [else
                                      (let ([d (d2 x)])
                                        (and d
                                          (cons (list (begin 'v1* '()) ...)
                                                d)))]))])
                       (lambda (x)
                         (let ([x (f x)])
                           (and x (append (car x) (cdr x)))))))))]
            [(pat1 . pat2)
             (let-values ([(p1 d1) (parse-pat #'pat1)]
                          [(p2 d2) (parse-pat #'pat2)])
               (with-syntax ([d1 d1] [d2 d2])
                 (values (append p1 p2)
                    #'(lambda (x)
                        (and (syntax-pair? x)
                          (let ([q (d1 (syntax-car x))])
                            (and q
                              (let ([r (d2 (syntax-cdr x))])
                                (and r (append q r))))))))))]
            [#(pats ...)
             (let-values ([(pvars d) (parse-pat #'(pats ...))])
                (with-syntax ([d d])
                  (values pvars
                    #'(lambda (x)
                        (and (syntax-vector? x)
                             (d (syntax-vector->list x)))))))]
            [datum
             (values '()
               #'(lambda (x)
                   (and (equal? (strip x '()) 'datum) '())))]))
        (syntax-case cls ()
          [(pat body)
           (let-values ([(pvars decon) (parse-pat #'pat)])
             (with-syntax ([(v* ...) pvars])
               (values decon
                       #'(lambda (v* ...) #t)
                       #'(lambda (v* ...) body))))]
          [(pat guard body)
           (let-values ([(pvars decon) (parse-pat #'pat)])
             (with-syntax ([(v* ...) pvars])
               (values decon
                       #'(lambda (v* ...) guard)
                       #'(lambda (v* ...) body))))]))
      (syntax-case ctx ()
        [(_ expr (lits ...)) (andmap sys:identifier? #'(lits ...))
         #'(stx-error expr "invalid syntax")]
        [(_ expr (lits ...) cls cls* ...) (andmap sys:identifier? #'(lits ...))
         (let-values ([(decon guard body)
                       (parse-clause #'(lits ...) #'cls)])
           (with-syntax ([decon decon] [guard guard] [body body])
             #'(let ([t expr])
                 (let ([ls/false (decon t)])
                   (if (and ls/false (apply guard ls/false))
                       (apply body ls/false)
                       (syntax-match t (lits ...) cls* ...))))))])))
  (define parse-define
    (lambda (x)
      (syntax-match x ()
        [(_ (id . fmls) b b* ...) (id? id)
         (values id (cons 'defun (cons fmls (cons b b*))))]
        [(_ id val) (id? id)
         (values id (cons 'expr val))])))
  (define parse-define-syntax
    (lambda (x)
      (syntax-match x ()
        [(_ id val) (id? id) (values id val)])))
  (define scheme-stx
    (lambda (sym)
      (let ([subst
             (library-subst
               (find-library-by-name '(ikarus system $all)))])
        (cond
          [(assq sym subst) =>
           (lambda (x)
             (let ([name (car x)] [label (cdr x)])
               (add-subst
                 (make-rib (list name) (list top-mark*) (list label) #f)
                 (stx sym top-mark* '()))))]
          [else (stx sym top-mark* '())]))))
  ;;; macros
  (define add-lexical
    (lambda (lab lex r)
      (cons (list* lab 'lexical lex) r)))
  ;;;
  (define add-lexicals
    (lambda (lab* lex* r)
      (cond
        [(null? lab*) r]
        [else
         (add-lexicals (cdr lab*) (cdr lex*)
           (add-lexical (car lab*) (car lex*) r))])))
  ;;;
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
  (module (letrec-transformer letrec*-transformer)
    (define helper
      (lambda (e r mr letrec?)
        (syntax-match e ()
          [(_ ([lhs* rhs*] ...) b b* ...)
           (if (not (valid-bound-ids? lhs*))
               (stx-error e "duplicate identifiers")
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
                     ((if letrec? build-letrec build-letrec*)
                       no-source lex* rhs* body)))))])))
    (define letrec-transformer
      (lambda (e r mr) (helper e r mr #t)))
    (define letrec*-transformer
      (lambda (e r mr) (helper e r mr #f))))
  (define type-descriptor-transformer
    (lambda (e r mr)
      (syntax-match e ()
        [(_ id) (id? id)
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
             (stx-error stx "duplicate bindings"))]
        [(_ f ([lhs* rhs*] ...) b b* ...) (id? f)
         (if (valid-bound-ids? lhs*)
             (bless `(letrec ([,f (lambda ,lhs* ,b . ,b*)])
                        (,f . ,rhs*)))
             (stx-error stx "invalid syntax"))])))
  (define let*-macro
    (lambda (stx)
      (syntax-match stx ()
        [(_ ([lhs* rhs*] ...) b b* ...) (andmap id? lhs*)
         (bless
           (let f ([x* (map list lhs* rhs*)])
             (cond
               [(null? x*) `(let () ,b . ,b*)]
               [else `(let (,(car x*)) ,(f (cdr x*)))])))])))
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
         (begin
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
                              pat* tmp*)))))])))
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
      (define cvt
        (lambda (p n ids)
          (syntax-match p ()
            [id (id? id)
             (cond
               [(bound-id-member? p keys)
                (values `#(free-id ,p) ids)]
               [(free-id=? p (scheme-stx '_))
                (values '_ ids)]
               [else (values 'any (cons (cons p n) ids))])]
            [(p dots) (ellipsis? dots)
             (let-values ([(p ids) (cvt p (+ n 1) ids)])
               (values
                 (if (eq? p 'any) 'each-any `#(each ,p))
                 ids))]
            [(x dots ys ... . z) (ellipsis? dots)
             (let-values ([(z ids) (cvt z n ids)])
               (let-values ([(ys ids) (cvt* ys n ids)])
                 (let-values ([(x ids) (cvt x (+ n 1) ids)])
                   (values `#(each+ ,x ,(reverse ys) ,z) ids))))]
            [(x . y)
             (let-values ([(y ids) (cvt y n ids)])
               (let-values ([(x ids) (cvt x n ids)])
                 (values (cons x y) ids)))]
            [() (values '() ids)]
            [#(p ...)
             (let-values ([(p ids) (cvt p n ids)])
               (values `#(vector ,p) ids))]
            [datum
             (values `#(atom ,(strip datum '())) ids)])))
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
               (stx-error pat "misplaced ellipsis in syntax-case pattern"))
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
                                    (add-subst (make-full-rib (list pat) (list lab)) expr)
                                    (extend-env lab (make-binding 'syntax (cons lex 0)) r)
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
           (begin
             (unless (andmap (lambda (x) (and (id? x) (not (ellipsis?  x)))) keys)
               (stx-error e "invalid literals"))
             (let ((x (gen-lexical 'tmp)))
               (let ([body (gen-syntax-case x keys clauses r mr)])
                 (build-application no-source
                   (build-lambda no-source (list x) body)
                   (list (chi-expr expr r mr))))))]))))
    (define syntax-transformer
      (let ()
        (define gen-syntax
          (lambda (src e r maps ellipsis? vec?)
            (syntax-match e ()
              [dots (ellipsis? dots)
               (stx-error src "misplaced ellipsis in syntax form")]
              [id (id? id)
               (let* ([label (id->label e)]
                      [b (label->binding label r)])
                   (if (eq? (binding-type b) 'syntax)
                       (let-values ([(var maps)
                                     (let ((var.lev (binding-value b)))
                                       (gen-ref src (car var.lev) (cdr var.lev) maps))])
                         (values (list 'ref var) maps))
                       (values (list 'quote e) maps)))]
              [(dots e) (ellipsis? dots)
               (if vec?
                   (stx-error src "misplaced ellipsis in syntax form")
                   (gen-syntax src e r maps (lambda (x) #f) #f))]
              [(x dots . y) (ellipsis? dots)
               (let f ([y y]
                       [k (lambda (maps)
                            (let-values ([(x maps)
                                          (gen-syntax src x r
                                            (cons '() maps) ellipsis? #f)])
                              (if (null? (car maps))
                                  (stx-error src
                                    "extra ellipsis in syntax form")
                                  (values (gen-map x (car maps)) (cdr maps)))))])
                 (syntax-match y ()
                   [() (k maps)]
                   [(dots . y) (ellipsis? dots)
                    (f y
                       (lambda (maps)
                         (let-values ([(x maps) (k (cons '() maps))])
                           (if (null? (car maps))
                               (stx-error src "extra ellipsis in syntax form")
                               (values (gen-mappend x (car maps)) (cdr maps))))))]
                   [_
                    (let-values ([(y maps)
                                  (gen-syntax src y r maps ellipsis? vec?)])
                      (let-values (((x maps) (k maps)))
                        (values (gen-append x y) maps)))]))]
              [(x . y)
               (let-values ([(xnew maps)
                             (gen-syntax src x r maps ellipsis? #f)])
                 (let-values ([(ynew maps)
                               (gen-syntax src y r maps ellipsis? vec?)])
                   (values (gen-cons e x y xnew ynew) maps)))]
              [#(ls ...)
               (let-values ([(lsnew maps)
                             (gen-syntax src ls r maps ellipsis? #t)])
                 (values (gen-vector e ls lsnew) maps))]
              [() (values '(quote ()) maps)]
              [_ (values `(quote ,e) maps)])))
        (define gen-ref
          (lambda (src var level maps)
            (if (= level 0)
                (values var maps)
                (if (null? maps)
                    (stx-error src "missing ellipsis in syntax form")
                    (let-values (((outer-var outer-maps)
                                  (gen-ref src var (- level 1) (cdr maps))))
                      (cond
                        [(assq outer-var (car maps)) =>
                         (lambda (b) (values (cdr b) maps))]
                        [else
                         (let ((inner-var (gen-lexical 'tmp)))
                           (values
                             inner-var
                             (cons
                               (cons (cons outer-var inner-var) (car maps))
                               outer-maps)))]))))))
        (define gen-append
          (lambda (x y)
            (if (equal? y '(quote ())) x `(append ,x ,y))))
        (define gen-mappend
          (lambda (e map-env)
            `(apply (primitive append) ,(gen-map e map-env))))
        (define gen-map
          (lambda (e map-env)
            (let ((formals (map cdr map-env))
                  (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
              (cond
               ; identity map equivalence:
               ; (map (lambda (x) x) y) == y
                [(eq? (car e) 'ref)
                 (car actuals)]
               ; eta map equivalence:
               ; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
                [(andmap
                   (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                   (cdr e))
                 (let ([args (map (let ((r (map cons formals actuals)))
                                    (lambda (x) (cdr (assq (cadr x) r))))
                                  (cdr e))])
                   `(map (primitive ,(car e)) . ,args))]
                [else (list* 'map (list 'lambda formals e) actuals)]))))
        (define gen-cons
          (lambda (e x y xnew ynew)
            (case (car ynew)
              [(quote)
               (if (eq? (car xnew) 'quote)
                   (let ((xnew (cadr xnew)) (ynew (cadr ynew)))
                     (if (and (eq? xnew x) (eq? ynew y))
                         `(quote ,e)
                         `(quote ,(cons xnew ynew))))
                   (if (eq? (cadr ynew) '())
                       `(list ,xnew)
                       `(cons ,xnew ,ynew)))]
              [(list) `(list ,xnew . ,(cdr ynew))]
              [else `(cons ,xnew ,ynew)])))
        (define gen-vector
          (lambda (e ls lsnew)
            (cond
              [(eq? (car lsnew) 'quote)
               (if (eq? (cadr lsnew) ls)
                   `(quote ,e)
                   `(quote #(,@(cadr lsnew))))]
              [(eq? (car lsnew) 'list)
               `(vector . ,(cdr lsnew))]
              [else `(list->vector ,lsnew)])))
        (define regen
          (lambda (x)
            (case (car x)
              [(ref) (build-lexical-reference no-source (cadr x))]
              [(primitive) (build-primref no-source (cadr x))]
              [(quote) (build-data no-source (cadr x))]
              [(lambda) (build-lambda no-source (cadr x) (regen (caddr x)))]
              [(map)
               (let ((ls (map regen (cdr x))))
                 (build-application no-source
                   (build-primref no-source 'map)
                   ls))]
              [else
               (build-application no-source
                 (build-primref no-source (car x))
                 (map regen (cdr x)))])))
        (lambda (e r mr)
          (syntax-match e ()
            [(_ x)
             (let-values (((e maps) (gen-syntax e x r '() ellipsis? #f)))
               (regen e))]))))
  (define core-macro-transformer
    (lambda (name)
      (case name
        [(quote)         quote-transformer]
        [(lambda)        lambda-transformer]
        [(case-lambda)   case-lambda-transformer]
        [(let-values)    let-values-transformer]
        [(letrec)        letrec-transformer]
        [(letrec*)       letrec*-transformer]
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
  (define (local-macro-transformer x)
    (car x))
  ;;; chi procedures
  (define chi-macro
    (lambda (p e)
      (let ([s ((macro-transformer p) (add-mark anti-mark e))])
        (add-mark (gen-mark) s))))
  (define chi-local-macro
    (lambda (p e)
      (let ([s ((local-macro-transformer p) (add-mark anti-mark e))])
        (add-mark (gen-mark) s))))
  (define (chi-global-macro p e)
    (let ([lib (car p)]
          [loc (cdr p)])
      (visit-library lib)
      (let ([x (symbol-value loc)])
        (let ([transformer
               (cond
                 [(procedure? x) x]
                 [else (error 'chi-global-macro "~s is not a procedure")])])
          (let ([s (transformer (add-mark anti-mark e))])
            (add-mark (gen-mark) s))))))
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
          [(global)
           (let* ([lib (car value)]
                  [loc (cdr value)])
             ((inv-collector) lib)
             (build-global-reference no-source loc))]
          [(core-prim)
           (let ([name value])
             (build-primref no-source name))]
          [(call) (chi-application e r mr)]
          [(lexical)
           (let ([lex value])
             (build-lexical-reference no-source lex))]
          [(global-macro)
           (chi-expr (chi-global-macro value e) r mr)]
          [(local-macro) (chi-expr (chi-local-macro value e) r mr)]
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
          [(displaced-lexical)
           (stx-error e "identifier out of context")]
          [else (error 'chi-expr "invalid type ~s for ~s" type
                       (strip e '())) (stx-error e)]))))
  (define chi-set!
    (lambda (e r mr)
      (syntax-match e ()
        [(_ x v) (id? x)
         (let-values ([(type value kwd) (syntax-type x r)])
           (case type
             [(lexical)
              (build-lexical-assignment no-source
                value
                (chi-expr v r mr))]
             ;;; FIXME: handle macro!
             [else (stx-error e)]))])))
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
      (let ([rib (make-empty-rib)])
        (let-values ([(e* r mr lex* rhs* mod** kwd*)
                      (chi-body* (map (lambda (x) (add-subst rib x))
                                      (syntax->list e*))
                         r mr '() '() '() '() rib #f)])
           (when (null? e*)
             (stx-error e* "no expression in body"))
           (let ([rhs* (chi-rhs* rhs* r mr)]
                 [init* (chi-expr* (append (apply append (reverse mod**)) e*) r mr)])
             (build-letrec* no-source
                (reverse lex*) (reverse rhs*)
                (build-sequence no-source init*)))))))
  (define chi-internal-module
    (lambda (e r mr lex* rhs* mod** kwd*)
      (define parse-module
        (lambda (e)
          (syntax-match e ()
            [(_ (export* ...) b* ...)
             (begin
               (unless (andmap id? export*)
                 (stx-error e "module exports must be identifiers"))
               (values #f export* b*))]
            [(_ name (export* ...) b* ...)
             (begin
               (unless (id? name)
                 (stx-error e "module name must be an identifier"))
               (unless (andmap id? export*)
                 (stx-error e "module exports must be identifiers"))
               (values name export* b*))])))
      (let-values ([(name exp-id* e*) (parse-module e)])
        (let* ([rib (make-empty-rib)]
               [e* (map (lambda (x) (add-subst rib x))
                        (syntax->list e*))])
          (let-values ([(e* r mr lex* rhs* mod** kwd*)
                        (chi-body* e* r mr lex* rhs* mod** kwd* rib #f)])
              (let ([exp-lab*
                     (map (lambda (x)
                             (or (id->label (add-subst rib x))
                                 (stx-error x "cannot find export")))
                          exp-id*)]
                    [mod** (cons e* mod**)])
                (if (not name) ;;; explicit export
                    (values lex* rhs* exp-id* exp-lab* r mr mod** kwd*)
                    (let ([lab (gen-label 'module)]
                          [iface (cons exp-id* exp-lab*)])
                      (values lex* rhs*
                              (list name) ;;; FIXME: module cannot
                              (list lab)  ;;;  export itself yet
                              (cons (cons lab (cons '$module iface)) r)
                              (cons (cons lab (cons '$module iface)) mr)
                              mod** kwd*)))))))))
  (define chi-body*
    (lambda (e* r mr lex* rhs* mod** kwd* rib top?)
      (cond
        [(null? e*) (values e* r mr lex* rhs* mod** kwd*)]
        [else
         (let ([e (car e*)])
           (let-values ([(type value kwd) (syntax-type e r)])
             (let ([kwd* (cons-id kwd kwd*)])
               (case type
                 [(define)
                  (let-values ([(id rhs) (parse-define e)])
                    (when (bound-id-member? id kwd*)
                      (stx-error e "cannot redefine keyword"))
                    (let ([lex (gen-lexical id)]
                          [lab (gen-label id)])
                      (extend-rib/check! rib id lab)
                      (chi-body* (cdr e*)
                         (add-lexical lab lex r) mr
                         (cons lex lex*) (cons rhs rhs*)
                         mod** kwd* rib top?)))]
                 [(define-syntax)
                  (let-values ([(id rhs) (parse-define-syntax e)])
                    (when (bound-id-member? id kwd*)
                      (stx-error e "cannot redefine keyword"))
                    (let ([lab (gen-label id)]
                          [expanded-rhs (expand-transformer rhs mr)])
                        (extend-rib/check! rib id lab)
                        (let ([b (make-eval-transformer expanded-rhs)])
                          (chi-body* (cdr e*)
                             (cons (cons lab b) r) (cons (cons lab b) mr)
                             lex* rhs* mod** kwd* rib top?))))]
                 [(module)
                  (let-values ([(lex* rhs* m-exp-id* m-exp-lab* r mr mod** kwd*)
                                (chi-internal-module e r mr lex* rhs* mod** kwd*)])
                    (for-each
                      (lambda (id lab) (extend-rib/check! rib id lab))
                      m-exp-id* m-exp-lab*)
                    (chi-body* (cdr e*) r mr lex* rhs* mod** kwd* rib top?))]
                 [(begin)
                  (syntax-match e ()
                    [(_ x* ...)
                     (chi-body* (append x* (cdr e*))
                        r mr lex* rhs* mod** kwd* rib top?)])]
                 [(global-macro)
                  (chi-body*
                     (cons (add-subst rib (chi-global-macro value e)) (cdr e*))
                     r mr lex* rhs* mod** kwd* rib top?)]
                 [(local-macro)
                  (chi-body*
                     (cons (add-subst rib (chi-local-macro value e)) (cdr e*))
                     r mr lex* rhs* mod** kwd* rib top?)]
                 [(macro)
                  (chi-body*
                     (cons (add-subst rib (chi-macro value e)) (cdr e*))
                     r mr lex* rhs* mod** kwd* rib top?)]
                 [else
                  (if top?
                      (chi-body* (cdr e*) r mr 
                          (cons (gen-lexical 'dummy) lex*)
                          (cons (cons 'expr e) rhs*)
                          mod** kwd* rib top?)
                      (values e* r mr lex* rhs* mod** kwd*))]))))])))
  (define (expand-transformer expr r)
    (let ([rtc (make-collector)])
      (let ([expanded-rhs
             (parameterize ([inv-collector rtc]
                            [vis-collector (lambda (x) (void))])
                 (chi-expr expr r r))])
        (for-each
          (let ([mark-visit (vis-collector)])
            (lambda (x)
              (invoke-library x)
              (mark-visit x)))
          (rtc))
        expanded-rhs)))
  (define (parse-exports exp*)
    (let f ([exp* exp*] [int* '()] [ext* '()])
      (cond
        [(null? exp*)
         (let ([id* (map (lambda (x) (stx x top-mark* '())) ext*)])
           (unless (valid-bound-ids? id*)
             (error #f "duplicate exports of ~s" (find-dups id*))))
         (values int* ext*)]
        [else
         (syntax-match (car exp*) ()
           [(rename (i* e*) ...)
            (begin
              (unless (and (eq? rename 'rename) (andmap symbol? i*) (andmap symbol? e*))
                (error #f "invalid export specifier ~s" (car exp*)))
              (f (cdr exp*) (append i* int*) (append e* ext*)))]
           [ie
            (begin
              (unless (symbol? ie) (error #f "invalid export ~s" ie))
              (f (cdr exp*) (cons ie int*) (cons ie ext*)))])])))
  (define parse-library
    (lambda (e)
      (syntax-match e ()
        [(_ (name name* ...)
            (export exp* ...)
            (import imp* ...)
            b* ...)
         (if (and (eq? export 'export)
                  (eq? import 'import)
                  (symbol? name)
                  (andmap symbol? name*))
             (let-values ([(exp-int* exp-ext*) (parse-exports exp*)])
               (values (cons name name*) exp-int* exp-ext* imp* b*))
             (error who "malformed library ~s" e))]
        [_ (error who "malformed library ~s" e)])))
  (define (set-cons x ls)
    (cond
      [(memq x ls) ls]
      [else (cons x ls)]))
  (define (set-union ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(memq (car ls1) ls2) (set-union (cdr ls1) ls2)]
      [else (cons (car ls1) (set-union (cdr ls1) ls2))]))
  (define (get-import-subst/libs imp*)
    (define (insert-to-subst a subst)
      (let ([name (car a)] [label (cdr a)])
        (cond
          [(assq name subst) =>
           (lambda (x)
             (cond
               [(eq? (cdr x) label) subst]
               [else
                (error 'import
                   "two imports of ~s with different bindings"
                   name)]))]
          [else
           (cons a subst)])))
    (define (merge-substs s subst)
      (cond
        [(null? s) subst]
        [else
         (insert-to-subst (car s)
           (merge-substs (cdr s) subst))]))
    (define (exclude* sym* subst)
      (define (exclude sym subst)
        (cond
          [(null? subst)
           (error 'import "cannot rename unbound identifier ~s" sym)]
          [(eq? sym (caar subst))
           (values (cdar subst) (cdr subst))]
          [else
           (let ([a (car subst)])
             (let-values ([(old subst) (exclude sym (cdr subst))])
               (values old (cons a subst))))]))
      (cond
        [(null? sym*) (values '() subst)]
        [else
         (let-values ([(old subst) (exclude (car sym*) subst)])
           (let-values ([(old* subst) (exclude* (cdr sym*) subst)])
             (values (cons old old*) subst)))]))
    (define (find* sym* subst)
      (map (lambda (x)
             (cond
               [(assq x subst) => cdr]
               [else (error 'import "cannot find identifier ~s" x)]))
           sym*))
    (define (rem* sym* subst)
      (let f ([subst subst])
        (cond
          [(null? subst) '()]
          [(memq (caar subst) sym*) (f (cdr subst))]
          [else (cons (car subst) (f (cdr subst)))])))
    (define (get-import spec)
      (define (remove-dups ls)
        (cond
          [(null? ls) '()]
          [(memq (car ls) (cdr ls)) (remove-dups (cdr ls))]
          [else (cons (car ls) (remove-dups (cdr ls)))]))
      (unless (pair? spec)
        (error 'import "invalid import spec ~s" spec))
      (case (car spec)
        [(rename)
         (syntax-match spec ()
           [(_ isp (old* new*) ...)
            (begin
              (unless (and (andmap symbol? old*) (andmap symbol? new*))
                (error 'import "invalid import spec ~s" spec))
              (let-values ([(subst lib) (get-import isp)])
                (let ([old-label* (find* old* subst)])
                  (let ([subst (rem* old* subst)])
                    ;;; FIXME: make sure map is valid
                    (values (merge-substs (map cons new* old-label*) subst)
                            lib)))))]
           [_ (error 'import "invalid rename spec ~s" spec)])]
        [(except)
         (syntax-match spec ()
           [(_ isp sym* ...)
            (begin
              (unless (andmap symbol? sym*)
                (error 'import "invalid import spec ~s" spec))
              (let-values ([(subst lib) (get-import isp)])
                (values (rem* sym* subst) lib)))]
           [_ (error 'import "invalid import spec ~s" spec)])]
        [(only)
         (syntax-match spec ()
           [(_ isp sym* ...)
            (begin
              (unless (andmap symbol? sym*)
                (error 'import "invalid import spec ~s" spec))
              (let-values ([(subst lib) (get-import isp)])
                (let ([sym* (remove-dups sym*)])
                  (let ([lab* (find* sym* subst)])
                    (values (map cons sym* lab*) lib)))))]
           [_ (error 'import "invalid import spec ~s" spec)])]
        [(prefix) (error #f "prefix found")]
        [else
         (let ([lib (find-library-by-name spec)])
           (unless lib
             (error 'import "cannot find library satisfying ~s" spec))
           (let ([s (library-subst lib)])
              (values s lib)))]))
    (cond
      [(null? imp*) (values '() '())]
      [else
       (let-values ([(subst1 lib1*)
                     (get-import-subst/libs (cdr imp*))])
         (let-values ([(subst2 lib2) (get-import (car imp*))])
           (values (merge-substs subst1 subst2)
                   (set-cons lib2 lib1*))))]))
  (define (make-top-rib subst)
    (let ([rib (make-empty-rib)])
      (for-each
        (lambda (x)
          (let ([name (car x)] [label (cdr x)])
            (extend-rib! rib (stx name top-mark* '()) label)))
        subst)
      rib))
  (define (make-collector)
    (let ([ls '()])
      (case-lambda
        [() ls]
        [(x) (set! ls (set-cons x ls))])))
  (define inv-collector
    (make-parameter
      (lambda args
        (error 'inv-collector "not initialized"))
      (lambda (x)
        (unless (procedure? x)
          (error 'inv-collector "~s is not a procedure" x))
        x)))
  (define vis-collector
    (make-parameter
      (lambda args
        (error 'vis-collector "not initialized"))
      (lambda (x)
        (unless (procedure? x)
          (error 'vis-collector "~s is not a procedure" x))
        x)))
  (define chi-library-internal
    (lambda (e* rib top?)
      (let-values ([(e* r mr lex* rhs* mod** _kwd*)
                    (chi-body* e* '() '() '() '() '() '() rib top?)])
        (values (append (apply append (reverse mod**)) e*)
           r mr (reverse lex*) (reverse rhs*)))))
  (define core-library-expander
    (lambda (e)
      (let-values ([(name exp-int* exp-ext* imp* b*) (parse-library e)])
        (let-values ([(subst imp*) (get-import-subst/libs imp*)])
          (let ([rib (make-top-rib subst)])
            (let ([b* (map (lambda (x) (stx x top-mark* (list rib))) b*)]
                  [rtc (make-collector)]
                  [vtc (make-collector)])
              (parameterize ([inv-collector rtc]
                             [vis-collector vtc])
                (let-values ([(init* r mr lex* rhs*)
                              (chi-library-internal b* rib #f)])
                  (seal-rib! rib)
                  (let ([rhs* (chi-rhs* rhs* r mr)])
                    (let ([invoke-body (if (and (null? init*) (null? lex*))
                                           (build-void)
                                           (build-sequence no-source
                                             (append
                                               (map build-export lex*)
                                               (chi-expr* init* r mr))))])
                      (unseal-rib! rib)
                      (let ([export-subst (make-export-subst exp-int* exp-ext* rib)])
                        (let-values ([(export-env macro*)
                                      (make-export-env/macros r)])
                          (values
                            name imp* (rtc) (vtc)
                            (build-letrec* no-source lex* rhs* invoke-body)
                            macro*
                            export-subst export-env)))))))))))))
  
  (define (parse-top-level-program e*)
    (syntax-match e* ()
      [((import imp* ...) b* ...) (eq? import 'import)
       (values imp* b*)]
      [_ (error "invalid syntax of top-level program")]))

  (define top-level-expander
    (lambda (e*)
      (let-values ([(imp* b*) (parse-top-level-program e*)])
        (let-values ([(subst imp*) (get-import-subst/libs imp*)])
          (let ([rib (make-top-rib subst)])
            (let ([b* (map (lambda (x) (stx x top-mark* (list rib))) b*)]
                  [rtc (make-collector)]
                  [vtc (make-collector)])
              (parameterize ([inv-collector rtc]
                             [vis-collector vtc])
                (let-values ([(init* r mr lex* rhs*)
                              (chi-library-internal b* rib #t)])
                  (seal-rib! rib)
                  (let ([rhs* (chi-rhs* rhs* r mr)])
                    (let ([invoke-body (if (null? init*)
                                           (build-void)
                                           (build-sequence no-source
                                             (chi-expr* init* r mr)))])
                      (unseal-rib! rib)
                      (values (rtc)
                        (build-letrec* no-source 
                          lex* rhs* invoke-body))))))))))))
  (define (visit! macro*)
    (for-each (lambda (x)
                (let ([loc (car x)] [proc (cadr x)])
                  (set-symbol-value! loc proc)))
              macro*))
  (define (build-visit-code macro*)
    (if (null? macro*)
        (build-void)
        (build-sequence no-source
          (map (lambda (x)
                 (let ([loc (car x)] [src (cddr x)])
                   (build-global-assignment no-source loc src)))
               macro*))))
  (define (library-expander x)
    (let-values ([(name imp* inv* vis* invoke-code macro* export-subst export-env)
                    (core-library-expander x)])
      (let ([id (gensym)]
            [name name]
            [ver '()]  ;;; FIXME
            [imp* (map library-spec imp*)]
            [vis* (map library-spec vis*)]
            [inv* (map library-spec inv*)])
        (install-library id name ver
           imp* vis* inv* export-subst export-env
           (lambda () (visit! macro*))
           (lambda () (eval-core invoke-code))
           #t)
        ;(pretty-print (build-visit-code macro*))
        (values invoke-code
                (build-visit-code macro*)
                export-subst export-env))))
  (define (boot-library-expand x)
    (let-values ([(invoke-code visit-code export-subst export-env)
                  (library-expander x)])
      (values invoke-code export-subst export-env)))
  (define build-export
    (lambda (x)
      ;;; exports use the same gensym
      `(#%$set-symbol-value! ',x ,x)))
  (define (make-export-subst int* ext* rib)
    (map
      (lambda (int ext)
        (let* ([id (stx int top-mark* (list rib))]
               [label (id->label id)])
          (unless label
            (stx-error id "cannot export unbound identifier"))
          (cons ext label)))
      int* ext*))
  (define (make-export-env/macros r)
    (let f ([r r] [env '()] [macro* '()])
      (cond
        [(null? r) (values env macro*)]
        [else
         (let ([x (car r)])
           (let ([label (car x)] [b (cdr x)])
             (case (binding-type b)
               [(lexical)
                (f (cdr r)
                   (cons (list* label 'global (binding-value b)) env)
                   macro*)]
               [(local-macro)
                (let ([loc (gensym)])
                  (f (cdr r)
                     (cons (list* label 'global-macro loc) env)
                     (cons (cons loc (binding-value b)) macro*)))]
               [($rtd) (f (cdr r) (cons x env) macro*)]
               [else
                (error #f "don't know how to export ~s ~s"
                       (binding-type b) (binding-value b))])))])))
  (define generate-temporaries
    (lambda (ls)
      (unless (list? ls)
        (error 'generate-temporaries "~s is not a list"))
      (map (lambda (x) (stx (gensym 't) top-mark* '())) ls)))
  (define free-identifier=?
    (lambda (x y)
      (if (id? x)
          (if (id? y)
              (free-id=? x y)
              (error 'free-identifier=? "~s is not an identifier" y))
          (error 'free-identifier=? "~s is not an identifier" x))))
  (define syntax-error
    (lambda (x . args)
      (unless (andmap string? args)
        (error 'syntax-error "invalid argument ~s" args))
      (error #f "~a: ~s"
             (apply string-append args)
             (strip x '()))))
  (define identifier? (lambda (x) (id? x)))
  (define eval-r6rs-top-level
    (lambda (x*)
      (let-values ([(lib* invoke-code) (top-level-expander x*)])
        (for-each invoke-library lib*)
        (eval-core invoke-code)
        (void))))
  (define eval-top-level
    (lambda (x)
      (unless (pair? x)
        (error #f "invalid expression at top-level ~s" x))
      (case (car x)
        [(library)
         (library-expander x)
         (void)]
        [(invoke)
         (syntax-match x ()
           [(_ (id** ...) ...)
            (begin
              (unless (andmap (lambda (id*) (andmap symbol? id*)) id**)
                (error #f "invalid invoke form ~s" x))
              (let ([lib*
                     (map (lambda (x)
                            (or (find-library-by-name x)
                                (error #f "cannot find library ~s"
                                       x)))
                          id**)])
                (for-each invoke-library lib*)))]
           [else (error #f "invalid invoke form ~s" x)])]
        [else (error #f "invalid top-level form ~s" x)])))
  ;;; FIXME: export the rest of the syntax-case procedures
)



