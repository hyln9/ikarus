

(define chi-top-library
  (let ()
    ;(define my-map
    ;  (lambda (ctxt f ls . ls*)
    ;    (cond
    ;      [(and (list? ls) 
    ;            (andmap list? ls*)
    ;            (let ([n (length ls)])
    ;              (andmap (lambda (ls) (= (length ls) n)) ls*)))
    ;       (let loop ([ls ls] [ls* ls*])
    ;         (cond
    ;           [(null? ls) '()]
    ;           [else
    ;            (cons (apply f (car ls) (#%map car ls*))
    ;                  (loop (cdr ls) (#%map cdr ls*)))]))]
    ;      [else (error ctxt "invalid args ~s" (cons ls ls*))])))
    ;(define-syntax map
    ;  (syntax-rules ()
    ;    [(_ f ls ls* ...)
    ;     (my-map '(map f ls ls* ...) f ls ls* ...)]))
    (define-syntax build-let
      (syntax-rules ()
        [(_ ae lhs* rhs* body)
         (build-application ae
           (build-lambda ae lhs* body)
           rhs*)]))
    (define who 'chi-top-library)
    (define-syntax assert
      (syntax-rules ()
        [(_ name pred* ...)
         (unless (and pred* ...)
           (error 'name "assertion ~s failed" '(pred* ...)))]))
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
    (define id/label-rib
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
            (error 'stx-subst* "~s is not a syntax object" x))))
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
    (define sym->free-id
      (lambda (x)
        (stx x top-mark* '())))
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
    (define syntax-pair?
      (lambda (x) (syntax-kind? x pair?)))
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
        (assert id->label (id? id))
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
                 [(lexical core-prim)
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
        (syntax-match e 
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
      (syntax-rules ()
        [(_ stx) (error 'chi "invalid syntax ~s" (strip stx '()))]
        [(_ stx msg) (error 'chi "~a: ~s" msg (strip stx '()))]))
    ;(define stx-error
    ;  (lambda (stx . args)
    ;    (error 'chi "invalid syntax ~s" (strip stx '()))))
    (define sanitize-binding
      (lambda (x)
        (cond
          [(procedure? x) (cons 'macro x)]
          [(and (pair? x) (eq? (car x) 'macro!) (procedure? (cdr x)))
           x]
          [(and (pair? x) (eq? (car x) '$rtd)) x]
          [else (error 'expand "invalid transformer ~s" x)])))
    (define make-eval-transformer
      (lambda (x)
        (sanitize-binding (local-eval-hook x)))) 
    (define-syntax syntax-match-test
      (lambda (stx)
        (define dots?
          (lambda (x)
            (and (identifier? x) 
                 (free-identifier=? x #'(... ...)))))
        (define f
          (lambda (stx)
            (syntax-case stx ()
              [id (identifier? #'id) #'(lambda (x) #t)]
              [(pat dots) (dots? #'dots)
               (with-syntax ([p (f #'pat)])
                 #'(lambda (x) 
                     (and (syntax-list? x) 
                          (andmap p (syntax->list x)))))]
              [(pat dots . last) (dots? #'dots)
               (with-syntax ([p (f #'pat)] [l (f #'last)])
                 #'(lambda (x)
                     (let loop ([x x])
                       (cond
                         [(syntax-pair? x) 
                          (and (p (syntax-car x)) 
                               (loop (syntax-cdr x)))]
                         [else (l x)]))))]
              [(a . d) 
               (with-syntax ([pa (f #'a)] [pd (f #'d)])
                 #'(lambda (x) 
                     (and (syntax-pair? x)
                          (pa (syntax-car x))
                          (pd (syntax-cdr x)))))]
              [datum 
               #'(lambda (x) 
                   (equal? (strip x '()) 'datum))])))
        (syntax-case stx ()
          [(_ x [pat code code* ...])
           (with-syntax ([pat-code (f #'pat)])
              #'(pat-code x))])))
    (define-syntax syntax-match-conseq
      (lambda (stx) 
        (define dots?
          (lambda (x)
            (and (identifier? x) 
                 (free-identifier=? x #'(... ...))))) 
        (define f
          (lambda (stx)
            (syntax-case stx ()
              [id (identifier? #'id) 
               (values (list #'id) #'(lambda (x) x))]
              [(pat dots) (dots? #'dots)
               (let-values ([(vars extractor) (f #'pat)])
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
               (let-values ([(pvars pext) (f #'pat)])
                 (let-values ([(lvars lext) (f #'last)])
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
               (let-values ([(avars aextractor) (f #'a)])
                 (let-values ([(dvars dextractor) (f #'d)])
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
              [datum 
               (values '() #'(lambda (x) (dot-call-me)))])))
        (syntax-case stx ()
          [(_ x [pat code code* ...])
           (let-values ([(vars extractor) 
                         (f #'pat)])
             (with-syntax ([e extractor] [(vs ...) vars])
               (case (length vars)
                 [(0) #'(begin code code* ...)]
                 [(1) #'(let ([vs ... (e x)]) code code* ...)]
                 [else #'(let-values ([(vs ...) (e x)]) code code* ...)])))])))
    (define-syntax syntax-match
      (lambda (x)
        (syntax-case x ()
          [(_ expr) #'(stx-error expr)]
          [(_ expr cls cls* ...) 
           #'(let ([t expr]) 
               (if (syntax-match-test t cls)
                   (syntax-match-conseq t cls)
                   (syntax-match t cls* ...)))])))
    (define parse-define
      (lambda (x)
        (syntax-match x
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
        (syntax-match x
          [(_ id val) 
           (if (id? id) 
               (values id val)
               (stx-error x))])))
    (define scheme-env ; the-env
      '([define     define-label     (define)]
        [define-syntax define-syntax-label (define-syntax)]
        [module     module-label     (module)]
        [begin      begin-label      (begin)]
        [set!       set!-label       (set!)]
        [define-record define-record-label (macro .  define-record)]
        [case          case-label          (core-macro .  case)]
        [foreign-call foreign-call-label (core-macro .  foreign-call)]
        [quote      quote-label      (core-macro . quote)]
        [lambda     lambda-label     (core-macro . lambda)]
        [case-lambda case-lambda-label (core-macro . case-lambda)]
        [let-values let-values-label (core-macro . let-values)]
        [let        let-label        (core-macro . let)]
        [let*       let*-label       (core-macro . let*)]
        [cond       cond-label       (core-macro . cond)]
        [if         if-label         (core-macro . if)]
        [when       when-label       (core-macro . when)]
        [unless     unless-label     (core-macro . unless)]
        [and        and-label        (core-macro . and)]
        [or         or-label         (core-macro . or)]
        [parameterize parameterize-label (core-macro . parameterize)]
        ;;; prims
        [void         void-label        (core-prim . void)]
        [not          not-label         (core-prim . not)]
        [boolean?     boolean-label     (core-prim . boolean?)]
        [null?        null?-label       (core-prim . null?)]
        [procedure?   procedure?-label  (core-prim . procedure?)]
        [eof-object?  eof-object?-label (core-prim . eof-object?)]
        [bwp-object?  bwp-object?-label (core-prim . bwp-object?)]
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
        [list       list-label       (core-prim . list)]
        [make-list make-list-label (core-prim . make-list)]
        [list*       list*-label       (core-prim . list*)]
        [list?      list?-label      (core-prim . list?)]
        [append     append-label     (core-prim . append)]
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
        ;;; chars
        [char?     char?-label     (core-prim . char?)]
        [char=?     char=?-label     (core-prim . char=?)]
        [integer->char integer->char-label (core-prim . integer->char)]
        [char->integer char->integer-label (core-prim . char->integer)]
        [char-whitespace? char-whitespace?-label (core-prim . char-whitespace?)]
        [$char?     $char?-label     (core-prim . $char?)]
        [$char<=    $char<=-label    (core-prim . $char<=)]
        [$char=    $char=-label    (core-prim . $char=)]
        [$char->fixnum $char->fixnum-label (core-prim . $char->fixnum)]
        [$fixnum->char $fixnum->char-label (core-prim . $fixnum->char)]
        ;;; strings
        [string?    string?-label    (core-prim . string?)]
        [make-string make-string-label (core-prim . make-string)]
        [string-ref string-ref-label (core-prim . string-ref)]
        [string-set! string-set!-label (core-prim . string-set!)]
        [string-length string-length-label (core-prim . string-length)]
        [string=?   string=?-label   (core-prim . string=?)]
        [substring  substring-label  (core-prim . substring)]
        [list->string list->string-label (core-prim . list->string)]
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
        [fxsll       fxsll-label       (core-prim . fxsll)]
        [fxsra       fxsra-label       (core-prim . fxsra)]
        [fxlogand    fxlogand-label    (core-prim . fxlogand)]
        [fxlogor    fxlogor-label    (core-prim . fxlogor)]
        [fxlognot    fxlognot-label    (core-prim . fxlognot)]
        [$fxadd1        $fxadd1-label        (core-prim . $fxadd1)]
        [$fx>=       $fx>=-label       (core-prim . $fx>=)]
        [$fx=        $fx=-label        (core-prim . $fx=)]
        [$fxsll        $fxsll-label        (core-prim . $fxsll)]
        [$fxsra        $fxsra-label        (core-prim . $fxsra)]
        [$fxlogxor        $fxlogxor-label        (core-prim . $fxlogxor)]
        [$fxlognot        $fxlognot-label        (core-prim . $fxlognot)]
        [$fxlogand        $fxlogand-label        (core-prim . $fxlogand)]
        [$fx+        $fx+-label        (core-prim . $fx+)]
        [$fx-        $fx--label        (core-prim . $fx-)]
        [$fx<        $fx<-label        (core-prim . $fx<)]
        [$fx>        $fx>-label        (core-prim . $fx>)]
        ;;; flonum
        [string->flonum string->flonum-label (core-prim . string->flonum)]
        ;;; generic arithmetic
        [-          minus-label      (core-prim . -)]
        [*          *-label          (core-prim . *)]
        [+          plus-label       (core-prim . +)]
        [number?   number?-label   (core-prim . number?)]
        [quotient   quotient-label   (core-prim . quotient)]
        [number->string   number->string-label   (core-prim . number->string)]
        ;;; symbols/gensyms
        [symbol?    symbol?-label    (core-prim . symbol?)]
        [gensym?    gensym?-label    (core-prim . gensym?)]
        [gensym     gensym-label     (core-prim . gensym)]
        [getprop    getprop-label    (core-prim . getprop)]
        [putprop    putprop-label    (core-prim . putprop)]
        [string->symbol string->symbol-label (core-prim . string->symbol)]
        [symbol->string symbol->string-label (core-prim . symbol->string)]
        [gensym->unique-string gensym->unique-string-label (core-prim . gensym->unique-string)]
        [$set-symbol-value! $set-symbol-value!-label (core-prim . $set-symbol-value!)]
        ;;; top-level
        [top-level-bound?     top-level-bound-label      (core-prim . top-level-bound?)]
        [top-level-value      top-level-value-label      (core-prim .  top-level-value)]
        [set-top-level-value! set-top-level-value!-label (core-prim .  set-top-level-value!)]
        ;;; IO/ports
        [output-port?        output-port?-label        (core-prim .  output-port?)]
        [input-port?        input-port?-label        (core-prim .  input-port?)]
        [input-port-name        input-port-name-label        (core-prim .  input-port-name)]
        [output-port-name        output-port-name-label        (core-prim .  output-port-name)]
        [open-input-file  open-input-file-label  (core-prim .  open-input-file)]
        [open-output-file  open-output-file-label  (core-prim .  open-output-file)]
        [open-output-string  open-output-string-label  (core-prim .  open-output-string)]
        [get-output-string  get-output-string-label  (core-prim .  get-output-string)]
        [close-input-port  close-input-port-label  (core-prim .  close-input-port)]
        [console-input-port  console-input-port-label  (core-prim .  console-input-port)]
        [console-output-port console-output-port-label (core-prim .  console-output-port)]
        [current-input-port  current-input-port-label  (core-prim .  current-input-port)]
        [current-output-port current-output-port-label (core-prim .  current-output-port)]
        [standard-input-port  standard-input-port-label  (core-prim .  standard-input-port)]
        [standard-output-port standard-output-port-label (core-prim .  standard-output-port)]
        [standard-error-port  standard-error-port-label  (core-prim .  standard-error-port)]
        [flush-output-port   flush-output-port-label   (core-prim .  flush-output-port)]
        [reset-input-port!   reset-input-port!-label   (core-prim .  reset-input-port!)]
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
        [eval       eval-label       (core-prim . eval)]
        [load       load-label       (core-prim . load)]
        [new-cafe   new-cafe-label   (core-prim . new-cafe)]
        [command-line-arguments command-line-arguments-label (core-prim .  command-line-arguments)]
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
        [$record      $record-label      (core-prim . $record)]
        [$record?     $record?-label     (core-prim . $record?)]
        [$record/rtd? $record/rtd?-label (core-prim . $record/rtd?)]
        ;;; codes
        [$closure-code  $closure-code-label (core-prim . $closure-code)]
        [$code? $code?-label (core-prim . $code?)]
        [$code-reloc-vector $code-reloc-vector-label (core-prim . $code-reloc-vector)]
        [$code-freevars $code-freevars-label (core-prim . $code-freevars)]
        [$code-size $code-size-label (core-prim . $code-size)]
        [$code-ref $code-ref-label (core-prim . $code-ref)]
        [$code-set! $code-set!-label (core-prim . $code-set!)]
        [code? code?-label (core-prim . code?)]
        [code-reloc-vector code-reloc-vector-label (core-prim . code-reloc-vector)]
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
        [primitive-set! primitive-set!-label (core-prim . primitive-set!)]
        [primitive-ref primitive-ref-label (core-prim .  primitive-ref)]
        [$forward-ptr?     $forward-ptr?-label    (core-prim . $forward-ptr?)]
        [$unbound-object?     $unbound-object?-label    (core-prim . $unbound-object?)]
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
    (define let-values-transformer
      (lambda (e r mr) 
        (syntax-match e
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
                           (id/label-rib fml* lab*)
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
    (define let*-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ ([lhs* rhs*] ...) b b* ...)
           (let f ([lhs* lhs*] [rhs* rhs*]
                   [subst-lhs* '()] [subst-lab* '()]
                   [r r])
             (cond
               [(null? lhs*) 
                (chi-internal
                  (add-subst 
                    (id/label-rib subst-lhs* subst-lab*)
                    (cons b b*))
                  r mr)]
               [else
                (let ([lhs (car lhs*)]
                      [rhs (chi-expr
                             (add-subst
                               (id/label-rib subst-lhs* subst-lab*)
                               (car rhs*))
                             r mr)])
                  (unless (id? lhs)
                    (stx-error lhs "invalid binding"))
                  (let ([lex (gen-lexical lhs)]
                        [lab (gen-label lhs)])
                    (build-let no-source (list lex) (list rhs)
                      (f (cdr lhs*) (cdr rhs*)
                         (cons lhs subst-lhs*) 
                         (cons lab subst-lab*)
                         (add-lexicals (list lab) (list lex) r)))))]))])))
    (define let-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ ([lhs* rhs*] ...) b b* ...) 
           (if (not (valid-bound-ids? lhs*))
               (stx-error e)
               (let ([rhs* (chi-expr* rhs* r mr)]
                     [lex* (map gen-lexical lhs*)]
                     [lab* (map gen-label lhs*)])
                 (let ([body (chi-internal 
                               (add-subst 
                                 (id/label-rib lhs* lab*)
                                 (cons b b*))
                               (add-lexicals lab* lex* r)
                               mr)])
                   (build-application no-source 
                     (build-lambda no-source lex* body)
                     rhs*))))]
          [(_ loop ([lhs* rhs*] ...) b b* ...)
           (if (and (id? loop) (valid-bound-ids? lhs*))
               (let ([rhs* (chi-expr* rhs* r mr)]
                     [lex* (map gen-lexical lhs*)]
                     [lab* (map gen-label lhs*)]
                     [looplex (gen-lexical loop)]
                     [looplab (gen-label loop)])
                 (let ([b* (add-subst (id/label-rib (list loop) (list looplab))
                             (add-subst (id/label-rib lhs* lab*)
                               (cons b b*)))]
                       [r (add-lexicals 
                            (cons looplab lab*)
                            (cons looplex lex*)
                            r)])
                   (let ([body (chi-internal b* r mr)])
                     (build-letrec no-source 
                        (list looplex)
                        (list (build-lambda no-source lex* body))
                        (build-application no-source 
                          looplex rhs*)))))
               (stx-error e))])))
    (define when-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ test e e* ...) 
           (build-conditional no-source
             (chi-expr test r mr)
             (build-sequence no-source
               (chi-expr* (cons e e*) r mr))
             (chi-void))])))
    (define unless-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ test e e* ...) 
           (build-conditional no-source
             (chi-expr test r mr)
             (chi-void) 
             (build-sequence no-source
               (chi-expr* (cons e e*) r mr)))])))
    (define if-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ e0 e1 e2)
           (build-conditional no-source
             (chi-expr e0 r mr)
             (chi-expr e1 r mr)
             (chi-expr e2 r mr))]
          [(_ e0 e1)
           (build-conditional no-source
             (chi-expr e0 r mr)
             (chi-expr e1 r mr)
             (chi-void))])))
    (define case-transformer
      (lambda (e r mr)
        (define build-one 
          (lambda (t cls rest)
            (syntax-match cls
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
            (syntax-match cls
              [((d* ...) e e* ...)
               (build-one t cls (chi-void))]
              [(else-kwd x x* ...)
               (if (and (id? else-kwd)
                        (free-id=? else-kwd (sym->free-id 'else)))
                   (build-sequence no-source
                     (chi-expr* (cons x x*) r mr))
                   (stx-error e))]
              [else (stx-error e)])))
        (syntax-match e
          [(_ expr) 
           (build-sequence no-source 
             (list (chi-expr expr r mr) (chi-void)))]
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
    (define cond-transformer
      (lambda (expr r mr)
        (define handle-arrow 
          (lambda (e v altern)
            (let ([t (gen-lexical 't)])
              (build-let no-source 
                 (list t) (list (chi-expr e r mr))
                 (build-conditional no-source
                   (build-lexical-reference no-source t)
                   (build-application no-source
                     (chi-expr v r mr) 
                     (list (build-lexical-reference no-source t)))
                   altern)))))
        (define chi-last
          (lambda (e)
            (syntax-match e
              [(e0 e1 e2* ...)
               (if (and (id? e0) 
                        (free-id=? e0 (sym->free-id 'else)))
                   (build-sequence no-source 
                     (chi-expr* (cons e1 e2*) r mr))
                   (chi-one e (chi-void)))]
              [_ (chi-one e (chi-void))])))
        (define chi-one 
          (lambda (e rest)
            (define chi-test
              (lambda (e rest)
                (syntax-match e
                  [(e0 e1 e2 ...)
                   (build-conditional no-source 
                     (chi-expr e0 r mr)
                     (build-sequence no-source
                        (chi-expr* (cons e1 e2) r mr))
                     rest)]
                  [_ (stx-error expr)])))
            (syntax-match e
              [(e0 e1 e2) 
               (if (and (id? e1) 
                        (free-id=? e1 (sym->free-id '=>)))
                   (handle-arrow e0 e2 rest)
                   (chi-test e rest))]
              [_ (chi-test e rest)])))
        (syntax-match expr
          [(_) (chi-void)]
          [(_ e e* ...)
           (let f ([e e] [e* e*])
             (cond
               [(null? e*) (chi-last e)]
               [else (chi-one e (f (car e*) (cdr e*)))]))])))
    (define quote-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ datum) (build-data no-source (strip datum '()))])))
    (define case-lambda-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ [fmls* b* b** ...] ...)
           (let-values ([(fmls* body*)
                         (chi-lambda-clause* fmls*
                           (map cons b* b**) r mr)])
             (build-case-lambda no-source fmls* body*))])))
    (define lambda-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ fmls b b* ...)
           (let-values ([(fmls body) 
                         (chi-lambda-clause fmls 
                            (cons b b*) r mr)])
             (build-lambda no-source fmls body))])))
    (define bless
      (lambda (x)
        (let ([rib (make-scheme-rib)])
          (let f ([x x])
            (cond
              [(pair? x) 
               (cons (f (car x)) (f (cdr x)))]
              [(symbol? x) 
               (make-stx x top-mark* (list rib))]
              [else x])))))
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
        (syntax-match e
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
    (define parameterize-transformer
      (lambda (e r mr)
        (syntax-match e
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
    (define and-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_) (build-data no-source #t)]
          [(_ e e* ...) 
           (let f ([e e] [e* e*])
             (cond
               [(null? e*) (chi-expr e r mr)]
               [else 
                (build-conditional no-source
                  (chi-expr e r mr)
                  (f (car e*) (cdr e*))
                  (build-data no-source #f))]))])))
    (define or-transformer
      (lambda (e r mr)
        (syntax-match e 
          [(_) (build-data no-source #f)]
          [(_ e e* ...)
           (let f ([e e] [e* e*])
             (cond
               [(null? e*) (chi-expr e r mr)]
               [else
                (let ([t (gen-lexical 't)])
                  (build-let no-source 
                    (list t)
                    (list (chi-expr e r mr))
                    (build-conditional no-source
                      (build-lexical-reference no-source t)
                      (build-lexical-reference no-source t)
                      (f (car e*) (cdr e*)))))]))])))
    (define foreign-call-transformer
      (lambda (e r mr)
        (syntax-match e
          [(_ name arg* ...)
           (build-foreign-call no-source
             (chi-expr name r mr)
             (chi-expr* arg* r mr))])))
    (define core-macro-transformer
      (lambda (name)
        (case name
          [(quote)         quote-transformer]
          [(lambda)        lambda-transformer]
          [(case-lambda)   case-lambda-transformer]
          [(let-values)    let-values-transformer]
          [(let)           let-transformer]
          [(let*)          let*-transformer]
          [(cond)          cond-transformer]
          [(case)          case-transformer]
          [(if)            if-transformer]
          [(when)          when-transformer]
          [(unless)        unless-transformer]
          [(and)           and-transformer]
          [(or)            or-transformer]
          [(parameterize)  parameterize-transformer]
          [(foreign-call)  foreign-call-transformer]
          [else (error 'macro-transformer "cannot find ~s" name)])))
    (define macro-transformer
      (lambda (x)
        (cond
          [(procedure? x) x]
          [(symbol? x)
           (case x
             [(define-record) define-record-macro]
             [else (error 'macro-transformer 
                          "invalid macro ~s" x)])]
          [else (error 'core-macro-transformer 
                       "invalid macro ~s" x)])))
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
        (syntax-match e 
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
             (syntax-match e
               [(_ x x* ...)
                (build-sequence no-source
                  (chi-expr* (cons x x*) r mr))])]
            [else (error 'chi-expr "invalid type ~s for ~s" type
                         (strip e '())) (stx-error e)]))))
    (define chi-set!
      (lambda (e r mr)
        (syntax-match e
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
        (syntax-match fmls
          [(x* ...) 
           (if (valid-bound-ids? x*) 
               (let ([lex* (map gen-lexical x*)]
                     [lab* (map gen-label x*)])
                 (values
                   lex*
                   (chi-internal 
                     (add-subst 
                       (id/label-rib x* lab*)
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
                       (id/label-rib (cons x x*) (cons lab lab*))
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
    (define chi-internal
      (lambda (e* r mr)
        (define return
          (lambda (init* module-init** r mr lhs* lex* rhs*)
            (let ([mod-init* (apply append (reverse module-init**))])
              (unless (valid-bound-ids? lhs*)
                (error 'chi-internal "multiple definitions"))
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
                   (let ([kwd* (cons kwd kwd*)])
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
                       [(module)
                        (let-values ([(m-lhs* m-lex* m-rhs* m-init* m-exp-id* m-exp-lab* r mr kwd*)
                                      (chi-internal-module e r mr kwd*)])
                          (for-each
                            (lambda (id lab) (extend-rib! rib id lab))
                            m-exp-id* m-exp-lab*)
                          (f (cdr e*)
                             (cons m-init* module-init**)
                             r mr 
                             (append m-lhs* lhs*)
                             (append m-lex* lex*)
                             (append m-rhs* rhs*)
                             kwd*))]
                       [else 
                        (return e* module-init** r mr lhs* lex* rhs*)]))))])))))
    (define chi-internal-module
      (lambda (e r mr kwd*)
        (define parse-module
          (lambda (e)
            (syntax-match e
              [(_ (export* ...) b* ...)
               (unless (andmap id? export*) (stx-error e))
               (values #f export* b*)]
              [(_ name (export* ...) b* ...)
               (unless (and (id? name) (andmap id? export*)) (stx-error e))
               (values name export* b*)])))
        (let-values ([(name exp-id* e*) (parse-module e)])
          (let* ([rib (make-empty-rib)]
                 [e* (map (lambda (x) (add-subst rib x)) (syntax->list e*))])
            (define return
              (lambda (init* r mr lhs* lex* rhs* kwd*)
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
                     (let ([kwd* (cons kwd kwd*)])
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
                         [else 
                          (error 'chi-internal-module
                                 "cannot handle ~s"
                                 type)]))))]))))))
    (define chi-library-internal 
      (lambda (e* r rib kwd*)
        (define return
          (lambda (init* r mr lhs* lex* rhs*)
            (values init* r mr (reverse lhs*) (reverse lex*) (reverse rhs*))))
        (let f ([e* e*] [r r] [mr r] [lhs* '()] [lex* '()] [rhs* '()] [kwd* kwd*])
          (cond
            [(null? e*) (return e* r mr lhs* lex* rhs*)]
            [else
             (let ([e (car e*)])
               ;(printf "chi ~s\n" e)
               (let-values ([(type value kwd) (syntax-type e r)])
                 (let ([kwd* (cons kwd kwd*)])
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
                                 (cons (cons lab b) r)
                                 (cons (cons lab b) mr) 
                                 lhs* lex* rhs* kwd*)))))] 
                     [(begin)
                      (syntax-match e
                        [(_ x* ...)
                         (f (append x* (cdr e*)) r mr lhs* lex* rhs*
                            kwd*)])]
                     [(macro)
                      (f (cons (add-subst rib (chi-macro value e)) (cdr e*))
                         r mr lhs* lex* rhs* kwd*)]
                     [else 
                      (return e* r mr lhs* lex* rhs*)]))))]))))
    (define chi-top-library
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
                      (chi-void)
                      (build-sequence no-source 
                        (chi-expr* init* r mr))))))))))
    (lambda (x) 
      (let ([x (chi-top-library x)])
    ;    (pretty-print x)
        x))
    ))
