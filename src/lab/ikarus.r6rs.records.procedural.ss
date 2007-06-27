
(library (ikarus.r6rs.records.procedural)
  (export 
    make-record-type-descriptor
    make-record-constructor-descriptor
    record-accessor record-mutator
    record-constructor record-predicate)
  (import 
    (except (ikarus) record-constructor record-predicate)
    (ikarus system $records))

  (define-record rtd (name size parent sealed? opaque? uid fields))
  (define rtd-alist '())
  (define (intern-rtd! uid rtd)
    (set! rtd-alist (cons (cons uid rtd) rtd-alist)))
  (define (lookup-rtd uid)
    (cond
      [(assq uid rtd-alist) => cdr]
      [else #f]))


  (define (record-type-descriptor? x) (rtd? x))

  (module (make-record-type-descriptor)
    (define who 'make-record-type-descriptor)
    (define (make-rtd-aux name parent uid sealed? opaque? fields)
      (make-rtd name (vector-length fields) parent sealed? opaque? uid fields))
    (define (convert-fields pfv sv)
      (unless (vector? sv) 
        (error who "invalid fields argument ~s" sv))
      (let ([n1 (vector-length pfv)]
            [n2 (vector-length sv)])
        (let ([v (make-vector (+ n1 n2))])
          (let f ([i 0])
            (unless (= i n1) 
              (vector-set! v i (vector-ref pfv i))
              (f (add1 i))))
          (let f ([i 0])
            (unless (= i n2)
              (let ([x (vector-ref sv i)])
                (if (pair? x) 
                    (let ([m/u (car x)] [x (cdr x)])
                      (if (pair? x) 
                          (let ([name (car x)])
                            (unless (and (null? (cdr x)) (symbol? name))
                              (error who "invalid fields argument ~s" sv))
                            (vector-set! v (+ i n1) 
                              (cons (case m/u
                                      [(mutable)   #t]
                                      [(immutable) #f]
                                      [else 
                                       (error who "invalid fields argument ~s" sv)]) 
                                    name)))
                          (error who "invalid fields argument ~s" sv)))
                    (error who "invalid fields argument ~s" sv)))
              (f (add1 i))))
          v)))
    (define generate-rtd
      (lambda (name parent uid sealed? opaque? fields)
        (cond
          [(rtd? parent)
           (when (rtd-sealed? parent) 
             (error who "cannot extend sealed parent ~s" parent))
           (make-rtd-aux name parent uid sealed? 
             (or opaque? (rtd-opaque? parent))
             (convert-fields (rtd-fields parent) fields))]
          [(eqv? parent #f) 
           (make-rtd-aux name parent uid sealed? opaque?
             (convert-fields '#() fields))]
          [else (error who "~s is not a valid parent" parent)])))
    (define (same-fields-as-rtd? fields rtd)
      (let* ([fv (rtd-fields rtd)]
             [n (vector-length fv)])
        (and (vector? fields)
             (= (vector-length fields) n)
             (let f ([i 0])
               (or (= i n) 
                   (let ([a (vector-ref fields i)]
                         [b (vector-ref fv i)])
                     (and
                       (pair? a)
                       (case (car a) 
                         [(mutable) (eqv? (car b) #t)]
                         [(immutable) (eqv? (car b) #f)]
                         [else #f])
                       (let ([a (cdr a)])
                         (and (pair? a)
                              (null? (cdr a))
                              (eq? (car a) (cdr b))))
                       (f (+ i 1)))))))))
    (define make-nongenerative-rtd 
      (lambda (name parent uid sealed? opaque? fields)
        (cond
          [(lookup-rtd uid) =>
           (lambda (rtd) 
             (unless
               (and (eqv? name (rtd-name rtd))
                    (eqv? parent (rtd-parent rtd))
                    (eqv? sealed? (rtd-sealed? rtd))
                    (eqv? opaque? (rtd-opaque? rtd))
                    (same-fields-as-rtd? fields rtd))
               (error who "invalid arguments"))
             rtd)]
          [else
           (let ([rtd (generate-rtd name parent uid sealed? opaque? fields)])
             (intern-rtd! uid rtd)
             rtd)])))
    (define make-record-type-descriptor
      (lambda (name parent uid sealed? opaque? fields)
        (unless (symbol? name)
          (error who "~s is not a valid record type name" name))
        (unless (boolean? sealed?)
          (error who "~s is not a valid sealed? argument" sealed?))
        (unless (boolean? opaque?)
          (error who "~s is not a valid opaque? argument" opaque?))
        (cond
          [(symbol? uid) 
           (make-nongenerative-rtd name parent uid sealed? opaque? fields)]
          [(eqv? uid #f) 
           (generate-rtd name parent uid sealed? opaque? fields)]
          [else (error who "~s is not a valid uid" uid)]))))

  (define-record rcd (rtd pproc proc))
  (define make-record-constructor-descriptor
    (lambda (rtd parent protocol)
      (define who 'make-record-constructor-descriptor)
      (define (make-rcd/default-proto&prcd rtd) 
        (make-rcd rtd #f #f))
      (define (make-rcd/default-proto rtd parent)
        (cond
          [(not parent) 
           (make-rcd/default-proto&prcd rtd)]
          [(rcd? parent) 
           (error who "BUG1")]
          [else (error who "~s is not a valid record constructor descriptor"
                       parent)]))
      (define (make-rcd/procedure-proto rtd parent protocol) 
        (error who "BUG2"))
      (unless (rtd? rtd)
        (error who "~s is not an rtd" rtd))
      (cond
        [(not protocol) 
         (make-rcd/default-proto rtd parent)]
        [(procedure? protocol) 
         (make-rcd/procedure-proto rtd parent protocol)]
        [else (error who "~s is not a valid protocol" protocol)])))

  (define (iota i n)
    (if (= i n)
        '()
        (cons i (iota (+ i 1) n))))
  (define (sym n)
    (string->symbol (format "v~s" n)))

  (define (default-constructor-maker n) 
    ;;; FIXME: should cache compiled procedures
    (let ([vars (map sym (iota 0 n))])
      (eval `(lambda (rtd) 
               (lambda ,vars 
                 ($record rtd . ,vars)))
            (environment '(ikarus) '(ikarus system $records)))))



  (define (record-constructor rcd)
    (define who 'record-constructor)
    (unless (rcd? rcd)
      (error who "~s is not a record constructor descriptor" rcd))
    (let ([rtd (rcd-rtd rcd)]
          [pproc (rcd-pproc rcd)]
          [proc (rcd-proc rcd)])
      (cond
        [(not pproc)
         (cond
           [(not proc) 
            ((default-constructor-maker (rtd-size rtd)) rtd)]
           [else (error who "BUG")])]
        [else (error who "BUG")])))

  (define (record-accessor rtd k) 
    (define who 'record-accessor)
    (unless (rtd? rtd)
      (error who "~s is not an rtd" rtd))
    (unless (and (fixnum? k) (fx>= k 0)) 
      (error who "~s is not a valid index" k))
    (let ([sz (rtd-size rtd)]
          [p (rtd-parent rtd)])
      (let ([i (if p (+ k (rtd-size p)) k)])
        (unless (fx< i sz) 
          (error who "~s is not a valid index" k))
        (lambda (x) 
          (cond
            [($record/rtd? x rtd) ($record-ref x i)]
            [($record? x)
             (let ([xrtd ($record-rtd x)])
               (unless (rtd? xrtd) 
                 (error who "~s is not of type ~s" x rtd))
               (let f ([prtd (rtd-parent xrtd)] [rtd rtd] [x x] [i i])
                 (cond
                   [(eq? prtd rtd) ($record-ref x i)]
                   [(not prtd) 
                    (error who "~s is not of type ~s" x rtd)]
                   [else (f (rtd-parent prtd) rtd x i)])))]
            [else (error who "~s is not of type ~s" x rtd)])))))

  (define (record-mutator rtd k) 
    (define who 'record-mutator)
    (unless (rtd? rtd)
      (error who "~s is not an rtd" rtd))
    (unless (and (fixnum? k) (fx>= k 0)) 
      (error who "~s is not a valid index" k))
    (let ([sz (rtd-size rtd)]
          [p (rtd-parent rtd)])
      (let ([i (if p (+ k (rtd-size p)) k)])
        (unless (fx< i sz) 
          (error who "~s is not a valid index" k))
        (unless (car (vector-ref (rtd-fields rtd) k))
          (error who "field ~s of ~s is not mutable" k rtd))
        (lambda (x v) 
          (cond
            [($record/rtd? x rtd) ($record-set! x i v)]
            [($record? x)
             (let ([xrtd ($record-rtd x)])
               (unless (rtd? xrtd) 
                 (error who "~s is not of type ~s" x rtd))
               (let f ([prtd (rtd-parent xrtd)] [rtd rtd] [x x] [i i] [v v])
                 (cond
                   [(eq? prtd rtd) ($record-set! x i v)]
                   [(not prtd) 
                    (error who "~s is not of type ~s" x rtd)]
                   [else (f (rtd-parent prtd) rtd x i v)])))]
            [else (error who "~s is not of type ~s" x rtd)])))))

  (define (record-predicate rtd) 
    (define who 'record-predicate)
    (unless (rtd? rtd)
      (error who "~s is not an rtd" rtd))
    (let ([sz (rtd-size rtd)]
          [p (rtd-parent rtd)])
       (lambda (x) 
         (cond
           [($record/rtd? x rtd) #t]
           [($record? x)
            (let ([xrtd ($record-rtd x)])
              (and (rtd? xrtd) 
                   (let f ([prtd (rtd-parent xrtd)] [rtd rtd])
                     (cond
                       [(eq? prtd rtd) #t]
                       [(not prtd)     #f]
                       [else (f (rtd-parent prtd) rtd)]))))]
           [else #f]))))


                  


)
