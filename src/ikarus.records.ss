

(library (ikarus records)
  (export
    make-record-type record-type-name record-type-symbol
    record-type-field-names record-constructor record-predicate
    record-field-accessor record-field-mutator record?  record-rtd
    (rename (record-rtd record-type-descriptor)) 
    record-name record-printer record-length record-ref record-set!)

  (import
    (ikarus system $records)
    (ikarus system $pairs)
    (ikarus system $fx)
    (except (ikarus)
      make-record-type record-type-name record-type-symbol
      record-type-field-names record-constructor record-predicate
      record-field-accessor record-field-mutator record? record-rtd
      record-type-descriptor record-name record-printer record-length
      record-ref record-set!)
    (only (scheme)
          set-top-level-value! top-level-value top-level-bound?))



  (define rtd?
    (lambda (x)
      (and ($record? x)
           (eq? ($record-rtd x) (base-rtd)))))

  (define rtd-name
    (lambda (rtd)
      ($record-ref rtd 0)))

  (define rtd-length
    (lambda (rtd)
      ($record-ref rtd 1)))

  (define rtd-fields
    (lambda (rtd)
      ($record-ref rtd 2)))

  (define rtd-printer
    (lambda (rtd)
      ($record-ref rtd 3)))

  (define rtd-symbol
    (lambda (rtd)
      ($record-ref rtd 4)))

  (define set-rtd-name!
    (lambda (rtd name)
      ($record-set! rtd 0 name)))
 
  (define set-rtd-length!
    (lambda (rtd n)
      ($record-set! rtd 1 n)))

  (define set-rtd-fields!
    (lambda (rtd fields)
      ($record-set! rtd 2 fields)))

  (define set-rtd-printer!
    (lambda (rtd printer)
      ($record-set! rtd 3 printer)))
   
  (define set-rtd-symbol!
    (lambda (rtd symbol)
      ($record-set! rtd 4 symbol)))

  (define make-rtd
    (lambda (name fields printer symbol)
      ($record (base-rtd) name (length fields) fields printer symbol)))

  (define verify-field
    (lambda (x)
      (unless (symbol? x) 
        (error 'make-record-type "~s is not a valid field name" x))))
  
  (define set-fields
    (lambda (r f* i n)
      (cond
        [(null? f*)
         (if ($fx= i n)
             r
             #f)]
        [($fx< i n)
         (if (null? f*)
             #f
             (begin
               ($record-set! r i ($car f*))
               (set-fields r ($cdr f*) ($fxadd1 i) n)))]
        [else #f])))

  (define make-record-type
    (case-lambda
      [(name fields)
       (unless (string? name)
         (error 'make-record-type "name must be a string, got ~s" name))
       (unless (list? fields)
         (error 'make-record-type "fields must be a list, got ~s" fields))
       (for-each verify-field fields)
       (let ([g (gensym name)])
         (let ([rtd (make-rtd name fields #f g)])
           (set-top-level-value! g rtd)
           rtd))]
      [(name fields g)
       (unless (string? name)
         (error 'make-record-type "name must be a string, got ~s" name))
       (unless (list? fields)
         (error 'make-record-type "fields must be a list, got ~s" fields))
       (for-each verify-field fields)
       (cond
         [(top-level-bound? g)
          (let ([rtd (top-level-value g)])
            (unless (and (string=? name (record-type-name rtd))
                         (equal? fields (record-type-field-names rtd)))
              (error 'make-record-type "definition mismatch"))
            rtd)]
         [else
          (let ([rtd (make-rtd name fields #f g)])
            (set-top-level-value! g rtd)
            rtd)])]))

  (define record-type-name
    (lambda (rtd)
      (unless (rtd? rtd)
        (error 'record-type-name "~s is not an rtd" rtd))
      (rtd-name rtd)))

  (define record-type-symbol
    (lambda (rtd)
      (unless (rtd? rtd)
        (error 'record-type-symbol "~s is not an rtd" rtd))
      (rtd-symbol rtd)))
  
  (define record-type-field-names
    (lambda (rtd)
      (unless (rtd? rtd)
        (error 'record-type-field-names "~s is not an rtd" rtd))
      (rtd-fields rtd)))
 

  (define record-constructor
    (lambda (rtd)
      (unless (rtd? rtd)
        (error 'record-constructor "~s is not an rtd"))
      (lambda args
        (let ([n (rtd-length rtd)])
          (let ([r ($make-record rtd n)])
            (or (set-fields r args 0 n)
                (error 'record-constructor 
                  "incorrect number of arguments to the constructor of ~s" 
                  rtd)))))))
  
  (define record-predicate
    (lambda (rtd)
      (unless (rtd? rtd)
        (error 'record-predicate "~s is not an rtd"))
      (lambda (x)
        (and ($record? x)
             (eq? ($record-rtd x) rtd)))))

  (define field-index 
    (lambda (i rtd who)
      (cond
        [(fixnum? i)
         (unless (and ($fx>= i 0) ($fx< i (rtd-length rtd)))
           (error who "~s is out of range for rtd ~s" rtd))
         i]
        [(symbol? i)
         (letrec ([lookup
                   (lambda (n ls)
                     (cond
                       [(null? ls) 
                        (error who "~s is not a field in ~s" rtd)]
                       [(eq? i ($car ls)) n]
                       [else (lookup ($fx+ n 1) ($cdr ls))]))])
           (lookup 0 (rtd-fields rtd)))]
        [else (error who "~s is not a valid index" i)])))

  (define record-field-accessor
    (lambda (rtd i)
      (unless (rtd? rtd)
        (error 'record-field-accessor "~s is not an rtd" rtd))
      (let ([i (field-index i rtd 'record-field-accessor)])
        (lambda (x)
          (unless (and ($record? x) 
                       (eq? ($record-rtd x) rtd))
            (error 'record-field-accessor "~s is not of type ~s" x rtd))
          ($record-ref x i)))))

  (define record-field-mutator
    (lambda (rtd i)
      (unless (rtd? rtd)
        (error 'record-field-mutator "~s is not an rtd" rtd))
      (let ([i (field-index i rtd 'record-field-mutator)])
        (lambda (x v)
          (unless (and ($record? x) 
                       (eq? ($record-rtd x) rtd))
            (error 'record-field-mutator "~s is not of type ~s" x rtd))
          ($record-set! x i v)))))

  (define record?
    (lambda (x . rest)
      (if (null? rest)
          ($record? x)
          (let ([rtd ($car rest)])
            (unless (null? ($cdr rest))
              (error 'record? "too many arguments"))
            (unless (rtd? rtd)
              (error 'record? "~s is not an rtd"))
            (and ($record? x)
                 (eq? ($record-rtd x) rtd))))))

  (define record-rtd
    (lambda (x)
      (if ($record? x)
          ($record-rtd x)
          (error 'record-rtd "~s is not a record" x))))

  (define record-length
    (lambda (x)
      (if ($record? x)
          (rtd-length ($record-rtd x))
          (error 'record-length "~s is not a record" x))))
            
  (define record-name
    (lambda (x)
      (if ($record? x)
          (rtd-name ($record-rtd x))
          (error 'record-name "~s is not a record" x))))

  (define record-printer
    (lambda (x)
      (if ($record? x)
          (rtd-printer ($record-rtd x))
          (error 'record-printer "~s is not a record" x))))

  (define record-ref
    (lambda (x i)
      (unless ($record? x) (error 'record-ref "~s is not a record" x))
      (unless (fixnum? i) (error 'record-ref "~s is not a valid index" i))
      (let ([n (rtd-length ($record-rtd x))])
        (unless (and ($fx>= i 0) ($fx< i n))
          (error 'record-ref "index ~s is out of range for ~s" i x))
        ($record-ref x i))))

  (define record-set!
    (lambda (x i v)
      (unless ($record? x) (error 'record-set! "~s is not a record" x))
      (unless (fixnum? i) (error 'record-set! "~s is not a valid index" i))
      (let ([n (rtd-length ($record-rtd x))])
        (unless (and ($fx>= i 0) ($fx< i n))
          (error 'record-set! "index ~s is out of range for ~s" i x))
        ($record-set! x i v))))

  (set-rtd-fields! (base-rtd) '(name fields length printer symbol))
  (set-rtd-name! (base-rtd) "base-rtd")
  (set-rtd-printer! (base-rtd)
    (lambda (x p)
      (unless (rtd? x)
        (error 'record-type-printer "not an rtd"))
      (display "#<" p)
      (display (rtd-name x) p)
      (display " rtd>" p)))
  )
