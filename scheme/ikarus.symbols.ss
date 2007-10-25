
(library (ikarus base symbols)
  (export gensym gensym? gensym->unique-string gensym-prefix
          gensym-count print-gensym string->symbol symbol->string
          getprop putprop remprop property-list
          top-level-value top-level-bound? set-top-level-value!
          symbol-value symbol-bound? set-symbol-value!
          reset-symbol-proc!)
  (import 
    (ikarus system $symbols)
    (ikarus system $pairs)
    (ikarus system $fx)
    (except (ikarus) gensym gensym? gensym->unique-string
      gensym-prefix gensym-count print-gensym
      string->symbol symbol->string
      getprop putprop remprop property-list
      top-level-value top-level-bound? set-top-level-value!
      symbol-value symbol-bound? set-symbol-value! reset-symbol-proc!))

  (define gensym
    (case-lambda
      [() ($make-symbol #f)]
      [(s) 
       (if (string? s)
           ($make-symbol s)
           (if (symbol? s)
               ($make-symbol ($symbol-string s))
               (error 'gensym "~s is neither a string nor a symbol" s)))]))

  (define gensym?
    (lambda (x)
      (and (symbol? x) 
           (let ([s ($symbol-unique-string x)])
             (and s #t)))))

  (define top-level-value
    (lambda (x)
      (unless (symbol? x)
        (error 'top-level-value "~s is not a symbol" x))
      (let ([v ($symbol-value x)])
        (when ($unbound-object? v)
          (error 'eval "unbound variable" 
            (string->symbol
              (symbol->string x))))
        v)))

  (define top-level-bound?
    (lambda (x)
      (unless (symbol? x)
        (error 'top-level-bound? "~s is not a symbol" x))
      (not ($unbound-object? ($symbol-value x)))))

  (define set-top-level-value!
    (lambda (x v)
      (unless (symbol? x)
        (error 'set-top-level-value! "~s is not a symbol" x))
      ($set-symbol-value! x v)))

  (define symbol-value
    (lambda (x)
      (unless (symbol? x)
        (error 'symbol-value "~s is not a symbol" x))
      (let ([v ($symbol-value x)])
        (when ($unbound-object? v)
          (error 'symbol-value "unbound ~s" x))
        v)))

  (define symbol-bound?
    (lambda (x)
      (unless (symbol? x)
        (error 'symbol-bound? "~s is not a symbol" x))
      (not ($unbound-object? ($symbol-value x)))))

  (define set-symbol-value!
    (lambda (x v)
      (unless (symbol? x)
        (error 'set-symbol-value! "~s is not a symbol" x))
      ($set-symbol-value! x v)
      ($set-symbol-proc! x 
        (if (procedure? v) v 
            (lambda args
              (error 'apply "~s is not a procedure" 
                     ($symbol-value x)))))))
         
  (define reset-symbol-proc!
    (lambda (x) 
      (let ([v ($symbol-value x)])
        ($set-symbol-proc! x
          (if (procedure? v)
              v
              (lambda args
                (let ([v ($symbol-value x)])
                  (if ($unbound-object? v)
                      (error 'eval "unbound variable" 
                        (string->symbol
                          (symbol->string x)))
                      (error 'apply "~s is not a procedure" v)))))))))

  (define string->symbol
    (lambda (x)
      (unless (string? x) 
        (error 'string->symbol "~s is not a string" x))
      (foreign-call "ikrt_string_to_symbol" x)))
  
  (define symbol->string
    (lambda (x)
      (unless (symbol? x)
        (error 'symbol->string "~s is not a symbol" x))
      (let ([str ($symbol-string x)])
        (or str
            (let ([ct (gensym-count)])
              ;;; FIXME: what if gensym-count is a bignum?
              (let ([str (string-append (gensym-prefix) (fixnum->string ct))])
                ($set-symbol-string! x str)
                (gensym-count ($fxadd1 ct))
                str))))))

  (define putprop
    (lambda (x k v)
      (unless (symbol? x) (error 'putprop "~s is not a symbol" x))
      (unless (symbol? k) (error 'putprop "~s is not a symbol" k))
      (let ([p ($symbol-plist x)])
        (cond
          [(assq k p) => (lambda (x) (set-cdr! x v))]
          [else 
           ($set-symbol-plist! x (cons (cons k v) p))]))))

  (define getprop
    (lambda (x k)
      (unless (symbol? x) (error 'getprop "~s is not a symbol" x))
      (unless (symbol? k) (error 'getprop "~s is not a symbol" k))
      (let ([p ($symbol-plist x)])
        (cond
          [(assq k p) => cdr]
          [else #f]))))

  (define remprop
    (lambda (x k)
      (unless (symbol? x) (error 'remprop "~s is not a symbol" x))
      (unless (symbol? k) (error 'remprop "~s is not a symbol" k))
      (let ([p ($symbol-plist x)])
        (unless (null? p)
          (let ([a ($car p)])
            (cond
              [(eq? ($car a) k) ($set-symbol-plist! x ($cdr p))]
              [else 
               (let f ([q p] [p ($cdr p)])
                 (unless (null? p)
                   (let ([a ($car p)])
                     (cond
                       [(eq? ($car a) k)
                        ($set-cdr! q ($cdr p))]
                       [else 
                        (f p ($cdr p))]))))]))))))

  (define property-list
    (lambda (x)
      (unless (symbol? x)
        (error 'property-list "~s is not a symbol" x))
      (letrec ([f 
                (lambda (ls ac)
                  (cond
                    [(null? ls) ac]
                    [else
                     (let ([a ($car ls)])
                       (f ($cdr ls) 
                          (cons ($car a) (cons ($cdr a) ac))))]))])
        (f ($symbol-plist x) '()))))

  (define gensym->unique-string
    (lambda (x)
      (unless (symbol? x)
        (error 'gensym->unique-string "~s is not a gensym" x))
      (let ([us ($symbol-unique-string x)])
        (cond
          [(string? us) us]
          [(not us)
           (error 'gensym->unique-string "~s is not a gensym" x)]
          [else
           (let f ([x x])
             (let ([id (uuid)])
               ($set-symbol-unique-string! x id)
               (cond
                 [(foreign-call "ikrt_intern_gensym" x) id]
                 [else (f x)])))]))))
  
  (define gensym-prefix
    (make-parameter
      "g"
      (lambda (x)
        (unless (string? x)
          (error 'gensym-prefix "~s is not a string" x))
        x)))
  
  (define gensym-count
    (make-parameter
      0
      (lambda (x)
        (unless (and (fixnum? x) ($fx>= x 0))
          (error 'gensym-count "~s is not a valid count" x))
        x)))
  
  (define print-gensym
    (make-parameter
      #t
      (lambda (x)
        (unless (or (boolean? x) (eq? x 'pretty))
          (error 'print-gensym "~s is not in #t|#f|pretty" x))
        x)))

  )

