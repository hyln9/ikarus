
(library (ikarus symbols)
  (export gensym gensym?
    string->symbol
    getprop putprop remprop property-list
    top-level-value top-level-bound? set-top-level-value!)
  (import 
    (only (scheme) $make-symbol $symbol-string $symbol-unique-string
          $symbol-value $set-symbol-value! $set-symbol-plist!
          $symbol-plist
          $car $cdr $set-cdr! $unbound-object?)
    (except (ikarus) gensym gensym?
      string->symbol
      getprop putprop remprop property-list
      top-level-value top-level-bound? set-top-level-value!))

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
          (error 'top-level-value "unbound variable ~s" x))
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

  (define string->symbol
    (lambda (x)
      (unless (string? x) 
        (error 'string->symbol "~s is not a string" x))
      (foreign-call "ikrt_string_to_symbol" x)))
  

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

  )

