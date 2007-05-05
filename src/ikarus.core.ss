

(library (ikarus core)
  (export)
  (import (scheme))


(primitive-set! 'eof-object
  (lambda () (eof-object)))

(primitive-set! 'void
  (lambda () (void)))
  
(primitive-set! 'integer->char
  (lambda (n)
    (unless (fixnum? n)
      (error 'integer->char "~s is not a fixnum" n))
    (unless (and ($fx>= n 0)
                 ($fx<= n 255))
      (error 'integer->char "~s is out of range[0..255]" n))
    ($fixnum->char n)))
  
(primitive-set! 'char->integer 
  (lambda (x) 
    (unless (char? x)
      (error 'char->integer "~s is not a character" x))
    ($char->fixnum x)))

(primitive-set! 'gensym?
  (lambda (x)
    (and (symbol? x) 
         (let ([s ($symbol-unique-string x)])
           (and s #t)))))


(primitive-set! 'top-level-value
  (lambda (x)
    (unless (symbol? x)
      (error 'top-level-value "~s is not a symbol" x))
    (let ([v ($symbol-value x)])
      (when ($unbound-object? v)
        (error 'top-level-value "unbound variable ~s" x))
      v)))

(primitive-set! 'top-level-bound?
  (lambda (x)
    (unless (symbol? x)
      (error 'top-level-bound? "~s is not a symbol" x))
    (not ($unbound-object? ($symbol-value x)))))

(primitive-set! 'set-top-level-value!
  (lambda (x v)
    (unless (symbol? x)
      (error 'set-top-level-value! "~s is not a symbol" x))
    ($set-symbol-value! x v)))
 
(primitive-set! 'primitive-set!
  (lambda (x v)
    (unless (symbol? x)
      (error 'primitive-set! "~s is not a symbol" x))
    (primitive-set! x v)
    (set-top-level-value! x v)))

(primitive-set! 'string->symbol
  (lambda (x)
    (unless (string? x) 
      (error 'string->symbol "~s is not a string" x))
    (foreign-call "ikrt_string_to_symbol" x)))
  
(primitive-set! 'gensym
  (case-lambda
    [() ($make-symbol #f)]
    [(s) 
     (if (string? s)
         ($make-symbol s)
         (if (symbol? s)
             ($make-symbol ($symbol-string s))
             (error 'gensym "~s is neither a string nor a symbol" s)))]))

(primitive-set! 'putprop
  (lambda (x k v)
    (unless (symbol? x) (error 'putprop "~s is not a symbol" x))
    (unless (symbol? k) (error 'putprop "~s is not a symbol" k))
    (let ([p ($symbol-plist x)])
      (cond
        [(assq k p) => (lambda (x) (set-cdr! x v))]
        [else 
         ($set-symbol-plist! x (cons (cons k v) p))]))))

(primitive-set! 'getprop
  (lambda (x k)
    (unless (symbol? x) (error 'getprop "~s is not a symbol" x))
    (unless (symbol? k) (error 'getprop "~s is not a symbol" k))
    (let ([p ($symbol-plist x)])
      (cond
        [(assq k p) => cdr]
        [else #f]))))

(primitive-set! 'remprop
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

(primitive-set! 'property-list
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

(primitive-set! 'apply
  (let ()
    (define (err f ls)
      (if (procedure? f)
          (error 'apply "not a list")
          (error 'apply "~s is not a procedure" f)))
    (define (fixandgo f a0 a1 ls p d)
      (cond
        [(null? ($cdr d))
         (let ([last ($car d)])
           ($set-cdr! p last)
           (if (and (procedure? f) (list? last))
               ($$apply f a0 a1 ls)
               (err f last)))]
        [else (fixandgo f a0 a1 ls d ($cdr d))]))
    (define apply
      (case-lambda
        [(f ls) 
         (if (and (procedure? f) (list? ls))
             ($$apply f ls)
             (err f ls))]
        [(f a0 ls)
         (if (and (procedure? f) (list? ls))
             ($$apply f a0 ls)
             (err f ls))]
        [(f a0 a1 ls)
         (if (and (procedure? f) (list? ls))
             ($$apply f a0 a1 ls)
             (err f ls))]
        [(f a0 a1 . ls)
         (fixandgo f a0 a1 ls ls ($cdr ls))]))
    apply))
  



(primitive-set! 'gensym->unique-string
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




(primitive-set! 'gensym-prefix
  (make-parameter
    "g"
    (lambda (x)
      (unless (string? x)
        (error 'gensym-prefix "~s is not a string" x))
      x)))

(primitive-set! 'gensym-count
  (make-parameter
    0
    (lambda (x)
      (unless (and (fixnum? x) ($fx>= x 0))
        (error 'gensym-count "~s is not a valid count" x))
      x)))

(primitive-set! 'print-gensym
  (make-parameter
    #t
    (lambda (x)
      (unless (or (boolean? x) (eq? x 'pretty))
        (error 'print-gensym "~s is not in #t|#f|pretty" x))
      x)))



(primitive-set! 'pointer-value
  (lambda (x)
    (pointer-value x)))

(primitive-set! 'date-string
  (lambda ()
    (let ([s (make-string 10)])
      (foreign-call "ikrt_strftime" s "%F")
      s)))

(primitive-set! 'list*
  (lambda (fst . rest)
    (let f ([fst fst] [rest rest])
      (cond
        [(null? rest) fst]
        [else 
         (cons fst (f ($car rest) ($cdr rest)))]))))

(primitive-set! 'command-line-arguments
  (make-parameter ($arg-list)
    (lambda (x)
      (if (and (list? x) (andmap string? x))
          x
          (error 'command-list "invalid command-line-arguments ~s\n" x)))))



(let ()
  (define f
    (lambda (n i j)
      (cond
        [($fxzero? n) 
         (values (make-string i) j)]
        [else
         (let ([q ($fxquotient n 10)])
           (call-with-values
             (lambda () (f q ($fxadd1 i) j))
             (lambda (str j)
               (let ([r ($fx- n ($fx* q 10))])
                 (string-set! str j
                    ($fixnum->char ($fx+ r ($char->fixnum #\0))))
                 (values str ($fxadd1 j))))))])))
  (primitive-set! 'fixnum->string
    (lambda (x)
      (unless (fixnum? x) (error 'fixnum->string "~s is not a fixnum" x))
      (cond
        [($fxzero? x) "0"]
        [($fx> x 0) 
         (call-with-values
           (lambda () (f x 0 0))
           (lambda (str j) str))]
        [($fx= x -536870912) "-536870912"]
        [else
         (call-with-values
           (lambda () (f ($fx- 0 x) 1 1))
           (lambda (str j)
             ($string-set! str 0 #\-)
             str))]))))



(primitive-set! 'symbol->string
  (lambda (x)
    (unless (symbol? x)
      (error 'symbol->string "~s is not a symbol" x))
    (let ([str ($symbol-string x)])
      (or str
          (let ([ct (gensym-count)])
            (let ([str (string-append (gensym-prefix) (fixnum->string ct))])
              ($set-symbol-string! x str)
              (gensym-count ($fxadd1 ct))
              str))))))


(primitive-set! 'string->number
  (lambda (x)
    (define (convert-data str len pos? idx ac)
      (cond
        [($fx= idx len) (if pos? ac (- 0 ac))]
        [else
         (let ([c ($string-ref str idx)])
           (cond
             [(and ($char<= #\0 c) ($char<= c #\9))
              (convert-data str len pos? ($fxadd1 idx) 
                 (+ (* ac 10)
                    ($fx- ($char->fixnum c) ($char->fixnum #\0))))]
             [else #f]))]))
    (define (convert-data-init str len pos? idx c)
      (cond
        [($char= c #\0) 
         (if ($fx= idx len)
             0
             (convert-data-init str len pos? 
                ($fxadd1 idx) 
                ($string-ref str idx)))]
        [(and ($char<= #\1 c) ($char<= c #\9))
         (convert-data str len pos? idx
            ($fx- ($char->fixnum c) ($char->fixnum #\0)))]
        [else #f]))
    (define (convert-num str len pos?)
      (cond
        [($fx> len 1)
         (convert-data-init str len pos? 2 ($string-ref str 1))]
        [else #f]))
    (define (convert-sign str len)
      (cond
        [($fx> len 0)
         (let ([c ($string-ref str 0)])
           (case c
             [(#\+) (convert-num str len #t)]
             [(#\-) (convert-num str len #f)]
             [else
              (convert-data-init str len #t 1 c)]))]
        [else #f]))
    (cond
      [(string? x) 
       (convert-sign x ($string-length x))]
      [else (error 'string->number "~s is not a string" x)])))

)
