
(let ()
  
  (define bignum? 
    ; FIXME: temporary definition.  Compiler should be made aware
    ; of numeric representation once it's stable enough.
    (lambda (x)
      (foreign-call "ikrt_isbignum" x)))

  (define binary+
    (lambda (x y)
      (cond
        [(fixnum? x) 
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxfxplus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnplus" x y)]
           [else 
            (error '+ "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnplus" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnplus" x y)]
           [else 
            (error '+ "~s is not a number" y)])]
        [else (error '+ "~s is not a number" x)])))

  (define binary-logand
    (lambda (x y)
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y) (#%$fxlogand x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnlogand" x y)]
           [else 
            (error 'logand "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnlogand" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnlogand" x y)]
           [else 
            (error 'logand "~s is not a number" y)])]
        [else (error 'logand "~s is not a number" x)])))


  (define binary-
    (lambda (x y)
      (cond
        [(fixnum? x) 
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxfxminus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnminus" x y)]
           [else 
            (error '- "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_bnfxminus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnminus" x y)]
           [else 
            (error '- "~s is not a number" y)])]
        [else (error '- "~s is not a number" x)])))

  (define binary*
    (lambda (x y)
      (cond
        [(fixnum? x) 
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxfxmult" x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnmult" x y)]
           [else 
            (error '* "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnmult" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnmult" x y)]
           [else 
            (error '* "~s is not a number" y)])]
        [else (error '* "~s is not a number" x)])))

  (define +
    (case-lambda
      [(x y) (binary+ x y)]
      [(x y z) (binary+ (binary+ x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(bignum? a) a]
         [else (error '+ "~s is not a number" a)])]
      [() 0]
      [(a b c d . e*)
       (let f ([ac (binary+ (binary+ (binary+ a b) c) d)]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary+ ac (car e*)) (cdr e*))]))]))

  (define logand
    (case-lambda
      [(x y) (binary-logand x y)]
      [(x y z) (binary-logand (binary-logand x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(bignum? a) a]
         [else (error 'logand "~s is not a number" a)])]
      [() -1]
      [(a b c d . e*)
       (let f ([ac (binary-logand (binary-logand (binary-logand a b) c) d)]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary-logand ac (car e*)) (cdr e*))]))]))

  (define -
    (case-lambda
      [(x y) (binary- x y)]
      [(x y z) (binary- (binary- x y) z)]
      [(a) (binary- 0 a)]
      [(a b c d . e*)
       (let f ([ac (binary- (binary- (binary- a b) c) d)]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary- ac (car e*)) (cdr e*))]))]))

  (define *
    (case-lambda
      [(x y) (binary* x y)]
      [(x y z) (binary* (binary* x y) z)]
      [(a)
       (cond
         [(fixnum? a) a]
         [(bignum? a) a]
         [else (error '* "~s is not a number" a)])]
      [() 1]
      [(a b c d . e*)
       (let f ([ac (binary* (binary* (binary* a b) c) d)]
               [e* e*])
         (cond
           [(null? e*) ac]
           [else (f (binary* ac (car e*)) (cdr e*))]))]))

  (define expt
    (lambda (n m)
      (cond
        [(#%$fxzero? m) 1]
        [(#%$fxzero? (#%$fxlogand m 1))
         (expt (binary* n n) (#%$fxsra m 1))]
        [else
         (binary* n (expt (binary* n n) (#%$fxsra m 1)))])))

  (define max
    (case-lambda
      [(x y)
       (cond
         [(fixnum? x) 
          (cond
            [(fixnum? y) 
             (if ($fx> x y) x y)]
            [(bignum? y)
             (if (positive-bignum? y) y x)]
            [else (error 'max "~s is not a number" y)])]
         [(bignum? x)
          (cond
            [(fixnum? y)
             (if (positive-bignum? x) x y)]
            [(bignum? y)
             (if (bnbn> x y) x y)]
            [else (error 'max "~s is not a number" y)])]
         [else (error 'max "~s is not a number" x)])]
      [(x y z . rest)
       (let f ([a (max x y)] [b z] [ls rest])
         (cond
           [(null? ls) (max a b)]
           [else
            (f (max a b) (car ls) (cdr ls))]))]
      [(x) 
       (if (number? x) 
           x 
           (error 'max "~s is not a number" x))]))

(define min
    (case-lambda
      [(x y)
       (cond
         [(fixnum? x) 
          (cond
            [(fixnum? y) 
             (if ($fx> x y) y x)]
            [(bignum? y)
             (if (positive-bignum? y) x y)]
            [else (error 'min "~s is not a number" y)])]
         [(bignum? x)
          (cond
            [(fixnum? y)
             (if (positive-bignum? x) y x)]
            [(bignum? y)
             (if (bnbn> x y) y x)]
            [else (error 'min "~s is not a number" y)])]
         [else (error 'min "~s is not a number" x)])]
      [(x y z . rest)
       (let f ([a (min x y)] [b z] [ls rest])
         (cond
           [(null? ls) (min a b)]
           [else
            (f (min a b) (car ls) (cdr ls))]))]
      [(x) 
       (if (number? x) 
           x 
           (error 'min "~s is not a number" x))]))

  (define number?
    (lambda (x)
      (or (fixnum? x)
          (bignum? x))))

  (define complex?
    (lambda (x) (number? x)))
  (define real?
    (lambda (x) (number? x)))
  (define rational?
    (lambda (x) (number? x)))
  (define integer?
    (lambda (x) (number? x)))
  (define exact?
    (lambda (x) 
      (or (number? x)
          (error 'exact? "~s is not a number" x))))

  (define inexact?
    (lambda (x) 
      (if (number? x)
          #f
          (error 'inexact? "~s is not a number" x))))

  (define positive-bignum?
    (lambda (x) 
      (foreign-call "ikrt_positive_bn" x)))


  (define even-bignum?
    (lambda (x) 
      (foreign-call "ikrt_even_bn" x)))

  (define ($fxeven? x)
    ($fxzero? ($fxlogand x 1)))

  (define (even? x)
    (cond
      [(fixnum? x) ($fxeven? x)]
      [(bignum? x) (even-bignum? x)]
      [else (error 'even? "~s is not an integer" x)]))

  (define (odd? x)
    (not
      (cond
        [(fixnum? x) ($fxeven? x)]
        [(bignum? x) (even-bignum? x)]
        [else (error 'odd? "~s is not an integer" x)])))

  (define number->string
    (lambda (x)
      (cond
        [(fixnum? x) (fixnum->string x)]
        [(bignum? x) (foreign-call "ikrt_bntostring" x)]
        [else (error 'number->string "~s is not a number" x)])))

  (define-syntax mk<
    (syntax-rules ()
      [(_ name fxfx< fxbn< bnfx< bnbn<)
       (let ()
         (define err
           (lambda (x) (error 'name "~s is not a number" x)))
         (define fxloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (fxfx< x y)
                    (if (fxfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (fxbn< x y)
                    (if (fxbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define bnloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (bnfx< x y)
                    (if (bnfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (bnbn< x y)
                    (if (bnbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define loopf
           (lambda (x ls)
             (cond
               [(number? x) 
                (or (null? ls) (loopf (car ls) (cdr ls)))]
               [else (err x)])))
         (case-lambda
           [(x y)
            (cond
              [(fixnum? x)
               (cond
                 [(fixnum? y) (fxfx< x y)]
                 [(bignum? y) (fxbn< x y)]
                 [else (err y)])]
              [(bignum? x)
               (cond
                 [(fixnum? y) (bnfx< x y)]
                 [(bignum? y) (bnbn< x y)]
                 [else (err y)])]
              [else (err x)])]
           [(x y z)
            (cond
              [(fixnum? x)
               (cond
                 [(fixnum? y)
                  (cond
                    [(fixnum? z) (and (fxfx< x y) (fxfx< y z))]
                    [(bignum? z)
                     (and (fxfx< x y) (fxbn< y z))]
                    [else (err z)])]
                 [(bignum? y)
                  (cond
                    [(fixnum? z) #f]
                    [(bignum? z) 
                     (and (fxbn< x y) (bnbn< y z))]
                    [else (err z)])]
                 [else (err y)])]
              [(bignum? x)
               (cond
                 [(fixnum? y)
                  (cond
                    [(fixnum? z) (and (fxfx< y z) (bnfx< x y))]
                    [(bignum? z)
                     (and (bnfx< x y) (bnfx< y z))]
                    [else (err z)])]
                 [(bignum? y)
                  (cond
                    [(fixnum? z) (and (bnfx< y z) (bnbn< x y))]
                    [(bignum? z) (and (bnbn< x y) (bnbn< y z))]
                    [else (err z)])]
                 [else (err y)])]
              [else (err x)])]
           [(x) (if (number? x) #t (err x))]
           [(x y . ls) 
            (cond
              [(fixnum? x) (fxloopt x y ls)]
              [(bignum? x) (bnloopt x y ls)]
              [else (err x)])]))]))

  (define-syntax false (syntax-rules () [(_ x y) #f]))
  (define-syntax bnbncmp
    (syntax-rules ()
      [(_ x y cmp)
       (cmp (foreign-call "ikrt_bnbncomp" x y) 0)]))
  (define-syntax bnbn= (syntax-rules () [(_ x y) (bnbncmp x y #%$fx=)]))
  (define-syntax bnbn< (syntax-rules () [(_ x y) (bnbncmp x y #%$fx<)]))
  (define-syntax bnbn> (syntax-rules () [(_ x y) (bnbncmp x y #%$fx>)]))
  (define-syntax bnbn<= (syntax-rules () [(_ x y) (bnbncmp x y #%$fx<=)]))
  (define-syntax bnbn>= (syntax-rules () [(_ x y) (bnbncmp x y #%$fx>=)]))
  (define-syntax fxbn< (syntax-rules () [(_ x y) (positive-bignum? y)]))
  (define-syntax bnfx< (syntax-rules () [(_ x y) (not (positive-bignum? x))]))
  (define-syntax fxbn> (syntax-rules () [(_ x y) (not (positive-bignum? y))]))
  (define-syntax bnfx> (syntax-rules () [(_ x y) (positive-bignum? x)]))




  (primitive-set! '+ +)
  (primitive-set! '- -)
  (primitive-set! '* *)
  (primitive-set! '= (mk< = #%$fx= false false bnbn=))
  (primitive-set! '< (mk< < #%$fx< fxbn< bnfx< bnbn<))
  (primitive-set! '> (mk< > #%$fx> fxbn> bnfx> bnbn>))
  (primitive-set! '<= (mk< <= #%$fx<= fxbn< bnfx< bnbn<=))
  (primitive-set! '>= (mk< >= #%$fx>= fxbn> bnfx> bnbn>=))
  (primitive-set! 'logand logand)
  (primitive-set! 'number? number?)
  (primitive-set! 'number->string number->string)

  (primitive-set! 'add1
    (lambda (x)
      (cond
        [(fixnum? x) 
         (foreign-call "ikrt_fxfxplus" x 1)]
        [(bignum? x)
         (foreign-call "ikrt_fxbnplus" 1 x)]
        [else (error 'add1 "~s is not a number" x)])))

  (primitive-set! 'sub1
    (lambda (x)
      (cond
        [(fixnum? x) 
         (foreign-call "ikrt_fxfxplus" x -1)]
        [(bignum? x)
         (foreign-call "ikrt_fxbnplus" -1 x)]
        [else (error 'sub1 "~s is not a number" x)])))

  (primitive-set! 'zero?
    (lambda (x)
      (cond
        [(fixnum? x) (eq? x 0)]
        [(bignum? x) #f]
        [else (error 'zero? "~s is not a number" x)])))

  (primitive-set! 'expt
    (lambda (n m)
      (unless (number? n)
        (error 'expt "~s is not a numebr" n))
      (cond
        [(fixnum? m) 
         (if (#%$fx>= m 0)
             (expt n m)
             (error 'expt "power should be positive, got ~s" m))]
        [(bignum? m) 
         (cond
           [(eq? n 0) 0]
           [(eq? n 1) 1]
           [(eq? n -1)
            (if (positive-bignum? m)
                (if (even-bignum? m)
                    1
                    -1)
                (error 'expt "power should be positive, got ~s" m))]
           [else 
            (if (positive-bignum? m)
                (error 'expt "(expt ~s ~s) is too big to compute" n m)
                (error 'expt "power should be positive, got ~s" m))])]
        [else (error 'expt "~s is not a number" m)])))

  (primitive-set! 'quotient
    (lambda (x y)
      (let-values ([(q r) (quotient+remainder x y)])
        q)))

  (primitive-set! 'remainder
    (lambda (x y)
      (let-values ([(q r) (quotient+remainder x y)])
        r)))

  (primitive-set! 'quotient+remainder
    (lambda (x y)
      (cond
        [(eq? y 0) 
         (error 'quotient+remainder
                "second argument must be non-zero")]
        [(fixnum? x) 
         (cond
           [(fixnum? y)
            (values (fxquotient x y)
                    (fxremainder x y))]
           [(bignum? y) (values 0 x)]
           [else (error 'quotient+remainder 
                        "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (let ([p (foreign-call "ikrt_bnfxdivrem" x y)])
              (values (car p) (cdr p)))]
           [(bignum? y)
            (let ([p (foreign-call "ikrt_bnbndivrem" x y)])
              (values (car p) (cdr p)))]
           [else (error 'quotient+remainder 
                        "~s is not a number" y)])]
        [else (error 'quotient+remainder 
                  "~s is not a number" x)])))

  (primitive-set! 'positive?
    (lambda (x)
      (cond
        [(fixnum? x) (#%$fx> x 0)]
        [(bignum? x) (positive-bignum? x)]
        [else (error 'positive? "~s is not a number" x)])))

  (primitive-set! 'negative?
    (lambda (x)
      (cond
        [(fixnum? x) (#%$fx< x 0)]
        [(bignum? x) (not (positive-bignum? x))]
        [else (error 'negative? "~s is not a number" x)])))

  (primitive-set! 'even? even?)
  (primitive-set! 'odd? odd?)
  (primitive-set! 'max max)
  (primitive-set! 'min min)
  (primitive-set! 'complex? complex?)
  (primitive-set! 'real? real?)
  (primitive-set! 'rational? rational?)
  (primitive-set! 'exact? exact?)
  (primitive-set! 'inexact? inexact?)
  (primitive-set! 'integer? integer?)
  )
