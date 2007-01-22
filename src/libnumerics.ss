
(let ()
  (define (flonum? x)
    (foreign-call "ikrt_is_flonum" x))
  (define (flonum->string x)
    (or (foreign-call "ikrt_flonum_to_string" x)
        (error 'flonum->string "~s is not a flonum" x)))
  (define (string->flonum x)
    (cond
      [(string? x) (foreign-call "ikrt_string_to_flonum" x)]
      [else 
       (error 'string->flonum "~s is not a string" x)]))

  (primitive-set! 'flonum? flonum?)
  (primitive-set! 'flonum->string flonum->string)
  (primitive-set! 'string->flonum string->flonum)
)



(let ()
  
  (define bignum? 
    ; FIXME: temporary definition.  Compiler should be made aware
    ; of numeric representation once it's stable enough.
    (lambda (x)
      (foreign-call "ikrt_isbignum" x)))

  (define (fixnum->flonum x)
    (foreign-call "ikrt_fixnum_to_flonum" x))
  (define (bignum->flonum x)
    (foreign-call "ikrt_bignum_to_flonum" x))
  (define ($fl+ x y)
    (foreign-call "ikrt_fl_plus" x y))
  (define ($fl- x y)
    (foreign-call "ikrt_fl_minus" x y))
  (define ($fl* x y)
    (foreign-call "ikrt_fl_times" x y))

  (define binary+
    (lambda (x y)
      (cond
        [(fixnum? x) 
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxfxplus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_fxbnplus" x y)]
           [(flonum? y)
            ($fl+ (fixnum->flonum x) y)]
           [else 
            (error '+ "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnplus" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnplus" x y)]
           [(flonum? y)
            ($fl+ (bignum->flonum x) y)]
           [else 
            (error '+ "~s is not a number" y)])]
        [(flonum? x)
         (cond
           [(fixnum? y)
            ($fl+ x (fixnum->flonum y))]
           [(bignum? y)
            ($fl+ x (bignum->flonum y))]
           [(flonum? y)
            ($fl+ x y)]
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
           [(flonum? y)
            ($fl- (fixnum->flonum x) y)]
           [else 
            (error '- "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_bnfxminus" x y)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnminus" x y)]
           [(flonum? y)
            ($fl- (bignum->flonum x) y)]
           [else 
            (error '- "~s is not a number" y)])]
        [(flonum? x)
         (cond
           [(fixnum? y)
            ($fl- x (fixnum->flonum y))]
           [(bignum? y)
            ($fl- x (bignum->flonum y))]
           [(flonum? y)
            ($fl- x y)]
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
           [(flonum? y)
            ($fl* (fixnum->flonum x) y)]
           [else 
            (error '* "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (foreign-call "ikrt_fxbnmult" y x)]
           [(bignum? y)
            (foreign-call "ikrt_bnbnmult" x y)]
           [(flonum? y)
            ($fl* (bignum->flonum x) y)]
           [else 
            (error '* "~s is not a number" y)])]
        [(flonum? x)
         (cond
           [(fixnum? y)
            ($fl* x (fixnum->flonum y))]
           [(bignum? y)
            ($fl* x (bignum->flonum y))]
           [(flonum? y)
            ($fl* x y)]
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

  (define binary/
    (lambda (x y)
      (cond
        [(flonum? x)
         (cond
           [(flonum? y) 
            (foreign-call "ikrt_fl_div" x y)]
           [(fixnum? y)
            (foreign-call "ikrt_fl_div" x (fixnum->flonum y))]
           [else (error '/ "unspported ~s ~s" x y)])]
        [(fixnum? x)
         (cond
           [(flonum? y)
            (foreign-call "ikrt_fl_div" (fixnum->flonum x) y)]
           [else (error '/ "unsupported ~s ~s" x y)])]
        [else (error '/ "unsupported ~s ~s" x y)])))

  (define /
    (case-lambda
      [(x y) (binary/ x y)]
      [(x) 
       (cond
         [(flonum? x) (foreign-call "ikrt_fl_invert" x)]
         [else (error '/ "unspported argument ~s" x)])]
      [(x y z . rest)
       (let f ([a (binary/ x y)] [b z] [ls rest])
         (cond
           [(null? rest) (binary/ a b)]
           [else (f (binary/ a b) (car ls) (cdr ls))]))]))

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
          (bignum? x)
          (flonum? x))))

  (define complex?
    (lambda (x) (number? x)))
  
  (define real?
    (lambda (x) (number? x)))

  (define rational?
    (lambda (x) 
      (cond
        [(fixnum? x) #t]
        [(bignum? x) #t]
        [(flonum? x) #f]
        [else (error 'rational? "~s is not a number" x)])))

  (define integer?
    (lambda (x) (number? x)))

  (define exact?
    (lambda (x) 
      (cond
        [(fixnum? x) #t]
        [(bignum? x) #t]
        [(flonum? x) #f]
        [else 
         (error 'exact? "~s is not a number" x)])))

  (define exact->inexact
    (lambda (x)
      (cond
        [(fixnum? x) (fixnum->flonum x)]
        [(bignum? x) (bignum->flonum x)]
        [else
         (error 'exact->inexact 
                "~s is not an exact number" x)])))

  (define inexact?
    (lambda (x) 
      (cond
        [(fixnum? x) #f]
        [(bignum? x) #f]
        [(flonum? x) #t]
        [else 
         (error 'inexact? "~s is not a number" x)])))

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
        [(flonum? x) (foreign-call "ikrt_flonum_to_string" x)]
        [else (error 'number->string "~s is not a number" x)])))

  (define modulo
    (lambda (n m)
      (cond
        [(fixnum? n)
         (cond
           [(fixnum? m) ($fxmodulo n m)]
           [else (error 'modulo "unsupported ~s" m)])]
        [else (error 'modulo "unsupported ~s" n)])))

  (define-syntax mk<
    (syntax-rules ()
      [(_ name fxfx< fxbn< bnfx< bnbn<
               fxfl< flfx< bnfl< flbn< flfl<)
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
               [(flonum? y)
                (if (null? ls)
                    (fxfl< x y)
                    (if (fxfl< x y)
                        (flloopt y (car ls) (cdr ls))
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
               [(flonum? y)
                (if (null? ls)
                    (bnfl< x y)
                    (if (bnfl< x y)
                        (flloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define flloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (flfx< x y)
                    (if (flfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (flbn< x y)
                    (if (flbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(flonum? y)
                (if (null? ls)
                    (flfl< x y)
                    (if (flfl< x y)
                        (flloopt y (car ls) (cdr ls))
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
                 [(flonum? y) (fxfl< x y)]
                 [else (err y)])]
              [(bignum? x)
               (cond
                 [(fixnum? y) (bnfx< x y)]
                 [(bignum? y) (bnbn< x y)]
                 [(flonum? y) (bnfl< x y)]
                 [else (err y)])]
              [(flonum? x)
               (cond
                 [(fixnum? y) (flfx< x y)]
                 [(bignum? y) (flbn< x y)]
                 [(flonum? y) (flfl< x y)]
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
                    [(flonum? z)
                     (and (fxfx< x y) (fxfl< y z))]
                    [else (err z)])]
                 [(bignum? y)
                  (cond
                    [(fixnum? z) #f]
                    [(bignum? z) 
                     (and (fxbn< x y) (bnbn< y z))]
                    [(flonum? z)
                     (and (fxbn< x y) (bnfl< y z))]
                    [else (err z)])]
                 [(flonum? y)
                  (cond
                    [(fixnum? z) 
                     (and (fxfx< x z) 
                          (fxfl< x y)
                          (flfx< y z))]
                    [(bignum? z)
                     (and (fxbn< x z)
                          (fxfl< x y)
                          (flbn< y z))]
                    [(flonum? z)
                     (and (flfl< y z)
                          (fxfl< x y))]
                    [else (err z)])]
                 [else (err y)])]
              [(bignum? x)
               (cond
                 [(fixnum? y)
                  (cond
                    [(fixnum? z) (and (fxfx< y z) (bnfx< x y))]
                    [(bignum? z)
                     (and (bnfx< x y) (bnfx< y z))]
                    [(flonum? z)
                     (and (bnfx< x y) (fxfl< y z))]
                    [else (err z)])]
                 [(bignum? y)
                  (cond
                    [(fixnum? z) (and (bnfx< y z) (bnbn< x y))]
                    [(bignum? z) (and (bnbn< x y) (bnbn< y z))]
                    [(flonum? z) (and (bnfl< y z) (bnbn< x y))]
                    [else (err z)])]
                 [(flonum? y) 
                  (cond
                    [(fixnum? z) 
                     (and (flfx< y z) (bnfl< x y))]
                    [(bignum? z)
                     (and (bnfl< x y) (flbn< y z))]
                    [(flonum? z)
                     (and (flfl< y z) (bnfl< x y))]
                    [else (err z)])]
                 [else (err y)])]
              [(flonum? x) 
               (cond
                 [(fixnum? y)  
                  (cond
                    [(fixnum? z)
                     (and (fxfx< y z) (flfx< x y))]
                    [(bignum? z)
                     (and (flfx< x y) (fxbn< y z))]
                    [(flonum? z)
                     (and (flfx< x y) (fxfl< y z))]
                    [else (err z)])]
                 [(bignum? y)  
                  (cond
                    [(fixnum? z)
                     (and (bnfx< y z) (flbn< x y))]
                    [(bignum? z)
                     (and (bnbn< y z) (flbn< x y))]
                    [(flonum? z)
                     (and (flbn< x y) (bnfl< y z))]
                    [else (err z)])]
                 [(flonum? y) 
                  (cond
                    [(fixnum? z)
                     (and (flfx< y z) (flfl< x y))]
                    [(bignum? z)
                     (and (flfl< x y) (flbn< y z))]
                    [(flonum? z)
                     (and (flfl< x y) (flfl< y z))]
                    [else (err z)])]
                 [else (err y)])]
              [else (err x)])]
           [(x) (if (number? x) #t (err x))]
           [(x y . ls) 
            (cond
              [(fixnum? x) (fxloopt x y ls)]
              [(bignum? x) (bnloopt x y ls)]
              [(flonum? x) (flloopt x y ls)]
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

  (define-syntax flcmp
    (syntax-rules ()
      [(_ flfl? flfx? fxfl? flbn? bnfl? fl?)
       (begin
         (define-syntax flfl? 
           (syntax-rules () [(_ x y) (fl? x y)]))
         (define-syntax flfx? 
           (syntax-rules () [(_ x y) (fl? x (fixnum->flonum y))]))
         (define-syntax flbn? 
           (syntax-rules () [(_ x y) (fl? x (bignum->flonum y))]))
         (define-syntax fxfl? 
           (syntax-rules () [(_ x y) (fl? (fixnum->flonum x) y)]))
         (define-syntax bnfl? 
           (syntax-rules () [(_ x y) (fl? (bignum->flonum x) y)])))]))

  (define-syntax $fl=
    (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_equal" x y)]))
  (define-syntax $fl<
    (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less" x y)]))
  (define-syntax $fl<=
    (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less_or_equal" x y)]))
  (define-syntax $fl>
    (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less" y x)]))
  (define-syntax $fl>=
    (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less_or_equal" y x)]))

  (flcmp flfl= flfx= fxfl= flbn= bnfl= $fl=)
  (flcmp flfl< flfx< fxfl< flbn< bnfl< $fl<)
  (flcmp flfl> flfx> fxfl> flbn> bnfl> $fl>)
  (flcmp flfl<= flfx<= fxfl<= flbn<= bnfl<= $fl<=)
  (flcmp flfl>= flfx>= fxfl>= flbn>= bnfl>= $fl>=)


  (primitive-set! '+ +)
  (primitive-set! '- -)
  (primitive-set! '* *)
  (primitive-set! '/ /)
  (primitive-set! '= (mk< = #%$fx= false false bnbn= 
                          fxfl= flfx= bnfl= flbn= flfl=))
  (primitive-set! '< (mk< < #%$fx< fxbn< bnfx< bnbn<
                          fxfl< flfx< bnfl< flbn< flfl<))
  (primitive-set! '> (mk< > #%$fx> fxbn> bnfx> bnbn>
                          fxfl> flfx> bnfl> flbn> flfl>))
  (primitive-set! '<= (mk< <= #%$fx<= fxbn< bnfx< bnbn<=
                          fxfl<= flfx<= bnfl<= flbn<= flfl<=))
  (primitive-set! '>= (mk< >= #%$fx>= fxbn> bnfx> bnbn>=
                          fxfl>= flfx>= bnfl>= flbn>= flfl>=))
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

  (primitive-set! 'sin
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_sin" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_sin" x)]
        [else (error 'sin "unsupported ~s" x)])))

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
  (primitive-set! 'exact->inexact exact->inexact)
  (primitive-set! 'modulo modulo)

  )
