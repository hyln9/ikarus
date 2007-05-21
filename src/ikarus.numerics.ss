





(library (ikarus flonums)
  (export string->flonum flonum->string)
  (import 
    (ikarus system $bytevectors)
    (except (ikarus) flonum->string string->flonum))
  
  (define (flonum->string x)
    (utf8-bytevector->string
      (or (foreign-call "ikrt_flonum_to_bytevector" x
            ($make-bytevector 80))
          (error 'flonum->string "~s is not a flonum" x))))
  
  (define (string->flonum x)
    (cond
      [(string? x)
       (foreign-call "ikrt_bytevector_to_flonum" 
         (string->utf8-bytevector x))]
      [else 
       (error 'string->flonum "~s is not a string" x)])))



(library (ikarus generic-arithmetic)
  (export + - * / zero? = < <= > >= add1 sub1 quotient remainder
          positive? expt gcd lcm
          quotient+remainder number->string string->number)
  (import 
    (ikarus system $fx)
    (ikarus system $ratnums)
    (ikarus system $bignums)
    (ikarus system $chars)
    (ikarus system $strings)
    (except (ikarus) + - * / zero? = < <= > >= add1 sub1 quotient
            remainder quotient+remainder number->string positive?
            string->number expt gcd lcm))

  (define (fixnum->flonum x)
    (foreign-call "ikrt_fixnum_to_flonum" x))
  (define (bignum->flonum x)
    (foreign-call "ikrt_bignum_to_flonum" x))
  (define (ratnum->flonum x) 
    (binary/ (exact->inexact ($ratnum-n x)) 
             (exact->inexact ($ratnum-d x))))
  (define ($fl+ x y)
    (foreign-call "ikrt_fl_plus" x y))
  (define ($fl- x y)
    (foreign-call "ikrt_fl_minus" x y))
  (define ($fl* x y)
    (foreign-call "ikrt_fl_times" x y))
  (define ($fl/ x y)
    (foreign-call "ikrt_fl_div" x y))
  


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
           [(ratnum? y)
            ($make-ratnum
              (+ (* x ($ratnum-d y)) ($ratnum-n y))
              ($ratnum-d y))]
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
           [(ratnum? y)
            ($make-ratnum
              (+ (* x ($ratnum-d y)) ($ratnum-n y))
              ($ratnum-d y))] 
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
           [(ratnum? y)
            ($fl+ x (ratnum->flonum y))]
           [else 
            (error '+ "~s is not a number" y)])]
        [(ratnum? x)
         (cond
           [(or (fixnum? y) (bignum? y))
            ($make-ratnum
              (+ (* y ($ratnum-d x)) ($ratnum-n x))
              ($ratnum-d x))]
           [(flonum? y)
            ($fl+ y (ratnum->flonum x))]
           [(ratnum? y)
            (let ([n0 ($ratnum-n x)] [n1 ($ratnum-n y)]
                  [d0 ($ratnum-d x)] [d1 ($ratnum-d y)])
              ;;; FIXME: inefficient
              (/ (+ (* n0 d1) (* n1 d0)) (* d0 d1)))]
           [else 
            (error '+ "~s is not a number" y)])] 
        [else (error '+ "~s is not a number" x)])))

  (define binary-logand
    (lambda (x y)
      (cond
        [(fixnum? x)
         (cond
           [(fixnum? y) ($fxlogand x y)]
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

  (define (binary-gcd x y) 
    (define (gcd x y)
      (cond
        [($fx= y 0) x]
        [else (gcd y (remainder x y))]))
    (let ([x (if (< x 0) (- x) x)]
          [y (if (< y 0) (- y) y)])
      (cond
        [(> x y) (gcd x y)]
        [(< x y) (gcd y x)]
        [else x])))

  (define gcd
    (case-lambda
      [(x y) 
       (cond
         [(or (fixnum? x) (bignum? x))
          (cond
            [(or (fixnum? y) (bignum? y)) 
             (binary-gcd x y)]
            [(number? y)
             (error 'gcd "~s is not an exact integer" y)]
            [else 
             (error 'gcd "~s is not a number" y)])]
         [(number? x)
          (error 'gcd "~s is not an exact integer" x)]
         [else 
          (error 'gcd "~s is not a number" x)])]
      [(x)
       (cond
         [(or (fixnum? x) (bignum? x)) x]
         [(number? x)
          (error 'gcd "~s is not an exact integer" x)]
         [else 
          (error 'gcd "~s is not a number" x)])]
      [() 0]
      [(x y z . ls) 
       (let f ([g (gcd (gcd x y) z)] [ls ls])
         (cond
           [(null? ls) g]
           [else (f (gcd g (car ls)) (cdr ls))]))]))


  (define lcm
    (case-lambda
      [(x y) 
       (cond
         [(or (fixnum? x) (bignum? x))
          (cond
            [(or (fixnum? y) (bignum? y)) 
             (let ([x (if (< x 0) (- x) x)]
                   [y (if (< y 0) (- y) y)])
               (let ([g (binary-gcd x y)])
                 (binary* y (quotient x g))))]
            [(number? y)
             (error 'lcm "~s is not an exact integer" y)]
            [else 
             (error 'lcm "~s is not a number" y)])]
         [(number? x)
          (error 'lcm "~s is not an exact integer" x)]
         [else 
          (error 'lcm "~s is not a number" x)])]
      [(x)
       (cond
         [(or (fixnum? x) (bignum? x)) x]
         [(number? x)
          (error 'lcm "~s is not an exact integer" x)]
         [else 
          (error 'lcm "~s is not a number" x)])]
      [() 1]
      [(x y z . ls) 
       (let f ([g (lcm (lcm x y) z)] [ls ls])
         (cond
           [(null? ls) g]
           [else (f (lcm g (car ls)) (cdr ls))]))]))




  (define binary/ ;;; implements ratnums
    (lambda (x y)
      (cond
        [(flonum? x)
         (cond
           [(flonum? y) ($fl/ x y)]
           [(fixnum? y) ($fl/ x (fixnum->flonum y))]
           [(bignum? y) ($fl/ x (bignum->flonum y))]
           [(ratnum? y) ($fl/ x (ratnum->flonum y))]
           [else (error '/ "unspported ~s ~s" x y)])]
        [(fixnum? x)
         (cond
           [(flonum? y) ($fl/ (fixnum->flonum x) y)]
           [(fixnum? y)
            (cond
              [($fx= y 0) (error '/ "division by 0")]
              [($fx> y 0)
               (if ($fx= y 1)
                   x
                   (let ([g (binary-gcd x y)])
                     (cond
                       [($fx= g y) (fxquotient x g)]
                       [($fx= g 1) ($make-ratnum x y)]
                       [else ($make-ratnum (fxquotient x g) (fxquotient y g))])))]
              [else
               (if ($fx= y -1)
                   (binary- 0 x)
                   (let ([g (binary-gcd x y)])
                     (cond
                       [($fx= ($fx- 0 g) y) (binary- 0 (fxquotient x g))]
                       [($fx= g 1) ($make-ratnum (binary- 0 x) (binary- 0 y))]
                       [else
                        ($make-ratnum 
                          (binary- 0 (fxquotient x g))
                          (binary- 0 (fxquotient y g)))])))])]
           [(bignum? y)
            (let ([g (binary-gcd x y)])
              (cond
                [(= g y) (quotient x g)] ;;; should not happen
                [($bignum-positive? y)
                 (if ($fx= g 1) 
                     ($make-ratnum x y)
                     ($make-ratnum (fxquotient x g) (quotient y g)))]
                [else
                 (if ($fx= g 1)
                     ($make-ratnum (binary- 0 x) (binary- 0 y))
                     ($make-ratnum 
                        (binary- 0 (fxquotient x g))
                        (binary- 0 (quotient y g))))]))]
           [(ratnum? y) 
            (/ (* x ($ratnum-d y)) ($ratnum-n y))]
           [else (error '/ "unsupported ~s ~s" x y)])]
        [(bignum? x) 
         (cond
           [(fixnum? y) 
            (cond
              [($fx= y 0) (error '/ "division by 0")]
              [($fx> y 0)
               (if ($fx= y 1)
                   x
                   (let ([g (binary-gcd x y)])
                     (cond
                       [($fx= g 1) ($make-ratnum x y)]
                       [($fx= g y) (quotient x g)]
                       [else
                        ($make-ratnum (quotient x g) (quotient y g))])))]
              [else
               (if ($fx= y -1)
                   (- x)
                   (let ([g (binary-gcd x y)])
                     (cond
                       [(= (- g) y) (- (quotient x g))]
                       [else
                        ($make-ratnum 
                          (- (quotient x g))
                          (- (quotient y g)))])))])]
           [(bignum? y) 
            (let ([g (binary-gcd x y)])
              (cond
                [($fx= g 1) ($make-ratnum x y)]
                [($bignum-positive? y)
                 (if (= g y)
                     (quotient x g)
                     ($make-ratnum (quotient x g) (quotient y g)))]
                [else
                 (let ([y (binary- 0 y)])
                   (if (= g y)
                       (binary- 0 (quotient x g))
                       ($make-ratnum (binary- 0 (quotient x g))
                                     (quotient y g))))]))]
           [(flonum? y) ($fl/ (bignum->flonum x) y)]
           [(ratnum? y) 
            (binary/ (binary* x ($ratnum-n y)) ($ratnum-d y))]
           [else (error '/ "~s is not a number" y)])]
        [(ratnum? x)
         (cond
           [(ratnum? y) 
            (binary/
              (binary* ($ratnum-n x) ($ratnum-d y))
              (binary* ($ratnum-n y) ($ratnum-d x)))]
           [else (binary/ 1 (binary/ y x))])]
        [else (error '/ "~s is not a number" x)])))

  (define /
    (case-lambda
      [(x y) (binary/ x y)]
      [(x) 
       (cond
         [(fixnum? x)
          (cond
            [($fxzero? x) (error '/ "division by 0")]
            [($fx> x 0)
             (if ($fx= x 1)
                 1
                 ($make-ratnum 1 x))]
            [else
             (if ($fx= x -1)
                 -1
                 ($make-ratnum -1 (- x)))])]
         [(bignum? x)
          (if ($bignum-positive? x)
              ($make-ratnum 1 x)
              ($make-ratnum -1 (- x)))]
         [(flonum? x) (foreign-call "ikrt_fl_invert" x)]
         [(ratnum? x)
          (let ([n ($ratnum-n x)] [d ($ratnum-d x)])
            (cond
              [($fx= n 1) d]
              [($fx= n -1) (- d)]
              [else ($make-ratnum d n)]))]
         [else (error '/ "unspported argument ~s" x)])]
      [(x y z . rest)
       (let f ([a (binary/ x y)] [b z] [ls rest])
         (cond
           [(null? rest) (binary/ a b)]
           [else (f (binary/ a b) (car ls) (cdr ls))]))]))


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

  (define bignum->string
    (lambda (x)
      (utf8-bytevector->string
        (foreign-call "ikrt_bignum_to_bytevector" x))))
  
  (define ratnum->string
    (lambda (x) 
      (string-append 
        (number->string ($ratnum-n x))
        "/"
        (number->string ($ratnum-d x)))))
  
  (define number->string
    (lambda (x)
      (cond
        [(fixnum? x) (fixnum->string x)]
        [(bignum? x) (bignum->string x)]
        [(flonum? x) (flonum->string x)]
        [(ratnum? x) (ratnum->string x)]
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
  (define-syntax bnbn= (syntax-rules () [(_ x y) (bnbncmp x y $fx=)]))
  (define-syntax bnbn< (syntax-rules () [(_ x y) (bnbncmp x y $fx<)]))
  (define-syntax bnbn> (syntax-rules () [(_ x y) (bnbncmp x y $fx>)]))
  (define-syntax bnbn<= (syntax-rules () [(_ x y) (bnbncmp x y $fx<=)]))
  (define-syntax bnbn>= (syntax-rules () [(_ x y) (bnbncmp x y $fx>=)]))
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


  (define = 
    (mk< = $fx= false false bnbn= fxfl= flfx= bnfl= flbn= flfl=))
  (define < 
    (mk< < $fx< fxbn< bnfx< bnbn< fxfl< flfx< bnfl< flbn< flfl<))
  (define >
    (mk< > $fx> fxbn> bnfx> bnbn> fxfl> flfx> bnfl> flbn> flfl>))
  (define <= 
    (mk< <= $fx<= fxbn< bnfx< bnbn<= fxfl<= flfx<= bnfl<= flbn<= flfl<=))
  (define >= 
    (mk< >= $fx>= fxbn> bnfx> bnbn>= fxfl>= flfx>= bnfl>= flbn>= flfl>=))

  (define add1
    (lambda (x)
      (cond
        [(fixnum? x) 
         (foreign-call "ikrt_fxfxplus" x 1)]
        [(bignum? x)
         (foreign-call "ikrt_fxbnplus" 1 x)]
        [else (error 'add1 "~s is not a number" x)])))

  (define sub1
    (lambda (x)
      (cond
        [(fixnum? x) 
         (foreign-call "ikrt_fxfxplus" x -1)]
        [(bignum? x)
         (foreign-call "ikrt_fxbnplus" -1 x)]
        [else (error 'sub1 "~s is not a number" x)])))

  (define zero?
    (lambda (x)
      (cond
        [(fixnum? x) (eq? x 0)]
        [(bignum? x) #f]
        [(flonum? x) (= x (exact->inexact 0))]
        [else (error 'zero? "tag=~s / ~s  is not a number" 
                     ($fxlogand 255 
                      ($fxsll x 2))
                     ($fxlogand x -1)
                     )])))

  (define expt
    (lambda (n m)
      (define fxexpt
        (lambda (n m)
          (cond
            [($fxzero? m) 1]
            [($fxzero? ($fxlogand m 1))
             (fxexpt (binary* n n) ($fxsra m 1))]
            [else
             (binary* n (fxexpt (binary* n n) ($fxsra m 1)))])))
      (unless (number? n)
        (error 'expt "~s is not a numebr" n))
      (cond
        [(fixnum? m) 
         (if ($fx>= m 0)
             (fxexpt n m)
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

  (define quotient
    (lambda (x y)
      (let-values ([(q r) (quotient+remainder x y)])
        q)))

  (define remainder
    (lambda (x y)
      (let-values ([(q r) (quotient+remainder x y)])
        r)))

  (define quotient+remainder
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

  (define positive?
    (lambda (x)
      (cond
        [(fixnum? x) ($fx> x 0)]
        [(bignum? x) (positive-bignum? x)]
        [else (error 'positive? "~s is not a number" x)])))

  (define negative?
    (lambda (x)
      (cond
        [(fixnum? x) ($fx< x 0)]
        [(bignum? x) (not (positive-bignum? x))]
        [else (error 'negative? "~s is not a number" x)])))

  (define sin
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_sin" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_sin" x)]
        [else (error 'sin "unsupported ~s" x)])))

  (define cos
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_cos" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_cos" x)]
        [else (error 'cos "unsupported ~s" x)])))

  (define atan
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_atan" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_atan" x)]
        [else (error 'atan "unsupported ~s" x)])))

  (define sqrt
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_sqrt" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_sqrt" x)]
        [else (error 'sqrt "unsupported ~s" x)])))



  (define string->number
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
      (define (digit c radix)
        (cond
          [(and ($char<= #\0 c) ($char<= c #\9)) 
           (let ([n ($fx- ($char->fixnum c) ($char->fixnum #\0))])
             (and
               (or ($fx>= radix 10)
                   (and ($fx= radix 8) ($char<= c #\7))
                   (and ($fx= radix 2) ($char<= c #\1)))
               n))]
          [(and ($char<= #\a c) ($char<= c #\f)) 
           (let ([n ($fx+ 10 ($fx- ($char->fixnum c) ($char->fixnum #\a)))])
             (and ($fx= radix 16) n))] 
          [(and ($char<= #\A c) ($char<= c #\F)) 
           (let ([n ($fx+ 10 ($fx- ($char->fixnum c) ($char->fixnum #\A)))])
             (and ($fx= radix 16) n))] 
          [else #f]))
      (define (convert-subseq str idx len radix ac)
        (cond
          [($fx< idx len)
           (let ([c (string-ref str idx)])
             (cond
               [(digit c radix) =>
                (lambda (n) 
                  (convert-subseq str ($fxadd1 idx) len radix
                    (+ (* ac radix) n)))]
               [else #f]))]
          [else ac]))
      (define (convert-init str idx len radix)
        (cond
          [($fx< idx len)
           (let ([c (string-ref str idx)])
             (cond
               [(digit c radix) =>
                (lambda (n) 
                  (convert-subseq str ($fxadd1 idx) len radix n))]
               [else #f]))]
          [else #f]))
      (define (convert-init-sign str idx len radix)
        (cond
          [($fx< idx len)
           (let ([c (string-ref str idx)])
             (cond
               [(char=? c #\+)
                (convert-init str ($fxadd1 idx) len radix)]
               [(char=? c #\-)
                (let ([n (convert-init str ($fxadd1 idx) len radix)])
                  (and n (- n)))]
               [else (convert-init str idx len radix)]))]
          [else #f]))
      (define (convert-radix str len)
        (cond
          [($fx>= len 2)
           (let ([c (string-ref str 1)])
             (case c
               [(#\x #\X) (convert-init-sign str 2 len 16)]
               [(#\b #\B) (convert-init-sign str 2 len 2)]
               [(#\d #\D) (convert-init-sign str 2 len 10)]
               [(#\o #\O) (convert-init-sign str 2 len 8)]
               [else #f]))]
          [else #f]))
      (define (convert-sign str len)
        (cond
          [($fx> len 0)
           (let ([c ($string-ref str 0)])
             (case c
               [(#\+) (convert-num str len #t)]
               [(#\-) (convert-num str len #f)]
               [(#\#) (convert-radix str len)]
               [else
                (convert-data-init str len #t 1 c)]))]
          [else #f]))
      (cond
        [(string? x) 
         (convert-sign x ($string-length x))]
        [else (error 'string->number "~s is not a string" x)])))
  )
