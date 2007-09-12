






(library (ikarus flonums)
  (export $flonum->exact $flonum-signed-biased-exponent flonum-parts
          inexact->exact exact $flonum-rational? $flonum-integer? $flzero?
          $flnegative? flpositive? flabs fixnum->flonum
          flsin flcos fltan flasin flacos flatan fleven? flodd?
          flfloor flceiling flnumerator fldenominator flexp fllog
          flinteger? flonum-bytes flnan? flfinite? flinfinite?
          flexpt)
  (import 
    (ikarus system $bytevectors)
    (ikarus system $fx) 
    (only (ikarus system $flonums) $fl>=)
    (ikarus system $bignums)
    (except (ikarus system $flonums) $flonum-signed-biased-exponent
            $flonum-rational? $flonum-integer?)
    (except (ikarus) inexact->exact exact flpositive? flabs fixnum->flonum
            flsin flcos fltan flasin flacos flatan fleven? flodd?
            flfloor flceiling flnumerator fldenominator flexp fllog
            flexpt flinteger? flonum-parts flonum-bytes flnan? flfinite?
            flinfinite?))
  
  (define (flonum-bytes f)
    (unless (flonum? f) 
      (error 'flonum-bytes "~s is not a flonum" f))
    (values 
      ($flonum-u8-ref f 0)
      ($flonum-u8-ref f 1)
      ($flonum-u8-ref f 2)
      ($flonum-u8-ref f 3)
      ($flonum-u8-ref f 4)
      ($flonum-u8-ref f 5)
      ($flonum-u8-ref f 6)
      ($flonum-u8-ref f 7)))
  (define (flonum-parts x)
    (unless (flonum? x) 
      (error 'flonum-parts "~s is not a flonum" x))
    (let-values ([(b0 b1 b2 b3 b4 b5 b6 b7) (flonum-bytes x)])
      (values 
        (zero? (fxlogand b0 128)) 
        (+ (fxsll (fxlogand b0 127) 4)
           (fxsra b1 4))
        (+ (+ b7 (fxsll b6 8) (fxsll b5 16))
           (* (+ b4
                 (fxsll b3 8)
                 (fxsll b2 16)
                 (fxsll (fxlogand b1 #b1111) 24))
              (expt 2 24))))))
  (define ($zero-m? f) 
    (and ($fxzero? ($flonum-u8-ref f 7))
         ($fxzero? ($flonum-u8-ref f 6))
         ($fxzero? ($flonum-u8-ref f 5))
         ($fxzero? ($flonum-u8-ref f 4))
         ($fxzero? ($flonum-u8-ref f 3))
         ($fxzero? ($flonum-u8-ref f 2))
         ($fxzero? ($fxlogand ($flonum-u8-ref f 1) #b1111))))



  (define ($flonum-signed-biased-exponent x)
    (let ([b0 ($flonum-u8-ref x 0)]
          [b1 ($flonum-u8-ref x 1)])
      (fxlogor (fxsll b0 4) (fxsra b1 4))))

  (define ($flonum-rational? x)
    (let ([be (fxlogand ($flonum-signed-biased-exponent x) (sub1 (fxsll 1 11)))])
      (fx< be 2047)))

  (define ($flonum-integer? x)
    (let ([be (fxlogand ($flonum-signed-biased-exponent x) (sub1 (fxsll 1 11)))])
      (cond
        [(fx= be 2047)  ;;; nans and infs
         #f]
        [(fx>= be 1075) ;;; magnitue large enough
         #t]
        [(fx= be 0) ;;; denormalized double, only +/-0.0 is integer
         (and (fx= ($flonum-u8-ref x 7) 0)
              (fx= ($flonum-u8-ref x 6) 0) 
              (fx= ($flonum-u8-ref x 5) 0) 
              (fx= ($flonum-u8-ref x 4) 0) 
              (fx= ($flonum-u8-ref x 3) 0) 
              (fx= ($flonum-u8-ref x 2) 0) 
              (fx= ($flonum-u8-ref x 1) 0))]
        [(fx< be (fx+ 1075 -52)) ;;; too small to be an integer
         #f]
        [else
         (let ([v ($flonum->exact x)])
           (or (fixnum? v) (bignum? v)))])))

  (define (flnumerator x) 
    (unless (flonum? x) 
      (error 'flnumerator "~s is not a flonum" x))
    (cond
      [($flonum-integer? x) x]
      [($flonum-rational? x) 
       (exact->inexact (numerator ($flonum->exact x)))]
      [else x]))

  (define (fldenominator x) 
    (unless (flonum? x) 
      (error 'fldenominator "~s is not a flonum" x))
    (cond
      [($flonum-integer? x) 1.0]
      [($flonum-rational? x) 
       (exact->inexact (denominator ($flonum->exact x)))]
      [(flnan? x) x]
      [else 1.0]))

  (define (fleven? x)
    (unless (flonum? x) 
      (error 'fleven? "~s is not a flonum" x))
    (let ([v ($flonum->exact x)])
      (cond
        [(fixnum? v) ($fx= ($fxlogand v 1) 0)]
        [(bignum? v) 
         (foreign-call "ikrt_even_bn" v)]
        [else (error 'fleven? "~s is not an integer flonum" x)])))

  (define (flodd? x)
    (unless (flonum? x) 
      (error 'flodd? "~s is not a flonum" x))
    (let ([v ($flonum->exact x)])
      (cond
        [(fixnum? v) ($fx= ($fxlogand v 1) 1)]
        [(bignum? v) 
         (not (foreign-call "ikrt_even_bn" v))]
        [else (error 'flodd? "~s is not an integer flonum" x)])))

  (define (flinteger? x)
    (if (flonum? x)
        ($flonum-integer? x)
        (error 'flinteger? "~s is not a flonum" x)))

  (define (flinfinite? x) 
    (if (flonum? x) 
        (let ([be (fxlogand ($flonum-signed-biased-exponent x) (sub1 (fxsll 1 11)))])
          (and (fx= be 2047)  ;;; nans and infs
               ($zero-m? x)))
        (error 'flinfinite? "~s is not a flonum" x)))

  (define (flnan? x) 
    (if (flonum? x) 
        (let ([be (fxlogand ($flonum-signed-biased-exponent x) (sub1 (fxsll 1 11)))])
          (and (fx= be 2047)  ;;; nans and infs
               (not ($zero-m? x))))
        (error 'flnan? "~s is not a flonum" x)))

  (define (flfinite? x) 
    (if (flonum? x) 
        (let ([be (fxlogand ($flonum-signed-biased-exponent x) (sub1 (fxsll 1 11)))])
          (not (fx= be 2047)))
        (error 'flfinite? "~s is not a flonum" x)))
  
  (define ($flzero? x)
    (let ([be (fxlogand ($flonum-signed-biased-exponent x) (sub1 (fxsll 1 11)))])
       (and 
         (fx= be 0) ;;; denormalized double, only +/-0.0 is integer
         (and (fx= ($flonum-u8-ref x 7) 0)
              (fx= ($flonum-u8-ref x 6) 0) 
              (fx= ($flonum-u8-ref x 5) 0) 
              (fx= ($flonum-u8-ref x 4) 0) 
              (fx= ($flonum-u8-ref x 3) 0) 
              (fx= ($flonum-u8-ref x 2) 0) 
              (fx= ($flonum-u8-ref x 1) 0)))))

  (define ($flnegative? x)
    (let ([b0 ($flonum-u8-ref x 0)])
      (fx> b0 127)))

  (define ($flonum->exact x)
    (let-values ([(pos? be m) (flonum-parts x)])
      (cond
        [(<= 1 be 2046) ; normalized flonum
         (* (if pos? 1 -1)
            (* (+ m (expt 2 52)) (expt 2 (- be 1075))))]
        [(= be 0) 
         (* (if pos? 1 -1) 
            (* m (expt 2 -1074)))]
        [else #f])))
  

  (define (inexact->exact x)
    (cond
      [(flonum? x)
       (or ($flonum->exact x)
           (error 'inexact->exact "~s has no real value" x))]
      [(or (fixnum? x) (ratnum? x) (bignum? x)) x]
      [else
       (error 'inexact->exact "~s is not an inexact number" x)]))

  (define (exact x)
    (cond
      [(flonum? x)
       (or ($flonum->exact x)
           (error 'exact "~s has no real value" x))]
      [(or (fixnum? x) (ratnum? x) (bignum? x)) x]
      [else
       (error 'exact "~s is not an inexact number" x)]))


  (define (flpositive? x)
    (if (flonum? x) 
        ($fl> x 0.0)
        (error 'flpositive? "~s is not a flonum" x)))
  
  (define (flabs x)
    (if (flonum? x) 
        (if ($fl> x 0.0) 
            ($fl* x -1.0)
            x)
        (error 'flabs "~s is not a flonum" x)))

  (define (fixnum->flonum x)
    (if (fixnum? x) 
        ($fixnum->flonum x)
        (error 'fixnum->flonum "~s is not a fixnum")))

  (define (flsin x)
    (if (flonum? x) 
        (foreign-call "ikrt_fl_sin" x)
        (error 'flsin "~s is not a flonum" x)))

  (define (flcos x)
    (if (flonum? x) 
        (foreign-call "ikrt_fl_cos" x)
        (error 'flcos "~s is not a flonum" x)))

  (define (fltan x)
    (if (flonum? x) 
        (foreign-call "ikrt_fl_tan" x)
        (error 'fltan "~s is not a flonum" x)))

  (define (flasin x)
    (if (flonum? x) 
        (foreign-call "ikrt_fl_asin" x)
        (error 'flasin "~s is not a flonum" x)))

  (define (flacos x)
    (if (flonum? x) 
        (foreign-call "ikrt_fl_acos" x)
        (error 'flacos "~s is not a flonum" x)))

  (define (flatan x)
    (if (flonum? x) 
        (foreign-call "ikrt_fl_atan" x)
        (error 'flatan "~s is not a flonum" x)))


  (define (flfloor x)
    (define (ratnum-floor x)
      (let ([n (numerator x)] [d (denominator x)])
        (let ([q (quotient n d)])
          (if (>= n 0) q (- q 1)))))
    (cond
      [(flonum? x) 
       (let ([e ($flonum->exact x)])
         (cond
           [(ratnum? e)
            (exact->inexact (ratnum-floor e))] 
           [else x]))]
      [else (error 'flfloor "~s is not a flonum" x)]))

  (define (flceiling x)
    (cond
      [(flonum? x) 
       (let ([e ($flonum->exact x)])
         (cond
           [(ratnum? e)
            (exact->inexact (ceiling e))] 
           [else x]))]
      [else (error 'flceiling "~s is not a flonum" x)]))

  (define (flexp x)
    (if (flonum? x) 
        (foreign-call "ikrt_fl_exp" x ($make-flonum))
        (error 'flexp "~s is not a flonum" x)))

  (define (fllog x)
    (if (flonum? x)
        (if ($fl>= x 0.0) 
            (foreign-call "ikrt_fl_log" x)
            (error 'fllog "argument ~s should not be negative" x))
        (error 'fllog "~s is not a flonum" x)))

  (define (flexpt x y)
    (if (flonum? x)
        (if (flonum? y)
            (let ([y^ ($flonum->exact y)])
              (cond
                [(fixnum? y^) (inexact (expt x y^))]
                [(bignum? y^) (inexact (expt x y^))]
                [else
                 (foreign-call "ikrt_flfl_expt" x y ($make-flonum))]))
            (error 'flexpt "~s is not a flonum" y))
        (error 'fllog "~s is not a flonum" x)))


  )





(library (ikarus generic-arithmetic)
  (export + - * / zero? = < <= > >= add1 sub1 quotient remainder
          modulo even? odd? logand $two-bignums
          positive? negative? expt gcd lcm numerator denominator exact-integer-sqrt
          quotient+remainder number->string string->number min max
          abs truncate fltruncate
          exact->inexact inexact floor ceiling round log fl=? fl<? fl<=? fl>?
          fl>=? fl+ fl- fl* fl/ flsqrt flmin flzero? flnegative?
          sin cos tan asin acos atan sqrt
          flround flmax random)
  (import 
    (ikarus system $fx)
    (ikarus system $flonums)
    (ikarus system $ratnums)
    (ikarus system $bignums)
    (ikarus system $chars)
    (ikarus system $strings)
    (only (ikarus flonums) $flonum->exact $flzero? $flnegative?)
    (except (ikarus) + - * / zero? = < <= > >= add1 sub1 quotient
            remainder modulo even? odd? quotient+remainder number->string 
            positive? negative? logand $two-bignums
            string->number expt gcd lcm numerator denominator
            exact->inexact inexact floor ceiling round log
            exact-integer-sqrt min max abs
            fl=? fl<? fl<=? fl>? fl>=? fl+ fl- fl* fl/ flsqrt flmin
            flzero? flnegative?
            sin cos tan asin acos atan sqrt truncate fltruncate
            flround flmax random))

  (define ($two-bignums)
    (list 1234567890 -1234567890
          12345678901234567890
          -12345678901234567890
          1234567890123456789012345678901234567890
          -1234567890123456789012345678901234567890))
;    (foreign-call "ikrt_fixnum_to_flonum" x))

  (module (bignum->flonum)
    ;  sbe         f6     f5       f4       f3       f2       f1       f0
    ;SEEEEEEE|EEEEmmmm|mmmmmmmm|mmmmmmmm|mmmmmmmm|mmmmmmmm|mmmmmmmm|mmmmmmmm 
    ;        |        |        |        |        |        |        |
    ;   v0       v1       v2       v3       v4       v5       v6       v7
    (define ($flonum pos? e f6 f5 f4 f3 f2 f1 f0)
      (let ([be (fx+ e 1075)])
        (let ([v ($make-flonum)])
          (cond
            [(fx< be 2047)
             (let ([sbe (if pos? be (fxlogor be (fxsll 1 11)))])
               ($flonum-set! v 0 (fxsra sbe 4))
               ($flonum-set! v 1 (fxlogor (fxsll sbe 4) (fxlogand f6 #b1111)))
               ($flonum-set! v 2 f5)
               ($flonum-set! v 3 f4)
               ($flonum-set! v 4 f3)
               ($flonum-set! v 5 f2)
               ($flonum-set! v 6 f1)
               ($flonum-set! v 7 f0))]
            [else ;;; inf
             (let ([sbe (if pos? 2047 (fxlogor 2047 (fxsll 1 11)))])
               ($flonum-set! v 0 (fxsra sbe 4))
               ($flonum-set! v 1 (fxsll sbe 4))
               ($flonum-set! v 2 0)
               ($flonum-set! v 3 0)
               ($flonum-set! v 4 0)
               ($flonum-set! v 5 0)
               ($flonum-set! v 6 0)
               ($flonum-set! v 7 0))])
          v)))
    (define ($flonum/c0 pos? e f6 f5 f4 f3 f2 f1 f0 c)
      (define ($fxeven? x)
        (fxzero? (fxlogand x 1)))
      (define-syntax cond*
        (syntax-rules (else)
          [(_ [test conseq] [else val]) 
           (if test conseq val)]
          [(_ [test conseq] [var  val] rest ...)
           (if test conseq (let ([var val]) (cond* rest ...)))]))
      (cond*
        [($fxeven? c) ($flonum pos? e f6 f5 f4 f3 f2 f1 f0)]
        [f0 (fx+ (fxlogand f0 255) 1)]
        [(fx< f0 256) ($flonum pos? e f6 f5 f4 f3 f2 f1 f0)]
        [f1 (fx+ (fxlogand f1 255) 1)]
        [(fx< f1 256) ($flonum pos? e f6 f5 f4 f3 f2 f1 0)]
        [f2 (fx+ (fxlogand f2 255) 1)]
        [(fx< f2 256) ($flonum pos? e f6 f5 f4 f3 f2 0 0)]
        [f3 (fx+ (fxlogand f3 255) 1)]
        [(fx< f3 256) ($flonum pos? e f6 f5 f4 f3 0 0 0)]
        [f4 (fx+ (fxlogand f4 255) 1)]
        [(fx< f4 256) ($flonum pos? e f6 f5 f4 0 0 0 0)]
        [f5 (fx+ (fxlogand f5 255) 1)]
        [(fx< f5 256) ($flonum pos? e f6 f5 0 0 0 0 0)]
        [f6 (fx+ (fxlogand f6 #b1111) 1)]
        [(fx< f6 16) ($flonum pos? e f6 0 0 0 0 0 0)]
        [else ($flonum pos? (+ e 1) 0 0 0 0 0 0 0)]))
    (define ($flonum/aux pos? e b7 b6 b5 b4 b3 b2 b1 b0)
      (cond
        [(fx>= b7 #x80) 
         ($flonum/c0 pos? (fx+ e 3)
                                  (fxsra b7 3)
            (fxlogor (fxsll b7 5) (fxsra b6 3))
            (fxlogor (fxsll b6 5) (fxsra b5 3))
            (fxlogor (fxsll b5 5) (fxsra b4 3))
            (fxlogor (fxsll b4 5) (fxsra b3 3))
            (fxlogor (fxsll b3 5) (fxsra b2 3))
            (fxlogor (fxsll b2 5) (fxsra b1 3))
            (fxsra b1 2))]
        [(fx>= b7 #x40) 
         ($flonum/c0 pos? (fx+ e 2)
                                  (fxsra b7 2)
            (fxlogor (fxsll b7 6) (fxsra b6 2))
            (fxlogor (fxsll b6 6) (fxsra b5 2))
            (fxlogor (fxsll b5 6) (fxsra b4 2))
            (fxlogor (fxsll b4 6) (fxsra b3 2))
            (fxlogor (fxsll b3 6) (fxsra b2 2))
            (fxlogor (fxsll b2 6) (fxsra b1 2))
            (fxsra b1 1))]
        [(fx>= b7 #x20) 
         ($flonum/c0 pos? (fx+ e 1)
                                  (fxsra b7 1)
            (fxlogor (fxsll b7 7) (fxsra b6 1))
            (fxlogor (fxsll b6 7) (fxsra b5 1))
            (fxlogor (fxsll b5 7) (fxsra b4 1))
            (fxlogor (fxsll b4 7) (fxsra b3 1))
            (fxlogor (fxsll b3 7) (fxsra b2 1))
            (fxlogor (fxsll b2 7) (fxsra b1 1))
            b1)]
        [(fx>= b7 #x10) 
         ($flonum/c0 pos? e b7 b6 b5 b4 b3 b2 b1
            (fxsra b0 7))]
        [(fx>= b7 #x08)
         ($flonum/c0 pos? (fx- e 1)
            (fxlogor (fxsll b7 1) (fxsra b6 7))
            (fxlogor (fxsll b6 1) (fxsra b5 7))
            (fxlogor (fxsll b5 1) (fxsra b4 7))
            (fxlogor (fxsll b4 1) (fxsra b3 7))
            (fxlogor (fxsll b3 1) (fxsra b2 7))
            (fxlogor (fxsll b2 1) (fxsra b1 7))
            (fxlogor (fxsll b1 1) (fxsra b0 7))
            (fxsra b0 6))]
        [(fx>= b7 #x04)
         ($flonum/c0 pos? (fx- e 2)
            (fxlogor (fxsll b7 2) (fxsra b6 6))
            (fxlogor (fxsll b6 2) (fxsra b5 6))
            (fxlogor (fxsll b5 2) (fxsra b4 6))
            (fxlogor (fxsll b4 2) (fxsra b3 6))
            (fxlogor (fxsll b3 2) (fxsra b2 6))
            (fxlogor (fxsll b2 2) (fxsra b1 6))
            (fxlogor (fxsll b1 2) (fxsra b0 6))
            (fxsra b0 5))]
        [(fx>= b7 #x02)
         ($flonum/c0 pos? (fx- e 3)
            (fxlogor (fxsll b7 3) (fxsra b6 5))
            (fxlogor (fxsll b6 3) (fxsra b5 5))
            (fxlogor (fxsll b5 3) (fxsra b4 5))
            (fxlogor (fxsll b4 3) (fxsra b3 5))
            (fxlogor (fxsll b3 3) (fxsra b2 5))
            (fxlogor (fxsll b2 3) (fxsra b1 5))
            (fxlogor (fxsll b1 3) (fxsra b0 5))
            (fxsra b0 4))]
        [(fx>= b7 #x01)
         ($flonum/c0 pos? (fx- e 4)
            (fxlogor (fxsll b7 4) (fxsra b6 4))
            (fxlogor (fxsll b6 4) (fxsra b5 4))
            (fxlogor (fxsll b5 4) (fxsra b4 4))
            (fxlogor (fxsll b4 4) (fxsra b3 4))
            (fxlogor (fxsll b3 4) (fxsra b2 4))
            (fxlogor (fxsll b2 4) (fxsra b1 4))
            (fxlogor (fxsll b1 4) (fxsra b0 4))
            (fxsra b0 3))]
        [else (error '$float/aux "invalid b7=~s" b7)]))
    (define (bignum->flonum x)
      (define (bignum/4->flonum x) 
        ($flonum/aux ($bignum-positive? x) -24 
            ($bignum-byte-ref x 3)
            ($bignum-byte-ref x 2)
            ($bignum-byte-ref x 1)
            ($bignum-byte-ref x 0)
            0 0 0 0))
      (define (bignum/8->flonum x)
        ;;; bignum:  [b0 b1 b2 b3 b4 b5 b6 b7]
        (let ([b0 ($bignum-byte-ref x 0)]
              [b1 ($bignum-byte-ref x 1)]
              [b2 ($bignum-byte-ref x 2)]
              [b3 ($bignum-byte-ref x 3)]
              [b4 ($bignum-byte-ref x 4)]
              [b5 ($bignum-byte-ref x 5)]
              [b6 ($bignum-byte-ref x 6)]
              [b7 ($bignum-byte-ref x 7)] 
              [pos? ($bignum-positive? x)])
          (if (fx= b7 0)
              (if (fx= b6 0)
                  (if (fx= b5 0)
                      (if (fx= b4 0)
                          (error 'bignum8->flonum "malformed bignum")
                          ($flonum/aux pos? -16 b4 b3 b2 b1 b0 0 0 0))
                      ($flonum/aux pos? -8 b5 b4 b3 b2 b1 b0 0 0))
                  ($flonum/aux pos? 0 b6 b5 b4 b3 b2 b1 b0 0))
              ($flonum/aux pos? 8 b7 b6 b5 b4 b3 b2 b1 b0))))
      (define (bignum/n->flonum x bytes) 
        (define (aux x b7 bytes)
          ($flonum/aux ($bignum-positive? x) (+ (* bytes 8) -48)
             b7
             ($bignum-byte-ref x (fx- bytes 1))
             ($bignum-byte-ref x (fx- bytes 2))
             ($bignum-byte-ref x (fx- bytes 3))
             ($bignum-byte-ref x (fx- bytes 4))
             ($bignum-byte-ref x (fx- bytes 5))
             ($bignum-byte-ref x (fx- bytes 6))
             ($bignum-byte-ref x (fx- bytes 7))))
        ;;; bignum: [b0 b1 b2 b3 ... b_{bytes-1}]
        (let* ([bytes (fxsub1 bytes)] [bn ($bignum-byte-ref x bytes)])
          (if (fx= bn 0)
              (let* ([bytes (fxsub1 bytes)] [bn ($bignum-byte-ref x bytes)])
                (if (fx= bn 0)
                    (let* ([bytes (fxsub1 bytes)] [bn ($bignum-byte-ref x bytes)])
                      (if (fx= bn 0)
                          (let* ([bytes (fxsub1 bytes)] [bn ($bignum-byte-ref x bytes)])
                            (if (fx= bn 0)
                                (error 'bignum/n->flonum "malformed bignum")
                                (aux x bn bytes)))
                          (aux x bn bytes)))
                    (aux x bn bytes)))
              (aux x bn bytes))))
              
      (unless (bignum? x)
        (error 'bignum->flonum "~s is not a bignum" x))
      (let ([bytes ($bignum-size x)])
        (case bytes
          [(4)  (bignum/4->flonum x)]
          [(8)  (bignum/8->flonum x)]
          [else (bignum/n->flonum x bytes)]))))
  (define (ratnum->flonum x) 
    (binary/ (exact->inexact ($ratnum-n x)) 
             (exact->inexact ($ratnum-d x))))

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
            ($fl+ ($fixnum->flonum x) y)]
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
            ($fl+ x ($fixnum->flonum y))]
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
            (if ($fx= x 0)
                ($fl* y -1.0)
                ($fl- ($fixnum->flonum x) y))]
           [(ratnum? y) 
            (let ([n ($ratnum-n y)] [d ($ratnum-d y)])
              (binary/ (binary- (binary* d x) n) d))]
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
           [(ratnum? y) 
            (let ([n ($ratnum-n y)] [d ($ratnum-d y)])
              (binary/ (binary- (binary* d x) n) d))]
           [else 
            (error '- "~s is not a number" y)])]
        [(flonum? x)
         (cond
           [(fixnum? y)
            ($fl- x ($fixnum->flonum y))]
           [(bignum? y)
            ($fl- x (bignum->flonum y))]
           [(flonum? y)
            ($fl- x y)]
           [(ratnum? y) 
            (let ([n ($ratnum-n y)] [d ($ratnum-d y)])
              (binary/ (binary- (binary* d x) n) d))]
           [else
            (error '- "~s is not a number" y)])]
        [(ratnum? x)
         (let ([nx ($ratnum-n x)] [dx ($ratnum-d x)])
           (cond
             [(or (fixnum? y) (bignum? y) (flonum? y))
              (binary/ (binary- nx (binary* dx y)) dx)]
             [(ratnum? y)
              (let ([ny ($ratnum-n y)] [dy ($ratnum-d y)])
                (binary/ (binary- (binary* nx dy) (binary* ny dx))
                         (binary* dx dy)))]
             [else
              (error '- "~s is not a number" y)]))]
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
            ($fl* ($fixnum->flonum x) y)]
           [(ratnum? y) 
            (binary/ (binary* x ($ratnum-n y)) ($ratnum-d y))]
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
           [(ratnum? y) 
            (binary/ (binary* x ($ratnum-n y)) ($ratnum-d y))]
           [else 
            (error '* "~s is not a number" y)])]
        [(flonum? x)
         (cond
           [(fixnum? y)
            ($fl* x ($fixnum->flonum y))]
           [(bignum? y)
            ($fl* x (bignum->flonum y))]
           [(flonum? y)
            ($fl* x y)]
           [(ratnum? y) 
            (binary/ (binary* x ($ratnum-n y)) ($ratnum-d y))]
           [else
            (error '* "~s is not a number" y)])]
        [(ratnum? x) 
         (if (ratnum? y) 
             (binary/ (binary* ($ratnum-n x) ($ratnum-n y))
                      (binary* ($ratnum-d x) ($ratnum-d y)))
             (binary* y x))]
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
           [(fixnum? y) ($fl/ x ($fixnum->flonum y))]
           [(bignum? y) ($fl/ x (bignum->flonum y))]
           [(ratnum? y) ($fl/ x (ratnum->flonum y))]
           [else (error '/ "unspported ~s ~s" x y)])]
        [(fixnum? x)
         (cond
           [(flonum? y) ($fl/ ($fixnum->flonum x) y)]
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


  (define flmax
    (case-lambda
      [(x y)
       (if (flonum? x) 
           (if (flonum? y) 
               (if ($fl< x y) 
                   y
                   x)
               (error 'flmax "~s is not a flonum" y))
           (error 'flmax "~s is not a flonum" x))]
      [(x y z . rest)
       (let f ([a (flmax x y)] [b z] [ls rest])
         (cond
           [(null? ls) (flmax a b)]
           [else
            (f (flmax a b) (car ls) (cdr ls))]))]
      [(x) 
       (if (flonum? x) 
           x 
           (error 'flmax "~s is not a number" x))]))

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

  (define (abs x)
    (cond
      [(fixnum? x) 
       (if ($fx< x 0) (- x) x)]
      [(bignum? x)
       (if ($bignum-positive? x) x (- x))]
      [(flonum? x)
       (if ($flnegative? x) 
           ($fl* x -1.0)
           x)]
      [(ratnum? x) 
       (let ([n ($ratnum-n x)])
         (if (< n 0) 
             ($make-ratnum (- n) ($ratnum-d x))
             x))]
      [else (error 'abs "~s is not a number" x)]))

  (define flmin
    (case-lambda
      [(x y)
       (if (flonum? x)
           (if (flonum? y) 
               (if ($fl< x y) x y)
               (error 'flmin "~s is not a flonum" y))
           (error 'flmin "~s is not a flonum" x))]
      [(x y z . rest)
       (let f ([a (flmin x y)] [b z] [ls rest])
         (cond
           [(null? ls) (flmin a b)]
           [else
            (f (flmin a b) (car ls) (cdr ls))]))]
      [(x) 
       (if (flonum? x) 
           x 
           (error 'flmin "~s is not a flonum" x))]))

  (define exact->inexact
    (lambda (x)
      (cond
        [(fixnum? x) ($fixnum->flonum x)]
        [(bignum? x) (bignum->flonum x)]
        [(ratnum? x) 
         (binary/ (exact->inexact ($ratnum-n x)) ($ratnum-d x))]
        [else
         (error 'exact->inexact 
                "~s is not an exact number" x)])))

  (define inexact
    (lambda (x)
      (cond
        [(fixnum? x) ($fixnum->flonum x)]
        [(bignum? x) (bignum->flonum x)]
        [(ratnum? x) 
         (binary/ (exact->inexact ($ratnum-n x)) ($ratnum-d x))]
        [(flonum? x) x]
        [else
         (error 'inexact "~s is not a number" x)])))


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
               fxfl< flfx< bnfl< flbn< flfl<
               fxrt< rtfx< bnrt< rtbn< flrt< rtfl< rtrt<)
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
               [(ratnum? y) 
                (if (null? ls) 
                    (fxrt< x y)
                    (if (fxrt< x y)
                        (rtloopt y (car ls) (cdr ls))
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
               [(ratnum? y)
                (if (null? ls)
                    (bnrt< x y)
                    (if (bnrt< x y)
                        (rtloopt y (car ls) (cdr ls))
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
               [(ratnum? y)
                (if (null? ls)
                    (flrt< x y)
                    (if (flrt< x y)
                        (rtloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)])))
         (define rtloopt
           (lambda (x y ls)
             (cond
               [(fixnum? y)
                (if (null? ls)
                    (rtfx< x y)
                    (if (rtfx< x y)
                        (fxloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(bignum? y)
                (if (null? ls)
                    (rtbn< x y)
                    (if (rtbn< x y)
                        (bnloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(flonum? y)
                (if (null? ls)
                    (rtfl< x y)
                    (if (rtfl< x y)
                        (flloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [(ratnum? y)
                (if (null? ls)
                    (rtrt< x y)
                    (if (rtrt< x y)
                        (rtloopt y (car ls) (cdr ls))
                        (loopf (car ls) (cdr ls))))]
               [else (err y)]))) 
         (define loopf
           (lambda (x ls)
             (cond
               [(number? x) 
                (if (null? ls) 
                    #f
                    (loopf (car ls) (cdr ls)))]
               [else (err x)])))
         (define f
           (case-lambda
             [(x y)
              (cond
                [(fixnum? x)
                 (cond
                   [(fixnum? y) (fxfx< x y)]
                   [(bignum? y) (fxbn< x y)]
                   [(flonum? y) (fxfl< x y)]
                   [(ratnum? y) (fxrt< x y)]
                   [else (err y)])]
                [(bignum? x)
                 (cond
                   [(fixnum? y) (bnfx< x y)]
                   [(bignum? y) (bnbn< x y)]
                   [(flonum? y) (bnfl< x y)]
                   [(ratnum? y) (bnrt< x y)]
                   [else (err y)])]
                [(flonum? x)
                 (cond
                   [(fixnum? y) (flfx< x y)]
                   [(bignum? y) (flbn< x y)]
                   [(flonum? y) (flfl< x y)]
                   [(ratnum? y) (flrt< x y)]
                   [else (err y)])]
                [(ratnum? x)
                 (cond
                   [(fixnum? y) (rtfx< x y)]
                   [(bignum? y) (rtbn< x y)]
                   [(flonum? y) (rtfl< x y)]
                   [(ratnum? y) (rtrt< x y)]
                   [else (err y)])]
                [else (err x)])]
             [(x y z) (and (f x y) (f y z))]
             [(x) (if (number? x) #t (err x))]
             [(x y . ls) 
              (cond
                [(fixnum? x) (fxloopt x y ls)]
                [(bignum? x) (bnloopt x y ls)]
                [(flonum? x) (flloopt x y ls)]
                [(ratnum? x) (rtloopt x y ls)]
                [else (err x)])]))
         f)]))

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
           (syntax-rules () [(_ x y) (fl? x ($fixnum->flonum y))]))
         (define-syntax flbn? 
           (syntax-rules () [(_ x y) (fl? x (bignum->flonum y))]))
         (define-syntax fxfl? 
           (syntax-rules () [(_ x y) (fl? ($fixnum->flonum x) y)]))
         (define-syntax bnfl? 
           (syntax-rules () [(_ x y) (fl? (bignum->flonum x) y)])))]))

 ;;;  #;
 ;;; (begin
 ;;;   (define-syntax $fl=
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_equal" x y)]))
 ;;;   (define-syntax $fl<
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less" x y)]))
 ;;;   (define-syntax $fl<=
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less_or_equal" x y)]))
 ;;;   (define-syntax $fl>
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less" y x)]))
 ;;;   (define-syntax $fl>=
 ;;;     (syntax-rules () [(_ x y) (foreign-call "ikrt_fl_less_or_equal" y x)])))

  (define-syntax define-flcmp
    (syntax-rules ()
      [(_ fl<? $fl<)
       (define fl<?
         (case-lambda
           [(x y) 
            (if (flonum? x)
                (if (flonum? y) 
                    ($fl< x y)
                    (error 'fl<? "~s is not a flonum" y))
                (error 'fl<? "~s is not a flonum" x))]
           [(x y z) 
            (if (flonum? x)
                (if (flonum? y) 
                    (if (flonum? z) 
                        (and ($fl< x y) ($fl< y z))
                        (error 'fl<? "~s is not a flonum" z))
                    (error 'fl<? "~s is not a flonum" y))
                (error 'fl<? "~s is not a flonum" x))]
           [(x) 
            (or (flonum? x)
                (error 'fl<? "~s is not a flonum" x))]
           [(x y . rest) 
            (let ()
              (define (loopf a ls)
                (unless (flonum? a) 
                  (error 'fl<? "~s is not a flonum" a))
                (if (null? ls) 
                    #f
                    (loopf (car ls) (cdr ls))))
              (if (flonum? x) 
                  (if (flonum? y) 
                      (if ($fl< x y) 
                          (let f ([x y] [y (car rest)] [ls (cdr rest)])
                            (if (flonum? y) 
                                (if (null? ls)
                                    ($fl< x y)
                                    (if ($fl< x y) 
                                        (f y (car ls) (cdr ls))
                                        (loopf (car ls) (cdr ls))))
                                (error 'fl<? "~s is not a flonum" y)))
                          (loopf (car rest) (cdr rest)))
                      (error 'fl<? "~s is not a flonum" y))
                  (error 'fl<? "~s is not a flonum" x)))]))]))
  (define-flcmp fl=? $fl=)
  (define-flcmp fl<? $fl<)
  (define-flcmp fl<=? $fl<=)
  (define-flcmp fl>? $fl>)
  (define-flcmp fl>=? $fl>=)

  (define fl+ 
    (case-lambda
      [(x y) 
       (if (flonum? x)
           (if (flonum? y) 
               ($fl+ x y)
               (error 'fl+ "~s is not a flonum" y))
           (error 'fl+ "~s is not a flonum" x))]
      [(x y z) 
       (fl+ (fl+ x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl+ (fl+ (fl+ x y) z) q)] [rest rest])
         (if (null? rest) 
             ac
             (f (fl+ ac (car rest)) (cdr rest))))]
      [(x) 
       (if (flonum? x) 
           x
           (error 'fl+ "~s is not a flonum" x))]
      [() (exact->inexact 0)]))


  (define fl-
    (case-lambda
      [(x y) 
       (if (flonum? x)
           (if (flonum? y) 
               ($fl- x y)
               (error 'fl- "~s is not a flonum" y))
           (error 'fl- "~s is not a flonum" x))]
      [(x y z) 
       (fl- (fl- x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl- (fl- (fl- x y) z) q)] [rest rest])
         (if (null? rest) 
             ac
             (f (fl- ac (car rest)) (cdr rest))))]
      [(x) 
       (if (flonum? x) 
           ($fl* -1.0 x)
           (error 'fl+ "~s is not a flonum" x))]))

  (define fl*
    (case-lambda
      [(x y) 
       (if (flonum? x)
           (if (flonum? y) 
               ($fl* x y)
               (error 'fl* "~s is not a flonum" y))
           (error 'fl* "~s is not a flonum" x))]
      [(x y z) 
       (fl* (fl* x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl* (fl* (fl* x y) z) q)] [rest rest])
         (if (null? rest) 
             ac
             (f (fl* ac (car rest)) (cdr rest))))]
      [(x) 
       (if (flonum? x) 
           x
           (error 'fl* "~s is not a flonum" x))]
      [() 1.0]))

  (define fl/
    (case-lambda
      [(x y) 
       (if (flonum? x)
           (if (flonum? y) 
               ($fl/ x y)
               (error 'fl/ "~s is not a flonum" y))
           (error 'fl/ "~s is not a flonum" x))]
      [(x y z) 
       (fl/ (fl/ x y) z)]
      [(x y z q . rest)
       (let f ([ac (fl/ (fl/ (fl/ x y) z) q)] [rest rest])
         (if (null? rest) 
             ac
             (f (fl/ ac (car rest)) (cdr rest))))]
      [(x) 
       (if (flonum? x) 
           ($fl/ 1.0 x)
           (error 'fl/ "~s is not a flonum" x))])) 

  (flcmp flfl= flfx= fxfl= flbn= bnfl= $fl=)
  (flcmp flfl< flfx< fxfl< flbn< bnfl< $fl<)
  (flcmp flfl> flfx> fxfl> flbn> bnfl> $fl>)
  (flcmp flfl<= flfx<= fxfl<= flbn<= bnfl<= $fl<=)
  (flcmp flfl>= flfx>= fxfl>= flbn>= bnfl>= $fl>=)

  (define-syntax flrt= (syntax-rules () [(_ x y) (= (inexact->exact x) y)]))
  (define-syntax rtfl= (syntax-rules () [(_ x y) (= x (inexact->exact y))]))
  (define-syntax flrt<  (syntax-rules () [(_ x y) (< (inexact->exact x) y)]))
  (define-syntax rtfl<  (syntax-rules () [(_ x y) (< x (inexact->exact y))]))
  (define-syntax flrt<= (syntax-rules () [(_ x y) (<= (inexact->exact x) y)]))
  (define-syntax rtfl<= (syntax-rules () [(_ x y) (<= x (inexact->exact y))]))
  (define-syntax flrt>  (syntax-rules () [(_ x y) (> (inexact->exact x) y)]))
  (define-syntax rtfl>  (syntax-rules () [(_ x y) (> x (inexact->exact y))]))
  (define-syntax flrt>= (syntax-rules () [(_ x y) (>= (inexact->exact x) y)]))
  (define-syntax rtfl>= (syntax-rules () [(_ x y) (>= x (inexact->exact y))]))
  (define (exrt< x y) (< (* x ($ratnum-d y)) ($ratnum-n y)))
  (define (rtex< x y) (< ($ratnum-n x) (* y ($ratnum-d x))))
  (define (rtrt< x y) (< (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (rtrt<= x y) (<= (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (exrt> x y) (> (* x ($ratnum-d y)) ($ratnum-n y)))
  (define (rtex> x y) (> ($ratnum-n x) (* y ($ratnum-d x))))
  (define (rtrt> x y) (> (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (rtrt>= x y) (>= (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
  (define (rtrt= x y)
    (and (= ($ratnum-n x) ($ratnum-n y)) (= ($ratnum-d x) ($ratnum-d y))))

  (define = 
    (mk< = $fx= false false bnbn= fxfl= flfx= bnfl= flbn= flfl=
               false false false false flrt= rtfl= rtrt=))
  (define < 
    (mk< < $fx< fxbn< bnfx< bnbn< fxfl< flfx< bnfl< flbn< flfl<
               exrt< rtex< exrt< rtex< flrt< rtfl< rtrt<))
  (define >
    (mk< > $fx> fxbn> bnfx> bnbn> fxfl> flfx> bnfl> flbn> flfl>
               exrt> rtex> exrt> rtex> flrt> rtfl> rtrt>))
  (define <= 
    (mk< <= $fx<= fxbn< bnfx< bnbn<= fxfl<= flfx<= bnfl<= flbn<= flfl<=
               exrt< rtex< exrt< rtex< flrt<= rtfl<= rtrt<=))
  (define >= 
    (mk< >= $fx>= fxbn> bnfx> bnbn>= fxfl>= flfx>= bnfl>= flbn>= flfl>=
               exrt> rtex> exrt> rtex> flrt>= rtfl>= rtrt>=))

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
        [(flonum? x)
         (or ($fl= x 0.0) ($fl= x -0.0))]
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
             (/ 1 (expt n (- m))))]
        [(bignum? m) 
         (cond
           [(eq? n 0) 0]
           [(eq? n 1) 1]
           [(eq? n -1)
            (if (positive-bignum? m)
                (if (even-bignum? m)
                    1
                    -1)
                (/ 1 (expt n (- m))))]
           [else 
            (error 'expt "(expt ~s ~s) is too big to compute" n m)])]
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
           [(flonum? y) 
            (let ([v ($flonum->exact y)])
              (cond
                [(or (fixnum? v) (bignum? v)) 
                 (let-values ([(q r) (quotient+remainder x v)])
                   (values (inexact q) (inexact r)))]
                [else
                 (error 'quotient+remainder "~s is not an integer" y)]))]
           [else (error 'quotient+remainder "~s is not a number" y)])]
        [(bignum? x)
         (cond
           [(fixnum? y)
            (let ([p (foreign-call "ikrt_bnfxdivrem" x y)])
              (values (car p) (cdr p)))]
           [(bignum? y)
            (let ([p (foreign-call "ikrt_bnbndivrem" x y)])
              (values (car p) (cdr p)))]
           [(flonum? y) 
            (let ([v ($flonum->exact y)])
              (cond
                [(or (fixnum? v) (bignum? v)) 
                 (let-values ([(q r) (quotient+remainder x v)])
                   (values (inexact q) (inexact r)))]
                [else
                 (error 'quotient+remainder "~s is not an integer" y)]))] 
           [else (error 'quotient+remainder "~s is not a number" y)])]
        [(flonum? x) 
         (let ([v ($flonum->exact x)])
           (cond
             [(or (fixnum? v) (bignum? v)) 
              (let-values ([(q r) (quotient+remainder v y)])
                (values (inexact q) (inexact r)))]
             [else (error 'quotient+remainder "~s is not an integer" x)]))]
        [else (error 'quotient+remainder "~s is not a number" x)])))

  (define positive?
    (lambda (x)
      (cond
        [(fixnum? x) ($fx> x 0)]
        [(flonum? x) ($fl> x 0.0)]
        [(bignum? x) (positive-bignum? x)]
        [(ratnum? x) (positive? ($ratnum-n x))]
        [else (error 'positive? "~s is not a number" x)])))

  (define negative?
    (lambda (x)
      (cond
        [(fixnum? x) ($fx< x 0)]
        [(flonum? x) ($fl< x 0.0)]
        [(bignum? x) (not (positive-bignum? x))]
        [(ratnum? x) (negative? ($ratnum-n x))]
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

  (define tan
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_tan" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_tan" x)]
        [else (error 'tan "unsupported ~s" x)])))

  (define asin
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_asin" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_asin" x)]
        [else (error 'asin "unsupported ~s" x)])))

  (define acos
    (lambda (x)
      (cond
        [(flonum? x) (foreign-call "ikrt_fl_acos" x)]
        [(fixnum? x) (foreign-call "ikrt_fx_acos" x)]
        [else (error 'acos "unsupported ~s" x)])))

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
        [(bignum? x) (error 'sqrt "BUG: bignum sqrt not implemented")]
        [(ratnum? x) (/ (sqrt ($ratnum-n x)) (sqrt ($ratnum-d x)))]
        [else (error 'sqrt "unsupported ~s" x)])))

  (define flsqrt
    (lambda (x)
      (if (flonum? x) 
          (foreign-call "ikrt_fl_sqrt" x)
          (error 'flsqrt "~s is not a flonum" x))))

  (define flzero?
    (lambda (x)
      (if (flonum? x) 
          ($flzero? x)
          (error 'flzero? "~s is not a flonum" x))))

  (define flnegative?
    (lambda (x)
      (if (flonum? x) 
          ($fl< x 0.0)
          (error 'flnegative? "~s is not a flonum" x))))

  (define exact-integer-sqrt
    (lambda (x)
      (define who 'exact-integer-sqrt)
      (define (fxsqrt x i k) 
        (let ([j ($fxsra ($fx+ i k) 1)])
          (let ([j^2 ($fx* j j)])
             (if ($fx> j^2 x)
                 (fxsqrt x i j)
                 (if ($fx= i j) 
                     (values j ($fx- x j^2))
                     (fxsqrt x j k))))))
      (define (bnsqrt x i k) 
        (let ([j (quotient (+ i k) 2)])
          (let ([j^2 (* j j)])
             (if (> j^2 x)
                 (bnsqrt x i j)
                 (if (= i j) 
                     (values j (- x j^2))
                     (bnsqrt x j k))))))
      (cond
        [(fixnum? x) 
         (cond
           [($fx< x 0) (error who "invalid argument ~s" x)]
           [($fx= x 0) (values 0 0)]
           [($fx< x 4) (values 1 ($fx- x 1))]
           [($fx< x 9) (values 2 ($fx- x 4))]
           [($fx< x 46340) (fxsqrt x 3 ($fxsra x 1))]
           [else           (fxsqrt x 215 23171)])]
        [(bignum? x) 
         (cond
           [($bignum-positive? x) 
            (bnsqrt x 23170 (quotient x 23170))]
           [else (error who "invalid argument ~s" x)])]
        [else (error who "invalid argument ~s" x)])))


  (define numerator
    (lambda (x)
      (cond
        [(ratnum? x) ($ratnum-n x)]
        [(or (fixnum? x) (bignum? x)) x]
        [(flonum? x) (flnumerator x)]
        [else (error 'numerator "~s is not an exact integer" x)])))

  (define denominator
    (lambda (x)
      (cond
        [(ratnum? x) ($ratnum-d x)]
        [(or (fixnum? x) (bignum? x)) 1]
        [(flonum? x) (fldenominator x)]
        [else (error 'denominator "~s is not an exact integer" x)])))


  (define (floor x)
    (define (ratnum-floor x)
      (let ([n (numerator x)] [d (denominator x)])
        (let ([q (quotient n d)])
          (if (>= n 0) q (- q 1)))))
    (cond
      [(flonum? x) 
       (let ([e (or ($flonum->exact x)
                    (error 'floor "~s has no real value" x))])
         (cond
           [(ratnum? e) 
            (exact->inexact (ratnum-floor e))] 
           [else x]))]
      [(ratnum? x) (ratnum-floor x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (error 'floor "~s is not a number" x)]))
  
  (define (ceiling x)
    (define (ratnum-ceiling x)
      (let ([n (numerator x)] [d (denominator x)])
        (let ([q (quotient n d)])
          (if (< n 0) q (+ q 1)))))
    (cond
      [(flonum? x) 
       (let ([e (or ($flonum->exact x)
                    (error 'ceiling "~s has no real value" x))])
         (cond
           [(ratnum? e) (exact->inexact (ratnum-ceiling e))] 
           [else x]))]
      [(ratnum? x) (ratnum-ceiling x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (error 'ceiling "~s is not a number" x)]))


  (define ($ratnum-round x)
    (let ([n ($ratnum-n x)] [d ($ratnum-d x)])
      (let-values ([(q r) (quotient+remainder n d)])
        (let ([r2 (+ r r)]) 
          (if (> n 0) 
              (cond
                [(< r2 d) q]
                [(> r2 d) (+ q 1)]
                [else
                 (if (even? q) q (+ q 1))])
              (let ([r2 (- r2)])
                (cond
                  [(< r2 d) q]
                  [(< r2 d) (- q 1)]
                  [else
                   (if (even? q) q (- q 1))])))))))


  (define ($ratnum-truncate x)
    (let ([n ($ratnum-n x)] [d ($ratnum-d x)])
      (quotient n d)))

  ;(define ($flround x)
  ;  (foreign-call "ikrt_fl_round" x ($make-flonum)))

  ;  (let ([e ($flonum->exact x)])
  ;    (cond
  ;      [(not e) x] ;;; infs and nans round to themselves
  ;      [(ratnum? e) (exact->inexact ($ratnum-round e))]
  ;      [else (exact->inexact e)])))

  (define (flround x)
    (if (flonum? x)
        (let ([e ($flonum->exact x)])
           (cond
             [(ratnum? e) (exact->inexact ($ratnum-round e))]
             [else x]))
        (error 'flround "~s is not a flonum" x)))

  (define (round x)
    (cond
      [(flonum? x) 
       (let ([e (or ($flonum->exact x) 
                    (error 'round "~s has no real value" x))])
         (cond
           [(ratnum? e) (exact->inexact ($ratnum-round e))]
           [else x]))]
      [(ratnum? x) ($ratnum-round x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (error 'round "~s is not a number" x)]))

  (define (truncate x)
    (cond
      [(flonum? x) 
       (let ([e (or ($flonum->exact x) 
                    (error 'truncate "~s has no real value" x))])
         (cond
           [(ratnum? e) (exact->inexact ($ratnum-truncate e))]
           [else x]))]
      [(ratnum? x) ($ratnum-truncate x)]
      [(or (fixnum? x) (bignum? x)) x]
      [else (error 'truncate "~s is not a number" x)]))
  
  (define (fltruncate x)
    (unless (flonum? x)
      (error 'fltruncate "~s is not a flonum" x))
    (let ([v ($flonum->exact x)])
      (cond
        [(ratnum? v) (exact->inexact ($ratnum-truncate x))]
        [else x])))

  (define log
    (lambda (x)
      (cond
        [(fixnum? x) 
         (cond
           [($fx= x 1) 0]
           [($fx= x 0) (error 'log "undefined around 0")]
           [($fx> x 0) (foreign-call "ikrt_fx_log" x)]
           [else (error 'log "negative argument ~s" x)])]
        [(flonum? x) 
         (cond
           [(>= x 0) (foreign-call "ikrt_fl_log" x)]
           [else (error 'log "negative argument ~s" x)])]
        [(bignum? x) (log (exact->inexact x))]
        [(ratnum? x) (- (log (numerator x)) (log (denominator x)))]
        [else (error 'log "~s is not a number" x)])))

  (define string->number
    (lambda (x)
      (define (convert-char c radix)
        (case radix
          [(10) 
           (cond
             [(char<=? #\0 c #\9) 
              (fx- (char->integer c) (char->integer #\0))]
             [else #f])]
          [(16) 
           (cond
             [(char<=? #\0 c #\9) 
              (fx- (char->integer c) (char->integer #\0))]
             [(char<=? #\a c #\f) 
              (fx- (char->integer c) (fx- (char->integer #\a) 10))]
             [(char<=? #\A c #\F)
              (fx- (char->integer c) (fx- (char->integer #\A) 10))]
             [else #f])]
          [(8) 
           (cond
             [(char<=? #\0 c #\7) 
              (fx- (char->integer c) (char->integer #\0))]
             [else #f])] 
          [(2) 
           (case c
             [(#\0) 0]
             [(#\1) 1]
             [else #f])]
          [else (error 'convert-char "invalid radix ~s" radix)]))
      (define (parse-exponent-start x n i radix)
        (define (parse-exponent x n i radix ac) 
          (cond
            [(fx= i n) ac]
            [else
             (let ([c (string-ref x i)])
               (cond
                 [(convert-char c radix) =>
                  (lambda (d)
                    (parse-exponent x n (fxadd1 i) radix
                      (+ d (* ac radix))))]
                 [else #f]))]))
        (define (parse-exponent-sign x n i radix)
          (cond
            [(fx= i n) #f]
            [else
             (let ([c (string-ref x i)])
               (cond
                 [(convert-char c radix) =>
                  (lambda (d) (parse-exponent x n (fxadd1 i) radix d))]
                 [else #f]))]))
        (cond
          [(fx= i n) #f]
          [else
           (let ([c (string-ref x i)])
             (cond
               [(convert-char c radix) =>
                (lambda (d)
                  (parse-exponent x n (fxadd1 i) radix d))]
               [(char=? c #\+) 
                (parse-exponent-sign x n (fxadd1 i) radix)]
               [(char=? c #\-) 
                (let ([v (parse-exponent-sign x n (fxadd1 i) radix)])
                  (and v (- v)))]
               [else #f]))]))
      (define (parse-decimal x n i pos? radix exact? ac exp)
        (cond
          [(fx= i n) 
           (let ([ac (* (if pos? ac (- ac)) (expt radix exp))])
             (exact-conv (or exact? 'i) ac))]
          [else
           (let ([c (string-ref x i)])
             (cond
               [(convert-char c radix) =>
                (lambda (d)
                  (parse-decimal x n (fxadd1 i) pos? radix exact? 
                    (+ (* ac radix) d) (fxsub1 exp)))]
               [(memv c '(#\e #\E)) 
                (let ([ex (parse-exponent-start x n (fxadd1 i) radix)])
                  (and ex 
                       (exact-conv (or exact? 'i) 
                         (* (if pos? ac (- ac)) (expt radix (+ exp ex))))))]
               [else #f]))]))
      (define (parse-decimal-no-digits x n i pos? radix exact?)
        (cond
          [(fx= i n) #f]
          [else
           (let ([c (string-ref x i)])
             (cond
               [(convert-char c radix) =>
                (lambda (d)
                  (parse-decimal x n (fxadd1 i) pos? radix exact?  d -1))]
               [else #f]))])) 
      (define (parse-integer x n i pos? radix exact? ac)
        (define (parse-denom-start x n i radix)
          (define (parse-denom x n i radix ac)
            (cond
              [(fx= n i) ac]
              [else
               (let ([c (string-ref x i)])
                 (cond
                   [(convert-char c radix) =>
                    (lambda (d)
                      (parse-denom x n (fxadd1 i) radix 
                        (+ (* radix ac) d)))]
                   [else #f]))]))
          (cond
            [(fx= n i) #f]
            [else
             (let ([c (string-ref x i)])
               (cond
                 [(convert-char c radix) =>
                  (lambda (d) 
                    (parse-denom x n (fxadd1 i) radix d))]
                 [else #f]))]))
        (cond
          [(fx= i n)
           (let ([ac (exact-conv exact? ac)])
             (if pos? ac (- ac)))]
          [else
           (let ([c (string-ref x i)])
             (cond
               [(convert-char c radix) =>
                (lambda (d)
                  (parse-integer x n (fxadd1 i) pos? radix exact? (+ (* ac radix) d)))]
               [(char=? c #\.) 
                (parse-decimal x n (fxadd1 i) pos? radix exact? ac 0)]
               [(char=? c #\/) 
                (let ([denom (parse-denom-start x n (fxadd1 i) radix)])
                  (and denom 
                       (not (= denom 0))
                       (let ([ac (exact-conv exact? ac)])
                         (/ (if pos? ac (- ac)) denom))))]
               [(memv c '(#\e #\E))
                (let ([ex (parse-exponent-start x n (fxadd1 i) radix)])
                  (and ex 
                       (let ([ac (exact-conv (or exact? 'i) ac)])
                         (* (if pos? ac (- ac)) (expt radix ex)))))]
               [else #f]))]))
      (define (parse-integer-no-digits x n i pos? radix exact?) 
        (cond
          [(fx= i n) #f]
          [else
           (let ([c (string-ref x i)])
             (cond
               [(convert-char c radix) =>
                (lambda (d)
                  (parse-integer x n (fxadd1 i) pos? radix exact? d))]
               [(char=? c #\.) 
                (parse-decimal-no-digits x n (fxadd1 i) pos? radix exact?)]
               [else #f]))]))
      (define (exact-conv exact? x)
        (and x (if (eq? exact? 'i) (exact->inexact x) x)))
      (define (start x n i exact? radix?)
        (cond
          [(fx= i n) #f]
          [else
           (let ([c (string-ref x i)])
             (cond
               [(char=? c #\-) 
                (parse-integer-no-digits x n (fxadd1 i) #f (or radix? 10) exact?)]
               [(char=? c #\+)
                (parse-integer-no-digits x n (fxadd1 i) #t (or radix? 10) exact?)]
               [(char=? c #\#)
                (let ([i (fxadd1 i)])
                  (cond
                    [(fx= i n) #f]
                    [else
                     (let ([c (string-ref x i)])
                       (case c
                         [(#\x #\X) 
                          (and (not radix?) (start x n (fxadd1 i) exact? 16))]
                         [(#\b #\B) 
                          (and (not radix?) (start x n (fxadd1 i) exact? 2))]
                         [(#\o #\O) 
                          (and (not radix?) (start x n (fxadd1 i) exact? 8))]
                         [(#\d #\D) 
                          (and (not radix?) (start x n (fxadd1 i) exact? 10))]
                         [(#\e #\E) 
                          (and (not exact?) (start x n (fxadd1 i) 'e radix?))]
                         [(#\i #\I) 
                          (and (not exact?) (start x n (fxadd1 i) 'i radix?))]
                         [else #f]))]))]
               [(char=? c #\.)
                (parse-decimal-no-digits x n (fxadd1 i) #t (or radix?  10) exact?)]
               [(convert-char c (or radix? 10)) =>
                (lambda (d)
                  (parse-integer x n (fxadd1 i) #t (or radix? 10) exact? d))]
               [else #f]))]))
      ;;;
      (unless (string? x)
        (error 'string->number "~s is not a string" x))
      (let ([n (string-length x)])
        (cond
          [(fx= n (string-length "+xxx.0"))
           (cond
             [(string=? x "+inf.0") +inf.0]
             [(string=? x "-inf.0") -inf.0]
             [(string=? x "+nan.0") +nan.0]
             [(string=? x "-nan.0") -nan.0]
             [else (start x n 0 #f #f)])]
          [(fx> n 0) (start x n 0 #f #f)]
          [else #f]))))


  (define (random n) 
    (if (fixnum? n) 
        (if (fx> n 1) 
            (foreign-call "ikrt_fxrandom" n) 
            (if (fx= n 1) 
                0
                (error 'random "incorrect argument ~s" n)))
        (error 'random "~s is not a fixnum" n)))
  )


(library (ikarus flonum-conversion)
  (export string->flonum flonum->string)
  (import 
    (ikarus system $bytevectors)
    (ikarus system $flonums)
    (except (ikarus) flonum->string string->flonum ))
  
  (module (flonum->string)
    (module (flonum->digits)
      (define flonum->digits
        (lambda (f e min-e p b B)
          ;;; flonum v = f * b^e
          ;;; p = precision  (p >= 1)
          (let ([round? (even? f)])
            (if (>= e 0)
                (if (not (= f (expt b (- p 1))))
                    (let ([be (expt b e)])
                      (scale (* f be 2) 2 be be 0 B round? f e))
                    (let* ([be (expt b e)] [be1 (* be b)])
                      (scale (* f be1 2) (* b 2) be1 be 0 B round? f e)))
                (if (or (= e min-e) (not (= f (expt b (- p 1)))))
                    (scale (* f 2) (* (expt b (- e)) 2) 1 1 0 B round? f e)
                    (scale (* f b 2) (* (expt b (- 1 e)) 2) b 1 0 B round? f e))))))
      (define (len n) 
        (let f ([n n] [i 0])
          (cond
            [(zero? n) i]
            [else (f (quotient n 2) (+ i 1))])))
      (define scale
        (lambda (r s m+ m- k B round? f e)
          (let ([est (inexact->exact
                       (ceiling 
                         (- (* (+ e (len f) -1) (invlog2of B)) 
                            1e-10)))])
            (if (>= est 0)
                (fixup r (* s (exptt B est)) m+ m- est B round?)
                (let ([scale (exptt B (- est))])
                  (fixup (* r scale) s (* m+ scale) (* m- scale) est B round?))))))
      (define fixup
        (lambda (r s m+ m- k B round?) 
          (if ((if round? >= >) (+ r m+) s) ; too low?
              (values (+ k 1) (generate r s m+ m- B round?))
              (values k (generate (* r B) s (* m+ B) (* m- B) B round?)))))
      (define (chr x)
        (vector-ref '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) x))
      (define generate
        (lambda (r s m+ m- B round?) 
          (let-values ([(q r) (quotient+remainder r s)])
            (let ([tc1 ((if round? <= <) r m-)]
                  [tc2 ((if round? >= >) (+ r m+) s)])
              (if (not tc1)
                  (if (not tc2) 
                      (cons (chr q) (generate (* r B) s (* m+ B) (* m- B) B round?))
                      (list (chr (+ q 1))))
                  (if (not tc2)
                      (list (chr q))
                      (if (< (* r 2) s)
                          (list (chr q))
                          (list (chr (+ q 1))))))))))
      (define invlog2of
        (let ([table (make-vector 37)]
              [log2 (log 2)])
          (do ([B 2 (+ B 1)])
              ((= B 37))
            (vector-set! table B (/ log2 (log B))))
          (lambda (B)
            (if (<= 2 B 36)
                (vector-ref table B)
                (/ log2 (log B))))))
      (define exptt
        (let ([table (make-vector 326)])
          (do ([k 0 (+ k 1)] [v 1 (* v 10)])
              ((= k 326))
              (vector-set! table k v))
          (lambda (B k)
            (if (and (= B 10) (<= 0 k 325))
                (vector-ref table k)
                (expt B k))))))
    (define (format-flonum pos? expt digits)
      (define (next x)
        (if (null? x) 
            (values #\0 '())
            (values (car x) (cdr x))))
      (define (format-flonum-no-expt expt d0 d*)
        (cond
          [(= expt 1) 
           (cons d0 (if (null? d*) '(#\. #\0) (cons #\. d*)))]
          [else
           (cons d0
             (let-values ([(d0 d*) (next d*)])
               (format-flonum-no-expt (- expt 1) d0 d*)))]))
      (define (format-flonum-no-expt/neg expt d*)
        (cond
          [(= expt 0) d*]
          [else (cons #\0 (format-flonum-no-expt/neg (+ expt 1) d*))]))
      (define (sign pos? ls)
        (if pos?
           (list->string ls)
           (list->string (cons #\- ls))))
      (let ([d0 (car digits)] [d* (cdr digits)])
        (cond
          [(null? d*) 
           (if (char=? d0 #\0) 
               (if pos? "0.0" "-0.0")
               (if (= expt 1) 
                   (if pos?
                       (string d0 #\. #\0)
                       (string #\- d0 #\. #\0))
                   (if (= expt 0)
                       (if pos?
                           (string #\0 #\. d0)
                           (string #\- #\0 #\. d0))
                       (string-append
                         (if pos? "" "-")
                         (string d0) "e" (fixnum->string (- expt 1))))))]
          [(and (null? d*) (char=? d0 #\0)) (if pos? "0.0" "-0.0")]
          [(<= 1 expt 9)
           (sign pos? (format-flonum-no-expt expt d0 d*))]
          [(<= -3 expt 0)
           (sign pos? (cons* #\0 #\. (format-flonum-no-expt/neg expt digits)))]
          [else 
           (string-append
             (if pos? "" "-")
             (string d0) "." (list->string d*) 
             "e" (fixnum->string (- expt 1)))])))
    (define (flo->string pos? m e p)
    ;  (printf "compo: ~s ~s ~s\n" m e p)
      (let-values ([(expt digits) (flonum->digits m e 10 p 2 10)])
         (format-flonum pos? expt digits)))
    (define (flonum->string x)
      (let-values ([(pos? be m) (flonum-parts x)])
        (cond
          [(<= 1 be 2046) ; normalized flonum
           (flo->string pos? (+ m (expt 2 52)) (- be 1075) 53)]
          [(= be 0) 
           (flo->string pos? m -1074 52)]
          [(= be 2047)
           (if (= m 0) 
               (if pos? "+inf.0" "-inf.0") 
               ;;; Gee!  nans have no sign!
               "+nan.0")]
          [else (error 'flonum->string "cannot happen")]))))
  ;;;
  (define (string->flonum x)
    (cond
      [(string? x)
       (foreign-call "ikrt_bytevector_to_flonum" 
         (string->utf8-bytevector x))]
      [else 
       (error 'string->flonum "~s is not a string" x)])) )

(library (ikarus rationalize)
  (export rationalize)
  (import 
    (except (ikarus) rationalize))
 
  (define (rationalize x eps) 
    (define who 'rationalize)
    (define (simplest x y) 
      (cond
        [(< y x) (simplest y x)]
        [(= x y) x]
        [(> x 0)
         (let ([n (numerator x)] [d (denominator x)]
               [n^ (numerator y)] [d^ (denominator y)])
           (simplest^ n d n^ d^))]
        [(< y 0) 
         (let ([n (numerator x)] [d (denominator x)]
               [n^ (numerator y)] [d^ (denominator y)])
           (- (simplest^ (- n^) d^ (- n) d)))]
        [else 1]))
    (define (simplest^ n d n^ d^)
      (let-values ([(q r) (quotient+remainder n d)])
        (if (= r 0) 
            q
            (let-values ([(q^ r^) (quotient+remainder n^ d^)])
              (if (= q q^) 
                  (let ([v (simplest^ d^ r^ d r)])
                    (let ([n^^ (numerator v)] [d^^ (denominator v)])
                      (/ (+ (* q n^^) d^^) n^^)))
                  (+ q 1))))))
    (define (go x eps)
      (simplest (- x eps) (+ x eps)))
    (cond
      [(flonum? x) 
       (if (flfinite? x)
           (cond
             [(flonum? eps) 
              (if (flfinite? eps) (go x eps) +nan.0)]
             [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
              (go x eps)]
             [else (error who "~s is not a number" eps)])
           (cond
             [(flonum? eps) 
              (if (flfinite? eps) x +nan.0)]
             [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
              x]
             [else (error who "~s is not a number" eps)]))]
      [(or (fixnum? x) (bignum? x) (ratnum? x))
       (cond
         [(flonum? eps) 
          (if (flfinite? eps) (go x eps) +nan.0)]
         [(or (fixnum? eps) (bignum? eps) (ratnum? eps))
          (go x eps)]
         [else (error who "~s is not a number" eps)])]
      [else (error who "~s is not a number" x)])))


(library (ikarus r6rs-fu div/mod)
  (export div mod div-and-mod div0 mod0 div0-and-mod0)
  (import 
    (except (ikarus) 
      div mod div-and-mod div0 mod0 div0-and-mod0))

  (define (div-and-mod x y) 
    (define who 'div-and-mod)
    (unless (integer? x)
      (error who "~s is not an integer" x))
    (unless (and (integer? y) (not (= y 0)))
      (error who "~s is not an integer" y))
    (if (> x 0) 
        (quotient+remainder x y)
        (if (> y 0) 
            (let-values ([(q r) (quotient+remainder (- x y) y)])
              (values q (+ r y)))
            (let-values ([(q r) (quotient+remainder (+ x y) y)])
              (values q (- r y))))))
  
  (define (div x y) 
    (let-values ([(n m) (div-and-mod x y)])
       n))
  
  (define (mod x y) 
    (let-values ([(n m) (div-and-mod x y)])
       m))
  
  (define (div0-and-mod0 x y) 
    (define who 'div0-and-mod0)
    (unless (integer? x)
      (error who "~s is not an integer" x))
    (unless (and (integer? y) (not (= y 0)))
      (error who "~s is not an integer" y))
    (let-values ([(d m) (div-and-mod x y)])
      (if (> y 0) 
          (if (< m (/ y 2))
              (values d m)
              (values (+ d 1) (- m y)))
          (if (> m (/ y -2))
              (values (- d 1) (+ m y))
              (values d m)))))
  
  (define (div0 x y) 
    (let-values ([(n m) (div0-and-mod0 x y)])
       n))
  
  (define (mod0 x y) 
    (let-values ([(n m) (div0-and-mod0 x y)])
       m)))
  

