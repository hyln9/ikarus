; tak tak
(define-syntax if-fixflo (syntax-rules () ((if-fixflo yes no) no)))
;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (time (run-bench name count ok? run))))
    (if (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline))))
  (exit 0))

(define (fatal-error . args)
  (apply error #f args))

 (define (call-with-output-file/truncate filename proc)
   (call-with-output-file filename proc 'truncate))

;------------------------------------------------------------------------------

; Macros...

(if-fixflo

(begin

; Specialize fixnum and flonum arithmetic.

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax FLOATvector?
  (syntax-rules ()
    ((FLOATvector? x) (vector? x))))

(define-syntax FLOATvector
  (syntax-rules ()
    ((FLOATvector x ...) (vector x ...))))

(define-syntax FLOATmake-vector
  (syntax-rules ()
    ((FLOATmake-vector n) (make-vector n 0.0))
    ((FLOATmake-vector n init) (make-vector n init))))

(define-syntax FLOATvector-ref
  (syntax-rules ()
    ((FLOATvector-ref v i) (vector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (vector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (vector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+ x ...) (fl+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (fl- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (fl* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (fl/ x ...))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (fl= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (fl< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (fl<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (fl> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (fl>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (fl< x 0.0))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (fl< 0.0 x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (fl= 0.0 x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (flabs x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (sin x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (cos x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (atan x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (sqrt x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (min x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (max x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (round x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

(define (GENERIC+ x y) (+ x y))
(define (GENERIC- x y) (- x y))
(define (GENERIC* x y) (* x y))
(define (GENERIC/ x y) (/ x y))
(define (GENERICquotient x y) (quotient x y))
(define (GENERICremainder x y) (remainder x y))
(define (GENERICmodulo x y) (modulo x y))
(define (GENERIC= x y) (= x y))
(define (GENERIC< x y) (< x y))
(define (GENERIC<= x y) (<= x y))
(define (GENERIC> x y) (> x y))
(define (GENERIC>= x y) (>= x y))
(define (GENERICexpt x y) (expt x y))

(define-syntax +
  (syntax-rules ()
    ((+ x ...) (fx+ x ...))))

(define-syntax -
  (syntax-rules ()
    ((- x ...) (fx- x ...))))

(define-syntax *
  (syntax-rules ()
    ((* x ...) (fx* x ...))))

(define-syntax quotient
  (syntax-rules ()
    ((quotient x ...) (fxquotient x ...))))

(define-syntax modulo
  (syntax-rules ()
    ((modulo x ...) (fxmodulo x ...))))

(define-syntax remainder
  (syntax-rules ()
    ((remainder x ...) (fxremainder x ...))))

(define-syntax =
  (syntax-rules ()
    ((= x y) (fx= x y))))

(define-syntax <
  (syntax-rules ()
    ((< x y) (fx< x y))))

(define-syntax <=
  (syntax-rules ()
    ((<= x y) (fx<= x y))))

(define-syntax >
  (syntax-rules ()
    ((> x y) (fx> x y))))

(define-syntax >=
  (syntax-rules ()
    ((>= x y) (fx>= x y))))

(define-syntax negative?
  (syntax-rules ()
    ((negative? x) (fxnegative? x))))

(define-syntax positive?
  (syntax-rules ()
    ((positive? x) (fxpositive? x))))

(define-syntax zero?
  (syntax-rules ()
    ((zero? x) (fxzero? x))))

(define-syntax odd?
  (syntax-rules ()
    ((odd? x) (fxodd? x))))

(define-syntax even?
  (syntax-rules ()
    ((even? x) (fxeven? x))))

; FIXME

;(define-syntax bitwise-or
;  (syntax-rules ()
;    ((bitwise-or x y) (fxior x y))))

;(define-syntax bitwise-and
;  (syntax-rules ()
;    ((bitwise-and x y) (fxand x y))))

;(define-syntax bitwise-not
;  (syntax-rules ()
;    ((bitwise-not x) (fxnot x))))
)

(begin

; Don't specialize fixnum and flonum arithmetic.

(define-syntax FLOATvector-const
  (syntax-rules ()
    ((FLOATvector-const x ...) '#(x ...))))

(define-syntax FLOATvector?
  (syntax-rules ()
    ((FLOATvector? x) (vector? x))))

(define-syntax FLOATvector
  (syntax-rules ()
    ((FLOATvector x ...) (vector x ...))))

(define-syntax FLOATmake-vector
  (syntax-rules ()
    ((FLOATmake-vector n) (make-vector n 0.0))
    ((FLOATmake-vector n init) (make-vector n init))))

(define-syntax FLOATvector-ref
  (syntax-rules ()
    ((FLOATvector-ref v i) (vector-ref v i))))

(define-syntax FLOATvector-set!
  (syntax-rules ()
    ((FLOATvector-set! v i x) (vector-set! v i x))))

(define-syntax FLOATvector-length
  (syntax-rules ()
    ((FLOATvector-length v) (vector-length v))))

(define-syntax nuc-const
  (syntax-rules ()
    ((FLOATnuc-const x ...) '#(x ...))))

(define-syntax FLOAT+
  (syntax-rules ()
    ((FLOAT+ x ...) (+ x ...))))

(define-syntax FLOAT-
  (syntax-rules ()
    ((FLOAT- x ...) (- x ...))))

(define-syntax FLOAT*
  (syntax-rules ()
    ((FLOAT* x ...) (* x ...))))

(define-syntax FLOAT/
  (syntax-rules ()
    ((FLOAT/ x ...) (/ x ...))))

(define-syntax FLOAT=
  (syntax-rules ()
    ((FLOAT= x y) (= x y))))

(define-syntax FLOAT<
  (syntax-rules ()
    ((FLOAT< x y) (< x y))))

(define-syntax FLOAT<=
  (syntax-rules ()
    ((FLOAT<= x y) (<= x y))))

(define-syntax FLOAT>
  (syntax-rules ()
    ((FLOAT> x y) (> x y))))

(define-syntax FLOAT>=
  (syntax-rules ()
    ((FLOAT>= x y) (>= x y))))

(define-syntax FLOATnegative?
  (syntax-rules ()
    ((FLOATnegative? x) (negative? x))))

(define-syntax FLOATpositive?
  (syntax-rules ()
    ((FLOATpositive? x) (positive? x))))

(define-syntax FLOATzero?
  (syntax-rules ()
    ((FLOATzero? x) (zero? x))))

(define-syntax FLOATabs
  (syntax-rules ()
    ((FLOATabs x) (abs x))))

(define-syntax FLOATsin
  (syntax-rules ()
    ((FLOATsin x) (sin x))))

(define-syntax FLOATcos
  (syntax-rules ()
    ((FLOATcos x) (cos x))))

(define-syntax FLOATatan
  (syntax-rules ()
    ((FLOATatan x) (atan x))))

(define-syntax FLOATsqrt
  (syntax-rules ()
    ((FLOATsqrt x) (sqrt x))))

(define-syntax FLOATmin
  (syntax-rules ()
    ((FLOATmin x y) (min x y))))

(define-syntax FLOATmax
  (syntax-rules ()
    ((FLOATmax x y) (max x y))))

(define-syntax FLOATround
  (syntax-rules ()
    ((FLOATround x) (round x))))

(define-syntax FLOATinexact->exact
  (syntax-rules ()
    ((FLOATinexact->exact x) (inexact->exact x))))

; Generic arithmetic.

(define-syntax GENERIC+
  (syntax-rules ()
    ((GENERIC+ x ...) (+ x ...))))

(define-syntax GENERIC-
  (syntax-rules ()
    ((GENERIC- x ...) (- x ...))))

(define-syntax GENERIC*
  (syntax-rules ()
    ((GENERIC* x ...) (* x ...))))

(define-syntax GENERIC/
  (syntax-rules ()
    ((GENERIC/ x ...) (/ x ...))))

(define-syntax GENERICquotient
  (syntax-rules ()
    ((GENERICquotient x y) (quotient x y))))

(define-syntax GENERICremainder
  (syntax-rules ()
    ((GENERICremainder x y) (remainder x y))))

(define-syntax GENERICmodulo
  (syntax-rules ()
    ((GENERICmodulo x y) (modulo x y))))

(define-syntax GENERIC=
  (syntax-rules ()
    ((GENERIC= x y) (= x y))))

(define-syntax GENERIC<
  (syntax-rules ()
    ((GENERIC< x y) (< x y))))

(define-syntax GENERIC<=
  (syntax-rules ()
    ((GENERIC<= x y) (<= x y))))

(define-syntax GENERIC>
  (syntax-rules ()
    ((GENERIC> x y) (> x y))))

(define-syntax GENERIC>=
  (syntax-rules ()
    ((GENERIC>= x y) (>= x y))))

(define-syntax GENERICexpt
  (syntax-rules ()
    ((GENERICexpt x y) (expt x y))))
)
)

;------------------------------------------------------------------------------
; Gabriel benchmarks
(define boyer-iters        20)
(define browse-iters      600)
(define cpstak-iters     1000)
(define ctak-iters        100)
(define dderiv-iters  2000000)
(define deriv-iters   2000000)
(define destruc-iters     500)
(define diviter-iters 1000000)
(define divrec-iters  1000000)
(define puzzle-iters      100)
(define tak-iters        2000)
(define takl-iters        300)
(define trav1-iters       100)
(define trav2-iters        20)
(define triangl-iters      10)

; Kernighan and Van Wyk benchmarks
(define ack-iters           1)
(define array1-iters        1)
(define cat-iters           1)
(define string-iters        1)
(define sum1-iters          1)
(define sumloop-iters       1)
(define tail-iters          1)
(define wc-iters            1)

; C benchmarks
(define fft-iters        2000)
(define fib-iters           5)
(define fibfp-iters         2)
(define mbrot-iters       100)
(define nucleic-iters       5)
(define pnpoly-iters   100000)
(define sum-iters       10000)
(define sumfp-iters      5000)
(define tfib-iters         20)

; Other benchmarks
(define conform-iters      40)
(define dynamic-iters      20)
(define earley-iters      200)
(define fibc-iters        500)
(define graphs-iters      300)
(define lattice-iters       1)
(define matrix-iters      400)
(define maze-iters       4000)
(define mazefun-iters    1000)
(define nqueens-iters    2000)
(define paraffins-iters  1000)
(define peval-iters       200)
(define pi-iters            2)
(define primes-iters   100000)
(define ray-iters           5)
(define scheme-iters    20000)
(define simplex-iters  100000)
(define slatex-iters       20)
(define perm9-iters        10)
(define nboyer-iters      100)
(define sboyer-iters      100)
(define gcbench-iters       1)
(define compiler-iters    300)

; New benchmarks
(define parsing-iters    1000)
(define gcold-iters     10000)
;(define nbody-iters         1) ; nondeterministic (order of evaluation)
;;; TAK -- A vanilla version of the TAKeuchi function.
 
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))
 
(define (main . args)
  (run-benchmark
    "tak"
    tak-iters
    (lambda (result) (equal? result 7))
    (lambda (x y z) (lambda () (tak x y z)))
    18
    12
    6))
; tak tak
