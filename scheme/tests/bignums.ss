(library (tests bignums)
  (export test-bignums test-bignum-conversion)
  (import (ikarus) (tests framework))

  (define (test-bignum-conversion)
    (define (test x) 
      (define (test1 x prefix radix)
        (let ([s (string-append prefix 
                   (number->string x radix))])
          (assert (equal? x (read (open-input-string s))))))
      (test1 x "#x" 16)
      (test1 x "#o" 8)
      (test1 x "#b" 2))
    (test #b11111111111111111111111111111111111111111111111111)
    (test #b1111111111111111111111111111111111111111)
    (test 39487932748923498234)
    (test #b-11111111111111111111111111111111111111111111111111)
    (test #b-1111111111111111111111111111111111111111)
    (test -39487932748923498234))



  (define-tests test-bignums
    ; first, some simple quotients
    [(lambda (x) (= x 101))  (quotient 348972 3434)]
    [(lambda (x) (= x -101)) (quotient -348972 3434)]
    [(lambda (x) (= x -101)) (quotient 348972 -3434)]
    [(lambda (x) (= x 101))  (quotient -348972 -3434)]
    ; then bump first argument to a small bignum:
    [(lambda (x) (= x 2255760))  (quotient 536870912 238)]
    [(lambda (x) (= x -2255760)) (quotient -536870912 238)]
    [(lambda (x) (= x -2255760)) (quotient 536870912 -238)]
    [(lambda (x) (= x 2255760))  (quotient -536870912 -238)]
    ; then bump first argument to a big bignum:
    [(lambda (x) (= x 1652556267336712615))
     (quotient 536870912238479837489374 324873)]
    [(lambda (x) (= x -1652556267336712615))
     (quotient -536870912238479837489374 324873)]
    [(lambda (x) (= x -1652556267336712615))
     (quotient 536870912238479837489374 -324873)]
    [(lambda (x) (= x 1652556267336712615))
     (quotient -536870912238479837489374 -324873)]
    ; then make both arguments bignums, but result fixnum:
    [(lambda (x) (= x 165)) 
     (quotient 536870912238479837489374 3248732398479823749283)]
    [(lambda (x) (= x -165)) 
     (quotient -536870912238479837489374 3248732398479823749283)]
    [(lambda (x) (= x -165)) 
     (quotient 536870912238479837489374 -3248732398479823749283)]
    [(lambda (x) (= x 165)) 
     (quotient -536870912238479837489374 -3248732398479823749283)]
    ; then both arguments and result are all bignums:
    [(lambda (x) (= x 1652555047284588078))
     (quotient 5368709122384798374893743894798327498234 3248732398479823749283)]
    [(lambda (x) (= x -1652555047284588078))
     (quotient -5368709122384798374893743894798327498234 3248732398479823749283)]
    [(lambda (x) (= x -1652555047284588078))
     (quotient 5368709122384798374893743894798327498234 -3248732398479823749283)]
    [(lambda (x) (= x 1652555047284588078))
     (quotient -5368709122384798374893743894798327498234 -3248732398479823749283)]




    [(lambda (x) (= x 23))            (remainder 23 349839489348)]
    [(lambda (x) (= x -23))           (remainder -23 349839489348)]
    [(lambda (x) (= x 23))            (remainder 23 -349839489348)]
    [(lambda (x) (= x -23))           (remainder -23 -349839489348)]
    

    ;;; Next, modulo
    ; first, some simple arguments
    [(lambda (x) (= x 2138))  (modulo 348972 3434)]
    [(lambda (x) (= x 1296))  (modulo -348972 3434)]
    [(lambda (x) (= x -1296)) (modulo 348972 -3434)]
    [(lambda (x) (= x -2138)) (modulo -348972 -3434)]
    ; then bignum second argument: can be done with +/-
    [(lambda (x) (= x 349839489325))  (modulo -23 349839489348)]
    [(lambda (x) (= x -23))           (modulo -23 -349839489348)]
    [(lambda (x) (= x 23))            (modulo 23 349839489348)]
    [(lambda (x) (= x -349839489325)) (modulo 23 -349839489348)]

    ; then bump first argument to a small bignum:
    [(lambda (x) (= x 32))  (remainder 536870912 238)]
    [(lambda (x) (= x -32)) (remainder -536870912 238)]
    [(lambda (x) (= x 32))  (remainder 536870912 -238)]
    [(lambda (x) (= x -32)) (remainder -536870912 -238)]

    [(lambda (x) (= x 32))   (modulo 536870912 238)]
    [(lambda (x) (= x 206))  (modulo -536870912 238)]
    [(lambda (x) (= x -206)) (modulo 536870912 -238)]
    [(lambda (x) (= x -32))  (modulo -536870912 -238)]
    ; then bump first argument to a big bignum:
    [(lambda (x) (= x 116479))
     (modulo 536870912238479837489374 324873)]
    [(lambda (x) (= x 208394))
     (modulo -536870912238479837489374 324873)]
    [(lambda (x) (= x -208394))
     (modulo 536870912238479837489374 -324873)]
    [(lambda (x) (= x -116479))
     (modulo -536870912238479837489374 -324873)]
    ; then make both arguments bignums
    [(lambda (x) (= x 830066489308918857679)) 
     (modulo 536870912238479837489374 3248732398479823749283)]
    [(lambda (x) (= x -2418665909170904891604)) 
     (modulo 536870912238479837489374 -3248732398479823749283)]
    [(lambda (x) (= x 2418665909170904891604)) 
     (modulo -536870912238479837489374 3248732398479823749283)]
    [(lambda (x) (= x -830066489308918857679))
     (modulo -536870912238479837489374 -3248732398479823749283)]




    [(lambda (x) (= x -13)) (bitwise-not 12)]
    [(lambda (x) (= x 11)) (bitwise-not -12)]
    [(lambda (x) (= x 0)) (bitwise-not -1)]
    [(lambda (x) (= x -1)) (bitwise-not 0)]
    [(lambda (x) (= x (least-fixnum))) (bitwise-not (greatest-fixnum))]
    [(lambda (x) (= x (greatest-fixnum))) (bitwise-not (least-fixnum))]

    [(lambda (x) (= x -38947389478348937489375)) 
     (bitwise-not 38947389478348937489374)]
    [(lambda (x) (= x -22300745198530623141535718272648361505980416))
     (bitwise-not #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)]
    [(lambda (x) (= x 38947389478348937489374)) 
     (bitwise-not -38947389478348937489375)]
    [(lambda (x) (= x 22300745198530623141535718272648361505980414))
     (bitwise-not #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)]
    [(lambda (x) (= x -340282366920938463463374607431768211456))
     (bitwise-not #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)]
    [(lambda (x) (= x 340282366920938463463374607431768211454))
     (bitwise-not #x-FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)]
    [(lambda (x) (= x -79228162514264337593543950337))
     (bitwise-not #x1000000000000000000000000)]
    [(lambda (x) (= x 79228162514264337593543950335))
     (bitwise-not #x-1000000000000000000000000)]


    ))


