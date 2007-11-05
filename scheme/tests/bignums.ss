(library (tests bignums)
  (export test-bignums)
  (import (ikarus) (tests framework))

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
    ))


