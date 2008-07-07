
(library (tests numerics)
  (export test-numerics)
  (import (ikarus))

  (define (test-round x)
    (let ([rx (round x)])
      (unless (integer? rx)
        (error 'test-round "not an integer result for" x rx))
      (let ([diff (abs (- (abs x) (abs rx)))])
        (cond
          [(= diff 1/2) 
           (unless (even? rx)
             (error 'test-round "non-even rounding for" x rx))]
          [else
           (unless (< diff 1/2)
             (error 'test-round "rounding the wrong way for" x rx))]))))

  (define (test-numerics)
    (test-round -251/100)
    (test-round -250/100)
    (test-round -249/100)
    (test-round +251/100)
    (test-round +250/100)
    (test-round +249/100)

    (test-round -151/100)
    (test-round -150/100)
    (test-round -149/100)
    (test-round +151/100)
    (test-round +150/100)
    (test-round +149/100)

    (test-round -351/100)
    (test-round -350/100)
    (test-round -349/100)
    (test-round +351/100)
    (test-round +350/100)
    (test-round +349/100)))
