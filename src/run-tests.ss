#!/usr/bin/env ikarus --r6rs-script

(import (ikarus)
        (tests reader)
        (tests bytevectors))

(define (test-exact-integer-sqrt)
  (define (f i j inc)
    (when (< i j)
      (let-values ([(s r) (exact-integer-sqrt i)])
        (unless (and (= (+ (* s s) r) i)
                     (< i (* (+ s 1) (+ s 1))))
          (error 'exact-integer-sqrt "wrong result for ~s" i))
        (f (+ i inc) j inc))))
  (f 0 10000 1)
  (f 0 536870911 10000)
  (f 0 536870911000 536870911)
  (printf "[exact-integer-sqrt] Happy Happy Joy Joy\n"))

(test-reader)
(test-bytevectors)
(test-exact-integer-sqrt)
(printf "Happy Happy Joy Joy\n")
