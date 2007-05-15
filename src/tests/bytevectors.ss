
(library (tests bytevectors)
  (export test-bytevectors)
  (import (ikarus) (tests framework))

  (define (not-bytevector? x) 
    (not (bytevector? x)))

  (define-tests test-bytevectors
    [bytevector? (make-bytevector 1)]
    [bytevector? (make-bytevector 1 17)]
    [bytevector? (make-bytevector 10 -17)]
    [not-bytevector? 'foo] 
    [not-bytevector? "hey"] 
    [not-bytevector? (current-output-port)] 
    [not-bytevector? (current-input-port)] 
    [not-bytevector? '#(2837 2398 239)] 
    [zero? (bytevector-length (make-bytevector 0))]
    [(lambda (x) (= x 100)) (bytevector-length (make-bytevector 100 -30))]
    [(lambda (x) (equal? x '(-127 129 -1 255)))
     (let ([b1 (make-bytevector 16 -127)]
           [b2 (make-bytevector 16 255)])
       (list 
         (bytevector-s8-ref b1 0)
         (bytevector-u8-ref b1 0)
         (bytevector-s8-ref b2 0)
         (bytevector-u8-ref b2 0)))]

    ))





