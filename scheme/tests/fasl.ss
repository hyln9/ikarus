
(library (tests fasl)
  (export test-fasl)
  (import (ikarus) (tests framework))

  (define (test x)
    (printf "test-fasl ~s\n" x)
    (let-values ([(p e) (open-bytevector-output-port)])
      (fasl-write x p)
      (let ([bv (e)])
        (let ([y (fasl-read (open-bytevector-input-port bv))])
          (unless (equal? x y)
            (error 'test-fasl "failed/expected" y x))))))

  (define (test-fasl) 
    (test 12)
    (test -12)
    (test 0)
    (test #t)
    (test #f)
    (test '())
    (test 'hello)
    (test "Hello")
    (test '(Hello There))
    (test 3498798327498723894789237489324)
    (test -3498798327498723894789237489324)
    (test 2389478923749872389723894/23498739874892379482374)
    (test -2389478923749872389723894/23498739874892379482374)
    (test 127487384734.4)
    (test (make-rectangular 12 13)))

)


