
(library (tests pointers)
  (export test-pointers)
  (import (ikarus))

  (define bits 
    (if (<= (fixnum-width) 32) 32 64))
  
  (define mask (sub1 (sll 1 bits)))

  (define (test-pointer n)
    (let* ([np (integer->pointer n)]
           [m (pointer->integer np)]
           [mp (integer->pointer m)])
      (printf "test ~x/~s  => ~x/~s\n" n np m mp)
      (unless (= (bitwise-and n mask) (bitwise-and m mask))
        (error 'test "failed/got" n m 
               (bitwise-and n mask) (bitwise-and m mask)))))
      
  (define (test-pointers)
    (test-pointer 0)
    (test-pointer 100)
    (test-pointer -100)
    (test-pointer (greatest-fixnum))
    (test-pointer (least-fixnum))
    (test-pointer (+ 1 (greatest-fixnum)))
    (test-pointer (+ 1 (least-fixnum)))
    (test-pointer (- 1 (greatest-fixnum)))
    (test-pointer (- 1 (least-fixnum)))
    (test-pointer (+ -1 (greatest-fixnum)))
    (test-pointer (+ -1 (least-fixnum)))
    (test-pointer (- -1 (greatest-fixnum)))
    (test-pointer (- -1 (least-fixnum)))
    (test-pointer (* 2 (greatest-fixnum)))
    (test-pointer (* 2 (least-fixnum)))
    (test-pointer (* 4 (greatest-fixnum)))
    (test-pointer (* 4 (least-fixnum)))
    (test-pointer (* 8 (greatest-fixnum)))
    (test-pointer (* 8 (least-fixnum)))
    (test-pointer (* 16 (greatest-fixnum)))
    (test-pointer (* 16 (least-fixnum)))))

