
(library (tests pointers)
  (export test-pointers)
  (import (ikarus) (ikarus system $foreign))

  (define bits 
    (if (<= (fixnum-width) 32) 32 64))
     
  (define (test-pointer-values)
    (define mask (sub1 (sll 1 bits)))
    (define (test-pointer n)
      (let* ([np (integer->pointer n)]
             [m (pointer->integer np)]
             [mp (integer->pointer m)])
        (unless (= (bitwise-and n mask) (bitwise-and m mask))
          (error 'test "failed/got" n m 
                 (bitwise-and n mask) (bitwise-and m mask)))))
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
    (test-pointer (* 16 (least-fixnum))))
  
  (define (combinations n)
    (define (one-bit-combinations n)
      (let ([n (- n 1)])
        (if (< n 0) 
            '()
            (cons (sll 1 n) (one-bit-combinations n)))))
    (define (or* ls1 ls2)
      (apply append
        (map 
          (lambda (n1)
            (map 
              (lambda (n2)
                (bitwise-ior n1 n2))
              ls2))
          ls1)))
    (let ([n (min bits n)])
      (let* ([ls1 (one-bit-combinations n)]
             [ls2 (or* ls1 ls1)]
             [ls3 (or* ls2 ls1)])
        (append 
          (list 0 (sub1 (sll 1 (- n 1))) (sub1 (sll 1 n))) 
          ls1 ls2 ls3))))
          



  (define (u* n)
    (let ([n (min n bits)])
      (combinations n)))

  (define (s* n)
    (let ([n (min n bits)])
      (let ([mx (- (expt 2 (- n 1)) 1)])
        (map 
          (lambda (x)
            (if (> x mx)
                (- x (expt 2 n))
                x))
          (combinations n)))))


  (define (test-ref/set type combinations getter setter)
    (printf "testing memory access (~s combination for type ~s)\n" 
            (length combinations)
            type)
    (for-each
      (lambda (n)
        (let ([m
               (let ([p (malloc 8)])
                 (setter p 0 n)
                 (let ([m (getter p 0)])
                   (free p)
                   m))])
          (unless (= n m) 
            (error 'test "failed" getter setter n m))))
      combinations))

  (define (check-combinations n)
    (define (same-pattern? u s i)
      (cond
        [(= i 1) 
         (cond
           [(= u 0) (= s 0)]
           [(= u 1) (= s -1)]
           [else #f])]
        [else
         (and (= (bitwise-and u 1) (bitwise-and s 1))
              (same-pattern? (sra u 1) (sra s 1) (- i 1)))]))
    (define (check u s)
      (unless (same-pattern? u s (min n bits)) 
        (error 'check "failed" u s)))
    (for-each check (u* n) (s* n)))
  

  (define (test-pointers)
    (for-each check-combinations '(8 16 32 64))

    (test-pointer-values)
    (test-ref/set 'char   (s*  8) pointer-ref-char   pointer-set-char)
    (test-ref/set 'short  (s* 16) pointer-ref-short  pointer-set-short)
    (test-ref/set 'int    (s* 32) pointer-ref-int    pointer-set-int)
    (test-ref/set 'long   (s* 64) pointer-ref-long   pointer-set-long)
    (test-ref/set 'uchar  (u*  8) pointer-ref-uchar  pointer-set-char)
    (test-ref/set 'ushort (u* 16) pointer-ref-ushort pointer-set-short)
    (test-ref/set 'uint   (u* 32) pointer-ref-uint   pointer-set-int)
    (test-ref/set 'ulong  (u* 64) pointer-ref-ulong  pointer-set-long)
    )



  )



