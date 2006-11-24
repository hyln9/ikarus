
(define collect void)

(define test
  (lambda (name x1 x2)
    (unless (equal? x1 x2)
      (error 'test "test ~s failed: expected ~s, got ~s" name x1 x2))
    (printf "~a ok\n" name)))

(test 1
      '(12 17 yes)
      (let ([h (make-hash-table)])
        (put-hash-table! h 'foo 12)
        (put-hash-table! h 'bar 17)
        (collect)
        (list
          (get-hash-table h 'foo 'no)
          (get-hash-table h 'bar 'no)
          (get-hash-table h 'baz 'yes))))


(define iota
  (lambda (i n)
    (cond
      [(fx= i n) '()]
      [else (cons i (iota (fx+ 1 i) n))])))

(test 2
      (iota 1 10001)
      (let ([h (make-hash-table)])
        (for-each
          (lambda (i) (put-hash-table! h i (fx+ 1 i)))
          (iota 0 10000))
        (collect)
        (map
          (lambda (i) (get-hash-table h i #f))
          (iota 0 10000))))

(test 3
      (iota 2 10002)
      (let ([h (make-hash-table)])
        (for-each
          (lambda (i) (put-hash-table! h i (fx+ 1 i)))
          (iota 0 10000))
        (for-each
          (lambda (i) (put-hash-table! h i 
                        (fx+ 1 
                          (get-hash-table h i -1000))))
          (iota 0 10000))
        (collect)
        (map
          (lambda (i) (get-hash-table h i #f))
          (iota 0 10000))))



