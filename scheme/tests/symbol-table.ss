
(library (tests symbol-table)
  (export run-tests)
  (import
    (ikarus) 
    (only (ikarus system $symbols) $symbol-table-size))

  (define (test-gcable-symbols n)
    (let ([st1 ($symbol-table-size)])
      (do ((i 0 (+ i 1))) 
          ((= i n)) 
        (string->symbol (number->string i)))
      (collect)
      (let ([st2 ($symbol-table-size)])
        (assert (< (- st2 st1) n)))))

  (define (run-tests)
    (test-gcable-symbols 1000000)))

