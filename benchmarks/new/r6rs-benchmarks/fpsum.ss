;;; FPSUM - Compute sum of integers from 0 to 1e6 using floating point

(library (r6rs-benchmarks fpsum)
  (export main)
  (import (r6rs) (r6rs arithmetic flonums) (r6rs-benchmarks))

  (define (run)
    (let loop ((i 1e6) (n 0.))
      (if (fl<? i 0.)
        n
        (loop (fl- i 1.) (fl+ i n)))))
   
  (define (main . args)
    (run-benchmark
      "fpsum"
      fpsum-iters
      (lambda (result) (equal? result 500000500000.)) 
      (lambda () run))))
