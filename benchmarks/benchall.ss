#!/usr/bin/env ikarus --r6rs-script

(import (ikarus))

(define all-benchmarks
  '(ack array1 boyer browse cat compiler conform cpstak ctak dderiv
    deriv destruc diviter divrec dynamic earley fft fib fibc fibfp
    fpsum gcbench gcold graphs lattice matrix maze mazefun mbrot
    nbody nboyer nqueens ntakl nucleic paraffins parsing perm9 peval
    pi pnpoly primes puzzle quicksort ray sboyer scheme simplex
    slatex string sum sum1 sumfp sumloop tail tak takl trav1 trav2
    triangl wc))


(for-each 
  (lambda (x) 
    (unless (zero? (system (format "ikarus --r6rs-script bench.ss ~a" x)))
      (fprintf (standard-error-port) "ERROR: ~s failed\n" x)))
  all-benchmarks)

