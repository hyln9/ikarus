#!/usr/bin/env ikarus --r6rs-script

(import (ikarus))

(define (run name)
  (let ([proc (time-it (format "compile-~a" name) 
                (lambda ()
                  (eval 'main 
                    (environment
                      (list 'r6rs-benchmarks name)))))])
    (proc)))

(apply 
  (case-lambda
    [(script-name bench-name) 
     (run (string->symbol bench-name))]
    [(script-name . args)
     (error script-name 
        (if (null? args)
            "missing benchmark name"
            "too many arguments"))])
  (command-line-arguments))
