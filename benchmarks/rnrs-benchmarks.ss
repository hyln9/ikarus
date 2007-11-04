
(library (rnrs-benchmarks)
  (export run-benchmark fatal-error include-source 
    call-with-output-file/truncate
     ack-iters 
     array1-iters
     boyer-iters
     browse-iters
     cat-iters
     conform-iters
     cpstak-iters
     ctak-iters
     dderiv-iters
     deriv-iters
     destruc-iters
     diviter-iters
     divrec-iters
     dynamic-iters
     earley-iters
     fft-iters
     fib-iters
     fibc-iters
     fibfp-iters
     fpsum-iters
     gcbench-iters
     gcold-iters
     graphs-iters
     lattice-iters
     matrix-iters
     maze-iters
     mazefun-iters
     mbrot-iters
     nbody-iters
     nboyer-iters
     nqueens-iters
     nucleic-iters
     takl-iters
     paraffins-iters
     parsing-iters
     perm9-iters
     pnpoly-iters
     peval-iters
     pi-iters
     primes-iters
     puzzle-iters
     quicksort-iters
     ray-iters
     sboyer-iters
     scheme-iters
     simplex-iters
     slatex-iters
     sum-iters
     sum1-iters
     string-iters
     sumfp-iters
     sumloop-iters
     tail-iters
     tak-iters
     trav1-iters
     trav2-iters
     triangl-iters
     wc-iters)

  (import (ikarus))

  (define call-with-output-file/truncate
    (lambda (file-name proc)
      (call-with-output-file file-name proc 'truncate)))

  (define-syntax include-source
    (lambda (x)
      (syntax-case x ()
        [(ctxt name) 
         (cons #'begin
           (with-input-from-file 
             (format "r6rs-benchmarks/~a" (syntax->datum #'name))
             (lambda ()
               (let f ()
                 (let ([x (read)])
                   (cond
                     [(eof-object? x) '()]
                     [else 
                      (cons (datum->syntax #'ctxt x) (f))]))))))])))

  (define (fatal-error . args)
    (error 'fatal-error "~a"
      (apply (lambda (x) (format "~a" x)) args)))
  
  (define (run-bench count run)
    (unless (= count 0)
      (let f ([count (- count 1)])
        (cond
          [(= count 0) (run)]
          [else 
           (begin (run) (f (- count 1)))]))))

  (define (run-benchmark name count ok? run-maker . args)
    (let ([run (apply run-maker args)])
      (let ([result 
             (time-it name
              (lambda () (run-bench count run)))])
        (unless (ok? result) 
          (error #f "*** wrong result ***")))))


  ; Gabriel benchmarks
  (define boyer-iters        20)
  (define browse-iters      600)
  (define cpstak-iters     1000)
  (define ctak-iters        100)
  (define dderiv-iters  2000000)
  (define deriv-iters   2000000)
  (define destruc-iters     500)
  (define diviter-iters 1000000)
  (define divrec-iters  1000000)
  (define puzzle-iters      100)
  (define tak-iters        2000)
  (define takl-iters        300)
  (define trav1-iters       100)
  (define trav2-iters        20)
  (define triangl-iters      10)
  ; Kernighan and Van Wyk benchmarks
  (define ack-iters           1)
  (define array1-iters        1)
  (define cat-iters           1)
  (define string-iters        1)
  (define sum1-iters          1)
  (define sumloop-iters       1)
  (define tail-iters          1)
  (define wc-iters            1)
  
  ; C benchmarks
  (define fft-iters        2000)
  (define fib-iters           5)
  (define fibfp-iters         2)
  (define mbrot-iters       100)
  (define nucleic-iters       5)
  (define pnpoly-iters   100000)
  (define sum-iters       10000)
  (define sumfp-iters      5000)
  (define tfib-iters         20)
  
  ; Other benchmarks
  (define conform-iters      40)
  (define dynamic-iters      20)
  (define earley-iters      200)
  (define fibc-iters        500)
  (define graphs-iters      300)
  (define lattice-iters       1)
  (define matrix-iters      400)
  (define maze-iters       4000)
  (define mazefun-iters    1000)
  (define nqueens-iters    2000)
  (define paraffins-iters  1000)
  (define peval-iters       200)
  (define pi-iters            2)
  (define primes-iters   100000)
  (define ray-iters           5)
  (define scheme-iters    20000)
  (define simplex-iters  100000)
  (define slatex-iters       20)
  (define perm9-iters        10)
  (define nboyer-iters      100)
  (define sboyer-iters      100)
  (define gcbench-iters       1)
  (define compiler-iters    300)

  ; New benchmarks
  (define parsing-iters    1000)
  (define gcold-iters     10000)

  (define quicksort-iters 1)
  (define fpsum-iters 10)
  (define nbody-iters         1) ; nondeterministic (order of evaluation)
    )
  
