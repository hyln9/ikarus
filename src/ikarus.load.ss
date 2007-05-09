
(library (ikarus load)
  (export load load-r6rs-top-level)
  (import 
    (except (ikarus) load)
    (only (ikarus syntax) eval-top-level eval-r6rs-top-level)
    (only (ikarus reader) read-initial))

  (define load-handler
    (lambda (x)
      (eval-top-level x)))
  (define read-and-eval
    (lambda (p eval-proc)
      (let ([x (read p)])
        (unless (eof-object? x)
          (eval-proc x)
          (read-and-eval p eval-proc)))))
  (define load
    (case-lambda
      [(x) (load x load-handler)]
      [(x eval-proc)
       (unless (string? x)
         (error 'load "~s is not a string" x))
       (unless (procedure? eval-proc)
         (error 'load "~s is not a procedure" eval-proc))
       (let ([p (open-input-file x)])
         (let ([x (read-initial p)])
           (unless (eof-object? x)
             (eval-proc x)
             (read-and-eval p eval-proc)))
         (close-input-port p))]))
  (define load-r6rs-top-level
    (lambda (x)
      (define (read-file)
        (let ([p (open-input-file x)])
          (let ([x (read-initial p)])
            (if (eof-object? x)
                (begin (close-input-port p) '())
                (cons x 
                  (let f ()
                    (let ([x (read p)])
                      (cond
                        [(eof-object? x) 
                         (close-input-port p)
                         '()]
                        [else (cons x (f))]))))))))
      (let ([prog (read-file)])
        (eval-r6rs-top-level prog))))
  )
