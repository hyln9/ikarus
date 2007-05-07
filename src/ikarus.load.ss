
(library (ikarus load)
  (export load)
  (import 
    (except (ikarus) load)
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
         (close-input-port p))])))
