
(library (ikarus exceptions)
  (export with-exception-handler raise raise-continuable)
  (import 
    (only (rnrs) condition make-non-continuable-violation
          make-message-condition)
    (except (ikarus)
      with-exception-handler raise raise-continuable))

  (define (print-condition x)
    (printf "CONDITION: ~s\n" x))

  (define handlers
    (make-parameter
      (list 
        (lambda (x) 
          (printf "unhandled exception:\n")
          (print-condition x)
          (exit -1)))))
  
  (define (with-exception-handler handler proc2)
    (unless (procedure? handler)
      (error 'with-exception-handler
        "handler ~s is not a procedure" handler))
    (unless (procedure? proc2)
      (error 'with-exception-handler
        "~s is not a procedure" proc2))
    (parameterize ([handlers (cons handler (handlers))])
      (proc2)))
  
  (define (raise-continuable x)
    (let ([h* (handlers)])
      (let ([h (car h*)] [h* (cdr h*)])
        (parameterize ([handlers h*])
          (h x)))))
  
  (define (raise x)
    (let ([h* (handlers)])
      (let ([h (car h*)] [h* (cdr h*)])
        (parameterize ([handlers h*])
          (h x)
          (raise
            (condition
              (make-non-continuable-violation)
              (make-message-condition "handler returned")))))))
)

