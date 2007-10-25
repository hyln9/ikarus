
(library (ikarus promises)
  (export force make-promise)
  (import 
    (except (ikarus) force make-promise))

  (define (force x)
    (unless (procedure? x) 
      (error 'force "not a procedure" x))
    (x))

  (define (make-promise proc)
    (unless (procedure? proc)
      (error 'make-promise "not a procedure" proc))
    (let ([results #f])
      (lambda () 
        (if results
            (apply values results)
            (call-with-values proc
              (lambda x*
                (if results
                    (apply values results) 
                    (begin
                      (set! results x*) 
                      (apply values x*))))))))))

