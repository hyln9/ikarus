

(let ()
  (define eval-depth 0)
  (define display-prompt
    (lambda (i)
      (if (fx= i eval-depth)
          (display " " (console-output-port))
          (begin
            (display ">" (console-output-port))
            (display-prompt (fx+ i 1))))))
  (define wait
    (lambda (eval)
      (display-prompt 0)
      (let ([x (read (console-input-port))])
        (cond
          [(eof-object? x) 
           (newline (console-output-port))]
          [else
           (let ([v (eval x)])
             (unless (eq? v (void))
               (write v (console-output-port))
               (newline (console-output-port))))
           (wait eval)]))))
  ($pcb-set! new-cafe
    (lambda args
      (let ([eval
             (if (null? args)
                 (current-eval)
                 (if (null? (cdr args))
                     (let ([f (car args)])
                       (if (procedure? f)
                           f
                           (error 'new-cafe "not a procedure ~s" f)))
                     (error 'new-cafe "too many arguments")))])
        (set! eval-depth (fxadd1 eval-depth))
        (wait eval)
        (set! eval-depth (fxsub1 eval-depth))))))

