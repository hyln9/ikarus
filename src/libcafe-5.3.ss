(let ()
  (define with-error-handler
    (lambda (p thunk)
      (let ([old-error-handler (current-error-handler)])
        (dynamic-wind
          (lambda () 
            (current-error-handler
              (lambda args
                (current-error-handler old-error-handler)
                (apply p args)
                (apply error args))))
          thunk
          (lambda ()
            (current-error-handler old-error-handler))))))

  (define eval-depth 0)

  (define display-prompt
    (lambda (i)
      (if (fx= i eval-depth)
          (display " " (console-output-port))
          (begin
            (display ">" (console-output-port))
            (display-prompt (fx+ i 1))))))

  (define wait
    (lambda (eval escape-k)
      (call/cc
        (lambda (k)
          (with-error-handler
            (lambda args
              (apply print-error args)
              (k (void)))
            (lambda ()
              (display-prompt 0)
              (let ([x (read (console-input-port))])
                (cond
                  [(eof-object? x) 
                   (newline (console-output-port))
                   (escape-k (void))]
                  [else
                   (let ([v (eval x)])
                     (unless (eq? v (void))
                       (write v (console-output-port))
                       (newline (console-output-port))))]))))))
      (wait eval escape-k)))

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
        (dynamic-wind
          (lambda () (set! eval-depth (fxadd1 eval-depth)))
          (lambda ()
            (call/cc 
              (lambda (k)
                (wait eval k))))
          (lambda () (set! eval-depth (fxsub1 eval-depth))))))))

