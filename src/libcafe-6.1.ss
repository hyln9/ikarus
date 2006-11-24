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
              (reset-input-port! (console-input-port))
              ;(display "repl catch\n" (console-output-port))
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
                   (call-with-values
                     (lambda () (eval x))
                     (lambda v*
                       (unless (andmap (lambda (v) (eq? v (void))) v*)
                         (for-each
                           (lambda (v)
                             (write v (console-output-port))
                             (newline (console-output-port)))
                           v*))))]))))))
      (wait eval escape-k)))

  (define new-cafe
    (lambda (eval)
      (dynamic-wind
        (lambda () (set! eval-depth (fxadd1 eval-depth)))
        (lambda ()
          (call/cc 
            (lambda (k)
              (wait eval k))))
        (lambda () (set! eval-depth (fxsub1 eval-depth))))))

  (primitive-set! 'new-cafe
    (case-lambda
      [() (new-cafe eval)]
      [(p)
       (unless (procedure? p) 
         (error 'new-cafe "~s is not a procedure" p))
       (new-cafe p)]))
  )

