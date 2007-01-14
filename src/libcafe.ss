#|procedure:new-cafe
synopsis:
  (new-cafe [eval])
description:
  The procedure new-cafe starts a new read-eval-print loop inside
  the current cafe (if one exists).  It prompts the user for an
  expression, evaluates it, prints the result back, and repeats the
  process.  If new-cafe is called with an argument, eval, then that
  argument must be a procedure that takes a single argument.  The 
  eval procedure will be used to evaluate the expressions.
  
  Every time a new cafe is started, the prompt is changed to reflect
  the depth of the current cafe (i.e. how many eof objects is takes
  to exit the outermost cafe).  
  
  Input and output performed by the cafe can be changed by the
  console-input-port and console-output-port parameters.
  
  If an error occurs during reading, evaluating, or printing an
  expression, then the error is printed to the error-port and the
  operations of the cafe resume as normal.|#
#|FIXME:new-cafe
  Be specific about what the error-port is |#
(let ()
  (define with-error-handler
    (lambda (p thunk)
      (let ([old-error-handler (error-handler)])
        (dynamic-wind
          (lambda () 
            (error-handler
              (lambda args
                (error-handler old-error-handler)
                (apply p args)
                (apply error args))))
          thunk
          (lambda ()
            (error-handler old-error-handler))))))

  (define eval-depth 0)

  (define display-prompt
    (lambda (i)
      (if (fx= i eval-depth)
          (display " " (console-output-port))
          (begin
            (display ">" (console-output-port))
            (display-prompt (fx+ i 1))))))

  (define my-read
    (lambda (k)
      (parameterize ([interrupt-handler
                      (lambda ()
                        (flush-output-port (console-output-port))
                        (reset-input-port! (console-input-port))
                        (newline (console-output-port))
                        (k))])
         (read (console-input-port)))))

  (define wait
    (lambda (eval escape-k)
      (call/cc
        (lambda (k)
          (with-error-handler
            (lambda args
              (reset-input-port! (console-input-port))
              (apply print-error args)
              (k (void)))
            (lambda ()
              (display-prompt 0)
              (let ([x (my-read k)])
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
                             (pretty-print v (console-output-port)))
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

