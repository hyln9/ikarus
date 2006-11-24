
(primitive-set! 'error
  (lambda args
    (foreign-call "ik_error" args)))

(primitive-set! '$apply-nonprocedure-error-handler
  (lambda (x)
    (error 'apply "~s is not a procedure" x)))

(primitive-set! '$incorrect-args-error-handler
  (lambda (p n)
    (error 'apply "incorrect number of argument (~s) to ~s" n p)))

(primitive-set! '$multiple-values-error
  (lambda args
    (error 'apply 
           "incorrect number of values ~s returned to single value context" 
           args)))

(primitive-set! '$debug
  (lambda (x)
    (foreign-call "ik_error" (cons "DEBUG" x))))

(primitive-set! '$underflow-misaligned-error
  (lambda ()
    (foreign-call "ik_error" "misaligned")))

(primitive-set! 'top-level-value-error
  (lambda (x)
    (cond
      [(symbol? x)
       (if (top-level-bound? x)
           (error 'top-level-value "BUG in ~s" x)
           (error 'top-level-value "~s is unbound" x))]
      [else
       (error 'top-level-value "~s is not a symbol" x)])))

(primitive-set! 'car-error
  (lambda (x)
    (error 'car "~s is not a pair" x)))

(primitive-set! 'cdr-error
  (lambda (x)
    (error 'cdr "~s is not a pair" x)))

