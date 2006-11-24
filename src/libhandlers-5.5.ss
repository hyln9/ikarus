

($pcb-set! $apply-nonprocedure-error-handler
  (lambda (x)
    (error 'apply "~s is not a procedure" x)))

($pcb-set! $incorrect-args-error-handler
  (lambda (p n)
    (error 'apply "incorrect number of argument (~s) to ~s" n p)))

($pcb-set! $multiple-values-error
  (lambda args
    (error 'apply 
           "incorrect number of values ~s returned to single value context" 
           args)))

