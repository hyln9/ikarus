
(library (ikarus system parameters)
  (export make-parameter)
  (import (except (ikarus) make-parameter))
  (define make-parameter
    (case-lambda
      [(x) 
       (case-lambda
         [() x]
         [(v) (set! x v)])]
      [(x guard)
       (unless (procedure? guard)
         (error 'make-parameter "~s is not a procedure" guard))
       (set! x (guard x))
       (case-lambda
         [() x]
         [(v) (set! x (guard v))])])))


(library (ikarus system handlers)
  (export
    interrupt-handler $apply-nonprocedure-error-handler
    $incorrect-args-error-handler $multiple-values-error $debug
    $underflow-misaligned-error top-level-value-error car-error
    cdr-error fxadd1-error fxsub1-error cadr-error fx+-type-error
    fx+-types-error fx+-overflow-error $do-event)
  (import (except (ikarus) interrupt-handler)
          (only (scheme)
                $interrupted?
                $unset-interrupted!
                top-level-bound?))

  (define interrupt-handler
    (make-parameter
      (lambda ()
        (flush-output-port (console-output-port))
        (error #f "interrupted"))
      (lambda (x)
        (if (procedure? x)
            x
            (error 'interrupt-handler "~s is not a procedure" x)))))

  (define $apply-nonprocedure-error-handler
    (lambda (x)
      (error 'apply "~s is not a procedure" x)))
  
  (define $incorrect-args-error-handler
    (lambda (p n)
      (error 'apply "incorrect number of argument (~s) to ~s" n p)))
  
  (define $multiple-values-error
    (lambda args
      (error 'apply 
             "incorrect number of values ~s returned to single value context" 
             args)))
  
  (define $debug
    (lambda (x)
      (foreign-call "ik_error" (cons "DEBUG" x))))
  
  (define $underflow-misaligned-error
    (lambda ()
      (foreign-call "ik_error" "misaligned")))
  
  (define top-level-value-error
    (lambda (x)
      (cond
        [(symbol? x)
         (if (top-level-bound? x)
             (error 'top-level-value "BUG in ~s" x)
             (error 'top-level-value "~s is unbound" x))]
        [else
         (error 'top-level-value "~s is not a symbol" x)])))
  
  (define car-error
    (lambda (x)
      (error 'car "~s is not a pair" x)))
  
  (define cdr-error
    (lambda (x)
      (error 'cdr "~s is not a pair" x)))
  
  (define fxadd1-error
    (lambda (x)
      (if (fixnum? x)
          (error 'fxadd1 "overflow")
          (error 'fxadd1 "~s is not a fixnum" x))))
  
  (define fxsub1-error
    (lambda (x)
      (if (fixnum? x)
          (error 'fxsub1 "underflow")
          (error 'fxsub1 "~s is not a fixnum" x))))
  
  (define cadr-error
    (lambda (x)
      (error 'cadr "invalid list structure in ~s" x)))
  
  (define fx+-type-error
    (lambda (x)
      (error 'fx+ "~s is not a fixnum" x)))
  
  (define fx+-types-error
    (lambda (x y)
      (error 'fx+ "~s is not a fixnum"
             (if (fixnum? x) y x))))
  
  (define fx+-overflow-error
    (lambda (x y)
      (error 'fx+ "overflow")))
  
  (define $do-event
    (lambda ()
      (if ($interrupted?)
          (begin
            ($unset-interrupted!)
            ((interrupt-handler)))
          (display "Engine Expired\n" (console-output-port)))))
  )
