

(primitive-set! '$reset-symbol-function!
  (lambda (x)
    (let ([v ($symbol-value x)])
      (if (procedure? v)
          ($set-symbol-function! x v)
          ($set-symbol-function! x 
            (lambda args
              (error 'apply "~s is not a procedure" v)))))))

(primitive-set! 'make-parameter
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
       [(v) (set! x (guard v))])]))


(primitive-set! 'error
  (lambda args
    (foreign-call "ik_error" args)))

(primitive-set! 'interrupt-handler
  (make-parameter
    (lambda ()
      (flush-output-port (console-output-port))
      (error #f "interrupted"))
    (lambda (x)
      (if (procedure? x)
          x
          (error 'interrupt-handler "~s is not a procedure" x)))))

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

(primitive-set! 'fxadd1-error
  (lambda (x)
    (if (fixnum? x)
        (error 'fxadd1 "overflow")
        (error 'fxadd1 "~s is not a fixnum" x))))

(primitive-set! 'fxsub1-error
  (lambda (x)
    (if (fixnum? x)
        (error 'fxsub1 "underflow")
        (error 'fxsub1 "~s is not a fixnum" x))))

(primitive-set! 'cadr-error
  (lambda (x)
    (error 'cadr "invalid list structure in ~s" x)))

(primitive-set! 'fx+-type-error
  (lambda (x)
    (error 'fx+ "~s is not a fixnum" x)))

(primitive-set! 'fx+-types-error
  (lambda (x y)
    (error 'fx+ "~s is not a fixnum"
           (if (fixnum? x) y x))))

(primitive-set! 'fx+-overflow-error
  (lambda (x y)
    (error 'fx+ "overflow")))

(primitive-set! '$do-event
  (lambda ()
    (if ($interrupted?)
        (begin
          ($unset-interrupted!)
          ((interrupt-handler)))
        (display "Engine Expired\n" (console-output-port)))))


