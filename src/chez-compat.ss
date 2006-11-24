(define-syntax $pcb-set!
  (syntax-rules ()
    [(_ name val) 
     (set-top-level-value! 'name val)]))

(define primitive-set! set-top-level-value!)

(define (immediate? x)
  (or (fixnum? x)
      (null? x)
      (char? x)
      (boolean? x)
      (eof-object? x)
      (eq? x (void))))

(define fxadd1
  (lambda (x)
    (import scheme)
    (unless (fixnum? x) (error 'fxadd1 "~s is not a fixnum" x))
    (let ([v (+ x 1)])
      (unless (fixnum? v) (error 'fxadd1 "overflow"))
      v)))

(define fxsub1
  (lambda (x)
    (import scheme)
    (unless (fixnum? x) (error 'fxsub1 "~s is not a fixnum" x))
    (let ([v (- x 1)])
      (unless (fixnum? v) (error 'fxsub1 "overflow"))
      v)))

(define char= char=?)



