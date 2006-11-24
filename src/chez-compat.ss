(define-syntax $pcb-set!
  (syntax-rules ()
    [(_ name val) 
     (set-top-level-value! 'name val)]))

(define (immediate? x)
  (or (fixnum? x)
      (char? x)
      (boolean? x)
      (eof-object? x)
      (eq? x (void))))

(define-syntax add1 syntax-error)
(define fxadd1
  (lambda (x)
    (unless (fixnum? x) (error 'fxadd1 "~s is not a fixnum" x))
    (let ([v (+ x 1)])
      (unless (fixnum? v) (error 'fxadd1 "overflow"))
      v)))

(define-syntax sub1 syntax-error)
(define fxsub1
  (lambda (x)
    (unless (fixnum? x) (error 'fxsub1 "~s is not a fixnum" x))
    (let ([v (- x 1)])
      (unless (fixnum? v) (error 'fxsub1 "overflow"))
      v)))




(define-syntax - syntax-error)
(define-syntax fx-
  (let () 
    (import scheme)
    (syntax-rules ()
     [(_ x y) (#%fx- x y)])))

(define-syntax * syntax-error)
(define-syntax fx*
  (let ()
    (import scheme)
    (syntax-rules ()
     [(_ x y) (#%fx* x y)])))

(define-syntax + syntax-error)
(define-syntax fx+
  (let ()
    (import scheme)
    (syntax-rules ()
     [(_ x y) (#%fx+ x y)])))

(define-syntax = syntax-error)
(define-syntax fx=
  (let ()
    (import scheme)
    (syntax-rules ()
     [(_ x y) (#%fx= x y)])))




(define-syntax integer? syntax-error)
(define char= char=?)



