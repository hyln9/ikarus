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

(set! $base-rtd #%$base-rtd)
(define-syntax |#primitive|
  (syntax-rules ()
    [(_ n prim) prim]
    [(_ prim) prim]))

(define (date-string)
  (system "date +\"%F\" > build-date.tmp")
  (let ([ip (open-input-file "build-date.tmp")])
    (list->string
      (let f ()
        (let ([x (read-char ip)])
          (if (char=? x #\newline)
              '()
              (cons x (f))))))))

(define ($record rtd . args)
  (apply (record-constructor rtd) args))
(define ($record/rtd? x rtd)
  (and (record? x) (eq? (record-type-descriptor x) rtd)))
(define ($record-ref x i)
  ((record-field-accessor (record-type-descriptor x) i) x))
(define ($record-set! x i v)
  ((record-field-mutator (record-type-descriptor x) i) x v))
