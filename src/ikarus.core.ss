

(library (ikarus core)
  (export)
  (import (scheme))

(primitive-set! 'primitive-set!
  (lambda (x v)
    (unless (symbol? x)
      (error 'primitive-set! "~s is not a symbol" x))
    (primitive-set! x v)
    (set-top-level-value! x v)))

(primitive-set! 'eof-object
  (lambda () (eof-object)))

(primitive-set! 'pointer-value
  (lambda (x)
    (pointer-value x)))

(primitive-set! 'date-string
  (lambda ()
    (let ([s (make-string 10)])
      (foreign-call "ikrt_strftime" s "%F")
      s)))


)
