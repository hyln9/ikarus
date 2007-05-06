

(library (ikarus core)
  (export)
  (import (scheme))

(primitive-set! 'primitive-set!
  (lambda (x v)
    (unless (symbol? x)
      (error 'primitive-set! "~s is not a symbol" x))
    (primitive-set! x v)
    (set-top-level-value! x v)))


(primitive-set! 'pointer-value
  (lambda (x)
    (pointer-value x)))



)
