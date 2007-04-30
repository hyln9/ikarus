
(library (ikarus collect)
  (export)
  (import (scheme))

(define do-overflow
  (lambda (n)
    (foreign-call "ik_collect" n)
    (void)))

(primitive-set! 'do-overflow do-overflow)

(primitive-set! 'do-overflow-words
  (lambda (n)
    (foreign-call "ik_collect" ($fxsll n 2))
    (void)))

(primitive-set! 'do-vararg-overflow
  (lambda (n)
    (foreign-call "ik_collect_vararg" n)
    (void)))

(primitive-set! 'collect
  (lambda ()
    (do-overflow 4096)))

(primitive-set! 'do-stack-overflow
  (lambda ()
    (foreign-call "ik_stack_overflow")))

(primitive-set! 'dump-metatable
  (lambda ()
    (foreign-call "ik_dump_metatable")))

(primitive-set! 'dump-dirty-vector
  (lambda ()
    (foreign-call "ik_dump_dirty_vector")))
)
