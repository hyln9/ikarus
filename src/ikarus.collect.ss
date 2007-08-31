
(library (ikarus collect)
  (export do-overflow do-overflow-words do-vararg-overflow collect
          do-stack-overflow collect-key)
  (import 
    (except (ikarus) collect collect-key)
    (ikarus system $fx)
    (ikarus system $arg-list))

(define do-overflow
  (lambda (n)
    (foreign-call "ik_collect" n)
    (void)))

(define do-overflow-words
  (lambda (n)
    (foreign-call "ik_collect" ($fxsll n 2))
    (void)))

(define do-vararg-overflow
  (lambda (n)
    (foreign-call "ik_collect_vararg" n)
    (void)))

(define collect
  (lambda ()
    (do-overflow 4096)))

(define do-stack-overflow
  (lambda ()
    (foreign-call "ik_stack_overflow")))

(define dump-metatable
  (lambda ()
    (foreign-call "ik_dump_metatable")))

(define dump-dirty-vector
  (lambda ()
    (foreign-call "ik_dump_dirty_vector")))

(define (collect-key)
  (or ($collect-key) 
      (begin
        ($collect-key (string #\k))
        (collect-key))))

)
