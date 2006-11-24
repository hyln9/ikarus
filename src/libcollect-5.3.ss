
;($pcb-set! do-overflow
;  (lambda ()
;    ($do-overflow 4096)))

($pcb-set! do-overflow
  (lambda ()
    (foreign-call "S_collect" 4096)
    (void)))

($pcb-set! collect
  (lambda ()
    (do-overflow)))

($pcb-set! do-overflow-with-byte-count
  (lambda (n)
    (foreign-call "S_collect" n)
    (void)))

($pcb-set! do-stack-overflow
  (lambda ()
    (foreign-call "S_stack_overflow")))

