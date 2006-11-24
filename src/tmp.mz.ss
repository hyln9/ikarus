
(define f 
  (lambda (i) 
    ;(write i)
    ;(display " ")
    ;(flush-output-port)
    (call-with-current-continuation
      (lambda (k)
        (if (> i 0) 
          (f (- i 1))
          13) 
        12))))
(write (f 100000))
(newline)
