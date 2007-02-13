
(let ()
  (define call-with-current-frame
    (lambda (f)
      (if ($fp-at-base)
          (f ($current-frame))
          ($seal-frame-and-call f))))
  
  (define primitive-call/cc
    (lambda (f)
      (call-with-current-frame
        (lambda (frm)
          (f ($frame->continuation frm))))))

  (primitive-set! 'call/cf call-with-current-frame)
  (primitive-set! '$primitive-call/cc primitive-call/cc))


