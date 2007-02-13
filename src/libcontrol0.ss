
(let ()
  (define call-with-current-frame
    (lambda (f)
      (if ($fp-at-base)
          (f ($current-frame))
          ($seal-frame-and-call f))))
  (primitive-set! 'call/cf call-with-current-frame))
 
(let () 
  (define primitive-call/cc
    (lambda (f)
      (call/cf
        (lambda (frm)
          (f ($frame->continuation frm))))))
  (primitive-set! '$primitive-call/cc primitive-call/cc))


