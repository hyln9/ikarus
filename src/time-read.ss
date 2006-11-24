
(let ()
  (define r 
    (lambda (p)
      (let ([x (read p)])
        (unless (eof-object? x)
          (r p)))))
  (let ([p (open-input-file "psyntax.pp")])
    (r p)
    (close-input-port p)))

