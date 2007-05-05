
(library (ikarus lists)
  (export $memq)
  (import (ikarus))

  (define $memq
    (lambda (x ls)
      (let f ([x x] [ls ls])
        (and (pair? ls)
             (if (eq? x (car ls))
                 ls
                 (f x (cdr ls)))))))
  
  )

