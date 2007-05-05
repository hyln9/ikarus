
;;; The procedure make-guardian is coped en verbatim 
;;; from Dybvig et al. Guardians paper.

(library (ikarus guardians)
  (export make-guardian)
  (import (except (ikarus) make-guardian))
  (define make-guardian 
    (lambda ()
      (let ([tc
             (let ([x (cons #f #f)])
               (cons x x))])
        (case-lambda
          [()
           (and (not (eq? (car tc) (cdr tc)))
                (let ([x (car tc)])
                  (let ([y (car x)])
                    (set-car! tc (cdr x))
                    (set-car! x #f)
                    (set-cdr! x #f)
                    y)))]
          [(obj) 
           (foreign-call "ikrt_register_guardian" tc obj)
           (void)])))))
