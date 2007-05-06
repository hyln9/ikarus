
(library (ikarus date-string)
  (export date-string)
  (import (except (ikarus) date-string))
  (define date-string
    (lambda ()
      (let ([s (make-string 10)])
        (foreign-call "ikrt_strftime" s "%F")
        s))))
