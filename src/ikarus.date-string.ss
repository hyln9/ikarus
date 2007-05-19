
(library (ikarus date-string)
  (export date-string)
  (import (except (ikarus) date-string))
  (define date-string
    (lambda ()
      (let ([s (make-bytevector 10)])
        (foreign-call "ikrt_bvftime" s 
          (string->utf8-bytevector "%F"))
        (utf8-bytevector->string s)))))
