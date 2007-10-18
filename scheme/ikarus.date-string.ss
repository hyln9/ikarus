
(library (ikarus date-string)
  (export date-string)
  (import
    (rnrs bytevectors)
    (except (ikarus) date-string))
  (define date-string
    (lambda ()
      (let ([s (make-bytevector 10)])
        (foreign-call "ikrt_bvftime" s 
          (string->utf8 "%F"))
        (utf8->string s)))))
