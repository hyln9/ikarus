
(let ()
  (define-record code (closure-size code-string reloc-vector))
  
  (define make-code^
    (lambda (code-size closure-size)
      (let ([code-size (fxsll (fxsra (fx+ code-size 3) 2) 2)]) 
        (make-code 
          closure-size
          (make-string code-size)
          #f))))
  
  (define code-set! 
    (lambda (code idx byte)
      (string-set! (code-code-string code) idx (integer->char byte))))

  (define code-ref
    (lambda (code idx)
      (char->integer (string-ref (code-code-string code) idx))))
 
  (define (code-size code)
    (string-length (code-code-string code)))

  (primitive-set! 'make-code                   make-code^)
  (primitive-set! 'code?                       code?)
  (primitive-set! 'code-reloc-vector           code-reloc-vector)
  (primitive-set! 'code-closure-size           code-closure-size)
  (primitive-set! 'code-size                   code-size)
  (primitive-set! 'code-set!                   code-set!) 
  (primitive-set! 'code-ref                    code-ref) 
  (primitive-set! 'set-code-reloc-vector!      set-code-reloc-vector!))

