
(define-record code (closure-size code-vec reloc-vec))

(define make-code
  (let ([make-code make-code])
    (lambda (code-size reloc-size closure-size)
      (let ([code-size (fxsll (fxsra (fx+ code-size 3) 2) 2)]) 
        (make-code 
          closure-size
          (make-string code-size (integer->char 0))
          (make-vector (fxsra reloc-size 2)))))))

(define set-code-byte! 
  (lambda (code idx byte)
    (string-set! (code-code-vec code) idx (integer->char byte))))


(define set-code-word!
  (lambda (code idx x)
    (cond
      [(fixnum? x) 
       (set-code-byte! code (fx+ idx 0) (fxsll (fxlogand x #x3F) 2))
       (set-code-byte! code (fx+ idx 1) (fxlogand (fxsra x 6) #xFF))
       (set-code-byte! code (fx+ idx 2) (fxlogand (fxsra x 14) #xFF))
       (set-code-byte! code (fx+ idx 3) (fxlogand (fxsra x 22) #xFF))]
      [else (error 'set-code-word! "unhandled ~s" x)])))

(define set-code-object!
  (lambda (code obj code-idx reloc-idx)
    (let ([v (code-reloc-vec code)])
      (vector-set! v reloc-idx (list 'object code-idx obj)))))

(define set-code-foreign-object!
  (lambda (code obj code-idx reloc-idx) 
    (let ([v (code-reloc-vec code)])
      (vector-set! v reloc-idx (list 'foreign code-idx obj))
      (vector-set! v (fxadd1 reloc-idx) '(skip)))))


(define set-code-object+offset/rel! 
  (lambda (code obj code-idx obj-idx reloc-idx)
    (let ([v (code-reloc-vec code)])
      (vector-set! v reloc-idx 
        (list 'object+off/rel code-idx obj obj-idx))
      (vector-set! v (fxadd1 reloc-idx) '(skip)))))

(define set-code-object+offset!
  (lambda (code obj code-idx obj-idx reloc-idx)
    (let ([v (code-reloc-vec code)])
      (vector-set! v reloc-idx 
        (list 'object+off code-idx obj obj-idx))
      (vector-set! v (fxadd1 reloc-idx) '(skip)))))

(define make-code-executable!
  (lambda (x) (void)))

