

(library (ikarus core)
  (export)
  (import (scheme))

(primitive-set! 'primitive-set!
  (lambda (x v)
    (unless (symbol? x)
      (error 'primitive-set! "~s is not a symbol" x))
    (primitive-set! x v)
    (set-top-level-value! x v)))

(primitive-set! 'eof-object
  (lambda () (eof-object)))

(primitive-set! 'void
  (lambda () (void)))
  
(primitive-set! 'apply
  (let ()
    (define (err f ls)
      (if (procedure? f)
          (error 'apply "not a list")
          (error 'apply "~s is not a procedure" f)))
    (define (fixandgo f a0 a1 ls p d)
      (cond
        [(null? ($cdr d))
         (let ([last ($car d)])
           ($set-cdr! p last)
           (if (and (procedure? f) (list? last))
               ($$apply f a0 a1 ls)
               (err f last)))]
        [else (fixandgo f a0 a1 ls d ($cdr d))]))
    (define apply
      (case-lambda
        [(f ls) 
         (if (and (procedure? f) (list? ls))
             ($$apply f ls)
             (err f ls))]
        [(f a0 ls)
         (if (and (procedure? f) (list? ls))
             ($$apply f a0 ls)
             (err f ls))]
        [(f a0 a1 ls)
         (if (and (procedure? f) (list? ls))
             ($$apply f a0 a1 ls)
             (err f ls))]
        [(f a0 a1 . ls)
         (fixandgo f a0 a1 ls ls ($cdr ls))]))
    apply))
  




(primitive-set! 'pointer-value
  (lambda (x)
    (pointer-value x)))

(primitive-set! 'date-string
  (lambda ()
    (let ([s (make-string 10)])
      (foreign-call "ikrt_strftime" s "%F")
      s)))


)
