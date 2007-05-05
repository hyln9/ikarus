

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
  



(primitive-set! 'gensym->unique-string
  (lambda (x)
    (unless (symbol? x)
      (error 'gensym->unique-string "~s is not a gensym" x))
    (let ([us ($symbol-unique-string x)])
      (cond
        [(string? us) us]
        [(not us)
         (error 'gensym->unique-string "~s is not a gensym" x)]
        [else
         (let f ([x x])
           (let ([id (uuid)])
             ($set-symbol-unique-string! x id)
             (cond
               [(foreign-call "ikrt_intern_gensym" x) id]
               [else (f x)])))]))))




(primitive-set! 'gensym-prefix
  (make-parameter
    "g"
    (lambda (x)
      (unless (string? x)
        (error 'gensym-prefix "~s is not a string" x))
      x)))

(primitive-set! 'gensym-count
  (make-parameter
    0
    (lambda (x)
      (unless (and (fixnum? x) ($fx>= x 0))
        (error 'gensym-count "~s is not a valid count" x))
      x)))

(primitive-set! 'print-gensym
  (make-parameter
    #t
    (lambda (x)
      (unless (or (boolean? x) (eq? x 'pretty))
        (error 'print-gensym "~s is not in #t|#f|pretty" x))
      x)))



(primitive-set! 'pointer-value
  (lambda (x)
    (pointer-value x)))

(primitive-set! 'date-string
  (lambda ()
    (let ([s (make-string 10)])
      (foreign-call "ikrt_strftime" s "%F")
      s)))


(primitive-set! 'command-line-arguments
  (make-parameter ($arg-list)
    (lambda (x)
      (if (and (list? x) (andmap string? x))
          x
          (error 'command-list "invalid command-line-arguments ~s\n" x)))))






(primitive-set! 'symbol->string
  (lambda (x)
    (unless (symbol? x)
      (error 'symbol->string "~s is not a symbol" x))
    (let ([str ($symbol-string x)])
      (or str
          (let ([ct (gensym-count)])
            (let ([str (string-append (gensym-prefix) (fixnum->string ct))])
              ($set-symbol-string! x str)
              (gensym-count ($fxadd1 ct))
              str))))))



)
