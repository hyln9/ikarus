
(define-syntax record-case
  (lambda (x)
    (define (enumerate fld* i)
      (syntax-case fld* ()
       [() #'()]
       [(x . x*) 
        (with-syntax ([i i] [i* (enumerate #'x* (fx+ i 1))])
          #'(i . i*))]))
    (define (generate-body ctxt cls*)
      (syntax-case cls* (else)
       [() (with-syntax ([x x]) #'(error #f "unmatched ~s in ~s" v #'x))]
       [([else b b* ...])  #'(begin b b* ...)]
       [([(rec-name rec-field* ...) b b* ...] . rest) (identifier? #'rec-name)
        (with-syntax ([altern (generate-body ctxt #'rest)]
                      [(id* ...) (enumerate #'(rec-field* ...) 0)]
                      [rtd #'(type-descriptor rec-name)])
         #'(if ((record-predicate rtd) v)
               (let ([rec-field* ;($record-ref v id*)] ...)
                      ((record-field-accessor rtd id*) v)] ...)
                 b b* ...)
               altern))]))
    (syntax-case x ()
     [(_ expr cls* ...)
      (with-syntax ([body (generate-body #'_ #'(cls* ...))])
        #'(let ([v expr]) body))])))
