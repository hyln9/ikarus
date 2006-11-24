
(define-syntax $define-record-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ name (field* ...)) 
       (let* ([namestr (symbol->string (syntax-object->datum #'name))]
              [fields (syntax-object->datum #'(field* ...))]
              [fieldstr* (map symbol->string fields)]
              [rtd (make-record-type namestr fields)])
         (with-syntax ([constr 
                        (datum->syntax-object #'name
                          (string->symbol
                            (string-append "$make-" namestr)))]
                       [pred 
                        (datum->syntax-object #'name
                          (string->symbol
                            (string-append "$" namestr "?")))]
                       [(i ...)
                        (datum->syntax-object #'name
                          (let f ([i 0] [f* fieldstr*])
                            (cond
                              [(null? f*) '()]
                              [else (cons i (f (fxadd1 i) (cdr f*)))])))]
                       [(getters ...)
                        (datum->syntax-object #'name
                          (map (lambda (x)
                                 (string->symbol 
                                   (string-append "$" namestr "-" x)))
                               fieldstr*))]
                       [(setters ...)
                        (datum->syntax-object #'name
                          (map (lambda (x) 
                                 (string->symbol
                                   (string-append "$set-" namestr "-" x "!")))
                               fieldstr*))]
                       [rtd rtd])
            #'(begin
                (define-syntax name (cons '$rtd 'rtd))
                (define-syntax constr
                  (syntax-rules ()
                    [(_ field* ...) ($record 'rtd field* ...)]))
                (define-syntax pred
                  (syntax-rules ()
                    [(_ x) ($record/rtd? x 'rtd)]))
                (define-syntax getters 
                  (syntax-rules ()
                    [(_ x) ($record-ref x i)])) ...
                (define-syntax setters 
                  (syntax-rules ()
                    [(_ x v) ($record-set! x i v)])) ...
                )))])))
