
(let ()
  
  (define self-evaluating?
    (lambda (x)
      (or (fixnum? x) (immediate? x))))

  (define syntax-error
    (lambda (x)
      (error 'expand "invalid syntax ~s" x)))

  (define build-quoted-constant
    (lambda (x)
      (list 'quote x)))

  (define build-application
    (lambda (rator rand*)
      (list 'funcall rator rand*)))

  (define empty-env
    (lambda () '()))

  (define Etop
    (lambda (expression global-environment)
      (define lookup
        (lambda (sym env ctxt)
          (cond
            [(assq sym env) => cdr]
            [(getprop sym (environment-key global-environment))]
            [(environment-mutable? global-environment)
              
      (define E
        (lambda (x env)
          (cond
            [(self-evaluating? x) 
             (build-quoted-constant x)]
            [(pair? x)
             (let ([a (car x)] [d (cdr x)])
               (cond
                 [(symbol? a)
                  (let ([b (lookup a env x)])
                    (case (binding-type b)
                      [else (bug "invalid binding ~s" b)]))]
                 [(list? d)
                  (build-application
                    (E a env)
                    (map (lambda (x) (E x env)) d))]
                 [else (syntax-error x)]))]
            [else (syntax-error x)])))
      (E expression (empty-env))))


  (define env-rtd (make-record-type "environment" '(mutable? key)))
  (define environment? (record-predicate env-rtd))
  (define environment-mutable? (record-field-accessor env-rtd 0))
  (define environment-key (record-field-accessor env-rtd 1))
  (define make-environment (record-constructor env-rtd))

        

  (define expand
    (lambda (x env)
      (unless (environment? env)
        (error 'expand "~s is not an environment" env))
      (Etop x env)))


)
