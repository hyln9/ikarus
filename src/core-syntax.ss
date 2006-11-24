(module core-syntax (if lambda letrec or let)
  (define-syntax if (getprop 'if '*sc-expander*))
  (define-syntax lambda (getprop 'lambda '*sc-expander*))
  (define-syntax letrec (getprop 'letrec '*sc-expander*))

  (define-syntax or
    (lambda (x)
      (syntax-case x ()
         ((_) (syntax #f))
         ((_ e) (syntax e))
         ((_ e1 e2 e3 ...)
          (syntax (let ((t e1)) (if t t (or e2 e3 ...))))))))
  
  (define-syntax let
    (lambda (x)
      (syntax-case x ()
         ((_ ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (x ...)))
          (syntax ((lambda (x ...) e1 e2 ...) v ...)))
         ((_ f ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (f x ...)))
          (syntax ((letrec ((f (lambda (x ...) e1 e2 ...))) f)
                    v ...))))))
)
