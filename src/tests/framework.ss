
(library (tests framework)
  (export define-tests)
  (import (ikarus))
  (define-syntax define-tests
    (syntax-rules ()
      [(_ test-all [p0 e0] ...)
       (define test-all
         (lambda ()
           (let ([p p0] [e e0])
             (unless (p e)
               (error 'test-all "~s failed, got ~s" 
                      '(p0 e0) e))) 
           ...
           (printf "[~s: ~s] Happy Happy Joy Joy\n" 
                   (length '(p0 ...))'test-all  )))])))
