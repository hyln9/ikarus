(library (tests reader)
  (export test-reader)
  (import (ikarus) (tests framework))

  (define t
    (lambda (str)
      (lambda (n?)
        (and (number? n?)
             (= (with-input-from-string str read) n?)))))

  (define-syntax reader-tests
    (syntax-rules ()
      [(_ name str* ...)
       (define-tests name 
         [(t str*) (string->number str*)] ...)]))
        
  (reader-tests test-reader
    "12"))

