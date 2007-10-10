(library (tests lists)
  (export test-lists)
  (import (ikarus) (tests framework))

  (define-tests test-lists
    [values (equal? (for-all even? '(1 2 3 4)) #f)]
    [values (equal? (for-all even? '(10 12 14 16)) #t)]
    [values (equal? (for-all even? '(2 3 4)) #f)]
    [values (equal? (for-all even? '(12 14 16)) #t)]
    [values (equal? (for-all (lambda (x) x) '(12 14 16)) 16)]
    [values (equal? (for-all (lambda (x) x) '(12 14)) 14)]
    [values (equal? (for-all (lambda (x) x) '(12)) 12)]
    [values (equal? (for-all (lambda (x) x) '()) #t)]
    [values (equal? (for-all even? '(13 . 14)) #f)]
    [values (equal? (for-all cons '(1 2 3) '(a b c)) '(3 . c))]
    [values (equal? (for-all (lambda (a b) (= a 1)) '(1 2 3) '(a b c)) #f)]
    [values (equal? (for-all (lambda (a b) (= a 1)) '(1 2) '(a b c)) #f)]
   )) 
    
