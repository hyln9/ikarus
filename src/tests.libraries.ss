

(library (F)
  (export f)
  (import (ikarus))
  (define f
    (lambda (x) 12))
  (printf "F invoked\n"))

(library (G)
  (export)
  (import (ikarus) (F))
  (define-syntax t f)
  (printf "G invoked: f=~s\n" t))
    
(library (Q)
  (export foo)
  (import (ikarus))
  (define-record foo (bar baz)))

(library (R)
  (export)
  (import (ikarus) (Q))
  (printf "RTD=~s\n" (type-descriptor foo)))

(invoke (R))

(library (F0)
  (export f)
  (import (ikarus))
  (define g 17)
  (define-syntax f
    (lambda (x) #'h))
  (define-syntax h 
    (lambda (x) #'g)))

(library (F0 client)
  (export)
  (import (ikarus) (F0))
  (unless (= (f) 17) 
    (error #f "F0 client"))
  (printf "F0 client ok\n"))

(invoke (F0 client))


