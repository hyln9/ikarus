
(import (ikarus) (ikarus system $foreign))

(define self (dlopen #f))
(define hosym (dlsym self "ho"))

(define ho 
  ((make-ffi 'sint32 '(pointer sint32)) hosym))

(define foradd1 
  ((make-callback 'sint32 '(sint32)) 
     (trace-lambda add1 (n) 
       (printf "collecting ...\n")
       (collect)
       (printf "collecting done\n")
       (add1 n))))

(define foradd1^
  ((make-callback 'sint32 '(sint32))
     (lambda (n) 
       (printf "collecting ...\n")
       (collect)
       (printf "collecting done\n")
       (add1 n))))

(define-syntax assert^
  (syntax-rules ()
    [(_ expr)
     (begin 
       (line)
       (printf "TESTING ~s\n" 'expr)
       (assert expr)
       (printf "OK\n"))]))

(define (line)
  (printf "=========================================================\n"))

(assert^ (= (ho (dlsym self "cadd1") 17) (+ 18 18)))
(assert^ (= (ho foradd1^ 17) (+ 18 18)))
(assert^ (= (ho foradd1 17) (+ 18 18)))

(line)
(printf "Happy Happy Joy Joy\n")
