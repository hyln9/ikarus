
(define (fact n ac)
  (if (zero? n)
      ac
      (fact (- n 1) (* n ac))))
(begin (fact 10000 1) #f)
