
(library (ikarus vectors)
  (export make-vector vector-length)
  (import 
    (except (ikarus) make-vector vector-length)
    (only (scheme)
          $fx= $fx>= $fx+ $vector-set! 
          $make-vector $vector-length))


  (define vector-length
    (lambda (x)
      (unless (vector? x) 
        (error 'vector-length "~s is not a vector" x))
      ($vector-length x)))

  (module (make-vector)
    (define fill!
      (lambda (v i n fill)
        (cond
          [($fx= i n) v]
          [else
           ($vector-set! v i fill)
           (fill! v ($fx+ i 1) n fill)])))
    (define make-vector
      (case-lambda
        [(n) (make-vector n (void))]
        [(n fill)
         (unless (and (fixnum? n) ($fx>= n 0))
           (error 'make-vector "~s is not a valid length" n))
         (fill! ($make-vector n) 0 n fill)])))

  )
