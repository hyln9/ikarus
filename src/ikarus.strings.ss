
(library (ikarus strings)
  (export string-length string-ref make-string string->list)
  (import 
    (except (ikarus) string-length string-ref make-string
            string->list)
    (only (scheme) 
          $fx+ $fxsub1 $fxzero? $fx= $fx<= $fx<
          $string-length $string-ref 
          $make-string $string-set!))


  (define string-length
    (lambda (x)
      (unless (string? x)
        (error 'string-length "~s is not a string" x))
      ($string-length x)))


  (define (string-ref s i)
    (unless (string? s) 
      (error 'string-ref "~s is not a string" s))
    (unless (fixnum? i)
      (error 'string-ref "~s is not a valid index" i))
    (unless (and ($fx< i ($string-length s))
                 ($fx<= 0 i))
      (error 'string-ref "index ~s is out of range for ~s" i s))
    ($string-ref s i))
  
  
  (define make-string
    (let ()
      (define fill!
        (lambda (s i n c)
          (cond
            [($fx= i n) s]
            [else
             ($string-set! s i c)
             (fill! s ($fx+ i 1) n c)])))
      (define make-string
        (case-lambda
          [(n) 
           (unless (and (fixnum? n) (fx>= n 0))
             (error 'make-string "~s is not a valid length" n))
           ($make-string n)]
          [(n c)
           (unless (and (fixnum? n) (fx>= n 0))
             (error 'make-string "~s is not a valid length" n))
           (unless (char? c)
             (error 'make-string "~s is not a character" c))
           (fill! ($make-string n) 0 n c)]))
        make-string))


  (define string->list
    (lambda (x)
      (unless (string? x)
        (error 'string->list "~s is not a string" x))
      (let f ([x x] [i ($string-length x)] [ac '()])
        (cond
          [($fxzero? i) ac]
          [else
           (let ([i ($fxsub1 i)])
             (f x i (cons ($string-ref x i) ac)))]))))



  )
