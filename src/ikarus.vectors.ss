
(library (ikarus vectors)
  (export make-vector vector vector-length vector-ref vector-set!)
  (import 
    (except (ikarus) make-vector vector 
            vector-length vector-ref vector-set!)
    (only (scheme)
          $fx= $fx>= $fx< $fx<= $fx+ $car $cdr 
          $vector-set! $vector-ref $make-vector $vector-length))


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


  (define vector
    ;;; FIXME: add case-lambda
    (letrec ([length
              (lambda (ls n)
                (cond
                 [(null? ls) n]
                 [else (length ($cdr ls) ($fx+ n 1))]))]
             [loop 
              (lambda (v ls i n)
                (cond
                 [($fx= i n) v]
                 [else 
                  ($vector-set! v i ($car ls))
                  (loop v ($cdr ls) ($fx+ i 1) n)]))])
       (lambda ls
         (let ([n (length ls 0)])
           (let ([v (make-vector n)])
             (loop v ls 0 n))))))


  (define vector-ref 
    (lambda (v i)
      (unless (vector? v)
        (error 'vector-ref "~s is not a vector" v))
      (unless (fixnum? i)
        (error 'vector-ref "~s is not a valid index" i))
      (unless (and ($fx< i ($vector-length v))
                   ($fx<= 0 i))
        (error 'vector-ref "index ~s is out of range for ~s" i v))
      ($vector-ref v i)))
  
  (define vector-set! 
    (lambda (v i c) 
      (unless (vector? v) 
        (error 'vector-set! "~s is not a vector" v))
      (unless (fixnum? i)
        (error 'vector-set! "~s is not a valid index" i))
      (unless (and ($fx< i ($vector-length v))
                   ($fx<= 0 i))
        (error 'vector-set! "index ~s is out of range for ~s" i v))
      ($vector-set! v i c)))
  




  )
