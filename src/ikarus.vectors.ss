
(library (ikarus vectors)
  (export make-vector vector vector-length vector-ref vector-set!
          vector->list list->vector)
  (import 
    (except (ikarus) make-vector vector 
            vector-length vector-ref vector-set!
            vector->list list->vector)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $vectors))


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

  (define vector->list
    (lambda (v)
      (define f
        (lambda (v i ls)
          (cond
            [($fx< i 0) ls]
            [else
             (f v ($fxsub1 i) (cons ($vector-ref v i) ls))])))
      (if (vector? v)
          (let ([n ($vector-length v)])
            (if ($fxzero? n)
                '()
                (f v ($fxsub1 n) '())))
          (error 'vector->list "~s is not a vector" v))))

  (define list->vector
    (letrec ([race
              (lambda (h t ls n)
               (if (pair? h)
                   (let ([h ($cdr h)])
                      (if (pair? h)
                          (if (not (eq? h t))
                              (race ($cdr h) ($cdr t) ls ($fx+ n 2))
                              (error 'list->vector "circular list ~s" ls))
                          (if (null? h)
                              ($fx+ n 1)
                              (error 'list->vector "~s is not a proper list" ls))))
                   (if (null? h)
                       n
                       (error 'list->vector "~s is not a proper list" ls))))]
              [fill
               (lambda (v i ls)
                 (cond
                   [(null? ls) v]
                   [else
                    (let ([c ($car ls)])
                      ($vector-set! v i c)
                      (fill v ($fxadd1 i) (cdr ls)))]))])
       (lambda (ls)
         (let ([n (race ls ls ls 0)])
           (let ([v (make-vector n)])
             (fill v 0 ls))))))


  )
