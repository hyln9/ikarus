
(library (ikarus strings)
  (export string-length string-ref string-set! make-string string->list string=?
          string-append)
  (import 
    (except (ikarus) string-length string-ref string-set! make-string
            string->list string=? string-append)
    (only (scheme) 
          $fx+ $fxsub1 $fxadd1 $char= $car $cdr
          $fxzero? $fx= $fx<= $fx< $fx>=
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
  

  (define string-set! 
    (lambda (s i c) 
      (unless (string? s) 
        (error 'string-set! "~s is not a string" s))
      (unless (fixnum? i)
        (error 'string-set! "~s is not a valid index" i))
      (unless (and ($fx< i ($string-length s))
                   ($fx>= i 0))
        (error 'string-set! "index ~s is out of range for ~s" i s))
      (unless (char? c)
        (error 'string-set! "~s is not a character" c))
      ($string-set! s i c)))
  
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


  (module (string=?)
    (define bstring=?
      (lambda (s1 s2 i j)
        (or ($fx= i j)
            (and ($char= ($string-ref s1 i) ($string-ref s2 i))
                 (bstring=? s1 s2 ($fxadd1 i) j)))))
    (define check-strings-and-return-false
      (lambda (s*)
        (cond
          [(null? s*) #f]
          [(string? ($car s*)) 
           (check-strings-and-return-false ($cdr s*))]
          [else (err ($car s*))])))
    (define strings=?
      (lambda (s s* n)
        (or (null? s*)
            (let ([a ($car s*)])
              (unless (string? a)
                (error 'string=? "~s is not a string" a))
              (if ($fx= n ($string-length a))
                  (and (strings=? s ($cdr s*) n)
                       (bstring=? s a 0 n))
                  (check-strings-and-return-false ($cdr s*)))))))
    (define (err x)
      (error 'string=? "~s is not a string" x))
    (define string=?
      (case-lambda
        [(s s1) 
         (if (string? s) 
             (if (string? s1)
                 (let ([n ($string-length s)])
                   (and ($fx= n ($string-length s1))
                        (bstring=? s s1 0 n)))
                 (err s1))
             (err s))]
        [(s . s*)
         (if (string? s)
             (strings=? s s* ($string-length s))
             (err s))])))

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


  (module (string-append)
    ;; FIXME: make nonconsing on 0,1,2, and 3 args
    (define length*
      (lambda (s* n)
        (cond
          [(null? s*) n]
          [else
           (let ([a ($car s*)])
             (unless (string? a) 
               (error 'string-append "~s is not a string" a))
             (length* ($cdr s*) ($fx+ n ($string-length a))))])))
    (define fill-string
      (lambda (s a si sj ai)
        (unless ($fx= si sj)
          ($string-set! s si ($string-ref a ai))
          (fill-string s a ($fxadd1 si) sj ($fxadd1 ai)))))
    (define fill-strings
      (lambda (s s* i)
        (cond
          [(null? s*) s]
          [else
           (let ([a ($car s*)])
             (let ([n ($string-length a)])
               (let ([j ($fx+ i n)])
                 (fill-string s a i j 0)
                 (fill-strings s ($cdr s*) j))))])))
    (define string-append
      (lambda s*
        (let ([n (length* s* 0)])
          (let ([s ($make-string n)])
            (fill-strings s s* 0))))))

  )
