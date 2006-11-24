
($pcb-set! error
  (lambda args
    (foreign-call "S_error" args)))


($pcb-set! exit
  (lambda args
    (if (null? args)
        ($exit 0)
        (if (null? ($cdr args))
            ($exit ($car args))
            (error 'exit "too many arguments")))))


($pcb-set! eof-object
  (lambda () (eof-object)))

($pcb-set! void
  (lambda () (void)))
  
($pcb-set! eof-object?
  (lambda (x) (eof-object? x)))


($pcb-set! fxadd1
  (lambda (n)
    (unless (fixnum? n)
      (error 'fxadd1 "~s is not a fixnum" n))
    ($fxadd1 n)))
  
($pcb-set! fxsub1 
  (lambda (n) 
    (unless (fixnum? n)
      (error 'fxsub1 "~s is not a fixnum" n))
    ($fxsub1 n)))
  
($pcb-set! fixnum->char
  (lambda (n)
    (unless (fixnum? n)
      (error 'fixnum->char "~s is not a fixnum" n))
    (unless (and ($fx>= n 0)
                 ($fx<= n 127))
      (error 'fixnum->char "~s is out of range[0..127]" n))
    ($fixnum->char n)))
  
($pcb-set! char->fixnum 
  (lambda (x) 
    (unless (char? x)
      (error 'char->fixnum "~s is not a character" x))
    ($char->fixnum x)))
  
($pcb-set! fxlognot 
  (lambda (x)
    (unless (fixnum? x) 
      (error 'fxlognot "~s is not a fixnum" x))
    ($fxlognot x)))
  
($pcb-set! fixnum? (lambda (x) (fixnum? x)))
  
($pcb-set! fxzero? 
  (lambda (x)
    (unless (fixnum? x)
      (error 'fxzero? "~s is not a fixnum" x))
    ($fxzero? x)))


($pcb-set! boolean? (lambda (x) (boolean? x)))
  
($pcb-set! char? (lambda (x) (char? x)))

($pcb-set! vector? (lambda (x) (vector? x)))

($pcb-set! string? (lambda (x) (string? x)))

($pcb-set! procedure? (lambda (x) (procedure? x)))

($pcb-set! null? (lambda (x) (null? x)))

($pcb-set! pair? (lambda (x) (pair? x)))

($pcb-set! car 
  (lambda (x)
    (unless (pair? x)
      (error 'car "~s is not a pair" x))
    ($car x)))

($pcb-set! cdr
  (lambda (x)
    (unless (pair? x)
      (error 'cdr "~s is not a pair" x))
    ($cdr x)))

($pcb-set! caar 
  (lambda (x)
    (unless (pair? x) (error 'caar "incorrect list structure ~s" x))
    (let ([a ($car x)])
      (unless (pair? a) (error 'caar "incorrect list structure ~s" x))
      ($car a))))

($pcb-set! cadr 
  (lambda (x)
    (unless (pair? x) (error 'cadr "incorrect list structure ~s" x))
    (let ([d ($cdr x)])
      (unless (pair? d) (error 'cadr "incorrect list structure ~s" x))
      ($car d))))

($pcb-set! cdar 
  (lambda (x)
    (unless (pair? x) (error 'cdar "incorrect list structure ~s" x))
    (let ([a ($car x)])
      (unless (pair? a) (error 'cdar "incorrect list structure ~s" x))
      ($cdr a))))
 
($pcb-set! cddr 
  (lambda (x)
    (unless (pair? x) (error 'cddr "incorrect list structure ~s" x))
    (let ([d ($cdr x)])
      (unless (pair? d) (error 'cddr "incorrect list structure ~s" x))
      ($cdr d))))

($pcb-set! caddr 
  (lambda (x)
    (unless (pair? x) (error 'caddr "incorrect list structure ~s" x))
    (let ([d ($cdr x)])
      (unless (pair? d) (error 'caddr "incorrect list structure ~s" x))
      (let ([dd ($cdr d)])
        (unless (pair? dd) (error 'caddr "correct list structure ~s" x))
        ($car dd))))) 
      
($pcb-set! cadddr 
  (lambda (x)
    (unless (pair? x) (error 'cadddr "incorrect list structure ~s" x))
    (let ([d ($cdr x)])
      (unless (pair? d) (error 'cadddr "incorrect list structure ~s" x))
      (let ([dd ($cdr d)])
        (unless (pair? dd) (error 'cadddr "correct list structure ~s" x))
        (let ([ddd ($cdr dd)])
          (unless (pair? ddd) (error 'cadddr "correct list structure ~s" x))
          ($car ddd))))))


($pcb-set! cddddr 
  (lambda (x)
    (unless (pair? x) (error 'cddddr "incorrect list structure ~s" x))
    (let ([d ($cdr x)])
      (unless (pair? d) (error 'cddddr "incorrect list structure ~s" x))
      (let ([dd ($cdr d)])
        (unless (pair? dd) (error 'cddddr "correct list structure ~s" x))
        (let ([ddd ($cdr dd)])
          (unless (pair? ddd) (error 'cddddr "correct list structure ~s" x))
          ($cdr ddd))))))

(let ()
  (define fill!
    (lambda (v i n fill)
      (cond
        [($fx= i n) v]
        [else
         ($vector-set! v i fill)
         (fill! v ($fx+ i 1) n fill)])))
  ($pcb-set! make-vector 
    (lambda (n . opt) 
      (unless (and (fixnum? n) ($fx>= n 0))
        (error 'make-vector "~s is not a valid size" n))
      (let ([fill (if (null? opt)
                      #f
                      (if (null? ($cdr opt))
                          ($car opt)
                          (error 'make-vector "too many arguments")))])
        (let ([v ($make-vector n)])
          (fill! v 0 n fill))))))

($pcb-set! vector-length
  (lambda (x)
    (unless (vector? x) 
      (error 'vector-length "~s is not a vector" x))
    ($vector-length x)))

($pcb-set! make-string 
  (lambda (x)
    (unless (and (fixnum? x) ($fx>= x 0))
      (error 'make-string "~s is not a valid size" x))
    ($make-string x)))
  
($pcb-set! string-length
  (lambda (x)
    (unless (string? x)
      (error 'string-length "~s is not a string" x))
    ($string-length x)))
  
($pcb-set! not (lambda (x) (not x)))
  
($pcb-set! symbol->string
  (lambda (x)
    (unless (symbol? x)
      (error 'symbol->string "~s is not a symbol" x))
    ($symbol-string x)))

($pcb-set! top-level-value
  (lambda (x)
    (unless (symbol? x)
      (error 'top-level-value "~s is not a symbol" x))
    (let ([v ($symbol-value x)])
      (when ($unbound-object? v)
        (error 'top-level-value "unbound variable ~s" x))
      v)))

($pcb-set! top-level-bound?
  (lambda (x)
    (unless (symbol? x)
      (error 'top-level-bound? "~s is not a symbol" x))
    (not ($unbound-object? ($symbol-value x)))))

($pcb-set! set-top-level-value!
  (lambda (x v)
    (unless (symbol? x)
      (error 'set-top-level-value! "~s is not a symbol" x))
    ($set-symbol-value! x v)))
 
($pcb-set! symbol? (lambda (x) (symbol? x)))
  
($pcb-set! fx+ 
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx+ "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx+ "~s is not a fixnum" y))
    ($fx+ x y)))

($pcb-set! fx-
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx- "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx- "~s is not a fixnum" y))
    ($fx- x y)))
  
($pcb-set! fx*
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx* "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx* "~s is not a fixnum" y))
    ($fx* x y)))
  


($pcb-set! fxquotient
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxquotient "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxquotient "~s is not a fixnum" y))
    (when ($fxzero? y)
      (error 'fxquotient "zero dividend ~s" y))
    ($fxquotient x y))) 


($pcb-set! fxremainder
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxremainder "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxremainder "~s is not a fixnum" y))
    (when ($fxzero? y)
      (error 'fxremainder "zero dividend ~s" y))
    (let ([q ($fxquotient x y)])
      ($fx- x ($fx* q y)))))
 

($pcb-set! fxlogor
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxlogor "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxlogor "~s is not a fixnum" y))
    ($fxlogor x y)))

($pcb-set! fxlogxor
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxlogxor "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxlogxor "~s is not a fixnum" y))
    ($fxlogxor x y)))
  
($pcb-set! fxlogand
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxlogand "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxlogand "~s is not a fixnum" y))
    ($fxlogand x y)))
 
($pcb-set! fxsra
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxsra "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxsra "~s is not a fixnum" y))
    (unless ($fx>= y 0)
      (error 'fxsra "negative shift not allowed, got ~s" y))
    ($fxsra x y)))
 
($pcb-set! fxsll
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxsll "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxsll "~s is not a fixnum" y))
    (unless ($fx>= y 0)
      (error 'fxsll "negative shift not allowed, got ~s" y))
    ($fxsll x y))) 

($pcb-set! fx=
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx= "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx= "~s is not a fixnum" y))
    ($fx= x y))) 

($pcb-set! fx<
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx< "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx< "~s is not a fixnum" y))
    ($fx< x y)))

($pcb-set! fx<=
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx<= "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx<= "~s is not a fixnum" y))
    ($fx<= x y)))
 
($pcb-set! fx>
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx> "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx> "~s is not a fixnum" y))
    ($fx> x y)))

($pcb-set! fx>=
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx>= "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx>= "~s is not a fixnum" y))
    ($fx>= x y)))
  
($pcb-set! char=
  (lambda (x y) 
    (unless (char? x)
      (error 'char= "~s is not a character" x))
    (unless (char? y)
      (error 'char= "~s is not a character" y))
    ($char= x y)))
  
($pcb-set! char<
  (lambda (x y) 
    (unless (char? x)
      (error 'char< "~s is not a character" x))
    (unless (char? y)
      (error 'char< "~s is not a character" y))
    ($char< x y)))
  
($pcb-set! char<=
  (lambda (x y) 
    (unless (char? x)
      (error 'char<= "~s is not a character" x))
    (unless (char? y)
      (error 'char<= "~s is not a character" y))
    ($char<= x y)))
  
($pcb-set! char>
  (lambda (x y) 
    (unless (char? x)
      (error 'char> "~s is not a character" x))
    (unless (char? y)
      (error 'char> "~s is not a character" y))
    ($char> x y)))
  
($pcb-set! char>=
  (lambda (x y) 
    (unless (char? x)
      (error 'char>= "~s is not a character" x))
    (unless (char? y)
      (error 'char>= "~s is not a character" y))
    ($char>= x y)))

($pcb-set! cons (lambda (x y) (cons x y)))

($pcb-set! eq? (lambda (x y) (eq? x y)))

($pcb-set! set-car!
  (lambda (x y) 
    (unless (pair? x)
      (error 'set-car! "~s is not a pair" x))
    ($set-car! x y)))

($pcb-set! set-cdr! 
  (lambda (x y)
    (unless (pair? x)
      (error 'set-cdr! "~s is not a pair" x))
    ($set-cdr! x y)))

($pcb-set! vector-ref 
  (lambda (v i)
    (unless (vector? v)
      (error 'vector-ref "~s is not a vector" v))
    (unless (fixnum? i)
      (error 'vector-ref "~s is not a valid index" i))
    (unless (and ($fx< i ($vector-length v))
                 ($fx<= 0 i))
      (error 'vector-ref "index ~s is out of range for ~s" i v))
    ($vector-ref v i)))

($pcb-set! string-ref 
  (lambda (s i) 
    (unless (string? s) 
      (error 'string-ref "~s is not a string" s))
    (unless (fixnum? i)
      (error 'string-ref "~s is not a valid index" i))
    (unless (and ($fx< i ($string-length s))
                 ($fx<= 0 i))
      (error 'string-ref "index ~s is out of range for ~s" i s))
    ($string-ref s i)))

($pcb-set! vector-set! 
  (lambda (v i c) 
    (unless (vector? v) 
      (error 'vector-set! "~s is not a vector" v))
    (unless (fixnum? i)
      (error 'vector-set! "~s is not a valid index" i))
    (unless (and ($fx< i ($vector-length v))
                 ($fx<= 0 i))
      (error 'vector-set! "index ~s is out of range for ~s" i v))
    ($vector-set! v i c)))


($pcb-set! string-set! 
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

($pcb-set! vector
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
         (let ([v ($make-vector n)])
           (loop v ls 0 n))))))

(letrec ([length
          (lambda (ls n)
            (cond
             [(null? ls) n]
             [else (length ($cdr ls) ($fx+ n 1))]))]
         [loop 
          (lambda (s ls i n)
            (cond
             [($fx= i n) s]
             [else 
              (let ([c ($car ls)])
                (unless (char? c)
                  (error 'string "~s is not a character" c))
                ($string-set! s i c)
                (loop s ($cdr ls) ($fx+ i 1) n))]))])
  (let ([f 
        (lambda ls
          (let ([n (length ls 0)])
            (let ([s ($make-string n)])
              (loop s ls 0 n))))])
    ($pcb-set! string f)))
  
($pcb-set! list?
  (letrec ([race
            (lambda (h t)
             (if (pair? h)
                 (let ([h ($cdr h)])
                    (if (pair? h)
                        (and (not (eq? h t))
                             (race ($cdr h) ($cdr t)))
                        (null? h)))
                 (null? h)))])
     (lambda (x) (race x x))))



($pcb-set! reverse
  (letrec ([race
            (lambda (h t ls ac)
             (if (pair? h)
                 (let ([h ($cdr h)] [ac (cons ($car h) ac)])
                    (if (pair? h)
                        (if (not (eq? h t))
                            (race ($cdr h) ($cdr t) ls (cons ($car h) ac))
                            (error 'reverse "~s is a circular list" ls))
                        (if (null? h)
                            ac
                            (error 'reverse "~s is not a proper list" ls))))
                 (if (null? h)
                     ac
                     (error 'reverse "~s is not a proper list" ls))))])
     (lambda (x)
       (race x x x '()))))

($pcb-set! memq
  (letrec ([race
            (lambda (h t ls x)
               (if (pair? h)
                   (if (eq? ($car h) x)
                       h
                       (let ([h ($cdr h)])
                         (if (pair? h)
                             (if (eq? ($car h) x)
                                 h
                                 (if (not (eq? h t))
                                     (race ($cdr h) ($cdr t) ls x)
                                     (error 'memq "circular list ~s" ls)))
                             (if (null? h)
                                 '#f
                                 (error 'memq "~s is not a proper list" ls)))))
                   (if (null? h)
                       '#f
                       (error 'memq "~s is not a proper list" ls))))])
     (lambda (x ls)
       (race ls ls ls x))))
 
($pcb-set! list->string
  (letrec ([race
            (lambda (h t ls n)
             (if (pair? h)
                 (let ([h ($cdr h)])
                    (if (pair? h)
                        (if (not (eq? h t))
                            (race ($cdr h) ($cdr t) ls ($fx+ n 2))
                            (error 'reverse "circular list ~s" ls))
                        (if (null? h)
                            ($fx+ n 1)
                            (error 'reverse "~s is not a proper list" ls))))
                 (if (null? h)
                     n
                     (error 'reverse "~s is not a proper list" ls))))]
            [fill
             (lambda (s i ls)
               (cond
                 [(null? ls) s]
                 [else
                  (let ([c ($car ls)])
                    (unless (char? c)
                      (error 'list->string "~s is not a character" c))
                    ($string-set! s i c)
                    (fill s ($fxadd1 i) (cdr ls)))]))])
     (lambda (ls)
       (let ([n (race ls ls ls 0)])
         (let ([s ($make-string n)])
           (fill s 0 ls))))))

($pcb-set! length
  (letrec ([race
            (lambda (h t ls n)
             (if (pair? h)
                 (let ([h ($cdr h)])
                    (if (pair? h)
                        (if (not (eq? h t))
                            (race ($cdr h) ($cdr t) ls ($fx+ n 2))
                            (error 'length "circular list ~s" ls))
                        (if (null? h)
                            ($fx+ n 1)
                            (error 'length "~s is not a proper list" ls))))
                 (if (null? h)
                     n
                     (error 'length "~s is not a proper list" ls))))])
     (lambda (ls)
       (race ls ls ls 0))))

($pcb-set! apply
  (letrec ([fix
            (lambda (arg arg*)
              (cond
               [(null? arg*) 
                (if (list? arg)
                    arg
                    (error 'apply "~s is not a list" arg))]
               [else
                (cons arg (fix ($car arg*) ($cdr arg*)))]))])
    (lambda (f arg . arg*)
      ($apply f (fix arg arg*)))))
   
($pcb-set! assq
  (letrec ([race
            (lambda (x h t ls)
              (if (pair? h)
                  (let ([a ($car h)] [h ($cdr h)])
                     (if (pair? a)
                         (if (eq? ($car a) x)
                             a
                             (if (pair? h)
                                 (if (not (eq? h t))
                                     (let ([a ($car h)])
                                        (if (pair? a)
                                            (if (eq? ($car a) x)
                                                a
                                                (race x ($cdr h) ($cdr t) ls))
                                            (error 'assq "malformed alist ~s"
                                                   ls)))
                                     (error 'assq "circular list ~s" ls))
                                 (if (null? h)
                                     #f
                                     (error 'assq "~s is not a proper list" ls))))
                         (error 'assq "malformed alist ~s" ls)))
                  (if (null? h)
                      #f
                      (error 'assq "~s is not a proper list" ls))))])
     (lambda (x ls) 
       (race x ls ls ls))))

($pcb-set! string->symbol
  (lambda (x)
    (unless (string? x) 
      (error 'string->symbol "~s is not a string" x))
    ($intern x)))
  
($pcb-set! gensym
  (lambda args
    (if (null? args)
        ($make-symbol "g")
        (if (null? ($cdr args))
            (let ([a ($car args)])
               (if (string? a)
                   ($make-symbol a)
                   (error 'gensym "~s is not a string" a)))
            (error 'gensym "too many arguments")))))

($pcb-set! make-parameter
  (letrec ([make-param-no-guard
            (lambda (x)
              (lambda args
                (if (null? args)
                    x
                    (if (null? ($cdr args))
                        (set! x ($car args))
                        (error #f "too many arguments to parameter")))))]
           [make-param-with-guard
            (lambda (x g)
              (let ([f
                     (lambda args
                       (if (null? args)
                           x
                           (if (null? ($cdr args))
                               (set! x (g ($car args)))
                               (error #f "too many arguments to parameter"))))])
               (if (procedure? g)
                   (begin (set! x (g x)) f)
                   (error 'make-parameter "not a procedure ~s" g))))])
    (lambda args
       (if (pair? args)
           (let ([x ($car args)] [args ($cdr args)])
             (if (null? args)
                 (make-param-no-guard x)
                 (let ([g ($car args)])
                   (if (null? ($cdr args))
                       (make-param-with-guard x g)
                       (error 'make-parameter "too many arguments")))))
           (error 'make-parameter "insufficient arguments")))))

(let ()
   (define vector-loop
     (lambda (x y i n)
       (or ($fx= i n)
           (and (equal? ($vector-ref x i) ($vector-ref y i))
                (vector-loop x y ($fxadd1 i) n)))))
   (define string-loop
     (lambda (x y i n)
       (or ($fx= i n)
           (and ($char= ($string-ref x i) ($string-ref y i))
                (string-loop x y ($fxadd1 i) n)))))
   (define equal?
     (lambda (x y)
       (cond
         [(eq? x y) #t]
         [(pair? x) 
          (and (pair? y)
               (equal? ($car x) ($car y))
               (equal? ($cdr x) ($cdr y)))]
         [(vector? x)
          (and (vector? y)
               (let ([n ($vector-length x)])
                 (and ($fx= n ($vector-length y))
                      (vector-loop x y 0 n))))]
         [(string? x)
          (and (string? y)
               (let ([n ($string-length x)])
                 (and ($fx= n ($string-length y))
                      (string-loop x y 0 n))))]
         [else #f])))
   ($pcb-set! equal? equal?))

(let ()
  (define map1
    (lambda (h t ls f)
     (if (pair? h)
         (let ([h ($cdr h)] [a1 ($car h)])
            (if (pair? h)
                (if (not (eq? h t))
                    (let ([a2 ($car h)])
                      (cons (f a1) (cons (f a2) (map1 ($cdr h) ($cdr t) ls f))))
                    (error 'map "circular list ~s" ls))
                (if (null? h)
                    (cons (f a1) '())
                    (error 'map "~s is not a proper list" ls))))
         (if (null? h)
             '()
             (error 'map "~s is not a proper list" ls)))))
  ($pcb-set! map
     (lambda (f ls . ls*)
       (unless (procedure? f)
         (error 'map "not a procedure ~s" f))
       (if (null? ls*)
           (map1 ls ls ls f)
           (error 'map "multiarg not supported yet")))))

(let ()
  (define for-each1
    (lambda (h t ls f)
     (if (pair? h)
         (let ([h ($cdr h)] [a1 ($car h)])
            (if (pair? h)
                (if (not (eq? h t))
                    (let ([a2 ($car h)])
                      (f a1)
                      (f a2)
                      (for-each1 ($cdr h) ($cdr t) ls f))
                    (error 'for-each "circular list ~s" ls))
                (if (null? h)
                    (begin (f a1) (void))
                    (error 'for-each "~s is not a proper list" ls))))
         (if (null? h)
             (void)
             (error 'for-each "~s is not a proper list" ls)))))
  ($pcb-set! for-each
     (lambda (f ls . ls*)
       (unless (procedure? f)
         (error 'for-each "not a procedure ~s" f))
       (if (null? ls*)
           (for-each1 ls ls ls f)
           (error 'for-each "multiarg not supported yet")))))

(let ()
  (define andmap1
    (lambda (a h t ls f)
     (if (pair? h)
         (let ([h ($cdr h)] [a1 ($car h)])
            (if (pair? h)
                (if (not (eq? h t))
                    (let ([a2 ($car h)])
                      (and (f a)
                           (f a1)
                           (andmap1 a2 ($cdr h) ($cdr t) ls f)))
                    (error 'andmap "circular list ~s" ls))
                (if (null? h)
                    (and (f a) (f a1))
                    (error 'andmap "~s is not a proper list" ls))))
         (if (null? h)
             (f a)
             (error 'map "~s is not a proper list" ls)))))
  ($pcb-set! andmap
     (lambda (f ls . ls*)
       (unless (procedure? f)
         (error 'andmap "not a procedure ~s" f))
       (if (null? ls*)
           (if (null? ls)
               #t
               (andmap1 (car ls) (cdr ls) (cdr ls) ls f))
           (error 'andmap "multiarg not supported yet")))))

(let ()
  (define reverse
    (lambda (h t ls ac)
      (if (pair? h)
          (let ([h ($cdr h)] [a1 ($car h)])
             (if (pair? h)
                 (if (not (eq? h t))
                     (let ([a2 ($car h)])
                       (reverse ($cdr h) ($cdr t) ls (cons a2 (cons a1 ac))))
                     (error 'append "circular list ~s" ls))
                 (if (null? h)
                     (cons a1 '())
                     (error 'append "~s is not a proper list" ls))))
          (if (null? h)
              ac 
              (error 'append "~s is not a proper list" ls)))))
  (define revcons
    (lambda (ls ac)
      (cond
        [(pair? ls)
         (revcons ($cdr ls) (cons ($car ls) ac))]
        [else ac])))
  (define append
    (lambda (ls ls*)
      (cond
        [(null? ls*) ls]
        [else 
         (revcons (reverse ls ls ls '())
            (append ($car ls*) ($cdr ls*)))])))
  ($pcb-set! append
    (lambda (ls . ls*)
      (append ls ls*))))


($pcb-set! list->vector
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
         (let ([v ($make-vector n)])
           (fill v 0 ls))))))


(let ()
  (define f
    (lambda (v i ls)
      (cond
        [($fx< i 0) ls]
        [else
         (f v ($fxsub1 i) (cons ($vector-ref v i) ls))])))
  ($pcb-set! vector->list
    (lambda (v)
      (if (vector? v)
          (let ([n ($vector-length v)])
            (if ($fxzero? n)
                '()
                (f v ($fxsub1 n) '())))
          (error 'vector->list "~s is not a vector" v)))))
  
(let ()
  (define f
    (lambda (n fill ls)
      (cond
        [($fxzero? n) ls]
        [else
         (f ($fxsub1 n) fill (cons fill ls))])))
  ($pcb-set! make-list
    (lambda (n . args)
      (let ([fill 
             (if (null? args)
                 (void)
                 (if (null? (cdr args))
                     (car args)
                     (error 'make-list "too many arguments")))])
        (if (fixnum? n)
            (if ($fx>= n 0)
                (f n fill '())
                (error 'make-list "negative size ~s" n))
            (error 'make-list "invalid size ~s" n))))))

($pcb-set! list (lambda x x))
