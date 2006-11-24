
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
  
($pcb-set! integer->char
  (lambda (n)
    (unless (fixnum? n)
      (error 'integer->char "~s is not a fixnum" n))
    (unless (and ($fx>= n 0)
                 ($fx<= n 127))
      (error 'integer->char "~s is out of range[0..127]" n))
    ($fixnum->char n)))
  
($pcb-set! char->integer 
  (lambda (x) 
    (unless (char? x)
      (error 'char->integer "~s is not a character" x))
    ($char->fixnum x)))
  
($pcb-set! fxlognot 
  (lambda (x)
    (unless (fixnum? x) 
      (error 'fxlognot "~s is not a fixnum" x))
    ($fxlognot x)))
  
($pcb-set! fixnum? (lambda (x) (fixnum? x)))
($pcb-set! immediate? (lambda (x) (immediate? x)))

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

($pcb-set! string->list
  (lambda (x)
    (unless (string? x)
      (error 'string->list "~s is not a string" x))
    (let f ([x x] [i ($string-length x)] [ac '()])
      (cond
        [($fxzero? i) ac]
        [else
         (let ([i ($fxsub1 i)])
           (f x i (cons ($string-ref x i) ac)))]))))

(let ()
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
        [else (error 'string=? "~s is not a string" ($car s*))])))
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
  ($pcb-set! string=?
    (lambda (s . s*)
      (if (string? s)
          (strings=? s s* ($string-length s))
          (error 'string=? "~s is not a string" s)))))

(let ()
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
  ($pcb-set! string-append
     (lambda s*
       (let ([n (length* s* 0)])
         (let ([s ($make-string n)])
           (fill-strings s s* 0))))))


(let ()
  (define fill
    (lambda (s d si sj di)
      (cond
        [($fx= si sj) d]
        [else
         ($string-set! d di ($string-ref s si))
         (fill s d ($fxadd1 si) sj ($fxadd1 di))])))
  ($pcb-set! substring
     (lambda (s n m)
       (unless (string? s)
         (error 'substring "~s is not a string" s))
       (let ([len ($string-length s)])
         (unless (and (fixnum? n)
                      ($fx>= n 0)
                      ($fx< n len))
           (error 'substring "~s is not a valid start index for ~s" n s))
         (unless (and (fixnum? m)
                      ($fx>= m 0)
                      ($fx<= m len))
           (error 'substring "~s is not a valid end index for ~s" m s))
         (let ([len ($fx- m n)])
           (if ($fx<= len 0)
               ""
               (fill s ($make-string len) n m 0)))))))

($pcb-set! not (lambda (x) (not x)))
  
($pcb-set! symbol->string
  (lambda (x)
    (unless (symbol? x)
      (error 'symbol->string "~s is not a symbol" x))
    (let ([str ($symbol-string x)])
      (or str
          (let ([ct (gensym-count)])
            (let ([str (string-append (gensym-prefix) (fixnum->string ct))])
              ($set-symbol-string! x str)
              (gensym-count ($fxadd1 ct))
              str))))))

($pcb-set! gensym?
  (lambda (x)
    (and (symbol? x) 
         (let ([s ($symbol-unique-string x)])
           (and s #t)))))

(let ()
  (define f
    (lambda (n i j)
      (cond
        [($fxzero? n) 
         (values (make-string i) j)]
        [else
         (let ([q ($fxquotient n 10)])
           (call-with-values
             (lambda () (f q ($fxadd1 i) j))
             (lambda (str j)
               (let ([r ($fx- n ($fx* q 10))])
                 (string-set! str j
                    ($fixnum->char ($fx+ r ($char->fixnum #\0))))
                 (values str ($fxadd1 j))))))])))
  ($pcb-set! fixnum->string
    (lambda (x)
      (unless (fixnum? x) (error 'fixnum->string "~s is not a fixnum" x))
      (cond
        [($fxzero? x) "0"]
        [($fx> x 0) 
         (call-with-values
           (lambda () (f x 0 0))
           (lambda (str j) str))]
        [($fx= x -536870912) "-536870912"]
        [else
         (call-with-values
           (lambda () (f ($fx- 0 x) 1 1))
           (lambda (str j)
             ($string-set! str 0 #\-)
             str))]))))

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
 

($pcb-set! fxmodulo
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxmodulo "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxmodulo "~s is not a fixnum" y))
    (when ($fxzero? y)
      (error 'fxmodulo "zero dividend ~s" y))
    ($fxmodulo x y)))


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


($pcb-set! list-ref
  (lambda (list index)
    (define f
      (lambda (ls i)
        (cond
          [($fxzero? i) 
           (if (pair? ls)
               ($car ls)
               (error 'list-ref "index ~s is out of range for ~s" index list))]
          [(pair? ls)
           (f ($cdr ls) ($fxsub1 i))]
          [(null? ls) 
           (error 'list-rec "index ~s is out of range for ~s" index list)]
          [else (error 'list-ref "~s is not a list" list)])))
    (unless (and (fixnum? index) ($fx>= index 0))
      (error 'list-ref "~s is not a valid index" index))
    (f list index)))

  

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
        ($make-symbol #f)
        (if (null? ($cdr args))
            (let ([a ($car args)])
               (if (string? a)
                   ($make-symbol a)
                   (error 'gensym "~s is not a string" a)))
            (error 'gensym "too many arguments")))))

($pcb-set! putprop
  (lambda (x k v)
    (unless (symbol? x) (error 'putprop "~s is not a symbol" x))
    (unless (symbol? k) (error 'putprop "~s is not a symbol" k))
    (let ([p ($symbol-plist x)])
      (cond
        [(assq k p) => (lambda (x) (set-cdr! x v))]
        [else 
         ($set-symbol-plist! x (cons (cons k v) p))]))))

($pcb-set! getprop
  (lambda (x k)
    (unless (symbol? x) (error 'getprop "~s is not a symbol" x))
    (unless (symbol? k) (error 'getprop "~s is not a symbol" k))
    (let ([p ($symbol-plist x)])
      (cond
        [(assq k p) => cdr]
        [else #f]))))

($pcb-set! remprop
  (lambda (x k)
    (unless (symbol? x) (error 'remprop "~s is not a symbol" x))
    (unless (symbol? k) (error 'remprop "~s is not a symbol" k))
    (let ([p ($symbol-plist x)])
      (unless (null? p)
        (let ([a ($car p)])
          (cond
            [(eq? ($car a) k) ($set-symbol-plist! x ($cdr p))]
            [else 
             (let f ([q p] [p ($cdr p)])
               (unless (null? p)
                 (let ([a ($car p)])
                   (cond
                     [(eq? ($car a) k)
                      ($set-cdr! q ($cdr p))]
                     [else 
                      (f p ($cdr p))]))))]))))))

($pcb-set! property-list
  (lambda (x)
    (unless (symbol? x)
      (error 'property-list "~s is not a symbol" x))
    (letrec ([f 
              (lambda (ls ac)
                (cond
                  [(null? ls) ac]
                  [else
                   (let ([a ($car ls)])
                     (f ($cdr ls) 
                        (cons ($car a) (cons ($cdr a) ac))))]))])
      (f ($symbol-plist x) '()))))


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
  (define who 'map)
  (define len
    (lambda (h t n)
      (if (pair? h)
          (let ([h ($cdr h)])
            (if (pair? h)
                (if (eq? h t)
                    (error who "circular list")
                    (len ($cdr h) ($cdr t) ($fx+ n 2)))
                (if (null? h)
                    ($fxadd1 n)
                    (error who "improper list"))))
          (if (null? h)
              n
              (error who "improper list")))))

  (define map1
    (lambda (f a d n)
      (cond
        [(pair? d)
         (if ($fxzero? n)
             (error who "list was altered!")
             (cons (f a)
                   (map1 f ($car d) ($cdr d) ($fxsub1 n))))]
        [(null? d)
         (if ($fxzero? n)
             (cons (f a) '())
             (error who "list was altered"))]
        [else (error who "list was altered")])))

  (define map2
    (lambda (f a1 a2 d1 d2 n)
      (cond
        [(pair? d1)
         (cond
           [(pair? d2)
            (if ($fxzero? n)
                (error who "list was altered")
                (cons (f a1 a2) 
                      (map2 f
                            ($car d1) ($car d2)
                            ($cdr d1) ($cdr d2)
                            ($fxsub1 n))))]
           [else (error who "length mismatch")])]
        [(null? d1)
         (cond
           [(null? d2)
            (if ($fxzero? n)
                (cons (f a1 a2) '())
                (error who "list was altered"))]
           [else (error who "length mismatch")])]
        [else (error who "list was altered")])))

  ($pcb-set! map
     (lambda (f ls . ls*)
       (unless (procedure? f)
         (error who "~s is not a procedure" f))
       (cond
         [(null? ls*)
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (map1 f ($car ls) d (len d d 0)))]
            [(null? ls) '()]
            [else (error who "improper list")])]
         [(null? ($cdr ls*))
          (let ([ls2 ($car ls*)])
            (cond
              [(pair? ls)
               (if (pair? ls2)
                   (let ([d ($cdr ls)])
                     (map2 f ($car ls) ($car ls2) d ($cdr ls2) (len d d 0)))
                   (error who "length mismatch"))]
              [(null? ls)
               (if (null? ls2)
                   '()
                   (error who "length mismatch"))]
              [else (error who "not a list")]))]
         [else (error who "vararg not supported yet")]))))

(let ()
  (define who 'for-each)
  (define len
    (lambda (h t n)
      (if (pair? h)
          (let ([h ($cdr h)])
            (if (pair? h)
                (if (eq? h t)
                    (error who "circular list")
                    (len ($cdr h) ($cdr t) ($fx+ n 2)))
                (if (null? h)
                    ($fxadd1 n)
                    (error who "improper list"))))
          (if (null? h)
              n
              (error who "improper list")))))

  (define for-each1
    (lambda (f a d n)
      (cond
        [(pair? d)
         (if ($fxzero? n)
             (error who "list was altered!")
             (begin 
               (f a)
               (for-each1 f ($car d) ($cdr d) ($fxsub1 n))))]
        [(null? d)
         (if ($fxzero? n)
             (f a)
             (error who "list was altered"))]
        [else (error who "list was altered")])))

  (define for-each2
    (lambda (f a1 a2 d1 d2 n)
      (cond
        [(pair? d1)
         (cond
           [(pair? d2)
            (if ($fxzero? n)
                (error who "list was altered")
                (begin
                  (f a1 a2) 
                  (for-each2 f
                    ($car d1) ($car d2)
                    ($cdr d1) ($cdr d2)
                    ($fxsub1 n))))]
           [else (error who "length mismatch")])]
        [(null? d1)
         (cond
           [(null? d2)
            (if ($fxzero? n)
                (f a1 a2)
                (error who "list was altered"))]
           [else (error who "length mismatch")])]
        [else (error who "list was altered")])))

  ($pcb-set! for-each
     (lambda (f ls . ls*)
       (unless (procedure? f)
         (error who "~s is not a procedure" f))
       (cond
         [(null? ls*)
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (for-each1 f ($car ls) d (len d d 0)))]
            [(null? ls) (void)]
            [else (error who "improper list")])]
         [(null? ($cdr ls*))
          (let ([ls2 ($car ls*)])
            (cond
              [(pair? ls)
               (if (pair? ls2)
                   (let ([d ($cdr ls)])
                     (for-each2 f
                        ($car ls) ($car ls2) d ($cdr ls2) (len d d 0)))
                   (error who "length mismatch"))]
              [(null? ls)
               (if (null? ls2)
                   (void)
                   (error who "length mismatch"))]
              [else (error who "not a list")]))]
         [else (error who "vararg not supported yet")]))))



(let ()
  (define who 'andmap)
  (define len
    (lambda (h t n)
      (if (pair? h)
          (let ([h ($cdr h)])
            (if (pair? h)
                (if (eq? h t)
                    (error who "circular list")
                    (len ($cdr h) ($cdr t) ($fx+ n 2)))
                (if (null? h)
                    ($fxadd1 n)
                    (error who "improper list"))))
          (if (null? h)
              n
              (error who "improper list")))))

  (define andmap1
    (lambda (f a d n)
      (cond
        [(pair? d)
         (if ($fxzero? n)
             (error who "list was altered!")
             (and (f a)
                  (andmap1 f ($car d) ($cdr d) ($fxsub1 n))))]
        [(null? d)
         (if ($fxzero? n)
             (f a)
             (error who "list was altered"))]
        [else (error who "list was altered")])))

  ($pcb-set! andmap
     (lambda (f ls . ls*)
       (unless (procedure? f)
         (error who "~s is not a procedure" f))
       (cond
         [(null? ls*)
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (andmap1 f ($car ls) d (len d d 0)))]
            [(null? ls) #t]
            [else (error who "improper list")])]
         [else (error who "vararg not supported yet")]))))


(let ()
  (define who 'ormap)
  (define len
    (lambda (h t n)
      (if (pair? h)
          (let ([h ($cdr h)])
            (if (pair? h)
                (if (eq? h t)
                    (error who "circular list")
                    (len ($cdr h) ($cdr t) ($fx+ n 2)))
                (if (null? h)
                    ($fxadd1 n)
                    (error who "improper list"))))
          (if (null? h)
              n
              (error who "improper list")))))

  (define ormap1
    (lambda (f a d n)
      (cond
        [(pair? d)
         (if ($fxzero? n)
             (error who "list was altered!")
             (or (f a)
                 (ormap1 f ($car d) ($cdr d) ($fxsub1 n))))]
        [(null? d)
         (if ($fxzero? n)
             (f a)
             (error who "list was altered"))]
        [else (error who "list was altered")])))

  ($pcb-set! ormap
     (lambda (f ls . ls*)
       (unless (procedure? f)
         (error who "~s is not a procedure" f))
       (cond
         [(null? ls*)
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (ormap1 f ($car ls) d (len d d 0)))]
            [(null? ls) #f]
            [else (error who "improper list")])]
         [else (error who "vararg not supported yet")]))))




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
                     (cons a1 ac)
                     (error 'append "~s is not a proper list" ls))))
          (if (null? h)
              ac 
              (error 'append "~s is not a proper list" ls)))))
  (define revcons
    (lambda (ls ac)
      (cond
        [(null? ls) ac]
        [else
         (revcons ($cdr ls) (cons ($car ls) ac))])))
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

($pcb-set! uuid
   (lambda ()
     (let ([s (make-string 36)])
       (foreign-call "S_uuid" s))))

($pcb-set! gensym->unique-string
  (lambda (x)
    (unless (symbol? x)
      (error 'gensym->unique-string "~s is not a gensym" x))
    (let ([us ($symbol-unique-string x)])
      (cond
        [(string? us) us]
        [(eq? us #t) 
         (error 'gensym->unique-string "~s is not a gensym" x)]
        [else
         (let ([id (uuid)])
           ($set-symbol-unique-string! x id)
           id)]))))

($pcb-set! gensym-prefix
  (make-parameter
    "g"
    (lambda (x)
      (unless (string? x)
        (error 'gensym-prefix "~s is not a string" x))
      x)))

($pcb-set! gensym-count
  (make-parameter
    0
    (lambda (x)
      (unless (and (fixnum? x) ($fx>= x 0))
        (error 'gensym-count "~s is not a valid count" x))
      x)))

($pcb-set! print-gensym
  (make-parameter
    #t
    (lambda (x)
      (unless (boolean? x) 
        (error 'print-gensym "~s is not a boolean" x))
      x)))


