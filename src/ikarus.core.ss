

(library (ikarus core)
  (export)
  (import (scheme))


(primitive-set! 'eof-object
  (lambda () (eof-object)))

(primitive-set! 'void
  (lambda () (void)))
  
(primitive-set! 'integer->char
  (lambda (n)
    (unless (fixnum? n)
      (error 'integer->char "~s is not a fixnum" n))
    (unless (and ($fx>= n 0)
                 ($fx<= n 255))
      (error 'integer->char "~s is out of range[0..255]" n))
    ($fixnum->char n)))
  
(primitive-set! 'char->integer 
  (lambda (x) 
    (unless (char? x)
      (error 'char->integer "~s is not a character" x))
    ($char->fixnum x)))
  


(primitive-set! 'vector-length
  (lambda (x)
    (unless (vector? x) 
      (error 'vector-length "~s is not a vector" x))
    ($vector-length x)))









(primitive-set! 'not 
  (lambda (x) (if x #f #t)))
  


(primitive-set! 'gensym?
  (lambda (x)
    (and (symbol? x) 
         (let ([s ($symbol-unique-string x)])
           (and s #t)))))


(primitive-set! 'top-level-value
  (lambda (x)
    (unless (symbol? x)
      (error 'top-level-value "~s is not a symbol" x))
    (let ([v ($symbol-value x)])
      (when ($unbound-object? v)
        (error 'top-level-value "unbound variable ~s" x))
      v)))

(primitive-set! 'top-level-bound?
  (lambda (x)
    (unless (symbol? x)
      (error 'top-level-bound? "~s is not a symbol" x))
    (not ($unbound-object? ($symbol-value x)))))

(primitive-set! 'set-top-level-value!
  (lambda (x v)
    (unless (symbol? x)
      (error 'set-top-level-value! "~s is not a symbol" x))
    ($set-symbol-value! x v)))
 
  

(primitive-set! 'primitive-set!
  (lambda (x v)
    (unless (symbol? x)
      (error 'primitive-set! "~s is not a symbol" x))
    (primitive-set! x v)
    (set-top-level-value! x v)))

(primitive-set! 'fx+ 
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx+ "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx+ "~s is not a fixnum" y))
    ($fx+ x y)))


(primitive-set! 'fx-
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx- "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx- "~s is not a fixnum" y))
    ($fx- x y)))


(primitive-set! 'fx*
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx* "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx* "~s is not a fixnum" y))
    ($fx* x y)))
  
(primitive-set! 'fxquotient
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxquotient "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxquotient "~s is not a fixnum" y))
    (when ($fxzero? y)
      (error 'fxquotient "zero dividend ~s" y))
    ($fxquotient x y))) 

(primitive-set! 'fxremainder
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxremainder "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxremainder "~s is not a fixnum" y))
    (when ($fxzero? y)
      (error 'fxremainder "zero dividend ~s" y))
    (let ([q ($fxquotient x y)])
      ($fx- x ($fx* q y)))))
 
(primitive-set! 'fxmodulo
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxmodulo "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxmodulo "~s is not a fixnum" y))
    (when ($fxzero? y)
      (error 'fxmodulo "zero dividend ~s" y))
    ($fxmodulo x y)))

(primitive-set! 'fxlogor
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxlogor "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxlogor "~s is not a fixnum" y))
    ($fxlogor x y)))

(primitive-set! 'fxlogxor
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxlogxor "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxlogxor "~s is not a fixnum" y))
    ($fxlogxor x y)))
  
(primitive-set! 'fxlogand
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxlogand "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxlogand "~s is not a fixnum" y))
    ($fxlogand x y)))
 

(primitive-set! 'fxsra
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxsra "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxsra "~s is not a fixnum" y))
    (unless ($fx>= y 0)
      (error 'fxsra "negative shift not allowed, got ~s" y))
    ($fxsra x y)))
 
(primitive-set! 'fxsll
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fxsll "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fxsll "~s is not a fixnum" y))
    (unless ($fx>= y 0)
      (error 'fxsll "negative shift not allowed, got ~s" y))
    ($fxsll x y))) 

(primitive-set! 'fx=
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx= "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx= "~s is not a fixnum" y))
    ($fx= x y))) 

(primitive-set! 'fx<
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx< "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx< "~s is not a fixnum" y))
    ($fx< x y)))

(primitive-set! 'fx<=
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx<= "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx<= "~s is not a fixnum" y))
    ($fx<= x y)))
 
(primitive-set! 'fx>
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx> "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx> "~s is not a fixnum" y))
    ($fx> x y)))

(primitive-set! 'fx>=
  (lambda (x y) 
    (unless (fixnum? x)
      (error 'fx>= "~s is not a fixnum" x))
    (unless (fixnum? y)
      (error 'fx>= "~s is not a fixnum" y))
    ($fx>= x y)))
  

(primitive-set! 'char=?
  (let ()
    (define (err x)
      (error 'char=? "~s is not a character" x))
    (case-lambda
      [(c1 c2)
       (if (char? c1)
           (if (char? c2)
               ($char= c1 c2)
               (err c2))
           (err c1))]
      [(c1 c2 c3)
       (if (char? c1)
           (if (char? c2)
               (if (char? c3)
                   (and ($char= c1 c2)
                        ($char= c2 c3))
                   (err c3))
               (err c2))
           (err c1))]
      [(c1 . c*)
       (if (char? c1)
           (let f ([c* c*])
             (or (null? c*) 
                 (let ([c2 ($car c*)])
                   (if (char? c2)
                       (if ($char= c1 c2)
                           (f ($cdr c*))
                           (let g ([c* ($cdr c*)])
                             (if (null? c*)
                                 #f
                                 (if (char? ($car c*))
                                     (g ($cdr c*))
                                     (err ($car c*))))))
                       (err c2)))))
           (err c1))])))


(primitive-set! 'char<?
  (let ()
    (define (err x)
      (error 'char<? "~s is not a character" x))
    (case-lambda
      [(c1 c2)
       (if (char? c1)
           (if (char? c2)
               ($char< c1 c2)
               (err c2))
           (err c1))]
      [(c1 c2 c3)
       (if (char? c1)
           (if (char? c2)
               (if (char? c3)
                   (and ($char< c1 c2)
                        ($char< c2 c3))
                   (err c3))
               (err c2))
           (err c1))]
      [(c1 . c*)
       (if (char? c1)
           (let f ([c1 c1] [c* c*])
             (or (null? c*) 
                 (let ([c2 ($car c*)])
                   (if (char? c2)
                       (if ($char< c1 c2)
                           (f c2 ($cdr c*))
                           (let g ([c* ($cdr c*)])
                             (if (null? c*)
                                 #f
                                 (if (char? ($car c*))
                                     (g ($cdr c*))
                                     (err ($car c*))))))
                       (err c2)))))
           (err c1))])))

(primitive-set! 'char<=?
  (let ()
    (define (err x)
      (error 'char<=? "~s is not a character" x))
    (case-lambda
      [(c1 c2)
       (if (char? c1)
           (if (char? c2)
               ($char<= c1 c2)
               (err c2))
           (err c1))]
      [(c1 c2 c3)
       (if (char? c1)
           (if (char? c2)
               (if (char? c3)
                   (and ($char<= c1 c2)
                        ($char<= c2 c3))
                   (err c3))
               (err c2))
           (err c1))]
      [(c1 . c*)
       (if (char? c1)
           (let f ([c1 c1] [c* c*])
             (or (null? c*) 
                 (let ([c2 ($car c*)])
                   (if (char? c2)
                       (if ($char<= c1 c2)
                           (f c2 ($cdr c*))
                           (let g ([c* ($cdr c*)])
                             (if (null? c*)
                                 #f
                                 (if (char? ($car c*))
                                     (g ($cdr c*))
                                     (err ($car c*))))))
                       (err c2)))))
           (err c1))])))

(primitive-set! 'char>?
  (let ()
    (define (err x)
      (error 'char>? "~s is not a character" x))
    (case-lambda
      [(c1 c2)
       (if (char? c1)
           (if (char? c2)
               ($char> c1 c2)
               (err c2))
           (err c1))]
      [(c1 c2 c3)
       (if (char? c1)
           (if (char? c2)
               (if (char? c3)
                   (and ($char> c1 c2)
                        ($char> c2 c3))
                   (err c3))
               (err c2))
           (err c1))]
      [(c1 . c*)
       (if (char? c1)
           (let f ([c1 c1] [c* c*])
             (or (null? c*) 
                 (let ([c2 ($car c*)])
                   (if (char? c2)
                       (if ($char> c1 c2)
                           (f c2 ($cdr c*))
                           (let g ([c* ($cdr c*)])
                             (if (null? c*)
                                 #f
                                 (if (char? ($car c*))
                                     (g ($cdr c*))
                                     (err ($car c*))))))
                       (err c2)))))
           (err c1))])))

(primitive-set! 'char>=?
  (let ()
    (define (err x)
      (error 'char>=? "~s is not a character" x))
    (case-lambda
      [(c1 c2)
       (if (char? c1)
           (if (char? c2)
               ($char>= c1 c2)
               (err c2))
           (err c1))]
      [(c1 c2 c3)
       (if (char? c1)
           (if (char? c2)
               (if (char? c3)
                   (and ($char>= c1 c2)
                        ($char>= c2 c3))
                   (err c3))
               (err c2))
           (err c1))]
      [(c1 . c*)
       (if (char? c1)
           (let f ([c1 c1] [c* c*])
             (or (null? c*) 
                 (let ([c2 ($car c*)])
                   (if (char? c2)
                       (if ($char>= c1 c2)
                           (f c2 ($cdr c*))
                           (let g ([c* ($cdr c*)])
                             (if (null? c*)
                                 #f
                                 (if (char? ($car c*))
                                     (g ($cdr c*))
                                     (err ($car c*))))))
                       (err c2)))))
           (err c1))])))

(primitive-set! '$memq
  (lambda (x ls)
    (let f ([x x] [ls ls])
      (and (pair? ls)
           (if (eq? x (car ls))
               ls
               (f x (cdr ls)))))))

(primitive-set! 'char-whitespace?
  (lambda (c)
    (cond 
      [(memq c '(#\space #\tab #\newline #\return)) #t]
      [(char? c) #f]
      [else
       (error 'char-whitespace? "~s is not a character" c)])))

(primitive-set! 'char-alphabetic?
  (lambda (c)
    (cond
      [(char? c)
       (cond
         [($char<= #\a c) ($char<= c #\z)]
         [($char<= #\A c) ($char<= c #\Z)]
         [else #f])]
      [else 
       (error 'char-alphabetic?  "~s is not a character" c)])))

(primitive-set! 'char-downcase
  (lambda (c)
    (cond
      [(char? c)
       (cond
         [(and ($char<= #\A c) ($char<= c #\Z))
          ($fixnum->char 
            ($fx+ ($char->fixnum c)
                  ($fx- ($char->fixnum #\a)
                        ($char->fixnum #\A))))]
         [else c])]
      [else 
       (error 'char-downcase "~s is not a character" c)])))




(primitive-set! 'cons (lambda (x y) (cons x y)))

(primitive-set! 'eq? (lambda (x y) (eq? x y)))

(primitive-set! 'eqv?
  (lambda (x y)
    (or (eq? x y)
        (and (number? x) (number? y) (= x y)))))

(primitive-set! 'set-car!
  (lambda (x y) 
    (unless (pair? x)
      (error 'set-car! "~s is not a pair" x))
    ($set-car! x y)))

(primitive-set! 'set-cdr! 
  (lambda (x y)
    (unless (pair? x)
      (error 'set-cdr! "~s is not a pair" x))
    ($set-cdr! x y)))

(primitive-set! 'vector-ref 
  (lambda (v i)
    (unless (vector? v)
      (error 'vector-ref "~s is not a vector" v))
    (unless (fixnum? i)
      (error 'vector-ref "~s is not a valid index" i))
    (unless (and ($fx< i ($vector-length v))
                 ($fx<= 0 i))
      (error 'vector-ref "index ~s is out of range for ~s" i v))
    ($vector-ref v i)))


(primitive-set! 'vector-set! 
  (lambda (v i c) 
    (unless (vector? v) 
      (error 'vector-set! "~s is not a vector" v))
    (unless (fixnum? i)
      (error 'vector-set! "~s is not a valid index" i))
    (unless (and ($fx< i ($vector-length v))
                 ($fx<= 0 i))
      (error 'vector-set! "index ~s is out of range for ~s" i v))
    ($vector-set! v i c)))



(primitive-set! 'vector
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

 
(primitive-set! 'list?
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



(primitive-set! 'reverse
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

(primitive-set! 'last-pair
  (letrec ([race
            (lambda (h t ls last)
              (if (pair? h)
                  (let ([h ($cdr h)] [last h])
                     (if (pair? h)
                         (if (not (eq? h t))
                             (race ($cdr h) ($cdr t) ls h)
                             (error 'last-pair "~s is a circular list" ls))
                         last))
                  last))])
     (lambda (x)
       (if (pair? x)
           (let ([d (cdr x)])
             (race d d x x))
           (error 'last-pair "~s is not a pair" x)))))

(primitive-set! 'memq
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

(primitive-set! 'memv
  (letrec ([race
            (lambda (h t ls x)
               (if (pair? h)
                   (if (eqv? ($car h) x)
                       h
                       (let ([h ($cdr h)])
                         (if (pair? h)
                             (if (eqv? ($car h) x)
                                 h
                                 (if (not (eq? h t))
                                     (race ($cdr h) ($cdr t) ls x)
                                     (error 'memv "circular list ~s" ls)))
                             (if (null? h)
                                 '#f
                                 (error 'memv "~s is not a proper list" ls)))))
                   (if (null? h)
                       '#f
                       (error 'memv "~s is not a proper list" ls))))])
     (lambda (x ls)
       (race ls ls ls x))))

(primitive-set! 'member
  (letrec ([race
            (lambda (h t ls x)
               (if (pair? h)
                   (if (equal? ($car h) x)
                       h
                       (let ([h ($cdr h)])
                         (if (pair? h)
                             (if (equal? ($car h) x)
                                 h
                                 (if (not (eq? h t))
                                     (race ($cdr h) ($cdr t) ls x)
                                     (error 'member "circular list ~s" ls)))
                             (if (null? h)
                                 '#f
                                 (error 'member "~s is not a proper list" ls)))))
                   (if (null? h)
                       '#f
                       (error 'member "~s is not a proper list" ls))))])
     (lambda (x ls)
       (race ls ls ls x))))



(primitive-set! 'length
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


(primitive-set! 'list-ref
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




 
(primitive-set! 'assq
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

(primitive-set! 'assv
  (letrec ([race
            (lambda (x h t ls)
              (if (pair? h)
                  (let ([a ($car h)] [h ($cdr h)])
                     (if (pair? a)
                         (if (eqv? ($car a) x)
                             a
                             (if (pair? h)
                                 (if (not (eq? h t))
                                     (let ([a ($car h)])
                                        (if (pair? a)
                                            (if (eqv? ($car a) x)
                                                a
                                                (race x ($cdr h) ($cdr t) ls))
                                            (error 'assv "malformed alist ~s"
                                                   ls)))
                                     (error 'assv "circular list ~s" ls))
                                 (if (null? h)
                                     #f
                                     (error 'assv "~s is not a proper list" ls))))
                         (error 'assv "malformed alist ~s" ls)))
                  (if (null? h)
                      #f
                      (error 'assv "~s is not a proper list" ls))))])
     (lambda (x ls) 
       (race x ls ls ls))))

(primitive-set! 'assoc
  (letrec ([race
            (lambda (x h t ls)
              (if (pair? h)
                  (let ([a ($car h)] [h ($cdr h)])
                     (if (pair? a)
                         (if (equal? ($car a) x)
                             a
                             (if (pair? h)
                                 (if (not (eq? h t))
                                     (let ([a ($car h)])
                                        (if (pair? a)
                                            (if (equal? ($car a) x)
                                                a
                                                (race x ($cdr h) ($cdr t) ls))
                                            (error 'assoc "malformed alist ~s"
                                                   ls)))
                                     (error 'assoc "circular list ~s" ls))
                                 (if (null? h)
                                     #f
                                     (error 'assoc "~s is not a proper list" ls))))
                         (error 'assoc "malformed alist ~s" ls)))
                  (if (null? h)
                      #f
                      (error 'assoc "~s is not a proper list" ls))))])
     (lambda (x ls) 
       (race x ls ls ls))))


(primitive-set! 'string->symbol
  (lambda (x)
    (unless (string? x) 
      (error 'string->symbol "~s is not a string" x))
    (foreign-call "ikrt_string_to_symbol" x)))
  
(primitive-set! 'gensym
  (case-lambda
    [() ($make-symbol #f)]
    [(s) 
     (if (string? s)
         ($make-symbol s)
         (if (symbol? s)
             ($make-symbol ($symbol-string s))
             (error 'gensym "~s is neither a string nor a symbol" s)))]))

(primitive-set! 'putprop
  (lambda (x k v)
    (unless (symbol? x) (error 'putprop "~s is not a symbol" x))
    (unless (symbol? k) (error 'putprop "~s is not a symbol" k))
    (let ([p ($symbol-plist x)])
      (cond
        [(assq k p) => (lambda (x) (set-cdr! x v))]
        [else 
         ($set-symbol-plist! x (cons (cons k v) p))]))))

(primitive-set! 'getprop
  (lambda (x k)
    (unless (symbol? x) (error 'getprop "~s is not a symbol" x))
    (unless (symbol? k) (error 'getprop "~s is not a symbol" k))
    (let ([p ($symbol-plist x)])
      (cond
        [(assq k p) => cdr]
        [else #f]))))

(primitive-set! 'remprop
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

(primitive-set! 'property-list
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
         [(number? x) (and (number? y) (= x y))]
         [else #f])))
   (primitive-set! 'equal? equal?))




(primitive-set! 'apply
  (let ()
    (define (err f ls)
      (if (procedure? f)
          (error 'apply "not a list")
          (error 'apply "~s is not a procedure" f)))
    (define (fixandgo f a0 a1 ls p d)
      (cond
        [(null? ($cdr d))
         (let ([last ($car d)])
           ($set-cdr! p last)
           (if (and (procedure? f) (list? last))
               ($$apply f a0 a1 ls)
               (err f last)))]
        [else (fixandgo f a0 a1 ls d ($cdr d))]))
    (define apply
      (case-lambda
        [(f ls) 
         (if (and (procedure? f) (list? ls))
             ($$apply f ls)
             (err f ls))]
        [(f a0 ls)
         (if (and (procedure? f) (list? ls))
             ($$apply f a0 ls)
             (err f ls))]
        [(f a0 a1 ls)
         (if (and (procedure? f) (list? ls))
             ($$apply f a0 a1 ls)
             (err f ls))]
        [(f a0 a1 . ls)
         (fixandgo f a0 a1 ls ls ($cdr ls))]))
    apply))

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
  (define cars
    (lambda (ls*)
      (cond
        [(null? ls*) '()]
        [else
         (let ([a (car ls*)])
           (cond
             [(pair? a) 
              (cons (car a) (cars (cdr ls*)))]
             [else 
              (error 'map "length mismatch")]))])))
  (define cdrs
    (lambda (ls*)
      (cond
        [(null? ls*) '()]
        [else
         (let ([a (car ls*)])
           (cond
             [(pair? a) 
              (cons (cdr a) (cdrs (cdr ls*)))]
             [else 
              (error 'map "length mismatch")]))])))
  (define mapm
    (lambda (f ls ls* n)
      (cond
        [(null? ls)
         (if (andmap null? ls*)
             (if (fxzero? n)
                 '()
                 (error 'map "lists were mutated during operation"))
             (error 'map "length mismatch"))]
        [(fxzero? n)
         (error 'map "lists were mutated during operation")]
        [else
         (cons
           (apply f (car ls) (cars ls*))
           (mapm f (cdr ls) (cdrs ls*) (fxsub1 n)))])))
  (primitive-set! 'map
     (case-lambda
       [(f ls) 
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
        (cond
          [(pair? ls)
           (let ([d ($cdr ls)])
             (map1 f ($car ls) d (len d d 0)))]
          [(null? ls) '()]
          [else (error who "improper list")])]
       [(f ls ls2)
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
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
          [else (error who "not a list")])]
       [(f ls . ls*)
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
        (cond
          [(pair? ls)
           (let ([n (len ls ls 0)])
             (mapm f ls ls* n))]
          [(null? ls)
           (if (andmap null? ls*)
               '()
               (error who "length mismatch"))])])))


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
  (primitive-set! 'for-each
     (case-lambda
       [(f ls)
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
        (cond
          [(pair? ls)
           (let ([d ($cdr ls)])
             (for-each1 f ($car ls) d (len d d 0)))]
          [(null? ls) (void)]
          [else (error who "improper list")])]
       [(f ls ls2)
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
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
          [else (error who "not a list")])]
       [_ (error who "vararg not supported yet")])))



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
  (define andmap2
    (lambda (f a1 a2 d1 d2 n)
      (cond
        [(pair? d1)
         (cond
           [(pair? d2)
            (if ($fxzero? n)
                (error who "list was altered")
                (and
                  (f a1 a2) 
                  (andmap2 f
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
  (primitive-set! 'andmap
     (case-lambda
       [(f ls)
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
        (cond
          [(pair? ls)
           (let ([d ($cdr ls)])
             (andmap1 f ($car ls) d (len d d 0)))]
          [(null? ls) #t]
          [else (error who "improper list")])]
       [(f ls ls2)
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
        (cond
          [(pair? ls)
           (if (pair? ls2)
               (let ([d ($cdr ls)])
                 (andmap2 f
                    ($car ls) ($car ls2) d ($cdr ls2) (len d d 0)))
               (error who "length mismatch"))]
          [(null? ls)
           (if (null? ls2)
               #t
               (error who "length mismatch"))]
          [else (error who "not a list")])]
       [(f . ls*) (error who "vararg not supported yet in ~s" 
                         (length ls*))])))


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
  (primitive-set! 'ormap
     (case-lambda
       [(f ls)
        (unless (procedure? f)
          (error who "~s is not a procedure" f))
        (cond
          [(pair? ls)
           (let ([d ($cdr ls)])
             (ormap1 f ($car ls) d (len d d 0)))]
          [(null? ls) #f]
          [else (error who "improper list")])]
       [_ (error who "vararg not supported yet")])))




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
  (primitive-set! 'append
    (case-lambda
      [() '()]
      [(ls) ls]
      [(ls . ls*)
       (append ls ls*)])))




(primitive-set! 'list->vector
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


(let ()
  (define f
    (lambda (v i ls)
      (cond
        [($fx< i 0) ls]
        [else
         (f v ($fxsub1 i) (cons ($vector-ref v i) ls))])))
  (primitive-set! 'vector->list
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
  (primitive-set! 'make-list
    (case-lambda
      [(n)
       (if (and (fixnum? n) ($fx>= n 0))
           (f n (void) '())
           (error 'make-list "~s is not a valid length" n))]
      [(n fill)
       (if (and (fixnum? n) ($fx>= n 0))
           (f n fill '())
           (error 'make-list "~s is not a valid length" n))])))

(primitive-set! 'list (lambda x x))


(primitive-set! 'gensym->unique-string
  (lambda (x)
    (unless (symbol? x)
      (error 'gensym->unique-string "~s is not a gensym" x))
    (let ([us ($symbol-unique-string x)])
      (cond
        [(string? us) us]
        [(not us)
         (error 'gensym->unique-string "~s is not a gensym" x)]
        [else
         (let f ([x x])
           (let ([id (uuid)])
             ($set-symbol-unique-string! x id)
             (cond
               [(foreign-call "ikrt_intern_gensym" x) id]
               [else (f x)])))]))))




(primitive-set! 'gensym-prefix
  (make-parameter
    "g"
    (lambda (x)
      (unless (string? x)
        (error 'gensym-prefix "~s is not a string" x))
      x)))

(primitive-set! 'gensym-count
  (make-parameter
    0
    (lambda (x)
      (unless (and (fixnum? x) ($fx>= x 0))
        (error 'gensym-count "~s is not a valid count" x))
      x)))

(primitive-set! 'print-gensym
  (make-parameter
    #t
    (lambda (x)
      (unless (or (boolean? x) (eq? x 'pretty))
        (error 'print-gensym "~s is not in #t|#f|pretty" x))
      x)))


(primitive-set! 'bwp-object?
  (lambda (x)
    (bwp-object? x)))

(primitive-set! 'weak-cons
  (lambda (a d)
    (foreign-call "ikrt_weak_cons" a d)))

(primitive-set! 'weak-pair?
  (lambda (x)
    (and (pair? x)
         (foreign-call "ikrt_is_weak_pair" x))))

(primitive-set! 'pointer-value
  (lambda (x)
    (pointer-value x)))

(primitive-set! 'date-string
  (lambda ()
    (let ([s (make-string 10)])
      (foreign-call "ikrt_strftime" s "%F")
      s)))

(primitive-set! 'list*
  (lambda (fst . rest)
    (let f ([fst fst] [rest rest])
      (cond
        [(null? rest) fst]
        [else 
         (cons fst (f ($car rest) ($cdr rest)))]))))

(primitive-set! 'command-line-arguments
  (make-parameter ($arg-list)
    (lambda (x)
      (if (and (list? x) (andmap string? x))
          x
          (error 'command-list "invalid command-line-arguments ~s\n" x)))))



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
  (primitive-set! 'fixnum->string
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



(primitive-set! 'symbol->string
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


(primitive-set! 'string->number
  (lambda (x)
    (define (convert-data str len pos? idx ac)
      (cond
        [($fx= idx len) (if pos? ac (- 0 ac))]
        [else
         (let ([c ($string-ref str idx)])
           (cond
             [(and ($char<= #\0 c) ($char<= c #\9))
              (convert-data str len pos? ($fxadd1 idx) 
                 (+ (* ac 10)
                    ($fx- ($char->fixnum c) ($char->fixnum #\0))))]
             [else #f]))]))
    (define (convert-data-init str len pos? idx c)
      (cond
        [($char= c #\0) 
         (if ($fx= idx len)
             0
             (convert-data-init str len pos? 
                ($fxadd1 idx) 
                ($string-ref str idx)))]
        [(and ($char<= #\1 c) ($char<= c #\9))
         (convert-data str len pos? idx
            ($fx- ($char->fixnum c) ($char->fixnum #\0)))]
        [else #f]))
    (define (convert-num str len pos?)
      (cond
        [($fx> len 1)
         (convert-data-init str len pos? 2 ($string-ref str 1))]
        [else #f]))
    (define (convert-sign str len)
      (cond
        [($fx> len 0)
         (let ([c ($string-ref str 0)])
           (case c
             [(#\+) (convert-num str len #t)]
             [(#\-) (convert-num str len #f)]
             [else
              (convert-data-init str len #t 1 c)]))]
        [else #f]))
    (cond
      [(string? x) 
       (convert-sign x ($string-length x))]
      [else (error 'string->number "~s is not a string" x)])))

)
