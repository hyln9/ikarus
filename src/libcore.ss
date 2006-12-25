
;;; 6.9: * removed uuid
;;;      * top-level-value is now open-coded.
;;;
;;; 6.2: * added bwp-object?, weak-cons, weak-pair?
;;;      * pointer-value
;;; 6.1: * added uses of case-lambda to replace the ugly code 
;;; 6.0: * basic version working


(primitive-set! 'call-with-values 
  ($make-call-with-values-procedure))

(primitive-set! 'values 
  ($make-values-procedure))

(primitive-set! 'exit
  (case-lambda
    [() (exit 0)]
    [(status) (foreign-call "exit" status)]))

(primitive-set! 'eof-object
  (lambda () (eof-object)))

(primitive-set! 'void
  (lambda () (void)))
  
(primitive-set! 'eof-object?
  (lambda (x) (eof-object? x)))

(primitive-set! 'fxadd1
  (lambda (n)
    (fxadd1 n)))
  
(primitive-set! 'fxsub1 
  (lambda (n) 
    (fxsub1 n)))
  
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
  
(primitive-set! 'fxlognot 
  (lambda (x)
    (unless (fixnum? x) 
      (error 'fxlognot "~s is not a fixnum" x))
    ($fxlognot x)))
  
(primitive-set! 'fixnum? (lambda (x) (fixnum? x)))
(primitive-set! 'immediate? (lambda (x) (immediate? x)))

(primitive-set! 'fxzero? 
  (lambda (x)
    (unless (fixnum? x)
      (error 'fxzero? "~s is not a fixnum" x))
    ($fxzero? x)))

(primitive-set! 'boolean? (lambda (x) (boolean? x)))
  
(primitive-set! 'char? (lambda (x) (char? x)))

(primitive-set! 'vector? (lambda (x) (vector? x)))

(primitive-set! 'string? (lambda (x) (string? x)))

(primitive-set! 'procedure? (lambda (x) (procedure? x)))

(primitive-set! 'null? (lambda (x) (null? x)))

(primitive-set! 'pair? (lambda (x) (pair? x)))

(let ()
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
       (unless (and (fixnum? n) (fx>= n 0))
         (error 'make-vector "~s is not a valid length" n))
       (fill! ($make-vector n) 0 n fill)]))
  (primitive-set! 'make-vector make-vector))

(primitive-set! 'vector-length
  (lambda (x)
    (unless (vector? x) 
      (error 'vector-length "~s is not a vector" x))
    ($vector-length x)))

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
  (primitive-set! 'make-string make-string))


(primitive-set! 'string-length
  (lambda (x)
    (unless (string? x)
      (error 'string-length "~s is not a string" x))
    ($string-length x)))

(primitive-set! 'string->list
  (lambda (x)
    (unless (string? x)
      (error 'string->list "~s is not a string" x))
    (let f ([x x] [i ($string-length x)] [ac '()])
      (cond
        [($fxzero? i) ac]
        [else
         (let ([i ($fxsub1 i)])
           (f x i (cons ($string-ref x i) ac)))]))))

#|procedure:string=?
synopsis:
  (string=? s s* ...)
description:
  string=? takes 1 or more strings and returns #t if all strings are
  equal.  Two strings s1 and s2 are string=? if they have the same 
  length and if (char=? (string-ref s1 i) (string-ref s2 i)) for all
  0 <= i < (string-length s1)
|#
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
  (primitive-set! 'string=?
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

#|procedure:string-append
synopsis:
  (string-append str ...)
description:  
  Takes 0 or more strings and returns a new string that results from
  appending the contents of the strings together.
reference-implementation:
  (define (string-append . s*)
    (list->string (apply append (map string->list s*))))
|#
(let ()
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
  (primitive-set! 'string-append
    (lambda s*
      (let ([n (length* s* 0)])
        (let ([s ($make-string n)])
          (fill-strings s s* 0))))))

#|procedure:substring
  (substring str i j)
  Returns a substring of str starting from index i (inclusive)
  and ending with index j (exclusive).|#
(let ()
  (define fill
    (lambda (s d si sj di)
      (cond
        [($fx= si sj) d]
        [else
         ($string-set! d di ($string-ref s si))
         (fill s d ($fxadd1 si) sj ($fxadd1 di))])))
  (primitive-set! 'substring
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

(primitive-set! 'not (lambda (x) (not x)))
  
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

(primitive-set! 'gensym?
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

;;; OLD (primitive-set! 'top-level-value
;;; OLD   (lambda (x)
;;; OLD     (unless (symbol? x)
;;; OLD       (error 'top-level-value "~s is not a symbol" x))
;;; OLD     (let ([v ($symbol-value x)])
;;; OLD       (when ($unbound-object? v)
;;; OLD         (error 'top-level-value "unbound variable ~s" x))
;;; OLD       v)))

(primitive-set! 'top-level-value
  (lambda (x)
    (top-level-value x)))

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
 
(primitive-set! 'symbol? (lambda (x) (symbol? x)))
  
(primitive-set! 'primitive?
  (lambda (x)
    (unless (symbol? x)
      (error 'primitive? "~s is not a symbol" x))
    (procedure? (primitive-ref x))))

(primitive-set! 'primitive-ref
  (lambda (x)
    (unless (symbol? x)
      (error 'primitive-ref "~s is not a symbol" x))
    (let ([v (primitive-ref x)])
      (unless (procedure? v)
        (error 'primitive-ref "~s is not a primitive" x))
      v)))

(primitive-set! 'primitive-set!
  (lambda (x v)
    (unless (symbol? x)
      (error 'primitive-set! "~s is not a symbol" x))
    (primitive-set! x v)
    (set-top-level-value! x v)))





(primitive-set! 'fx+ 
  (lambda (x y) 
    (fx+ x y)))

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

(primitive-set! 'string-ref 
  (lambda (s i) 
    (unless (string? s) 
      (error 'string-ref "~s is not a string" s))
    (unless (fixnum? i)
      (error 'string-ref "~s is not a valid index" i))
    (unless (and ($fx< i ($string-length s))
                 ($fx<= 0 i))
      (error 'string-ref "index ~s is out of range for ~s" i s))
    ($string-ref s i)))

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


(primitive-set! 'string-set! 
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

(primitive-set! 'string
  ;;; FIXME: add case-lambda
  (letrec ([length
            (lambda (ls n)
              (cond
               [(null? ls) n]
               [(char? ($car ls)) (length ($cdr ls) ($fx+ n 1))]
               [else (error 'string "~s is not a character" ($car ls))]))]
           [loop 
            (lambda (s ls i n)
              (cond
               [($fx= i n) s]
               [else 
                ($string-set! s i ($car ls))
                (loop s ($cdr ls) ($fx+ i 1) n)]))])
     (lambda ls
       (let ([n (length ls 0)])
         (let ([s (make-string n)])
           (loop s ls 0 n))))))
 
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


(primitive-set! 'list->string
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
               ($apply f a0 a1 ls)
               (err f last)))]
        [else (fixandgo f a0 a1 ls d ($cdr d))]))
    (define apply
      (case-lambda
        [(f ls) 
         (if (and (procedure? f) (list? ls))
             ($apply f ls)
             (err f ls))]
        [(f a0 ls)
         (if (and (procedure? f) (list? ls))
             ($apply f a0 ls)
             (err f ls))]
        [(f a0 a1 ls)
         (if (and (procedure? f) (list? ls))
             ($apply f a0 a1 ls)
             (err f ls))]
        [(f a0 a1 . ls)
         (fixandgo f a0 a1 ls ls ($cdr ls))]))
    apply))


             

   
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
       [_ (error who "vararg not supported yet")])))


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
    (lambda (ls . ls*)
      (append ls ls*))))


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

(primitive-set! 'uuid
   (lambda ()
     (let ([s (make-string 16)])
       (or (foreign-call "ik_uuid" s)
           (error 'uuid "failed!")))))

(primitive-set! 'gensym->unique-string
  (lambda (x)
    (unless (symbol? x)
      (error 'gensym->unique-string "~s is not a gensym" x))
    (let ([us ($symbol-unique-string x)])
      (cond
        [(string? us) us]
        [(eq? us #t) 
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
      (unless (boolean? x) 
        (error 'print-gensym "~s is not a boolean" x))
      x)))

;; X (primitive-set! 'make-hash-table
;; X   (lambda ()
;; X     (make-hash-table)))
;; X 
;; X (primitive-set! 'hash-table?
;; X   (lambda (x)
;; X     (hash-table? x)))
;; X 
;; X (primitive-set! 'get-hash-table
;; X   (lambda (h k v)
;; X     (foreign-call "ik_get_hash_table" h k v)))
;; X 
;; X (primitive-set! 'put-hash-table!
;; X   (lambda (h k v)
;; X     (foreign-call "ik_put_hash_table" h k v)))

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

(primitive-set! 'features
  (lambda ()
    (append (macros) (public-primitives) '())))

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

