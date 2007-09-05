
(library (ikarus strings)
  (export string-length string-ref string-set! make-string string->list
          string-append substring string list->string uuid
          string-copy string-for-each string-fill! 
          string=? string<? string<=? string>? string>=?)
  (import 
    (ikarus system $strings)
    (ikarus system $fx)
    (ikarus system $chars)
    (ikarus system $bytevectors)
    (ikarus system $pairs)
    (except (ikarus) string-length string-ref string-set! make-string
            string->list string-append substring string
            list->string uuid string-copy string-for-each
            string=? string<? string<=? string>? string>=?
            string-fill!))


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
    (let ([c ($string-ref s i)])
      (unless (char? c)
        (error 'string-ref "BUG: got a non-char"))
      c))
  

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
           (fill! ($make-string n) 0 n (integer->char 0))]
          [(n c)
           (unless (and (fixnum? n) (fx>= n 0))
             (error 'make-string "~s is not a valid length" n))
           (unless (char? c)
             (error 'make-string "~s is not a character" c))
           (fill! ($make-string n) 0 n c)]))
        make-string))


  (define string
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

  (module (substring)
    (define fill
      (lambda (s d si sj di)
        (cond
          [($fx= si sj) d]
          [else
           ($string-set! d di ($string-ref s si))
           (fill s d ($fxadd1 si) sj ($fxadd1 di))])))
    (define substring
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

  (define string-copy 
    (lambda (s)
      (if (string? s)
          (substring s 0 (string-length s))
          (error 'string-copy "~s is not a string" s))))
  
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


  (define string-cmp
    (lambda (who cmp s1 s*)
      (if (string? s1) 
          (let f ([s1 s1] [s* s*]) 
            (cond
              [(null? s*) #t]
              [else
               (let ([s2 (car s*)])
                 (if (string? s2) 
                     (if (cmp s1 s2) 
                         (f s2 (cdr s*))
                         (let f ([s* (cdr s*)])
                           (cond
                             [(null? s*) #f]
                             [(string? (car s*)) 
                              (f (cdr s*))]
                             [else 
                              (error who "~s is not a string" 
                                (car s*))]))))
                     (error who "~s is not a string" s2))])))
          (error who "~s is not a string" s1)))
  
  (define ($string<? s1 s2)
    (let ([n1 ($string-length s1)]
          [n2 ($string-length s2)])
      (if ($fx< n1 n2)
          (let f ([i 0] [n n1] [s1 s1] [s2 s2])
            (if ($fx= i n)
                #t
                (let ([c1 ($string-ref s1 i)]
                      [c2 ($string-ref s2 i)])
                  (if ($char< c1 c2) 
                      #t
                      (if ($char= c1 c2) 
                          (f ($fxadd1 i) n s1 s2)
                          #f)))))
          (let f ([i 0] [n n2] [s1 s1] [s2 s2])
            (if ($fx= i n)
                #f
                (let ([c1 ($string-ref s1 i)]
                      [c2 ($string-ref s2 i)])
                  (if ($char< c1 c2) 
                      #t
                      (if ($char= c1 c2) 
                          (f ($fxadd1 i) n s1 s2)
                          #f))))))))

  (define ($string<=? s1 s2)
    (let ([n1 ($string-length s1)]
          [n2 ($string-length s2)])
      (if ($fx<= n1 n2)
          (let f ([i 0] [n n1] [s1 s1] [s2 s2])
            (if ($fx= i n)
                #t
                (let ([c1 ($string-ref s1 i)]
                      [c2 ($string-ref s2 i)])
                  (if ($char< c1 c2)
                      #t
                      (if ($char= c1 c2) 
                          (f ($fxadd1 i) n s1 s2)
                          #f)))))
          (let f ([i 0] [n n2] [s1 s1] [s2 s2])
            (if ($fx= i n)
                #f
                (let ([c1 ($string-ref s1 i)]
                      [c2 ($string-ref s2 i)])
                  (if ($char< c1 c2) 
                      #t
                      (if ($char= c1 c2) 
                          (f ($fxadd1 i) n s1 s2)
                          #f))))))))

  (define ($string>? s1 s2) 
    ($string<? s2 s1))
  
  (define ($string>=? s1 s2) 
    ($string<=? s2 s1))

  (define string<?
    (case-lambda
      [(s1 s2) 
       (if (string? s1)
           (if (string? s2) 
               ($string<? s1 s2)
               (error 'string<? "~s is not a string" s2))
           (error 'string<? "~s is not a string" s2))]
      [(s . s*) 
       (string-cmp 'string<? $string<? s s*)]))

  (define string<=?
    (case-lambda
      [(s1 s2) 
       (if (string? s1)
           (if (string? s2) 
               ($string<=? s1 s2)
               (error 'string<=? "~s is not a string" s2))
           (error 'string<=? "~s is not a string" s2))]
      [(s . s*) 
       (string-cmp 'string<=? $string<=? s s*)]))

  (define string>?
    (case-lambda
      [(s1 s2) 
       (if (string? s1)
           (if (string? s2) 
               ($string>? s1 s2)
               (error 'string>? "~s is not a string" s2))
           (error 'string>? "~s is not a string" s2))]
      [(s . s*) 
       (string-cmp 'string>? $string>? s s*)]))

  (define string>=?
    (case-lambda
      [(s1 s2) 
       (if (string? s1)
           (if (string? s2) 
               ($string>=? s1 s2)
               (error 'string>=? "~s is not a string" s2))
           (error 'string>=? "~s is not a string" s2))]
      [(s . s*) 
       (string-cmp 'string>=? $string>=? s s*)]))

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


  (define list->string
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


  (module (string-for-each)
    (define who 'string-for-each)
    (define string-for-each
      (case-lambda
        [(p v) 
         (unless (procedure? p) 
           (error who "~s is not a procedure" p))
         (unless (string? v) 
           (error who "~s is not a string" v))
         (let f ([p p] [v v] [i 0] [n (string-length v)])
           (cond
             [($fx= i n) (void)]
             [else 
              (p (string-ref v i))
              (f p v ($fxadd1 i) n)]))]
        [(p v0 v1) 
         (unless (procedure? p) 
           (error who "~s is not a procedure" p))
         (unless (string? v0) 
           (error who "~s is not a string" v0))
         (unless (string? v1) 
           (error who "~s is not a string" v1))
         (let ([n (string-length v0)])
           (unless ($fx= n ($string-length v1))
             (error who "length mismatch between ~s and ~s" v0 v1))
           (let f ([p p] [v0 v0] [v1 v1] [i 0] [n n])
             (cond
               [($fx= i n) (void)]
               [else 
                (p ($string-ref v0 i) ($string-ref v1 i))
                (f p v0 v1 ($fxadd1 i) n)])))]
        [(p v0 v1 . v*) 
         (unless (procedure? p) 
           (error who "~s is not a procedure" p))
         (unless (string? v0) 
           (error who "~s is not a string" v0))
         (unless (string? v1) 
           (error who "~s is not a string" v1))
         (let ([n (string-length v0)])
           (unless ($fx= n ($string-length v1))
             (error who "length mismatch between ~s and ~s" v0 v1))
           (let f ([v* v*] [n n])
             (unless (null? v*) 
               (let ([a ($car v*)])
                 (unless (string? a) 
                   (error who "~s is not a string" a))
                 (unless ($fx= ($string-length a) n) 
                   (error who "length mismatch")))
               (f ($cdr v*) n)))
           (let f ([p p] [v0 v0] [v1 v1] [v* v*] [i 0] [n n])
             (cond
               [($fx= i n) (void)] 
               [else 
                (apply p ($string-ref v0 i) ($string-ref v1 i)
                  (let f ([i i] [v* v*]) 
                    (if (null? v*) 
                        '()
                        (cons ($string-ref ($car v*) i) 
                              (f i ($cdr v*))))))
                (f p v0 v1 v* ($fxadd1 i) n)])))])))

  (define (string-fill! v fill)
    (unless (string? v) 
      (error 'string-fill! "~s is not a vector" v))
    (unless (char? fill)
      (error 'string-fill! "~s is not a character" fill))
    (let f ([v v] [i 0] [n ($string-length v)] [fill fill])
      (unless ($fx= i n) 
        ($string-set! v i fill)
        (f v ($fxadd1 i) n fill))))

  (define uuid
    (lambda ()
      (let ([s ($make-bytevector 16)])
        (utf8-bytevector->string
          (or (foreign-call "ik_uuid" s)
              (error 'uuid "failed!"))))))
  )
