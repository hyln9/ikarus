
(library (ikarus lists)
  (export $memq list? length list-ref reverse last-pair 
          memq memv member assq assv assoc
          map for-each andmap ormap)
  (import 
    (only (scheme) $car $cdr $fx+ $fxadd1 $fxsub1 $fxzero? $fx>=)
    (except (ikarus) list? reverse last-pair length list-ref
            memq memv member assq assv assoc
            map for-each andmap ormap))

  (define $memq
    (lambda (x ls)
      (let f ([x x] [ls ls])
        (and (pair? ls)
             (if (eq? x (car ls))
                 ls
                 (f x (cdr ls)))))))

  (define list?
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
  
  (define length
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

  (define list-ref
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

  (define reverse
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
  
  (define last-pair
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

  (define memq
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

  (define memv
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
  
  (define member
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

  (define assq
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

  (define assv
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

  (define assoc
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

  (module (map)
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
    (define map
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

  (module (for-each)
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
    (define for-each
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

  (module (andmap)
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
    (define andmap
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
         [(f . ls*) (error who "vararg not supported yet in ~s" (length ls*))])))



  (module (ormap)
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
    (define ormap
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

  )

