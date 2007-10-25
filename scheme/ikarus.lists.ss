
(library (ikarus lists)
  (export $memq list? list cons* make-list append length list-ref reverse
          last-pair memq memp memv member find assq assp assv assoc
          remq remv remove remp filter map for-each andmap ormap list-tail
          partition for-all exists fold-left fold-right)
  (import 
    (ikarus system $fx)
    (ikarus system $pairs)
    (except (ikarus) list? list cons* make-list append reverse
            last-pair length list-ref memq memp memv member find
            assq assp assv assoc remq remv remove remp filter
            map for-each andmap ormap list-tail partition
            for-all exists fold-left fold-right))

  (define $memq
    (lambda (x ls)
      (let f ([x x] [ls ls])
        (and (pair? ls)
             (if (eq? x (car ls))
                 ls
                 (f x (cdr ls)))))))

  (define list (lambda x x))

  (define cons*
    (lambda (fst . rest)
      (let f ([fst fst] [rest rest])
        (cond
          [(null? rest) fst]
          [else 
           (cons fst (f ($car rest) ($cdr rest)))]))))

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

  (module (make-list)
    (define f
      (lambda (n fill ls)
        (cond
          [($fxzero? n) ls]
          [else
           (f ($fxsub1 n) fill (cons fill ls))])))
    (define make-list
      (case-lambda
        [(n)
         (if (and (fixnum? n) ($fx>= n 0))
             (f n (void) '())
             (error 'make-list "not a valid length" n))]
        [(n fill)
         (if (and (fixnum? n) ($fx>= n 0))
             (f n fill '())
             (error 'make-list "not a valid length" n))])))


  (define length
    (letrec ([race
              (lambda (h t ls n)
               (if (pair? h)
                   (let ([h ($cdr h)])
                      (if (pair? h)
                          (if (not (eq? h t))
                              (race ($cdr h) ($cdr t) ls ($fx+ n 2))
                              (error 'length "circular list" ls))
                          (if (null? h)
                              ($fx+ n 1)
                              (error 'length "not a proper list" ls))))
                   (if (null? h)
                       n
                       (error 'length "not a proper list" ls))))])
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
                 (error 'list-ref "index is out of range" index list))]
            [(pair? ls)
             (f ($cdr ls) ($fxsub1 i))]
            [(null? ls) 
             (error 'list-rec "index is out of range" index list)]
            [else (error 'list-ref "not a list" list)])))
      (unless (and (fixnum? index) ($fx>= index 0))
        (error 'list-ref "not a valid index" index))
      (f list index)))


  (define list-tail
    (lambda (list index)
      (define f
        (lambda (ls i)
          (cond
            [($fxzero? i) ls]
            [(pair? ls)
             (f ($cdr ls) ($fxsub1 i))]
            [(null? ls) 
             (error 'list-tail "index is out of range" index list)]
            [else (error 'list-tail "not a list" list)])))
      (unless (and (fixnum? index) ($fx>= index 0))
        (error 'list-tail "not a valid index" index))
      (f list index)))

  (module (append)
    (define reverse
      (lambda (h t ls ac)
        (if (pair? h)
            (let ([h ($cdr h)] [a1 ($car h)])
               (if (pair? h)
                   (if (not (eq? h t))
                       (let ([a2 ($car h)])
                         (reverse ($cdr h) ($cdr t) ls (cons a2 (cons a1 ac))))
                       (error 'append "circular list" ls))
                   (if (null? h)
                       (cons a1 ac)
                       (error 'append "not a proper list" ls))))
            (if (null? h)
                ac 
                (error 'append "not a proper list" ls)))))
    (define revcons
      (lambda (ls ac)
        (cond
          [(null? ls) ac]
          [else
           (revcons ($cdr ls) (cons ($car ls) ac))])))
    (define append1
      (lambda (ls ls*)
        (cond
          [(null? ls*) ls]
          [else 
           (revcons (reverse ls ls ls '())
              (append1 ($car ls*) ($cdr ls*)))])))
    (define append
      (case-lambda
        [() '()]
        [(ls) ls]
        [(ls . ls*)
         (append1 ls ls*)])))

  (define reverse
    (letrec ([race
              (lambda (h t ls ac)
               (if (pair? h)
                   (let ([h ($cdr h)] [ac (cons ($car h) ac)])
                      (if (pair? h)
                          (if (not (eq? h t))
                              (race ($cdr h) ($cdr t) ls (cons ($car h) ac))
                              (error 'reverse "circular list" ls))
                          (if (null? h)
                              ac
                              (error 'reverse "not a proper list" ls))))
                   (if (null? h)
                       ac
                       (error 'reverse "not a proper list" ls))))])
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
                               (error 'last-pair "circular list" ls))
                           last))
                    last))])
       (lambda (x)
         (if (pair? x)
             (let ([d (cdr x)])
               (race d d x x))
             (error 'last-pair "not a pair" x)))))

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
                                       (error 'memq "circular list" ls)))
                               (if (null? h)
                                   '#f
                                   (error 'memq "not a proper list" ls)))))
                     (if (null? h)
                         '#f
                         (error 'memq "not a proper list" ls))))])
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
                                       (error 'memv "circular list" ls)))
                               (if (null? h)
                                   '#f
                                   (error 'memv "not a proper list" ls)))))
                     (if (null? h)
                         '#f
                         (error 'memv "not a proper list" ls))))])
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
                                       (error 'member "circular list" ls)))
                               (if (null? h)
                                   '#f
                                   (error 'member "not a proper list" ls)))))
                     (if (null? h)
                         '#f
                         (error 'member "not a proper list" ls))))])
       (lambda (x ls)
         (race ls ls ls x))))


  (define memp
    (letrec ([race
              (lambda (h t ls p)
                 (if (pair? h)
                     (if (p ($car h))
                         h
                         (let ([h ($cdr h)])
                           (if (pair? h)
                               (if (p ($car h))
                                   h
                                   (if (not (eq? h t))
                                       (race ($cdr h) ($cdr t) ls p)
                                       (error 'memp "circular list" ls)))
                               (if (null? h)
                                   '#f
                                   (error 'memp "not a proper list" ls)))))
                     (if (null? h)
                         '#f
                         (error 'memp "not a proper list" ls))))])
       (lambda (p ls)
         (unless (procedure? p)
           (error 'memp "not a procedure" p))
         (race ls ls ls p))))

  (define find
    (letrec ([race
              (lambda (h t ls p)
                 (if (pair? h)
                     (let ([a ($car h)])
                       (if (p a)
                           a
                           (let ([h ($cdr h)])
                             (if (pair? h)
                                 (let ([a ($car h)])
                                   (if (p a)
                                       a
                                       (if (not (eq? h t))
                                           (race ($cdr h) ($cdr t) ls p)
                                           (error 'find "circular list" ls))))
                                 (if (null? h)
                                     '#f
                                     (error 'find "not a proper list" ls))))))
                     (if (null? h)
                         '#f
                         (error 'find "not a proper list" ls))))])
       (lambda (p ls)
         (unless (procedure? p)
           (error 'find "not a procedure" p))
         (race ls ls ls p))))

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
                                              (error 'assq "malformed alist"
                                                     ls)))
                                       (error 'assq "circular list" ls))
                                   (if (null? h)
                                       #f
                                       (error 'assq "not a proper list" ls))))
                           (error 'assq "malformed alist" ls)))
                    (if (null? h)
                        #f
                        (error 'assq "not a proper list" ls))))])
       (lambda (x ls) 
         (race x ls ls ls))))


  (define assp
    (letrec ([race
              (lambda (p h t ls)
                (if (pair? h)
                    (let ([a ($car h)] [h ($cdr h)])
                       (if (pair? a)
                           (if (p ($car a))
                               a
                               (if (pair? h)
                                   (if (not (eq? h t))
                                       (let ([a ($car h)])
                                          (if (pair? a)
                                              (if (p ($car a))
                                                  a
                                                  (race p ($cdr h) ($cdr t) ls))
                                              (error 'assp "malformed alist"
                                                     ls)))
                                       (error 'assp "circular list" ls))
                                   (if (null? h)
                                       #f
                                       (error 'assp "not a proper list" ls))))
                           (error 'assp "malformed alist" ls)))
                    (if (null? h)
                        #f
                        (error 'assp "not a proper list" ls))))])
       (lambda (p ls)
         (unless (procedure? p) 
           (error 'assp "not a procedure" p))
         (race p ls ls ls))))

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
                                              (error 'assv "malformed alist"
                                                     ls)))
                                       (error 'assv "circular list" ls))
                                   (if (null? h)
                                       #f
                                       (error 'assv "not a proper list" ls))))
                           (error 'assv "malformed alist" ls)))
                    (if (null? h)
                        #f
                        (error 'assv "not a proper list" ls))))])
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
                                              (error 'assoc "malformed alist"
                                                     ls)))
                                       (error 'assoc "circular list" ls))
                                   (if (null? h)
                                       #f
                                       (error 'assoc "not a proper list" ls))))
                           (error 'assoc "malformed alist" ls)))
                    (if (null? h)
                        #f
                        (error 'assoc "not a proper list" ls))))])
       (lambda (x ls) 
         (race x ls ls ls))))


  (module (remq remv remove remp filter)
    (define-syntax define-remover 
      (syntax-rules ()
        [(_ name cmp check)
         (define name
           (letrec ([race
                     (lambda (h t ls x)
                        (if (pair? h)
                            (if (cmp ($car h) x)
                                (let ([h ($cdr h)])
                                  (if (pair? h)
                                      (if (not (eq? h t))
                                          (if (cmp ($car h) x)
                                              (race ($cdr h) ($cdr t) ls x)
                                              (cons ($car h) (race ($cdr h) ($cdr t) ls x)))
                                          (error 'name "circular list" ls))
                                      (if (null? h)
                                          '()
                                          (error 'name "not a proper list" ls))))
                                (let ([a0 ($car h)] [h ($cdr h)])
                                  (if (pair? h)
                                      (if (not (eq? h t))
                                          (if (cmp ($car h) x)
                                              (cons a0 (race ($cdr h) ($cdr t) ls x))
                                              (cons* a0 ($car h) (race ($cdr h) ($cdr t) ls x)))
                                          (error 'name "circular list" ls))
                                      (if (null? h)
                                          (list a0)
                                          (error 'name "not a proper list" ls)))))
                            (if (null? h)
                                '()
                                (error 'name "not a proper list" ls))))])
              (lambda (x ls)
                (check x ls)
                (race ls ls ls x))))]))
    (define-remover remq eq? (lambda (x ls) #t))
    (define-remover remv eqv? (lambda (x ls) #t))
    (define-remover remove equal? (lambda (x ls) #t))
    (define-remover remp (lambda (elt p) (p elt))
      (lambda (x ls) 
        (unless (procedure? x)
          (error 'remp "not a procedure" x)))) 
    (define-remover filter (lambda (elt p) (not (p elt)))
      (lambda (x ls) 
        (unless (procedure? x)
          (error 'filter "not a procedure" x)))))


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
            (error who "not a procedure" f))
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (map1 f ($car ls) d (len d d 0)))]
            [(null? ls) '()]
            [else (error who "improper list")])]
         [(f ls ls2)
          (unless (procedure? f)
            (error who "not a procedure" f))
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
            (error who "not a procedure" f))
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
            (error who "not a procedure" f))
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (for-each1 f ($car ls) d (len d d 0)))]
            [(null? ls) (void)]
            [else (error who "improper list")])]
         [(f ls ls2)
          (unless (procedure? f)
            (error who "not a procedure" f))
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
         [(f ls . ls*)
          (unless (procedure? f) 
            (error 'for-each "not a procedure" f))
          (unless (list? ls) 
            (error 'for-each "not a list" ls))
          (let ([n (length ls)])
            (for-each 
              (lambda (x) 
                (unless (and (list? x) (= (length x) n))
                  (error 'for-each "not a list" x)))
              ls*)
            (let loop ([n (length ls)] [ls ls] [ls* ls*])
              (cond
                [($fx= n 0) 
                 (unless (and (null? ls) (andmap null? ls*))
                   (error 'for-each "list modified" f))]
                [else
                 (unless (and (pair? ls) (andmap pair? ls*))
                   (error 'for-each "list modified" f))
                 (apply f (car ls) (map car ls*))
                 (loop (fx- n 1) (cdr ls) (map cdr ls*))])))])))

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
            (error who "not a procedure" f))
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (andmap1 f ($car ls) d (len d d 0)))]
            [(null? ls) #t]
            [else (error who "improper list")])]
         [(f ls ls2)
          (unless (procedure? f)
            (error who "not a procedure" f))
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
         [(f ls . ls*)
          (unless (procedure? f) 
            (error who "not a procedure" f))
          (error who "vararg not yet supported")])))




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
            (error who "not a procedure" f))
          (cond
            [(pair? ls)
             (let ([d ($cdr ls)])
               (ormap1 f ($car ls) d (len d d 0)))]
            [(null? ls) #f]
            [else (error who "improper list")])]
         [_ (error who "vararg not supported yet")])))



  (define partition
    (letrec ([race
              (lambda (h t ls p)
                 (if (pair? h)
                     (let ([a0 ($car h)] [h ($cdr h)])
                        (if (pair? h)
                            (if (eq? h t)
                                (error 'partition "circular list" ls)
                                (let ([a1 ($car h)])
                                  (let-values ([(a* b*) (race ($cdr h) ($cdr t) ls p)])
                                    (if (p a0) 
                                        (if (p a1) 
                                            (values (cons* a0 a1 a*) b*)
                                            (values (cons a0 a*) (cons a1 b*)))
                                        (if (p a1) 
                                            (values (cons a0 a*) (cons a1 b*))
                                            (values a* (cons* a0 a1 b*)))))))
                            (if (null? h)
                                (if (p a0)
                                    (values (list a0) '())
                                    (values '() (list a0)))
                                (error 'parititon "not a proper list" ls))))
                     (if (null? h)
                         (values '() '())
                         (error 'parition "not a proper list" ls))))])
       (lambda (p ls)
         (unless (procedure? p) 
           (error 'partition "not a procedure" p))
         (race ls ls ls p))))



  (define-syntax define-iterator
    (syntax-rules ()
      [(_ name combine)
       (module (name)
         (define who 'name)
         (define (null*? ls)
           (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))
         (define (err* ls*)
           (if (null? ls*) 
               (error who "length mismatch")
               (if (list? (car ls*))
                   (err* (cdr ls*))
                   (error who "not a proper list" (car ls*)))))
         (define (cars+cdrs ls ls*)
           (cond
             [(null? ls) (values '() '())]
             [else
              (let ([a (car ls)])
                (if (pair? a) 
                    (let-values ([(cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))])
                      (values (cons (car a) cars) (cons (cdr a) cdrs)))
                    (if (list? (car ls*)) 
                        (error who "length mismatch")
                        (error who "not a proper list" (car ls*)))))]))
         (define (loop1 f a h t ls)
           (if (pair? h)
               (let ([b (car h)] [h (cdr h)])
                 (combine (f a)
                   (if (pair? h)
                       (if (eq? h t)
                           (error who "circular" ls)
                           (let ([c (car h)] [h (cdr h)])
                             (combine (f b) (loop1 f c h (cdr t) ls))))
                       (if (null? h) 
                           (f b)
                           (combine (f b) (error who "not a proper list" ls))))))
               (if (null? h)
                   (f a)
                   (combine (f a) (error who "not a proper list" ls)))))
         (define (loopn f a a* h h* t ls ls*)
           (if (pair? h)
               (let-values ([(b* h*) (cars+cdrs h* ls*)])
                 (let ([b (car h)] [h (cdr h)])
                   (combine (apply f a a*)
                     (if (pair? h)
                         (if (eq? h t)
                             (error who "circular" ls)
                             (let-values ([(c* h*) (cars+cdrs h* ls*)])
                               (let ([c (car h)] [h (cdr h)])
                                 (combine (apply f b b*) 
                                   (loopn f c c* h h* (cdr t) ls ls*)))))
                         (if (and (null? h) (null*? h*))
                             (apply f b b*)
                             (combine (apply f b b*) (err* (cons ls ls*))))))))
               (if (and (null? h) (null*? h*))
                   (apply f a a*)
                   (combine (apply f a a*) (err* (cons ls ls*))))))
         (define name 
           (case-lambda
             [(f ls)
              (unless (procedure? f) 
                (error who "not a procedure" f))
              (if (pair? ls)
                  (loop1 f (car ls) (cdr ls) (cdr ls) ls)
                  (if (null? ls)
                      (combine)
                      (error who "not a list" ls)))]
             [(f ls . ls*)
              (unless (procedure? f) 
                (error who "not a procedure" f))
              (if (pair? ls)
                  (let-values ([(cars cdrs) (cars+cdrs ls* ls*)])
                    (loopn f (car ls) cars (cdr ls) cdrs (cdr ls) ls ls*))
                  (if (and (null? ls) (null*? ls*))
                      (combine) 
                      (err* ls*)))])))]))

  (define-iterator for-all and)
  (define-iterator exists  or)

  (module (fold-left)
    (define who 'fold-left)
    (define (null*? ls)
      (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))
    (define (err* ls*)
      (if (null? ls*)
          (error who "length mismatch")
          (if (list? (car ls*))
              (err* (cdr ls*))
              (error who "not a proper list" (car ls*)))))
    (define (cars+cdrs ls ls*)
      (cond
        [(null? ls) (values '() '())]
        [else
         (let ([a (car ls)])
           (if (pair? a) 
               (let-values ([(cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))])
                 (values (cons (car a) cars) (cons (cdr a) cdrs)))
               (if (list? (car ls*)) 
                   (error who "length mismatch")
                   (error who "not a proper list" (car ls*)))))]))
     (define (loop1 f nil h t ls)
       (if (pair? h)
           (let ([a (car h)] [h (cdr h)])
             (if (pair? h) 
                 (if (eq? h t)
                     (error who "circular" ls)
                     (let ([b (car h)] [h (cdr h)] [t (cdr t)])
                       (loop1 f (f (f nil a) b) h t ls)))
                 (if (null? h)
                     (f nil a)
                     (error who "not a proper list" ls))))
           (if (null? h)
               nil
               (error who "not a proper list" ls))))
    (define (loopn f nil h h* t ls ls*)
      (if (pair? h)
          (let-values ([(a* h*) (cars+cdrs h* ls*)])
            (let ([a (car h)] [h (cdr h)])
              (if (pair? h) 
                  (if (eq? h t)
                      (error who "circular" ls)
                      (let-values ([(b* h*) (cars+cdrs h* ls*)])
                        (let ([b (car h)] [h (cdr h)] [t (cdr t)])
                          (loopn f 
                            (apply f (apply f nil a a*) b b*)
                            h h* t ls ls*))))
                  (if (and (null? h) (null*? h*))
                      (apply f nil a a*)
                      (err* (cons ls ls*))))))
          (if (and (null? h) (null*? h*))
              nil
              (err* (cons ls ls*))))) 
    (define fold-left
      (case-lambda
        [(f nil ls) 
         (unless (procedure? f)
           (error who "not a procedure" f))
         (loop1 f nil ls ls ls)]
        [(f nil ls . ls*) 
         (unless (procedure? f)
           (error who "not a procedure" f))
         (loopn f nil ls ls* ls ls ls*)])))
  
  (module (fold-right)
    (define who 'fold-right)
    (define (null*? ls)
      (or (null? ls) (and (null? (car ls)) (null*? (cdr ls)))))
    (define (err* ls*)
      (if (null? ls*)
          (error who "length mismatch")
          (if (list? (car ls*))
              (err* (cdr ls*))
              (error who "not a proper list" (car ls*)))))
    (define (cars+cdrs ls ls*)
      (cond
        [(null? ls) (values '() '())]
        [else
         (let ([a (car ls)])
           (if (pair? a) 
               (let-values ([(cars cdrs) (cars+cdrs (cdr ls) (cdr ls*))])
                 (values (cons (car a) cars) (cons (cdr a) cdrs)))
               (if (list? (car ls*)) 
                   (error who "length mismatch")
                   (error who "not a proper list" (car ls*)))))]))
     (define (loop1 f nil h t ls)
       (if (pair? h)
           (let ([a (car h)] [h (cdr h)])
             (if (pair? h) 
                 (if (eq? h t)
                     (error who "circular" ls)
                     (let ([b (car h)] [h (cdr h)] [t (cdr t)])
                       (f a (f b (loop1 f nil h t ls)))))
                 (if (null? h)
                     (f a nil)
                     (error who "not a proper list" ls))))
           (if (null? h)
               nil
               (error who "not a proper list" ls))))
    (define (loopn f nil h h* t ls ls*)
      (if (pair? h)
          (let-values ([(a* h*) (cars+cdrs h* ls*)])
            (let ([a (car h)] [h (cdr h)])
              (if (pair? h) 
                  (if (eq? h t)
                      (error who "circular" ls)
                      (let-values ([(b* h*) (cars+cdrs h* ls*)])
                        (let ([b (car h)] [h (cdr h)] [t (cdr t)])
                          (apply f a
                            (append a* 
                              (list 
                                (apply f
                                  b (append b* 
                                      (list (loopn f nil h h* t ls ls*))))))))))
                  (if (and (null? h) (null*? h*))
                      (apply f a (append a* (list nil)))
                      (err* (cons ls ls*))))))
          (if (and (null? h) (null*? h*))
              nil
              (err* (cons ls ls*))))) 
    (define fold-right
      (case-lambda
        [(f nil ls) 
         (unless (procedure? f)
           (error who "not a procedure" f))
         (loop1 f nil ls ls ls)]
        [(f nil ls . ls*) 
         (unless (procedure? f)
           (error who "not a procedure" f))
         (loopn f nil ls ls* ls ls ls*)]
        )))

  )

