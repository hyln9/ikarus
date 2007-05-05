
(library (ikarus lists)
  (export $memq list? length list-ref reverse last-pair 
          memq memv member)
  (import 
    (only (scheme) $car $cdr $fx+ $fxsub1 $fxzero? $fx>=)

    (except (ikarus) list? reverse last-pair memq memv member
            length list-ref))

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



  )

