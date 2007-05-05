
(library (ikarus lists)
  (export $memq list? reverse last-pair memq memv member)
  (import 
    (only (scheme) $car $cdr)

    (except (ikarus) list? reverse last-pair memq memv member))

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

