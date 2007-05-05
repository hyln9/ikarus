
(library (ikarus lists)
  (export $memq list? reverse last-pair)
  (import 
    (only (scheme) $car $cdr)

    (except (ikarus) list? reverse last-pair))

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
  

  
  )

