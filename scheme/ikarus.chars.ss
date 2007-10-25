
(library (ikarus chars)
  (export char=? char<? char<=? char>? char>=? char->integer integer->char)
  (import 
    (except (ikarus)
      char=? char<? char<=? char>? char>=?  integer->char char->integer)
    (ikarus system $pairs)
    (ikarus system $chars)
    (ikarus system $fx))

  (define integer->char
    (lambda (n)
      (cond
        [(not (fixnum? n)) (error 'integer->char "invalid argument" n)]
        [($fx< n 0) (error 'integer->char "negative" n)]
        [($fx<= n #xD7FF) ($fixnum->char n)]
        [($fx< n #xE000)
         (error 'integer->char "integer does not have a unicode representation" n)]
        [($fx<= n #x10FFFF) ($fixnum->char n)]
        [else (error 'integer->char 
                "integer does not have a unicode representation" n)])))
  
  (define char->integer 
    (lambda (x) 
      (unless (char? x)
        (error 'char->integer "not a character" x))
      ($char->fixnum x)))

  ;;; FIXME: this file is embarrasing
  (define char=?
    (let ()
      (define (err x)
        (error 'char=? "not a character" x))
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

  (define char<?
    (let ()
      (define (err x)
        (error 'char<? "not a character" x))
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

  (define char<=?
    (let ()
      (define (err x)
        (error 'char<=? "not a character" x))
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

  (define char>?
    (let ()
      (define (err x)
        (error 'char>? "not a character" x))
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

  (define char>=?
    (let ()
      (define (err x)
        (error 'char>=? "not a character" x))
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


)
