
(library (ikarus chars)
  (export char=? char<? char<=? char>? char>=? char-whitespace?
          char->integer integer->char
          char-alphabetic? char-downcase)
  (import 
    (except (ikarus)
      char=? char<? char<=? char>? char>=?
      integer->char char->integer
      char-whitespace? char-alphabetic? char-downcase)
    (ikarus system $pairs)
    (ikarus system $chars)
    (ikarus system $fx))

  (define integer->char
    (lambda (n)
      (unless (fixnum? n)
        (error 'integer->char "~s is not a fixnum" n))
      (unless (and ($fx>= n 0)
                   ($fx<= n 255))
        (error 'integer->char "~s is out of range[0..255]" n))
      ($fixnum->char n)))
  
  (define char->integer 
    (lambda (x) 
      (unless (char? x)
        (error 'char->integer "~s is not a character" x))
      ($char->fixnum x)))

  ;;; FIXME: this file is embarrasing
  (define char=?
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

  (define char<?
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

  (define char<=?
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

  (define char>?
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

  (define char>=?
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

  (define char-whitespace?
    (lambda (c)
      (cond 
        [(memq c '(#\space #\tab #\newline #\return)) #t]
        [(char? c) #f]
        [else
         (error 'char-whitespace? "~s is not a character" c)])))
  
  (define char-alphabetic?
    (lambda (c)
      (cond
        [(char? c)
         (cond
           [($char<= #\a c) ($char<= c #\z)]
           [($char<= #\A c) ($char<= c #\Z)]
           [else #f])]
        [else 
         (error 'char-alphabetic?  "~s is not a character" c)])))
  
  (define char-downcase
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

)
