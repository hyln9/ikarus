
(library (ikarus unicode-data)
  (export unicode-printable-char?
          char-downcase char-upcase char-titlecase char-foldcase
          char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?
          string-foldcase
          string-ci=?)
  (import 
    (ikarus system $fx)
    (ikarus system $vectors)
    (ikarus system $chars)
    (ikarus system $pairs)
    (ikarus system $strings)
    (ikarus system $io)
    (except (ikarus) char-downcase char-upcase char-titlecase char-foldcase
            char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?
            string-foldcase
            string-ci=?))

  (include "unicode/unicode-constituents.ss")
  (include "unicode/unicode-char-cases.ss")

  (define (binary-search n v)
    (let ([k ($fx- ($vector-length v) 1)])
      (let f ([i 0] [k k] [n n] [v v])
        (cond
          [($fx= i k) i]
          [else
           (let ([j ($fxsra ($fx+ i ($fx+ k 1)) 1)])
             (cond
               [($fx<= ($vector-ref v j) n) (f j k n v)]
               [else (f i ($fx- j 1) n v)]))]))))

  (define (binary-search-on? n v)
    ($fx= ($fxlogand (binary-search n v) 1) 1))

  (define (unicode-printable-char? c)
    (binary-search-on?
      ($char->fixnum c) 
      unicode-constituents-vector))
  
  (define (convert-char x adjustment-vec)
    (let ([n ($char->fixnum x)])
      (let ([idx (binary-search n charcase-search-vector)])
        (let ([adj ($vector-ref adjustment-vec idx)])
          ($fx+ adj n)))))

  (define (char-downcase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-downcase-adjustment-vector))
        (error 'char-downcase "~s is not a character" x)))

  (define (char-upcase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-upcase-adjustment-vector))
        (error 'char-downcase "~s is not a character" x)))
  
  (define (char-titlecase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-titlecase-adjustment-vector))
        (error 'char-downcase "~s is not a character" x)))

  (define (char-foldcase x)
    (if (char? x)
        ($fixnum->char ($fold x))
        (error 'char-downcase "~s is not a character" x)))

  (define ($fold x) 
    (convert-char x char-foldcase-adjustment-vector))

  (define char-ci=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx= ($fold x) ($fold y))
                   (error 'char-ci=? "~s is not a char" y)))
           (error 'char-ci=? "~s is not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci=? "~s is not a char" x))]
      [ls (error 'char-ci=? "not supported ~s" ls)]))

  (define char-ci<? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx< ($fold x) ($fold y))
                   (error 'char-ci<? "~s is not a char" y)))
           (error 'char-ci<? "~s is not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci<? "~s is not a char" x))]
      [ls (error 'char-ci<? "not supported ~s" ls)]))

  (define char-ci<=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx<= ($fold x) ($fold y))
                   (error 'char-ci<=? "~s is not a char" y)))
           (error 'char-ci<=? "~s is not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci<=? "~s is not a char" x))]
      [ls (error 'char-ci<=? "not supported ~s" ls)]))

  (define char-ci>? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx> ($fold x) ($fold y))
                   (error 'char-ci>? "~s is not a char" y)))
           (error 'char-ci>? "~s is not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci>? "~s is not a char" x))]
      [ls (error 'char-ci>? "not supported ~s" ls)]))
  
  (define char-ci>=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx>= ($fold x) ($fold y))
                   (error 'char-ci>=? "~s is not a char" y)))
           (error 'char-ci>=? "~s is not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci>=? "~s is not a char" x))]
      [ls (error 'char-ci>=? "not supported ~s" ls)]))
               
  (define ($string-foldcase str)
    (let f ([str str] [i 0] [n (string-length str)] [p (open-output-string)])
      (cond
        [($fx= i n) (get-output-string p)]
        [else
         (let* ([n ($char->fixnum ($string-ref str i))])
           (let ([n/ls
                  (vector-ref string-foldcase-adjustment-vector
                    (binary-search n charcase-search-vector))])
             (if (fixnum? n/ls)
                 ($write-char ($fixnum->char ($fx+ n n/ls)) p)
                 (let f ([ls n/ls])
                   ($write-char ($car ls) p)
                   (let ([ls ($cdr ls)])
                     (if (pair? ls) 
                         (f ls)
                         ($write-char ls p)))))))
         (f str ($fxadd1 i) n p)])))

  (define (string-foldcase str)
    (if (string? str)
        ($string-foldcase str)
        (error 'string-foldcase "~s is not a string" str)))

  (define (string-ci=? s1 s2)
    (if (string? s1)
        (if (string? s2) 
            (string=? ($string-foldcase s1) ($string-foldcase s2))
            (error 'string-ci=? "~s is not a string" s2))
        (error 'string-ci=? "~s is not a string" s1)))


  )

