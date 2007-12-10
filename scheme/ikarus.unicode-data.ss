;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



 

(library (ikarus unicode-data)
  
  (export 
    unicode-printable-char?  char-downcase char-upcase
    char-titlecase char-foldcase char-ci=? char-ci<?  char-ci<=?
    char-ci>? char-ci>=?  string-ci=?  string-ci<?  string-ci<=?
    string-ci>?  string-ci>=?  string-foldcase char-general-category
    char-alphabetic?  char-numeric?  char-whitespace?
    char-upper-case?  char-lower-case?  char-title-case?  )

  (import 
    (ikarus system $fx)
    (ikarus system $vectors)
    (ikarus system $chars)
    (ikarus system $pairs)
    (ikarus system $strings)
    (except (ikarus) 
      char-downcase char-upcase char-titlecase char-foldcase
      char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?
      string-ci=?  string-ci<?  string-ci<=?  string-ci>?
      string-ci>=?  string-foldcase char-general-category
      char-alphabetic?  char-numeric?  char-whitespace?
      char-upper-case?  char-lower-case?  char-title-case?  ))

  (include "unicode/unicode-char-cases.ss")
  (include "unicode/unicode-charinfo.ss")

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

  (define (lookup-char-info c)
    (let ([v unicode-categories-lookup-vector]
          [t unicode-categories-values-vector])
      (define (f i k n) 
        (cond
          [($fx= i k) 
           (let ([idx ($vector-ref t i)])
             (if (fixnum? idx) 
                 idx
                 (let ([idx2 ($fx- n ($vector-ref v i))])
                   ($vector-ref idx idx2))))]
          [else
           (let ([j ($fxsra ($fx+ i ($fx+ k 1)) 1)])
             (cond
               [($fx<= ($vector-ref v j) n) (f j k n)]
               [else (f i ($fx- j 1) n)]))]))
      (f 0 (fx- (vector-length v) 1) (char->integer c))))

  (define (char-general-category c)
    (if (char? c) 
        (vector-ref unicode-categories-name-vector
          (fxlogand 63 (lookup-char-info c)))
        (error 'char-general-category "not a char" c)))

  (define (char-has-property? c prop-val who)
    (if (char? c) 
        (not (fxzero? (fxlogand (lookup-char-info c) prop-val)))
        (error who "not a char" c)))

  (define (unicode-printable-char? c) 
    (char-has-property? c constituent-property 'unicode-printable-char?))
  (define (char-alphabetic? c) 
    (char-has-property? c alphabetic-property 'char-alphabetic?))
  (define (char-numeric? c) 
    (char-has-property? c numeric-property 'char-numeric?))
  (define (char-whitespace? c) 
    (char-has-property? c whitespace-property 'char-whitespace?))
  (define (char-upper-case? c) 
    (char-has-property? c uppercase-property 'char-upper-case?))
  (define (char-lower-case? c) 
    (char-has-property? c lowercase-property 'char-lower-case?))
  (define (char-title-case? c) 
    (char-has-property? c titlecase-property 'char-title-case?))
  



  (define (convert-char x adjustment-vec)
    (let ([n ($char->fixnum x)])
      (let ([idx (binary-search n charcase-search-vector)])
        (let ([adj ($vector-ref adjustment-vec idx)])
          ($fx+ adj n)))))

  (define (char-downcase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-downcase-adjustment-vector))
        (error 'char-downcase "not a character" x)))

  (define (char-upcase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-upcase-adjustment-vector))
        (error 'char-downcase "not a character" x)))
  
  (define (char-titlecase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-titlecase-adjustment-vector))
        (error 'char-downcase "not a character" x)))

  (define (char-foldcase x)
    (if (char? x)
        ($fixnum->char ($fold x))
        (error 'char-downcase "not a character" x)))

  (define ($fold x) 
    (convert-char x char-foldcase-adjustment-vector))

  (define (char-ci-loop c0 ls p? who) 
    (or (null? ls)
        (let ([c1 (car ls)])
          (unless (char? c1) (error who "not a char" c1))
          (let ([c1 ($fold c1)])
            (if (p? c0 c1) 
                (char-ci-loop c1 (cdr ls) p? who)
                (let f ([ls (cdr ls)] [who who])
                  (cond
                    [(null? ls) #f]
                    [(char? (car ls)) 
                     (f (cdr ls) who)]
                    [else (error who "not a char" (car ls))])))))))

  (define char-ci=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx= ($fold x) ($fold y))
                   (error 'char-ci=? "not a char" y)))
           (error 'char-ci=? "not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci=? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop x x* char=? 'char-ci=?)
           (error 'char-ci=? "not a char" x))]))

  (define char-ci<? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx< ($fold x) ($fold y))
                   (error 'char-ci<? "not a char" y)))
           (error 'char-ci<? "not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci<? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop x x* char<? 'char-ci<?)
           (error 'char-ci<? "not a char" x))]))

  (define char-ci<=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx<= ($fold x) ($fold y))
                   (error 'char-ci<=? "not a char" y)))
           (error 'char-ci<=? "not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci<=? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop x x* char<=? 'char-ci<=?)
           (error 'char-ci<=? "not a char" x))]))

  (define char-ci>? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx> ($fold x) ($fold y))
                   (error 'char-ci>? "not a char" y)))
           (error 'char-ci>? "not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci>? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop x x* char>? 'char-ci>?)
           (error 'char-ci>? "not a char" x))]))
  
  (define char-ci>=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx>= ($fold x) ($fold y))
                   (error 'char-ci>=? "not a char" y)))
           (error 'char-ci>=? "not a char" x))]
      [(x)
       (or (char? x) (error 'char-ci>=? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop x x* char>=? 'char-ci>=?)
           (error 'char-ci>=? "not a char" x))]))
               
  (define ($string-foldcase str)
    (let-values ([(p e) (open-string-output-port)])
      (let f ([str str] [i 0] [n (string-length str)])
        (cond
          [($fx= i n) (e)]
          [else
           (let* ([n ($char->fixnum ($string-ref str i))])
             (let ([n/ls
                    (vector-ref string-foldcase-adjustment-vector
                      (binary-search n charcase-search-vector))])
               (if (fixnum? n/ls)
                   (write-char ($fixnum->char ($fx+ n n/ls)) p)
                   (let f ([ls n/ls])
                     (write-char ($car ls) p)
                     (let ([ls ($cdr ls)])
                       (if (pair? ls) 
                           (f ls)
                           (write-char ls p)))))))
           (f str ($fxadd1 i) n)]))))

  (define (string-foldcase str)
    (if (string? str)
        ($string-foldcase str)
        (error 'string-foldcase "not a string" str)))

  ;;; FIXME: case-insensitive comparison procedures are slow.

  (define string-ci-cmp
    (lambda (who cmp)
      (case-lambda
        [(s1 s2) 
         (if (string? s1)
             (if (string? s2) 
                 (cmp ($string-foldcase s1) ($string-foldcase s2))
                 (error who "not a string" s2))
             (error who "not a string" s1))]
        [(s1 . s*) 
         (if (string? s1) 
             (let ([s1 ($string-foldcase s1)])
               (let f ([s1 s1] [s* s*]) 
                 (cond
                   [(null? s*) #t]
                   [else
                    (let ([s2 (car s*)])
                      (if (string? s2) 
                          (let ([s2 ($string-foldcase s2)])
                            (if (cmp s1 s2) 
                                (f s2 (cdr s*))
                                (let f ([s* (cdr s*)])
                                  (cond
                                    [(null? s*) #f]
                                    [(string? (car s*)) 
                                     (f (cdr s*))]
                                    [else 
                                     (error who "not a string" 
                                       (car s*))]))))
                          (error who "not a string" s2)))])))
             (error who "not a string" s1))])))


  (define string-ci=?  (string-ci-cmp 'string-ci=? string=?))
  (define string-ci<?  (string-ci-cmp 'string-ci<? string<?))
  (define string-ci<=? (string-ci-cmp 'string-ci<=? string<=?))
  (define string-ci>?  (string-ci-cmp 'string-ci>? string>?))
  (define string-ci>=? (string-ci-cmp 'string-ci>=? string>=?))


  )

