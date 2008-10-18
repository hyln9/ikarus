;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
    char-upper-case?  char-lower-case?  char-title-case?  
    string-upcase string-downcase)

  (import 
    (ikarus system $fx)
    (ikarus system $vectors)
    (ikarus system $chars)
    (ikarus system $pairs)
    (ikarus system $strings)
    (except (ikarus) 
      unicode-printable-char?
      char-downcase char-upcase char-titlecase char-foldcase
      char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=?
      string-ci=?  string-ci<?  string-ci<=?  string-ci>?
      string-ci>=?  string-foldcase char-general-category
      char-alphabetic?  char-numeric?  char-whitespace?
      char-upper-case?  char-lower-case?  char-title-case?  
      string-upcase string-downcase))

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
        (die 'char-general-category "not a char" c)))

  (define (char-has-property? c prop-val who)
    (if (char? c) 
        (not (fxzero? (fxlogand (lookup-char-info c) prop-val)))
        (die who "not a char" c)))

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
        (die 'char-downcase "not a character" x)))

  (define (char-upcase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-upcase-adjustment-vector))
        (die 'char-downcase "not a character" x)))
  
  (define (char-titlecase x)
    (if (char? x) 
        ($fixnum->char
          (convert-char x char-titlecase-adjustment-vector))
        (die 'char-downcase "not a character" x)))

  (define (char-foldcase x)
    (if (char? x)
        ($fixnum->char ($fold x))
        (die 'char-downcase "not a character" x)))

  (define ($fold x) 
    (convert-char x char-foldcase-adjustment-vector))

  (define (char-ci-loop c0 ls p? who) 
    (or (null? ls)
        (let ([c1 (car ls)])
          (unless (char? c1) (die who "not a char" c1))
          (let ([c1 ($fold c1)])
            (if (p? c0 c1) 
                (char-ci-loop c1 (cdr ls) p? who)
                (let f ([ls (cdr ls)] [who who])
                  (cond
                    [(null? ls) #f]
                    [(char? (car ls)) 
                     (f (cdr ls) who)]
                    [else (die who "not a char" (car ls))])))))))

  (define char-ci=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx= ($fold x) ($fold y))
                   (die 'char-ci=? "not a char" y)))
           (die 'char-ci=? "not a char" x))]
      [(x)
       (or (char? x) (die 'char-ci=? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop ($fold x) x* = 'char-ci=?)
           (die 'char-ci=? "not a char" x))]))

  (define char-ci<? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx< ($fold x) ($fold y))
                   (die 'char-ci<? "not a char" y)))
           (die 'char-ci<? "not a char" x))]
      [(x)
       (or (char? x) (die 'char-ci<? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop ($fold x) x* < 'char-ci<?)
           (die 'char-ci<? "not a char" x))]))

  (define char-ci<=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx<= ($fold x) ($fold y))
                   (die 'char-ci<=? "not a char" y)))
           (die 'char-ci<=? "not a char" x))]
      [(x)
       (or (char? x) (die 'char-ci<=? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop ($fold x) x* <= 'char-ci<=?)
           (die 'char-ci<=? "not a char" x))]))

  (define char-ci>? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx> ($fold x) ($fold y))
                   (die 'char-ci>? "not a char" y)))
           (die 'char-ci>? "not a char" x))]
      [(x)
       (or (char? x) (die 'char-ci>? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop ($fold x) x* > 'char-ci>?)
           (die 'char-ci>? "not a char" x))]))
  
  (define char-ci>=? 
    (case-lambda 
      [(x y) 
       (if (char? x) 
           (or (eq? x y)
               (if (char? y)
                   ($fx>= ($fold x) ($fold y))
                   (die 'char-ci>=? "not a char" y)))
           (die 'char-ci>=? "not a char" x))]
      [(x)
       (or (char? x) (die 'char-ci>=? "not a char" x))]
      [(x . x*) 
       (if (char? x) 
           (char-ci-loop ($fold x) x* >= 'char-ci>=?)
           (die 'char-ci>=? "not a char" x))]))
               
  (define ($string-change-case str adjustment-vector)
    (define (extend-length str ac) 
      (define (chars ac n)
        (cond
          [(null? ac) n]
          [else 
           (chars (cdr ac)
             (let f ([p (cdar ac)] [n n])
               (cond
                 [(pair? p) (f (cdr p) (+ n 1))]
                 [else n])))]))
      (let ([dst-len (chars ac (string-length str))])
        (let f ([str str] [dst (make-string dst-len)] [i 0] [j 0] [ac (reverse ac)])
          (cond
            [(null? ac) 
             (string-copy! str i dst j (fx- (string-length str) i))
             dst]
            [else
             (let ([idx (caar ac)] [c* (cdar ac)] [ac (cdr ac)])
               (let ([cnt (fx- idx i)])
                 (string-copy! str i dst j cnt)
                 (let g ([str str]       [dst dst] 
                         [i (fx+ i cnt)] [j (fx+ j cnt)] 
                         [ac ac]         [c* c*]) 
                   (cond
                     [(pair? c*) 
                      (string-set! dst j (car c*))
                      (g str dst i (fx+ j 1) ac (cdr c*))]
                     [else
                      (string-set! dst j c*)
                      (f str dst (fx+ i 1) (fx+ j 1) ac)]))))]))))
    (let ([n (string-length str)])
      (let f ([str str] [dst (make-string n)] [i 0] [n n] [ac '()])
        (cond
          [($fx= i n) 
           (if (null? ac) 
               dst 
               (extend-length dst ac))]
          [else
           (let* ([cn ($char->fixnum ($string-ref str i))])
             (let ([n/ls
                    (vector-ref adjustment-vector
                      (binary-search cn charcase-search-vector))])
               (cond
                 [(fixnum? n/ls)
                  (string-set! dst i ($fixnum->char ($fx+ cn n/ls)))
                  (f str dst ($fxadd1 i) n ac)]
                 [else
                  (f str dst (fxadd1 i) n 
                     (cons (cons i n/ls) ac))])))]))))

  (define ($string-foldcase str)
    ($string-change-case str string-foldcase-adjustment-vector))

  (define (string-foldcase str)
    (if (string? str)
        ($string-foldcase str)
        (die 'string-foldcase "not a string" str)))

  (define (string-upcase x)
    (if (string? x)
        ($string-change-case x string-upcase-adjustment-vector)
        (die 'string-upcase "not a string" x)))

  (define (string-downcase x)
    (if (string? x)
        ($string-change-case x string-downcase-adjustment-vector)
        (die 'string-downcase "not a string" x)))

  ;;; FIXME: case-insensitive comparison procedures are slow.

  (define string-ci-cmp
    (lambda (who cmp)
      (case-lambda
        [(s1 s2) 
         (if (string? s1)
             (if (string? s2) 
                 (cmp ($string-foldcase s1) ($string-foldcase s2))
                 (die who "not a string" s2))
             (die who "not a string" s1))]
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
                                     (die who "not a string" 
                                       (car s*))]))))
                          (die who "not a string" s2)))])))
             (die who "not a string" s1))])))


  (define string-ci=?  (string-ci-cmp 'string-ci=? string=?))
  (define string-ci<?  (string-ci-cmp 'string-ci<? string<?))
  (define string-ci<=? (string-ci-cmp 'string-ci<=? string<=?))
  (define string-ci>?  (string-ci-cmp 'string-ci>? string>?))
  (define string-ci>=? (string-ci-cmp 'string-ci>=? string>=?))


  )

