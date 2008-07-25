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

(library (ikarus enumerations)
  (export make-enumeration enum-set-universe enum-set-indexer
    enum-set-constructor enum-set->list enum-set-member?
    enum-set-subset? enum-set=? enum-set-union enum-set-difference
    enum-set-intersection enum-set-complement enum-set-projection
    make-file-options)
  (import 
    (except (ikarus)
      make-enumeration enum-set-universe enum-set-indexer
      enum-set-constructor enum-set->list enum-set-member?
      enum-set-subset? enum-set=? enum-set-union enum-set-difference
      enum-set-intersection enum-set-complement
      enum-set-projection
      make-file-options))

  (define-record-type enum
    (fields g univ values)
    (opaque #f) (sealed #t)
    (nongenerative))
  
  (define (remove-dups ls)
    (cond
      [(null? ls) '()]
      [else (cons (car ls) (remq (car ls) (cdr ls)))]))


  (define (make-enumeration ls) 
    (unless (and (list? ls) (for-all symbol? ls))
      (die 'make-enumeration "not a list of symbols" ls))
    (let ([u (remove-dups ls)])
      (make-enum (gensym) u u)))
  
  (define (enum-set-universe x)
    (unless (enum? x) 
      (die 'enum-set-universe "not an enumeration" x))
    (let ([u (enum-univ x)])
      (make-enum (enum-g x) u u)))
  
  (define (enum-set-indexer x)
    (unless (enum? x) 
      (die 'enum-set-indexer "not an enumeration" x))
    (lambda (s)
      (unless (symbol? s) 
        (die 'enum-set-indexer "not a symbol" s))
      (let f ([s s] [i 0] [ls (enum-univ x)])
        (cond
          [(pair? ls) 
           (if (eq? s (car ls)) 
               i
               (f s (+ i 1) (cdr ls)))]
          [else #f]))))
  
  (define (enum-set-constructor x)
    (unless (enum? x)
      (die 'enum-set-constructor "not an enumeration" x))
    (lambda (ls)
      (unless (and (list? ls) (for-all symbol? ls))
        (die 'enum-set-constructor "not a list of symbols" ls))
      (for-each
        (lambda (s)
          (unless (memq s (enum-univ x))
            (die 'enum-set-constructor "not in the universe" s x)))
        ls)
      (let ([idx (enum-set-indexer x)])
        (make-enum (enum-g x) (enum-univ x)
          (map car
            (list-sort (lambda (a b) (< (cdr a) (cdr b)))
              (map (lambda (x) (cons x (idx x)))
                   ls)))))))
    
  (define (enum-set->list x)
    (unless (enum? x) 
      (die 'enum-set->list "not an enumeration" x))
    (let ([idx (enum-set-indexer x)]
          [ls (enum-values x)])
      (map car
        (list-sort (lambda (a b) (< (cdr a) (cdr b)))
          (map (lambda (x) (cons x (idx x)))
               ls)))))
  
  (define (enum-set-member? s x) 
    (if (enum? x)
        (if (memq s (enum-values x))
            #t
            (if (symbol? s) 
                #f 
                (die 'enum-set-member? "not a symbol" s)))
        (die 'enum-set-member? "not an enumeration" x)))
  
  (define (enum-set-subset? x1 x2) 
    (define (subset? s1 s2) 
      (or (null? s1) 
          (and (memq (car s1) s2)
               (subset? (cdr s1) s2))))
    (if (enum? x1) 
        (if (enum? x2) 
            (and (subset? (enum-values x1) (enum-values x2))
                 (or (eq? (enum-g x1) (enum-g x2))
                     (subset? (enum-univ x1) (enum-univ x2))))
            (die 'enum-set-subset? "not an enumeration" x2))
        (die 'enum-set-subset? "not an enumeration" x1)))
  
  (define (enum-set=? x1 x2) 
    (define (subset? s1 s2) 
      (or (null? s1) 
          (and (memq (car s1) s2)
               (subset? (cdr s1) s2))))
    (if (enum? x1) 
        (if (enum? x2) 
            (and (subset? (enum-values x1) (enum-values x2))
                 (subset? (enum-values x2) (enum-values x1))
                 (or (eq? (enum-g x1) (enum-g x2))
                     (and (subset? (enum-univ x1) (enum-univ x2))
                          (subset? (enum-univ x2) (enum-univ x1)))))
            (die 'enum-set=? "not an enumeration" x2))
        (die 'enum-set=? "not an enumeration" x1)))
  
  (define (enum-set-op x1 x2 who combine)
    (if (enum? x1) 
        (if (enum? x2) 
            (let ([g (enum-g x1)] [u (enum-univ x1)])
              (if (eq? g (enum-g x2))
                  (make-enum g u (combine u (enum-values x1) (enum-values x2)))
                  (die who 
                    "enum sets have different enumeration types" x1 x2)))
            (die who "not an enumeration" x2))
        (die who "not an enumeration" x1)))
  
  (define (enum-set-union x1 x2)
    (define (union u s1 s2) 
      (if (pair? s1) 
          (if (pair? s2) 
              (let ([x (car u)])
                (if (eq? x (car s1))
                    (cons x 
                      (union (cdr u) (cdr s1) 
                        (if (eq? x (car s2)) (cdr s2) s2)))
                    (if (eq? x (car s2)) 
                        (cons x (union (cdr u) s1 (cdr s2)))
                        (union (cdr u) s1 s2))))
              s1)
          s2))
    (enum-set-op x1 x2 'enum-set-union union))
  
  (define (enum-set-intersection x1 x2)
    (define (intersection u s1 s2) 
      (if (pair? s1) 
          (if (pair? s2) 
              (let ([x (car u)])
                (if (eq? x (car s1))
                    (if (eq? x (car s2)) 
                        (cons x (intersection (cdr u) (cdr s1) (cdr s2)))
                        (intersection (cdr u) (cdr s1) s2))
                    (intersection (cdr u) s1
                      (if (eq? x (car s2)) (cdr s2) s2))))
              '())
          '()))
    (enum-set-op x1 x2 'enum-set-intersection intersection))
  
  (define (enum-set-difference x1 x2)
    (define (difference u s1 s2) 
      (if (pair? s1) 
          (if (pair? s2)
              (let ([x (car u)])
                (if (eq? x (car s1))
                    (if (eq? x (car s2)) 
                        (difference (cdr u) (cdr s1) (cdr s2))
                        (cons x (difference (cdr u) (cdr s1) s2)))
                    (difference (cdr u) s1
                      (if (eq? x (car s2)) (cdr s2) s2))))
              s1)
          '()))
    (enum-set-op x1 x2 'enum-set-difference difference))
  
  (define (enum-set-complement x)
    (define (complement u s) 
      (if (pair? s) 
          (let ([x (car u)])
            (if (eq? x (car s)) 
                (complement (cdr u) (cdr s))
                (cons x (complement (cdr u) s))))
          u))
    (if (enum? x) 
        (let ([g (enum-g x)] [u (enum-univ x)])
           (make-enum g u (complement u (enum-values x))))
        (die 'enum-set-complement "not an enumeration" x)))
  
  (define (enum-set-projection x1 x2)
    (define (combine u s) 
      (if (pair? u)
          (let ([x (car u)])
            (if (memq x s) 
                (cons x (combine (cdr u) s))
                (combine (cdr u) s)))
          '()))
    (if (enum? x1)
        (if (enum? x2) 
            (let ([g (enum-g x2)])
              (if (eq? g (enum-g x1))
                  x1
                  (let ([u (enum-univ x2)] [s (enum-values x1)])
                    (if (null? s) 
                        (make-enum g u '())
                        (make-enum g u (combine u s))))))
            (die 'enum-set-projection "not an enumeration" x2))
        (die 'enum-set-projection "not an enumeration" x1)))

  (define make-file-options
    (enum-set-constructor
      (make-enumeration 
        '(no-create no-fail no-truncate))))

)

#!eof

(define (trace-equal? x y) (equal? x y))

(assert 
  (trace-equal? 
    (let* ([e (make-enumeration '(red green blue))]
           [i (enum-set-indexer e)])
      (list (i 'red) (i 'green) (i 'blue) (i 'yellow)))
    '(0 1 2 #f)))

(assert 
  (trace-equal? 
    (let* ([e (make-enumeration '(red green blue))]
           [c (enum-set-constructor e)])
      (enum-set->list (c '(blue red))))
    '(red blue)))

(assert 
  (trace-equal? 
    (let* ([e (make-enumeration '(red green blue))]
           [c (enum-set-constructor e)])
      (list 
        (enum-set-member? 'blue (c '(red blue)))
        (enum-set-member? 'green (c '(red blue)))
        (enum-set-subset? (c '(red blue)) e)
        (enum-set-subset? (c '(red blue)) (c '(blue red)))
        (enum-set-subset? (c '(red blue)) (c '(red)))
        (enum-set=? (c '(red blue)) (c '(blue red)))))
    '(#t #f #t #t #f #t)))

(assert 
  (trace-equal? 
    (let* ([e (make-enumeration '(red green blue))]
           [c (enum-set-constructor e)])
      (list 
        (enum-set->list (enum-set-union (c '(blue)) (c '(red))))
        (enum-set->list (enum-set-intersection (c '(red green)) (c '(red blue))))
        (enum-set->list (enum-set-difference (c '(red green)) (c '(red blue))))
        (enum-set->list (enum-set-complement (c '(red))))))
    '((red blue) (red) (green) (green blue))))

(assert 
  (trace-equal? 
    (let* ([e1 (make-enumeration '(red green blue black))]
           [e2 (make-enumeration '(red black white))])
      (enum-set->list (enum-set-projection e1 e2)))
    '(red black)))


#!eof
(define-condition-type &c &condition
  make-c c?
  (x c-x))

(pretty-print (make-c 'hello))
(assert (c? (make-c 'hello)))
(assert (condition? (make-c 'hello)))

(pretty-print (record-type-descriptor &no-nans))

(assert (no-nans-violation? (make-no-nans-violation)))

(pretty-print 
  (condition
    (condition
      ((record-constructor (record-constructor-descriptor &message))
       "Hello")
      (condition)
      (condition (make-message-condition "hi"))
      (make-message-condition "there"))))
