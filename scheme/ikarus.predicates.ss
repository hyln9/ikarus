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


(library (ikarus predicates)

  (export fixnum? flonum? bignum? ratnum? number? complex? real? rational?
          integer? exact? inexact? eof-object? bwp-object? immediate?
          boolean? char? vector? bytevector? string? procedure? null? pair?
          symbol? code? not weak-pair? eq? eqv? equal? boolean=?
          symbol=? finite? infinite? nan? real-valued?
          rational-valued? integer-valued? 
          output-port? input-port? port? transcoder?)

  (import 
    (except (ikarus) fixnum? flonum? bignum? ratnum? number? complex? real?
            rational? integer? exact? inexact? eof-object? bwp-object?
            immediate? boolean? char? vector? bytevector? string? procedure?
            null? pair? weak-pair? symbol? code? not eq? eqv? equal?
            transcoder? port? input-port? output-port? boolean=? symbol=?
            finite? infinite? nan? real-valued? rational-valued? 
            integer-valued?)
    (ikarus system $fx)
    (ikarus system $flonums)
    (ikarus system $pairs)
    (ikarus system $chars)
    (ikarus system $strings)
    (ikarus system $vectors)
    (rename (only (ikarus) fixnum? flonum? bignum? ratnum? eof-object?
                  bwp-object? immediate? boolean? char? vector? string?
                  bytevector? procedure? null? pair? symbol? code? eq?
                  transcoder?
                  port? input-port? output-port?)
            (fixnum? sys:fixnum?)
            (flonum? sys:flonum?)
            (bignum? sys:bignum?)
            (ratnum? sys:ratnum?)
            (eof-object? sys:eof-object?)
            (bwp-object? sys:bwp-object?)
            (immediate? sys:immediate?)
            (boolean? sys:boolean?)
            (char? sys:char?)
            (vector? sys:vector?)
            (bytevector? sys:bytevector?)
            (string? sys:string?)
            (procedure? sys:procedure?)
            (null? sys:null?)
            (pair? sys:pair?)
            (symbol? sys:symbol?)
            (code? sys:code?)
            (eq? sys:eq?)
            (transcoder? sys:transcoder?)
            (port? sys:port?)
            (input-port? sys:input-port?)
            (output-port? sys:output-port?)
            ))

  (define fixnum?
    (lambda (x) (sys:fixnum? x)))
  
  (define bignum? 
    (lambda (x) (sys:bignum? x)))
  
  (define ratnum? 
    (lambda (x) (sys:ratnum? x)))
  
  (define flonum? 
    (lambda (x) (sys:flonum? x)))
  
  (define number?
    (lambda (x)
      (or (sys:fixnum? x)
          (sys:bignum? x)
          (sys:flonum? x)
          (sys:ratnum? x))))
  
  (define complex?
    (lambda (x) (number? x)))
  
  (define real?
    (lambda (x) (number? x)))

  (define real-valued?
    (lambda (x) (number? x)))

  (define rational?
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #t]
        [(sys:flonum? x) ($flonum-rational? x)]
        [else #f])))

  (define rational-valued? 
    (lambda (x) (rational? x)))

  (define integer? 
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #f]
        [(sys:flonum? x) ($flonum-integer? x)]
        [else #f])))

  (define integer-valued? 
    (lambda (x) (integer? x)))

  (define exact?
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #t]
        [(sys:flonum? x) #f]
        [else 
         (error 'exact? "not a number" x)])))
  

  (define inexact?
    (lambda (x) 
      (cond
        [(sys:flonum? x) #t]
        [(sys:fixnum? x) #f]
        [(sys:bignum? x) #f]
        [(sys:ratnum? x) #f]
        [else 
         (error 'inexact? "not a number" x)])))

  (define finite?
    (lambda (x)
      (cond
        [(sys:flonum? x) (flfinite? x)]
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:ratnum? x) #t]
        [else 
         (error 'finite? "not a number" x)])))

  (define infinite?
    (lambda (x)
      (cond
        [(sys:flonum? x) (flinfinite? x)]
        [(sys:fixnum? x) #f]
        [(sys:bignum? x) #f]
        [(sys:ratnum? x) #f]
        [else 
         (error 'infinite? "not a number" x)])))

  (define nan?
    (lambda (x)
      (cond
        [(sys:flonum? x) (flnan? x)]
        [(sys:fixnum? x) #f]
        [(sys:bignum? x) #f]
        [(sys:ratnum? x) #f]
        [else 
         (error 'nan? "not a number" x)])))




  (define eof-object? (lambda (x) (sys:eof-object? x)))
  (define bwp-object? (lambda (x) (sys:bwp-object? x)))
  (define transcoder? (lambda (x) (sys:transcoder? x)))
  (define immediate? (lambda (x) (sys:immediate? x)))
  (define boolean? (lambda (x) (sys:boolean? x)))
  (define char? (lambda (x) (sys:char? x)))
  (define vector? (lambda (x) (sys:vector? x)))
  (define bytevector? (lambda (x) (sys:bytevector? x)))
  (define string? (lambda (x) (sys:string? x)))
  (define procedure? (lambda (x) (sys:procedure? x)))
  (define null? (lambda (x) (sys:null? x)))
  (define pair? (lambda (x) (sys:pair? x)))
  (define symbol? (lambda (x) (sys:symbol? x)))
  (define code? (lambda (x) (sys:code? x)))


  (define weak-pair?
    (lambda (x)
      (and (pair? x)
           (foreign-call "ikrt_is_weak_pair" x))))

  (define not (lambda (x) (if x #f #t)))

  (define eq? (lambda (x y) (sys:eq? x y)))

  (define eqv?
    (lambda (x y)
      (or (sys:eq? x y)
          (and (number? x) (number? y) (= x y)))))

  (define boolean=?
    (lambda (x y) 
      (if (sys:boolean? x)
          (if (sys:eq? x y) 
              #t
              (if (sys:boolean? y) 
                  #f
                  (error 'boolean=? "not a boolean" y)))
          (error 'boolean=? "not a boolean" x))))


  (define symbol=?
    (lambda (x y) 
      (if (sys:symbol? x)
          (if (sys:eq? x y) 
              #t
              (if (sys:symbol? y) 
                  #f
                  (error 'symbol=? "not a symbol" y)))
          (error 'symbol=? "not a symbol" x))))

  (module (equal?)
    (define vector-loop
      (lambda (x y i n)
        (or ($fx= i n)
            (and (equal? ($vector-ref x i) ($vector-ref y i))
                 (vector-loop x y ($fxadd1 i) n)))))
    (define string-loop
      (lambda (x y i n)
        (or ($fx= i n)
            (and ($char= ($string-ref x i) ($string-ref y i))
                 (string-loop x y ($fxadd1 i) n)))))
    (define equal?
      (lambda (x y)
        (cond
          [(sys:eq? x y) #t]
          [(pair? x) 
           (and (pair? y)
                (equal? ($car x) ($car y))
                (equal? ($cdr x) ($cdr y)))]
          [(vector? x)
           (and (vector? y)
                (let ([n ($vector-length x)])
                  (and ($fx= n ($vector-length y))
                       (vector-loop x y 0 n))))]
          [(string? x)
           (and (string? y)
                (let ([n ($string-length x)])
                  (and ($fx= n ($string-length y))
                       (string-loop x y 0 n))))]
          [(number? x) (and (number? y) (= x y))]
          [(sys:bytevector? x)
           (and (sys:bytevector? y) (bytevector=? x y))]
          [else #f]))))

  (define port?
    (lambda (x) (sys:port? x)))
  (define input-port?
    (lambda (x) (sys:input-port? x)))
  (define output-port?
    (lambda (x) (sys:output-port? x)))
 

  )
