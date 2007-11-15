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


(library (ikarus fixnums)
  (export fxzero? fxadd1 fxsub1 fxlognot fx+ fx- fx* fxquotient
          fx+/carry fx*/carry fx-/carry
          fxremainder fxmodulo fxlogor fxlogand fxlogxor fxsll fxsra
          fx= fx< fx<= fx> fx>= 
          fx=? fx<? fx<=? fx>? fx>=? 
          fxior fxand fxxor fxnot fxif
          fxpositive? fxnegative?
          fxeven? fxodd?
          fixnum->string 
          fxarithmetic-shift-left fxarithmetic-shift-right fxarithmetic-shift
          fxmin fxmax
          error@fx+)
  (import 
    (ikarus system $fx)
    (ikarus system $chars)
    (ikarus system $pairs)
    (ikarus system $strings)
    (prefix (only (ikarus) fx+) sys:)
    (except (ikarus) fxzero? fxadd1 fxsub1 fxlognot fx+ fx- fx*
            fxquotient fxremainder fxmodulo fxlogor fxlogand
            fxlogxor fxsll fxsra fx= fx< fx<= fx> fx>=
            fx=? fx<? fx<=? fx>? fx>=? 
            fxior fxand fxxor fxnot fxif
            fxpositive? fxnegative?
            fxeven? fxodd?
            fxarithmetic-shift-left fxarithmetic-shift-right fxarithmetic-shift
            fx+/carry fx*/carry fx-/carry
            fxmin fxmax
            fixnum->string))

  (define fxzero?
    (lambda (x)
      (cond
        [(eq? x 0) #t]
        [(fixnum? x) #f]
        [else (error 'fxzero? "not a fixnum" x)])))
  
  (define fxadd1
    (lambda (n)
      (if (fixnum? n)
          ($fxadd1 n)
          (error 'fxadd1 "not a fixnum" n))))
  
  (define fxsub1 
    (lambda (n) 
      (if (fixnum? n)
          ($fxsub1 n)
          (error 'fxsub1 "not a fixnum" n))))

  (define fxlognot 
    (lambda (x)
      (unless (fixnum? x) 
        (error 'fxlognot "not a fixnum" x))
      ($fxlognot x)))

  (define fxnot 
    (lambda (x)
      (unless (fixnum? x) 
        (error 'fxnot "not a fixnum" x))
      ($fxlognot x)))
  
  (define error@fx+
    (lambda (x y) 
      (if (fixnum? x) 
          (if (fixnum? y) 
              (error 'fx+ "overflow when adding numbers" x y)
              (error 'fx+ "not a fixnum" y))
          (error 'fx+ "not a fixnum" x))))

  (define fx+ 
    (lambda (x y) 
      (sys:fx+ x y)))

  (define fx-
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fx- "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fx- "not a fixnum" y))
      ($fx- x y)))

  (define fx*
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fx* "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fx* "not a fixnum" y))
      ($fx* x y)))
  

  (define false-loop
    (lambda (who ls)
      (if (pair? ls) 
          (if (fixnum? ($car ls)) 
              (false-loop who ($cdr ls))
              (error who "not a fixnum" ($car ls)))
          #f)))

  (define-syntax fxcmp 
    (syntax-rules ()
      [(_ who $op)
       (case-lambda
         [(x y) 
          (unless (fixnum? x)
            (error 'who "not a fixnum" x))
          (unless (fixnum? y)
            (error 'who "not a fixnum" y))
          ($op x y)]
         [(x y . ls)
          (if (fixnum? x)
              (if (fixnum? y) 
                  (if ($op x y) 
                      (let f ([x y] [ls ls]) 
                        (if (pair? ls) 
                            (let ([y ($car ls)] [ls ($cdr ls)])
                              (if (fixnum? y) 
                                  (if ($op x y) 
                                      (f y ls)
                                      (false-loop 'who ls))
                                  (error 'who "not a fixnum" y)))
                            #t))
                      (false-loop 'who ls))
                  (error 'who "not a fixnum" y))
              (error 'who "not a fixnum" x))]
         [(x) 
          (if (fixnum? x) #t (error 'who "not a fixnum" x))])]))
  
  (define fx=   (fxcmp fx= $fx=))
  (define fx<   (fxcmp fx< $fx<))
  (define fx<=  (fxcmp fx<= $fx<=))
  (define fx>   (fxcmp fx> $fx>))
  (define fx>=  (fxcmp fx>= $fx>=))
  (define fx=?  (fxcmp fx=? $fx=))
  (define fx<?  (fxcmp fx<? $fx<))
  (define fx<=? (fxcmp fx<=? $fx<=))
  (define fx>?  (fxcmp fx>? $fx>))
  (define fx>=? (fxcmp fx>=? $fx>=))


  (define fxquotient
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxquotient "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxquotient "not a fixnum" y))
      (when ($fxzero? y)
        (error 'fxquotient "zero dividend" y))
      ($fxquotient x y))) 
  
  (define fxremainder
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxremainder "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxremainder "not a fixnum" y))
      (when ($fxzero? y)
        (error 'fxremainder "zero dividend" y))
      (let ([q ($fxquotient x y)])
        ($fx- x ($fx* q y)))))
   
  (define fxmodulo
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxmodulo "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxmodulo "not a fixnum" y))
      (when ($fxzero? y)
        (error 'fxmodulo "zero dividend" y))
      ($fxmodulo x y)))

  (define-syntax fxbitop
    (syntax-rules ()
      [(_ who $op identity)
       (case-lambda 
         [(x y) 
          (if (fixnum? x) 
              (if (fixnum? y) 
                  ($op x y)
                  (error 'who "not a fixnum" y))
              (error 'who "not a fixnum" x))]
         [(x y . ls) 
          (if (fixnum? x)
              (if (fixnum? y) 
                  (let f ([a ($op x y)] [ls ls])
                    (cond
                      [(pair? ls) 
                       (let ([b ($car ls)])
                         (if (fixnum? b) 
                             (f ($op a b) ($cdr ls))
                             (error 'who "not a fixnum" b)))]
                      [else a]))
                  (error 'who "not a fixnum" y))
              (error 'who "not a fixnum" x))]
         [(x) (if (fixnum? x) x (error 'who "not a fixnum" x))]
         [()   identity])]))

  (define fxlogor (fxbitop fxlogor $fxlogor 0))
  (define fxlogand (fxbitop fxlogand $fxlogand -1))
  (define fxlogxor (fxbitop fxlogxor $fxlogxor 0))
  (define fxior (fxbitop fxior $fxlogor 0))
  (define fxand (fxbitop fxand $fxlogand -1))
  (define fxxor (fxbitop fxxor $fxlogxor 0))
  
  (define (fxif x y z)
    (if (fixnum? x)
        (if (fixnum? y) 
            (if (fixnum? z) 
                ($fxlogor 
                  ($fxlogand x y) 
                  ($fxlogand ($fxlognot x) z))
                (error 'fxif "not a fixnum" z))
            (error 'fxif "not a fixnum" y))
        (error 'fxif "not a fixnum" x)))

  (define fxsra
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxsra "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxsra "not a fixnum" y))
      (unless ($fx>= y 0)
        (error 'fxsra "negative shift not allowed" y))
      ($fxsra x y)))
   

  (define fxarithmetic-shift-right
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxarithmetic-shift-right "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxarithmetic-shift-right "not a fixnum" y))
      (unless ($fx>= y 0)
        (error 'fxarithmetic-shift-right "negative shift not allowed" y))
      ($fxsra x y)))

  (define fxsll
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxsll "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxsll "not a fixnum" y))
      (unless ($fx>= y 0)
        (error 'fxsll "negative shift not allowed" y))
      ($fxsll x y))) 


  (define fxarithmetic-shift-left
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxarithmetic-shift-left "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxarithmetic-shift-left "not a fixnum" y))
      (unless ($fx>= y 0)
        (error 'fxarithmetic-shift-left "negative shift not allowed" y))
      ($fxsll x y)))

  (define fxarithmetic-shift
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxarithmetic-shift "not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxarithmetic-shift "not a fixnum" y))
      (if ($fx>= y 0)
          ($fxsll x y)
          (if ($fx< x -100) ;;; arbitrary number < (fixnum-width)
              ($fxsra x 32)
              ($fxsra x ($fx- 0 y))))))

  (define (fxpositive? x)
    (if (fixnum? x) 
        ($fx> x 0)
        (error 'fxpositive? "not a fixnum" x)))

  (define (fxnegative? x)
    (if (fixnum? x) 
        ($fx< x 0)
        (error 'fxnegative? "not a fixnum" x)))
  
  (define (fxeven? x)
    (if (fixnum? x) 
        ($fxzero? ($fxlogand x 1))
        (error 'fxeven? "not a fixnum" x)))

  (define (fxodd? x)
    (if (fixnum? x) 
        (not ($fxzero? ($fxlogand x 1)))
        (error 'fxodd? "not a fixnum" x)))

  (define fxmin
    (case-lambda 
      [(x y) 
       (if (fixnum? x) 
           (if (fixnum? y) 
               (if ($fx< x y) x y)
               (error 'fxmin "not a fixnum" y))
           (error 'fxmin "not a fixnum" x))]
      [(x y z . ls)
       (fxmin (fxmin x y) 
         (if (fixnum? z) 
             (let f ([z z] [ls ls])
               (if (null? ls)
                   z
                   (let ([a ($car ls)])
                     (if (fixnum? a) 
                         (if ($fx< a z) 
                             (f a ($cdr ls))
                             (f z ($cdr ls)))
                         (error 'fxmin "not a fixnum" a)))))
             (error 'fxmin "not a fixnum" z)))]
      [(x) (if (fixnum? x) x (error 'fxmin "not a fixnum" x))]))

  (define fxmax
    (case-lambda 
      [(x y)
       (if (fixnum? x) 
           (if (fixnum? y) 
               (if ($fx> x y) x y)
               (error 'fxmax "not a fixnum" y))
           (error 'fxmax "not a fixnum" x))]
      [(x y z . ls)
       (fxmax (fxmax x y) 
         (if (fixnum? z) 
             (let f ([z z] [ls ls])
               (if (null? ls)
                   z
                   (let ([a ($car ls)])
                     (if (fixnum? a) 
                         (if ($fx> a z)
                             (f a ($cdr ls))
                             (f z ($cdr ls)))
                         (error 'fxmax "not a fixnum" a)))))
             (error 'fxmax "not a fixnum" z)))]
      [(x) (if (fixnum? x) x (error 'fxmax "not a fixnum" x))]))

  (define (fx*/carry fx1 fx2 fx3)
    (let ([s0 ($fx+ ($fx* fx1 fx2) fx3)])
      (values 
        s0
        (sra (+ (* fx1 fx2) (- fx3 s0)) (fixnum-width)))))
  
  (define (fx+/carry fx1 fx2 fx3)
    (let ([s0 ($fx+ ($fx+ fx1 fx2) fx3)])
      (values 
        s0
        (sra (+ (+ fx1 fx2) (- fx3 s0)) (fixnum-width)))))
  
  (define (fx-/carry fx1 fx2 fx3)
    (let ([s0 ($fx- ($fx- fx1 fx2) fx3)])
      (values 
        s0
        (sra (- (- fx1 fx2) (+ s0 fx3)) (fixnum-width)))))

  (module (fixnum->string)
    (define mapping-string "0123456789ABCDEF")
    (define f
      (lambda (n i j radix)
        (cond
          [($fxzero? n) 
           (values (make-string i) j)]
          [else
           (let* ([q ($fxquotient n radix)]
                  [c ($string-ref mapping-string 
                       ($fx- n ($fx* q radix)))])
             (call-with-values
               (lambda () (f q ($fxadd1 i) j radix))
               (lambda (str j)
                 (string-set! str j c)
                 (values str ($fxadd1 j)))))])))
    (define $fixnum->string
      (lambda (x radix)
        (cond
          [($fxzero? x) "0"]
          [($fx> x 0)
           (call-with-values
             (lambda () (f x 0 0 radix))
             (lambda (str j) str))]
          [($fx= x (least-fixnum))
           (string-append
             ($fixnum->string ($fxquotient x radix) radix)
             ($fixnum->string ($fx- radix ($fxmodulo x radix)) radix))]
          [else
           (call-with-values
             (lambda () (f ($fx- 0 x) 1 1 radix))
             (lambda (str j)
               ($string-set! str 0 #\-)
               str))])))
    (define fixnum->string
      (case-lambda
        [(x) 
         (unless (fixnum? x) (error 'fixnum->string "not a fixnum" x))
         ($fixnum->string x 10)]
        [(x r) 
         (unless (fixnum? x) (error 'fixnum->string "not a fixnum" x))
         (case r
           [(2)  ($fixnum->string x 2)]
           [(8)  ($fixnum->string x 8)]
           [(10) ($fixnum->string x 10)]
           [(16) ($fixnum->string x 16)]
           [else (error 'fixnum->string "invalid radix" r)])])))


  )

(library (ikarus fixnums div-and-mod)
  (export fxdiv fxmod fxdiv-and-mod fxdiv0 fxmod0 fxdiv0-and-mod0)
  (import 
    (ikarus system $fx)
    (except (ikarus) fxdiv fxmod fxdiv-and-mod fxdiv0 fxmod0 fxdiv0-and-mod0))

  (define ($fxdiv-and-mod n m)
    (let ([d0 ($fxquotient n m)])
      (let ([m0 ($fx- n ($fx* d0 m))])
        (if ($fx>= m0 0)
            (values d0 m0)
            (if ($fx>= m 0)
                (values ($fx- d0 1) ($fx+ m0 m))
                (values ($fx+ d0 1) ($fx- m0 m)))))))

  (define ($fxdiv n m)
    (let ([d0 ($fxquotient n m)])
      (if ($fx>= n ($fx* d0 m))
          d0
          (if ($fx>= m 0)
              ($fx- d0 1)
              ($fx+ d0 1)))))

  (define ($fxmod n m)
    (let ([d0 ($fxquotient n m)])
      (let ([m0 ($fx- n ($fx* d0 m))])
        (if ($fx>= m0 0)
            m0
            (if ($fx>= m 0)
                ($fx+ m0 m)
                ($fx- m0 m))))))

  (define (fxdiv-and-mod x y)
    (if (fixnum? x) 
        (if (fixnum? y) 
            (if ($fx= y 0) 
                (error 'fxdiv-and-mod "division by 0")
                ($fxdiv-and-mod x y))
            (error 'fxdiv-and-mod "not a fixnum" y))
        (error 'fxdiv-and-mod "not a fixnum" x)))

  (define (fxdiv x y)
    (if (fixnum? x) 
        (if (fixnum? y) 
            (if ($fx= y 0) 
                (error 'fxdiv "division by 0")
                ($fxdiv x y))
            (error 'fxdiv "not a fixnum" y))
        (error 'fxdiv "not a fixnum" x)))

  (define (fxmod x y)
    (if (fixnum? x) 
        (if (fixnum? y) 
            (if ($fx= y 0) 
                (error 'fxmod "modision by 0")
                ($fxmod x y))
            (error 'fxmod "not a fixnum" y))
        (error 'fxmod "not a fixnum" x)))

  (define ($fxdiv0-and-mod0 n m)
    (let ([d0 (quotient n m)])
      (let ([m0 (- n (* d0 m))])
        (if (>= m 0)
            (if (< (* m0 2) m)
                (if (<= (* m0 -2) m)
                    (values d0 m0)
                    (values (- d0 1) (+ m0 m)))
                (values (+ d0 1) (- m0 m)))
            (if (> (* m0 -2) m)
                (if (>= (* m0 2) m)
                    (values d0 m0)
                    (values (+ d0 1) (- m0 m)))
                (values (- d0 1) (+ m0 m)))))))

  (define ($fxdiv0 n m)
    (let ([d0 (quotient n m)])
      (let ([m0 (- n (* d0 m))])
        (if (>= m 0)
            (if (< (* m0 2) m)
                (if (<= (* m0 -2) m)
                    d0
                    (- d0 1))
                (+ d0 1))
            (if (> (* m0 -2) m)
                (if (>= (* m0 2) m)
                    d0
                    (+ d0 1))
                (- d0 1))))))

  (define ($fxmod0 n m)
    (let ([d0 (quotient n m)])
      (let ([m0 (- n (* d0 m))])
        (if (>= m 0)
            (if (< (* m0 2) m)
                (if (<= (* m0 -2) m)
                    m0
                    (+ m0 m))
                (- m0 m))
            (if (> (* m0 -2) m)
                (if (>= (* m0 2) m)
                    m0
                    (- m0 m))
                (+ m0 m))))))

  (define (fxdiv0-and-mod0 x y)
    (if (fixnum? x)
        (if (fixnum? y)
            (if ($fx= y 0)
                (error 'fxdiv0-and-mod0 "division by 0")
                (let-values ([(d m) ($fxdiv0-and-mod0 x y)])
                  (if (and (fixnum? d) (fixnum? m))
                      (values d m)
                      (error 'fxdiv0-and-mod0 
                        "results not representable as fixnums"
                        x y))))
            (error 'fxdiv0-and-mod0 "not a fixnum" y))
        (error 'fxdiv0-and-mod0 "not a fixnum" x)))

  (define (fxdiv0 x y)
    (if (fixnum? x)
        (if (fixnum? y)
            (if ($fx= y 0)
                (error 'fxdiv0 "division by 0")
                (let ([d ($fxdiv0 x y)])
                  (if (fixnum? d)
                      d
                      (error 'fxdiv0 
                        "result not representable as fixnum"
                        x y))))
            (error 'fxdiv0 "not a fixnum" y))
        (error 'fxdiv0 "not a fixnum" x)))

  (define (fxmod0 x y)
    (if (fixnum? x)
        (if (fixnum? y)
            (if ($fx= y 0)
                (error 'fxmod0 "division by 0")
                (let ([d ($fxmod0 x y)])
                  (if (fixnum? d)
                      d
                      (error 'fxmod0 
                        "result not representable as fixnum"
                        x y))))
            (error 'fxmod0 "not a fixnum" y))
        (error 'fxmod0 "not a fixnum" x)))
  )


