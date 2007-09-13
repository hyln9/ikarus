
(library (ikarus fixnums)
  (export fxzero? fxadd1 fxsub1 fxlognot fx+ fx- fx* fxquotient
          fxremainder fxmodulo fxlogor fxlogand fxlogxor fxsll fxsra
          fx= fx< fx<= fx> fx>= 
          fx=? fx<? fx<=? fx>? fx>=? 
          fixnum->string)
  (import 
    (ikarus system $fx)
    (ikarus system $chars)
    (ikarus system $pairs)
    (ikarus system $strings)
    (except (ikarus) fxzero? fxadd1 fxsub1 fxlognot fx+ fx- fx*
            fxquotient fxremainder fxmodulo fxlogor fxlogand
            fxlogxor fxsll fxsra fx= fx< fx<= fx> fx>=
            fx=? fx<? fx<=? fx>? fx>=? 
            fixnum->string))

  (define fxzero?
    (lambda (x)
      (cond
        [(eq? x 0) #t]
        [(fixnum? x) #f]
        [else (error 'fxzero? "~s is not a fixnum" x)])))
  
  (define fxadd1
    (lambda (n)
      (if (fixnum? n)
          ($fxadd1 n)
          (error 'fxadd1 "~s is not a fixnum" n))))
  
  (define fxsub1 
    (lambda (n) 
      (if (fixnum? n)
          ($fxsub1 n)
          (error 'fxsub1 "~s is not a fixnum" n))))

  (define fxlognot 
    (lambda (x)
      (unless (fixnum? x) 
        (error 'fxlognot "~s is not a fixnum" x))
      ($fxlognot x)))

  (define fx+ 
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fx+ "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fx+ "~s is not a fixnum" y))
      ($fx+ x y)))

  (define fx-
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fx- "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fx- "~s is not a fixnum" y))
      ($fx- x y)))

  (define fx*
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fx* "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fx* "~s is not a fixnum" y))
      ($fx* x y)))
  
  (define false-loop
    (lambda (who ls)
      (if (pair? ls) 
          (if (fixnum? ($car ls)) 
              (false-loop who ($cdr ls))
              (error who "~s is not a fixnum" ($car ls)))
          #f)))

  (define-syntax fxcmp 
    (syntax-rules ()
      [(_ who $op)
       (case-lambda
         [(x y) 
          (unless (fixnum? x)
            (error 'who "~s is not a fixnum" x))
          (unless (fixnum? y)
            (error 'who "~s is not a fixnum" y))
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
                                  (error 'who "~s is not a fixnum" y)))
                            #t))
                      (false-loop 'who ls))
                  (error 'who "~s is not a fixnum" y))
              (error 'who "~s is not a fixnum" x))]
         [(x) 
          (if (fixnum? x) #t (error 'who "~s is not a fixnum" x))])]))
  
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
        (error 'fxquotient "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxquotient "~s is not a fixnum" y))
      (when ($fxzero? y)
        (error 'fxquotient "zero dividend ~s" y))
      ($fxquotient x y))) 
  
  (define fxremainder
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxremainder "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxremainder "~s is not a fixnum" y))
      (when ($fxzero? y)
        (error 'fxremainder "zero dividend ~s" y))
      (let ([q ($fxquotient x y)])
        ($fx- x ($fx* q y)))))
   
  (define fxmodulo
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxmodulo "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxmodulo "~s is not a fixnum" y))
      (when ($fxzero? y)
        (error 'fxmodulo "zero dividend ~s" y))
      ($fxmodulo x y)))

  (define fxlogor
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxlogor "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxlogor "~s is not a fixnum" y))
      ($fxlogor x y)))
  
  (define fxlogxor
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxlogxor "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxlogxor "~s is not a fixnum" y))
      ($fxlogxor x y)))
    
  (define fxlogand
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxlogand "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxlogand "~s is not a fixnum" y))
      ($fxlogand x y)))
   
  (define fxsra
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxsra "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxsra "~s is not a fixnum" y))
      (unless ($fx>= y 0)
        (error 'fxsra "negative shift not allowed, got ~s" y))
      ($fxsra x y)))
   
  (define fxsll
    (lambda (x y) 
      (unless (fixnum? x)
        (error 'fxsll "~s is not a fixnum" x))
      (unless (fixnum? y)
        (error 'fxsll "~s is not a fixnum" y))
      (unless ($fx>= y 0)
        (error 'fxsll "negative shift not allowed, got ~s" y))
      ($fxsll x y))) 

  (module (fixnum->string)
    (define f
      (lambda (n i j)
        (cond
          [($fxzero? n) 
           (values (make-string i) j)]
          [else
           (let ([q ($fxquotient n 10)])
             (call-with-values
               (lambda () (f q ($fxadd1 i) j))
               (lambda (str j)
                 (let ([r ($fx- n ($fx* q 10))])
                   (string-set! str j
                      ($fixnum->char ($fx+ r ($char->fixnum #\0))))
                   (values str ($fxadd1 j))))))])))
    (define fixnum->string
      (lambda (x)
        (unless (fixnum? x) (error 'fixnum->string "~s is not a fixnum" x))
        (cond
          [($fxzero? x) "0"]
          [($fx> x 0) 
           (call-with-values
             (lambda () (f x 0 0))
             (lambda (str j) str))]
          ;;; FIXME: DON'T HARDCODE CONSTANTS
          [($fx= x -536870912) "-536870912"]
          [else
           (call-with-values
             (lambda () (f ($fx- 0 x) 1 1))
             (lambda (str j)
               ($string-set! str 0 #\-)
               str))]))))

  )
