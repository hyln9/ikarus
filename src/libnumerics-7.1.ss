
(let ()
  (define (generic+ a b)
    (cond
      [(fixnum? a)
       (cond
         [(fixnum? b) (foreign-call "iknum_add_fx_fx" a b)]
         [(bignum? b) (foreign-call "iknum_add_fx_bn" a b)]
         [else (error '+ "~s is not a number" b)])]
      [(bignum? a)
       (cond
         [(fixnum? b) (foreign-call "iknum_add_fx_bn" b a)]
         [(bignum? b) (foreign-call "iknum_add_bn_bn" a b)]
         [else (error '+ "~s is not a number" b)])]
      [else (error '+ "~s is not a number" a)]))

  (primitive-set! '+
    (case-lambda 
      [(a b) (generic+ a b)]
      [(a b c) (generic+ a (generic+ b c))]
      [(a) (if (number? a) a (error '+ "~s is not a number" a))]
      [() 0]
      [(a b . rest)
       (let f ([a a] [b b] [rest rest])
         (generic+ a
            (if (null? rest)
                b
                (f b ($car rest) ($cdr rest)))))]))
  
  (primitive-set! 'add1
    (lambda (a)
      (cond
        [(fixnum? a) 
         (if ($fx< a (most-positive-fixnum))
             ($fxadd1 a)
             (foreign-call "iknum_add_fx_fx" a 1))]
        [(bignum? a)
         (foreign-call "iknum_add_fx_bn" 1 a)]
        [else (error 'add1 "~s is not a number" a)])))
  
  (primitive-set! 'sub1
    (lambda (a)
      (cond
        [(fixnum? a) 
         (if ($fx> a (most-negative-fixnum))
             ($fxsub1 a)
             (foreign-call "iknum_add_fx_fx" a -1))]
        [(bignum? a)
         (foreign-call "iknum_add_fx_bn" -1 a)]
        [else (error 'add1 "~s is not a number" a)])))

  )

