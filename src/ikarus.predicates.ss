
(library (ikarus predicates)
  (export fixnum? flonum? bignum? number? complex? real? rational? 
          integer? exact? eof-object?)
  (import 
    (except (ikarus) fixnum? flonum? bignum? number? complex? real?
            rational? integer? exact? eof-object?)
    (rename (only (ikarus) fixnum? flonum? bignum? eof-object?) 
            (fixnum? sys:fixnum?)
            (flonum? sys:flonum?)
            (bignum? sys:bignum?)
            (eof-object? sys:eof-object?)
            ))

  (define fixnum?
    (lambda (x) (sys:fixnum? x)))
  
  (define bignum? 
    (lambda (x) (sys:bignum? x)))
  
  (define flonum? 
    (lambda (x) (sys:flonum? x)))
  
  (define number?
    (lambda (x)
      (or (sys:fixnum? x)
          (sys:bignum? x)
          (sys:flonum? x))))
  
  (define complex?
    (lambda (x) (number? x)))
  
  (define real?
    (lambda (x) (number? x)))
  
  (define rational?
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:flonum? x) #f]
        [else (error 'rational? "~s is not a number" x)])))

  (define integer? 
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:flonum? x) (error 'integer "dunno for ~s" x)]
        [else #f])))

  (define exact?
    (lambda (x) 
      (cond
        [(sys:fixnum? x) #t]
        [(sys:bignum? x) #t]
        [(sys:flonum? x) #f]
        [else 
         (error 'exact? "~s is not a number" x)])))
  
  (define eof-object?
    (lambda (x)
      (sys:eof-object? x)))

  )
