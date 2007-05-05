
(library (ikarus predicates)
  (export fixnum? flonum? bignum? number? complex? real? rational? 
          integer? exact?)
  (import 
    (except (ikarus) fixnum? flonum? bignum? number? complex? real?
            rational? integer? exact?)
    (rename (only (ikarus) fixnum? flonum? bignum?) 
            (fixnum? sys:fixnum?)
            (flonum? sys:flonum?)
            (bignum? sys:bignum?)))

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
         (error 'exact? "~s is not a number" x)]))))
