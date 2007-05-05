
(library (ikarus predicates)

  (export fixnum? flonum? bignum? number? complex? real? rational?
          integer? exact? eof-object? immediate? boolean? char?
          vector? string? procedure? null? pair? symbol? not) 

  (import 

    (except (ikarus) fixnum? flonum? bignum? number? complex? real?
            rational? integer? exact? eof-object?  immediate?
            boolean?  char?  vector?  string?  procedure?  null?
            pair? symbol? not)

    (rename (only (ikarus) fixnum? flonum? bignum? eof-object?
              immediate? boolean? char? vector? string? procedure? 
              null? pair? symbol?)
            (fixnum? sys:fixnum?)
            (flonum? sys:flonum?)
            (bignum? sys:bignum?)
            (eof-object? sys:eof-object?)
            (immediate? sys:immediate?)
            (boolean? sys:boolean?)
            (char? sys:char?)
            (vector? sys:vector?)
            (string? sys:string?)
            (procedure? sys:procedure?)
            (null? sys:null?)
            (pair? sys:pair?)
            (symbol? sys:symbol?)))

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
  
  (define eof-object? (lambda (x) (sys:eof-object? x)))
  (define immediate? (lambda (x) (sys:immediate? x)))
  (define boolean? (lambda (x) (sys:boolean? x)))
  (define char? (lambda (x) (sys:char? x)))
  (define vector? (lambda (x) (sys:vector? x)))
  (define string? (lambda (x) (sys:string? x)))
  (define procedure? (lambda (x) (sys:procedure? x)))
  (define null? (lambda (x) (sys:null? x)))
  (define pair? (lambda (x) (sys:pair? x)))
  (define symbol? (lambda (x) (sys:symbol? x)))

  (define not (lambda (x) (if x #f #t)))
  )
