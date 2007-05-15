
(library (ikarus predicates)

  (export fixnum? flonum? bignum? number? complex? real? rational?
          integer? exact? eof-object? bwp-object? immediate?
          boolean? char? vector? bytevector? string? procedure? null? pair?
          symbol? code? not weak-pair? eq? eqv? equal?) 

  (import 

    (except (ikarus) fixnum? flonum? bignum? number? complex? real?
            rational? integer? exact? eof-object? bwp-object?
            immediate? boolean? char? vector? bytevector? string? procedure?
            null? pair? weak-pair? symbol? code? not eq? eqv? equal?
            port? input-port? output-port?)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $chars)
    (ikarus system $strings)
    (ikarus system $vectors)
    (rename (only (ikarus) fixnum? flonum? bignum? eof-object?
                  bwp-object? immediate? boolean? char? vector? string?
                  bytevector? procedure? null? pair? symbol? code? eq?
                  port? input-port? output-port?)
            (fixnum? sys:fixnum?)
            (flonum? sys:flonum?)
            (bignum? sys:bignum?)
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
            (port? sys:port?)
            (input-port? sys:input-port?)
            (output-port? sys:output-port?)
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
  
  (define eof-object? (lambda (x) (sys:eof-object? x)))
  (define bwp-object? (lambda (x) (sys:bwp-object? x)))
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
          [else #f]))))

  (define port?
    (lambda (x) (sys:port? x)))
  (define input-port?
    (lambda (x) (sys:input-port? x)))
  (define output-port?
    (lambda (x) (sys:output-port? x)))
 

  )
