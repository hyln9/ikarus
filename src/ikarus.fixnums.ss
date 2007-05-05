
(library (ikarus fixnums)
  (export fxzero? fxadd1 fxsub1 fxlognot)
  (import 
    (only (scheme) $fxadd1 $fxsub1 $fxlognot)
    (except (ikarus) fxzero? fxadd1 fxsub1 fxlognot))

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
  )
