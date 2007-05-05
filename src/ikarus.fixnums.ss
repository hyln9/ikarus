
(library (ikarus fixnums)
  (export fxzero? fxadd1 fxsub1 fxlognot fx+ fx- fx*)
  (import 
    (only (scheme) $fxadd1 $fxsub1 $fxlognot $fx+ $fx- $fx*)
    (except (ikarus) fxzero? fxadd1 fxsub1 fxlognot fx+ fx- fx*))

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
 

  )
