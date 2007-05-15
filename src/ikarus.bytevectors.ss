
(library (ikarus bytevectors)
  (export make-bytevector bytevector-length bytevector-s8-ref
          bytevector-u8-ref bytevector-u8-set! bytevector-s8-set!)
  (import 
    (except (ikarus) 
        make-bytevector bytevector-length bytevector-s8-ref
        bytevector-u8-ref bytevector-u8-set! bytevector-s8-set!)
    (ikarus system $fx)
    (ikarus system $bytevectors))

  (define ($bytevector-fill x i j fill)
    (cond
      [($fx= i j) x]
      [else
       ($bytevector-set! x i fill)
       ($bytevector-fill x ($fxadd1 i) j fill)]))

  (define make-bytevector
    (case-lambda 
      [(k) 
       (if (and (fixnum? k) ($fx>= k 0))
           ($make-bytevector k)
           (error 'make-bytevector "~s is not a valid size" k))]
      [(k fill)
       (if (and (fixnum? fill) ($fx<= -128 fill) ($fx<= fill 255))
           ($bytevector-fill (make-bytevector k) 0 k fill)
           (error 'make-bytevector "~s is not a valid fill" fill))]))

  (define bytevector-length
    (lambda (x)
      (if (bytevector? x)
          ($bytevector-length x)
          (error 'bytevector-length "~s is not a bytevector" x))))

  (define bytevector-s8-ref
    (lambda (x i)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              ($bytevector-s8-ref x i)
              (error 'bytevector-s8-ref "invalid index ~s for ~s" i x))
          (error 'bytevector-s8-ref "~s is not a bytevector" x))))

  (define bytevector-u8-ref
    (lambda (x i)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              ($bytevector-u8-ref x i)
              (error 'bytevector-u8-ref "invalid index ~s for ~s" i x))
          (error 'bytevector-u8-ref "~s is not a bytevector" x))))


  (define bytevector-s8-set!
    (lambda (x i v)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              (if (and (fixnum? v) ($fx<= -128 v) ($fx<= v 127)) 
                  ($bytevector-set! x i v)
                  (error 'bytevector-s8-set! "~s is not a byte" v))
              (error 'bytevector-s8-set! "invalid index ~s for ~s" i x))
          (error 'bytevector-s8-set! "~s is not a bytevector" x))))
  
  (define bytevector-u8-set!
    (lambda (x i v)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              (if (and (fixnum? v) ($fx<= 0 v) ($fx<= v 255))
                  ($bytevector-set! x i v)
                  (error 'bytevector-u8-set! "~s is not an octet" v))
              (error 'bytevector-u8-set! "invalid index ~s for ~s" i x))
          (error 'bytevector-u8-set! "~s is not a bytevector" x))))
  
  )


