
(library (ikarus bytevectors)
  (export make-bytevector bytevector-length bytevector-s8-ref
          bytevector-u8-ref bytevector-u8-set! bytevector-s8-set!
          bytevector-copy! u8-list->bytevector bytevector->u8-list
          bytevector-fill! bytevector-copy bytevector=?)
  (import 
    (except (ikarus) 
        make-bytevector bytevector-length bytevector-s8-ref
        bytevector-u8-ref bytevector-u8-set! bytevector-s8-set! 
        bytevector-copy! u8-list->bytevector bytevector->u8-list
        bytevector-fill! bytevector-copy bytevector=?)
    (ikarus system $fx)
    (ikarus system $pairs)
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

  (define bytevector-fill!
    (lambda (x fill)
      (unless (bytevector? x)
        (error 'bytevector-fill! "~s is not a bytevector" x))
      (unless (and (fixnum? fill) ($fx<= -128 fill) ($fx<= fill 255))
        (error 'bytevector-fill! "~s is not a valid fill" fill))
      ($bytevector-fill x 0 ($bytevector-length x) fill)))


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
  
  (define bytevector->u8-list
    (lambda (x)
      (unless (bytevector? x)
        (error 'bytevector->u8-list "~s is not a bytevector" x))
      (let f ([x x] [i ($bytevector-length x)] [ac '()])
        (cond
          [($fx= i 0) ac]
          [else
           (let ([i ($fxsub1 i)])
             (f x i (cons ($bytevector-u8-ref x i) ac)))]))))

  (define u8-list->bytevector
    (letrec ([race
              (lambda (h t ls n)
               (if (pair? h)
                   (let ([h ($cdr h)])
                      (if (pair? h)
                          (if (not (eq? h t))
                              (race ($cdr h) ($cdr t) ls ($fx+ n 2))
                              (error 'u8-list->bytevector "circular list ~s" ls))
                          (if (null? h)
                              ($fx+ n 1)
                              (error 'u8-list->bytevector "~s is not a proper list" ls))))
                   (if (null? h)
                       n
                       (error 'u8-list->bytevector "~s is not a proper list" ls))))]
              [fill
               (lambda (s i ls)
                 (cond
                   [(null? ls) s]
                   [else
                    (let ([c ($car ls)])
                      (unless (and (fixnum? c) ($fx<= 0 c) ($fx<= c 255))
                        (error 'u8-list->bytevector "~s is not an octet" c))
                      ($bytevector-set! s i c)
                      (fill s ($fxadd1 i) (cdr ls)))]))])
       (lambda (ls)
         (let ([n (race ls ls ls 0)])
           (let ([s ($make-bytevector n)])
             (fill s 0 ls))))))


  (define bytevector-copy
    (lambda (src)
      (unless (bytevector? src)
        (error 'bytevector-copy "~s is not a bytevector" src))
      (let ([n ($bytevector-length src)])
        (let f ([src src] [dst ($make-bytevector n)] [i 0] [n n])
          (cond
            [($fx= i n) dst]
            [else
             ($bytevector-set! dst i ($bytevector-u8-ref src i))
             (f src dst ($fxadd1 i) n)])))))

  (define bytevector=? 
    (lambda (x y)
      (unless (bytevector? x)
        (error 'bytevector=? "~s is not a bytevector" x))
      (unless (bytevector? y)
        (error 'bytevector=? "~s is not a bytevector" y))
      (let ([n ($bytevector-length x)])
        (and ($fx= n ($bytevector-length y))
             (let f ([x x] [y y] [i 0] [n n])
               (or ($fx= i n)
                   (and ($fx= ($bytevector-u8-ref x i)
                              ($bytevector-u8-ref y i))
                        (f x y ($fxadd1 i) n))))))))

  (define bytevector-copy!
    (lambda (src src-start dst dst-start k) 
      (cond
        [(or (not (fixnum? src-start)) ($fx< src-start 0))
         (error 'bytevector-copy! "~s is not a valid starting index" src-start)]
        [(or (not (fixnum? dst-start)) ($fx< dst-start 0))
         (error 'bytevector-copy! "~s is not a valid starting index" dst-start)]
        [(or (not (fixnum? k)) ($fx< k 0))
         (error 'bytevector-copy! "~s is not a valid length" k)]
        [(not (bytevector? src)) 
         (error 'bytevector-copy! "~s is not a bytevector" src)]
        [(not (bytevector? dst)) 
         (error 'bytevector-copy! "~s is not a bytevector" dst)]
        [(let ([n ($fx+ src-start k)])
           (or ($fx< n 0) ($fx>= n ($bytevector-length src))))
         (error 'bytevector-copy! "~s+~s is out of range" src-start k)]
        [(let ([n ($fx+ dst-start k)])
           (or ($fx< n 0) ($fx>= n ($bytevector-length dst))))
         (error 'bytevector-copy! "~s+~s is out of range" dst-start k)]
        [(eq? src dst)
         (cond
           [($fx< dst-start src-start)
            (let f ([src src] [si src-start] [di dst-start] [sj ($fx+ src-start k)])
              (unless ($fx= si sj)
                ($bytevector-set! src di ($bytevector-u8-ref src si))
                (f src ($fxadd1 si) ($fxadd1 di) sj)))]
           [($fx< src-start dst-start)
            (let f ([src src] [si ($fx+ src-start k)] [di ($fx+ dst-start k)] [sj src-start])
              (unless ($fx= si sj)
                (let ([si ($fxsub1 si)] [di ($fxsub1 di)])
                  ($bytevector-set! src di ($bytevector-u8-ref src si))
                  (f src si di sj))))]
           [else (void)])]
        [else
         (let f ([src src] [si src-start] [dst dst] [di dst-start] [sj ($fx+ src-start k)])
           (unless ($fx= si sj)
             ($bytevector-set! dst di ($bytevector-u8-ref src si))
             (f src ($fxadd1 si) dst ($fxadd1 di) sj)))])))



  )


