
(library (ikarus fasl write)
  (export fasl-write)
  (import
    (ikarus system $codes)
    (ikarus system $pairs)
    (ikarus system $records)
    (ikarus system $io)
    (ikarus system $bytevectors)
    (ikarus system $fx)
    (ikarus system $chars)
    (ikarus system $strings)
    (ikarus system $flonums)
    (ikarus system $bignums)
    (except (ikarus code-objects) procedure-annotation)
    (except (ikarus) fasl-write))

  (define write-fixnum 
    (lambda (x p)
      (unless (fixnum? x) (error 'write-fixnum "not a fixnum ~s" x))
      (write-byte (fxsll (fxlogand x #x3F) 2) p)
      (write-byte (fxlogand (fxsra x 6) #xFF) p)
      (write-byte (fxlogand (fxsra x 14) #xFF) p)
      (write-byte (fxlogand (fxsra x 22) #xFF) p)))
  (define write-int 
    (lambda (x p)
      (unless (fixnum? x) (error 'write-int "not a fixnum ~s" x))
      (write-byte (fxlogand x #xFF) p)
      (write-byte (fxlogand (fxsra x 8) #xFF) p)
      (write-byte (fxlogand (fxsra x 16) #xFF) p)
      (write-byte (fxlogand (fxsra x 24) #xFF) p)))
  (define fasl-write-immediate
    (lambda (x p)
      (cond
        [(null? x) (write-char #\N p)]
        [(fixnum? x) 
         (write-char #\I p)
         (write-fixnum x p)]
        [(char? x)
         (let ([n ($char->fixnum x)])
           (if ($fx<= n 255)
               (begin
                 (write-char #\c p)
                 (write-byte n p))
               (begin
                 (write-char #\C p)
                 (write-int n p))))]
        ; (write-char #\C p)
        ; (write-char x p)]
        [(boolean? x)
         (write-char (if x #\T #\F) p)]
        [(eof-object? x) (write-char #\E p)]
        [(eq? x (void)) (write-char #\U p)]
        [else (error 'fasl-write "~s is not a fasl-writable immediate" x)])))
  
  (define (ascii-string? s)
    (let f ([s s] [i 0] [n (string-length s)])
      (or ($fx= i n)
          (and ($char<= ($string-ref s i) ($fixnum->char 127))
               (f s ($fxadd1 i) n)))))

  (define (count-unshared-cdrs x h n)
    (cond
      [(and (pair? x) (eq? (get-hash-table h x #f) 0))
       (count-unshared-cdrs ($cdr x) h ($fxadd1 n))]
      [else n]))

  (define (write-pairs x p h m n)
    (cond
      [($fx= n 0) (fasl-write-object x p h m)]
      [else 
       (write-pairs (cdr x) p h 
         (fasl-write-object (car x) p h m)
         ($fxsub1 n))]))
       
  (define do-write
    (lambda (x p h m)
      (cond
        [(pair? x)
         (let ([d ($cdr x)])
           (let ([n (count-unshared-cdrs d h 0)])
             (cond
               [($fx= n 0)
                (write-char #\P p)
                (fasl-write-object d p h
                  (fasl-write-object (car x) p h m))]
               [else 
                (cond
                  [($fx<= n 255) 
                   (write-char #\l p)
                   (write-byte n p)]
                  [else
                   (write-char #\L p)
                   (write-int n p)])
                (write-pairs d p h 
                  (fasl-write-object (car x) p h m)
                  n)])))]
        [(vector? x)
         (write-char #\V p)
         (write-int (vector-length x) p)
         (let f ([x x] [i 0] [n (vector-length x)] [m m])
           (cond
             [(fx= i n) m]
             [else
              (f x (fxadd1 i) n
                 (fasl-write-object (vector-ref x i) p h m))]))]
        [(string? x) 
         (cond
           [(ascii-string? x)
            (write-char #\s p)
            (write-int (string-length x) p)
            (let f ([x x] [i 0] [n (string-length x)])
              (unless (fx= i n)
                (write-char (string-ref x i) p)
                (f x (fxadd1 i) n)))]
           [else
            (write-char #\S p)
            (write-int (string-length x) p)
            (let f ([x x] [i 0] [n (string-length x)])
              (unless (= i n)
                (write-int (char->integer (string-ref x i)) p)
                (f x (fxadd1 i) n)))])
         m]
        [(gensym? x)
         (write-char #\G p)
         (fasl-write-object (gensym->unique-string x) p h
           (fasl-write-object (symbol->string x) p h m))]
        [(symbol? x) 
         (write-char #\M p)
         (fasl-write-object (symbol->string x) p h m)]
        [(code? x)
         (write-char #\x p)
         (write-int (code-size x) p)
         (write-fixnum (code-freevars x) p)
         (let f ([i 0] [n (code-size x)])
           (unless (fx= i n)
             (write-byte (code-ref x i) p)
             (f (fxadd1 i) n)))
         (fasl-write-object (code-reloc-vector x) p h m)]
        [(record? x)
         (let ([rtd (record-type-descriptor x)])
           (cond
             [(eq? rtd (base-rtd))
              ;;; rtd record
              (write-char #\R p)
              (let ([names (record-type-field-names x)]
                    [m 
                     (fasl-write-object (record-type-symbol x) p h
                       (fasl-write-object (record-type-name x) p h m))])
                (write-int (length names) p)
                (let f ([names names] [m m])
                  (cond
                    [(null? names) m]
                    [else
                     (f (cdr names)
                        (fasl-write-object (car names) p h m))])))]
             [else
              ;;; non-rtd record
              (write-char #\{ p)
              (write-int (length (record-type-field-names rtd)) p)
              (let f ([names (record-type-field-names rtd)] 
                      [m (fasl-write-object rtd p h m)])
                (cond
                  [(null? names) m]
                  [else
                   (f (cdr names) 
                      (fasl-write-object 
                         ((record-field-accessor rtd (car names)) x)
                         p h m))]))]))]
        [(procedure? x)
         (write-char #\Q p)
         (fasl-write-object ($closure-code x) p h m)]
        [(bytevector? x) 
         (write-char #\v p)
         (let ([n ($bytevector-length x)])
           (write-int n p)
           (write-bytevector x 0 n p))
         m]
        [(flonum? x) 
         (write-char #\f p)
         (write-byte ($flonum-u8-ref x 7) p)
         (write-byte ($flonum-u8-ref x 6) p)
         (write-byte ($flonum-u8-ref x 5) p)
         (write-byte ($flonum-u8-ref x 4) p)
         (write-byte ($flonum-u8-ref x 3) p)
         (write-byte ($flonum-u8-ref x 2) p)
         (write-byte ($flonum-u8-ref x 1) p)
         (write-byte ($flonum-u8-ref x 0) p)
         m]
        [(ratnum? x)
         (write-char #\r p)
         (fasl-write-object (numerator x) p h
           (fasl-write-object (denominator x) p h m))]
        [(bignum? x) 
         (write-char #\b p) 
         (let ([sz ($bignum-size x)])
           (write-int (if ($bignum-positive? x) sz (- sz)) p)
           (let f ([i 0])
             (unless (fx= i sz)
               (write-byte ($bignum-byte-ref x i) p)
               (f (fxadd1 i)))))
         m]
        [else (error 'fasl-write "~s is not fasl-writable" x)])))
  (define (write-bytevector x i j p)
    (unless ($fx= i j)
      ($write-byte ($bytevector-u8-ref x i) p)
      (write-bytevector x ($fxadd1 i) j p)))
  (define fasl-write-object 
    (lambda (x p h m)
      (cond
        [(immediate? x) (fasl-write-immediate x p) m]
        [(get-hash-table h x #f) =>
         (lambda (mark)
           (unless (fixnum? mark)
             (error 'fasl-write "BUG: invalid mark ~s" mark))
           (cond
             [(fx= mark 0) ; singly referenced
              (do-write x p h m)]
             [(fx> mark 0) ; marked but not written
              (put-hash-table! h x (fx- 0 m))
              (write-char #\> p)
              (write-int m p)
              (do-write x p h (fxadd1 m))]
             [else
              (write-char #\< p)
              (write-int (fx- 0 mark) p)
              m]))]
        [else (error 'fasl-write "BUG: not in hash table ~s" x)]))) 
  (define make-graph
    (lambda (x h)
      (unless (immediate? x)
        (cond
          [(get-hash-table h x #f) =>
           (lambda (i) 
             (put-hash-table! h x (fxadd1 i)))]
          [else
           (put-hash-table! h x 0)
           (cond
             [(pair? x) 
              (make-graph (car x) h)
              (make-graph (cdr x) h)]
             [(vector? x)
              (let f ([x x] [i 0] [n (vector-length x)])
                (unless (fx= i n) 
                  (make-graph (vector-ref x i) h)
                  (f x (fxadd1 i) n)))]
             [(symbol? x) 
              (make-graph (symbol->string x) h)
              (when (gensym? x) (make-graph (gensym->unique-string x) h))]
             [(string? x) (void)]
             [(code? x) 
              (make-graph (code-reloc-vector x) h)]
             [(record? x)
              (when (eq? x (base-rtd))
                (error 'fasl-write "base-rtd is not writable"))
              (let ([rtd (record-type-descriptor x)])
                (cond
                  [(eq? rtd (base-rtd))
                   ;;; this is an rtd
                   (make-graph (record-type-name x) h)
                   (make-graph (record-type-symbol x) h)
                   (for-each (lambda (x) (make-graph x h))
                     (record-type-field-names x))]
                  [else
                   ;;; this is a record
                   (make-graph rtd h)
                   (for-each 
                     (lambda (name) 
                       (make-graph ((record-field-accessor rtd name) x) h))
                     (record-type-field-names rtd))]))]
             [(procedure? x)
              (let ([code ($closure-code x)])
                (unless (fxzero? (code-freevars code))
                  (error 'fasl-write
                         "Cannot write a non-thunk procedure; the one given has ~s free vars"
                         (code-freevars code)))
                (make-graph code h))]
             [(bytevector? x) (void)]
             [(flonum? x)     (void)]
             [(bignum? x)     (void)]
             [(ratnum? x) 
              (make-graph (numerator x) h)
              (make-graph (denominator x) h)]
             [else (error 'fasl-write "~s is not fasl-writable" x)])]))))
  (define fasl-write-to-port
    (lambda (x port)
      (let ([h (make-hash-table)])
         (make-graph x h)
         (write-char #\# port)
         (write-char #\@ port)
         (write-char #\I port)
         (write-char #\K port)
         (write-char #\0 port)
         (write-char #\1 port)
         (fasl-write-object x port h 1)
         (void))))
  (define fasl-write
     (case-lambda 
       [(x) (fasl-write-to-port x (current-output-port))]
       [(x port)
        (unless (output-port? port)
          (error 'fasl-write "~s is not an output port" port))
        (fasl-write-to-port x port)])))
