;;; not finished yet 

;;; FASL 
;;;
;;; A fasl object is a header followed by one or more objects followed by an
;;; end-of-fasl marker
;;;
;;; The header is the string "#@IK01"
;;; The end of fasl marker is "@"
;;;
;;; An object is either:
;;;   "N" : denoting the empty list
;;;   "T" : denoting #t
;;;   "F" : denoting #f
;;;   "E" : denoting the end of file object
;;;   "U" : denoting the unspecified value
;;;   "I" + 4-bytes : denoting a fixnum (in host byte order)
;;;   "C" + 1-byte : denoting a character
;;;   "P" + object1 + object2 : a pair
;;;   "V" + 4-bytes(n) + object ... : a vector of length n followed by n
;;;                                   objects
;;;   "S" + 4-bytes(n) + char ... : a string
;;;   "M" + object + object : a symbol with name field and a unique-name field
;;;   ">" + 4-bytes(i) : mark the next object with index i
;;;   "<" + 4-bytes(i) : dereference the object marked with index i
;;;


(let ()
  (define write-fixnum 
    (lambda (x p)
      (unless (fixnum? x) (error 'write-fixnum "not a fixnum ~s" x))
      (write-char (integer->char (fxsll (fxlogand x #x3F) 2)) p)
      (write-char (integer->char (fxlogand (fxsra x 6) #xFF)) p)
      (write-char (integer->char (fxlogand (fxsra x 14) #xFF)) p)
      (write-char (integer->char (fxlogand (fxsra x 22) #xFF)) p)))
  (define write-int 
    (lambda (x p)
      (unless (fixnum? x) (error 'write-int "not a fixnum ~s" x))
      (write-char (integer->char (fxlogand x #xFF)) p)
      (write-char (integer->char (fxlogand (fxsra x 8) #xFF)) p)
      (write-char (integer->char (fxlogand (fxsra x 16) #xFF)) p)
      (write-char (integer->char (fxlogand (fxsra x 24) #xFF)) p)))
 

  (define fasl-write-immediate
    (lambda (x p)
      (cond
        [(null? x) (write-char #\N p)]
        [(fixnum? x) 
         (write-char #\I p)
         (write-fixnum x p)]
        [(char? x)
         (write-char #\C p)
         (write-char x p)]
        [(boolean? x)
         (write-char (if x #\T #\F) p)]
        [(eof-object? x) (write-char #\E p)]
        [(eq? x (void)) (write-char #\U p)]
        [else (error 'fasl-write "~s is not a fasl-writable immediate" x)])))

  (define do-write
    (lambda (x p h m)
      (cond
        [(pair? x)   
         (write-char #\P p)
         (fasl-write (cdr x) p h
           (fasl-write (car x) p h m))]
        [(vector? x)
         (write-char #\V p)
         (write-int (vector-length x) p)
         (let f ([x x] [i 0] [n (vector-length x)] [m m])
           (cond
             [(fx= i n) m]
             [else
              (f x (fxadd1 i) n
                 (fasl-write (vector-ref x i) p h m))]))]
        [(string? x) 
         (write-char #\S p)
         (write-int (string-length x) p)
         (let f ([x x] [i 0] [n (string-length x)])
           (cond
             [(fx= i n) m]
             [else
              (write-char (string-ref x i) p)
              (f x (fxadd1 i) n)]))]
        [(gensym? x)
         (write-char #\G p)
         (do-write (gensym->unique-name x) p h
           (do-write (symbol->string x) p h m))]
        [(symbol? x) 
         (write-char #\M p)
         (do-write (symbol->string x) p h m)]
        [(code? x)
         (write-char #\X p)
         (let ([code-vec (code-code-vec x)]
               [reloc-vec (code-reloc-vec x)]
               [closure-size (code-closure-size x)])
           (write-int (string-length code-vec) p)
           (write-int (fx* (vector-length reloc-vec) 4) p)
           (write-int closure-size p)
           (let f ([i 0] [n (string-length code-vec)])
             (unless (fx= i n)
               (write-char (string-ref code-vec i) p)
               (f (fxadd1 i) n)))
           (let f ([i 0] [n (vector-length reloc-vec)] [m m])
             (if (fx= i n)
                 m
                 (let ([b (vector-ref reloc-vec i)])
                   (case (car b)
                     [(object) 
                      (let ([code-idx (cadr b)] [object (caddr b)])
                        (write-char #\O p)
                        (write-int code-idx p)
                        (let ([m (fasl-write object p h m)])
                          (f (fxadd1 i) n m)))]
                     [(foreign)
                      (let ([code-idx (cadr b)] [object (caddr b)])
                        (write-char #\F p)
                        (write-int code-idx p)
                        (let ([m (fasl-write object p h m)])
                          (f (fx+ i 2) n m)))]
                     [(object+off/rel object+off)
                      (let ([code-idx (cadr b)] 
                            [object (caddr b)] 
                            [object-off (cadddr b)])
                        (if (eq? (car b) 'object+off/rel)
                            (write-char #\J p)
                            (write-char #\D p))
                        (write-int code-idx p)
                        (write-int object-off p)
                        (let ([m (fasl-write object p h m)])
                          (f (fx+ i 2) n m)))]
                     [else (error 'fasl-write "invalid reloc byte ~s" b)])))))]
        [else (error 'fasl-write "~s is not fasl-writable" x)])))
  (define fasl-write 
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
        [else (error 'fasl-write "BUG: not in hash table")]))) 
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
             [(symbol? x) (void)]
             [(string? x) (void)]
             [(code? x) 
              (let ([x (code-reloc-vec x)])
                (let f ([i 0] [n (vector-length x)])
                  (unless (fx= i n) 
                    (let ([b (vector-ref x i)])
                      (case (car b)
                        [(object)
                         (make-graph (caddr b) h)
                         (f (fxadd1 i) n)]
                        [(object+off/rel object+off foreign)
                         (make-graph (caddr b) h)
                         (f (fx+ i 2) n)]
                        [else (error 'fasl-write "unrecognized reloc ~s" b)]
                        )))))]
             [else (error 'fasl-write "~s is not fasl-writable" x)])]))))
  (primitive-set! 'fasl-write
     (lambda (x . rest)
       (let ([port 
              (if (null? rest)
                  (current-output-port)
                  (let ([a (car rest)])
                    (unless (output-port? a)
                      (error 'fasl-write "~s is not an output port" a))
                    a))])
         (let ([h (make-hash-table)])
           (make-graph x h)
           (write-char #\# port)
           (write-char #\@ port)
           (write-char #\I port)
           (write-char #\K port)
           (write-char #\0 port)
           (write-char #\1 port)
           (fasl-write x port h 1))))))

