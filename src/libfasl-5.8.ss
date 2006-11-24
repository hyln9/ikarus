
######## FIXME
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
;;;   "I" + 4-bytes : denoting a fixnum (in network byte order)
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
         (write-fixnum (vector-length x) p)
         (let f ([x x] [i 0] [n (vector-length x)] [m m])
           (cond
             [(fx= i n) m]
             [else
              (f x (fxadd1 i) n
                 (fasl-write (vector-ref x i) p h m))]))]
        [(string? x) 
         (write-char #\S p)
         (write-fixnum (string-length x) p)
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
         (write-char #\S p)
         (do-write (symbol->string x) p h m)]
        [else (error 'fasl-write "~s is not fasl-writable")])))
  (define fasl-write 
    (lambda (x p h m)
      (cond
        [(immediate? x) (fasl-write-immediate x p) m]
        [(hash-table-get x h #f) =>
         (lambda (mark)
           (unless (fixnum? mark)
             (error 'fasl-write "BUG: invalid mark ~s" mark))
           (cond
             [(fx= mark 0) (do-write x p h m)]
             [(fx> mark 0) 
              (hash-table-put! x h (fx- 0 m))
              (fasl-write-mark m p)
              (do-write x p h (fxadd1 m))]
             [else (values (fasl-write-ref (fx- 0 id) p) m)]))]
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
              (make-graph (cdr d) h)]
             [(vector? x)
              (let f ([x x] [i 0] [n (vector-length x)])
                (unless (fx= i n) 
                  (make-graph (vector-ref x i) h)
                  (f x (fxadd1 i) n)))]
             [(symbol? x) (void)]
             [(string? x) (void)]
             [else (error 'fasl-write "~s is not fasl-writable" x)])]))))
  ($pcb-set! fasl-write
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
           (fasl-write x port h 0)))))

