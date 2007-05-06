
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
;;;   "M" + symbol-name : a symbol
;;;   "G" + pretty-name + unique-name : a gensym
;;;   "R" + rtd-name + rtd-symbol + field-count + field-names
;;;   "{" + field-count + rtd + fields
;;;   ">" + 4-bytes(i) : mark the next object with index i
;;;   "<" + 4-bytes(i) : dereference the object marked with index i
;;;   "x" : denotes code
;;;   "T" : Thunk; followed by code.



(library (ikarus fasl write)
  (export fasl-write)
  (import
    (only (scheme) $closure-code )
    (except (ikarus) fasl-write))

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
         (fasl-write-object (cdr x) p h
           (fasl-write-object (car x) p h m))]
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
             (write-char (integer->char (code-ref x i)) p)
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
        [else (error 'fasl-write "~s is not fasl-writable" x)])))
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



#!eof

#not working yet



(library (ikarus fasl read)
  (export)
  (import (scheme))

  (define who 'fasl-read)
  (define (assert-eq? x y)
    (unless (eq? x y)
      (error who "Expected ~s, got ~s\n" y x)))
  (define (char->int x)
    (if (char? x)
        (char->integer x)
        (error who "unexpected eof inside a fasl object")))
  (define (read-fixnum p)
    (let ([c0 (char->int (read-char p))]
          [c1 (char->int (read-char p))]
          [c2 (char->int (read-char p))]
          [c3 (char->int (read-char p))])
      (cond
        [(fx<= c3 127)
         (fxlogor (fxlogor (fxsra c0 2) (fxsll c1 6))
                  (fxlogor (fxsll c2 14) (fxsll c3 22)))]
        [else
         (let ([c0 (fxlogand #xFF (fxlognot c0))]
               [c1 (fxlogand #xFF (fxlognot c1))]
               [c2 (fxlogand #xFF (fxlognot c2))]
               [c3 (fxlogand #xFF (fxlognot c3))])
           (fx- -1 
             (fxlogor (fxlogor (fxsra c0 2) 
                               (fxsll c1 6))
                      (fxlogor (fxsll c2 14) 
                               (fxsll c3 22)))))])))
  (define (read-int p)
    (let ([c0 (char->int (read-char p))]
          [c1 (char->int (read-char p))]
          [c2 (char->int (read-char p))]
          [c3 (char->int (read-char p))])
      (cond
        [(fx<= c3 127)
         (fxlogor (fxlogor c0 (fxsll c1 8))
                  (fxlogor (fxsll c2 16) (fxsll c3 24)))]
        [else
         (let ([c0 (fxlogand #xFF (fxlognot c0))]
               [c1 (fxlogand #xFF (fxlognot c1))]
               [c2 (fxlogand #xFF (fxlognot c2))]
               [c3 (fxlogand #xFF (fxlognot c3))])
           (fx- -1 
             (fxlogor (fxlogor c0 
                               (fxsll c1 8))
                      (fxlogor (fxsll c2 16) 
                               (fxsll c3 24)))))]))) 
  (define (do-read p)
    (define marks (make-vector 1 #f))
    (define (max x y)
      (if (fx> x y) x y))
    (define (put-mark m obj)
      (cond
        [(fx< m (vector-length marks))
         (when (vector-ref marks m)
           (error 'fasl-read "mark ~s set twice" m))
         (vector-set! marks m obj)]
        [else
         (let ([n (vector-length marks)])
           (let ([v (make-vector 
                      (max (fx* n 2) (fx+ m 1))
                      #f)])
             (let f ([i 0])
               (cond
                 [(fx= i n) 
                  (set! marks v)
                  (vector-set! marks m obj)]
                 [else
                  (vector-set! v i (vector-ref marks i))
                  (f (fxadd1 i))]))))]))
    (define (read) (read/mark #f))
    (define (read-code code-m clos-m)
      (let* ([code-size (read-int p)]
             [freevars (read-fixnum p)])
        (let ([code (make-code code-size freevars)])
          (when code-m (put-mark code-m code))
          (let f ([i 0])
            (unless (fx= i code-size)
              (code-set! code i (char->int (read-char p)))
              (f (fxadd1 i))))
          (cond
            [clos-m
             (let ([clos ($code->closure code)])
               (put-mark clos-m clos)
               (set-code-reloc-vector! code (read))
               code)]
            [else
             (set-code-reloc-vector! code (read))
             code]))))
    (define (read-thunk m)
      (let ([c (read-char p)])
        (case c
          [(#\x)
           (let ([code (read-code #f m)])
             (if m (vector-ref marks m) ($code->closure code)))]
          [(#\<) 
           (let ([cm (read-int p)])
             (unless (fx< cm (vector-length marks))
               (error who "invalid mark ~s\n" m))
             (let ([code (vector-ref marks cm)])
               (let ([proc ($code->closure code)])
                 (when m (put-mark m proc))
                 proc)))]
          [(#\>)
           (let ([cm (read-int p)])
             (assert-eq? (read-char p) #\x)
             (let ([code (read-code cm m)])
               (if m (vector-ref marks m) ($code->closure code))))]
          [else (error who "invalid code header ~s" c)])))
    (define (read/mark m)
      (define (nom)
        (when m (error who "unhandled mark")))
      (let ([h (read-char p)])
        (case h
          [(#\I) 
           (nom)
           (read-fixnum p)]
          [(#\P)
           (if m
               (let ([x (cons #f #f)])
                 (put-mark m x)
                 (set-car! x (read))
                 (set-cdr! x (read))
                 x)
               (let ([a (read)])
                 (cons a (read))))]
          [(#\N) '()]
          [(#\T) #t]
          [(#\F) #f]
          [(#\E) (eof-object)]
          [(#\U) (void)]
          [(#\S) ;;; string
           (let ([n (read-int p)])
             (let ([str (make-string n)])
               (let f ([i 0])
                 (unless (fx= i n)
                   (let ([c (read-char p)])
                     (string-set! str i c)
                     (f (fxadd1 i)))))
               (when m (put-mark m str))
               str))]
          [(#\M) ;;; symbol
           (let ([str (read)])
             (let ([sym (string->symbol str)])
               (when m (put-mark m sym))
               sym))]
          [(#\G)
           (let* ([pretty (read)]
                  [unique (read)])
             (foreign-call "ikrt_strings_to_gensym" pretty unique))]
          [(#\V) ;;; vector
           (let ([n (read-int p)])
             (let ([v (make-vector n)])
               (when m (put-mark m v))
               (let f ([i 0])
                 (unless (fx= i n)
                   (vector-set! v i (read))
                   (f (fxadd1 i))))
               v))]
          [(#\x) ;;; code
           (read-code m #f)]
          [(#\Q) ;;; thunk
           (read-thunk m)]
          [(#\R)
           (let* ([rtd-name (read)]
                  [rtd-symbol (read)]
                  [field-count (read-int p)])
             (let ([fields
                    (let f ([i 0])
                      (cond
                        [(fx= i field-count) '()]
                        [else 
                         (let ([a (read)])
                           (cons a (f (fxadd1 i))))]))])
               (let ([rtd (make-record-type 
                            rtd-name fields rtd-symbol)])
                 (when m (put-mark m rtd))
                 rtd)))]
          [(#\{)
           (let ([n (read-int p)])
             (let ([rtd (read)])
               (let ([x ($make-record rtd n)])
                 (when m (put-mark m x))
                 (let f ([i 0])
                   (unless (fx= i n)
                     (record-set! x i (read))
                     (f (fxadd1 i))))
                 x)))]
          [(#\C)
           (let ([c (read-char p)])
             (cond
               [(char? c) c]
               [else
                (error who "invalid eof inside a fasl object")]))]
          [(#\>)
           (let ([m (read-int p)])
             (read/mark m))]
          [(#\<)
           (let ([m (read-int p)])
             (unless (fx< m (vector-length marks))
               (error who "invalid mark ~s\n" m))
             (vector-ref marks m))]
          [else
           (error who "Unexpected ~s as a fasl object header" h)])))
    (read))
  (define $fasl-read
    (lambda (p)
      (assert-eq? (read-char p) #\I)
      (assert-eq? (read-char p) #\K)
      (assert-eq? (read-char p) #\0)
      (assert-eq? (read-char p) #\1)
      (do-read p))))

