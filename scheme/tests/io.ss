
(library (tests io)
  (export test-io)
  (import (ikarus))


(define-syntax test
  (syntax-rules ()
    [(_ name body) 
     (begin
       (printf "running ~s ... " 'name)
       body
       (printf "ok\n"))]))

(define (make-n-byte-custom-binary-input-port n)
  (assert (<= 0 n 256))
  (make-custom-binary-input-port "test0" 
     (let ([c 0])
       (lambda (bv i count) 
         (if (< c n) 
             (begin
               (bytevector-u8-set! bv i c)
               (set! c (+ c 1))
               1)
             0)))
     #f #f #f))

(define (make-n-byte-bytevector-binary-input-port n)
  (assert (<= 0 n 256))
  (let ([bv (make-bytevector n)])
    (let f ([i 0]) 
      (unless (= i n)
        (bytevector-u8-set! bv i i)
        (f (+ i 1))))
    (open-bytevector-input-port bv)))

(define (make-ascii-range-bytevector)
  (let ([bv (make-bytevector 128)])
    (let f ([i 0]) 
      (unless (= i 128)
        (bytevector-u8-set! bv i i)
        (f (+ i 1))))
    bv))

(define (make-ascii-range-bytevector+utf8-bom)
  (let ([bv (make-bytevector (+ 128 3))])
    (bytevector-u8-set! bv 0 #xEF)
    (bytevector-u8-set! bv 1 #xBB)
    (bytevector-u8-set! bv 2 #xBF)
    (let f ([i 0]) 
      (unless (= i 128)
        (bytevector-u8-set! bv (+ i 3) i)
        (f (+ i 1))))
    bv))

(define (test-get-u8-1 p n)
  (let f ([i 0])
    (let ([x (get-u8 p)])
      (cond
        [(eof-object? x)
         (unless (= i n) 
           (error 'test0 "premature termination" i))]
        [(= x i) (f (+ i 1))]
        [else 
         (error 'test0 "incorrect value returned" x)]))))

(define (test-get-char-1 p n)
  (let f ([i 0])
    (let ([x (get-char p)])
      (cond
        [(eof-object? x)
         (unless (= i n) 
           (error 'test0 "premature termination" i))]
        [(= (char->integer x) i) (f (+ i 1))]
        [else 
         (error 'test0 "incorrect value returned" x)]))))


(define (test-peek-u8-1 p n)
  (let f ([i 0])
    (let* ([px (lookahead-u8 p)]
           [x (get-u8 p)])
      (cond
        [(not (eqv? px x)) (error #f "peek invalid" px x)]
        [(eof-object? x)
         (unless (= i n) 
           (error #f "premature termination" i))]
        [(= x i) (f (+ i 1))]
        [else 
         (error #f "incorrect value returned" x i)]))))

(define (test-peek-char-1 p n)
  (let f ([i 0])
    (let* ([px (lookahead-char p)]
           [x (get-char p)])
      (cond
        [(not (eqv? px x)) (error #f "peek invalid" px x)]
        [(eof-object? x)
         (unless (= i n) 
           (error #f "premature termination" i))]
        [(= (char->integer x) i) (f (+ i 1))]
        [else 
         (error #f "incorrect value returned" x i)]))))

(define (test-binary-port-eof?-1 p n)
  (let f ([i 0])
    (cond
      [(port-eof? p)
       (unless (= i n)
         (error #f "premature termination" i))
       (assert (eof-object? (lookahead-u8 p)))
       (assert (eof-object? (get-u8 p)))]
      [(= (get-u8 p) i) (f (+ i 1))]
      [else 
       (error #f "incorrect value returned" i)])))

(define (test-textual-port-eof?-1 p n)
  (let f ([i 0])
    (cond
      [(port-eof? p)
       (unless (= i n)
         (error #f "premature termination" i))
       (assert (eof-object? (lookahead-char p)))
       (assert (eof-object? (get-char p)))]
      [(= (char->integer (get-char p)) i) (f (+ i 1))]
      [else 
       (error #f "incorrect value returned" i)])))

(define (test-custom-binary-input-ports)
  (test "reading 256 bytes in ascending order"
    (test-get-u8-1 (make-n-byte-custom-binary-input-port 256) 256))
  
  (test "reading 256 bytes in ascending order 2 at a time"
    (test-get-u8-1
      (make-custom-binary-input-port "test0" 
        (let ([c 0])
          (lambda (bv i count) 
            (if (< c 256) 
                (begin
                  (assert (>= count 2))
                  (bytevector-u8-set! bv i c)
                  (bytevector-u8-set! bv (+ i 1) (+ c 1))
                  (set! c (+ c 2))
                  2)
                0)))
        #f #f #f)
      256))
  
  (test "peeking 256 bytes in ascending order"
    (test-peek-u8-1 (make-n-byte-custom-binary-input-port 256) 256))
  
  (test "custom-binary-port port-eof?"
    (test-binary-port-eof?-1 (make-n-byte-custom-binary-input-port 256) 256))
  
  ;;;
  (test "reading 256 bytes from bytevector-input-port"
    (test-get-u8-1 (make-n-byte-bytevector-binary-input-port 256) 256))
  
  (test "peeking 256 bytes from bytevector-input-port"
    (test-peek-u8-1 (make-n-byte-bytevector-binary-input-port 256) 256))
  
  (test "bytevector-binary-port port-eof?"
    (test-binary-port-eof?-1 (make-n-byte-bytevector-binary-input-port 256) 256))
  
  ;;;
    
  (test "reading 256 latin1 chars from bytevector-input-port"
    (test-get-char-1 
      (transcoded-port (make-n-byte-bytevector-binary-input-port 256)
        (make-transcoder (latin-1-codec) 'none 'raise))
      256))
  
  (test "peeking 256 bytes from latin1 transcoded port"
    (test-peek-char-1 
      (transcoded-port (make-n-byte-bytevector-binary-input-port 256)
        (make-transcoder (latin-1-codec) 'none 'raise))
      256))
  
  (test "latin1 transcoded port port-eof?"
    (test-textual-port-eof?-1 
      (transcoded-port (make-n-byte-bytevector-binary-input-port 256)
        (make-transcoder (latin-1-codec) 'none 'raise))
      256))
  
  ;;;
  
  (test "reading 128 utf8 chars from bytevector-input-port"
    (test-get-char-1 
      (open-bytevector-input-port (make-ascii-range-bytevector) 
        (make-transcoder (utf-8-codec) 'none 'raise))
      128))
  
  (test "peeking 128 chars from utf8 port"
    (test-peek-char-1 
      (open-bytevector-input-port (make-ascii-range-bytevector) 
        (make-transcoder (utf-8-codec) 'none 'raise))
      128))
  
  (test "utf8 transcoded port port-eof?"
    (test-textual-port-eof?-1 
      (open-bytevector-input-port (make-ascii-range-bytevector) 
        (make-transcoder (utf-8-codec) 'none 'raise))
      128)))

(define (make-utf8-bytevector-range2) 
  (u8-list->bytevector
    (let f ([i #x80] [j #x7FF])
      (cond
        [(> i j) '()]
        [else 
         (cons* (fxior #b11000000 (fxsra i 6))
                (fxior #b10000000 (fxand i #b111111))
                (f (+ i 1) j))]))))

(define (make-utf8-bytevector-range3) 
  (u8-list->bytevector
    (let f ([i #x800] [j #xFFFF])
      (cond
        [(> i j) '()]
        [(fx= i #xD800) (f #xE000 j)]
        [else 
         (cons* (fxior #b11100000 (fxsra i 12))
                (fxior #b10000000 (fxand (fxsra i 6) #b111111))
                (fxior #b10000000 (fxand i #b111111))
                (f (+ i 1) j))]))))

(define (make-utf8-bytevector-range4) 
  (u8-list->bytevector
    (let f ([i #x10000] [j #x10FFFF])
      (cond
        [(> i j) '()]
        [else 
         (cons* (fxior #b11110000 (fxsra i 18))
                (fxior #b10000000 (fxand (fxsra i 12) #b111111))
                (fxior #b10000000 (fxand (fxsra i 6) #b111111))
                (fxior #b10000000 (fxand i #b111111))
                (f (+ i 1) j))]))))

(define (make-utf8-string-range2)
  (list->string
    (let f ([i #x80] [j #x7FF])
      (cond
        [(> i j) '()]
        [else 
         (cons (integer->char i)
               (f (+ i 1) j))]))))

(define (make-utf8-string-range3)
  (list->string
    (let f ([i #x800] [j #xFFFF])
      (cond
        [(> i j) '()]
        [(fx= i #xD800) (f #xE000 j)]
        [else 
         (cons (integer->char i)
               (f (+ i 1) j))]))))

(define (make-utf8-string-range4)
  (list->string
    (let f ([i #x10000] [j #x10FFFF])
      (cond
        [(> i j) '()]
        [else 
         (cons (integer->char i)
               (f (+ i 1) j))]))))

(define (test-port-string-output p str) 
  (let f ([i 0])
    (let ([x (get-char p)])
      (cond
        [(eof-object? x) 
         (unless (= i (string-length str))
           (error #f "premature eof"))]
        [(= i (string-length str))
         (error #f "too many chars")]
        [(char=? x (string-ref str i))
         (f (+ i 1))]
        [else 
         (error #f "mismatch" x (string-ref str i) i)]))))

(define (test-port-string-peeking-output p str) 
  (let f ([i 0])
    (let ([x (lookahead-char p)])
      (cond
        [(eof-object? x) 
         (unless (= i (string-length str))
           (error #f "premature eof"))]
        [(= i (string-length str))
         (error #f "too many chars")]
        [(not (char=? x (get-char p)))
         (error #f "peek not same as get")]
        [(char=? x (string-ref str i))
         (f (+ i 1))]
        [else 
         (error #f "mismatch" x (string-ref str i) i)]))))

(define (run-exhaustive-tests)
  (test "utf8 range 2"
    (test-port-string-output
      (open-bytevector-input-port (make-utf8-bytevector-range2)
        (make-transcoder (utf-8-codec) 'none 'raise))
      (make-utf8-string-range2)))
  
  (test "utf8 range 3"
    (test-port-string-output
      (open-bytevector-input-port (make-utf8-bytevector-range3)
        (make-transcoder (utf-8-codec) 'none 'raise))
      (make-utf8-string-range3)))
  
  (test "utf8 range 4"
    (test-port-string-output
      (open-bytevector-input-port (make-utf8-bytevector-range4)
        (make-transcoder (utf-8-codec) 'none 'raise))
      (make-utf8-string-range4)))
  
  (test "utf8 peek range 2"
    (test-port-string-peeking-output
      (open-bytevector-input-port (make-utf8-bytevector-range2)
        (make-transcoder (utf-8-codec) 'none 'raise))
      (make-utf8-string-range2)))
  
  (test "utf8 peek range 3"
    (test-port-string-peeking-output
      (open-bytevector-input-port (make-utf8-bytevector-range3)
        (make-transcoder (utf-8-codec) 'none 'raise))
      (make-utf8-string-range3)))
  
  (test "utf8 peek range 4"
    (test-port-string-peeking-output
      (open-bytevector-input-port (make-utf8-bytevector-range4)
        (make-transcoder (utf-8-codec) 'none 'raise))
      (make-utf8-string-range4)))
  
  (test "utf8 range 2 string"
    (test-port-string-output
      (open-string-input-port (make-utf8-string-range2))
      (make-utf8-string-range2)))
  
  (test "utf8 range 3 string"
    (test-port-string-output
      (open-string-input-port (make-utf8-string-range3))
      (make-utf8-string-range3)))
  
  (test "utf8 range 4 string"
    (test-port-string-output
      (open-string-input-port (make-utf8-string-range4))
      (make-utf8-string-range4)))
  
  (test "utf8 peek range 2 string"
    (test-port-string-peeking-output
      (open-string-input-port (make-utf8-string-range2))
      (make-utf8-string-range2)))
  
  (test "utf8 peek range 3 string"
    (test-port-string-peeking-output
      (open-string-input-port (make-utf8-string-range3))
      (make-utf8-string-range3)))
  
  (test "utf8 peek range 4 string"
    (test-port-string-peeking-output
      (open-string-input-port (make-utf8-string-range4))
      (make-utf8-string-range4))))


(define (run-interactive-tests)
  (display "now write something on the keyboard ...\n")
  (printf "you typed ~s\n"
    (list->string
      (let ([p (standard-input-port)])
        (let f ()
          (let ([x (get-u8 p)])
            (if (eof-object? x) 
                '()
                (cons (integer->char x) (f))))))))
  
  (display "let's do it again ...\n")
  (printf "you typed ~s\n"
    (list->string
      (let ([p (transcoded-port (standard-input-port)
                  (make-transcoder (utf-8-codec)))])
        (let f ()
          (let ([x (get-char p)])
            (if (eof-object? x) 
                '()
                (cons x (f)))))))))
  
(define (file-size filename)
  (with-input-from-file filename
    (lambda ()
      (let f ([i 0])
        (let ([x (get-char (current-input-port))])
          (if (eof-object? x)
              i
              (f (+ i 1))))))))


(define (file->bytevector filename)
  (let ([p (open-file-input-port filename (file-options) 'block #f)])
    (u8-list->bytevector
      (let f ()
        (let ([x (get-u8 p)])
          (if (eof-object? x) 
              (begin (close-input-port p) '())
              (cons x (f))))))))

(define (bytevector->binary-port bv p)
  (let f ([i 0])
    (unless (fx= i (bytevector-length bv))
      (put-u8 p (bytevector-u8-ref bv i))
      (f (fx+ i 1)))))

(define (bytevector->textual-port bv p)
  (let f ([i 0])
    (unless (fx= i (bytevector-length bv))
      (put-char p (integer->char (bytevector-u8-ref bv i)))
      (f (fx+ i 1)))))

(define (test-input-files)
  (assert (= (file-size "tests/SRFI-1.ss") 56573))
  (let ([bv (file->bytevector "tests/SRFI-1.ss")])
    (let-values ([(p extract) (open-bytevector-output-port #f)])
      (bytevector->binary-port bv p)
      (let ([bv2 (extract)])
        (assert (bytevector=? bv bv2))
        (assert (bytevector=? #vu8() (extract))))))
  
  (let ([bv (file->bytevector "tests/SRFI-1.ss")])
    (let-values ([(p extract) (open-bytevector-output-port 
                                (native-transcoder))])
      (bytevector->textual-port bv p)
      (let ([bv2 (extract)])
        (assert (bytevector=? bv bv2))
        (assert (bytevector=? #vu8() (extract))))))
  
  (let ([bv (file->bytevector "tests/SRFI-1.ss")])
    (let-values ([(p extract) (open-bytevector-output-port 
                                (make-transcoder (latin-1-codec)))])
      (bytevector->textual-port bv p)
      (let ([bv2 (extract)])
        (assert (bytevector=? bv bv2))
        (assert (bytevector=? #vu8() (extract))))))
  
  (let ([bv (file->bytevector "tests/SRFI-1.ss")])
    (let-values ([(p extract) (open-string-output-port)]) 
      (bytevector->textual-port bv p)
      (let ([str (extract)])
        (assert (bytevector=? bv (string->utf8 str)))
        (assert (string=? "" (extract))))))
  
  (let ([p (standard-output-port)])
    (bytevector->binary-port 
      (string->utf8 "HELLO THERE\n")
      p)
    (flush-output-port p))
  
  (let ([p (current-output-port)])
    (bytevector->textual-port 
      (string->utf8 "HELLO THERE\n")
      p)
    (flush-output-port p))
  
  (let ([p (current-output-port)])
    (put-string p "HELLO THERE\n")
    (flush-output-port p)))

(define (test-custom-binary-output-ports)
  (define ls '())
  (let ([p (make-custom-binary-output-port "foo"
              (lambda (bv i c) 
                (let f ([i i] [c c])
                  (unless (fx= c 0)
                    (set! ls (cons (bytevector-u8-ref bv i) ls))
                    (f (fx+ i 1) (fx- c 1))))
                c)
              #f
              #f
              #f)])
    (let f ([i 0])
      (unless (fx= i 10000)
        (put-u8 p (mod i 37))
        (f (+ i 1))))
    (flush-output-port p)
    (let f ([i 0] [ls (reverse ls)])
      (unless (null? ls) 
        (assert (fx= (mod i 37) (car ls)))
        (f (fx+ i 1) (cdr ls))))))



(define (test-io)
  (test-custom-binary-input-ports)
  (test-custom-binary-output-ports)
  (run-exhaustive-tests)
  (test-input-files))
)
;(run-interactive-tests)

