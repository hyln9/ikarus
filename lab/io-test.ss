#!/usr/bin/env scheme-script

(import 
  (except (ikarus) get-char get-u8 lookahead-u8 close-port input-port?)
  (io-spec))

(define-syntax test
  (syntax-rules ()
    [(_ name body) 
     (begin
       (printf "running ~s ..." 'name)
       body
       (printf " ok\n"))]))

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

(define (test-port-eof?-1 p n)
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
  (test-port-eof?-1 (make-n-byte-custom-binary-input-port 256) 256))

;;;
(test "reading 256 bytes from bytevector-input-port"
  (test-get-u8-1 (make-n-byte-bytevector-binary-input-port 256) 256))

(test "peeking 256 bytes from bytevector-input-port"
  (test-peek-u8-1 (make-n-byte-bytevector-binary-input-port 256) 256))

(test "bytevector-binary-port port-eof?"
  (test-port-eof?-1 (make-n-byte-bytevector-binary-input-port 256) 256))


