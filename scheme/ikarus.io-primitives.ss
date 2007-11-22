;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus io-primitives)
  (export read-char unread-char peek-char write-char write-byte
          put-u8 put-char put-string put-bytevector 
          get-char get-u8 get-string-n get-string-n! get-bytevector-n
          newline port-name input-port-name output-port-name
          close-input-port reset-input-port! 
          flush-output-port close-output-port get-line)
  (import 
    (ikarus system $io)
    (ikarus system $fx)
    (ikarus system $ports)
    (except (ikarus) read-char unread-char peek-char write-char write-byte 
            put-u8 put-char put-string put-bytevector
            get-char get-u8 get-string-n get-string-n! get-bytevector-n
            newline port-name input-port-name output-port-name
            close-input-port reset-input-port!  flush-output-port
            close-output-port get-line))

  (define write-char
    (case-lambda
      [(c)
       (if (char? c)
           ($write-char c (current-output-port))
           (error 'write-char "not a character" c))]
      [(c p)
       (if (char? c)
           (if (output-port? p)
               ($write-char c p)
               (error 'write-char "not an output-port" p))
           (error 'write-char "not a character" c))]))
   
  (define put-char
    (lambda (c p)
      (if (char? c)
          (if (output-port? p)
              ($write-char c p)
              (error 'put-char "not an output-port" p))
          (error 'put-char "not a character" c))))

  (define write-byte
    (case-lambda
      [(b)
       (if (and (fixnum? b) ($fx<= 0 b) ($fx<= b 255))
           ($write-byte b (current-output-port))
           (error 'write-byte "not a byte" b))]
      [(b p)
       (if (and (fixnum? b) ($fx<= 0 b) ($fx<= b 255))
           (if (output-port? p)
               ($write-byte b p)
               (error 'write-byte "not an output-port" p))
           (error 'write-byte "not a byte" b))]))

  (define put-u8
    (lambda (b p)
      (if (and (fixnum? b) ($fx<= 0 b) ($fx<= b 255))
          (if (output-port? p)
              ($write-byte b p)
              (error 'put-u8 "not an output-port" p))
          (error 'put-u8 "not a u8" b))))
  ;;;
  (define newline
    (case-lambda
      [() 
       ($write-char #\newline (current-output-port))
       ($flush-output-port (current-output-port))]
      [(p) 
       (if (output-port? p)
           (begin
             ($write-char #\newline p)
             ($flush-output-port p))
           (error 'newline "not an output port" p))]))
  ;;;
  (define port-name
    (lambda (p)
      (if (port? p) 
          (($port-handler p) 'port-name p)
          (error 'port-name "not a port" p))))

  (define input-port-name
    (lambda (p)
      (if (port? p) 
          (($port-handler p) 'port-name p)
          (error 'input-port-name "not a port" p))))

  (define output-port-name
    (lambda (p)
      (if (port? p) 
          (($port-handler p) 'port-name p)
          (error 'output-port-name "not a port" p))))

  (define get-char
    (lambda (p) 
      (if (input-port? p)
           ($read-char p)
           (error 'get-char "not an input-port" p))))

  (define get-u8
    (lambda (p) 
      (if (input-port? p)
           ($get-u8 p)
           (error 'get-u8 "not an input-port" p))))

  (define read-char
    (case-lambda
      [() ($read-char (current-input-port))]
      [(p)
       (if (input-port? p)
           ($read-char p)
           (error 'read-char "not an input-port" p))]))
  ;;;
  (define unread-char
    (case-lambda
      [(c) (if (char? c)
               ($unread-char c (current-input-port))
               (error 'unread-char "not a character" c))]
      [(c p)
       (if (input-port? p)
           (if (char? c)
               ($unread-char c p)
               (error 'unread-char "not a character" c))
           (error 'unread-char "not an input-port" p))]))
  ;;;
  (define peek-char
    (case-lambda
      [() ($peek-char (current-input-port))]
      [(p)
       (if (input-port? p)
           ($peek-char p)
           (error 'peek-char "not an input-port" p))]))
  ;;;
  (define reset-input-port!
    (case-lambda
      [() ($reset-input-port! (current-input-port))]
      [(p)
       (if (input-port? p)
           ($reset-input-port! p)
           (error 'reset-input-port! "not an input-port" p))]))
  ;;;
  (define close-input-port
    (case-lambda
      [() ($close-input-port (current-input-port))]
      [(p)
       (if (input-port? p)
           ($close-input-port p)
           (error 'close-input-port! "not an input-port" p))]))
  ;;;
  (define close-output-port
    (case-lambda
      [() ($close-output-port (current-output-port))]
      [(p)
       (if (output-port? p)
           ($close-output-port p)
           (error 'close-output-port "not an output-port" p))]))
  ;;;
  (define flush-output-port
    (case-lambda
      [() ($flush-output-port (current-output-port))]
      [(p)
       (if (output-port? p)
           ($flush-output-port p)
           (error 'flush-output-port "not an output-port" p))]))
  
  (define (get-line p)
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x ($read-char p)])
          (cond
            [(eqv? x #\newline) 
             (make-it n ac)]
            [(eof-object? x) 
             (if (null? ac) x (make-it n ac))]
            [else (f p (+ n 1) (cons x ac))]))))
    (define (make-it n revls)
      (let f ([s (make-string n)] [i (- n 1)] [ls revls])
        (cond
          [(pair? ls)
           (string-set! s i (car ls))
           (f s (- i 1) (cdr ls))]
          [else s])))
    (if (input-port? p)
        (get-it p)
        (error 'get-line "not an input port" p)))
  
  (module (put-string)
    (import (ikarus system $strings)
            (ikarus system $fx))
    (define ($put-string p s i j)
      (unless ($fx= i j) 
        ($write-char ($string-ref s i) p)
        ($put-string p s ($fx+ i 1) j))) 
    (define put-string
      (case-lambda
        [(p s) 
         (unless (output-port? p) 
           (error 'put-string "not an output port" p))
         (unless (string? s) 
           (error 'put-string "not a string" s))
         ($put-string p s 0 (string-length s))]
        [(p s i) 
         (unless (output-port? p) 
           (error 'put-string "not an output port" p))
         (unless (string? s) 
           (error 'put-string "not a string" s))
         (let ([len ($string-length s)])
           (unless (fixnum? i) 
             (error 'put-string "starting index is not a fixnum" i))
           (when (or ($fx< i 0) ($fx> i len))
             (error 'put-string 
               (format "starting index is out of range 0..~a" len)
               i))
           ($put-string p s i len))]
        [(p s i c) 
         (unless (output-port? p) 
           (error 'put-string "not an output port" p))
         (unless (string? s) 
           (error 'put-string "not a string" s))
         (let ([len ($string-length s)])
           (unless (fixnum? i) 
             (error 'put-string "starting index is not a fixnum" i))
           (when (or ($fx< i 0) ($fx> i len))
             (error 'put-string 
               (format "starting index is out of range 0..~a" len)
               i))
           (unless (fixnum? c) 
             (error 'put-string "count is not a fixnum" c))
           (let ([j (+ i c)])
             (when (or ($fx< c 0) (> j len))
               (error 'put-string 
                 (format "count is out of range 0..~a" (- len i))
                 c))
             ($put-string p s i j)))])))
 
  (module (put-bytevector)
    (import (ikarus system $bytevectors)
            (ikarus system $fx))
    (define ($put-bytevector p bv i j)
      (unless ($fx= i j) 
        ($write-byte ($bytevector-u8-ref bv i) p)
        ($put-bytevector p bv ($fx+ i 1) j)))
    (define put-bytevector
      (case-lambda
        [(p s) 
         (unless (output-port? p) 
           (error 'put-bytevector "not an output port" p))
         (unless (bytevector? s) 
           (error 'put-bytevector "not a bytevector" s))
         ($put-bytevector p s 0 (bytevector-length s))]
        [(p s i) 
         (unless (output-port? p) 
           (error 'put-bytevector "not an output port" p))
         (unless (bytevector? s) 
           (error 'put-bytevector "not a bytevector" s))
         (let ([len ($bytevector-length s)])
           (unless (fixnum? i) 
             (error 'put-bytevector "starting index is not a fixnum" i))
           (when (or ($fx< i 0) ($fx> i len))
             (error 'put-bytevector 
               (format "starting index is out of range 0..~a" len)
               i))
           ($put-bytevector p s i len))]
        [(p s i c) 
         (unless (output-port? p) 
           (error 'put-bytevector "not an output port" p))
         (unless (bytevector? s) 
           (error 'put-bytevector "not a bytevector" s))
         (let ([len ($bytevector-length s)])
           (unless (fixnum? i) 
             (error 'put-bytevector "starting index is not a fixnum" i))
           (when (or ($fx< i 0) ($fx> i len))
             (error 'put-bytevector 
               (format "starting index is out of range 0..~a" len)
               i))
           (unless (fixnum? c) 
             (error 'put-bytevector "count is not a fixnum" c))
           (let ([j (+ i c)])
             (when (or ($fx< c 0) (> j len))
               (error 'put-bytevector 
                 (format "count is out of range 0..~a" (- len i))
                 c))
             ($put-bytevector p s i j)))])))


  (define (get-string-n p n) 
    (import (ikarus system $fx) (ikarus system $strings))
    (unless (input-port? p) 
      (error 'get-string-n "not an input port" p))
    (unless (fixnum? n) 
      (error 'get-string-n "count is not a fixnum" n))
    (cond
      [($fx> n 0) 
       (let ([s ($make-string n)])
         (let f ([p p] [n n] [s s] [i 0])
           (let ([x ($read-char p)])
             (cond
               [(eof-object? x) 
                (if ($fx= i 0) 
                    (eof-object)
                    (substring s 0 i))]
               [else
                ($string-set! s i x) 
                (let ([i ($fxadd1 i)])
                  (if ($fx= i n) 
                      s
                      (f p n s i)))]))))]
      [($fx= n 0) ""]
      [else (error 'get-string-n "count is negative" n)]))

  (define (get-string-n! p s i c) 
    (import (ikarus system $fx) (ikarus system $strings))
    (unless (input-port? p) 
      (error 'get-string-n! "not an input port" p))
    (unless (string? s) 
      (error 'get-string-n! "not a string" s))
    (let ([len ($string-length s)])
      (unless (fixnum? i) 
        (error 'get-string-n! "starting index is not a fixnum" i))
      (when (or ($fx< i 0) ($fx> i len))
        (error 'get-string-n! 
          (format "starting index is out of range 0..~a" len)
          i))
      (unless (fixnum? c) 
        (error 'get-string-n! "count is not a fixnum" c))
      (cond
        [($fx> c 0)
         (let ([j (+ i c)])
           (when (> j len)
             (error 'get-string-n! 
               (format "count is out of range 0..~a" (- len i))
               c))
           (let ([x ($read-char p)])
             (cond
               [(eof-object? x) x]
               [else
                ($string-set! s i x)
                (let f ([p p] [s s] [start i] [i 1] [c c])
                  (let ([x ($read-char p)])
                    (cond
                      [(eof-object? x) i]
                      [else
                       ($string-set! s ($fx+ start i) x)
                       (let ([i ($fxadd1 i)])
                         (if ($fx= i c)
                             i
                             (f p s start i c)))])))])))]
        [($fx= c 0) 0]
        [else (error 'get-string-n! "count is negative" c)])))

  (define (get-bytevector-n p n) 
    (import (ikarus system $fx) (ikarus system $bytevectors))
    (define (subbytevector s n)
      (let ([p ($make-bytevector n)])
        (let f ([s s] [n n] [p p])
          (let ([n ($fx- n 1)])
            ($bytevector-set! p n ($bytevector-u8-ref s n))
            (if ($fx= n 0) 
                p
                (f s n p))))))
    (unless (input-port? p) 
      (error 'get-bytevector-n "not an input port" p))
    (unless (fixnum? n) 
      (error 'get-bytevector-n "count is not a fixnum" n))
    (cond
      [($fx> n 0) 
       (let ([s ($make-bytevector n)])
         (let f ([p p] [n n] [s s] [i 0])
           (let ([x ($get-u8 p)])
             (cond
               [(eof-object? x) 
                (if ($fx= i 0) 
                    (eof-object)
                    (subbytevector s i))]
               [else
                ($bytevector-set! s i x)
                (let ([i ($fxadd1 i)])
                  (if ($fx= i n) 
                      s
                      (f p n s i)))]))))]
      [($fx= n 0) '#vu8()]
      [else (error 'get-bytevector-n "count is negative" n)]))
  )

