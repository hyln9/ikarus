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
          put-u8 put-char get-char get-u8
          newline port-name input-port-name output-port-name
          close-input-port reset-input-port! 
          flush-output-port close-output-port get-line)
  (import 
    (ikarus system $io)
    (ikarus system $fx)
    (ikarus system $ports)
    (except (ikarus) read-char unread-char peek-char write-char write-byte 
            put-u8 put-char get-char get-u8
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
  
  )

