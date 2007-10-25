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


(library (ikarus trace)
  (export make-traced-procedure) 
  (import (except (ikarus) make-traced-procedure))

  (define k* '())
  
  (define display-prefix
    (lambda (ls t)
      (unless (null? ls)
        (display (if t "|" " "))
        (display-prefix (cdr ls) (not t)))))
  
  (define display-trace
    (lambda (k* v)
      (display-prefix k* #t)
      (write v)
      (newline)))

  (define make-traced-procedure
    (lambda (name proc)
      (lambda args
        (call/cf
          (lambda (f)
            (cond
              [(memq f k*) => 
               (lambda (ls)
                 (display-trace ls (cons name args))
                 (apply proc args))]
              [else
               (display-trace (cons 1 k*) (cons name args))
               (dynamic-wind
                 (lambda () (set! k* (cons f k*)))
                 (lambda () 
                   (call-with-values
                     (lambda ()
                       (call/cf
                         (lambda (nf)
                           (set! f nf)
                           (set-car! k* nf)
                           (apply proc args))))
                     (lambda v*
                       (display-prefix k* #t)
                       (unless (null? v*)
                         (write (car v*))
                         (let f ([v* (cdr v*)])
                           (cond
                             [(null? v*) (newline)]
                             [else
                              (write-char #\space)
                              (write (car v*))
                              (f (cdr v*))])))
                       (apply values v*))))
                 (lambda () (set! k* (cdr k*))))])))))))


#!eof
  
(define traced-symbols '())

  (define untrace-symbol!
    (lambda (s)
      (define loop
        (lambda (ls)
          (cond
            [(null? ls) '()]
            [(eq? s (caar ls))
             (let ([a (cdar ls)])
               (when (eq? (cdr a) (symbol-value s))
                 (set-symbol-value! s (car a)))
               (cdr ls))]
            [else (cons (car ls) (loop (cdr ls)))])))
      (set! traced-symbols (loop traced-symbols))))

  (define trace-symbol!
    (lambda (s)
      (cond
        [(assq s traced-symbols) =>
         (lambda (pr)
           (let ([a (cdr pr)] [v (symbol-value s)])
             (unless (eq? (cdr a) v)
               (unless (procedure? v)
                 (error 'trace
                        "the top-level value is not a procedure"
                     s v))
               (let ([p (make-traced-procedure s v)])
                 (set-car! a v)
                 (set-cdr! a p)
                 (set-symbol-value! s p)))))]
        [else
         (unless (symbol-bound? s)
           (error 'trace "unbound" s))
         (let ([v (symbol-value s)])
           (unless (procedure? v)
             (error 'trace "the top-level value is not a procedure" s v))
           (let ([p (make-traced-procedure s v)])
             (set! traced-symbols 
               (cons (cons s (cons v p)) traced-symbols))
             (set-symbol-value! s p)))])))
Try:

(trace-define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (sub1 n))))))
(fact 5)

(trace-define (fact n m)
  (cond
    [(zero? n) m]
    [else (fact (sub1 n) (* n m))]))
(fact 5 1)


(trace-define (fact n m k)
  (cond
    [(zero? n) (k m)]
    [else (begin (fact (sub1 n) (* n m) k) 0)]))


(call/cc
  (lambda (k)
    (fact 6 1
      (trace-lambda escape (v) (k v)))))


(call/cc
  (lambda k*
    (trace-define (fact n)
      (cond
        [(zero? n) 
         (call/cc
           (lambda (k) 
             (set! k* (cons k k*)) 
             1))]
        [else (* n (fact (sub1 n)))]))
    (fact 9)
    (let ([k (car k*)])
      (set! k* (cdr k*))
      (k 100000))))





(trace-define (infinite-loop n)
  (infinite-loop (add1 n)))
(infinite-loop 0)

