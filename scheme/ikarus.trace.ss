;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
  (export make-traced-procedure make-traced-macro) 
  (import (except (ikarus) make-traced-procedure make-traced-macro))


  (define-struct scell (cf trace filter prev))

  (define (mkcell prev)
    (make-scell #f #f #f prev))

  (define *scell* (mkcell #f))

  (define *trace-depth* 0)

  (define display-prefix
    (lambda (n)
      (let f ([i 0])
        (unless (= i n)
          (display (if (even? i) "|" " "))
          (f (+ i 1))))))

  (define display-trace
    (lambda (k* v)
      (display-prefix k* #t)
      (write v)
      (newline)))

  (define (display-return-trace n ls)
    (display-prefix n)
    (unless (null? ls)
      (write (car ls))
      (let f ([ls (cdr ls)])
        (unless (null? ls)
          (write-char #\space)
          (write (car ls))
          (f (cdr ls)))))
    (newline))

  (define (display-call-trace n ls)
    (display-prefix n)
    (write ls)
    (newline))
 
  (define (stacked-call pre thunk post)
    (call/cf
      (lambda (cf)
        (if (eq? cf (scell-cf *scell*))
            (thunk)
            (dynamic-wind
              (let ([scell (mkcell *scell*)])
                (lambda () 
                  (set! *scell* scell)
                  (pre)))
              (lambda ()
                (call-with-values
                  (lambda ()
                    (call/cf
                      (lambda (cf)
                        (set-scell-cf! *scell* cf)
                        (thunk))))
                  return-handler))
              (lambda ()
                (post)
                (set! *scell* (scell-prev *scell*))))))))

  (define return-handler
    (lambda v*
      (cond
        [(scell-trace *scell*) =>
         (lambda (n)
           (display-return-trace n ((scell-filter *scell*) v*)))])
      (apply values v*)))

  (define make-traced-procedure
    (case-lambda
      [(name proc) (make-traced-procedure name proc (lambda (x) x))]
      [(name proc filter)
       (lambda args
         (stacked-call 
           (lambda ()
             (set! *trace-depth* (add1 *trace-depth*)))
           (lambda ()
             (set-scell-trace! *scell* *trace-depth*)
             (set-scell-filter! *scell* filter)
             (display-call-trace *trace-depth* (filter (cons name args)))
             (apply proc args))
           (lambda ()
             (set! *trace-depth* (sub1 *trace-depth*)))))]))


  (define make-traced-macro
    (lambda (name x)
      (cond
        [(procedure? x) 
         (make-traced-procedure name x syntax->datum)]
        [(variable-transformer? x)
         (make-variable-transformer
           (make-traced-procedure name 
             (variable-transformer-procedure x)
             syntax->datum))]
        [else x]))))


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
                 (die 'trace
                        "the top-level value is not a procedure"
                     s v))
               (let ([p (make-traced-procedure s v)])
                 (set-car! a v)
                 (set-cdr! a p)
                 (set-symbol-value! s p)))))]
        [else
         (unless (symbol-bound? s)
           (die 'trace "unbound" s))
         (let ([v (symbol-value s)])
           (unless (procedure? v)
             (die 'trace "the top-level value is not a procedure" s v))
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

