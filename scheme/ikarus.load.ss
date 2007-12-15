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


(library (ikarus load)
  (export load load-r6rs-top-level)
  (import 
    (except (ikarus) load)
    (only (psyntax expander) eval-top-level eval-r6rs-top-level)
    (only (ikarus reader) read-initial))

  (define load-handler
    (lambda (x)
      (eval-top-level x)))
  (define read-and-eval
    (lambda (p eval-proc)
      (let ([x (read p)])
        (unless (eof-object? x)
          (eval-proc x)
          (read-and-eval p eval-proc)))))
  (define load
    (case-lambda
      [(x) (load x load-handler)]
      [(x eval-proc)
       (unless (string? x)
         (die 'load "not a string" x))
       (unless (procedure? eval-proc)
         (die 'load "not a procedure" eval-proc))
       (let ([p (open-input-file x)])
         (let ([x (read-initial p)])
           (unless (eof-object? x)
             (eval-proc x)
             (read-and-eval p eval-proc)))
         (close-input-port p))]))
  (define load-r6rs-top-level
    (lambda (x)
      (define (read-file)
        (let ([p (open-input-file x)])
          (let ([x (read-initial p)])
            (if (eof-object? x)
                (begin (close-input-port p) '())
                (cons x 
                  (let f ()
                    (let ([x (read p)])
                      (cond
                        [(eof-object? x) 
                         (close-input-port p)
                         '()]
                        [else (cons x (f))]))))))))
      (let ([prog (read-file)])
        (eval-r6rs-top-level prog))))
  )
