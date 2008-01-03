#!/usr/bin/env ikarus --r6rs-script
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

;;; vim:syntax=scheme
(import 
  (ikarus compiler)
  (except (ikarus) assembler-output))

(define (compile1 x)
  (printf "Compiling ~s\n" x)
  (let ([p (open-file-output-port "test64.boot" (file-options no-fail))])
    (parameterize ([assembler-output #t])
      (compile-core-expr-to-port x p))
    (close-output-port p)))

(define (compile-and-run x) 
  (compile1 x)
  (let ([rs (system "../src/ikarus -b test64.boot > test64.out")])
    (unless (= rs 0) (error 'run1 "died"))
    (with-input-from-file "test64.out" read)))

(define (compile-test-and-run expr expected)
  (let ([val (compile-and-run expr)])
    (unless (equal? val expected) 
      (error 'compile-test-and-run "failed:got:expected" val expected))))

(define all-tests
  '([(quote 42)   42]
    [(quote #f)   #f]
    [(quote ())  ()]))

(for-each
  (lambda (x)
     (compile-test-and-run (car x) (cadr x)))
  all-tests)


(printf "Happy Happy Joy Joy\n")

