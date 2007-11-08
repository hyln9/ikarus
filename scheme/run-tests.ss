#!../src/ikarus -b ikarus.boot --r6rs-script

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


(import (ikarus)
        (tests reader)
        (tests lists)
        (tests bytevectors)
        (tests strings)
        (tests hashtables)
        ;(tests numbers)
        (tests bignums)
        (tests fxcarry)
        (tests bignum-to-flonum)
        (tests string-to-number)
        (tests input-ports)
        )

(define (test-exact-integer-sqrt)
  (define (f i j inc)
    (when (< i j)
      (let-values ([(s r) (exact-integer-sqrt i)])
        (unless (and (= (+ (* s s) r) i)
                     (< i (* (+ s 1) (+ s 1))))
          (error 'exact-integer-sqrt "wrong result" i))
        (f (+ i inc) j inc))))
  (f 0 10000 1)
  (f 0 536870911 10000)
  (f 0 536870911000 536870911)
  (printf "[exact-integer-sqrt] Happy Happy Joy Joy\n"))

(test-reader)
(test-char-syntax)
(test-bytevectors)
(test-strings)
(test-exact-integer-sqrt)
(test-bignum-to-flonum)
(test-string-to-number)
;(test-div-and-mod)
(test-bignums)
(test-fxcarry)
(test-lists)
(test-hashtables)
(test-input-ports)
(test-bignum-conversion)
(printf "Happy Happy Joy Joy\n")
