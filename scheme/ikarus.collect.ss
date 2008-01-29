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


(library (ikarus collect)
  (export do-overflow do-overflow-words do-vararg-overflow collect
          do-stack-overflow collect-key)
  (import 
    (except (ikarus) collect collect-key)
    (ikarus system $fx)
    (ikarus system $arg-list))

(define do-overflow
  (lambda (n)
    (foreign-call "ik_collect" n)
    (void)))

(define do-overflow-words
  (lambda (n)
    (foreign-call "ik_collect" ($fxsll n 2))
    (void)))

(define do-vararg-overflow
  (lambda (n)
    (foreign-call "ik_collect_vararg" n)
    (void)))

(define collect
  (lambda ()
    (do-overflow 4096)))

(define do-stack-overflow
  (lambda ()
    (foreign-call "ik_stack_overflow")))

(define dump-metatable
  (lambda ()
    (foreign-call "ik_dump_metatable")))

(define dump-dirty-vector
  (lambda ()
    (foreign-call "ik_dump_dirty_vector")))

(define (collect-key)
  (or ($collect-key) 
      (begin
        ($collect-key (gensym))
        (collect-key))))

)
