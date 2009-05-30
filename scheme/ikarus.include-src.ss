;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2009  Abdulaziz Ghuloum
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

(library (ikarus.include-src)
  (export include-src)
  (import (ikarus) (ikarus include))
  (define-syntax include-src
    (lambda (x)
      (syntax-case x ()
        [(ctxt filename)
         (with-syntax ([filename 
                        (string-append 
                          (or (getenv "IKARUS_SRC_DIR") ".") 
                          "/"
                          (syntax->datum #'filename))])
           #'(include/lexical-context filename ctxt))]))))
