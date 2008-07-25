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


(library (psyntax compat)
  (export define-record make-parameter parameterize format gensym
          eval-core symbol-value set-symbol-value!
          make-struct-type read-annotated
          annotation? annotation-expression annotation-source
          annotation-stripped
          read-library-source-file)
  (import 
    (only (ikarus.compiler) eval-core)
    (only (ikarus.reader.annotated) read-library-source-file)
    (ikarus))


  (define-syntax define-record
    (syntax-rules ()
      [(_ name (field* ...) printer)
       (begin
         (define-struct name (field* ...))
         (module ()
            (set-rtd-printer! (type-descriptor name)
              printer)))]
      [(_ name (field* ...))
       (define-struct name (field* ...))])))

