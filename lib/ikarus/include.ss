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



(library (ikarus include)
  (export include include/lexical-context)
  (import (except (ikarus) include))

  (define-syntax include/lexical-context/form
    (lambda (x)
      (syntax-case x ()
        [(_ filename id form who)
         (let* ([filename^ 
                 (let ([x (syntax->datum #'filename)])
                   (if (string? x)
                       x
                       (syntax-violation #f "file name is not a string"
                          #'form #'filename)))]
                [content 
                 (with-exception-handler
                   (lambda (x)
                     (raise-continuable 
                       (condition 
                         (make-who-condition #'who)
                         x)))
                   (lambda () 
                     (with-input-from-file filename^
                       (lambda ()
                         (let loop ()
                           (let ([x (read)])
                             (if (eof-object? x)
                                 '()
                                 (cons (datum->syntax #'id x)
                                   (loop)))))))))])
           #`(stale-when
               (or (not (file-exists? filename))
                   (> (file-mtime filename) #,(file-mtime filename^)))
               #,@content))])))
  
  (define-syntax include/lexical-context
    (lambda (x)
      (syntax-case x ()
        [(kwd filename id)
         (if (identifier? #'id)
             #`(include/lexical-context/form filename id #,x kwd)
             (syntax-violation #f "not an identifier" x #'id))])))

  (define-syntax include
    (lambda (x)
      (syntax-case x ()
        [(kwd filename)
         #`(include/lexical-context/form filename kwd #,x kwd)]))))

