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



;;; this is here to test that we can import things from other
;;; libraries within the compiler itself.

(library (ikarus greeting)
  (export print-greeting)
  (import (ikarus))
  (letrec-syntax ([compile-time-string 
                    (lambda (x) 
                      (include "ikarus.version.ss")
                      ikarus-version)])
    (define (print-greeting)
      (printf "Ikarus Scheme version ~a\n" (compile-time-string))
      (display "Copyright (c) 2006-2007 Abdulaziz Ghuloum\n\n"))))


;;; Finally, we're ready to evaluate the files and enter the cafe.

(library (ikarus main)
  (export)
  (import (ikarus)
          (ikarus greeting)
          (only (ikarus load) load-r6rs-top-level))
  (let-values ([(files script script-type args)
                (let f ([args (command-line-arguments)])
                  (cond
                    [(null? args) (values '() #f #f '())]
                    [(string=? (car args) "--")
                     (values '() #f #f (cdr args))]
                    [(string=? (car args) "--script")
                     (let ([d (cdr args)])
                       (cond
                         [(null? d)
                          (error #f "--script requires a script name")]
                         [else
                          (values '() (car d) 'script (cdr d))]))]
                    [(string=? (car args) "--r6rs-script")
                     (let ([d (cdr args)])
                       (cond
                         [(null? d)
                          (error #f "--r6rs-script requires a script name")]
                         [else
                          (values '() (car d) 'r6rs-script (cdr d))]))]
                    [else
                     (let-values ([(f* script script-type a*) (f (cdr args))])
                       (values (cons (car args) f*) script script-type a*))]))])
    (cond
      [(eq? script-type 'r6rs-script)
       (command-line-arguments (cons script args))
       (load-r6rs-top-level script)
       (exit 0)]
      [(eq? script-type 'script) ; no greeting, no cafe
       (command-line-arguments (cons script args))
       (for-each load files)
       (load script)
       (exit 0)]
      [else
       (print-greeting)
       (command-line-arguments (cons "*interactive*" args))
       (for-each load files)
       (new-cafe)
       (exit 0)])))
