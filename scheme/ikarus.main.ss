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



;;; this is here to test that we can import things from other
;;; libraries within the compiler itself.

(library (ikarus startup)
  (export print-greeting init-library-path host-info)
  (import (except (ikarus) host-info))
  (include "ikarus.config.ss")

  (define (host-info) target)
  
  (define (print-greeting)
    (printf "Ikarus Scheme version ~a\n" 
      (if (zero? (string-length ikarus-revision))
          ikarus-version
          (format "~a+ (revision ~a, build ~a~a)"
            ikarus-version
            (+ 1 (string->number ikarus-revision))
            (let-syntax ([ds (lambda (x) (date-string))])
              ds)
            (if (= (fixnum-width) 30)
                ""
                ", 64-bit"))))
    (display "Copyright (c) 2006-2008 Abdulaziz Ghuloum\n\n"))

  (define (init-library-path) 
    (define (split s)
      (define (nodata i s ls)
        (cond
          [(= i (string-length s)) ls]
          [(char=? (string-ref s i) #\:) (nodata (+ i 1) s ls)]
          [else (data (+ i 1) s ls (list (string-ref s i)))]))
      (define (data i s ls ac)
        (cond
          [(= i (string-length s)) 
           (cons (list->string (reverse ac)) ls)]
          [(char=? (string-ref s i) #\:) 
           (nodata (+ i 1) s
             (cons (list->string (reverse ac)) ls))]
          [else (data (+ i 1) s ls (cons (string-ref s i) ac))]))
      (reverse (nodata 0 s '())))
    (library-path
      (cons "."
        (append 
          (split (getenv "IKARUS_LIBRARY_PATH"))
          (list ikarus-lib-dir))))
    (library-extensions 
      (append
        (map (lambda (x) (string-append ".ikarus" x))
             (library-extensions))
        (library-extensions)))))


;;; Finally, we're ready to evaluate the files and enter the cafe.

(library (ikarus main)
  (export)
  (import (ikarus)
          (except (ikarus startup) host-info)
          (only (ikarus load) load-r6rs-top-level))
  (init-library-path)
  (let-values ([(files script script-type args)
                (let f ([args (command-line-arguments)])
                  (cond
                    [(null? args) (values '() #f #f '())]
                    [(string=? (car args) "-O2")
                     (optimize-level 2)
                     (f (cdr args))]
                    [(string=? (car args) "-O1")
                     (optimize-level 1)
                     (f (cdr args))]
                    [(string=? (car args) "-O0")
                     (optimize-level 0)
                     (f (cdr args))]
                    [(string=? (car args) "--")
                     (values '() #f #f (cdr args))]
                    [(string=? (car args) "--script")
                     (let ([d (cdr args)])
                       (cond
                         [(null? d)
                          (die 'ikarus "--script requires a script name")]
                         [else
                          (values '() (car d) 'script (cdr d))]))]
                    [(string=? (car args) "--r6rs-script")
                     (let ([d (cdr args)])
                       (cond
                         [(null? d)
                          (die 'ikarus "--r6rs-script requires a script name")]
                         [else
                          (values '() (car d) 'r6rs-script (cdr d))]))]
                    [(string=? (car args) "--compile-dependencies")
                     (let ([d (cdr args)])
                       (cond
                         [(null? d)
                          (die 'ikarus
                            "--compile-dependencies requires a script name")]
                         [else
                          (values '() (car d) 'compile (cdr d))]))]
                    [else
                     (let-values ([(f* script script-type a*) (f (cdr args))])
                       (values (cons (car args) f*) script script-type a*))]))])
    (cond
      [(eq? script-type 'r6rs-script)
       (command-line-arguments (cons script args))
       (load-r6rs-top-level script 'run)
       (exit 0)]
      [(eq? script-type 'compile)
       (command-line-arguments (cons script args))
       (load-r6rs-top-level script 'compile)
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
