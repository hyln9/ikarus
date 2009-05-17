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
    (printf "Ikarus Scheme version ~a~a~a~a\n" 
      ikarus-version
      (if (zero? (string-length ikarus-revision)) "" "+")
      (if (= (fixnum-width) 30)
          ""
          ", 64-bit")
      (if (zero? (string-length ikarus-revision)) 
          ""
          (format " (revision ~a, build ~a)"
            (+ 1 (string->number ikarus-revision))
            (let-syntax ([ds (lambda (x) (date-string))])
              ds))))
    (display "Copyright (c) 2006-2009 Abdulaziz Ghuloum\n\n"))

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
          (split (or (getenv "IKARUS_LIBRARY_PATH") ""))
          (list ikarus-lib-dir))))
    (let ([prefix
           (lambda (ext ls)
             (append (map (lambda (x) (string-append ext x)) ls) ls))])
      (library-extensions 
        (prefix "/main" 
          (prefix ".ikarus"
            (library-extensions)))))))


;;; Finally, we're ready to evaluate the files and enter the cafe.

(library (ikarus main)
  (export)
  (import (except (ikarus) load-r6rs-script)
          (except (ikarus startup) host-info)
          (only (ikarus.compiler) generate-debug-calls)
          (ikarus.debugger)
          (only (psyntax library-manager) current-library-expander)
          (only (ikarus.reader.annotated) read-source-file)
          (only (ikarus.symbol-table) initialize-symbol-table!)
          (only (ikarus load) load-r6rs-script))
  (initialize-symbol-table!)
  (init-library-path)
  (let-values ([(files script script-type args)
                (let f ([args (command-line-arguments)])
                  (cond
                    [(null? args) (values '() #f #f '())]
                    [(member (car args) '("-d" "--debug"))
                     (generate-debug-calls #t)
                     (f (cdr args))]
                    [(member (car args) '("-nd" "--no-debug"))
                     (generate-debug-calls #f)
                     (f (cdr args))]
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
    (define (assert-null files who)
      (unless (null? files)
        (apply die 'ikarus
          (format "load files not allowed for ~a" who)
          files)))

    (define (start proc)
      (if (generate-debug-calls)
          (guarded-start proc)
          (proc)))
    (define-syntax doit
      (syntax-rules ()
        [(_ e e* ...)
         (start (lambda () e e* ...))]))
    
    (cond
      [(eq? script-type 'r6rs-script)
       (doit
         (command-line-arguments (cons script args))
         (for-each
           (lambda (filename)
             (for-each
               (lambda (src)
                 ((current-library-expander) src))
               (read-source-file filename)))
           files)
         (load-r6rs-script script #f #t))]
      [(eq? script-type 'compile)
       (assert-null files "--compile-dependencies")
       (doit
         (command-line-arguments (cons script args))
         (load-r6rs-script script #t #f))]
      [(eq? script-type 'script) ; no greeting, no cafe
       (command-line-arguments (cons script args))
       (doit
         (for-each load files)
         (load script))]
      [else
       (print-greeting)
       (command-line-arguments (cons "*interactive*" args))
       (doit (for-each load files))
       (new-cafe
         (lambda (x)
           (doit (eval x (interaction-environment)))))])

    (exit 0)))
