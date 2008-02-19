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


(library (ikarus load)
  (export load load-r6rs-top-level)
  (import 
    (except (ikarus) load)
    (only (ikarus.compiler) compile-core-expr)
    (only (psyntax library-manager) 
      serialize-all current-precompiled-library-loader)
    (only (psyntax expander) eval-top-level compile-r6rs-top-level)
    (only (ikarus reader) read-initial))


  (define-struct serialized-library (contents))

  (define (load-serialized-library filename sk)
    ;;; TODO: check file last-modified date
    (let ([ikfasl (string-append filename ".ikfasl")])
      (cond
        [(not (file-exists? ikfasl)) #f]
        [(<= (file-ctime ikfasl) (file-ctime filename))
         (printf 
            "WARNING: not using fasl file ~s because it is older \
             than the source file ~s\n" 
           ikfasl
           filename)
         #f]
        [else
         (let ([x 
                (let ([p (open-file-input-port ikfasl)])
                  (let ([x (fasl-read p)])
                    (close-input-port p)
                    x))])
           (if (serialized-library? x)
               (apply sk (serialized-library-contents x))
               (begin
                 (printf
                    "WARNING: not using fasl file ~s because it was \
                     compiled with a different version of ikarus.\n" 
                    ikfasl)
                 #f)))])))

  (define (do-serialize-library filename contents)
    (let ([ikfasl (string-append filename ".ikfasl")])
      (printf "Serializing ~s\n" ikfasl)
      (let ([p (open-file-output-port ikfasl (file-options no-fail))])
        (fasl-write (make-serialized-library contents) p)
        (close-output-port p))))

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
    (lambda (x how)
      (define (read-file)
        (let ([p (open-input-file x)])
          (let ([x (read-script-annotated p)])
            (if (eof-object? x)
                (begin (close-input-port p) '())
                (cons x 
                  (let f ()
                    (let ([x (read-annotated p)])
                      (cond
                        [(eof-object? x) 
                         (close-input-port p)
                         '()]
                        [else (cons x (f))]))))))))
      (let ([prog (read-file)])
        (let ([thunk (compile-r6rs-top-level prog)])
          (case how
            [(run) (thunk)]
            [(compile) 
             (serialize-all 
               (lambda (file-name contents)
                 (do-serialize-library file-name contents))
               (lambda (core-expr) 
                 (compile-core-expr core-expr)))]
            [else (error 'load-r6rs-top-level "invali argument" how)])))))

  (current-precompiled-library-loader load-serialized-library)
  
  )
