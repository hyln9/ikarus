

;;; this is here to test that we can import things from other
;;; libraries within the compiler itself.

(library (ikarus greeting)
  (export print-greeting)
  (import (ikarus))
  (define (print-greeting)
    (define-syntax compile-time-string
      (lambda (x) (date-string)))
    (printf "Ikarus Scheme (Build ~a)\n" (compile-time-string))
    (display "Copyright (c) 2006-2007 Abdulaziz Ghuloum\n\n")))


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
       ;(for-each load files)
       (load-r6rs-top-level script)
       ;(load script)
       (exit 0)]
      [script ; no greeting, no cafe
       (command-line-arguments (cons script args))
       (for-each load files)
       (load script)
       (exit 0)]
      [else
       (print-greeting)
       (command-line-arguments args)
       (for-each load files)
       (new-cafe)
       (exit 0)])))
