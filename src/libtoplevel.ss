



;;; Finally, we're ready to evaluate the files and enter the cafe.

(library (ikarus interaction)
  (export bar)
  (import (scheme))

  (define bar 'i-am-an-exported-primitive-named-foo)
  (define sc-expand
    (lambda (x)
      (if (and (pair? x) (equal? (car x) "noexpand"))
          (cadr x)
          (chi-top-library x))))

  (primitive-set! 'expand
    (lambda (x)
      (sc-expand x)))

  (let-values ([(files script args)
                (let f ([args (command-line-arguments)])
                  (cond
                    [(null? args) (values '() #f '())]
                    [(string=? (car args) "--")
                     (values '() #f (cdr args))]
                    [(string=? (car args) "--script")
                     (let ([d (cdr args)])
                       (cond
                         [(null? d) 
                          (error #f "--script requires a script name")]
                         [else
                          (values '() (car d) (cdr d))]))]
                    [else
                     (let-values ([(f* script a*) (f (cdr args))])
                       (values (cons (car args) f*) script a*))]))])
    (cond
      [script ; no greeting, no cafe
       (command-line-arguments (cons script args))
       (for-each load files)
       (load script)
       (exit 0)]
      [else
       (let ()
         (define-syntax compile-time-string
           (lambda (x) (date-string)))
         (printf "Ikarus Scheme (Build ~a)\n" (compile-time-string)))
       (display "Copyright (c) 2006-2007 Abdulaziz Ghuloum\n\n")
       (command-line-arguments args)
       (for-each load files)
       (new-cafe)
       (exit 0)])))
