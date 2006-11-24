
(define all-tests '())

(define input-filter 
  (make-parameter (lambda (x) x)
    (lambda (x)
      (unless (procedure? x)
        (error 'input-filter "not a procedure ~s" x))
      x)))

(define runtime-file 
  (make-parameter
    "runtime.c"
    (lambda (fname)
      (unless (string? fname) (error 'runtime-file "not a string" fname))
      fname)))

;;; (define-syntax add-tests-with-string-output
;;;   (syntax-rules (=>)
;;;     [(_ test-name [expr => output-string] ...)
;;;      (set! all-tests
;;;         (append all-tests
;;;            (cons `[banner test-name ,(length '(expr ...))]
;;;                  '([expr string  output-string] ...))))]))

(define compile-port
  (make-parameter
    (current-output-port)
    (lambda (p)
       (unless (output-port? p) 
         (error 'compile-port "not an output port ~s" p))
       p)))

(define show-compiler-output (make-parameter #f))

(define (run-compile expr)
  (let ([p (open-output-file "stst.s" 'replace)])
    (parameterize ([compile-port p])
       (compile-program expr))
    (close-output-port p))
  (when (show-compiler-output)
    (system (format "less stst.s"))))

(define (build)
  (unless (fxzero? (system (format "gcc -o stst ~a stst.s" (runtime-file))))
    (error 'make "could not build target")))

(define (execute)
  (unless (fxzero? (system "./stst > stst.out"))
    (error 'make "produced program exited abnormally")))


(define (build-program expr)
   (run-compile expr)
   (build))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (with-input-from-file "stst.out"
        (lambda ()
          (let f ()
            (let ([c (read-char)])
              (cond
               [(eof-object? c) (void)]
               [else (display c) (f)]))))))))

(define (test-with-string-output test-id expr expected-output)
   (run-compile expr)
   (build)
   (execute)
   (unless (string=? expected-output (get-string))
     (error 'test "output mismatch for test ~s, expected ~s, got ~s"
        test-id expected-output (get-string))))

(define (test-one test-id out-of test)
  (let ([expr ((input-filter) (car test))]
        [type (cadr test)]
        [out  (caddr test)])
    (printf "[~s/~s]: ~s ..." test-id out-of expr)
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out)]
     [else (error 'test "invalid test type ~s" type)])
    (printf " ok\n")))
  
(define (test-all)
  (let f ([total 0] [section 0] [i 0] [tests all-tests])
    (cond
     [(null? tests)
      (printf "passed all ~s tests\n" total)]
     [(eq? (caar tests) 'banner)
      (let ([b (cdar tests)])
        (let ([str (format "Performing ~a tests\n" (car b))])
          ;(display (make-string (string-length str) #\=))
          (newline)
          (display str)
          (display (make-string (string-length str) #\~))
          (newline))
        (f total (cadr b) 1 (cdr tests)))]
     [else
      (test-one i section (car tests))
      (f (fxadd1 total) section (fxadd1 i) (cdr tests))])))
  
(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))
