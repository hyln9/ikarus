
;;; WRITER provides display and write.

(let ()
  (define char-table ; first nonprintable chars
    '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel" "bs" "tab" "newline"
       "vt" "ff" "return" "so" "si" "dle" "dc1" "dc2" "dc3" "dc4" "nak" 
       "syn" "etb" "can" "em" "sub" "esc" "fs" "gs" "rs" "us" "space"))
  (define write-character
    (lambda (x p m)
      (if m 
          (let ([i (char->fixnum x)])
            (write-char #\# p)
            (write-char #\\ p)
            (cond
             [(fx< i (vector-length char-table))
              (write-char* (vector-ref char-table i) p)]
             [(fx< i 127)
              (write-char x p)]
             [(fx= i 127) 
              (write-char* "del" p)]
             [else
              (error 'writer "invalid character index ~s" i)]))
          (write-char x p))))
  (define write-list
    (lambda (x p m)
      (cond
       [(pair? x)
        (write-char #\space p)
        (writer (car x) p m)
        (write-list (cdr x) p m)]
       [(not (null? x))
        (write-char #\space p)
        (write-char #\. p)
        (write-char #\space p)
        (writer x p m)])))
  (define write-vector
    (lambda (x p m)
      (write-char #\# p)
      (write-char #\( p)
      (let ([n (vector-length x)])
        (when (fx> n 0)
          (writer (vector-ref x 0) p m)
          (letrec ([f 
                    (lambda (i)
                      (unless (fx= i n)
                        (write-char #\space p)
                        (writer (vector-ref x i) p m)
                        (f (fxadd1 i))))])
            (f 1))))
      (write-char #\) p)))
  (define write-record
    (lambda (x p m)
      (write-char #\# p)
      (write-char #\[ p)
      (writer (record-name x) p m)
      (let ([n (record-length x)])
        (letrec ([f 
                  (lambda (i)
                    (unless (fx= i n)
                      (write-char #\space p)
                      (writer (record-ref x i) p m)
                      (f (fxadd1 i))))])
          (f 0)))
      (write-char #\] p)))
  (define write-symbol
    (lambda (x p)
      (write-char* (symbol->string x) p)))
  (define write-gensym
    (lambda (x p)
      (write-char* "#{" p)
      (write-char* (symbol->string x) p)
      (write-char #\space p)
      (write-char* (gensym->unique-string x) p)
      (write-char #\} p)))
  (define write-string-escape
    (lambda (x p)
      (define loop 
        (lambda (x i n p)
         (unless (fx= i n)
           (let ([c (string-ref x i)])
             (when (or (char= #\" c) (char= #\\ c))
               (write-char #\\ p))
             (write-char c p))
           (loop x (fxadd1 i) n p)))) 
      (write-char #\" p)
      (loop x 0 (string-length x) p)
      (write-char #\" p)))
  (define write-string
    (lambda (x p m)
      (if m
          (write-string-escape x p)
          (write-char* x p))))
  (define write-fixnum
    (lambda (x p)
      (define loop
        (lambda (x p)
          (unless (fxzero? x)
            (loop (fxquotient x 10) p)
            (write-char 
               (fixnum->char
                  (fx+ (fxremainder x 10)
                       (char->fixnum #\0)))
               p))))
      (cond
       [(fxzero? x) (write-char #\0 p)]
       [(fx< x 0)
        (write-char #\- p)
        (if (fx= x -536870912)
            (write-char* "536870912" p)
            (loop (fx- 0 x) p))]
       [else (loop x p)])))
  (define write-char*
    (lambda (x p)
      (define loop 
        (lambda (x i n p)
         (unless (fx= i n)
           (write-char (string-ref x i) p)
           (loop x (fxadd1 i) n p)))) 
      (loop x 0 (string-length x) p)))
  (define writer
    (lambda (x p m)
      (cond
        [(pair? x) 
         (write-char #\( p)
         (writer (car x) p m)
         (write-list (cdr x) p m)
         (write-char #\) p)]
        [(symbol? x) 
         (if (gensym? x)
             (write-gensym x p)
             (write-symbol x p))]
        [(fixnum? x)
         (write-fixnum x p)]
        [(string? x)
         (write-string x p m)]
        [(boolean? x)
         (write-char* (if x "#t" "#f") p)]
        [(char? x)
         (write-character x p m)]
        [(procedure? x)
         (write-char* "#<procedure>" p)]
        [(output-port? x)
         (write-char* "#<output-port " p)
         (writer (output-port-name x) p #t)
         (write-char #\> p)]
        [(input-port? x)
         (write-char* "#<input-port " p)
         (writer (input-port-name x) p #t)
         (write-char #\> p)]
        [(vector? x)
         (write-vector x p m)]
        [(null? x) 
         (write-char #\( p)
         (write-char #\) p)]
        [(eq? x (void)) 
         (write-char* "#<void>" p)]
        [(eof-object? x)
         (write-char* "#!eof" p)]
        [(record? x)
         (let ([printer (record-printer x)])
           (if (procedure? printer)
               (printer x p)
               (write-record x p m)))]
        [(code? x)
         (write-char* "#<code>" p)]
        [else 
         (write-char* "#<unknown>" p)])))
  (define generic-writer
    (lambda (who)
      (lambda (x . p)
        (let ([port
               (if (null? p)
                   (current-output-port)
                   (if (null? (cdr p))
                       (let ([p (car p)])
                          (if (output-port? p)
                              p
                              (error who "not an output port ~s" p)))
                       (error who "too many arguments")))])
          (writer x port (eq? who 'write))
          (flush-output-port port)))))
  

  (define print-error
    (lambda (who fmt . args)
      (define p (standard-error-port))
      (define f
        (lambda (i args)
          (unless (fx= i (string-length fmt))
            (let ([c (string-ref fmt i)])
              (cond
               [(char= c #\~)
                (let ([i (fxadd1 i)])
                  (when (fx= i (string-length fmt))
                    (error 'error "invalid ~~ at end of format string ~s" fmt))
                  (let ([c (string-ref fmt i)])
                   (cond
                     [(char= c #\~) 
                      (write-char #\~ p)
                      (f (fxadd1 i) args)]
                     [(char= c #\a)
                      (when (null? args)
                        (error 'error "insufficient arguments"))
                      (display (car args) p)
                      (f (fxadd1 i) (cdr args))]
                     [(char= c #\s)
                      (when (null? args)
                        (error 'error "insufficient arguments"))
                      (write (car args) p)
                      (f (fxadd1 i) (cdr args))]
                     [else
                      (error 'error "invalid sequence ~~~a" c)])))]
               [else 
                (write-char c p)
                (f (fxadd1 i) args)])))))
      (display "Error" p)
      (when who
        (display " in " p)
        (display who p))
      (display ": " p)
      (unless (string? fmt)
        (error 'error "not a string ~s" fmt))
      (f 0 args)
      (write-char #\. p)
      (newline p)))

  ($pcb-set! display (generic-writer 'display))
  ($pcb-set! write (generic-writer 'write))

  ($pcb-set! print-error print-error)

  ($pcb-set! current-error-handler
    (make-parameter
      (lambda args
        (apply print-error args)
        (display "exiting\n")
        (flush-output-port)
        (exit -100))
      (lambda (x)
        (if (procedure? x)
            x
            (error 'current-error-handler "~s is not a procedure" x)))))

  ($pcb-set! error
    (lambda args
      (apply (current-error-handler) args)))
 
) 

