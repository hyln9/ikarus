
;;; 6.2: * added a printer for bwp-objects

;;; WRITER provides display and write.

(let ()
  (define char-table ; first nonprintable chars
    '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel" "bs" "tab" "newline"
       "vt" "ff" "return" "so" "si" "dle" "dc1" "dc2" "dc3" "dc4" "nak" 
       "syn" "etb" "can" "em" "sub" "esc" "fs" "gs" "rs" "us" "space"))
  (define write-character
    (lambda (x p m)
      (if m 
          (let ([i ($char->fixnum x)])
            (write-char #\# p)
            (cond
             [(fx< i (vector-length char-table))
              (write-char #\\ p)
              (write-char* (vector-ref char-table i) p)]
             [(fx< i 127)
              (write-char #\\ p)
              (write-char x p)]
             [(fx= i 127) 
              (write-char #\\ p)
              (write-char* "del" p)]
             [else
              (write-char #\+ p)
              (write-fixnum i p)]))
          (write-char x p))))
  (define write-list
    (lambda (x p m h i)
      (cond
       [(and (pair? x) 
             (or (not (get-hash-table h x #f))
                 (fxzero? (get-hash-table h x 0))))
        (write-char #\space p)
        (write-list (cdr x) p m h 
          (writer (car x) p m h i))]
       [(null? x) i]
       [else
        (write-char #\space p)
        (write-char #\. p)
        (write-char #\space p)
        (writer x p m h i)])))
  (define write-vector
    (lambda (x p m h i)
      (write-char #\# p)
      (write-char #\( p)
      (let ([n (vector-length x)])
        (let ([i 
               (cond
                 [(fx> n 0)
                  (let f ([idx 1] [i (writer (vector-ref x 0) p m h i)])
                    (cond
                      [(fx= idx n)
                       i]
                      [else
                       (write-char #\space p)
                       (f (fxadd1 idx)
                          (writer (vector-ref x idx) p m h i))]))]
                 [else i])])
           (write-char #\) p)
           i))))
  (define write-record
    (lambda (x p m h i)
      (write-char #\# p)
      (write-char #\[ p)
      (let ([i (writer (record-name x) p m h i)])
        (let ([n (record-length x)])
          (let f ([idx 0] [i i])
            (cond
              [(fx= idx n)
               (write-char #\] p)
               i]
              [else 
               (write-char #\space p)
               (f (fxadd1 idx) 
                  (writer (record-ref x idx) p m h i))]))))))
  (define initial?
    (lambda (c)
      (or (letter? c) (special-initial? c))))
  (define letter?
    (lambda (c)
      (or (and ($char<= #\a c) ($char<= c #\z))
          (and ($char<= #\A c) ($char<= c #\Z)))))
  (define digit?
    (lambda (c)
      (and ($char<= #\0 c) ($char<= c #\9))))
  (define special-initial? 
    (lambda (x)
      (memq x '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))))
  (define subsequent?
    (lambda (x)
      (or (initial? x)
          (digit? x)
          (special-subsequent? x))))
  (define special-subsequent?
    (lambda (x) 
      (memq x '(#\+ #\- #\. #\@))))
  (define subsequent*?
    (lambda (str i n)
      (or ($fx= i n)
          (and (subsequent? ($string-ref str i))
               (subsequent*? str ($fxadd1 i) n)))))
  (define valid-symbol-string?
    (lambda (str)
      (or (let ([n ($string-length str)])
            (and ($fx>= n 1)
                 (initial? ($string-ref str 0))
                 (subsequent*? str 1 n)))
          (string=? str "+")
          (string=? str "-")
          (string=? str "..."))))
  (define write-symbol-esc-loop
    (lambda (x i n p)
      (unless ($fx= i n)
        (let ([c ($string-ref x i)])
          (when (memq c '(#\\ #\|))
            (write-char #\\ p))
          (write-char c p))
        (write-symbol-esc-loop x ($fxadd1 i) n p))))
  (define write-symbol-esc
    (lambda (x p)
      (write-char #\| p)
      (write-symbol-esc-loop x 0 ($string-length x) p)
      (write-char #\| p)))
  (define write-symbol
    (lambda (x p m)
      (let ([str (symbol->string x)])
        (if m
            (if (valid-symbol-string? str)
                (write-char* str p)
                (write-symbol-esc str p))
            (write-char* str p)))))
  (define write-gensym
    (lambda (x p m h i)
      (cond
        [(and m (print-gensym))
         (let ([str (symbol->string x)])
           (write-char #\# p)
           (write-char #\{ p)
           (if (valid-symbol-string? str)
               (write-char* str p)
               (write-symbol-esc str p))
           (write-char #\space p)
           (write-symbol-esc (gensym->unique-string x) p)
           (write-char #\} p))
         i]
        [else 
         (write-symbol x p m)
         i])))
  (define write-string-escape
    (lambda (x p)
      (define loop 
        (lambda (x i n p)
         (unless (fx= i n)
           (let ([c (string-ref x i)])
             (cond
               [(or ($char= #\" c) ($char= #\\ c))
                (write-char #\\ p)
                (write-char c p)]
               [($char= #\newline c)
                (write-char #\\ p)
                (write-char #\n p)] 
               [($char= #\return c)
                (write-char #\\ p)
                (write-char #\r p)] 
               [($char= #\tab c)
                (write-char #\\ p)
                (write-char #\t p)]
               [else
                (write-char c p)]))
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
               ($fixnum->char
                  ($fx+ (fxremainder x 10)
                        ($char->fixnum #\0)))
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
  (define macro
    (lambda (x)
      (define macro-forms
        '([quote .            "'"]
          [quasiquote .       "`"]
          [unquote .          ","]
          [unquote-splicing . ",@"]
          [syntax .           "#'"]
          [|#primitive| .     "#%"]))
      (and (pair? x)
           (let ([d ($cdr x)])
             (and (pair? d)
                  (null? ($cdr d))))
           (assq ($car x) macro-forms))))
  (define write-pair
    (lambda (x p m h i)
      (write-char #\( p)
      (let ([i (writer (car x) p m h i)])
        (let ([i (write-list (cdr x) p m h i)])
          (write-char #\) p)
          i))))
  (define write-ref
    (lambda (n p)
      (write-char #\# p)
      (write-fixnum (fx- -1 n) p)
      (write-char #\# p)))
  (define write-mark
    (lambda (n p)
      (write-char #\# p)
      (write-fixnum (fx- -1 n) p)
      (write-char #\= p)))
  (define write-shareable
    (lambda (x p m h i k)
      (cond
        [(get-hash-table h x #f) =>
         (lambda (n)
           (cond
             [(fx< n 0) 
              (write-ref n p)
              i]
             [(fx= n 0)
              (k x p m h i)]
             [else
              (let ([i (fx- i 1)])
                (put-hash-table! h x i)
                (write-mark i p)
                (k x p m h i))]))]
        [else (k x p m h i)])))
  (define writer
    (lambda (x p m h i)
      (cond
        [(pair? x) 
         (write-shareable x p m h i write-pair)]
        [(symbol? x)
         (if (gensym? x)
             (write-gensym x p m h i)
             (begin (write-symbol x p m) i))]
        [(fixnum? x)
         (write-fixnum x p)
         i]
        [(string? x)
         (write-string x p m)
         i]
        [(boolean? x)
         (write-char* (if x "#t" "#f") p)
         i]
        [(char? x)
         (write-character x p m)
         i]
        [(procedure? x)
         (write-char* "#<procedure>" p)
         i]
        [(output-port? x)
         (write-char* "#<output-port " p)
         (let ([i (writer (output-port-name x) p #t h i)])
           (write-char #\> p)
           i)]
        [(input-port? x)
         (write-char* "#<input-port " p)
         (let ([i (writer (input-port-name x) p #t h i)])
           (write-char #\> p)
           i)]
        [(vector? x)
         (write-shareable x p m h i write-vector)]
        [(null? x) 
         (write-char #\( p)
         (write-char #\) p)
         i]
        [(eq? x (void)) 
         (write-char* "#<void>" p)
         i]
        [(eof-object? x)
         (write-char* "#!eof" p)
         i]
        [(bwp-object? x)
         (write-char* "#!bwp" p)
         i]
        [(hash-table? x)
         (write-char* "#<hash-table>" p)
         i]
        [(record? x)
         (let ([printer (record-printer x)])
           (if (procedure? printer)
               (begin (printer x p) i)
               (write-shareable x p m h i write-record)))]
        [(code? x)
         (write-char* "#<code>" p)]
        [($unbound-object? x)
         (write-char* "#<unbound-object>" p)
         i]
        [($forward-ptr? x)
         (write-char* "#<forward-ptr>" p)
         i]
        [(number? x)
         (write-char* (number->string x) p)
         i]
        [else 
         (write-char* "#<unknown>" p)
         i])))

  (define print-graph (make-parameter #f))

  (define (hasher x h)
    (define (vec-graph x i j h)
      (unless (fx= i j)
        (graph (vector-ref x i) h)
        (vec-graph x (fxadd1 i) j h)))
    (define (vec-dynamic x i j h)
      (unless (fx= i j)
        (dynamic (vector-ref x i) h)
        (vec-dynamic x (fxadd1 i) j h))) 
    (define (graph x h)
      (cond
        [(pair? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (graph (car x) h)
            (graph (cdr x) h)])]
        [(vector? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (vec-graph x 0 (vector-length x) h)])]
        [(gensym? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))])]))
    (define (dynamic x h)
      (cond
        [(pair? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (dynamic (car x) h)
            (dynamic (cdr x) h)
            (when (and (get-hash-table h x #f)
                       (fxzero? (get-hash-table h x #f)))
              (put-hash-table! h x #f))])]
        [(vector? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (vec-dynamic x 0 (vector-length x) h)
            (when (and (get-hash-table h x #f)
                       (fxzero? (get-hash-table h x #f)))
              (put-hash-table! h x #f))])])) 
    (if (print-graph) 
        (graph x h)
        (dynamic x h)))

  (define (write x p)
    (let ([h (make-hash-table)])
      (hasher x h)
      (writer x p #t h 0))
    (flush-output-port p))
  ;;;
  (define (display x p)
    (let ([h (make-hash-table)])
      (hasher x h)
      (writer x p #f h 0))
    (flush-output-port p))
  ;;;
  (define formatter
    (lambda (who p fmt args)
      (let f ([i 0] [args args])
        (unless (fx= i (string-length fmt))
          (let ([c (string-ref fmt i)])
            (cond
              [($char= c #\~)
               (let ([i (fxadd1 i)])
                 (when (fx= i (string-length fmt))
                   (error who "invalid ~~ at end of format string ~s" fmt))
                 (let ([c (string-ref fmt i)])
                  (cond
                    [($char= c #\~) 
                     (write-char #\~ p)
                     (f (fxadd1 i) args)]
                    [($char= c #\%) 
                     (write-char #\newline p)
                     (f (fxadd1 i) args)] 
                    [($char= c #\a)
                     (when (null? args)
                       (error who "insufficient arguments"))
                     (display (car args) p)
                     (f (fxadd1 i) (cdr args))]
                    [($char= c #\s)
                     (when (null? args)
                       (error who "insufficient arguments"))
                     (write (car args) p)
                     (f (fxadd1 i) (cdr args))]
                    [else
                     (error who "invalid sequence ~~~a" c)])))]
              [else 
               (write-char c p)
               (f (fxadd1 i) args)]))))
      (flush-output-port p)))

  (define fprintf
    (lambda (port fmt . args)
      (unless (output-port? port) 
        (error 'fprintf "~s is not an output port" port))
      (unless (string? fmt)
        (error 'fprintf "~s is not a string" fmt))
      (formatter 'fprintf port fmt args)))

  (define printf
    (lambda (fmt . args)
      (unless (string? fmt)
        (error 'printf "~s is not a string" fmt))
      (formatter 'printf (current-output-port) fmt args)))

  (define format
    (lambda (fmt . args)
      (unless (string? fmt)
        (error 'format "~s is not a string" fmt))
      (let ([p (open-output-string)])
        (formatter 'format p fmt args)
        (get-output-string p))))

  (define print-error
    (lambda (who fmt . args)
      (unless (string? fmt)
        (error 'print-error "~s is not a string" fmt))
      (let ([p (standard-error-port)])
        (if who
            (fprintf p "Error in ~a: " who)
            (fprintf p "Error: "))
        (formatter 'print-error p fmt args)
        (write-char #\. p)
        (newline p))))

  
  ;;;
  (primitive-set! 'format format)
  (primitive-set! 'printf printf)
  (primitive-set! 'fprintf fprintf)
  (primitive-set! 'print-graph print-graph)
  (primitive-set! 'write 
    (case-lambda
      [(x) (write x (current-output-port))]
      [(x p)
       (unless (output-port? p) 
         (error 'write "~s is not an output port" p))
       (write x p)]))
  (primitive-set! 'display 
    (case-lambda
      [(x) (display x (current-output-port))]
      [(x p)
       (unless (output-port? p) 
         (error 'display "~s is not an output port" p))
       (display x p)]))
  (primitive-set! 'print-error print-error)
  (primitive-set! 'error-handler
    (make-parameter
      (lambda args
        (apply print-error args)
        (flush-output-port (console-output-port))
        (exit -1))
      (lambda (x)
        (if (procedure? x)
            x
            (error 'error-handler "~s is not a procedure" x)))))
  (primitive-set! 'error
    (lambda args
      (apply (error-handler) args))))

