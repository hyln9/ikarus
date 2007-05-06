
(library (ikarus io output-strings)
  (export open-output-string get-output-string with-output-to-string)
  (import 
    (only (scheme) $string-length $string-ref $string-set! $car $cdr
          $fx+ $fxadd1 $fxsub1 $fx= $fx<
          $port-output-buffer $port-output-index $port-output-size
          $port-handler
          $set-port-output-size! $write-char $set-port-output-index!)
    (except (ikarus) open-output-string get-output-string with-output-to-string))

  (define-syntax message-case
    (syntax-rules (else)
      [(_ msg args 
          [(msg-name msg-arg* ...) b b* ...] ... 
          [else else1 else2 ...])
       (let ([tmsg msg] [targs args])
         (define-syntax match-and-bind
           (syntax-rules ()
             [(__ y () body)
              (if (null? y) 
                  body
                  (error 'message-case "unmatched ~s" (cons tmsg targs)))]
             [(__ y (a a* (... ...)) body)
              (if (pair? y)
                  (let ([a (car y)] [d (cdr y)])
                    (match-and-bind d (a* (... ...)) body))
                  (error 'message-case "unmatched ~s" (cons tmsg targs)))]))
         (case tmsg
           [(msg-name) 
            (match-and-bind targs (msg-arg* ...) (begin b b* ...))] ...
           [else else1 else2 ...]))]))
  
  (define string-copy 
    (lambda (s)
      (substring s 0 (string-length s))))

  (define concat 
    (lambda (str i ls)
      (let ([n (sum i ls)])
        (let ([outstr ($make-string n)])
          (let f ([n (copy outstr str i n)] [ls ls])
            (if (null? ls)
                outstr
                (let ([a ($car ls)])
                  (f (copy outstr a ($string-length a) n) ($cdr ls)))))))))

  (define sum 
    (lambda (ac ls)
      (cond
        [(null? ls) ac]
        [else (sum ($fx+ ac ($string-length ($car ls))) ($cdr ls))])))
  
  (define copy
    (lambda (dst src n end)
      (let f ([di end]
              [si n])
        (cond
          [($fx= si 0) di]
          [else
           (let ([di ($fxsub1 di)] [si ($fxsub1 si)])
             ($string-set! dst di ($string-ref src si))
             (f di si))]))))
  
  (define make-output-string-handler
    (lambda ()
      (define buffer-list '())
      (define open? #t)
      (define output-handler
        (lambda (msg . args)
          (message-case msg args
            [(write-char c p)
             (if (char? c)
                 (if (output-port? p)
                     (let ([idx ($port-output-index p)])
                        (if ($fx< idx ($port-output-size p))
                            (begin
                              ($string-set! ($port-output-buffer p) idx c)
                              ($set-port-output-index! p ($fxadd1 idx)))
                            (if open?
                              (begin
                                (set! buffer-list
                                  (cons (string-copy (port-output-buffer p))
                                        buffer-list))
                                ($set-port-output-size! p 
                                  ($string-length ($port-output-buffer p)))
                                ($write-char c p))
                              (error 'write-char "port ~s is closed" p))))
                     (error 'write-char "~s is not an output-port" p))
                 (error 'write-char "~s is not a character" c))]
            [(flush-output-port p)
             (void)]
            [(close-port p)
             (set! open? #f)]
            [(port-name p) 'string-port]
            [(get-output-string p) 
             (concat ($port-output-buffer p)
                     ($port-output-index p)
                     buffer-list)]
            [else (error 'output-handler 
                         "unhandled message ~s" (cons msg args))])))
      output-handler))

  (define open-output-string 
    (lambda ()
      (make-output-port 
        (make-output-string-handler)
        (make-string 10))))

  (define get-output-string
    (lambda (p)
      (if (output-port? p)
          (($port-handler p) 'get-output-string p)
          (error 'get-output-string "~s is not an output port" p))))
  
  (define with-output-to-string
    (lambda (f)
      (unless (procedure? f)
        (error 'with-output-to-string "~s is not a procedure" f))
      (let ([p (open-output-string)])
        (parameterize ([current-output-port p]) (f))
        (get-output-string p))))
  

)