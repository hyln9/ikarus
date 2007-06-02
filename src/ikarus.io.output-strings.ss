
(library (ikarus io output-strings)
  (export open-output-string get-output-string with-output-to-string)
  (import 
    (ikarus system $strings)
    (ikarus system $bytevectors)
    (ikarus system $chars)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $ports)
    (ikarus system $io)
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

  (define concat-old
    (lambda (str i ls)
      (let ([n (sum i ls)])
        (let ([outstr (make-string n)])
          (let f ([n (copy outstr str i n)] [ls ls])
            (if (null? ls)
                outstr
                (let ([a ($car ls)])
                  (f (copy outstr a (string-length a) n) ($cdr ls)))))))))

  (define concat 
    (lambda (bv i ls)
      (let ([n (sum i ls)])
        (let ([outbv ($make-bytevector n)])
          (let f ([n (copy outbv bv i n)] [ls ls])
            (if (null? ls)
                outbv
                (let ([a ($car ls)])
                  (f (copy outbv a ($bytevector-length a) n) ($cdr ls)))))))))
  (define sum 
    (lambda (ac ls)
      (cond
        [(null? ls) ac]
        [else (sum ($fx+ ac ($bytevector-length ($car ls))) ($cdr ls))])))
  
  (define sum-old
    (lambda (ac ls)
      (cond
        [(null? ls) ac]
        [else (sum ($fx+ ac (string-length ($car ls))) ($cdr ls))])))

  (define copy-old
    (lambda (dst src n end)
      (let f ([di end]
              [si n])
        (cond
          [($fx= si 0) di]
          [else
           (let ([di ($fxsub1 di)] [si ($fxsub1 si)])
             (string-set! dst di (string-ref src si))
             (f di si))]))))

  (define copy
    (lambda (dst src n end)
      (let f ([di end]
              [si n])
        (cond
          [($fx= si 0) di]
          [else
           (let ([di ($fxsub1 di)] [si ($fxsub1 si)])
             ($bytevector-set! dst di ($bytevector-u8-ref src si))
             (f di si))]))))

  (define bv-copy
    (lambda (src)
      (let ([n ($bytevector-length src)])
        (let f ([src src] [dst ($make-bytevector n)] [i 0] [n n])
          (cond
            [($fx= i n) dst]
            [else
             ($bytevector-set! dst i ($bytevector-u8-ref src i))
             (f src dst ($fxadd1 i) n)])))))



  (define make-output-string-handler
    (lambda ()
      (define buffer-list '())
      (define open? #t)
      (define output-handler
        (lambda (msg . args)
          (message-case msg args
            [(write-byte b p)
             (if (and (fixnum? b) ($fx<= 0 b) ($fx<= b 255))
                 (if (output-port? p)
                     (let ([idx ($port-output-index p)])
                       (if ($fx< idx ($port-output-size p))
                           (begin
                             ($bytevector-set! ($port-output-buffer p) idx b)
                             ($set-port-output-index! p ($fxadd1 idx)))
                           (if open?
                             (let ([buff ($port-output-buffer p)])
                               (set! buffer-list (cons (bv-copy buff) buffer-list))
                               ($bytevector-set! buff 0 b)
                               ($set-port-output-index! p 1))
                             (error 'write-byte "port ~s is closed" p))))
                     (error 'write-byte "~s is not an output-port" p))
                 (error 'write-byte "~s is not a byte" b))]
            [(write-char c p)
             (if (char? c)
                 (if (output-port? p)
                     (let ([b ($char->fixnum c)])
                       (if ($fx<= b 127)
                           ($write-byte b p)
                           (error 'write-char "multibyte write of ~s is not implemented" c)))
                     (error 'write-char "~s is not an output-port" p))
                 (error 'write-char "~s is not a character" c))]
            [(flush-output-port p)
             (void)]
            [(close-port p)
             (set! open? #f)]
            [(port-name p) 'string-port]
            [(get-output-string p) 
             (utf8-bytevector->string
               (concat 
                 ($port-output-buffer p) 
                 ($port-output-index p)
                 buffer-list))]
            [else (error 'output-handler 
                         "unhandled message ~s" (cons msg args))])))
      output-handler))

  (define open-output-string 
    (lambda ()
      (make-output-port 
        (make-output-string-handler)
        ($make-bytevector 59))))

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
