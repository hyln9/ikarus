
(library (ikarus io input-strings)
  (export open-input-string with-input-from-string)
  (import 
    (ikarus system $strings)
    (ikarus system $bytevectors)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $ports)
    (ikarus system $io)
    (except (ikarus) open-input-string with-input-from-string))
 
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

  (define make-input-string-handler
    (lambda (str)
      (let ((open? #t) (idx 0) (n (string-length str)))
        (lambda (msg . args)
          (message-case msg args
            [(read-char p)
             (if ($fx< idx n)
                 (let ([c ($string-ref str idx)])
                   (set! idx ($fxadd1 idx))
                   c)
                 (if open?
                     (eof-object)
                     (error 'read-char "port ~s is closed" p)))]
            [(peek-char p)
             (if ($fx< idx n)
                 ($string-ref str idx)
                 (if open?
                     (eof-object)
                     (error 'peek-char "port ~s is closed" p)))]
            [(unread-char c p)
             (let ([i ($fxsub1 idx)])
               (if (and ($fx>= i 0)
                        ($fx< i n))
                   (set! idx i)
                   (if open?
                       (error 'unread-char "port ~s is closed" p)
                       (error 'unread-char "too many unread-chars"))))]
            [(port-name p) '*string-port*]
            [(close-port p)
             (when open?
               (set! open? #f))]
            [else 
             (error 'input-string-handler
                    "message not handled ~s" (cons msg args))]))))) 

  (define open-input-string
    (lambda (str)
      (unless (string? str)
        (error 'open-input-string "~s is not a string" str))
      (let ([port (make-input-port
                    (make-input-string-handler str)
                    '#vu8())])
        port)))


  (define with-input-from-string
     (lambda (str proc)
       (unless (string? str) 
         (error 'with-input-from-string "~s is not a string" str))
       (unless (procedure? proc)
         (error 'with-input-from-string "~s is not a procedure" proc))
       (let ([p (open-input-string str)])
         (parameterize ([current-input-port p])
           (proc)))))

  )

