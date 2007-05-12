
(library (ikarus io input-strings)
  (export open-input-string)
  (import 
    (ikarus system $strings)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $ports)
    (ikarus system $io)
    (except (ikarus) open-input-string))
 
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
      (let ((open? #t))
        (lambda (msg . args)
          (message-case msg args
            [(read-char p)
             (let ([idx ($port-input-index p)])
               (if ($fx< idx ($port-input-size p))
                   (begin
                     ($set-port-input-index! p ($fxadd1 idx))
                     ($string-ref ($port-input-buffer p) idx))
                   (if open?
                       (eof-object)
                       (error 'read-char "port ~s is closed" p))))]
            [(peek-char p)
             (unless (input-port? p)
               (error 'peek-char "~s is not an input port" p))
             (let ([idx ($port-input-index p)])
               (if ($fx< idx ($port-input-size p))
                   ($string-ref ($port-input-buffer p) idx)
                   (if open?
                       (eof-object)
                       (error 'peek-char "port ~s is closed" p))))]
            [(unread-char c p)
             (unless (input-port? p)
               (error 'unread-char "~s is not an input port" p))
             (let ([idx ($fxsub1 ($port-input-index p))])
               (if (and ($fx>= idx 0)
                        ($fx< idx ($port-input-size p)))
                   (begin
                     ($set-port-input-index! p idx)
                     ($string-set! ($port-input-buffer p) idx c))
                   (if open?
                       (error 'unread-char "port ~s is closed" p)
                       (error 'unread-char "too many unread-chars"))))]
            [(port-name p) '*string-port*]
            [(close-port p)
             (unless (input-port? p)
               (error 'close-input-port "~s is not an input port" p))
             (when open?
               ($set-port-input-size! p 0)
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
                    str)])
        port)))
  )

