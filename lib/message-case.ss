

(define-syntax message-case
  (syntax-rules  (else)
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

