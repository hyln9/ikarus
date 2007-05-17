
(library (ikarus io input-files)
  (export open-input-file current-input-port console-input-port
          with-input-from-file call-with-input-file)
  (import
    (ikarus system $ports)
    (ikarus system $io)
    (ikarus system $fx)
    (ikarus system $strings)
    (ikarus system $chars)
    (except (ikarus)
            open-input-file current-input-port console-input-port
            with-input-from-file call-with-input-file
            *standard-input-port* *current-input-port*))

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

  (define guardian (make-guardian))
  
  (define close-ports
    (lambda ()
      (cond
        [(guardian) =>
         (lambda (p)
           (close-input-port p)
           (close-ports))])))
 
  (define make-input-file-handler
    (lambda (fd port-name)
      (let ((open? #t))
        (lambda (msg . args)
          (message-case msg args
            [(read-char p)
             (unless (input-port? p)
               (error 'read-char "~s is not an input port" p))
             (let ([idx ($port-input-index p)])
               (if ($fx< idx ($port-input-size p))
                   (begin
                     ($set-port-input-index! p ($fxadd1 idx))
                     (string-ref ($port-input-buffer p) idx))
                   (if open?
                       (let ([bytes
                              (foreign-call "ikrt_read" 
                                 fd ($port-input-buffer p))])
                         ;($do-event)
                         (cond
                           [($fx> bytes 0)
                            ($set-port-input-size! p bytes)
                            ($read-char p)]
                           [($fx= bytes 0)
                            (eof-object)]
                           [else
                            (error 'read-char "Cannot read from ~a"
                                   port-name)]))
                       (error 'read-char "port ~s is closed" p))))]
            [(peek-char p)
             (unless (input-port? p)
               (error 'peek-char "~s is not an input port" p))
             (let ([idx ($port-input-index p)])
               (if ($fx< idx ($port-input-size p))
                   (string-ref ($port-input-buffer p) idx)
                   (if open?
                       (let ([bytes
                              (foreign-call "ikrt_read" fd
                                            (port-input-buffer p))])
                         (cond
                           [(not bytes)
                            (error 'peek-char
                                   "Cannot read from ~s" port-name)]
                           [($fx= bytes 0)
                            (eof-object)]
                           [else
                            ($set-port-input-size! p bytes)
                            ($peek-char p)]))
                       (error 'peek-char "port ~s is closed" p))))]
            [(unread-char c p)
             (unless (input-port? p)
               (error 'unread-char "~s is not an input port" p))
             (let ([idx ($fxsub1 ($port-input-index p))])
               (if (and ($fx>= idx 0)
                        ($fx< idx ($port-input-size p)))
                   (begin
                     ($set-port-input-index! p idx)
                     (string-set! ($port-input-buffer p) idx c))
                   (if open?
                       (error 'unread-char "port ~s is closed" p)
                       (error 'unread-char "too many unread-chars"))))]
            [(port-name p) port-name]
            [(close-port p)
             (unless (input-port? p)
               (error 'close-input-port "~s is not an input port" p))
             (when open?
               ($set-port-input-size! p 0)
               (set! open? #f)
               (unless (foreign-call "ikrt_close_file" fd)
                  (error 'close-input-port "cannot close ~s" port-name)))]
            [else 
             (error 'input-file-handler
                    "message not handled ~s" (cons msg args))])))))

  (define $open-input-file
    (lambda (filename)
      (close-ports)
      (let ([fd/error (foreign-call "ikrt_open_input_file" filename)])
        (if (fixnum? fd/error)
            (let ([port (make-input-port 
                          (make-input-file-handler fd/error filename)
                          (make-string 4096))])
              (set-port-input-size! port 0)
              (guardian port)
              port)
            (error 'open-input-file "cannot open ~s: ~a" filename fd/error)))))

  (define open-input-file
    (lambda (filename)
       (if (string? filename)
           ($open-input-file filename)
           (error 'open-input-file "~s is not a string" filename))))

  (define with-input-from-file
     (lambda (name proc)
       (unless (string? name) 
         (error 'with-input-from-file "~s is not a string" name))
       (unless (procedure? proc)
         (error 'with-input-from-file "~s is not a procedure" proc))
       (let ([p ($open-input-file name)])
         (call-with-values 
           (lambda () 
             (parameterize ([current-input-port p])
               (proc)))
           (case-lambda
             [(v) (close-input-port p) v]
             [v*
              (close-input-port p)
              (apply values v*)])))))
    
  (define call-with-input-file
     (lambda (name proc)
       (unless (string? name) 
         (error 'call-with-input-file "~s is not a string" name))
       (unless (procedure? proc)
         (error 'call-with-input-file "~s is not a procedure" proc))
       (let ([p ($open-input-file name)])
         (call-with-values (lambda () (proc p))
            (case-lambda
              [(v) (close-input-port p) v]
              [v*
               (close-input-port p)
               (apply values v*)])))))

  (define *standard-input-port* #f)
  (define *current-input-port* #f)

  (define console-input-port 
    (lambda () *standard-input-port*))

  (define current-input-port 
    (case-lambda
      [() *current-input-port*]
      [(p) 
       (if (input-port? p)
           (set! *current-input-port* p)
           (error 'current-input-port "~s is not an input-port" p))]))

  (set! *standard-input-port* 
    (let ([p (make-input-port 
               (make-input-file-handler 0 '*stdin*)
               (make-string 4096))])
      (set-port-input-size! p 0)
      p))
  (set! *current-input-port* *standard-input-port*)
  )
