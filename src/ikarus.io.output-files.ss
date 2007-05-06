
(library (ikarus io output-files)
  (export standard-output-port standard-error-port
          console-output-port current-output-port
          open-output-file with-output-to-file call-with-output-file)
  (import 
    (only (scheme) $set-port-output-size! $write-char $string-set!
          $port-output-buffer $set-port-output-index! $fxadd1 $fx<
          $port-output-size $port-output-index)
    (except (ikarus)
            standard-output-port standard-error-port   
            console-output-port current-output-port    
            *standard-output-port* *standard-error-port*
            *current-output-port*
            open-output-file with-output-to-file
            call-with-output-file))

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
           (close-output-port p)
           (close-ports))])))

  (define do-write-buffer
    (lambda (fd port-name p caller)
      (let ([bytes (foreign-call "ikrt_write_file" 
                                 fd
                                 (port-output-buffer p)
                                 (port-output-index p))])
        (if (fixnum? bytes)
            (set-port-output-index! p 0)
            (error caller "cannot write to file ~s: ~a" port-name bytes)))))
  
  (define make-output-file-handler
    (lambda (fd port-name)
      (define open? #t)
      (define output-file-handler
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
                                (do-write-buffer fd port-name p 'write-char)
                                ($write-char c p))
                              (error 'write-char "port ~s is closed" p))))
                     (error 'write-char "~s is not an output-port" p))
                 (error 'write-char "~s is not a character" c))]
            [(flush-output-port p)
             (if (output-port? p)
                 (if open?
                     (do-write-buffer fd port-name p 'flush-output-port)
                     (error 'flush-output-port "port ~s is closed" p))
                 (error 'flush-output-port "~s is not an output-port" p))]
            [(close-port p)
             (when open?
               (flush-output-port p)
               ($set-port-output-size! p 0)
               (set! open? #f)
               (unless (foreign-call "ikrt_close_file" fd)
                 (error 'close-output-port "cannot close ~s" port-name)))]
            [(port-name p) port-name]
            [else (error 'output-file-handler 
                         "unhandled message ~s" (cons msg args))])))
      output-file-handler))
  
  (define (option-id x)
    (case x
      [(error)    0]
      [(replace)  1]
      [(truncate) 2]
      [(append)   3]
      [else (error 'open-output-file "~s is not a valid mode" x)]))

  (define $open-output-file
    (lambda (filename options)
      (close-ports)
      (let ([fd/error 
             (foreign-call "ikrt_open_output_file" 
                           filename 
                           (option-id options))])
        (if (fixnum? fd/error)
            (let ([port
                   (make-output-port
                     (make-output-file-handler fd/error filename)
                     (make-string 4096))])
              (guardian port)
              port)
            (error 'open-output-file "cannot open ~s: ~a" filename fd/error)))))

  (define *standard-output-port* #f)
  
  (define *standard-error-port* #f)
  
  (define *current-output-port* #f)

  (define standard-output-port 
     (lambda () *standard-output-port*))
  
  (define standard-error-port
     (lambda () *standard-error-port*))
  
  (define console-output-port 
     (lambda () *standard-output-port*))
  
  (define current-output-port 
     (case-lambda
       [() *current-output-port*]
       [(p)
        (if (output-port? p)
            (set! *current-output-port* p)
            (error 'current-output-port "~s is not an output port" p))]))

  (define open-output-file 
    (case-lambda
      [(filename)
       (if (string? filename)
           ($open-output-file filename 'error)
           (error 'open-output-file "~s is not a string" filename))]
      [(filename options)
       (if (string? filename)
           ($open-output-file filename options)
           (error 'open-output-file "~s is not a string" filename))]))

  (define with-output-to-file
     (lambda (name proc . args)
       (unless (string? name) 
         (error 'with-output-to-file "~s is not a string" name))
       (unless (procedure? proc)
         (error 'with-output-to-file "~s is not a procedure" proc))
       (let ([p (apply open-output-file name args)]
             [shot #f])
         (call-with-values 
           (lambda () 
             (parameterize ([current-output-port p])
               (proc)))
           (case-lambda
             [(v) (close-output-port p) v]
             [v*
              (close-output-port p)
              (apply values v*)])))))

  (define call-with-output-file
     (lambda (name proc . args)
       (unless (string? name) 
         (error 'call-with-output-file "~s is not a string" name))
       (unless (procedure? proc)
         (error 'call-with-output-file "~s is not a procedure" proc))
       (let ([p (apply open-output-file name args)])
         (call-with-values (lambda () (proc p))
            (case-lambda
              [(v) (close-output-port p) v]
              [v*
               (close-output-port p)
               (apply values v*)])))))

  (set! *standard-output-port* 
    (make-output-port
      (make-output-file-handler 1 '*stdout*)
      (make-string 4096)))
  (set! *current-output-port* *standard-output-port*)
  (set! *standard-error-port* 
    (make-output-port
      (make-output-file-handler 2 '*stderr*)
      (make-string 4096))) )