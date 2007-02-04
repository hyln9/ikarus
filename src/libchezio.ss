(let ()
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
  
  (let () ;;; GENERIC PORTS: BASIC PRIMITIVES
    ;;;
    ;;; Exports: 
    ;;; * Constructors:
    ;;;   (make-input-port handler input-buffer)
    ;;;   (make-output-port handler output-buffer)
    ;;;   (make-input/output-port handler input-buffer output-buffer)
    ;;;
    ;;; * Predicates:
    ;;;   (port? x)
    ;;;   (input-port? x)
    ;;;   (output-port? x)
    ;;;
    ;;; * Accessors:
    ;;;   (port-handler port)
    ;;;   (port-input-buffer port)
    ;;;   (port-input-index port)
    ;;;   (port-input-size port)
    ;;;   (port-output-buffer port)
    ;;;   (port-output-index port)
    ;;;   (port-output-size port)
    ;;;
    ;;; * Mutators:
    ;;;   (set-port-handler! port proc)
    ;;;   (set-port-input-buffer! port string)
    ;;;   (set-port-input-index! port fixnum)
    ;;;   (set-port-input-size! port fixnum)
    ;;;   (set-port-output-buffer! port string)
    ;;;   (set-port-output-index! port fixnum)
    ;;;   (set-port-output-size! port fixnum)
    ;;;
    (primitive-set! 'port?
      (lambda (x) (port? x)))
    ;;;
    (primitive-set! 'input-port?
      (lambda (x) (input-port? x)))
    ;;;
    (primitive-set! 'output-port?
      (lambda (x) (output-port? x)))
    ;;;
    (primitive-set! '$make-input-port
      (lambda (handler buffer)
        ($make-port/input handler buffer 0 ($string-length buffer) #f 0 0)))
    ;;;
    (primitive-set! 'make-input-port
      (lambda (handler buffer)
        (if (procedure? handler)
            (if (string? buffer)
                ($make-input-port handler buffer)
                (error 'make-input-port "~s is not a string" buffer))
            (error 'make-input-port "~s is not a procedure" handler))))
    ;;;
    (primitive-set! '$make-output-port
      (lambda (handler buffer)
        ($make-port/output handler #f 0 0 buffer 0 ($string-length buffer))))
    ;;;
    (primitive-set! 'make-output-port
      (lambda (handler buffer)
        (if (procedure? handler)
            (if (string? buffer)
                ($make-output-port handler buffer)
                (error 'make-output-port "~s is not a string" buffer))
            (error 'make-output-port "~s is not a procedure" handler))))
    ;;;
    (primitive-set! '$make-input/output-port
      (lambda (handler input-buffer output-buffer)
         ($make-port/both handler
                    input-buffer 0 ($string-length input-buffer)
                    output-buffer 0 ($string-length output-buffer))))
    (primitive-set! 'make-input/output-port
      (lambda (handler input-buffer output-buffer)
        (if (procedure? handler)
            (if (string? input-buffer)
                (if (string? output-buffer)
                    ($make-input/output-port handler input-buffer output-buffer) 
                    (error 'make-input/output-port 
                           "~s is not a string"
                           output-buffer))
                (error 'make-input/output-port "~s is not a string" input-buffer))
            (error 'make-input/output-port "~s is not a procedure" handler))))
    ;;;
    (primitive-set! '$port-handler
      (lambda (x) ($port-handler x)))
    ;;;
    (primitive-set! 'port-handler
      (lambda (x)
        (if (port? x)
            ($port-handler x)
            (error 'port-handler "~s is not a port" x))))
    ;;;
    (primitive-set! '$port-input-buffer
      (lambda (x) ($port-input-buffer x)))
    ;;;
    (primitive-set! 'port-input-buffer
      (lambda (x)
        (if (input-port? x)
            ($port-input-buffer x)
            (error 'port-input-buffer "~s is not an input-port" x))))
    ;;;
    (primitive-set! '$port-input-index
      (lambda (x) ($port-input-index x)))
    ;;;
    (primitive-set! 'port-input-index
      (lambda (x)
        (if (input-port? x)
            ($port-input-index x)
            (error 'port-input-index "~s is not an input-port" x))))
    ;;;
    (primitive-set! '$port-input-size
      (lambda (x) ($port-input-size x)))
    ;;;
    (primitive-set! 'port-input-size
      (lambda (x)
        (if (input-port? x)
            ($port-input-size x)
            (error 'port-input-size "~s is not an input-port" x))))
    ;;;
    (primitive-set! '$port-output-buffer
      (lambda (x) ($port-output-buffer x)))
    ;;;
    (primitive-set! 'port-output-buffer
      (lambda (x)
        (if (output-port? x)
            ($port-output-buffer x)
            (error 'port-output-buffer "~s is not an output-port" x))))
    ;;;
    (primitive-set! '$port-output-index
      (lambda (x) ($port-output-index x)))
    ;;;
    (primitive-set! 'port-output-index
      (lambda (x)
        (if (output-port? x)
            ($port-output-index x)
            (error 'port-output-index "~s is not an output-port" x))))
    ;;;
    (primitive-set! '$port-output-size
      (lambda (x) ($port-output-size x)))
    ;;;
    (primitive-set! 'port-output-size
      (lambda (x)
        (if (output-port? x)
            ($port-output-size x)
            (error 'port-output-size "~s is not an output-port" x))))
    ;;;
    (primitive-set! '$set-port-input-index!
      (lambda (p i) ($set-port-input-index! p i)))
    ;;;
    (primitive-set! 'set-port-input-index!
      (lambda (p i)
        (if (input-port? p)
            (if (fixnum? i)
                (if ($fx>= i 0)
                    (if ($fx<= i ($port-input-size p))
                        ($set-port-input-index! p i)
                        (error 'set-port-input-index! "index ~s is too big" i))
                    (error 'set-port-input-index! "index ~s is negative" i))
                (error 'set-port-input-index! "~s is not a valid index" i))
            (error 'set-port-input-index! "~s is not an input-port" p))))
    ;;;
    (primitive-set! '$set-port-input-size!
      (lambda (p i) 
        ($set-port-input-index! p 0)
        ($set-port-input-size! p i)))
    ;;;
    (primitive-set! 'set-port-input-size!
      (lambda (p i)
        (if (input-port? p)
            (if (fixnum? i)
                (if ($fx>= i 0)
                    (if ($fx<= i ($string-length ($port-input-buffer p)))
                        (begin
                          ($set-port-input-index! p 0)
                          ($set-port-input-size! p i))
                        (error 'set-port-input-size! "size ~s is too big" i))
                    (error 'set-port-input-size! "size ~s is negative" i))
                (error 'set-port-input-size! "~s is not a valid size" i))
            (error 'set-port-input-size! "~s is not an input-port" p))))
    ;;;
    (primitive-set! '$set-port-output-index!
      (lambda (p i) ($set-port-output-index! p i)))
    ;;;
    (primitive-set! 'set-port-output-index!
      (lambda (p i)
        (if (output-port? p)
            (if (fixnum? i)
                (if ($fx>= i 0)
                    (if ($fx<= i ($port-output-size p))
                        ($set-port-output-index! p i)
                        (error 'set-port-output-index! "index ~s is too big" i))
                    (error 'set-port-output-index! "index ~s is negative" i))
                (error 'set-port-output-index! "~s is not a valid index" i))
            (error 'set-port-output-index! "~s is not an output-port" p))))
    ;;;
    (primitive-set! '$set-port-output-size!
      (lambda (p i) 
        ($set-port-output-index! p 0)
        ($set-port-output-size! p i)))
    ;;;
    (primitive-set! 'set-port-output-size!
      (lambda (p i)
        (if (output-port? p)
            (if (fixnum? i)
                (if ($fx>= i 0)
                    (if ($fx<= i ($string-length ($port-output-buffer p)))
                        (begin
                          ($set-port-output-index! p 0)
                          ($set-port-output-size! p i))
                        (error 'set-port-output-size! "size ~s is too big" i))
                    (error 'set-port-output-size! "size ~s is negative" i))
                (error 'set-port-output-size! "~s is not a valid size" i))
            (error 'set-port-output-size! "~s is not an output-port" p)))))
  
  (let () ;;; IO PRIMITIVES
    ;;;
    (primitive-set! '$write-char
      (lambda (c p)
        (let ([idx (port-output-index p)])
          (if ($fx< idx ($port-output-size p))
              (begin
                ($string-set! ($port-output-buffer p) idx c)
                ($set-port-output-index! p ($fxadd1 idx)))
              (($port-handler p) 'write-char c p)))))
    ;;;
    (primitive-set! 'write-char
      (case-lambda
        [(c)
         (if (char? c)
             ($write-char c (current-output-port))
             (error 'write-char "~s is not a character" c))]
        [(c p)
         (if (char? c)
             (if (output-port? p)
                 ($write-char c p)
                 (error 'write-char "~s is not an output-port" p))
             (error 'write-char "~s is not a character" c))]))
    ;;;
    (primitive-set! 'newline
      (case-lambda
        [() 
         ($write-char #\newline (current-output-port))
         ($flush-output-port (current-output-port))]
        [(p) 
         (if (output-port? p)
             (begin
               ($write-char #\newline p)
               ($flush-output-port p))
             (error 'newline "~s is not an output port" p))]))
    ;;;
    (primitive-set! 'port-name
      (lambda (p)
        (if (port? p) 
            (($port-handler p) 'port-name p)
            (error 'port-name "~s is not a port" p))))
    (primitive-set! 'input-port-name port-name)
    (primitive-set! 'output-port-name port-name)
    (primitive-set! '$read-char
      (lambda (p)
        (let ([idx ($port-input-index p)])
          (if ($fx< idx ($port-input-size p))
              (begin
                ($set-port-input-index! p ($fxadd1 idx))
                ($string-ref ($port-input-buffer p) idx))
              (begin
                (($port-handler p) 'read-char p))))))
    ;;;
    (primitive-set! 'read-char
      (case-lambda
        [() ($read-char *current-input-port*)]
        [(p)
         (if (input-port? p)
             ($read-char p)
             (error 'read-char "~s is not an input-port" p))]))
    ;;;
    (primitive-set! '$unread-char
      (lambda (c p)
        (let ([idx ($fxsub1 ($port-input-index p))])
          (if (and ($fx>= idx 0)
                   ($fx< idx ($port-input-size p)))
              (begin
                ($set-port-input-index! p idx)
                ($string-set! ($port-input-buffer p) idx c))
              (($port-handler p) 'unread-char c p)))))
    ;;;
    (primitive-set! 'unread-char
      (case-lambda
        [(c) (if (char? c)
                 ($unread-char c (current-input-port))
                 (error 'unread-char "~s is not a character" c))]
        [(c p)
         (if (input-port? p)
             (if (char? c)
                 ($unread-char c p)
                 (error 'unread-char "~s is not a character" c))
             (error 'unread-char "~s is not an input-port" p))]))
    ;;;
    (primitive-set! '$peek-char
      (lambda (p)
        (let ([idx ($port-input-index p)])
          (if ($fx< idx ($port-input-size p))
              ($string-ref ($port-input-buffer p) idx)
              (($port-handler p) 'peek-char p)))))
    ;;;
    (primitive-set! 'peek-char
      (case-lambda
        [() ($peek-char (current-input-port))]
        [(p)
         (if (input-port? p)
             ($peek-char p)
             (error 'peek-char "~s is not an input-port" p))]))
    ;;;
    (primitive-set! '$unread-char
      (lambda (c p)
        (let ([idx ($fxsub1 ($port-input-index p))])
          (if (and ($fx>= idx 0)
                   ($fx< idx ($port-input-size p)))
              (begin
                ($set-port-input-index! p idx)
                ($string-set! ($port-input-buffer p) idx c))
              (($port-handler p) 'unread-char c p)))))
    ;;;
    (primitive-set! '$reset-input-port!
      (lambda (p)
        ($set-port-input-size! p 0)))
    ;;;
    (primitive-set! 'reset-input-port!
      (case-lambda
        [() ($reset-input-port! (current-input-port))]
        [(p)
         (if (input-port? p)
             ($reset-input-port! p)
             (error 'reset-input-port! "~s is not an input-port" p))]))
    ;;;
    (primitive-set! '$close-input-port
      (lambda (p)
        (($port-handler p) 'close-port p)))
    ;;;
    (primitive-set! 'close-input-port
      (case-lambda
        [() ($close-input-port (current-input-port))]
        [(p)
         (if (input-port? p)
             ($close-input-port p)
             (error 'close-input-port! "~s is not an input-port" p))]))
    ;;;
    (primitive-set! '$close-output-port
      (lambda (p)
        (($port-handler p) 'close-port p)))
    ;;;
    (primitive-set! 'close-output-port
      (case-lambda
        [() ($close-output-port (current-output-port))]
        [(p)
         (if (output-port? p)
             ($close-output-port p)
             (error 'close-output-port "~s is not an output-port" p))]))
    ;;;
    (primitive-set! '$flush-output-port
      (lambda (p)
        (($port-handler p) 'flush-output-port p)))
    ;;;
    (primitive-set! 'flush-output-port
      (case-lambda
        [() ($flush-output-port (current-output-port))]
        [(p)
         (if (output-port? p)
             ($flush-output-port p)
             (error 'flush-output-port "~s is not an output-port" p))])))
  
  (let () ;;; INPUT FILES
    (define guardian (make-guardian))
    (define close-ports
      (lambda ()
        (cond
          [(guardian) =>
           (lambda (p)
             (close-input-port p)
             (close-ports))])))
    ;;;
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
                       ($string-ref ($port-input-buffer p) idx))
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
                     ($string-ref ($port-input-buffer p) idx)
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
                       ($string-set! ($port-input-buffer p) idx c))
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
    (define open-input-file
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
    (define open-input-string
      (lambda (str)
        (unless (string? str)
          (error 'open-input-string "~s is not a string" str))
        (let ([port (make-input-port
                      (make-input-string-handler str)
                      str)])
          port)))
    (primitive-set! 'open-input-string open-input-string)
    (primitive-set! '*standard-input-port* 
      (let ([p (make-input-port 
                 (make-input-file-handler 0 '*stdin*)
                 (make-string 4096))])
        (set-port-input-size! p 0)
        p))
    (primitive-set! 'console-input-port (lambda () *standard-input-port*))
    (primitive-set! '*current-input-port* *standard-input-port*)
    (primitive-set! 'current-input-port 
      (case-lambda
        [() *current-input-port*]
        [(p) 
         (if (input-port? p)
             (primitive-set! '*current-input-port* p)
             (error 'current-input-port "~s is not an input-port" p))]))
    (primitive-set! 'open-input-file
      (lambda (filename)
         (if (string? filename)
             (open-input-file filename)
             (error 'open-input-file "~s is not a string" filename)))))
  
  (let () ;;; OUTPUT FILES
    (define guardian (make-guardian))
    (define close-ports
      (lambda ()
        (cond
          [(guardian) =>
           (lambda (p)
             (close-output-port p)
             (close-ports))])))
    ;;;
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
    (define open-output-file
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
    (primitive-set! '*standard-output-port* 
      (make-output-port
        (make-output-file-handler 1 '*stdout*)
        (make-string 4096)))
    (primitive-set! '*current-output-port* *standard-output-port*)
    (primitive-set! '*standard-error-port* 
      (make-output-port
        (make-output-file-handler 2 '*stderr*)
        (make-string 4096)))
    (primitive-set! 'standard-output-port 
       (lambda () *standard-output-port*))
    (primitive-set! 'standard-error-port
       (lambda () *standard-error-port*))
    (primitive-set! 'console-output-port 
       (lambda () *standard-output-port*))
    (primitive-set! 'current-output-port 
       (case-lambda
         [() *current-output-port*]
         [(p)
          (if (output-port? p)
              (primitive-set! '*current-output-port* p)
              (error 'current-output-port "~s is not an output port" p))]))
    (primitive-set! 'open-output-file 
      (case-lambda
        [(filename)
         (if (string? filename)
             (open-output-file filename 'error)
             (error 'open-output-file "~s is not a string" filename))]
        [(filename options)
         (if (string? filename)
             (open-output-file filename options)
             (error 'open-output-file "~s is not a string" filename))])))
  
  (let () ;;; OUTPUT STRINGS
    ;;;
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
    (primitive-set! 'open-output-string 
      (lambda ()
        (make-output-port 
          (make-output-string-handler)
          (make-string 10))))
    (primitive-set! 'get-output-string
      (lambda (p)
        (if (output-port? p)
            (($port-handler p) 'get-output-string p)
            (error 'get-output-string "~s is not an output port" p))))
  )
  
  (primitive-set! 'with-output-to-string
    (lambda (f)
      (unless (procedure? f)
        (error 'with-output-to-string "~s is not a procedure" f))
      (let ([p (open-output-string)])
        (parameterize ([current-output-port p]) (f))
        (get-output-string p))))

  (primitive-set! 'with-output-to-file
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
  
  (primitive-set! 'call-with-output-file
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
  
  (primitive-set! 'with-input-from-file
     (lambda (name proc)
       (unless (string? name) 
         (error 'with-input-from-file "~s is not a string" name))
       (unless (procedure? proc)
         (error 'with-input-from-file "~s is not a procedure" proc))
       (let ([p (open-input-file name)])
         (call-with-values 
           (lambda () 
             (parameterize ([current-input-port p])
               (proc)))
           (case-lambda
             [(v) (close-input-port p) v]
             [v*
              (close-input-port p)
              (apply values v*)])))))
    
  (primitive-set! 'call-with-input-file
     (lambda (name proc)
       (unless (string? name) 
         (error 'call-with-input-file "~s is not a string" name))
       (unless (procedure? proc)
         (error 'call-with-input-file "~s is not a procedure" proc))
       (let ([p (open-input-file name)])
         (call-with-values (lambda () (proc p))
            (case-lambda
              [(v) (close-input-port p) v]
              [v*
               (close-input-port p)
               (apply values v*)])))))
)
