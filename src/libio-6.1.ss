
;;; OUTPUT PORTS
  
(let ()
   ;;; only file-based ports are supported at this point
   ;;;
   ;;; an output port is a vector with the following fields:
   ;;; 0. id
   ;;; 1. file-name
   ;;; 2. file-descriptor
   ;;; 3. open?
   ;;; 4. buffer
   ;;; 5. buffer-size
   ;;; 6. index
   ;;; 7. flush-proc
   ;;; 8. close-proc
   (define output-port-id (gensym "output-port"))
   (define output-port?
     (lambda (x)
       (and (vector? x) 
            (fx= (vector-length x) 9)
            (eq? (vector-ref x 0) output-port-id))))
   (define output-port-name
     (lambda (p) (vector-ref p 1)))
   (define output-port-fd
     (lambda (p) (vector-ref p 2)))
   (define set-output-port-fd!
     (lambda (p x) (vector-set! p 2 x)))
   (define output-port-open?
     (lambda (p) (vector-ref p 3)))
   (define set-output-port-open?!
     (lambda (p b) (vector-set! p 3 b)))
   (define output-port-buffer
     (lambda (p) (vector-ref p 4)))
   (define set-output-port-buffer!
     (lambda (p b) (vector-set! p 4 b)))
   (define output-port-size
     (lambda (p) (vector-ref p 5)))
   (define output-port-index
     (lambda (p) (vector-ref p 6)))
   (define output-port-flush-proc
     (lambda (p) (vector-ref p 7)))
   (define output-port-close-proc
     (lambda (p) (vector-ref p 8)))
   (define set-output-port-index!
     (lambda (p i) (vector-set! p 6 i)))
   (define fd->port
     (lambda (fd filename)
       (vector output-port-id ; id
               filename
               fd
               #t
               (make-string 4096)
               4096
               0
               fd-flush-proc
               fd-close-proc)))
   (define open-output-string
     (lambda ()
       (vector output-port-id
               '*string-port*
               '()
               #t
               (make-string 4096)
               4096
               0
               str-flush-proc
               (lambda (port) (void)))))
   (define get-output-string
     (lambda (p)
       (define fill
         (lambda (dst src di si sj)
           (cond
             [(fx= si sj) dst]
             [else
              (string-set! dst di (string-ref src si))
              (fill dst src (fxadd1 di) (fxadd1 si) sj)])))
       (unless (output-port? p) 
         (error 'get-output-string "~s is not an output port" p))
       (let ([ls (output-port-fd p)])
         (unless (list? ls)
           (error 'get-output-string "~s is not an output port" p))
         (let f ([ls (reverse ls)] [n 0])
           (cond
             [(null? ls) 
              (let ([idx (output-port-index p)]
                    [buf (output-port-buffer p)])
                (let ([str (make-string (fx+ n idx))])
                  (fill str buf n 0 idx)))]
             [else
              (let ([buf (car ls)])
                (let ([idx (string-length buf)])
                  (let ([str (f (cdr ls) (fx+ n idx))])
                    (fill str buf n 0 idx))))])))))
   (define open-output-file
     (lambda (name mode)
       (unless (string? name)
         (error 'open-output-file "~s is not a valid file name" name))
       (let ([mode 
              (cond
               [(assq mode '([error 0] [append 1] [replace 2] [truncate 3]))
                => cadr]
               [else
                (error 'open-output-file "~s is not a valid mode" mode)])])
        (let ([fh (foreign-call "ik_open_file" name mode)])
          (fd->port fh name)))))
   (define write-char
     (lambda (c port)
       (unless (char? c)
         (error 'write-char "not a char: ~s" c))
       (unless (output-port-open? port)
         (error 'write-char "port ~s closed" port))
       (let ([idx (output-port-index port)] [size (output-port-size port)])
          (if (fx< idx size)
              (begin 
                (string-set! (output-port-buffer port) idx c)
                (set-output-port-index! port (fxadd1 idx))
                (when ($char= c #\newline)
                  (flush-output-port port)))
              (begin 
                (flush-output-port port)
                (write-char c port))))))
   (define fd-flush-proc
     (lambda (port)
       (let ([idx (output-port-index port)])
         (when (fx> idx 0)
           (foreign-call "ik_write" 
                          (output-port-fd port)
                          idx
                          (output-port-buffer port))))
        (set-output-port-index! port 0)))
   (define str-flush-proc
     (lambda (port)
       (let ([idx (output-port-index port)])
         (when (fx> idx 0)
           (let ([str (output-port-buffer port)])
             (when (fx= idx (string-length str))
               (set-output-port-fd! port
                 (cons str (output-port-fd port)))
               (set-output-port-buffer! port
                  (make-string (string-length str)))
               (set-output-port-index! port 0)))))))
   (define fd-close-proc
     (lambda (port)
       (let ([idx (output-port-index port)])
         (when (fx> idx 0)
           (foreign-call "ik_write" 
                          (output-port-fd port)
                          idx
                          (output-port-buffer port))))
         (foreign-call "ik_close" (output-port-fd port))))

   (define flush-output-port
     (lambda (port)
       (unless (output-port-open? port)
         (error 'flush-output-port "port ~s closed" port))
       ((output-port-flush-proc port) port)))
   (define close-output-port
     (lambda (port)
       (when (output-port-open? port)
         ((output-port-close-proc port) port)
         (set-output-port-open?! port #f))))
          
   ;;; init section
   (primitive-set! 'close-output-port 
     (case-lambda
       [() (close-output-port (current-output-port))]
       [(p) 
        (unless (output-port? p)
          (error 'close-output-port "~s is not an output port" p))
        (close-output-port p)]))
   (primitive-set! 'output-port? output-port?)
   (primitive-set! 'open-output-file 
     (case-lambda
       [(filename) (open-output-file filename 'error)]
       [(filename mode) (open-output-file filename mode)]))
   (primitive-set! 'write-char 
     (case-lambda
       [(c) (write-char c (current-output-port))]
       [(c p) 
        (unless (output-port? p)
          (error 'write-char "~s is not an output port" p))
        (write-char c p)]))
   (primitive-set! 'flush-output-port 
     (case-lambda
       [() (flush-output-port (current-output-port))]
       [(p) 
        (unless (output-port? p)
          (error 'flush-output-port "~s is not an output port" p))
        (flush-output-port p)]))
   (primitive-set! 'standard-output-port
     (let ([p (fd->port 1 '*stdout*)])
       (lambda () p)))
   (primitive-set! 'standard-error-port
     (let ([p (fd->port 2 '*stderr*)])
       (lambda () p)))
   (primitive-set! 'current-output-port
     (make-parameter (standard-output-port)
       (lambda (p)
         (unless (output-port? p)
           (error 'current-output-port "not a port ~s" p))
         p)))
   (primitive-set! 'console-output-port
     (make-parameter (standard-output-port)
       (lambda (p)
         (unless (output-port? p)
           (error 'console-output-port "not a port ~s" p))
         p)))
   (primitive-set! 'newline
     (case-lambda 
       [() (write-char #\newline (current-output-port))]
       [(p)
        (unless (output-port? p)
          (error 'newline "~s is not an output port" p))
        (write-char #\newline p)]))

   (primitive-set! 'open-output-string open-output-string)
   (primitive-set! 'get-output-string get-output-string)
   (primitive-set! 'output-port-name
     (lambda (x)
       (if (output-port? x)
           (output-port-name x)
           (error 'output-port-name "~s is not an output port" x)))))

;;; INPUT PORTS

(let ()
  ;;; input ports are similar to output ports, with the exception of
  ;;; the ungetchar buffer
  ;;; Fields:
  ;;; 0. id
  ;;; 1. file-name
  ;;; 2. file-descriptor
  ;;; 3. open?
  ;;; 4. buffer
  ;;; 5. buffer-size
  ;;; 6. index
  ;;; 7. unget
  (define input-port-id (gensym "input-port"))
  (define input-port?
    (lambda (x)
      (and (vector? x)
           (fx= (vector-length x) 8)
           (eq? (vector-ref x 0) input-port-id))))
  (define input-port-name 
    (lambda (x) 
      (vector-ref x 1)))
  (define input-port-fd
    (lambda (x)
      (vector-ref x 2)))
  (define input-port-open?
    (lambda (x)
      (vector-ref x 3)))
  (define input-port-buffer
    (lambda (x)
      (vector-ref x 4)))
  (define input-port-size
    (lambda (x)
      (vector-ref x 5)))
  (define set-input-port-size!
    (lambda (x i)
      (vector-set! x 5 i)))
  (define input-port-index
    (lambda (x)
      (vector-ref x 6)))
  (define set-input-port-index!
    (lambda (x i)
      (vector-set! x 6 i)))
  (define set-input-port-returned-char!
    (lambda (x i)
      (vector-set! x 7 i)))
  (define input-port-returned-char
    (lambda (x)
      (vector-ref x 7)))
  (define fd->port
    (lambda (fd filename)
      (vector input-port-id
              filename
              fd
              #t
              (make-string 4096)
              0
              0
              #f)))
  (define open-input-file
    (lambda (filename)
      (unless (string? filename)
        (error 'open-input-file "not a string: ~s" filename))
      (let ([fd (foreign-call "ik_open_file" filename 4)])
        (fd->port fd filename))))
  (define close-input-port
    (lambda port
      (let ([port
             (if (null? port)
                 (current-input-port)
                 (if (null? ($cdr port))
                     (let ([p ($car port)])
                       (if (input-port? p)
                           p
                           (error 'close-input-port "not an input port: ~s" p)))
                     (error 'close-input-port "too many arguments")))])
        (foreign-call "ik_close" (input-port-fd port))
        (void))))
  (define read-char
    (lambda (port)
      (unless (input-port-open? port)
        (error 'read-char "port closed"))
      (cond
        [(input-port-returned-char port) =>
         (lambda (c)
           (set-input-port-returned-char! port #f)
           c)]
        [else 
         (let ([idx (input-port-index port)]
               [size (input-port-size port)]
               [buf (input-port-buffer port)])
           (if ($fx< idx size)
               (let ([c ($string-ref buf idx)])
                 (set-input-port-index! port ($fxadd1 idx))
                 c)
               (let ([bytes 
                      (foreign-call "ik_read" 
                         (input-port-fd port)
                         buf
                         ($string-length buf))])
                 (set-input-port-size! port bytes)
                 (if ($fxzero? bytes)
                     (begin 
                       (set-input-port-index! port 0)
                       (eof-object))
                     (begin
                       (let ([c ($string-ref buf 0)])
                          (set-input-port-index! port 1)
                          c))))))])))
  (define peek-char
    (lambda (port)
      (unless (input-port-open? port)
        (error 'peek-char "port closed"))
      (cond
        [(input-port-returned-char port) =>
         (lambda (c) c)]
        [else 
         (let ([idx (input-port-index port)]
               [size (input-port-size port)]
               [buf (input-port-buffer port)])
           (if (fx< idx size)
               (string-ref buf idx)
               (let ([bytes 
                      (foreign-call "ik_read" 
                         (input-port-fd port)
                         buf
                         ($string-length buf))])
                 (set-input-port-size! port bytes)
                 (set-input-port-index! port 0)
                 (if (fxzero? bytes)
                     (eof-object)
                     (string-ref buf 0)))))])))
  (define reset-input-port!
    (lambda (p)
      (unless (input-port? p)
        (error 'reset-input-port! "~s is not an input port" p))
      (set-input-port-index! p 0)
      (set-input-port-size! p 0)
      (set-input-port-returned-char! p #f)))
  (define unread-char
    (lambda (c port)
      (unless (char? c)
        (error 'unread-char "not a character ~s" c))
      (unless (input-port-open? port)
        (error 'unread-char "port closed"))
      (when (input-port-returned-char port)
        (error 'unread-char "cannot unread twice"))
      (set-input-port-returned-char! port c)))
  (primitive-set! 'open-input-file open-input-file)
  (primitive-set! 'close-input-port 
    (case-lambda
      [() (close-input-port (current-input-port))]
      [(p)
       (unless (input-port? p) 
         (error 'close-input-port "~s is not an input port" p))
       (close-input-port p)]))
  (primitive-set! 'input-port? input-port?)
  (primitive-set! 'read-char 
    (case-lambda
      [() (read-char (current-input-port))]
      [(p)
       (unless (input-port? p) 
         (error 'read-char "~s is not an input port" p))
       (read-char p)]))
  (primitive-set! 'peek-char 
    (case-lambda
      [() (peek-char (current-input-port))]
      [(p)
       (unless (input-port? p) 
         (error 'peek-char "~s is not an input port" p))
       (peek-char p)]))
  (primitive-set! 'unread-char
    (case-lambda
      [(c) (unread-char c (current-input-port))]
      [(c p)
       (unless (input-port? p) 
         (error 'unread-char "~s is not an input port" p))
       (unread-char c p)]))
  (primitive-set! 'standard-input-port
    (let ([p (fd->port 0 '*stdin*)])
      (lambda () p)))
  (primitive-set! 'current-input-port
    (make-parameter (standard-input-port) 
      (lambda (x)
        (unless (input-port? x)
          (error 'current-input-port "not an input port ~s" x))
        x)))
  (primitive-set! 'console-input-port
    (make-parameter (standard-input-port) 
      (lambda (x)
        (unless (input-port? x)
          (error 'console-input-port "not an input port ~s" x))
        x)))
  (primitive-set! 'input-port-name
    (lambda (x)
      (if (input-port? x)
          (input-port-name x)
          (error 'input-port-name "~s is not an input port" x))))
  (primitive-set! 'reset-input-port! reset-input-port!))         

(primitive-set! 'with-output-to-file
   (lambda (name proc . args)
     (unless (string? name) 
       (error 'with-output-to-file "~s is not a string" name))
     (unless (procedure? proc)
       (error 'with-output-to-file "~s is not a procedure" proc))
     (let ([p (apply open-output-file name args)]
           [shot #f])
       (parameterize ([current-output-port p])
         (dynamic-wind 
           (lambda () 
             (when shot
               (error 'with-output-to-file 
                      "cannot reenter")))
           proc
           (lambda () 
             (close-output-port p)
             (set! shot #t)))))))

(primitive-set! 'call-with-output-file
   (lambda (name proc . args)
     (unless (string? name) 
       (error 'call-with-output-file "~s is not a string" name))
     (unless (procedure? proc)
       (error 'call-with-output-file "~s is not a procedure" proc))
     (let ([p (apply open-output-file name args)]
           [shot #f])
       (dynamic-wind 
         (lambda () 
           (when shot
             (error 'call-with-output-file "cannot reenter")))
         (lambda () (proc p))
         (lambda () 
           (close-output-port p)
           (set! shot #t))))))

(primitive-set! 'with-input-from-file
   (lambda (name proc . args)
     (unless (string? name) 
       (error 'with-input-from-file "~s is not a string" name))
     (unless (procedure? proc)
       (error 'with-input-from-file "~s is not a procedure" proc))
     (let ([p (apply open-input-file name args)]
           [shot #f])
       (parameterize ([current-input-port p])
         (dynamic-wind 
           (lambda () 
             (when shot
               (error 'with-input-from-file 
                      "cannot reenter")))
           proc
           (lambda () 
             (close-input-port p)
             (set! shot #t)))))))

(primitive-set! 'call-with-input-file
   (lambda (name proc . args)
     (unless (string? name) 
       (error 'call-with-input-file "~s is not a string" name))
     (unless (procedure? proc)
       (error 'call-with-input-file "~s is not a procedure" proc))
     (let ([p (apply open-input-file name args)]
           [shot #f])
       (dynamic-wind 
         (lambda () 
           (when shot
             (error 'call-with-input-file "cannot reenter")))
         (lambda () (proc p))
         (lambda () 
           (close-input-port p)
           (set! shot #t))))))

