;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus io input-files)
  (export open-input-file current-input-port console-input-port
          with-input-from-file call-with-input-file)
  (import
    (ikarus system $ports)
    (ikarus system $io)
    (ikarus system $fx)
    (ikarus system $strings)
    (ikarus system $bytevectors)
    (ikarus system $chars)
    (rnrs bytevectors)
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
                  (error 'message-case "unmatched" (cons tmsg targs)))]
             [(__ y (a a* (... ...)) body)
              (if (pair? y)
                  (let ([a (car y)] [d (cdr y)])
                    (match-and-bind d (a* (... ...)) body))
                  (error 'message-case "unmatched" (cons tmsg targs)))]))
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
 
  (define refill-buffer!
    (lambda (p bytes)
      (error 'refill-buffer! "not implemented")))

  (define read-multibyte-char 
    (lambda (p b0)
      (let ([idx ($port-index p)] 
            [size ($port-size p)])
        (cond
          [($fx= ($fxlogand b0 #b11100000) #b11000000) 
           ;;; 2-byte utf8 sequence
           (unless ($fx< ($fx+ idx 1) size)
             (refill-buffer! p 1))
           (let ([b1 ($bytevector-u8-ref 
                       ($port-buffer p)
                       ($fxadd1 idx))])
             (unless ($fx= ($fxlogand b1 #b11000000) #b10000000)
               (error 'read-char "invalid utf8 sequence" b0 b1))
             ($set-port-index! p ($fx+ idx 2))
             ($fixnum->char 
               ($fx+ ($fxsll ($fxlogand b0 #b11111) 6)
                     ($fxlogand b1 #b111111))))]
          [else 
           (error 'read-multibyte
             "BUG: bytesequence is not supported yet" b0)]))))

  (define peek-multibyte-char 
    (lambda (p)
      (error 'peek-multibyte-char "not implemented")))
  (define unread-multibyte-char 
    (lambda (c p)
      (error 'unread-multibyte-char "not implemented")))

  (define make-input-file-handler
    (lambda (fd port-name)
      (let ((open? #t))
        (lambda (msg . args)
          (message-case msg args
            [(read-char p)
             (unless (input-port? p)
               (error 'read-char "not an input port" p))
             (let ([idx ($port-index p)])
               (if ($fx< idx ($port-size p))
                   (let ([b ($bytevector-u8-ref ($port-buffer p) idx)])
                     (cond
                       [($fx< b 128) 
                        ($set-port-index! p ($fxadd1 idx))
                        ($fixnum->char b)]
                       [else (read-multibyte-char p b)]))
                   (if open?
                       (let ([bytes
                              (foreign-call "ikrt_read" 
                                 fd ($port-buffer p))])
                         (cond
                           [($fx> bytes 0)
                            ($set-port-size! p bytes)
                            ($read-char p)]
                           [($fx= bytes 0)
                            (eof-object)]
                           [else
                            (error 'read-char "Cannot read from file"
                                   port-name)]))
                       (error 'read-char "port is closed" p))))]
            [(get-u8 p)
             (unless (input-port? p)
               (error 'get-u8 "not an input port" p))
             (let ([idx ($port-index p)])
               (if ($fx< idx ($port-size p))
                   (let ([b ($bytevector-u8-ref ($port-buffer p) idx)])
                      ($set-port-index! p ($fxadd1 idx))
                      b)
                   (if open?
                       (let ([bytes
                              (foreign-call "ikrt_read" 
                                 fd ($port-buffer p))])
                         (cond
                           [($fx> bytes 0)
                            ($set-port-size! p bytes)
                            ($get-u8 p)]
                           [($fx= bytes 0)
                            (eof-object)]
                           [else
                            (error 'get-u8 "Cannot read from file" port-name)]))
                       (error 'get-u8 "port is closed" p))))]
            [(peek-char p)
             (unless (input-port? p)
               (error 'peek-char "not an input port" p))
             (let ([idx ($port-index p)])
               (if ($fx< idx ($port-size p))
                   (let ([b ($bytevector-u8-ref ($port-buffer p) idx)])
                     (cond
                       [($fx< b 128) ($fixnum->char b)]
                       [else (peek-multibyte-char p)]))
                   (if open?
                       (let ([bytes
                              (foreign-call "ikrt_read" fd
                                            (port-input-buffer p))])
                         (cond
                           [(not bytes)
                            (error 'peek-char
                                   "Cannot read from file" port-name)]
                           [($fx= bytes 0)
                            (eof-object)]
                           [else
                            ($set-port-size! p bytes)
                            ($peek-char p)]))
                       (error 'peek-char "port is closed" p))))]
            [(unread-char c p)
             (unless (input-port? p)
               (error 'unread-char "not an input port" p))
             (let ([idx ($fxsub1 ($port-index p))]
                   [b (if (char? c) 
                          ($char->fixnum c)
                          (error 'unread-char "not a char" c))])
               (if (and ($fx>= idx 0)
                        ($fx< idx ($port-size p)))
                   (cond
                     [($fx< b 128)
                      ($set-port-index! p idx)]
                     [else (unread-multibyte-char c p)])
                   (if open?
                       (error 'unread-char "port is closed" p)
                       (error 'unread-char "too many unread-chars"))))]
            [(port-name p) port-name]
            [(close-port p)
             (unless (input-port? p)
               (error 'close-input-port "not an input port" p))
             (when open?
               ($set-port-size! p 0)
               (set! open? #f)
               (unless (foreign-call "ikrt_close_file" fd)
                  (error 'close-input-port "cannot close port" port-name)))]
            [else 
             (error 'input-file-handler
                    "message not handled" (cons msg args))])))))

  (define $open-input-file
    (lambda (filename)
      (close-ports)
      (let ([fd/error (foreign-call "ikrt_open_input_file" 
                        (string->utf8 filename))])
        (if (fixnum? fd/error)
            (let ([port (make-input-port 
                          (make-input-file-handler fd/error filename)
                          ($make-bytevector 4096))])
              (set-port-input-size! port 0)
              (guardian port)
              port)
            (error 'open-input-file "cannot open file" 
                   filename fd/error)))))

  (define open-input-file
    (lambda (filename)
       (if (string? filename)
           ($open-input-file filename)
           (error 'open-input-file "not a string" filename))))

  (define with-input-from-file
     (lambda (name proc)
       (unless (string? name) 
         (error 'with-input-from-file "not a string" name))
       (unless (procedure? proc)
         (error 'with-input-from-file "not a procedure" proc))
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
         (error 'call-with-input-file "not a string" name))
       (unless (procedure? proc)
         (error 'call-with-input-file "not a procedure" proc))
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
           (error 'current-input-port "not an input-port" p))]))

  (set! *standard-input-port* 
    (let ([p (make-input-port 
               (make-input-file-handler 0 '*stdin*)
               ($make-bytevector 4096))])
      (set-port-input-size! p 0)
      p))
  (set! *current-input-port* *standard-input-port*)
  )
