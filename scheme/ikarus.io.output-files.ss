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


(library (ikarus io output-files)
  (export standard-output-port standard-error-port
          console-output-port current-output-port
          open-output-file with-output-to-file call-with-output-file)
  (import 
    (ikarus system $ports)
    (ikarus system $io)
    (ikarus system $strings)
    (ikarus system $chars)
    (ikarus system $bytevectors)
    (ikarus system $fx)
    (rnrs bytevectors)
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
           (close-output-port p)
           (close-ports))])))

  (define do-write-buffer
    (lambda (fd port-name buff idx caller)
      (let ([bytes (foreign-call "ikrt_write_file" fd buff idx)])
        (if (fixnum? bytes)
            bytes
            (error caller "cannot write to file" port-name bytes)))))

  (define make-output-file-handler
    (lambda (fd port-name)
      (define open? #t)
      (define output-file-handler
        (lambda (msg . args)
          (message-case msg args
            [(write-byte b p)
             (if (and (fixnum? b) ($fx<= 0 b) ($fx<= b 255))
                 (if (output-port? p)
                     (let ([idx ($port-index p)])
                       (if ($fx< idx ($port-size p))
                           (begin
                             ($bytevector-set! ($port-buffer p) idx b)
                             ($set-port-index! p ($fxadd1 idx)))
                           (if open?
                             (let ([bytes (do-write-buffer fd port-name
                                             ($port-buffer p) idx 'write-char)])
                               ($set-port-index! p 0)
                               ($write-byte b p))
                             (error 'write-byte "port is closed" p))))
                     (error 'write-byte "not an output-port" p))
                 (error 'write-byte "not a byte" b))]
            [(write-char c p)
             (if (char? c)
                 (if (output-port? p)
                     (let ([b ($char->fixnum c)])
                       (if ($fx<= b 255)
                           ($write-byte b p)
                           (error 'write-char 
                              "BUG: multibyte write of not implemented" c)))
                     (error 'write-char "not an output-port" p))
                 (error 'write-char "not a character" c))]
            [(flush-output-port p)
             (if (output-port? p)
                 (if open?
                     (let ([bytes (do-write-buffer fd port-name
                                     ($port-buffer p) 
                                     ($port-index p) 
                                     'flush-output-port)])
                       ($set-port-index! p 0))
                     (error 'flush-output-port "port is closed" p))
                 (error 'flush-output-port "not an output-port" p))]
            [(close-port p)
             (when open?
               (flush-output-port p)
               ($set-port-size! p 0)
               (set! open? #f)
               (unless (foreign-call "ikrt_close_file" fd)
                 (error 'close-output-port "cannot close" port-name)))]
            [(port-name p) port-name]
            [else (error 'output-file-handler 
                         "unhandled message" (cons msg args))])))
      output-file-handler))
  (define (option-id x)
    (case x
      [(error)    0]
      [(replace)  1]
      [(truncate) 2]
      [(append)   3]
      [else (error 'open-output-file "not a valid mode" x)]))

  (define $open-output-file
    (lambda (filename options)
      (close-ports)
      (let ([fd/error 
             (foreign-call "ikrt_open_output_file" 
                           (string->utf8 filename)
                           (option-id options))])
        (if (fixnum? fd/error)
            (let ([port
                   (make-output-port
                     (make-output-file-handler fd/error filename)
                     ($make-bytevector 4096))])
              (guardian port)
              port)
            (error 'open-output-file "cannot open file" filename fd/error)))))

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
            (error 'current-output-port "not an output port" p))]))

  (define open-output-file 
    (case-lambda
      [(filename)
       (if (string? filename)
           ($open-output-file filename 'error)
           (error 'open-output-file "not a string" filename))]
      [(filename options)
       (if (string? filename)
           ($open-output-file filename options)
           (error 'open-output-file "not a string" filename))]))

  (define with-output-to-file
     (lambda (name proc . args)
       (unless (string? name) 
         (error 'with-output-to-file "not a string" name))
       (unless (procedure? proc)
         (error 'with-output-to-file "not a procedure" proc))
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
         (error 'call-with-output-file "not a string" name))
       (unless (procedure? proc)
         (error 'call-with-output-file "not a procedure" proc))
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
      ($make-bytevector 4096)))
  (set! *current-output-port* *standard-output-port*)
  (set! *standard-error-port* 
    (make-output-port
      (make-output-file-handler 2 '*stderr*)
      ($make-bytevector 4096))) )
