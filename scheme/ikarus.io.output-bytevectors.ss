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


(library (ikarus io output-bytevectors)
  (export open-output-bytevector get-output-bytevector
          with-output-to-bytevector open-bytevector-output-port
          call-with-bytevector-output-port)
  (import 
    (ikarus system $bytevectors)
    (ikarus system $chars)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $ports)
    (ikarus system $io)
    (except (ikarus) 
      open-output-bytevector get-output-bytevector
      with-output-to-bytevector open-bytevector-output-port
      call-with-bytevector-output-port))

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



  (define make-output-bytevector-handler
    (lambda ()
      (define buffer-list '())
      (define open? #t)
      (define output-handler
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
                             (let ([buff ($port-buffer p)])
                               (set! buffer-list (cons (bv-copy buff) buffer-list))
                               ($bytevector-set! buff 0 b)
                               ($set-port-index! p 1))
                             (error 'write-byte "port is closed" p))))
                     (error 'write-byte "not an output-port" p))
                 (error 'write-byte "not a byte" b))]
            [(write-char c p)
             (if (char? c)
                 (if (output-port? p)
                     (let ([b ($char->fixnum c)])
                       (if ($fx<= b 127)
                           ($write-byte b p)
                           (error 'write-char 
                             "BUG: multibyte write of is not implemented" c)))
                     (error 'write-char "not an output-port" p))
                 (error 'write-char "not a character" c))]
            [(flush-output-port p)
             (void)]
            [(close-port p)
             (set! open? #f)]
            [(port-name p) 'bytevector-port]
            [(get-output-bytevector p) 
             (concat 
               ($port-buffer p) 
               ($port-index p)
               buffer-list)]
            [(reset-port p)
             ($set-port-index! p 0) 
             (set! buffer-list '())]
            [else 
             (error 'bytevector-output-handler 
                "unhandled message" (cons msg args))])))
      output-handler))

  (define open-output-bytevector 
    (lambda ()
      (make-output-port 
        (make-output-bytevector-handler)
        ($make-bytevector 59))))

  (define get-output-bytevector
    (lambda (p)
      (if (output-port? p)
          (($port-handler p) 'get-output-bytevector p)
          (error 'get-output-bytevector "not an output port" p))))
  
  (define with-output-to-bytevector
    (lambda (f)
      (unless (procedure? f)
        (error 'with-output-to-bytevector "not a procedure" f))
      (let ([p (open-output-bytevector)])
        (parameterize ([current-output-port p]) (f))
        (get-output-bytevector p))))
  
  (define open-bytevector-output-port
    (case-lambda
      [()
       (let ([p (open-output-bytevector)])
         ;;; FIXME: should empty string
         (values p 
           (lambda ()
             (let ([x (get-output-bytevector p)])
               (($port-handler p) 'reset-port p)
               x))))]
      [(transcoder) 
       (if (not transcoder) 
           (open-bytevector-output-port)
           (error 'open-bytevector-output-port 
              (format "BUG: transcoder (~s) is not supported"
                      transcoder)))]))

  (define call-with-bytevector-output-port
    (let ([who 'call-with-bytevector-output-port])
      (case-lambda
        [(proc) 
         (unless (procedure? proc) 
           (error who "not a procedure" proc))
         (let ([p (open-output-bytevector)])
           (proc p) ;;; why is this useful again?
           (let ([bv (get-output-bytevector p)])
             (close-output-port p)
             bv))]
        [(proc transcoder)
         (if (not transcoder) 
             (call-with-bytevector-output-port proc)
             (error who 
               (format "BUG: transcoder (~s) is not supported"
                       transcoder)))])))

)
