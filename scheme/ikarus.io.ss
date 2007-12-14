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

#;
(library (ikarus system $io tmp)
  (export $make-port $port-tag $port-id $port-cookie
          port? $port-transcoder 
          $port-index $port-size $port-buffer 
          $port-get-position $port-set-position! $port-close
          $port-read! $port-write! $set-port-index! $set-port-size!
          $port-attrs $set-port-attrs!)
  (import (except (ikarus) port?))
  (define-struct $port 
    (attrs index size buffer transcoder 
     id read! write! get-position set-position! close cookie))
  (define port? $port?)
  (define $set-port-index! set-$port-index!)
  (define $set-port-size! set-$port-size!)
  (define $set-port-attrs! set-$port-attrs!)
  (define $make-port make-$port)
  (define ($port-tag x) (if ($port? x) ($port-attrs x) 0)))

(library (io-spec) 
  
  (export 
    port? input-port? output-port? textual-port? binary-port?
    open-file-input-port open-input-file 
    call-with-input-file with-input-from-file
    standard-input-port current-input-port
    open-bytevector-input-port
    open-string-input-port
    make-custom-binary-input-port 
    make-custom-binary-output-port 
    make-custom-textual-input-port 
    make-custom-textual-output-port 
    transcoded-port port-transcoder
    close-port close-input-port close-output-port
    port-eof?
    get-char lookahead-char read-char peek-char
    get-string-n get-string-n! get-string-all get-line
    get-u8 lookahead-u8 
    get-bytevector-n get-bytevector-n!
    get-bytevector-some get-bytevector-all 
    ;port-has-port-position? port-position
    ;port-has-set-port-position!? set-port-position!
    call-with-port
    flush-output-port
    put-u8
    put-char write-char
    put-string
    open-bytevector-output-port
    call-with-bytevector-output-port
    open-string-output-port
    call-with-string-output-port
    standard-output-port standard-error-port
    current-output-port current-error-port
    open-file-output-port open-output-file
    console-output-port
    console-error-port
    console-input-port
    newline
    input-port-name
    output-port-name
    port-mode set-port-mode!
    reset-input-port!
    port-id
    )

  
  (import 
    (ikarus system $io)
    (except (ikarus)
      port? input-port? output-port? textual-port? binary-port? 
      open-file-input-port open-input-file 
      call-with-input-file with-input-from-file
      standard-input-port current-input-port
      open-bytevector-input-port
      open-string-input-port
      make-custom-binary-input-port
      make-custom-binary-output-port 
      make-custom-textual-input-port 
      make-custom-textual-output-port 
      transcoded-port port-transcoder
      close-port close-input-port close-output-port
      port-eof?
      get-char lookahead-char read-char peek-char
      get-string-n get-string-n! get-string-all get-line
      get-u8 lookahead-u8 
      get-bytevector-n get-bytevector-n!
      get-bytevector-some get-bytevector-all 
      ;port-has-port-position? port-position
      ;port-has-set-port-position!? set-port-position!
      call-with-port
      flush-output-port
      put-u8
      put-char write-char
      put-string
      open-bytevector-output-port
      call-with-bytevector-output-port
      open-string-output-port
      call-with-string-output-port
      standard-output-port standard-error-port
      current-output-port current-error-port
      open-file-output-port open-output-file
      console-output-port
      console-input-port
      console-error-port
      newline
      input-port-name
      output-port-name
      port-mode set-port-mode!
      reset-input-port!
      port-id
      ))

  (module UNSAFE  
    (fx< fx<= fx> fx>= fx= fx+ fx-
     fxior fxand fxsra fxsll
     integer->char char->integer
     string-ref string-set! string-length
     bytevector-u8-ref bytevector-u8-set!)

    (import 
      (rename (ikarus system $strings)
        ($string-length string-length)
        ($string-ref    string-ref)
        ($string-set!   string-set!))
      (rename (ikarus system $chars)
        ($char->fixnum char->integer)
        ($fixnum->char integer->char))
      (rename (ikarus system $bytevectors)
        ($bytevector-set!   bytevector-u8-set!)
        ($bytevector-u8-ref bytevector-u8-ref))
      (rename (ikarus system $fx) 
        ($fxsra    fxsra)
        ($fxsll    fxsll)
        ($fxlogor  fxior)
        ($fxlogand fxand)
        ($fx+      fx+)
        ($fx-      fx-)
        ($fx<      fx<)
        ($fx>      fx>)
        ($fx>=     fx>=)
        ($fx<=     fx<=)
        ($fx=      fx=))))

  (define (port? x)
    (import (only (ikarus) port?))
    (port? x))

  (define-syntax define-rrr
    (syntax-rules ()
      [(_ name)
       (define (name . args) 
         (apply error 'name "not implemented" args))]))

 
  (define-syntax u8?
    (let ()
      (import (ikarus system $fx))
      (syntax-rules ()
        [(_ x) 
         ($fxzero? ($fxlogand x -256))])))

  ;(define (u8? x) (and (fixnum? x) (fx>= x 0) (fx< x 256)))
  
  (define (textual-port? x) 
    (fx= (fxand ($port-tag x) textual-port-tag) textual-port-tag))

  (define (binary-port? x) 
    (fx= (fxand ($port-tag x) binary-port-tag) binary-port-tag))

  (define (output-port? x) 
    (fx= (fxand ($port-tag x) output-port-tag) output-port-tag))

  (define (input-port? x)
    (fx= (fxand ($port-tag x) input-port-tag) input-port-tag))

  ;;; everything above this line will turn into primitive
  ;;; ----------------------------------------------------------
  
  (define input-port-tag           #b00000000000001)
  (define output-port-tag          #b00000000000010)
  (define textual-port-tag         #b00000000000100)
  (define binary-port-tag          #b00000000001000)
  (define fast-char-text-tag       #b00000000010000)
  (define fast-u7-text-tag         #b00000000100000)
  (define fast-u8-text-tag         #b00000001100000)
  (define r6rs-mode-tag            #b01000000000000)
  (define closed-port-tag          #b10000000000000)

  (define port-type-mask           #b00000000001111)
  (define binary-input-port-bits   #b00000000001001)
  (define binary-output-port-bits  #b00000000001010)
  (define textual-input-port-bits  #b00000000000101)
  (define textual-output-port-bits #b00000000000110)

  (define fast-get-byte-tag        #b00000000001001)
  (define fast-get-char-tag        #b00000000010101)
  (define fast-get-utf8-tag        #b00000000100101)
  (define fast-get-latin-tag       #b00000001100101)

  (define fast-put-byte-tag        #b00000000001010)
  (define fast-put-char-tag        #b00000000010110)
  (define fast-put-utf8-tag        #b00000000100110)
  (define fast-put-latin-tag       #b00000001100110)

  (define fast-attrs-mask          #xFFF)
  (define ($port-fast-attrs x) (fxand ($port-tag x) fast-attrs-mask))

  (define (input-port-name p)
    (if (input-port? p) 
        ($port-id p)
        (error 'input-port-name "not an input port" p)))

  (define (output-port-name p)
    (if (output-port? p) 
        ($port-id p)
        (error 'output-port-name "not an output port" p)))

  (define (port-id p)
    (if (port? p) 
        ($port-id p)
        (error 'port-id "not a port" p)))

  (define guarded-port
    (let ([G (make-guardian)])
      (define (clean-up)
        (cond
          [(G) =>
           (lambda (p)
             (close-port p)
             (clean-up))]))
      (lambda (p)
        (clean-up)
        (when (fixnum? ($port-cookie p)) 
          (G p))
        p)))

  (define ($make-custom-binary-port attrs init-size id 
            read! write! get-position set-position! close buffer-size)
    (let ([bv (make-bytevector buffer-size)])
      ($make-port attrs 0 init-size bv #f id read! write! get-position
                  set-position! close #f)))

  (define ($make-custom-textual-port attrs init-size id 
            read! write! get-position set-position! close buffer-size)
    (let ([bv (make-string buffer-size)])
      ($make-port attrs 0 init-size bv #t id read! write! get-position
                  set-position! close #f)))

  (define (make-custom-binary-input-port id 
            read! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-binary-input-port)
    (unless (string? id)
      (error who "id is not a string" id))
    (unless (procedure? read!)
      (error who "read! is not a procedure" read!))
    (unless (or (procedure? close) (not close))
      (error who "close should be either a procedure or #f" close))
    ($make-custom-binary-port 
      binary-input-port-bits
      0
      id read! #f get-position
      set-position! close 256))

  (define (make-custom-binary-output-port id 
            write! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-binary-output-port)
    (unless (string? id)
      (error who "id is not a string" id))
    (unless (procedure? write!)
      (error who "write! is not a procedure" write!))
    (unless (or (procedure? close) (not close))
      (error who "close should be either a procedure or #f" close))
    ($make-custom-binary-port 
      binary-output-port-bits
      256
      id #f write! get-position
      set-position! close 256))

  (define (make-custom-textual-input-port id 
            read! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-textual-input-port)
    (unless (string? id)
      (error who "id is not a string" id))
    (unless (procedure? read!)
      (error who "read! is not a procedure" read!))
    (unless (or (procedure? close) (not close))
      (error who "close should be either a procedure or #f" close))
    ($make-custom-textual-port 
      textual-input-port-bits
      0
      id read! #f get-position
      set-position! close 256))

  (define (make-custom-textual-output-port id 
            write! get-position set-position! close)
    ;;; FIXME: get-position and set-position! are ignored for now
    (define who 'make-custom-textual-output-port)
    (unless (string? id)
      (error who "id is not a string" id))
    (unless (procedure? write!)
      (error who "write! is not a procedure" write!))
    (unless (or (procedure? close) (not close))
      (error who "close should be either a procedure or #f" close))
    ($make-custom-textual-port 
      textual-output-port-bits
      256
      id #f write! get-position
      set-position! close 256))



  (define (input-transcoder-attrs x)
    (cond
      [(not x) ;;; binary input port
       binary-input-port-bits]
      [(and (eq? 'latin-1-codec (transcoder-codec x))
            (eq? 'none (transcoder-eol-style x)))
       (fxior textual-input-port-bits fast-u8-text-tag)]
      [else textual-input-port-bits]))

  (define (output-transcoder-attrs x)
    (cond
      [(not x) ;;; binary input port
       binary-output-port-bits]
      [(and (eq? 'latin-1-codec (transcoder-codec x))
            (eq? 'none (transcoder-eol-style x)))
       (fxior textual-output-port-bits fast-u8-text-tag)]
      [(and (eq? 'utf-8-codec (transcoder-codec x))
            (eq? 'none (transcoder-eol-style x)))
       (fxior textual-output-port-bits fast-u7-text-tag)]
      [else textual-output-port-bits]))

  (define open-bytevector-input-port
    (case-lambda
      [(bv) (open-bytevector-input-port bv #f)]
      [(bv maybe-transcoder) 
       (unless (bytevector? bv) 
         (error 'open-bytevector-input-port 
                "not a bytevector" bv))
       (when (and maybe-transcoder
                  (not (transcoder? maybe-transcoder)))
         (error 'open-bytevector-input-port 
                "not a transcoder" maybe-transcoder))
       ($make-port 
          (input-transcoder-attrs maybe-transcoder)
          0 (bytevector-length bv) bv 
          maybe-transcoder 
          "*bytevector-input-port*" 
          (lambda (bv i c) 0) ;;; read!
          #f ;;; write!
          #f ;;; FIXME: get-position
          #f ;;; FIXME: set-position!
          #f ;;; close
          #f)]))

  (define open-bytevector-output-port
    (case-lambda
      [() (open-bytevector-output-port #f)]
      [(transcoder) 
       (define who 'open-bytevector-output-port)
       (unless (or (not transcoder) (transcoder? transcoder))
         (error who "invalid transcoder value" transcoder))
       (let ([buf* '()] [buffer-size 256])
         (let ([p 
                ($make-port 
                   (output-transcoder-attrs transcoder)
                   0 buffer-size (make-bytevector buffer-size)
                   transcoder
                   "*bytevector-output-port*"
                   #f
                   (lambda (bv i c) 
                     (unless (= c 0) 
                       (let ([x (make-bytevector c)])
                         (bytevector-copy! bv i x 0 c)
                         (set! buf* (cons x buf*))))
                     c)
                   #f  ;;; FIXME: get-position
                   #f  ;;; FIXME: set-position!
                   #f
                   #f)])
           (values
             p
             (lambda () 
               (define (append-bv-buf* ls) 
                 (let f ([ls ls] [i 0])
                   (cond
                     [(null? ls) 
                      (values (make-bytevector i) 0)]
                     [else 
                      (let* ([a (car ls)]
                             [n (bytevector-length a)])
                        (let-values ([(bv i) (f (cdr ls) (fx+ i n))])
                          (bytevector-copy! a 0 bv i n)
                          (values bv (fx+ i n))))])))
               (unless ($port-closed? p)
                 (flush-output-port p))
               (let-values ([(bv len) (append-bv-buf* buf*)])
                 (set! buf* '())
                 bv)))))]))

  (define call-with-bytevector-output-port
    (case-lambda
      [(proc) (call-with-bytevector-output-port proc #f)]
      [(proc transcoder) 
       (define who 'call-with-bytevector-output-port)
       (unless (procedure? proc) 
         (error who "not a procedure" proc))
       (unless (or (not transcoder) (transcoder? transcoder))
         (error who "invalid transcoder argument" transcoder))
       (let-values ([(p extract) 
                     (open-bytevector-output-port transcoder)])
         (proc p)
         (extract))]))

  (define (call-with-string-output-port proc)
    (define who 'call-with-string-output-port)
    (unless (procedure? proc) 
      (error who "not a procedure" proc))
    (let-values ([(p extract) (open-string-output-port)])
      (proc p)
      (extract)))
  
  (define (open-string-output-port)
    (define who 'open-string-output-port)
    (let ([buf* '()] [buffer-size 256])
      (let ([p 
             ($make-port
                (fxior textual-output-port-bits fast-char-text-tag)
                0 buffer-size (make-string buffer-size)
                #t ;;; transcoder
                "*string-output-port*"
                #f
                (lambda (str i c) 
                  (unless (= c 0) 
                    (let ([x (make-string c)])
                      (string-copy! str i x 0 c)
                      (set! buf* (cons x buf*))))
                  c)
                #f  ;;; FIXME: get-position
                #f  ;;; FIXME: set-position!
                #f
                #f)])
        (values
          p
          (lambda () 
            (define (append-str-buf* ls) 
              (let f ([ls ls] [i 0])
                (cond
                  [(null? ls) 
                   (values (make-string i) 0)]
                  [else 
                   (let* ([a (car ls)]
                          [n (string-length a)])
                     (let-values ([(bv i) (f (cdr ls) (fx+ i n))])
                       (string-copy! a 0 bv i n)
                       (values bv (fx+ i n))))])))
            (unless ($port-closed? p)
              (flush-output-port p))
            (let-values ([(bv len) (append-str-buf* buf*)])
              (set! buf* '())
              bv))))))

  (define (open-string-input-port str)
    (unless (string? str) 
      (error 'open-string-input-port str))
    ($make-port 
       (fxior textual-input-port-bits fast-char-text-tag)
       0 (string-length str) str
       #t ;;; transcoder
       "*string-input-port*" 
       (lambda (str i c) 0) ;;; read!
       #f ;;; write!
       #f ;;; FIXME: get-position
       #f ;;; FIXME: set-position!
       #f ;;; close
       #f))
    

  (define (transcoded-port p transcoder)
    (define who 'transcoded-port)
    (unless (transcoder? transcoder)
      (error who "not a transcoder" transcoder))
    (unless (port? p) (error who "not a port" p))
    (when ($port-transcoder p) (error who "not a binary port" p))
    (when ($port-closed? p) (error who "cannot transcode closed port" p))
    (let ([read! ($port-read! p)]
          [write! ($port-write! p)])
      ($mark-port-closed! p)
      (guarded-port
        ($make-port 
          (cond
            [read! (input-transcoder-attrs transcoder)]
            [write! (output-transcoder-attrs transcoder)]
            [else
             (error 'transcoded-port 
               "port is neither input nor output!")])
          ($port-index p)
          ($port-size p)
          ($port-buffer p)
          transcoder
          ($port-id p)
          read!
          write!
          ($port-get-position p)
          ($port-set-position! p)
          ($port-close p)
          ($port-cookie p)))))

  (define (reset-input-port! p)
    (if (input-port? p) 
        ($set-port-index! p ($port-size p))
        (error 'reset-input-port! "not an input port" p)))

  (define (port-transcoder p)
    (if (port? p)
        (let ([tr ($port-transcoder p)])
          (and (transcoder? tr) tr))
        (error 'port-transcoder "not a port" p)))
              
  (define ($port-closed? p) 
    (not (fxzero? (fxand ($port-attrs p) closed-port-tag))))
  (define ($mark-port-closed! p) 
    ($set-port-attrs! p 
      (fxior closed-port-tag 
             (fxand ($port-attrs p) port-type-mask))))


  (define (port-mode p)
    (if (port? p) 
        (if (fxzero? (fxand ($port-attrs p) r6rs-mode-tag))
            'ikarus-mode
            'r6rs-mode)
        (error 'port-mode "not a port" p)))

  (define (set-port-mode! p mode)
    (if (port? p) 
        (case mode
          [(r6rs-mode) 
           ($set-port-attrs! p
             (fxior ($port-attrs p) r6rs-mode-tag))]
          [(ikarus-mode)
           ($set-port-attrs! p
             (fxand ($port-attrs p) (fxnot r6rs-mode-tag)))]
          [else (error 'set-port-mode! "invalid mode" mode)])
        (error 'set-port-mode! "not a port" p)))


  (define flush-output-port
    (case-lambda
      [() (flush-output-port (*the-output-port*))] 
      [(p)
       (unless (output-port? p) 
         (error 'flush-output-port "not an output port" p))
       (when ($port-closed? p) 
         (error 'flush-output-port "port is closed" p))
       (let ([idx ($port-index p)]
             [buf ($port-buffer p)])
         (unless (fx= idx 0)
           (let ([bytes (($port-write! p) buf 0 idx)])
             (unless (and (fixnum? bytes) (fx>= bytes 0) (fx<= bytes idx))
               (error 'flush-output-port 
                      "write! returned an invalid value" 
                      bytes))
             (cond
               [(fx= bytes idx) 
                ($set-port-index! p 0)]
               [(fx= bytes 0) 
                (error 'flush-output-port "could not write bytes to sink")]
               [else
                (bytevector-copy! buf bytes buf 0 (fx- idx bytes))
                ($set-port-index! p (fx- idx bytes))
                (flush-output-port p)]))))]))

  (define ($close-port p)
    (cond
      [($port-closed? p) (void)]
      [else
       (when ($port-write! p)
         (flush-output-port p))
       ($mark-port-closed! p)
       (let ([close ($port-close p)])
         (when (procedure? close)
           (close)))]))

  (define (close-port p)
    (unless (port? p)
       (error 'close-port "not a port" p))
    ($close-port p))

  (define (close-input-port p)
    (unless (input-port? p)
       (error 'close-input-port "not an input port" p))
    ($close-port p))

  (define (close-output-port p)
    (unless (output-port? p)
       (error 'close-output-port "not an output port" p))
    ($close-port p))

  ;(define-rrr port-has-port-position?)
  ;(define-rrr port-position)
  ;(define-rrr port-has-set-port-position!?)
  ;(define-rrr set-port-position!)

  ;;; ----------------------------------------------------------
  (module (get-char lookahead-char)
    (import UNSAFE)
    (define (refill-bv-start p who)
      (when ($port-closed? p) (error who "port is closed" p))
      (let* ([bv ($port-buffer p)]
             [n (bytevector-length bv)])
        (let ([j (($port-read! p) bv 0 n)])
          (unless (fixnum? j)
            (error who "invalid return value from read! procedure" j))
          (cond
            [(fx>= j 0)
             (unless (fx<= j n)
               (error who "read! returned a value out of range" j))
             ($set-port-index! p 0)
             ($set-port-size! p j)
             j]
            [else 
             (error who "read! returned a value out of range" j)]))))
    (define (refill-bv-buffer p who)
      (when ($port-closed? p) (error who "port is closed" p))
      (let ([bv ($port-buffer p)] [i ($port-index p)] [j ($port-size p)])
        (let ([c0 (fx- j i)])
          (bytevector-copy! bv i bv 0 c0)
          (let* ([max (fx- (bytevector-length bv) c0)]
                 [c1 (($port-read! p) bv c0 max)])
            (unless (fixnum? c1)
              (error who "invalid return value from read! procedure" c1))
            (cond
              [(fx>= j 0)
               (unless (fx<= j max)
                 (error who "read! returned a value out of range" j))
               ($set-port-index! p c0)
               ($set-port-size! p (fx+ c1 c0))
               c1]
              [else 
               (error who "read! returned a value out of range" c1)])))))
    (define (get-char-latin-mode p who idx)
      (let ([n (refill-bv-start p who)])
        (cond
          [(fx= n 0) (eof-object)]
          [else 
           ($set-port-index! p idx)
           (integer->char (bytevector-u8-ref ($port-buffer p) 0))])))
    
    (define (get-char-utf8-mode p who)
      (define (do-error p who)
        (case (transcoder-error-handling-mode ($port-transcoder p))
          [(ignore)  (get-char p)]
          [(replace) #\xFFFD]
          [(raise)
           (raise (make-i/o-decoding-error p))]
          [else (error who "cannot happen")]))
      (let ([i ($port-index p)] 
            [j ($port-size p)]
            [buf ($port-buffer p)])
        (cond
          [(fx= i j) ;;; exhausted
           (let ([bytes (refill-bv-buffer p who)])
             (cond
               [(fx= bytes 0) (eof-object)]
               [else (get-char p)]))]
          [else
           (let ([b0 (bytevector-u8-ref buf i)])
             (cond
               [(fx= (fxsra b0 5) #b110) ;;; two-byte-encoding
                (let ([i (fx+ i 1)])
                  (cond
                    [(fx< i j) 
                     (let ([b1 (bytevector-u8-ref buf i)])
                       (cond
                         [(fx= (fxsra b1 6) #b10)
                          ($set-port-index! p (fx+ i 1))
                          (integer->char
                            (fxior (fxand b1 #b111111)
                                   (fxsll (fxand b0 #b11111) 6)))]
                         [else
                          ($set-port-index! p i)
                          (do-error p who)]))]
                    [else
                     (let ([bytes (refill-bv-buffer p who)])
                       (cond
                         [(fx= bytes 0) 
                          ($set-port-index! p (fx+ ($port-index p) 1))
                          (do-error p who)]
                         [else (get-char-utf8-mode p who)]))]))]
               [(fx= (fxsra b0 4) #b1110) ;;; three-byte-encoding
                (cond
                  [(fx< (fx+ i 2) j) 
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2) 6) #b10) 
                        (let ([n (fxlogor 
                                   (fxsll (fxand b0 #b1111) 12)
                                   (fxsll (fxand b1 #b111111) 6)
                                   (fxand b2 #b111111))])
                          (cond
                            [(and (fx<= #xD800 n) (fx<= n #xDFFF))
                             ($set-port-index! p (fx+ i 1))
                             (do-error p who)]
                            [else
                             ($set-port-index! p (fx+ i 3))
                             (integer->char n)]))]
                       [else
                        ($set-port-index! p (fx+ i 1))
                        (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0)
                        ($set-port-index! p (fx+ ($port-index p) 1))
                        (do-error p who)]
                       [else (get-char-utf8-mode p who)]))])]
               [(fx= (fxsra b0 3) #b11110) ;;; four-byte-encoding
                (cond
                  [(fx< (fx+ i 3) j) 
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))]
                         [b3 (bytevector-u8-ref buf (fx+ i 3))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2 b3) 6) #b10)
                        (let ([n (fxlogor 
                                   (fxsll (fxand b0 #b111) 18)
                                   (fxsll (fxand b1 #b111111) 12)
                                   (fxsll (fxand b2 #b111111) 6)
                                   (fxand b3 #b111111))])
                          (cond
                            [(and (fx<= #x10000 n) (fx<= n #x10FFFF))
                             ($set-port-index! p (fx+ i 4))
                             (integer->char n)]
                            [else
                             ($set-port-index! p (fx+ i 1))
                             (do-error p who)]))]
                       [else
                        ($set-port-index! p (fx+ i 1))
                        (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0)
                        ($set-port-index! p (fx+ ($port-index p) 1))
                        (do-error p who)]
                       [else (get-char-utf8-mode p who)]))])]
               [else 
                ($set-port-index! p (fx+ i 1))
                (do-error p who)]))])))
    
    (define (lookahead-char-utf8-mode p who)
      (define (do-error p who)
        (case (transcoder-error-handling-mode ($port-transcoder p))
          [(ignore)  (get-char p)]
          [(replace) #\xFFFD]
          [(raise)
           (raise (make-i/o-decoding-error p))]
          [else (error who "cannot happen")]))
      (let ([i ($port-index p)] 
            [j ($port-size p)]
            [buf ($port-buffer p)])
        (cond
          [(fx= i j) ;;; exhausted
           (let ([bytes (refill-bv-buffer p who)])
             (cond
               [(fx= bytes 0) (eof-object)]
               [else (lookahead-char p)]))]
          [else
           (let ([b0 (bytevector-u8-ref buf i)])
             (cond
               [(fx= (fxsra b0 5) #b110) ;;; two-byte-encoding
                (let ([i (fx+ i 1)])
                  (cond
                    [(fx< i j) 
                     (let ([b1 (bytevector-u8-ref buf i)])
                       (cond
                         [(fx= (fxsra b1 6) #b10)
                          (integer->char
                            (fxior (fxand b1 #b111111)
                                   (fxsll (fxand b0 #b11111) 6)))]
                         [else
                          (do-error p who)]))]
                    [else
                     (let ([bytes (refill-bv-buffer p who)])
                       (cond
                         [(fx= bytes 0) (do-error p who)]
                         [else (lookahead-char-utf8-mode p who)]))]))]
               [(fx= (fxsra b0 4) #b1110) ;;; three-byte-encoding
                (cond
                  [(fx< (fx+ i 2) j) 
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2) 6) #b10) 
                        (let ([n (fxlogor 
                                   (fxsll (fxand b0 #b1111) 12)
                                   (fxsll (fxand b1 #b111111) 6)
                                   (fxand b2 #b111111))])
                          (cond
                            [(and (fx<= #xD800 n) (fx<= n #xDFFF))
                             (do-error p who)]
                            [else (integer->char n)]))]
                       [else (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0) (do-error p who)]
                       [else (lookahead-char-utf8-mode p who)]))])]
               [(fx= (fxsra b0 3) #b11110) ;;; four-byte-encoding
                (cond
                  [(fx< (fx+ i 3) j) 
                   (let ([b1 (bytevector-u8-ref buf (fx+ i 1))]
                         [b2 (bytevector-u8-ref buf (fx+ i 2))]
                         [b3 (bytevector-u8-ref buf (fx+ i 3))])
                     (cond
                       [(fx= (fxsra (fxlogor b1 b2 b3) 6) #b10)
                        (let ([n (fxlogor 
                                   (fxsll (fxand b0 #b111) 18)
                                   (fxsll (fxand b1 #b111111) 12)
                                   (fxsll (fxand b2 #b111111) 6)
                                   (fxand b3 #b111111))])
                          (cond
                            [(and (fx<= #x10000 n) (fx<= n #x10FFFF))
                             (integer->char n)]
                            [else
                             (do-error p who)]))]
                       [else
                        (do-error p who)]))]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (cond
                       [(fx= bytes 0)
                        (do-error p who)]
                       [else (lookahead-char-utf8-mode p who)]))])]
               [else (do-error p who)]))])))

    (define (advance-bom p who bom-seq)
      ;;; return eof if port is eof, 
      ;;; #t if a bom is present, updating the port index to 
      ;;;    point just past the bom.
      ;;; #f otherwise. 
      (cond
        [(fx< ($port-index p) ($port-size p))
         (let f ([i 0] [ls bom-seq])
           (cond
             [(null? ls)
              ($set-port-index! p (fx+ ($port-index p) i))
              #t]
             [else
              (let ([idx (fx+ i ($port-index p))])
                (cond
                  [(fx< idx ($port-size p))
                   (if (fx=? (car ls)
                         (bytevector-u8-ref ($port-buffer p) idx))
                       (f (fx+ i 1) (cdr ls))
                       #f)]
                  [else
                   (let ([bytes (refill-bv-buffer p who)])
                     (if (fx= bytes 0)
                         #f
                         (f i ls)))]))]))]
        [else 
         (let ([bytes (refill-bv-buffer p who)])
           (if (fx= bytes 0)
               (eof-object)
               (advance-bom p who bom-seq)))]))

    (define (speedup-input-port p who)
      ;;; returns #t if port is eof, #f otherwise
      (unless (input-port? p) 
        (error who "not an input port" p))
      (let ([tr ($port-transcoder p)])
        (unless tr 
          (error who "not a textual port" p))
        (case (transcoder-codec tr)
          [(utf-8-codec)
           ;;;
           ($set-port-attrs! p 
             (fxior textual-input-port-bits fast-u7-text-tag))
           (eof-object? (advance-bom p who '(#xEF #xBB #xBF)))]
          [else (error 'slow-get-char "codec not handled")])))

    (define (lookahead-char-char-mode p who)
      (let ([str ($port-buffer p)]
            [read! ($port-read! p)])
        (let ([n (read! str 0 (string-length str))])
          (unless (fixnum? n) 
            (error who "invalid return value from read!" n))
          (unless (<= 0 n (string-length str))
            (error who "return value from read! is out of range" n))
          ($set-port-index! p 0) 
          ($set-port-size! p n)
          (cond
            [(fx= n 0)
             (eof-object)]
            [else
             (string-ref str 0)]))))
    ;;;
    (define (lookahead-char p)
      (define who 'lookahead-char)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-utf8-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (let ([b (bytevector-u8-ref ($port-buffer p) i)])
                  (cond
                    [(fx< b 128) (integer->char b)]
                    [else (lookahead-char-utf8-mode p who)]))]
               [else
                (lookahead-char-utf8-mode p who)]))]
          [(eq? m fast-get-char-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (string-ref ($port-buffer p) i)]
               [else
                (lookahead-char-char-mode p who)]))]
          [(eq? m fast-get-latin-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (integer->char 
                  (bytevector-u8-ref ($port-buffer p) i))]
               [else
                (get-char-latin-mode p who 0)]))]
          [else 
           (if (speedup-input-port p who)
               (eof-object)
               (lookahead-char p))])))
    ;;;
    (define (get-char-char-mode p who)
      (let ([str ($port-buffer p)]
            [read! ($port-read! p)])
        (let ([n (read! str 0 (string-length str))])
          (unless (fixnum? n) 
            (error who "invalid return value from read!" n))
          (unless (<= 0 n (string-length str))
            (error who "return value from read! is out of range" n))
          ($set-port-size! p n)
          (cond
            [(fx= n 0)
             ($set-port-index! p 0)
             (eof-object)]
            [else
             ($set-port-index! p 1) 
             (string-ref str 0)]))))

    (define (get-char p)
      (define who 'get-char)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-utf8-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (let ([b (bytevector-u8-ref ($port-buffer p) i)])
                  (cond
                    [(fx< b 128) 
                     ($set-port-index! p (fx+ i 1))
                     (integer->char b)]
                    [else (get-char-utf8-mode p who)]))]
               [else
                (get-char-utf8-mode p who)]))]
          [(eq? m fast-get-char-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (string-ref ($port-buffer p) i)]
               [else (get-char-char-mode p who)]))]
          [(eq? m fast-get-latin-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (integer->char 
                  (bytevector-u8-ref ($port-buffer p) i))]
               [else
                (get-char-latin-mode p who 1)]))]
          [else 
           (if (speedup-input-port p who)
               (eof-object)
               (get-char p))]))))

  ;;; ----------------------------------------------------------
  (define (assert-binary-input-port p who)
    (unless (port? p) (error who "not a port" p))
    (when ($port-closed? p) (error who "port is closed" p))
    (when ($port-transcoder p) (error who "port is not binary" p))
    (unless ($port-read! p)
      (error who "port is not an input port" p)))

  (module (get-u8 lookahead-u8)
    (import UNSAFE)
    (define (get-u8-byte-mode p who start) 
      (when ($port-closed? p) (error who "port is closed" p))
      (let* ([bv ($port-buffer p)]
             [n (bytevector-length bv)])
        (let ([j (($port-read! p) bv 0 n)])
          (unless (fixnum? j)
            (error who "invalid return value from read! procedure" j))
          (cond
            [(fx> j 0) 
             (unless (fx<= j n)
               (error who "read! returned a value out of range" j))
             ($set-port-index! p start) 
             ($set-port-size! p j)
             (bytevector-u8-ref bv 0)]
            [(fx= j 0) (eof-object)]
            [else 
             (error who "read! returned a value out of range" j)]))))
    (define (slow-get-u8 p who start) 
      (assert-binary-input-port p who)
      ($set-port-attrs! p fast-get-byte-tag)
      (get-u8-byte-mode p who start))
    ;;;
    (define (get-u8 p)
      (define who 'get-u8)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-byte-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (bytevector-u8-ref ($port-buffer p) i)]
               [else (get-u8-byte-mode p who 1)]))]
          [else (slow-get-u8 p who 1)])))
    (define (lookahead-u8 p)
      (define who 'lookahead-u8)
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-get-byte-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (bytevector-u8-ref ($port-buffer p) i)]
               [else (get-u8-byte-mode p who 0)]))]
          [else (slow-get-u8 p who 0)]))))

  (define (port-eof? p)
    (import UNSAFE)
    (define who 'port-eof?)
    (let ([m ($port-fast-attrs p)])
      (cond
        [(not (eq? m 0))
         (if (fx< ($port-index p) ($port-size p))
             #f
             (if ($port-transcoder p) 
                 (eof-object? (lookahead-char p))
                 (eof-object? (lookahead-u8 p))))]
        [(input-port? p)
         (when ($port-closed? p) 
           (error 'port-eof? "port is closed" p))
         (if (textual-port? p) 
             (eof-object? (lookahead-char p))
             (eof-object? (lookahead-u8 p)))]
        [else (error 'port-eof? "not an input port" p)])))

  (define io-errors-vec
    '#("unknown error"
       "bad file name"
       "operation interrupted"
       "not a directory"
       "file name too long"
       "missing entities"
       "insufficient access privileges"
       "circular path"
       "file is a directory"
       "file system is read-only"
       "maximum open files reached"
       "maximum open files reached"
       "ENXIO"
       "operation not supported"
       "not enough space on device"
       "quota exceeded"
       "io error"
       "device is busy"
       "access fault"
       "file already exists"
       "invalid file name"))

  (define (io-error who id err)
    (let ([msg
           (let ([err (fxnot err)])
             (cond
               [(fx< err (vector-length io-errors-vec))
                (vector-ref io-errors-vec err)]
               [else "unknown error"]))])
      (raise 
        (condition
          (make-who-condition who)
          (make-message-condition msg)
          (make-i/o-filename-error id)))))

  (define block-size 4096)
  (define input-file-buffer-size (+ block-size 128))
  (define output-file-buffer-size block-size)

  (define (fh->input-port fd id size transcoder close?)
    (guarded-port
      ($make-port 
        (input-transcoder-attrs transcoder)
        0 0 (make-bytevector size)
        transcoder
        id
        (lambda (bv idx cnt) 
          (let ([bytes
                 (foreign-call "ikrt_read_fd" fd bv idx 
                    (fxmin block-size cnt))])
            (when (fx< bytes 0) (io-error 'read id bytes))
            bytes))
        #f ;;; write!
        #f ;;; get-position
        #f ;;; set-position!
        (and close?
          (lambda () 
            (cond
              [(foreign-call "ikrt_close_fd" fd) =>
               (lambda (err) 
                 (io-error 'close id err))])))
        fd)))

  (define (fh->output-port fd id size transcoder close?)
    (guarded-port
      ($make-port 
        (output-transcoder-attrs transcoder)
        0 size (make-bytevector size)
        transcoder
        id
        #f
        (lambda (bv idx cnt) 
          (let ([bytes
                 (foreign-call "ikrt_write_fd" fd bv idx
                   (fxmin block-size cnt))])
            (when (fx< bytes 0) (io-error 'write id bytes))
            bytes))
        #f ;;; get-position
        #f ;;; set-position!
        (and close?
          (lambda () 
            (cond
              [(foreign-call "ikrt_close_fd" fd) =>
               (lambda (err) 
                 (io-error 'close id err))])))
        fd)))

  (define (open-input-file-handle filename who)
    (let ([fh (foreign-call "ikrt_open_input_fd"
                 (string->utf8 filename))])
      (cond
        [(fx< fh 0) (io-error who filename fh)]
        [else fh])))
  
  (define (open-output-file-handle filename file-options who)
    (let ([opt (case file-options
                 [(fo:default)                       0]
                 [(fo:no-create)                     1]
                 [(fo:no-fail)                       2]
                 [(fo:no-fail/no-create)             3]
                 [(fo:no-truncate)                   4]
                 [(fo:no-truncate/no-create)         5]
                 [(fo:no-truncate/no-fail)           6]
                 [(fo:no-truncate/no-fail/no-create) 7]
                 [else (error who "invalid file option" file-options)])])
      (let ([fh (foreign-call "ikrt_open_output_fd"
                   (string->utf8 filename)
                   opt)])
        (cond
          [(fx< fh 0) (io-error who filename fh)]
          [else fh]))))

  (define open-file-input-port
    (case-lambda
      [(filename) 
       (open-file-input-port filename (file-options) 'block #f)]
      [(filename file-options) 
       (open-file-input-port filename file-options 'block #f)]
      [(filename file-options buffer-mode) 
       (open-file-input-port filename file-options buffer-mode #f)]
      [(filename file-options buffer-mode transcoder)
       (unless (string? filename)
         (error 'open-file-input-port "invalid filename" filename))
       (unless (or (not transcoder) (transcoder? transcoder)) 
         (error 'open-file-input-port "invalid transcoder" transcoder))
       ; FIXME: file-options ignored
       ; FIXME: buffer-mode ignored
       (fh->input-port 
         (open-input-file-handle filename 'open-file-input-port)
         filename
         input-file-buffer-size
         transcoder
         #t)]))


  (define open-file-output-port
    (case-lambda
      [(filename) 
       (open-file-output-port filename (file-options) 'block #f)]
      [(filename file-options) 
       (open-file-output-port filename file-options 'block #f)]
      [(filename file-options buffer-mode) 
       (open-file-output-port filename file-options buffer-mode #f)]
      [(filename file-options buffer-mode transcoder)
       (unless (string? filename)
         (error 'open-file-output-port "invalid filename" filename))
       ; FIXME: file-options ignored
       ; FIXME: buffer-mode ignored
       (unless (or (not transcoder) (transcoder? transcoder)) 
         (error 'open-file-output-port "invalid transcoder" transcoder))
       (fh->output-port
         (open-output-file-handle filename file-options 
            'open-file-output-port)
         filename
         output-file-buffer-size
         transcoder
         #t)]))

  (define (open-output-file filename)
    (unless (string? filename)
      (error 'open-output-file "invalid filename" filename))
    (fh->output-port 
       (open-output-file-handle filename (file-options) 
          'open-input-file)
       filename
       output-file-buffer-size
       (native-transcoder)
       #t))

  (define (open-input-file filename)
    (unless (string? filename)
      (error 'open-input-file "invalid filename" filename))
    (fh->input-port 
       (open-input-file-handle filename 'open-input-file) 
       filename
       input-file-buffer-size
       (native-transcoder)
       #t))

  (define (call-with-input-file filename proc)
    (unless (string? filename)
      (error 'call-with-input-file "invalid filename" filename))
    (unless (procedure? proc)
      (error 'call-with-input-file "not a procedure" proc))
    (call-with-port
      (fh->input-port 
        (open-input-file-handle filename 'call-with-input-file) 
        filename
        input-file-buffer-size
        (native-transcoder)
        #t)
      proc))

  (define (with-input-from-file filename proc)
    (unless (string? filename)
      (error 'with-input-from-file "invalid filename" filename))
    (unless (procedure? proc)
      (error 'with-input-from-file "not a procedure" proc))
    (let ([p
           (fh->input-port 
             (open-input-file-handle filename 'with-input-from-file)
             filename
             input-file-buffer-size
             (native-transcoder)
             #t)])
      (parameterize ([*the-input-port* p])
        (proc))))

  (define (standard-input-port) 
    (fh->input-port 0 '*stdin* 256 #f #f))

  (define (standard-output-port)
    (fh->output-port 1 '*stdout* 256 #f #f))
  
  (define (standard-error-port)
    (fh->output-port 2 '*stderr* 256 #f #f))

  (define *the-input-port* 
    (make-parameter
      (transcoded-port 
        (fh->input-port 0 '*stdin* input-file-buffer-size #f #f)
        (native-transcoder))))

  (define *the-output-port* 
    (make-parameter
      (transcoded-port
        (fh->output-port 1 '*stdout* output-file-buffer-size #f #f)
        (native-transcoder))))
  
  (define *the-error-port* 
    (make-parameter
      (transcoded-port 
        (fh->output-port 2 '*stderr* output-file-buffer-size #f #f)
        (native-transcoder))))

  (define console-output-port
    (let ([p (*the-output-port*)])
      (lambda () p)))

  (define console-error-port
    (let ([p (*the-error-port*)])
      (lambda () p)))

  (define console-input-port
    (let ([p (*the-input-port*)])
      (lambda () p)))

  (define (current-input-port) (*the-input-port*))
  (define (current-output-port) (*the-output-port*))
  (define (current-error-port) (*the-error-port*))

  (define (call-with-port p proc)
    (if (port? p) 
        (if (procedure? proc) 
            (dynamic-wind
              void
              (lambda () (proc p))
              (lambda () (close-port p)))
            (error 'call-with-port "not a procedure" proc))
        (error 'call-with-port "not a port" p)))

  (define read-char
    (case-lambda
      [() (get-char (*the-input-port*))]
      [(p)
       (if (input-port? p)
           (if (textual-port? p)
               (get-char p)
               (error 'read-char "not a textual port" p))
           (error 'read-char "not an input-port" p))]))
  ;;;
  (define peek-char
    (case-lambda
      [() (lookahead-char (*the-input-port*))]
      [(p)
       (if (input-port? p)
           (if (textual-port? p)
               (lookahead-char p)
               (error 'peek-char "not a textual port" p))
           (error 'peek-char "not an input-port" p))]))

  (define (get-bytevector-n p n) 
    (import (ikarus system $fx) (ikarus system $bytevectors))
    (define (subbytevector s n)
      (let ([p ($make-bytevector n)])
        (let f ([s s] [n n] [p p])
          (let ([n ($fx- n 1)])
            ($bytevector-set! p n ($bytevector-u8-ref s n))
            (if ($fx= n 0) 
                p
                (f s n p))))))
    (unless (input-port? p) 
      (error 'get-bytevector-n "not an input port" p))
    (unless (binary-port? p)
      (error 'get-bytevector-n "not a binary port" p))
    (unless (fixnum? n) 
      (error 'get-bytevector-n "count is not a fixnum" n))
    (cond
      [($fx> n 0) 
       (let ([s ($make-bytevector n)])
         (let f ([p p] [n n] [s s] [i 0])
           (let ([x (get-u8 p)])
             (cond
               [(eof-object? x) 
                (if ($fx= i 0) 
                    (eof-object)
                    (subbytevector s i))]
               [else
                ($bytevector-set! s i x)
                (let ([i ($fxadd1 i)])
                  (if ($fx= i n) 
                      s
                      (f p n s i)))]))))]
      [($fx= n 0) '#vu8()]
      [else (error 'get-bytevector-n "count is negative" n)]))

  (define (get-bytevector-n! p s i c) 
    (import (ikarus system $fx) (ikarus system $bytevectors))
    (unless (input-port? p) 
      (error 'get-bytevector-n! "not an input port" p))
    (unless (binary-port? p)
      (error 'get-bytevector-n! "not a binary port" p))
    (unless (bytevector? s) 
      (error 'get-bytevector-n! "not a bytevector" s))
    (let ([len ($bytevector-length s)])
      (unless (fixnum? i) 
        (error 'get-bytevector-n! "starting index is not a fixnum" i))
      (when (or ($fx< i 0) ($fx> i len))
        (error 'get-bytevector-n! 
          (format "starting index is out of range 0..~a" len)
          i))
      (unless (fixnum? c)
        (error 'get-bytevector-n! "count is not a fixnum" c))
      (cond
        [($fx> c 0)
         (let ([j (+ i c)])
           (when (> j len)
             (error 'get-bytevector-n! 
               (format "count is out of range 0..~a" (- len i))
               c))
           (let ([x (get-u8 p)])
             (cond
               [(eof-object? x) x]
               [else
                ($bytevector-set! s i x)
                (let f ([p p] [s s] [start i] [i 1] [c c])
                  (let ([x (get-u8 p)])
                    (cond
                      [(eof-object? x) i]
                      [else
                       ($bytevector-set! s ($fx+ start i) x)
                       (let ([i ($fxadd1 i)])
                         (if ($fx= i c)
                             i
                             (f p s start i c)))])))])))]
        [($fx= c 0) 0]
        [else (error 'get-bytevector-n! "count is negative" c)])))

  (define-rrr get-bytevector-some)

  (define (get-bytevector-all p)
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x (get-u8 p)])
          (cond
            [(eof-object? x)
             (if (null? ac) 
                 (eof-object)
                 (make-it n ac))]
            [else (f p (+ n 1) (cons x ac))]))))
    (define (make-it n revls)
      (let f ([s (make-bytevector n)] [i (- n 1)] [ls revls])
        (cond
          [(pair? ls)
           (bytevector-u8-set! s i (car ls))
           (f s (- i 1) (cdr ls))]
          [else s])))
    (if (input-port? p)
        (if (binary-port? p)
            (get-it p)
            (error 'get-bytevector-all "not a binary port" p))
        (error 'get-bytevector-all "not an input port" p)))

  (define (get-string-n p n) 
    (import (ikarus system $fx) (ikarus system $strings))
    (unless (input-port? p) 
      (error 'get-string-n "not an input port" p))
    (unless (textual-port? p) 
      (error 'get-string-n "not a textual port" p))
    (unless (fixnum? n) 
      (error 'get-string-n "count is not a fixnum" n))
    (cond
      [($fx> n 0) 
       (let ([s ($make-string n)])
         (let f ([p p] [n n] [s s] [i 0])
           (let ([x (get-char p)])
             (cond
               [(eof-object? x) 
                (if ($fx= i 0) 
                    (eof-object)
                    (substring s 0 i))]
               [else
                ($string-set! s i x) 
                (let ([i ($fxadd1 i)])
                  (if ($fx= i n) 
                      s
                      (f p n s i)))]))))]
      [($fx= n 0) ""]
      [else (error 'get-string-n "count is negative" n)]))

  (define (get-string-n! p s i c) 
    (import (ikarus system $fx) (ikarus system $strings))
    (unless (input-port? p) 
      (error 'get-string-n! "not an input port" p))
    (unless (textual-port? p) 
      (error 'get-string-n! "not a textual port" p))
    (unless (string? s) 
      (error 'get-string-n! "not a string" s))
    (let ([len ($string-length s)])
      (unless (fixnum? i) 
        (error 'get-string-n! "starting index is not a fixnum" i))
      (when (or ($fx< i 0) ($fx> i len))
        (error 'get-string-n! 
          (format "starting index is out of range 0..~a" len)
          i))
      (unless (fixnum? c) 
        (error 'get-string-n! "count is not a fixnum" c))
      (cond
        [($fx> c 0)
         (let ([j (+ i c)])
           (when (> j len)
             (error 'get-string-n! 
               (format "count is out of range 0..~a" (- len i))
               c))
           (let ([x (get-char p)])
             (cond
               [(eof-object? x) x]
               [else
                ($string-set! s i x)
                (let f ([p p] [s s] [start i] [i 1] [c c])
                  (let ([x (get-char p)])
                    (cond
                      [(eof-object? x) i]
                      [else
                       ($string-set! s ($fx+ start i) x)
                       (let ([i ($fxadd1 i)])
                         (if ($fx= i c)
                             i
                             (f p s start i c)))])))])))]
        [($fx= c 0) 0]
        [else (error 'get-string-n! "count is negative" c)])))

  (define (get-line p)
    (import UNSAFE)
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x (get-char p)])
          (cond
            [(eqv? x #\newline) 
             (make-it n ac)]
            [(eof-object? x) 
             (if (null? ac) x (make-it n ac))]
            [else (f p (fx+ n 1) (cons x ac))]))))
    (define (make-it n revls)
      (let f ([s (make-string n)] [i (fx- n 1)] [ls revls])
        (cond
          [(pair? ls)
           (string-set! s i (car ls))
           (f s (fx- i 1) (cdr ls))]
          [else s])))
    (if (input-port? p)
        (if (textual-port? p)
            (get-it p)
            (error 'get-line "not a textual port" p))
        (error 'get-line "not an input port" p)))


  (define (get-string-all p)
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x (get-char p)])
          (cond
            [(eof-object? x)
             (if (null? ac) 
                 (eof-object)
                 (make-it n ac))]
            [else (f p (+ n 1) (cons x ac))]))))
    (define (make-it n revls)
      (let f ([s (make-string n)] [i (- n 1)] [ls revls])
        (cond
          [(pair? ls)
           (string-set! s i (car ls))
           (f s (- i 1) (cdr ls))]
          [else s])))
    (if (input-port? p)
        (if (textual-port? p)
            (get-it p)
            (error 'get-string-all "not a textual port" p))
        (error 'get-string-all "not an input port" p)))



  ;;; ----------------------------------------------------------
  
  (module (put-char write-char put-string)
    (import UNSAFE)
    (define (put-char-utf8-mode p b who) 
      (cond
        [(fx< b 128) 
         (flush-output-port p) 
         (let ([i ($port-index p)] [j ($port-size p)])
           (cond
             [(fx< i j) 
              (bytevector-u8-set! ($port-buffer p) i b) 
              ($set-port-index! p (fx+ i 1))]
             [else
              (error who "insufficient space on port" p)]))]
        [(fx<= b #x7FF) 
         (let ([i ($port-index p)] 
               [j ($port-size p)]
               [buf ($port-buffer p)])
           (cond
             [(fx< (fx+ i 1) j) 
              (bytevector-u8-set! buf i 
                (fxior #b11000000 (fxsra b 6)))
              (bytevector-u8-set! buf (fx+ i 1) 
                (fxior #b10000000 (fxand b #b111111)))
              ($set-port-index! p (fx+ i 2))]
             [else 
              (flush-output-port p) 
              (put-char-utf8-mode p b who)]))]
        [(fx<= b #xFFFF) 
         (let ([i ($port-index p)] 
               [j ($port-size p)]
               [buf ($port-buffer p)])
           (cond
             [(fx< (fx+ i 2) j) 
              (bytevector-u8-set! buf i 
                (fxior #b11100000 (fxsra b 12)))
              (bytevector-u8-set! buf (fx+ i 1) 
                (fxior #b10000000 (fxand (fxsra b 6) #b111111)))
              (bytevector-u8-set! buf (fx+ i 2) 
                (fxior #b10000000 (fxand b #b111111)))
              ($set-port-index! p (fx+ i 3))]
             [else 
              (flush-output-port p) 
              (put-char-utf8-mode p b who)]))]
        [else
         (let ([i ($port-index p)] 
               [j ($port-size p)]
               [buf ($port-buffer p)])
           (cond
             [(fx< (fx+ i 3) j) 
              (bytevector-u8-set! buf i 
                (fxior #b11110000 (fxsra b 18)))
              (bytevector-u8-set! buf (fx+ i 1)
                (fxior #b10000000 (fxand (fxsra b 12) #b111111)))
              (bytevector-u8-set! buf (fx+ i 2)
                (fxior #b10000000 (fxand (fxsra b 6) #b111111)))
              (bytevector-u8-set! buf (fx+ i 3) 
                (fxior #b10000000 (fxand b #b111111)))
              ($set-port-index! p (fx+ i 4))]
             [else 
              (flush-output-port p) 
              (put-char-utf8-mode p b who)]))]))
    (define (put-char-latin-mode p b who) 
      (cond
        [(fx< b 256) 
         (flush-output-port p)
         (let ([i ($port-index p)] [j ($port-size p)])
           (cond
             [(fx< i j) 
              (bytevector-u8-set! ($port-buffer p) i b)
              ($set-port-index! p (fx+ i 1))]
             [else 
              (error who "insufficient space in port" p)]))]
        [else
         (case (transcoder-error-handling-mode (port-transcoder p))
           [(ignore) (void)]
           [(replace) (put-char p #\?)]
           [(raise) 
            (raise (make-i/o-encoding-error p))]
           [else (error who "BUG: invalid error handling mode" p)])]))
    (define (put-char-char-mode p c who)
      (flush-output-port p)
      (let ([i ($port-index p)] [j ($port-size p)])
        (cond
          [(fx< i j) 
           (string-set! ($port-buffer p) i c)
           ($set-port-index! p (fx+ i 1))]
          [else 
           (error who "insufficient space in port" p)])))
    ;;;
    (define write-char
      (case-lambda
        [(c p) (do-put-char p c 'write-char)]
        [(c) (do-put-char (*the-output-port*) c 'write-char)]))
    (define (put-char p c) 
      (do-put-char p c 'put-char))
    (define (put-string p str)
      (unless (string? str) (error 'put-string "not a string" str))
      (unless (output-port? p)
        (error 'put-string "not an output port" p))
      (unless (textual-port? p)
        (error 'put-string "not a textual port" p))
      (let f ([i 0] [n (string-length str)])
        (unless (fx= i n)
          (do-put-char p (string-ref str i) 'put-string)
          (f (fx+ i 1) n))))
    (define (do-put-char p c who)
      (unless (char? c) (error who "not a char" c))
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-put-utf8-tag)
           (let ([i ($port-index p)])
             (let ([b (char->integer c)])
               (cond
                 [(and (fx< i ($port-size p)) (fx< b 128))
                  (bytevector-u8-set! ($port-buffer p) i b)
                  ($set-port-index! p (fx+ i 1))]
                 [else
                  (put-char-utf8-mode p b who)])))]
          [(eq? m fast-put-char-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (string-set! ($port-buffer p) i c)]
               [else
                (put-char-char-mode p c who)]))]
          [(eq? m fast-put-latin-tag)
           (let ([i ($port-index p)])
             (let ([b (char->integer c)])
               (cond
                 [(and (fx< i ($port-size p)) (fx< b 256))
                  (bytevector-u8-set! ($port-buffer p) i b)
                  ($set-port-index! p (fx+ i 1))]
                 [else
                  (put-char-latin-mode p b who)])))]
          [else 
           (if (output-port? p)
               (error who "not a textual port" p)
               (error who "not an output port" p))]))))

  (define newline
    (case-lambda
      [() 
       (put-char (*the-output-port*) #\newline)
       (flush-output-port (*the-output-port*))]
      [(p)
       (unless (output-port? p) 
         (error 'newline "not an output port" p))
       (unless (textual-port? p) 
         (error 'newline "not a textual port" p))
       (when ($port-closed? p) 
         (error 'newline "port is closed" p))
       (put-char p #\newline)
       (flush-output-port p)]))
       

       
  (module (put-u8)
    (import UNSAFE)
    (define (put-u8-byte-mode p b who)
      (let ([write! ($port-write! p)])
        (let ([i ($port-index p)] 
              [buf ($port-buffer p)])
          (let ([bytes (write! buf 0 i)])
            (when (or (not (fixnum? bytes))
                      (fx< bytes 0)
                      (fx> bytes i))
              (error who "write! returned an invalid value" bytes))
            (cond
              [(fx= bytes i) 
               (bytevector-u8-set! buf 0 b)
               ($set-port-index! p 1)]
              [(fx= bytes 0) 
               (error who "could not write bytes to sink")]
              [else
               (let ([i (fx- i bytes)])
                 (bytevector-copy! buf bytes buf 0 i)
                 (bytevector-u8-set! buf i b)
                 ($set-port-index! p (fx+ i 1)))])))))
    ;;;
    (define (put-u8 p b)
      (define who 'put-u8)
      (unless (u8? b) (error who "not a u8" b))
      (let ([m ($port-fast-attrs p)])
        (cond
          [(eq? m fast-put-byte-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (bytevector-u8-set! ($port-buffer p) i b)]
               [else
                (put-u8-byte-mode p b who)]))]
          [else 
           (if (output-port? p)
               (error who "not a binary port" p)
               (error who "not an output port" p))]))))


  )

