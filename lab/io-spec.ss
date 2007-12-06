
(library (io-spec) 
  (export 
    input-port? textual-port? port-eof?
    open-bytevector-input-port
    make-custom-binary-input-port 
    get-char lookahead-char get-u8 lookahead-u8 close-port
    transcoded-port)
  (import 
    (except (ikarus)
      input-port? textual-port? port-eof?
      open-bytevector-input-port
      make-custom-binary-input-port
      get-char lookahead-char get-u8 lookahead-u8 close-port
      transcoded-port))

  (define-struct $port 
    (index size buffer base-index codec closed? attrs 
     id read! write! get-position set-position! close))
  (define $set-port-index! set-$port-index!)
  (define $set-port-size! set-$port-size!)
  (define $set-port-attrs! set-$port-attrs!)
  (define $set-port-closed?! set-$port-closed?!)
  (define $make-port make-$port)

  (define fast-get-tag           #x0001)
  (define fast-put-tag           #x0002)
  (define fast-get-position-tag  #x0004)

  (define fast-get-mask          #x00F0)
  (define fast-get-utf8-tag      #x0010)
  (define fast-get-latin-tag     #x0030)
  (define fast-get-byte-tag      #x0040)
  (define fast-get-char-tag      #x0080)

  (define fast-put-mask          #x0F00)
  (define fast-put-utf8-tag      #x0100)
  (define fast-put-latin-tag     #x0300)
  (define fast-put-byte-tag      #x0400)
  (define fast-put-char-tag      #x0800)
  
  (define r6rs-mode-tag          #x1000)

  (define ($port-get-mode x)
    (and ($port? x) (fxand ($port-attrs x) fast-get-mask)))

  (define ($port-put-mode x)
    (and ($port? x) (fxand ($port-attrs x) fast-put-mask)))

  (define (u8? x)
    (and (fixnum? x) (fx>= x 0) (fx< x 256)))

  ;;; everything above this line will turn into primitive
  ;;; ----------------------------------------------------------
  


  (define ($make-custom-binary-input-port id 
            read! get-position set-position! close buffer-size)
    (let ([bv (make-bytevector buffer-size)])
      ($make-port 0 0 bv 0 #f #f 0 id read! #f get-position
                  set-position! close)))

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
    ($make-custom-binary-input-port id read! get-position
      set-position! close 256))

  (define (input-transcoder-attrs x)
    (cond
      [(not x) ;;; binary input port
       (fxior fast-get-tag fast-get-byte-tag)]
      [(and (eq? 'latin-1-codec (transcoder-codec x))
            (eq? 'none (transcoder-eol-style x)))
       (fxior fast-get-tag fast-get-latin-tag)]
      [else 0]))


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
       ($make-port 0 (bytevector-length bv) bv 0 
          maybe-transcoder 
          #f ;;; closed?
          (input-transcoder-attrs maybe-transcoder)
          "*bytevector-input-port*" 
          (lambda (bv i c) 0) ;;; read!
          #f ;;; write!
          #f ;;; FIXME: get-position
          #f ;;; FIXME: set-position!
          #f ;;; close
          )]))

  (define (transcoded-port p transcoder)
    (define who 'transcoded-port)
    (unless (transcoder? transcoder)
      (error who "not a transcoder" transcoder))
    (unless ($port? p) (error who "not a port" p))
    (when ($port-codec p) (error who "not a binary port" p))
    (let ([read! ($port-read! p)]
          [closed? ($port-closed? p)])
      ($set-port-closed?! p #t)
      ($make-port 
        ($port-index p)
        ($port-size p)
        ($port-buffer p)
        ($port-base-index p)
        transcoder
        closed? 
        (if read! (input-transcoder-attrs transcoder) 0)
        ($port-id p)
        read!
        ($port-write! p)
        ($port-get-position p)
        ($port-set-position! p)
        ($port-close p))))



  (define (input-port? p) 
    (and ($port? p) 
         ($port-read! p) 
         #t))

  (define (textual-port? p) 
    (and ($port? p) 
         ($port-codec p) 
         #t))

  (define (close-port p)
    (cond
      [(not ($port? p)) 
       (error 'close-port "not a port" p)]
      [($port-closed? p) (void)]
      [else
       (when ($port-write! p)
         (flush-output-port p))
       ($set-port-closed?! p #t)
       (let ([close ($port-close p)])
         (when (procedure? close)
           (close)))]))

  (define-syntax define-rrr
    (syntax-rules ()
      [(_ name)
       (define (name . args) 
         (error 'name "not implemented" args))]))

  ;;; ----------------------------------------------------------
  (module (get-char lookahead-char)
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
    (define (get-char-latin-mode p who idx)
      (let ([n (refill-bv-start p who)])
        (cond
          [(fx= n 0) (eof-object)]
          [else 
           ($set-port-index! p idx)
           (integer->char (bytevector-u8-ref ($port-buffer p) 0))])))
    (define-rrr get-char-utf8-mode)
    (define-rrr get-char-char-mode)
    (define-rrr slow-get-char)
    (define-rrr slow-lookahead-char)
    (define-rrr lookahead-char-utf8-mode)
    (define-rrr lookahead-char-char-mode)
    ;;;
    (define (lookahead-char p)
      (define who 'lookahead-char)
      (let ([m ($port-get-mode p)])
        (cond
          [(eq? m fast-get-utf8-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (let ([b (bytevector-u8-ref ($port-buffer p) i)])
                  (cond
                    [(fx< b 128) (integer->char b)]
                    [else (lookahead-char-utf8-mode p)]))]
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
          [else (slow-lookahead-char p who)])))
    ;;;
    (define (get-char p)
      (define who 'get-char)
      (let ([m ($port-get-mode p)])
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
                    [else (get-char-utf8-mode p)]))]
               [else
                (get-char-utf8-mode p who)]))]
          [(eq? m fast-get-char-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (string-ref ($port-buffer p) i)]
               [else
                (get-char-char-mode p who)]))]
          [(eq? m fast-get-latin-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (integer->char 
                  (bytevector-u8-ref ($port-buffer p) i))]
               [else
                (get-char-latin-mode p who 1)]))]
          [else (slow-get-char p who)]))))

  ;;; ----------------------------------------------------------
  (define (assert-binary-input-port p who)
    (unless ($port? p) (error who "not a port" p))
    (when ($port-closed? p) (error who "port is closed" p))
    (when ($port-codec p) (error who "port is not binary" p))
    (unless ($port-read! p)
      (error who "port is not an input port" p)))

  (module (get-u8 lookahead-u8)
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
      ($set-port-attrs! p (fxior fast-get-tag fast-get-byte-tag))
      (get-u8-byte-mode p who start))
    ;;;
    (define (get-u8 p)
      (define who 'get-u8)
      (let ([m ($port-get-mode p)])
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
      (let ([m ($port-get-mode p)])
        (cond
          [(eq? m fast-get-byte-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                (bytevector-u8-ref ($port-buffer p) i)]
               [else (get-u8-byte-mode p who 0)]))]
          [else (slow-get-u8 p who 0)]))))

  (define (port-eof? p)
    (define who 'port-eof?)
    (let ([m ($port-get-mode p)])
      (cond
        [(not (eq? m 0))
         (if (fx< ($port-index p) ($port-size p))
             #f
             (if ($port-codec p) 
                 (eof-object? (lookahead-char p))
                 (eof-object? (lookahead-u8 p))))]
        [(input-port? p)
         (when ($port-closed? p) 
           (error 'port-eof? "port is closed" p))
         (if (textual-port? p) 
             (eof-object? (lookahead-char p))
             (eof-object? (lookahead-u8 p)))]
        [else (error 'port-eof? "not an input port" p)])))



  )



#!eof
 
  ;;; ----------------------------------------------------------
  ;;; do input ports first.
  
  ;;; ----------------------------------------------------------
  (module (put-char)
    (define-rrr put-char-utf8-mode)
    (define-rrr put-char-latin-mode)
    (define-rrr put-char-char-mode)
    (define-rrr slow-put-char)
    ;;;
    (define (put-char p c)
      (define who 'put-char)
      (unless (char? c) (error who "not a char" c))
      (let ([m ($port-put-mode p)])
        (cond
          [(eq? m fast-put-utf8-tag)
           (let ([i ($port-index p)])
             (let ([b (char->integer c)])
               (cond
                 [(and (fx< i ($port-size p)) (fx< b 128))
                  ($set-port-index! p (fx+ i 1))
                  (bytevector-u8-set! ($port-buffer p) i b)]
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
                  ($set-port-index! p (fx+ i 1))
                  (bytevector-u8-set! ($port-buffer p) i b)]
                 [else
                  (put-char-latin-mode p b who)])))]
          [else (slow-put-char p c who)]))))

  (module (put-u8)
    (define-rrr put-u8-byte-mode)
    (define-rrr slow-put-u8)
    ;;;
    (define (put-u8 p b)
      (define who 'put-u8)
      (unless (u8? b) (error who "not a u8" b))
      (let ([m ($port-put-mode p)])
        (cond
          [(eq? m fast-put-byte-tag)
           (let ([i ($port-index p)])
             (cond
               [(fx< i ($port-size p))
                ($set-port-index! p (fx+ i 1))
                (bytevector-u8-set! ($port-buffer p) i b)]
               [else
                (put-u8-byte-mode p b who)]))]
          [else (slow-put-u8 p b who)]))))
