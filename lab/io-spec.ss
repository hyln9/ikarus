
(library (io-spec) 
  
  (export 
    port? input-port? output-port? textual-port? binary-port?
    open-file-input-port open-input-file 
    call-with-input-file with-input-from-file
    standard-input-port current-input-port
    open-bytevector-input-port
    open-string-input-port
    make-custom-binary-input-port 
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
    )

  
  (import 
    (except (ikarus)
      port? input-port? output-port? textual-port? binary-port? 
      open-file-input-port open-input-file 
      call-with-input-file with-input-from-file
      standard-input-port current-input-port
      open-bytevector-input-port
      open-string-input-port
      make-custom-binary-input-port
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
      ))

  (define-syntax define-rrr
    (syntax-rules ()
      [(_ name)
       (define (name . args) 
         (apply error 'name "not implemented" args))]))

  (define-struct $port 
    (index size buffer base-index transcoder closed? attrs 
     id read! write! get-position set-position! close))
  (define port? $port?)
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

  (define (open-string-input-port str)
    (unless (string? str) 
      (error 'open-string-input-port str))
    ($make-port 0 (string-length str) str 0 
       #t 
       #f ;;; closed?
       (fxior fast-get-tag fast-get-char-tag)
       "*string-input-port*" 
       (lambda (str i c) 0) ;;; read!
       #f ;;; write!
       #f ;;; FIXME: get-position
       #f ;;; FIXME: set-position!
       #f ;;; close
       ))
    

  (define (transcoded-port p transcoder)
    (define who 'transcoded-port)
    (unless (transcoder? transcoder)
      (error who "not a transcoder" transcoder))
    (unless ($port? p) (error who "not a port" p))
    (when ($port-transcoder p) (error who "not a binary port" p))
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


  (define (output-port? p) 
    (and ($port? p) 
         ($port-write! p)
         #t))

  (define (input-port? p) 
    (and ($port? p) 
         ($port-read! p) 
         #t))

  (define (textual-port? p) 
    (and ($port? p) 
         ($port-transcoder p) 
         #t))

  (define (binary-port? p) 
    (and ($port? p)
         (not ($port-transcoder p))))

  (define (port-transcoder p)
    (if ($port? p)
        (let ([tr ($port-transcoder p)])
          (and (transcoder? tr) tr))
        (error 'port-transcoder "not a port" p)))
              
  (define (flush-output-port p)
    (unless (output-port? p) 
      (error 'flush-output-port "not an output port" p))
    (when ($port-closed? p) 
      (error 'flush-output-port "port is closed" p))
    (let ([idx ($port-index p)] [size ($port-size p)])
      (unless (fx= idx size)
        (let ([cnt (fx- size idx)])
         (let ([bytes (($port-write! p) ($port-buffer p) idx cnt)])
           (unless (and (fixnum? bytes) (fx>= bytes 0) (fx<= bytes cnt))
             (error 'flush-output-port 
                    "write! returned an invalid value" 
                    bytes))
           ($set-port-index! p (fx+ idx bytes))
           (unless (fx= bytes cnt)
             (flush-output-port p)))))))

  (define ($close-port p)
    (cond
      [($port-closed? p) (void)]
      [else
       (when ($port-write! p)
         (flush-output-port p))
       ($set-port-closed?! p #t)
       (let ([close ($port-close p)])
         (when (procedure? close)
           (close)))]))

  (define (close-port p)
    (unless ($port? p)
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
                            [(fx<= #xD800 n #xDFFF) 
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
                            [(fx<= #x10000 n #x10FFFF) 
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
                            [(fx<= #xD800 n #xDFFF) (do-error p who)]
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
                            [(fx<= #x10000 n #x10FFFF) 
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
             (fxior fast-get-tag fast-get-utf8-tag))
           (eof-object? (advance-bom p who '(#xEF #xBB #xBF)))]
          [else (error 'slow-get-char "codec not handled")])))

    (define (lookahead-char-char-mode p who)
      (let ([str ($port-buffer p)]
            [read! ($port-read! p)])
        (let ([n (read! str 0 (string-length str))])
          (unless (fixnum? n) 
            (error who "invalid return value from read!" n))
          (unless (<= 0 n (fxsub1 (string-length str)))
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
      (let ([m ($port-get-mode p)])
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
          (unless (<= 0 n (fxsub1 (string-length str)))
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
    (unless ($port? p) (error who "not a port" p))
    (when ($port-closed? p) (error who "port is closed" p))
    (when ($port-transcoder p) (error who "port is not binary" p))
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
           (let ([err (- err)])
             (cond
               [(fx< err (vector-length io-errors-vec))
                "unknown error"]
               [else (vector-ref io-errors-vec err)]))])
      (raise 
        (condition
          (make-who-condition who)
          (make-message-condition msg)
          (make-i/o-filename-error id)))))

  (define read-size 4096)
  (define file-buffer-size (+ read-size 128))

  (define (fh->input-port fd id size transcoder close?)
    ($make-port 0 0 (make-bytevector size) 0 
      transcoder
      #f ;;; closed?
      (input-transcoder-attrs transcoder)
      id
      (lambda (bv idx cnt) 
        (let ([bytes
               (foreign-call "ikrt_read_fd" fd bv idx 
                  (fxmin read-size cnt))])
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
               (io-error 'close id err))])))))

  (define (open-file-handle filename who)
    (let ([fh (foreign-call "ikrt_open_input_fd"
                 (string->utf8 filename))])
      (cond
        [(fx< fh 0) (io-error who filename fh)]
        [else fh])))
  

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
       ; FIXME: file-options ignored
       ; FIXME: buffer-mode ignored
       (fh->input-port 
         (open-file-handle filename 'open-file-input-port) 
         filename
         file-buffer-size
         (cond
           [(or (not transcoder) (transcoder? transcoder)) 
            transcoder]
           [else (error 'open-file-input-port
                        "invalid transcoder" 
                        transcoder)])
         #t)]))

  (define (open-input-file filename)
    (unless (string? filename)
      (error 'open-input-file "invalid filename" filename))
    (fh->input-port 
       (open-file-handle filename 'open-input-file) 
       filename
       file-buffer-size
       (native-transcoder)
       #t))

  (define (call-with-input-file filename proc)
    (unless (string? filename)
      (error 'call-with-input-file "invalid filename" filename))
    (unless (procedure? proc)
      (error 'call-with-input-file "not a procedure" proc))
    (call-with-port
      (fh->input-port 
        (open-file-handle filename 'call-with-input-file) 
        filename
        file-buffer-size
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
             (open-file-handle filename 'with-input-from-file) 
             filename
             file-buffer-size
             (native-transcoder)
             #t)])
      (parameterize ([*the-input-port* p])
        (proc))))

  (define (standard-input-port) 
    (fh->input-port 0 '*stdin* 256 #f #f))

  (define *the-input-port* 
    (make-parameter
      (transcoded-port (standard-input-port) (native-transcoder))))

  (define (current-input-port) (*the-input-port*))

  (define (call-with-port p proc)
    (if ($port? p) 
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
    (define (get-it p)
      (let f ([p p] [n 0] [ac '()])
        (let ([x (get-char p)])
          (cond
            [(eqv? x #\newline) 
             (make-it n ac)]
            [(eof-object? x) 
             (if (null? ac) x (make-it n ac))]
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
