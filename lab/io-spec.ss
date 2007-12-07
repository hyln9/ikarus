
(library (io-spec) 
  (export 
    input-port? textual-port? port-eof?
    open-bytevector-input-port
    open-string-input-port
    make-custom-binary-input-port 
    get-char lookahead-char get-u8 lookahead-u8 close-port
    transcoded-port)
  (import 
    (except (ikarus)
      input-port? textual-port? port-eof?
      open-bytevector-input-port
      open-string-input-port
      make-custom-binary-input-port
      get-char lookahead-char get-u8 lookahead-u8 close-port
      transcoded-port))

  (define-struct $port 
    (index size buffer base-index transcoder closed? attrs 
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



  (define (input-port? p) 
    (and ($port? p) 
         ($port-read! p) 
         #t))

  (define (textual-port? p) 
    (and ($port? p) 
         ($port-transcoder p) 
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
         (apply error 'name "not implemented" args))]))

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
                       [else (get-char-utf8-mode p who)]))])]
               [else (do-error p who)]))])))

    (define-rrr get-char-char-mode)

    (define (advance-utf8-bom p who)
      (let ([i ($port-index p)]
            [j ($port-size p)]
            [buf ($port-buffer p)])
        (cond
          [(fx< (fx+ i 3) j) 
           (when (and (fx=? (bytevector-u8-ref buf i) #xEF)
                      (fx=? (bytevector-u8-ref buf i) #xBB) 
                      (fx=? (bytevector-u8-ref buf i) #xBF))
             ($set-port-index! p (fx+ i 3)))]
          [else 
           (let ([c (fx- j i)])
             (bytevector-copy! buf i buf 0 c)
             (let ([read! ($port-read! p)])
               (let ([c1 (read! buf c (fx- (bytevector-length buf) c))])
                 ($set-port-index! p c)
                 ($set-port-size! p (fx+ c c1))
                 (unless (fx= c1 0) 
                   (advance-utf8-bom p who)))))])))

    (define (speedup-input-port p who)
      (unless (input-port? p) 
        (error who "not an input port" p))
      (let ([tr ($port-transcoder p)])
        (unless tr 
          (error who "not a textual port" p))
        (case (transcoder-codec tr)
          [(utf-8-codec) 
           ;;;
           (advance-utf8-bom p who)
           ($set-port-attrs! p 
             (fxior fast-get-tag fast-get-utf8-tag))]
          [else (error 'slow-get-char "codec not handled")])))


    (define-rrr slow-lookahead-char)
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
           (speedup-input-port p who)
           (lookahead-char p)])))
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
                    [else (get-char-utf8-mode p who)]))]
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
          [else 
           (speedup-input-port p who)
           (get-char p)]))))

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
