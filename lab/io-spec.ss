
(library (io-spec) 
  (export )
  (import 
    (except (ikarus) get-char get-u8 put-char put-u8))

  (define-struct $port 
    (index size buffer base-index codec closed? handlers attrs))
  (define $set-port-index! set-$port-index!)

  (define fast-get-tag          #x0001)
  (define fast-put-tag         #x0002)
  (define fast-get-position-tag  #x0004)

  (define fast-get-mask         #x00F0)
  (define fast-get-utf8-tag     #x0010)
  (define fast-get-latin-tag    #x0030)
  (define fast-get-byte-tag     #x0040)
  (define fast-get-char-tag     #x0080)

  (define fast-put-mask        #x0F00)
  (define fast-put-utf8-tag    #x0100)
  (define fast-put-latin-tag   #x0300)
  (define fast-put-byte-tag    #x0400)
  (define fast-put-char-tag    #x0800)
  
  (define r6rs-mode-tag          #x1000)

  (define ($port-get-mode x)
    (and ($port? x) (fxand ($port-attrs x) fast-get-mask)))

  (define ($port-put-mode x)
    (and ($port? x) (fxand ($port-attrs x) fast-put-mask)))

  (define (u8? x)
    (and (fixnum? x) (fx>= x 0) (fx< x 256)))

  ;;; everything above this line will turn into primitive
  ;;; ----------------------------------------------------------
  
  (define-struct handler
    (data id read! write! get-position set-position! close))

  (define-syntax define-rrr
    (syntax-rules ()
      [(_ name)
       (define (name . args) 
         (error 'name "not implemented" args))]))

  ;;; ----------------------------------------------------------
  (module (get-char)
    (define-rrr get-char-utf8-mode)
    (define-rrr get-char-latin-mode)
    (define-rrr get-char-char-mode)
    (define-rrr slow-get-char)
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
                (get-char-latin-mode p who)]))]
          [else (slow-get-char p who)]))))

  ;;; ----------------------------------------------------------
  (module (get-u8)
    (define-rrr get-u8-byte-mode)
    (define-rrr slow-get-u8)
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
               [else (get-u8-byte-mode p who)]))]
          [else (slow-get-u8 p who)]))))

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

  ;;; ----------------------------------------------------------
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

  ;;; that's it for today.  see you tomorrow .

  )
