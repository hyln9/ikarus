
(library (ikarus transcoders)
  (export string->utf8-bytevector
          utf8-bytevector->string)
  (import (except (ikarus) string->utf8-bytevector utf8-bytevector->string)
          (ikarus system $strings)
          (ikarus system $bytevectors)
          (ikarus system $fx)
          (ikarus system $chars)) 
  ;;; From http://en.wikipedia.org/wiki/UTF-8
  ;;; hexadecimal      binary scalar value            UTF8
  ;;; 000000-00007F    00000000_00000000_0zzzzzzz     0zzzzzzz
  ;;; 000080-0007FF    00000000_00000yyy_yyzzzzzz     110yyyyy 10zzzzzz
  ;;; 000800-00FFFF    00000000_xxxxyyyy_yyzzzzzz     1110xxxx 10yyyyyy 10zzzzzz
  ;;; 010000-10FFFF    000wwwxx_xxxxyyyy_yyzzzzzz     11110www 10xxxxxx 10yyyyyy 10zzzzzz

  ;;; valid ranges:  [000000 - 00D7FF] \union [00E000 - 10FFFF]
  ;;; invalid hole:  [00D800 - 00DFFF]

  ;;; handling-modes: ignore, replace, raise
  ;;; ignore: skips over the offending bytes
  ;;; replace: places a U+FFFD in place of the malformed bytes
  ;;; raise: raises an error

  (define string->utf8-bytevector
    (lambda (str)
      (define (utf8-string-size str)
        (let f ([str str] [i 0] [j ($string-length str)] [n 0])
          (cond
            [($fx= i j) n]
            [else
             (let ([c ($string-ref str i)])
               (let ([b ($char->fixnum c)])
                 (f str ($fxadd1 i) j
                    ($fx+ n
                      (cond
                        [($fx<= b #x7F)    1]
                        [($fx<= b #x7FF)   2]
                        [($fx<= b #xFFFF)  3]
                        [else              4])))))])))
      (define (fill-utf8-bytevector bv str)
        (let f ([bv bv] [str str] [i 0] [j 0] [n ($string-length str)])
          (cond
            [($fx= i n) bv]
            [else
             (let ([c ($string-ref str i)])
               (let ([b ($char->fixnum c)])
                 (cond
                  [($fx<= b #x7F)
                   ($bytevector-set! bv j b)
                   (f bv str ($fxadd1 i) ($fxadd1 j) n)]
                  [($fx<= b #x7FF)  
                   ($bytevector-set! bv j
                     ($fxlogor #b11000000 ($fxsra b 6)))
                   ($bytevector-set! bv ($fx+ j 1)
                     ($fxlogor #b10000000 ($fxlogand b #b111111)))
                   (f bv str ($fxadd1 i) ($fx+ j 2) n)]
                  [($fx<= b #xFFFF) 
                   ($bytevector-set! bv j
                     ($fxlogor #b11100000 ($fxsra b 12)))
                   ($bytevector-set! bv ($fx+ j 1)
                     ($fxlogor #b10000000 ($fxlogand ($fxsra b 6) #b111111)))
                   ($bytevector-set! bv ($fx+ j 2)
                     ($fxlogor #b10000000 ($fxlogand b #b111111)))
                   (f bv str ($fxadd1 i) ($fx+ j 3) n)]
                  [else
                   ($bytevector-set! bv j
                     ($fxlogor #b11110000 ($fxsra b 18)))
                   ($bytevector-set! bv ($fx+ j 1)
                     ($fxlogor #b10000000 ($fxlogand ($fxsra b 12) #b111111)))
                   ($bytevector-set! bv ($fx+ j 2)
                     ($fxlogor #b10000000 ($fxlogand ($fxsra b 6) #b111111)))
                   ($bytevector-set! bv ($fx+ j 3)
                     ($fxlogor #b10000000 ($fxlogand b #b111111)))
                   (f bv str ($fxadd1 i) ($fx+ j 4) n)])))])))
      (unless (string? str) 
        (error 'string->utf8-bytevector "~s is not a string" str))
      (fill-utf8-bytevector
        ($make-bytevector (utf8-string-size str))
        str)))

  (define utf8-bytevector->string
    (let ()
      (define who 'utf8-bytevector->string)
      (define (count bv mode)
        (let f ([x bv] [i 0] [j ($bytevector-length bv)] [n 0] [mode mode])
          (cond
            [($fx= i j) n]
            [else
             (let ([b0 ($bytevector-u8-ref x i)])
               (cond
                 [($fx<= b0 #x7F) 
                  (f x ($fxadd1 i) j ($fxadd1 n) mode)]
                 [($fx= ($fxsra b0 5) #b110)
                  (let ([i ($fxadd1 i)])
                    (cond
                      [($fx< i j) 
                       (let ([b1 ($bytevector-u8-ref x i)])
                         (cond
                           [($fx= ($fxsra b1 6) #b10) 
                            (f x ($fxadd1 i) j ($fxadd1 n) mode)]
                           [(eq? mode 'ignore) 
                            (f x i j n mode)]
                           [(eq? mode 'replace) 
                            (f x i j ($fxadd1 n) mode)]
                           [else
                            (error who "invalid byte sequence ~s ~s
                                   in idx ~s of ~s" b0 b1 i bv)]))]
                      [(eq? mode 'ignore) n]
                      [(eq? mode 'replace) ($fxadd1 n)]
                      [else
                       (error who "invalid byte ~s near end of bytevector" b0)]))]
                 [($fx= ($fxsra b0 4) #b1110)
                  (cond
                    [($fx< ($fx+ i 2) j) 
                     (let ([b1 ($bytevector-u8-ref x ($fx+ i 1))]
                           [b2 ($bytevector-u8-ref x ($fx+ i 2))])
                       (cond
                         [($fx= ($fxsra ($fxlogor b1 b2) 6) #b10)
                          (f x ($fx+ i 3) j ($fxadd1 n) mode)]
                         [(eq? mode 'ignore) 
                          (f x ($fxadd1 i) j n mode)]
                         [(eq? mode 'replace)
                          (f x ($fxadd1 i) j ($fxadd1 n) mode)]
                         [else (error who "invalid sequence ~s ~s ~s" b0 b1 b2)]))]
                    [(eq? mode 'ignore) (f x ($fxadd1 i) j n mode)]
                    [(eq? mode 'replace) (f x ($fxadd1 i) j ($fxadd1 n) mode)]
                    [else (error who "incomplete char sequence")])]
                 [($fx= ($fxsra b0 3) #b11110)
                  (cond
                    [($fx< ($fx+ i 3) j) 
                     (let ([b1 ($bytevector-u8-ref x ($fx+ i 1))]
                           [b2 ($bytevector-u8-ref x ($fx+ i 2))]
                           [b3 ($bytevector-u8-ref x ($fx+ i 3))])
                       (cond
                         [($fx= ($fxsra ($fxlogor b1 ($fxlogor b2 b3)) 6) #b10)
                          (f x ($fx+ i 4) j ($fxadd1 n) mode)]
                         [(eq? mode 'ignore) 
                          (f x ($fxadd1 i) j n mode)]
                         [(eq? mode 'replace)
                          (f x ($fxadd1 i) j ($fxadd1 n) mode)]
                         [else (error who "invalid sequence ~s ~s ~s ~s" b0 b1 b2 b3)]))]
                    [(eq? mode 'ignore) (f x ($fxadd1 i) j n mode)]
                    [(eq? mode 'replace) (f x ($fxadd1 i) j ($fxadd1 n) mode)]
                    [else (error who "incomplete char sequence")])]
                 [(eq? mode 'ignore) (f x ($fxadd1 i) j n mode)]
                 [(eq? mode 'replace) (f x ($fxadd1 i) j ($fxadd1 n) mode)]
                 [else (error who "invalid byte ~s at index ~s of ~s" 
                              b0 i x)]))])))
      (define (fill str bv mode)
        (let f ([str str] [x bv] [i 0] [j ($bytevector-length bv)] [n 0] [mode mode])
          (cond
            [($fx= i j) str]
            [else
             (let ([b0 ($bytevector-u8-ref x i)])
               (cond
                 [($fx<= b0 #x7F) 
                  ($string-set! str n ($fixnum->char b0))
                  (f str x ($fxadd1 i) j ($fxadd1 n) mode)]
                 [($fx= ($fxsra b0 5) #b110)
                  (let ([i ($fxadd1 i)])
                    (cond
                      [($fx< i j) 
                       (let ([b1 ($bytevector-u8-ref x i)])
                         (cond
                           [($fx= ($fxsra b1 6) #b10) 
                            ($string-set! str n 
                              ($fixnum->char 
                                ($fx+ ($fxlogand b1 #b111111)
                                      ($fxsll ($fxlogand b0 #b11111) 6))))
                            (f str x ($fxadd1 i) j ($fxadd1 n) mode)]
                           [(eq? mode 'ignore) 
                            (f str x i j n mode)]
                           [(eq? mode 'replace)
                            ($string-set! str n ($fixnum->char #xFFFD))
                            (f str x i j ($fxadd1 n) mode)]
                           [else (error who "BUG")]))]
                      [(eq? mode 'ignore) str]
                      [(eq? mode 'replace) 
                       ($string-set! str n ($fixnum->char #xFFFD))
                       str]
                      [else (error who "BUG")]))]
                 [($fx= ($fxsra b0 4) #b1110)
                  (cond
                    [($fx< ($fx+ i 2) j) 
                     (let ([b1 ($bytevector-u8-ref x ($fx+ i 1))]
                           [b2 ($bytevector-u8-ref x ($fx+ i 2))])
                       (cond
                         [($fx= ($fxsra ($fxlogor b1 b2) 6) #b10)
                          ($string-set! str n 
                             ($fixnum->char 
                               ($fx+ ($fxlogand b2 #b111111)
                                 ($fx+ ($fxsll ($fxlogand b1 #b111111) 6)
                                   ($fxsll ($fxlogand b0 #b1111) 12)))))
                          (f str x ($fx+ i 3) j ($fxadd1 n) mode)]
                         [(eq? mode 'ignore) 
                          (f str x ($fxadd1 i) j n mode)]
                         [(eq? mode 'replace)
                          ($string-set! str n ($fixnum->char #xFFFD))
                          (f str x ($fxadd1 i) j ($fxadd1 n) mode)]
                         [else (error who "BUG")]))]
                    [(eq? mode 'ignore) (f str x ($fxadd1 i) j n mode)]
                    [(eq? mode 'replace) 
                     ($string-set! str n ($fixnum->char #xFFFD))
                     (f str x ($fxadd1 i) j ($fxadd1 n) mode)]
                    [else (error who "BUG")])]
                 [($fx= ($fxsra b0 3) #b11110)
                  (cond
                    [($fx< ($fx+ i 3) j) 
                     (let ([b1 ($bytevector-u8-ref x ($fx+ i 1))]
                           [b2 ($bytevector-u8-ref x ($fx+ i 2))]
                           [b3 ($bytevector-u8-ref x ($fx+ i 3))])
                       (cond
                         [($fx= ($fxsra ($fxlogor b1 ($fxlogor b2 b3)) 6) #b10)
                          ($string-set! str n 
                            ($fixnum->char 
                              ($fx+ ($fxlogand b3 #b111111)
                                ($fx+ ($fxsll ($fxlogand b2 #b111111) 6)
                                  ($fx+ ($fxsll ($fxlogand b1 #b111111) 12)
                                    ($fxsll ($fxlogand b0 #b111) 18))))))
                          (f str x ($fx+ i 4) j ($fxadd1 n) mode)]
                         [(eq? mode 'ignore)
                          (f str x ($fxadd1 i) j n mode)]
                         [(eq? mode 'replace)
                          ($string-set! str n ($fixnum->char #xFFFD))
                          (f str x ($fxadd1 i) j ($fxadd1 n) mode)]
                         [else (error who "BUG")]))]
                    [(eq? mode 'ignore) (f str x ($fxadd1 i) j n mode)]
                    [(eq? mode 'replace) 
                     ($string-set! str n ($fixnum->char #xFFFD))
                     (f str x ($fxadd1 i) j ($fxadd1 n) mode)]
                    [else (error who "BUG")])]
                 [(eq? mode 'ignore) (f str x ($fxadd1 i) j n mode)]
                 [(eq? mode 'replace)
                  ($string-set! str n ($fixnum->char #xFFFD))
                  (f str x ($fxadd1 i) j ($fxadd1 n) mode)]
                 [else (error who "BUG")]))])))
      (define (convert bv mode)
        (fill ($make-string (count bv mode)) bv mode))
      (case-lambda
        [(bv) (convert bv 'raise)]
        [(bv handling-mode)
         (unless (memq handling-mode '(ignore replace raise)) 
           (error 'utf8-bytevector->string
                  "~s is not a valid handling mode"
                  handling-mode))
         (convert bv handling-mode)])))

  )
