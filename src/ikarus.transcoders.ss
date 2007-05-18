
(library (ikarus transcoders)
  (export string->utf8-bytevector
          #;utf8-bytevector->string)
  (import (except (ikarus) string->utf8-bytevector)
          (ikarus system $strings)
          (ikarus system $bytevectors)
          (ikarus system $fx)
          (ikarus system $chars)
          )

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


  (define string->utf8-bytevector
    (lambda (str)
      (unless (string? str) 
        (error 'string->utf8-bytevector "~s is not a string" str))
      (fill-utf8-bytevector
        ($make-bytevector (utf8-string-size str))
        str)))


  )


#!eof
       (unless (memq handling-mode '(ignore replace raise)) 
         (error 'string->utf8-bytevector "~s is not a valid hanlding-mode, shound be one of ignore, replace, or raise" handling-mode))

