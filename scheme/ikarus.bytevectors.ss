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


(library (ikarus bytevectors)
  (export
    make-bytevector bytevector-length bytevector-s8-ref
    bytevector-u8-ref bytevector-u8-set! bytevector-s8-set!
    bytevector-copy! u8-list->bytevector bytevector->u8-list
    bytevector-u16-native-ref bytevector-u16-native-set!
    bytevector-s16-native-ref bytevector-s16-native-set!
    bytevector-u32-native-ref bytevector-u32-native-set!
    bytevector-s32-native-ref bytevector-s32-native-set!
    bytevector-u64-native-ref bytevector-u64-native-set!
    bytevector-s64-native-ref bytevector-s64-native-set!
    bytevector-u16-ref bytevector-u16-set!
    bytevector-s16-ref bytevector-s16-set!
    bytevector-u32-ref bytevector-u32-set!
    bytevector-s32-ref bytevector-s32-set!
    bytevector-u64-ref bytevector-u64-set!
    bytevector-s64-ref bytevector-s64-set!
    bytevector-fill! bytevector-copy bytevector=?
    bytevector-uint-ref bytevector-sint-ref 
    bytevector-uint-set!  bytevector-sint-set!
    bytevector->uint-list bytevector->sint-list
    uint-list->bytevector sint-list->bytevector
    bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
    bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
    bytevector-ieee-double-ref bytevector-ieee-double-set!
    bytevector-ieee-single-ref bytevector-ieee-single-set!
    native-endianness)
  (import 
    (except (ikarus) 
      make-bytevector bytevector-length bytevector-s8-ref
      bytevector-u8-ref bytevector-u8-set! bytevector-s8-set! 
      bytevector-copy! u8-list->bytevector bytevector->u8-list
      bytevector-u16-native-ref bytevector-u16-native-set!
      bytevector-s16-native-ref bytevector-s16-native-set!
      bytevector-u32-native-ref bytevector-u32-native-set!
      bytevector-s32-native-ref bytevector-s32-native-set!
      bytevector-u64-native-ref bytevector-u64-native-set!
      bytevector-s64-native-ref bytevector-s64-native-set!
      bytevector-u16-ref bytevector-u16-set!
      bytevector-s16-ref bytevector-s16-set!
      bytevector-u32-ref bytevector-u32-set!
      bytevector-s32-ref bytevector-s32-set!
      bytevector-u64-ref bytevector-u64-set!
      bytevector-s64-ref bytevector-s64-set!
      bytevector-fill! bytevector-copy bytevector=?
      bytevector-uint-ref bytevector-sint-ref
      bytevector-uint-set!  bytevector-sint-set!
      bytevector->uint-list bytevector->sint-list
      uint-list->bytevector sint-list->bytevector
      bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
      bytevector-ieee-double-ref bytevector-ieee-double-set!
      bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
      bytevector-ieee-single-ref bytevector-ieee-single-set!
      native-endianness)
    (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $pairs)
    (ikarus system $bytevectors))

  (define (native-endianness) 'big) ;;; HARDCODED


  (define ($bytevector-fill x i j fill)
    (cond
      [($fx= i j) x]
      [else
       ($bytevector-set! x i fill)
       ($bytevector-fill x ($fxadd1 i) j fill)]))

  (define make-bytevector
    (case-lambda 
      [(k) 
       (if (and (fixnum? k) ($fx>= k 0))
           ($make-bytevector k)
           (die 'make-bytevector "not a valid size" k))]
      [(k fill)
       (if (and (fixnum? fill) ($fx<= -128 fill) ($fx<= fill 255))
           ($bytevector-fill (make-bytevector k) 0 k fill)
           (die 'make-bytevector "not a valid fill" fill))]))

  (define bytevector-fill!
    (lambda (x fill)
      (unless (bytevector? x)
        (die 'bytevector-fill! "not a bytevector" x))
      (unless (and (fixnum? fill) ($fx<= -128 fill) ($fx<= fill 255))
        (die 'bytevector-fill! "not a valid fill" fill))
      ($bytevector-fill x 0 ($bytevector-length x) fill)))


  (define bytevector-length
    (lambda (x)
      (if (bytevector? x)
          ($bytevector-length x)
          (die 'bytevector-length "not a bytevector" x))))

  (define bytevector-s8-ref
    (lambda (x i)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              ($bytevector-s8-ref x i)
              (die 'bytevector-s8-ref "invalid index" i x))
          (die 'bytevector-s8-ref "not a bytevector" x))))

  (define bytevector-u8-ref
    (lambda (x i)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              ($bytevector-u8-ref x i)
              (die 'bytevector-u8-ref "invalid index" i x))
          (die 'bytevector-u8-ref "not a bytevector" x))))


  (define bytevector-s8-set!
    (lambda (x i v)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              (if (and (fixnum? v) ($fx<= -128 v) ($fx<= v 127)) 
                  ($bytevector-set! x i v)
                  (die 'bytevector-s8-set! "not a byte" v))
              (die 'bytevector-s8-set! "invalid index" i x))
          (die 'bytevector-s8-set! "not a bytevector" x))))
  
  (define bytevector-u8-set!
    (lambda (x i v)
      (if (bytevector? x)
          (if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
              (if (and (fixnum? v) ($fx<= 0 v) ($fx<= v 255))
                  ($bytevector-set! x i v)
                  (die 'bytevector-u8-set! "not an octet" v))
              (die 'bytevector-u8-set! "invalid index" i x))
          (die 'bytevector-u8-set! "not a bytevector" x))))

  (define bytevector-u16-native-ref ;;; HARDCODED
    (lambda (x i) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx< i ($fxsub1 ($bytevector-length x)))
                   ($fxzero? ($fxlogand i 1)))
              ($fxlogor
                ($fxsll ($bytevector-u8-ref x i) 8)
                ($bytevector-u8-ref x ($fxadd1 i)))
              (die 'bytevector-u16-native-ref "invalid index" i))
          (die 'bytevector-u16-native-ref "not a bytevector" x))))

  
  (define bytevector-u16-native-set! ;;; HARDCODED
    (lambda (x i n) 
      (if (bytevector? x) 
          (if (and (fixnum? n) 
                   ($fx<= 0 n) 
                   ($fx<= n #xFFFF))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx< i ($fxsub1 ($bytevector-length x)))
                       ($fxzero? ($fxlogand i 1)))
                  (begin
                    ($bytevector-set! x i ($fxsra n 8)) 
                    ($bytevector-set! x ($fxadd1 i) n))
                  (die 'bytevector-u16-native-set! "invalid index" i))
              (die 'bytevector-u16-native-set! "invalid value" n))
          (die 'bytevector-u16-native-set! "not a bytevector" x))))

  (define bytevector-s16-native-set! ;;; HARDCODED
    (lambda (x i n) 
      (if (bytevector? x) 
          (if (and (fixnum? n) 
                   ($fx<= #x-8000 n) 
                   ($fx<= n #x7FFF))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx< i ($fxsub1 ($bytevector-length x)))
                       ($fxzero? ($fxlogand i 1)))
                  (begin
                    ($bytevector-set! x i ($fxsra n 8)) 
                    ($bytevector-set! x ($fxadd1 i) n))
                  (die 'bytevector-s16-native-set! "invalid index" i))
              (die 'bytevector-s16-native-set! "invalid value" n))
          (die 'bytevector-s16-native-set! "not a bytevector" x))))

  (define bytevector-s16-native-ref ;;; HARDCODED
    (lambda (x i) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx< i ($fxsub1 ($bytevector-length x)))
                   ($fxzero? ($fxlogand i 1)))
              ($fxlogor
                ($fxsll ($bytevector-s8-ref x i) 8)
                ($bytevector-u8-ref x ($fxadd1 i)))
              (die 'bytevector-s16-native-ref "invalid index" i))
          (die 'bytevector-s16-native-ref "not a bytevector" x))))

  (define bytevector-u16-ref
    (lambda (x i end) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx< i ($fxsub1 ($bytevector-length x))))
              (case end
                [(big) 
                 ($fxlogor
                   ($fxsll ($bytevector-u8-ref x i) 8)
                   ($bytevector-u8-ref x ($fxadd1 i)))]
                [(little) 
                 ($fxlogor
                   ($fxsll ($bytevector-u8-ref x (fxadd1 i)) 8)
                   ($bytevector-u8-ref x i))]
                [else (die 'bytevector-u16-ref "invalid endianness" end)])
              (die 'bytevector-u16-ref "invalid index" i))
          (die 'bytevector-u16-ref "not a bytevector" x))))

  (define bytevector-u32-ref
    (lambda (x i end) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx< i ($fx- ($bytevector-length x) 3)))
              (case end
                [(big) 
                 (+ (sll ($bytevector-u8-ref x i) 24)
                    ($fxlogor
                      ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 16)
                      ($fxlogor 
                        ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 8)
                        ($bytevector-u8-ref x ($fx+ i 3)))))]
                [(little) 
                 (+ (sll ($bytevector-u8-ref x ($fx+ i 3)) 24)
                    ($fxlogor
                      ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 16)
                      ($fxlogor
                        ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 8)
                        ($bytevector-u8-ref x i))))]
                [else (die 'bytevector-u32-ref "invalid endianness" end)])
              (die 'bytevector-u32-ref "invalid index" i))
          (die 'bytevector-u32-ref "not a bytevector" x))))

  (define bytevector-u32-native-ref
    (lambda (x i) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx= 0 ($fxlogand i 3))
                   ($fx< i ($fx- ($bytevector-length x) 3)))
              (+ (sll ($bytevector-u8-ref x i) 24)
                 ($fxlogor
                   ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 16)
                   ($fxlogor 
                     ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 8)
                     ($bytevector-u8-ref x ($fx+ i 3)))))
              (die 'bytevector-u32-native-ref "invalid index" i))
          (die 'bytevector-u32-native-ref "not a bytevector" x))))

  (define bytevector-s32-ref
    (lambda (x i end) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx< i ($fx- ($bytevector-length x) 3)))
              (case end
                [(big) 
                 (+ (sll ($bytevector-s8-ref x i) 24)
                    ($fxlogor
                      ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 16)
                      ($fxlogor 
                        ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 8)
                        ($bytevector-u8-ref x ($fx+ i 3)))))]
                [(little) 
                 (+ (sll ($bytevector-s8-ref x ($fx+ i 3)) 24)
                    ($fxlogor
                      ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 16)
                      ($fxlogor
                        ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 8)
                        ($bytevector-u8-ref x i))))]
                [else (die 'bytevector-s32-ref "invalid endianness" end)])
              (die 'bytevector-s32-ref "invalid index" i))
          (die 'bytevector-s32-ref "not a bytevector" x))))

  (define bytevector-s32-native-ref
    (lambda (x i) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx= 0 ($fxlogand i 3))
                   ($fx< i ($fx- ($bytevector-length x) 3)))
              (+ (sll ($bytevector-s8-ref x i) 24)
                 ($fxlogor
                   ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 16)
                   ($fxlogor 
                     ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 8)
                     ($bytevector-u8-ref x ($fx+ i 3)))))
              (die 'bytevector-s32-native-ref "invalid index" i))
          (die 'bytevector-s32-native-ref "not a bytevector" x))))

  (define bytevector-u16-set!
    (lambda (x i n end) 
      (if (bytevector? x) 
          (if (and (fixnum? n) 
                   ($fx<= 0 n) 
                   ($fx<= n #xFFFF))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx< i ($fxsub1 ($bytevector-length x))))
                  (case end
                    [(big) 
                     ($bytevector-set! x i ($fxsra n 8))
                     ($bytevector-set! x ($fxadd1 i) n)]
                    [(little) 
                     ($bytevector-set! x i n)
                     ($bytevector-set! x ($fxadd1 i) (fxsra n 8))]
                    [else (die 'bytevector-u16-ref "invalid endianness" end)])
                  (die 'bytevector-u16-set! "invalid index" i))
              (die 'bytevector-u16-set! "invalid value" n))
          (die 'bytevector-u16-set! "not a bytevector" x))))


  (define bytevector-u32-set!
    (lambda (x i n end) 
      (if (bytevector? x) 
          (if (if (fixnum? n)
                  ($fx>= n 0)
                  (if (bignum? n)
                      (<= 0 n #xFFFFFFFF)
                      #f))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx< i ($fx- ($bytevector-length x) 3)))
                  (case end
                    [(big) 
                     (let ([b (sra n 16)])
                       ($bytevector-set! x i ($fxsra b 8))
                       ($bytevector-set! x ($fx+ i 1) b))
                     (let ([b (bitwise-and n #xFFFF)])
                       ($bytevector-set! x ($fx+ i 2) ($fxsra b 8))
                       ($bytevector-set! x ($fx+ i 3) b))]
                    [(little) 
                     (let ([b (sra n 16)])
                       ($bytevector-set! x ($fx+ i 3) ($fxsra b 8))
                       ($bytevector-set! x ($fx+ i 2) b))
                     (let ([b (bitwise-and n #xFFFF)])
                       ($bytevector-set! x ($fx+ i 1) ($fxsra b 8))
                       ($bytevector-set! x i b))]
                    [else (die 'bytevector-u32-ref "invalid endianness" end)])
                  (die 'bytevector-u32-set! "invalid index" i))
              (die 'bytevector-u32-set! "invalid value" n))
          (die 'bytevector-u32-set! "not a bytevector" x))))

  (define bytevector-u32-native-set!
    (lambda (x i n) 
      (if (bytevector? x) 
          (if (if (fixnum? n)
                  ($fx>= n 0)
                  (if (bignum? n)
                      (<= 0 n #xFFFFFFFF)
                      #f))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx= 0 ($fxlogand i 3))
                       ($fx< i ($fx- ($bytevector-length x) 3)))
                  (begin
                    (let ([b (sra n 16)])
                      ($bytevector-set! x i ($fxsra b 8))
                      ($bytevector-set! x ($fx+ i 1) b))
                    (let ([b (bitwise-and n #xFFFF)])
                      ($bytevector-set! x ($fx+ i 2) ($fxsra b 8))
                      ($bytevector-set! x ($fx+ i 3) b)))
                  (die 'bytevector-u32-native-set! "invalid index" i))
              (die 'bytevector-u32-native-set! "invalid value" n))
          (die 'bytevector-u32-native-set! "not a bytevector" x))))


  (define bytevector-s32-native-set!
    (lambda (x i n) 
      (if (bytevector? x) 
          (if (if (fixnum? n)
                  #t
                  (if (bignum? n)
                      (<= #x-80000000 n #x7FFFFFFF)
                      #f))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx= 0 ($fxlogand i 3))
                       ($fx< i ($fx- ($bytevector-length x) 3)))
                  (begin
                    (let ([b (sra n 16)])
                      ($bytevector-set! x i ($fxsra b 8))
                      ($bytevector-set! x ($fx+ i 1) b))
                    (let ([b (bitwise-and n #xFFFF)])
                      ($bytevector-set! x ($fx+ i 2) ($fxsra b 8))
                      ($bytevector-set! x ($fx+ i 3) b)))
                  (die 'bytevector-s32-native-set! "invalid index" i))
              (die 'bytevector-s32-native-set! "invalid value" n))
          (die 'bytevector-s32-native-set! "not a bytevector" x))))

  (define bytevector-s32-set!
    (lambda (x i n end) 
      (if (bytevector? x) 
          (if (if (fixnum? n)
                  #t
                  (if (bignum? n)
                      (<= #x-80000000 n #x7FFFFFFF)
                      #f))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx< i ($fx- ($bytevector-length x) 3)))
                  (case end
                    [(big) 
                     (let ([b (sra n 16)])
                       ($bytevector-set! x i ($fxsra b 8))
                       ($bytevector-set! x ($fx+ i 1) b))
                     (let ([b (bitwise-and n #xFFFF)])
                       ($bytevector-set! x ($fx+ i 2) ($fxsra b 8))
                       ($bytevector-set! x ($fx+ i 3) b))]
                    [(little) 
                     (let ([b (sra n 16)])
                       ($bytevector-set! x ($fx+ i 3) ($fxsra b 8))
                       ($bytevector-set! x ($fx+ i 2) b))
                     (let ([b (bitwise-and n #xFFFF)])
                       ($bytevector-set! x ($fx+ i 1) ($fxsra b 8))
                       ($bytevector-set! x i b))]
                    [else (die 'bytevector-s32-ref "invalid endianness" end)])
                  (die 'bytevector-s32-set! "invalid index" i))
              (die 'bytevector-s32-set! "invalid value" n))
          (die 'bytevector-s32-set! "not a bytevector" x))))

  (define bytevector-s16-ref
    (lambda (x i end) 
      (if (bytevector? x) 
          (if (and (fixnum? i)
                   ($fx<= 0 i)
                   ($fx< i ($fxsub1 ($bytevector-length x))))
              (case end
                [(big) 
                 ($fxlogor
                   ($fxsll ($bytevector-s8-ref x i) 8)
                   ($bytevector-u8-ref x ($fxadd1 i)))]
                [(little) 
                 ($fxlogor
                   ($fxsll ($bytevector-s8-ref x (fxadd1 i)) 8)
                   ($bytevector-u8-ref x i))]
                [else (die 'bytevector-s16-ref "invalid endianness" end)])
              (die 'bytevector-s16-ref "invalid index" i))
          (die 'bytevector-s16-ref "not a bytevector" x))))

  (define bytevector-s16-set!
    (lambda (x i n end) 
      (if (bytevector? x) 
          (if (and (fixnum? n) 
                   ($fx<= #x-8000 n) 
                   ($fx<= n #x7FFF))
              (if (and (fixnum? i)
                       ($fx<= 0 i)
                       ($fx< i ($fxsub1 ($bytevector-length x))))
                  (case end
                    [(big) 
                     ($bytevector-set! x i ($fxsra n 8))
                     ($bytevector-set! x ($fxadd1 i) n)]
                    [(little) 
                     ($bytevector-set! x i n)
                     ($bytevector-set! x ($fxadd1 i) (fxsra n 8))]
                    [else (die 'bytevector-s16-ref "invalid endianness" end)])
                  (die 'bytevector-s16-set! "invalid index" i))
              (die 'bytevector-s16-set! "invalid value" n))
          (die 'bytevector-s16-set! "not a bytevector" x))))

  (define bytevector->u8-list
    (lambda (x)
      (unless (bytevector? x)
        (die 'bytevector->u8-list "not a bytevector" x))
      (let f ([x x] [i ($bytevector-length x)] [ac '()])
        (cond
          [($fx= i 0) ac]
          [else
           (let ([i ($fxsub1 i)])
             (f x i (cons ($bytevector-u8-ref x i) ac)))]))))

  (define u8-list->bytevector
    (letrec ([race
              (lambda (h t ls n)
               (if (pair? h)
                   (let ([h ($cdr h)])
                      (if (pair? h)
                          (if (not (eq? h t))
                              (race ($cdr h) ($cdr t) ls ($fx+ n 2))
                              (die 'u8-list->bytevector "circular list" ls))
                          (if (null? h)
                              ($fx+ n 1)
                              (die 'u8-list->bytevector "not a proper list" ls))))
                   (if (null? h)
                       n
                       (die 'u8-list->bytevector "not a proper list" ls))))]
              [fill
               (lambda (s i ls)
                 (cond
                   [(null? ls) s]
                   [else
                    (let ([c ($car ls)])
                      (unless (and (fixnum? c) ($fx<= 0 c) ($fx<= c 255))
                        (die 'u8-list->bytevector "not an octet" c))
                      ($bytevector-set! s i c)
                      (fill s ($fxadd1 i) (cdr ls)))]))])
       (lambda (ls)
         (let ([n (race ls ls ls 0)])
           (let ([s ($make-bytevector n)])
             (fill s 0 ls))))))

  (define bytevector-copy
    (lambda (src)
      (unless (bytevector? src)
        (die 'bytevector-copy "not a bytevector" src))
      (let ([n ($bytevector-length src)])
        (let f ([src src] [dst ($make-bytevector n)] [i 0] [n n])
          (cond
            [($fx= i n) dst]
            [else
             ($bytevector-set! dst i ($bytevector-u8-ref src i))
             (f src dst ($fxadd1 i) n)])))))

  (define bytevector=? 
    (lambda (x y)
      (unless (bytevector? x)
        (die 'bytevector=? "not a bytevector" x))
      (unless (bytevector? y)
        (die 'bytevector=? "not a bytevector" y))
      (let ([n ($bytevector-length x)])
        (and ($fx= n ($bytevector-length y))
             (let f ([x x] [y y] [i 0] [n n])
               (or ($fx= i n)
                   (and ($fx= ($bytevector-u8-ref x i)
                              ($bytevector-u8-ref y i))
                        (f x y ($fxadd1 i) n))))))))

  (define bytevector-copy!
    (lambda (src src-start dst dst-start k) 
      (cond
        [(or (not (fixnum? src-start)) ($fx< src-start 0))
         (die 'bytevector-copy! "not a valid starting index" src-start)]
        [(or (not (fixnum? dst-start)) ($fx< dst-start 0))
         (die 'bytevector-copy! "not a valid starting index" dst-start)]
        [(or (not (fixnum? k)) ($fx< k 0))
         (die 'bytevector-copy! "not a valid length" k)]
        [(not (bytevector? src)) 
         (die 'bytevector-copy! "not a bytevector" src)]
        [(not (bytevector? dst)) 
         (die 'bytevector-copy! "not a bytevector" dst)]
        [(let ([n ($fx+ src-start k)])
           (or ($fx< n 0) ($fx> n ($bytevector-length src))))
         (die 'bytevector-copy! "out of range" src-start k)]
        [(let ([n ($fx+ dst-start k)])
           (or ($fx< n 0) ($fx> n ($bytevector-length dst))))
         (die 'bytevector-copy! "out of range" dst-start k)]
        [(eq? src dst)
         (cond
           [($fx< dst-start src-start)
            (let f ([src src] [si src-start] [di dst-start] [sj ($fx+ src-start k)])
              (unless ($fx= si sj)
                ($bytevector-set! src di ($bytevector-u8-ref src si))
                (f src ($fxadd1 si) ($fxadd1 di) sj)))]
           [($fx< src-start dst-start)
            (let f ([src src] [si ($fx+ src-start k)] [di ($fx+ dst-start k)] [sj src-start])
              (unless ($fx= si sj)
                (let ([si ($fxsub1 si)] [di ($fxsub1 di)])
                  ($bytevector-set! src di ($bytevector-u8-ref src si))
                  (f src si di sj))))]
           [else (void)])]
        [else
         (let f ([src src] [si src-start] [dst dst] [di dst-start] [sj ($fx+ src-start k)])
           (unless ($fx= si sj)
             ($bytevector-set! dst di ($bytevector-u8-ref src si))
             (f src ($fxadd1 si) dst ($fxadd1 di) sj)))])))

  (module (bytevector-uint-ref bytevector-sint-ref
           bytevector->uint-list bytevector->sint-list)
    (define (uref-big x ib il) ;; ib included, il excluded
      (cond
        [($fx= il ib) 0]
        [else
         (let ([b ($bytevector-u8-ref x ib)])
           (cond
             [($fx= b 0) (uref-big x ($fxadd1 ib) il)]
             [else
              (case ($fx- il ib)
                [(1) b]
                [(2) ($fx+ ($fxsll b 8)
                           ($bytevector-u8-ref x ($fxsub1 il)))]
                [(3) 
                 ($fx+ ($fxsll ($fx+ ($fxsll b 8)
                                     ($bytevector-u8-ref x ($fxadd1 ib)))
                               8)
                       ($bytevector-u8-ref x ($fxsub1 il)))]
                [else
                 (let ([im ($fxsra ($fx+ il ib) 1)])
                   (+ (uref-big x im il)
                      (* (uref-big x ib im)
                         (expt 256 ($fx- il im)))))])]))]))
    (define (uref-little x il ib) ;; il included, ib excluded
      (cond
        [($fx= il ib) 0]
        [else
         (let ([ib^ ($fxsub1 ib)])
          (let ([b ($bytevector-u8-ref x ib^)])
            (cond
              [($fx= b 0) (uref-little x il ib^)]
              [else
               (case ($fx- ib il)
                 [(1) b]
                 [(2) ($fx+ ($fxsll b 8) ($bytevector-u8-ref x il))]
                 [(3) 
                  ($fx+ ($fxsll ($fx+ ($fxsll b 8) 
                                      ($bytevector-u8-ref x ($fxadd1 il)))
                                8)
                        ($bytevector-u8-ref x il))]
                 [else
                  (let ([im ($fxsra ($fx+ il ib) 1)])
                    (+ (uref-little x il im) 
                       (* (uref-little x im ib) 
                          (expt 256 ($fx- im il)))))])])))]))
    (define (sref-big x ib il) ;; ib included, il excluded
      (cond
        [($fx= il ib) -1]
        [else
         (let ([b ($bytevector-u8-ref x ib)])
           (cond
             [($fx= b 0) (uref-big x ($fxadd1 ib) il)]
             [($fx= b 255) (sref-big-neg x ($fxadd1 ib) il)]
             [($fx< b 128) (uref-big x ib il)]
             [else (- (uref-big x ib il) (expt 256 ($fx- il ib)))]))]))
    (define (sref-big-neg x ib il) ;; ib included, il excluded
      (cond
        [($fx= il ib) -1]
        [else
         (let ([b ($bytevector-u8-ref x ib)])
           (cond
             [($fx= b 255) (sref-big-neg x ($fxadd1 ib) il)]
             [else (- (uref-big x ib il) (expt 256 ($fx- il ib)))]))]))
    (define (sref-little x il ib) ;; il included, ib excluded
      (cond
        [($fx= il ib) -1]
        [else
         (let ([ib^ ($fxsub1 ib)])
          (let ([b ($bytevector-u8-ref x ib^)])
            (cond
              [($fx= b 0) (uref-little x il ib^)]
              [($fx= b 255) (sref-little-neg x il ib^)]
              [($fx< b 128) (uref-little x il ib)]
              [else (- (uref-little x il ib) (expt 256 ($fx- ib il)))])))]))
    (define (sref-little-neg x il ib) ;; il included, ib excluded
      (cond
        [($fx= il ib) -1]
        [else
         (let ([ib^ ($fxsub1 ib)])
          (let ([b ($bytevector-u8-ref x ib^)])
            (cond
              [($fx= b 255) (sref-little-neg x il ib^)]
              [else (- (uref-little x il ib) (expt 256 ($fx- ib il)))])))])) 
    (define bytevector-sint-ref
      (lambda (x k endianness size)
        (define who 'bytevector-sint-ref)
        (unless (bytevector? x) (die who "not a bytevector" x))
        (unless (and (fixnum? k) ($fx>= k 0)) (die who "invalid index" k))
        (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
        (let ([n ($bytevector-length x)])
          (unless ($fx< k n) (die who "index is out of range" k))
          (let ([end ($fx+ k size)])
            (unless (and ($fx>= end 0) ($fx<= end n))
              (die who "out of range" k size))
            (case endianness
              [(little) (sref-little x k end)]
              [(big)    (sref-big x k end)]
              [else (die who "invalid endianness" endianness)])))))
    (define bytevector-uint-ref
      (lambda (x k endianness size)
        (define who 'bytevector-uint-ref)
        (unless (bytevector? x) (die who "not a bytevector" x))
        (unless (and (fixnum? k) ($fx>= k 0)) (die who "invalid index" k))
        (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
        (let ([n ($bytevector-length x)])
          (unless ($fx< k n) (die who "index is out of range" k))
          (let ([end ($fx+ k size)])
            (unless (and ($fx>= end 0) ($fx<= end n))
              (die who "out of range" k size))
            (case endianness
              [(little) (uref-little x k end)]
              [(big)    (uref-big x k end)]
              [else (die who "invalid endianness" endianness)])))))
    (define (bytevector->some-list x k n ls proc who)
      (cond
        [($fx= n 0) ls]
        [else
         (let ([i ($fx- n k)])
           (cond
             [($fx>= i 0)
              (bytevector->some-list x k i (cons (proc x i n) ls) proc who)]
             [else
              (die who "invalid size" k)]))]))
    (define bytevector->uint-list
      (lambda (x endianness size)
        (define who 'bytevector->uint-list)
        (unless (bytevector? x) (die who "not a bytevector" x))
        (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
        (case endianness
          [(little) (bytevector->some-list x size ($bytevector-length x)
                      '() uref-little 'bytevector->uint-list)]
          [(big)    (bytevector->some-list x size ($bytevector-length x) 
                      '() uref-big 'bytevector->uint-list)]
          [else (die who "invalid endianness" endianness)])))
    (define bytevector->sint-list
      (lambda (x endianness size)
        (define who 'bytevector->sint-list)
        (unless (bytevector? x) (die who "not a bytevector" x))
        (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
        (case endianness
          [(little) (bytevector->some-list x size ($bytevector-length x)
                      '() sref-little 'bytevector->sint-list)]
          [(big)    (bytevector->some-list x size ($bytevector-length x) 
                      '() sref-big 'bytevector->sint-list)]
          [else (die who "invalid endianness" endianness)]))))

  (module (bytevector-uint-set! bytevector-sint-set!)
    (define (lufx-set! x k1 n k2 who no)
      (cond
        [($fx= k1 k2) 
         (unless ($fxzero? n)
           (die who "number does not fit" no))]
        [else
         (lufx-set! x ($fxadd1 k1) ($fxsra n 8) k2 who no)
         ($bytevector-set! x k1 ($fxlogand n 255))]))
    (define (lsfx-set! x k1 n k2 who no)
      (cond
        [($fx= k1 k2) 
         (unless ($fx= n -1) ;;; BUG: does not catch all errors
           (die who "number does not fit" no))]
        [else
         (lsfx-set! x ($fxadd1 k1) ($fxsra n 8) k2 who no)
         ($bytevector-set! x k1 ($fxlogand n 255))]))
    (define (bufx-set! x k1 n k2 who no)
      (cond
        [($fx= k1 k2) 
         (unless ($fxzero? n)
           (die who "number does not fit" no))]
        [else
         (let ([k2 ($fxsub1 k2)])
           (bufx-set! x k1 ($fxsra n 8) k2 who no)
           ($bytevector-set! x k2 ($fxlogand n 255)))]))
    (define (bsfx-set! x k1 n k2 who no)
      (cond
        [($fx= k1 k2) 
         (unless ($fx= n -1)
           (die who "number does not fit" no))]
        [else
         (let ([k2 ($fxsub1 k2)])
           (bsfx-set! x k1 ($fxsra n 8) k2 who no)
           ($bytevector-set! x k2 ($fxlogand n 255)))])) 
    (define (lbn-copy! x k n i j)
      (unless ($fx= i j)
        ($bytevector-set! x k ($bignum-byte-ref n i))
        (lbn-copy! x ($fxadd1 k) n ($fxadd1 i) j)))
    (define (bbn-copy! x k n i j)
      (unless ($fx= i j)
        (let ([k ($fxsub1 k)])
          ($bytevector-set! x k ($bignum-byte-ref n i))
          (bbn-copy! x k n ($fxadd1 i) j))))
    (define (bv-zero! x i j)
      (unless ($fx= i j)
        ($bytevector-set! x i 0)
        (bv-zero! x ($fxadd1 i) j)))
    (define (make-lbn-neg-copy! who)
      (define (lbn-neg-copy! x xi n ni xj nj c)
        (cond
          [($fx= ni nj)
           (case ($fxsra c 7)
             [(#x01) ;;; borrow is 0, last byte was negative
              (bv-neg-zero! x xi xj)]
             [(#x00) ;;; borrow is 0, last byte was positive
              (if ($fx< xi xj)
                  (bv-neg-zero! x xi xj)
                  (die who "number does not fit" n))]
             [else (die 'lbn-neg-copy! "BUG: not handled" c)])]
          [else
           (let ([c ($fx- ($fx+ 255 ($fxsra c 8)) ($bignum-byte-ref n ni))])
             (lbn-neg-copy! x ($fxadd1 xi) n ($fxadd1 ni) xj nj c)
             ($bytevector-set! x xi ($fxlogand c 255)))]))
      lbn-neg-copy!)
    (define (make-bbn-neg-copy! who)
      (define (bbn-neg-copy! x xi n ni xj nj c)
        (cond
          [($fx= ni nj)
           (case ($fxsra c 7)
             [(#x01) ;;; borrow is 0, last byte was negative
              (bv-neg-zero! x xi xj)]
             [(#x00) ;;; borrow is 0, last byte was positive
              (if ($fx< xi xj)
                  (bv-neg-zero! x xi xj)
                  (die who "number does not fit" n))]
             [else (die 'bbn-neg-copy! "BUG: not handled" c)])]
          [else
           (let ([c ($fx- ($fx+ 255 ($fxsra c 8)) ($bignum-byte-ref n ni))]
                 [xj ($fxsub1 xj)])
             (bbn-neg-copy! x xi n ($fxadd1 ni) xj nj c)
             ($bytevector-set! x xj ($fxlogand c 255)))]))
      bbn-neg-copy!)
    (define (make-lbn-pos-copy! who)
      (define (lbn-pos-copy! x xi n ni nj xj c)
        (cond
          [($fx= ni nj)
           (cond
             [(or ($fx<= c 127) ($fx< xi xj))
              ;;; last byte was positive
              (bv-zero! x xi xj)]
             [else 
              (die who "number does not fit" n)])]
          [else
           (let ([c ($bignum-byte-ref n ni)])
             (lbn-pos-copy! x ($fxadd1 xi) n ($fxadd1 ni) nj xj c)
             ($bytevector-set! x xi ($fxlogand c 255)))]))
      lbn-pos-copy!)
    (define (make-bbn-pos-copy! who)
      (define (bbn-pos-copy! x xi n ni nj xj c)
        (cond
          [($fx= ni nj)
           (cond
             [(or ($fx<= c 127) ($fx< xi xj))
              ;;; last byte was positive
              (bv-zero! x xi xj)]
             [else 
              (die who "number does not fit" n)])]
          [else
           (let ([c ($bignum-byte-ref n ni)]
                 [xj ($fxsub1 xj)])
             (bbn-pos-copy! x xi n ($fxadd1 ni) nj xj c)
             ($bytevector-set! x xj ($fxlogand c 255)))]))
      bbn-pos-copy!)
    (define (bv-neg-zero! x i j)
      (unless ($fx= i j)
        ($bytevector-set! x i 255)
        (bv-neg-zero! x ($fxadd1 i) j)))
    (define (bignum-bytes n) 
      (let f ([n n] [i ($bignum-size n)])
        (let ([i-1 ($fxsub1 i)])
          (if ($fxzero? ($bignum-byte-ref n i-1))
              (f n i-1)
              i))))
    (define (make-bytevector-uint-set! who)
      (lambda (x k n endianness size)
        (unless (bytevector? x) (die who "not a bytevector" x))
        (unless (and (fixnum? k) ($fx>= k 0)) (die who "invalid index" k))
        (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
        (case endianness
          [(little)
           (cond
             [(fixnum? n) (lufx-set! x k n ($fx+ k size) who n)]
             [(bignum? n)
              (if ($bignum-positive? n)
                  (let ([sz (bignum-bytes n)])
                    (cond
                      [($fx= sz size) 
                       (lbn-copy! x k n 0 sz)]
                      [($fx< sz size)
                       (lbn-copy! x k n 0 sz)
                       (bv-zero! x ($fx+ k sz) ($fx+ k size))]
                      [else (die who "number does not fit" n)]))
                  (die who "value must be positive" n))]
             [else (die who "invalid value argument" n)])]
          [(big)
           (cond
             [(fixnum? n) (bufx-set! x k n ($fx+ k size) who n)]
             [(bignum? n)
              (if ($bignum-positive? n)
                  (let ([sz (bignum-bytes n)])
                    (cond
                      [($fx<= sz size) 
                       (bbn-copy! x ($fx+ k size) n 0 sz)]
                      [($fx< sz size)
                       (bbn-copy! x ($fx+ k size) n 0 sz)
                       (bv-zero! x k ($fx+ k ($fx- size sz)))]
                      [else (die who "number does not fit" n)]))
                  (die who "value must be positive" n))]
             [else (die who "invalid value argument" n)])]
          [else (die who "invalid endianness" endianness)])))
    (define bytevector-uint-set! (make-bytevector-uint-set! 'bytevector-uint-set!))
    (define (make-bytevector-sint-set! who)
      (define bbn-neg-copy! (make-bbn-neg-copy! who))
      (define bbn-pos-copy! (make-bbn-pos-copy! who))
      (define lbn-neg-copy! (make-lbn-neg-copy! who))
      (define lbn-pos-copy! (make-lbn-pos-copy! who))
      (lambda (x k n endianness size)
        (unless (bytevector? x) (die who "not a bytevector" x))
        (unless (and (fixnum? k) ($fx>= k 0)) (die who "invalid index" k))
        (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
        (case endianness
          [(little)
           (cond
             [(fixnum? n) (lsfx-set! x k n ($fx+ k size) who n)]
             [(bignum? n)
              (if ($bignum-positive? n)
                  (let ([sz (bignum-bytes n)])
                    (cond
                      [($fx<= sz size) 
                       (lbn-pos-copy! x k n 0 size sz 255)]
                      [else (die who "number does not fit" n)]))
                  (let ([sz (bignum-bytes n)])
                    (cond
                      [($fx<= sz size) 
                       (lbn-neg-copy! x k n 0 size sz 256)]
                      [else (die who "number does not fit" n)])))]
             [else (die who "invalid value argument" n)])]
          [(big)
           (cond
             [(fixnum? n) (bsfx-set! x k n ($fx+ k size) who n)]
             [(bignum? n)
              (if ($bignum-positive? n)
                  (let ([sz (bignum-bytes n)])
                    (cond
                      [($fx<= sz size) 
                       (bbn-pos-copy! x k n 0 size sz 255)]
                      [else (die who "number does not fit" n)]))
                  (let ([sz (bignum-bytes n)])
                    (cond
                      [($fx<= sz size) 
                       (bbn-neg-copy! x k n 0 size sz 256)]
                      [else (die who "number does not fit" n)])))] 
             [else (die who "invalid value argument" n)])]
          [else (die who "invalid endianness" endianness)])))
    (define bytevector-sint-set! (make-bytevector-sint-set! 'bytevector-sint-set!)))

  (module (uint-list->bytevector sint-list->bytevector)
    (define (make-xint-list->bytevector who bv-set!)
      (define (race h t ls idx endianness size)
        (if (pair? h)
            (let ([h ($cdr h)] [a ($car h)])
               (if (pair? h)
                   (if (not (eq? h t))
                       (let ([bv (race ($cdr h) ($cdr t) ls 
                                       ($fx+ idx ($fx+ size size))
                                       endianness size)])
                         (bv-set! bv idx a endianness size)
                         (bv-set! bv ($fx+ idx size) ($car h) endianness size)
                         bv)
                       (die who "circular list" ls))
                   (if (null? h)
                       (let ([bv (make-bytevector ($fx+ idx size))])
                         (bv-set! bv idx a endianness size)
                         bv)
                       (die who "not a proper list" ls))))
            (if (null? h)
                (make-bytevector idx)
                (die who "not a proper list" ls))))
      (lambda (ls endianness size)
        (race ls ls ls 0 endianness size)))
    (define uint-list->bytevector 
      (make-xint-list->bytevector 
        'uint-list->bytevector bytevector-uint-set!))
    (define sint-list->bytevector 
      (make-xint-list->bytevector 
        'sint-list->bytevector bytevector-sint-set!)))

  (define (bytevector-ieee-double-native-ref bv i) 
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 7))
                 ($fx< i ($bytevector-length bv)))
            ($bytevector-ieee-double-native-ref bv i)
            (die 'bytevector-ieee-double-native-ref "invalid index" i))
        (die 'bytevector-ieee-double-native-ref "not a bytevector" bv)))

  (define (bytevector-ieee-single-native-ref bv i)
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 3))
                 ($fx< i ($bytevector-length bv)))
            ($bytevector-ieee-single-native-ref bv i)
            (die 'bytevector-ieee-single-native-ref "invalid index" i))
        (die 'bytevector-ieee-single-native-ref "not a bytevector" bv)))

  (define (bytevector-ieee-double-native-set! bv i x) 
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 7))
                 ($fx< i ($bytevector-length bv)))
            (if (flonum? x) 
                ($bytevector-ieee-double-native-set! bv i x)
                (die 'bytevector-ieee-double-native-set! "not a flonum" x))
            (die 'bytevector-ieee-double-native-set! "invalid index" i))
        (die 'bytevector-ieee-double-native-set! "not a bytevector" bv)))
   
  (define (bytevector-ieee-single-native-set! bv i x) 
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 3))
                 ($fx< i ($bytevector-length bv)))
            (if (flonum? x) 
                ($bytevector-ieee-single-native-set! bv i x)
                (die 'bytevector-ieee-single-native-set! "not a flonum" x))
            (die 'bytevector-ieee-single-native-set! "invalid index" i))
        (die 'bytevector-ieee-single-native-set! "not a bytevector" bv)))

  (define (bytevector-ieee-double-ref bv i endianness) 
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 7))
                 ($fx< i ($bytevector-length bv)))
            (case endianness
              [(little) ($bytevector-ieee-double-native-ref bv i)]
              [(big) ($bytevector-ieee-double-nonnative-ref bv i)]
              [else (die 'bytevector-ieee-double-ref 
                      "invalid endianness" endianness)])
            (die 'bytevector-ieee-double-ref "invalid index" i))
        (die 'bytevector-ieee-double-ref "not a bytevector" bv)))

  (define (bytevector-ieee-single-ref bv i endianness) 
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 3))
                 ($fx< i ($bytevector-length bv)))
            (case endianness
              [(little) ($bytevector-ieee-single-native-ref bv i)]
              [(big) ($bytevector-ieee-single-nonnative-ref bv i)]
              [else (die 'bytevector-ieee-single-ref 
                      "invalid endianness" endianness)])
            (die 'bytevector-ieee-single-ref "invalid index" i))
        (die 'bytevector-ieee-single-ref "not a bytevector" bv)))

  (define (bytevector-ieee-double-set! bv i x endianness) 
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 7))
                 ($fx< i ($bytevector-length bv)))
            (if (flonum? x)
                (case endianness
                  [(little) ($bytevector-ieee-double-native-set! bv i x)]
                  [(big) ($bytevector-ieee-double-nonnative-set! bv i x)]
                  [else (die 'bytevector-ieee-double-set! 
                          "invalid endianness" endianness)])
                (die 'bytevector-ieee-double-set! "not a flonum" x))
            (die 'bytevector-ieee-double-set! "invalid index" i))
        (die 'bytevector-ieee-double-set! "not a bytevector" bv)))

  (define (bytevector-ieee-single-set! bv i x endianness) 
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 3))
                 ($fx< i ($bytevector-length bv)))
            (if (flonum? x)
                (case endianness
                  [(little) ($bytevector-ieee-single-native-set! bv i x)]
                  [(big) ($bytevector-ieee-single-nonnative-set! bv i x)]
                  [else (die 'bytevector-ieee-single-set! 
                          "invalid endianness" endianness)])
                (die 'bytevector-ieee-single-set! "not a flonum" x))
            (die 'bytevector-ieee-single-set! "invalid index" i))
        (die 'bytevector-ieee-single-set! "not a bytevector" bv)))


  (define ($bytevector-ref/64 bv i who decoder endianness)
    (if (bytevector? bv) 
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 7))
                 ($fx< i ($bytevector-length bv)))
            (case endianness
              [(little big) 
               (decoder bv i endianness 8)]
              [else (die who "invalid endianness" endianness)])
            (die who "invalid index" i))
        (die who "not a bytevector" bv)))

  (define (bytevector-u64-native-ref bv i) 
    ($bytevector-ref/64 bv i 'bytevector-u64-native-ref 
       bytevector-uint-ref 'little))
  (define (bytevector-s64-native-ref bv i) 
    ($bytevector-ref/64 bv i 'bytevector-s64-native-ref 
       bytevector-sint-ref 'little))
  (define (bytevector-u64-ref bv i endianness)
    ($bytevector-ref/64 bv i 'bytevector-u64-native-ref 
       bytevector-uint-ref endianness))
  (define (bytevector-s64-ref bv i endianness)
    ($bytevector-ref/64 bv i 'bytevector-s64-native-ref 
       bytevector-sint-ref endianness))

  (define ($bytevector-set/64 bv i n lo hi who setter endianness)
    (if (bytevector? bv)
        (if (and (fixnum? i) 
                 ($fx>= i 0)
                 ($fxzero? ($fxlogand i 7))
                 ($fx< i ($bytevector-length bv)))
            (case endianness
              [(little big) 
               (unless (or (fixnum? n) (bignum? n))
                 (die who 
                   (if (number? n) 
                       "number is not exact" 
                       "not a number")
                   n))
               (unless (and (<= lo n) (< n hi))
                 (die who "number out of range" n))
               (setter bv i n endianness 8)]
              [else (die who "invalid endianness" endianness)])
            (die who "invalid index" i))
        (die who "not a bytevector" bv)))

  (define (bytevector-u64-native-set! bv i n) 
    ($bytevector-set/64 bv i n 0 (expt 2 64)
       'bytevector-u64-native-ref bytevector-uint-set! 'little))
  (define (bytevector-s64-native-set! bv i n) 
    ($bytevector-set/64 bv i n (- (expt 2 63)) (expt 2 63)
       'bytevector-s64-native-ref bytevector-sint-set! 'little))
  (define (bytevector-u64-set! bv i n endianness) 
    ($bytevector-set/64 bv i n 0 (expt 2 64)
       'bytevector-u64-ref bytevector-uint-set! endianness))
  (define (bytevector-s64-set! bv i n endianness) 
    ($bytevector-set/64 bv i n (- (expt 2 63)) (expt 2 63) 
       'bytevector-s64-ref bytevector-sint-set! endianness))

  )


