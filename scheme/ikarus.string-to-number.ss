
(library (ikarus.string-to-number)
  (export string->number)
  (import (except (ikarus) string->number))

  (module (string->number)
    (define who 'string->number)
    (define (do-sn/ex sn ex ac)
      (* sn (if (eq? ex 'i) (inexact ac) ac)))
    (define (do-dec-sn/ex sn ex ac)
      (* sn (if (eq? ex 'e) ac (inexact ac))))
    (define (digit c r)
      (let ([n (fx- (char->integer c) (char->integer #\0))])
        (cond
          [(and (fx>=? n 0) (fx< n r)) n]
          [(eqv? r 16)
           (let ([n (fx- (char->integer c) (char->integer #\a))])
             (cond
               [(and (fx>=? n 0) (fx< n 6)) (+ n 10)]
               [else
                (let ([n (fx- (char->integer c) (char->integer #\A))])
                  (cond
                    [(and (fx>=? n 0) (fx< n 6)) (+ n 10)]
                    [else #f]))]))]
          [else #f])))

    (module (define-parser)
      (define-syntax gen-empty
        (syntax-rules (eof)
          [(_ C Ca) (C FAIL Ca)]
          [(_ C Ca [(eof) then] . rest) then]
          [(_ C Ca other . rest) (gen-empty C Ca . rest)]))
      (define-syntax gen-char
        (syntax-rules (eof =>)
          [(_ C Ca c) (C FAIL Ca)]
          [(_ C Ca c [(eof) then] . rest)
           (gen-char C Ca c . rest)]
          [(_ C Ca c [(test . args) => result then] . rest)
           (cond
             [(test c . args) => 
              (lambda (result) then)]
             [else (gen-char C Ca c . rest)])]
          [(_ C Ca c [ls then] . rest)
           (if (memv c 'ls)
               then
               (gen-char C Ca c . rest))]))
      (define-syntax gen-clause
        (syntax-rules ()
          [(_ (Ca ...) C next fail name (arg* ...) (clause* ...))
           (define (name Ca ... arg* ...)
             (define-syntax fail
               (syntax-rules ()
                 [(_) (C FAIL (Ca ...))]))
             (cond
               [(C GEN-EOF? (Ca ...))
                (gen-empty C (Ca ...) clause* ...)]
               [else
                (let ([c (C GEN-REF (Ca ...))])
                  (define-syntax next
                    (syntax-rules ()
                      [(_ who args (... ...))
                       (C GEN-NEXT (Ca ...) who args (... ...))]))
                  (gen-char C (Ca ...) c clause* ...))]))]))
      (define-syntax define-parser
        (syntax-rules ()
          [(_ (entries ...) config next fail
             [name* (arg** ...) clause** ...] ...)
           (begin
             (module M (entries ...)
               (config GEN-ARGS 
                  gen-clause config next fail name* 
                  (arg** ...)
                  (clause** ...))
               ...)
             (import M))])))

    (define-syntax string-config
      (syntax-rules (GEN-EOF? GEN-REF GEN-ARGS GEN-NEXT FAIL)
        [(_ GEN-EOF? (s n i)) (fx=? i n)]
        [(_ GEN-REF (s n i)) (string-ref s i)]
        [(_ GEN-ARGS k . rest) (k (s n i) . rest)]
        [(_ GEN-NEXT (s n i) who . rest) 
         (who s n (fx+ i 1) . rest)]
        [(_ FAIL (s n i)) #f]))

    (define-parser (do-parse) string-config next fail

      (ratio+ (r ex sn num ac)
        [(eof)
         (if (= ac 0)
             (fail)
             (do-sn/ex sn ex (/ num ac)))]
        [(digit r) => d
         (next ratio+ r ex sn num (+ (* ac r) d))]
        [(#\+)
         (if (= ac 0)
             (fail)
             (let ([real (do-sn/ex sn ex (/ num ac))])
               (next im:sign r real ex +1)))]
        [(#\-)
         (if (= ac 0)
             (fail)
             (let ([real (do-sn/ex sn ex (/ num ac))])
               (next im:sign r real ex -1)))]
        [(#\i)
         (if (= ac 0)
             (fail)
             (make-rectangular 0 (do-sn/ex sn ex (/ num ac))))])

      (im:ratio+ (r real ex sn num ac)
        [(digit r) => d
         (next im:ratio+ r real ex sn num (+ (* ac r) d))]
        [(#\i) 
         (if (= ac 0)
             (fail)
             (next im:done 
               (make-rectangular real (do-sn/ex sn ex (/ num ac)))))])

      (im:done (n)
        [(eof) n])

      (ratio (r ex sn num)
        [(digit r) => d
         (next ratio+ r ex sn num d)])

      (im:ratio (r real ex sn num)
        [(digit r) => d
         (next im:ratio+ r real ex sn num d)])

      (exponent+digit (r ex sn ac exp1 exp2 exp-sign)
        [(eof)
         (do-dec-sn/ex sn ex (* ac (expt 10 (+ exp1 (* exp2 exp-sign)))))]
        [(digit r) => d
         (next exponent+digit r ex sn ac exp1 (+ (* exp2 r) d) exp-sign)])

      (exponent+sign (r ex sn ac exp1 exp-sign)
        [(digit r) => d
         (next exponent+digit r ex sn ac exp1 d exp-sign)])

      (exponent (r ex sn ac exp1)
        [(digit r) => d
         (next exponent+digit r ex sn ac exp1 d +1)]
        [(#\+) (next exponent+sign r ex sn ac exp1 +1)]
        [(#\-) (next exponent+sign r ex sn ac exp1 -1)])

      (digit+dot (r ex sn ac exp)
        [(eof)
         (do-dec-sn/ex sn ex (* ac (expt 10 exp)))]
        [(digit r) => d
         (next digit+dot r ex sn (+ (* ac r) d) (- exp 1))]
        [(#\+)
         (let ([real (do-dec-sn/ex sn ex (* ac (expt 10 exp)))])
           (next im:sign r real ex +1))]
        [(#\-)
         (let ([real (do-dec-sn/ex sn ex (* ac (expt 10 exp)))])
           (next im:sign r real ex -1))]
        [(#\i)
         (let ([real (do-dec-sn/ex sn ex (* ac (expt 10 exp)))])
           (next im:done (make-rectangular 0.0 real)))]
        [(#\e)
         (if (fx=? r 10)
             (next exponent r ex sn ac exp)
             (fail))])

      (digit+ (r ex sn ac)
        [(eof) (do-sn/ex sn ex ac)]
        [(digit r) => d
         (next digit+ r ex sn (+ (* ac r) d))]
        [(#\/) (next ratio r ex sn ac)]
        [(#\.)
         (if (fx=? r 10) 
             (next digit+dot r ex sn ac 0)
             (fail))]
        [(#\+)
         (let ([real (do-sn/ex sn ex ac)])
           (next im:sign r real ex +1))]
        [(#\-)
         (let ([real (do-sn/ex sn ex ac)])
           (next im:sign r real ex -1))]
        [(#\i)
         (make-rectangular 0 (do-sn/ex sn ex ac))]
        [(#\e)
         (if (fx=? r 10) 
             (next exponent r ex sn ac 0)
             (fail))])

      (im:digit+ (r real ex sn ac)
        [(digit r) => d
         (next im:digit+ r real ex sn (+ (* ac r) d))]
        [(#\/)
         (next im:ratio r real ex sn ac)]
        [(#\i)
         (next im:done (make-rectangular real (do-sn/ex sn ex ac)))])

      (sign-i (r ex sn)
        [(eof)
         (make-rectangular 
           (if (eq? ex 'i) 0.0 0)
           sn)]
        [(#\n) (next sign-in r sn)])
      (sign-in (r sn)
        [(#\f) (next sign-inf r sn)])
      (sign-inf (r sn)
        [(#\.) (next sign-inf. r sn)])
      (sign-inf. (r sn)
        [(#\0) (next sign-inf.0 r sn)])
      (sign-inf.0 (r sn)
        [(eof) (* sn +inf.0)]
        [(#\i) 
         (next im:done (make-rectangular 0.0 (* sn +inf.0)))])

      (im:sign-i (real ex sn)
        [(eof) (make-rectangular real (do-sn/ex sn ex 1))]
        [(#\n) (next im:sign-in (make-rectangular real (* sn +inf.0)))])
      (im:sign-in (n)
        [(#\f) (next im:sign-inf n)])
      (im:sign-inf (n)
        [(#\.) (next im:sign-inf. n)])
      (im:sign-inf. (n)
        [(#\0) (next im:sign-inf.0 n)])
      (im:sign-inf.0 (n)
        [(#\i) (next im:done n)])

      (dot (r ex sn)
        [(digit r) => d
         (next digit+dot r ex sn d -1)])

      (im:sign (r real ex sn)
        [(digit r) => d
         (next im:digit+ r real ex sn d)]
        [(#\i) 
         (next im:sign-i real ex sn)])
    
      (sign (r ex sn)
        [(digit r) => d
         (next digit+ r ex sn d)]
        [(#\i)
         (next sign-i r ex sn)]
        [(#\.)
         (if (fx=? r 10)
             (next dot r ex sn)
             (fail))])

      (do-parse-h (dr r ex)
        [(#\x #\X)
         (if r (fail) (next do-parse 16 16 ex))]
        [(#\o #\O)
         (if r (fail) (next do-parse 8 8 ex))]
        [(#\b #\B)
         (if r (fail) (next do-parse 2 2 ex))]
        [(#\d #\D)
         (if r (fail) (next do-parse 10 10 ex))]
        [(#\e #\E)
         (if ex (fail) (next do-parse dr r 'e))]
        [(#\i #\I)
         (if ex (fail) (next do-parse dr r 'i))])

      (do-parse (dr r ex)
        [(#\#) (next do-parse-h dr r ex)]
        [(#\+) (next sign dr ex +1)]
        [(#\-) (next sign dr ex -1)]
        [(#\.)
         (if (fx=? dr 10) 
             (next dot dr ex +1)
             (fail))]
        [(digit dr) => d
         (next digit+ dr ex +1 d)])
    )

    (define string->number
      (case-lambda
        [(s)
         (unless (string? s) (die who "not a string" s))
         (do-parse s (string-length s) 0 10 #f #f)]
        [(s r)
         (unless (string? s) (die who "not a string" s))
         (unless (memv r '(10 16 2 8)) (die who "invalid radix" r))
         (do-parse s (string-length s) 0 r #f #f)]))
    
    ))



;;; <number>          ::= <num 2>
;;;                     | <num 8>
;;;                     | <num 10> 
;;;                     | <num 16>
;;; <num R>           ::= <prefix R> <complex R>
;;; <complex R>       ::= <real R>
;;;                     | <real R> "@" <real R>
;;;                     | <real R> "+" <ureal R> "i"
;;;                     | <real R> "-" <ureal R> "i"
;;;                     | <real R> "+" <naninf> "i"
;;;                     | <real R> "-" <naninf> "i"
;;;                     | <real R> "+" "i"
;;;                     | <real R> "-"  "i"
;;;                     | "+" <ureal R> "i"
;;;                     | "-" <ureal R> "i"
;;;                     | "+" <naninf> "i"
;;;                     | "-" <naninf> "i"
;;;                     | "+" "i"
;;;                     | "-" "i"
;;; <real R>          ::= <sign> <ureal R>
;;;                     | "+" <naninf>
;;;                     | "-" <naninf>
;;; <naninf>          ::= "nan.0" 
;;;                     | "inf.0"
;;; <ureal R>           | <uinteger R>
;;;                     | <uinteger R> "/" <uinteger R>
;;;                     | <decimal R> <mantissa width>
;;; <decimal 10>      ::= <uinteger 10> <suffix>
;;;                     | "." <digit 10> + <suffix>
;;;                     | <digit 10> + "." <digit 10> * <suffix>
;;;                     | <digit 10> + "." <suffix>
;;; <uinteger R>      ::= <digit R> +
;;; <prefix R>          | <radix R> <exactness>
;;;                     | <exactness <radix R>
;;; <suffix>          ::= epsilon
;;;                     | <exponent-marker> <sign> <digit 10> +
;;; <exponent-marker> ::= "e"
;;;                     | "E"
;;;                     | "s"
;;;                     | "S"
;;;                     | "f"
;;;                     | "F"
;;;                     | "d"
;;;                     | "D"
;;;                     | "l"
;;;                     | "L"
;;; <mantissa-width>  ::= epsilon
;;;                     | "|" <digit +>
;;; <sign>            ::= epsilon
;;;                     | "+"
;;;                     | "-"
;;; <exactness>       ::= epsilon
;;;                     | "#i"
;;;                     | "#I"
;;;                     | "#e"
;;;                     | "#E"
;;; <radix-2>         ::= "#b"
;;;                     | "#B"
;;; <radix-8>         ::= "#o"
;;;                     | "#O"
;;; <radix-10>        ::= epsilon
;;;                     | "#d"
;;;                     | "#D"
;;; <radix-16>        ::= "#x"
;;;                     | "#X"
;;; <digit-2>         ::= "0"
;;;                     | "1"
;;; <digit-8>         ::= "0"
;;;                     | "1"
;;;                     | "2"
;;;                     | "3"
;;;                     | "4"
;;;                     | "5"
;;;                     | "6"
;;;                     | "7"
;;; <digit-10>        ::= <digit>
;;; <digit-16>        ::= <hex-digit>
;;; <digit>           ::= "0"
;;;                     | "1"
;;;                     | "2"
;;;                     | "3"
;;;                     | "4"
;;;                     | "5"
;;;                     | "6"
;;;                     | "7"
;;;                     | "8"
;;;                     | "9"
;;; <hex-digit>       ::= <hex>
;;;                     | "A"
;;;                     | "B"
;;;                     | "C"
;;;                     | "D"
;;;                     | "E"
;;;                     | "F"
;;;                     | "a"
;;;                     | "b"
;;;                     | "c"
;;;                     | "d"
;;;                     | "e"
;;;                     | "f"
