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


(library (ikarus writer)
  (export write display format printf fprintf print-error print-unicode print-graph)
  (import 
    (rnrs hashtables)
    (ikarus system $chars)
    (ikarus system $strings)
    (ikarus system $vectors)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $symbols)
    (ikarus system $bytevectors)
    (ikarus system $transcoders)
    (only (ikarus unicode-data) unicode-printable-char?) 
    (except (ikarus) 
      write display format printf fprintf print-error print-unicode print-graph))

  (define print-unicode
    (make-parameter #t))

  (define char-table ; first nonprintable chars
    '#("nul" "x1" "x2" "x3" "x4" "x5" "x6" "alarm"
       "backspace" "tab" "linefeed" "vtab" "page" "return" "xE" "xF" 
       "x10" "x11" "x12" "x13" "x14" "x15" "x16" "x17" 
       "x18" "x19" "x1A" "esc" "x1C" "x1D" "x1E" "x1F" 
       "space"))

  (define write-positive-hex-fx
    (lambda (n p)
      (unless ($fx= n 0)
        (write-positive-hex-fx ($fxsra n 4) p)
        (let ([n ($fxlogand n #xF)])
          (cond
            [($fx<= n 9)
             (write-char ($fixnum->char 
                           ($fx+ ($char->fixnum #\0) n)) 
                         p)]
            [else
             (write-char ($fixnum->char
                           ($fx+ ($char->fixnum #\A) ($fx- n 10)))
                         p)])))))

  (define write-character
    (lambda (x p m)
      (if m 
          (let ([i ($char->fixnum x)])
            (write-char #\# p)
            (cond
             [(fx< i (vector-length char-table))
              (write-char #\\ p)
              (write-char* (vector-ref char-table i) p)]
             [(fx< i 127)
              (write-char #\\ p)
              (write-char x p)]
             [(fx= i 127) 
              (write-char #\\ p)
              (write-char* "delete" p)]
             [(and (print-unicode) (unicode-printable-char? x))
              (write-char #\\ p)
              (write-char x p)]
             [else
              (write-char #\\ p)
              (write-char #\x p)
              (write-positive-hex-fx i p)]))
          (write-char x p))))

  (define write-list
    (lambda (x p m h i)
      (cond
       [(and (pair? x) 
             (or (not (hashtable-ref h x #f))
                 (fxzero? (hashtable-ref h x 0))))
        (write-char #\space p)
        (write-list (cdr x) p m h 
          (writer (car x) p m h i))]
       [(null? x) i]
       [else
        (write-char #\space p)
        (write-char #\. p)
        (write-char #\space p)
        (writer x p m h i)])))

  (define write-vector
    (lambda (x p m h i)
      (write-char #\# p)
      (write-char #\( p)
      (let ([n (vector-length x)])
        (let ([i 
               (cond
                 [(fx> n 0)
                  (let f ([idx 1] [i (writer (vector-ref x 0) p m h i)])
                    (cond
                      [(fx= idx n)
                       i]
                      [else
                       (write-char #\space p)
                       (f (fxadd1 idx)
                          (writer (vector-ref x idx) p m h i))]))]
                 [else i])])
           (write-char #\) p)
           i))))

  (define write-bytevector
    (lambda (x p m h i)
      (write-char #\# p)
      (write-char #\v p)
      (write-char #\u p)
      (write-char #\8 p)
      (write-char #\( p)
      (let ([n ($bytevector-length x)])
        (when (fx> n 0)
          (write-fixnum ($bytevector-u8-ref x 0) p)
          (let f ([idx 1] [n n] [x x] [p p])
            (unless ($fx= idx n)
              (write-char #\space p)
              (write-fixnum ($bytevector-u8-ref x idx) p)
              (f (fxadd1 idx) n x p)))))
      (write-char #\) p)
      i))

  (define write-struct
    (lambda (x p m h i)
      (write-char #\# p)
      (write-char #\[ p)
      (let ([i (writer (struct-name x) p m h i)])
        (let ([n (struct-length x)])
          (let f ([idx 0] [i i])
            (cond
              [(fx= idx n)
               (write-char #\] p)
               i]
              [else 
               (write-char #\space p)
               (f (fxadd1 idx) 
                  (writer (struct-ref x idx) p m h i))]))))))
  
  (define initial?
    (lambda (c)
      (or (letter? c) (special-initial? c))))
  
  (define letter?
    (lambda (c)
      (or (and ($char<= #\a c) ($char<= c #\z))
          (and ($char<= #\A c) ($char<= c #\Z)))))
  
  (define digit?
    (lambda (c)
      (and ($char<= #\0 c) ($char<= c #\9))))
  
  (define special-initial? 
    (lambda (x)
      (memq x '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))))
  
  (define subsequent?
    (lambda (x)
      (or (initial? x)
          (digit? x)
          (special-subsequent? x))))
  
  (define special-subsequent?
    (lambda (x) 
      (memq x '(#\+ #\- #\. #\@))))
  
  (define subsequent*?
    (lambda (str i n)
      (or ($fx= i n)
          (and (subsequent? ($string-ref str i))
               (subsequent*? str ($fxadd1 i) n)))))
  
  (define peculiar-symbol-string?
    (lambda (str)
      (let ([n (string-length str)])
        (cond
          [(fx= n 1) 
           (memq (string-ref str 0) '(#\+ #\-))]
          [(fx>= n 2)
           (or (and (char=? (string-ref str 0) #\-)
                    (char=? (string-ref str 1) #\>)
                    (subsequent*? str 2 n))
               (string=? str "..."))]
          [else #f]))))

  (define valid-symbol-string?
    (lambda (str)
      (define normal-symbol-string?
        (lambda (str)
          (let ([n ($string-length str)])
            (and ($fx>= n 1)
                 (initial? ($string-ref str 0))
                 (subsequent*? str 1 n)))))
      (or (normal-symbol-string? str)
          (peculiar-symbol-string? str))))
  
  (define write-symbol-bar-esc-loop
    (lambda (x i n p)
      (unless ($fx= i n)
        (let* ([c ($string-ref x i)]
               [b ($char->fixnum c)])
          (cond
            [($fx< b 32)
             (cond
               [($fx< b 7) 
                (write-inline-hex b p)]
               [($fx< b 14)
                (write-char #\\ p)
                (write-char (string-ref "abtnvfr" ($fx- b 7)) p)]
               [else 
                (write-inline-hex b p)])]
            [(memq c '(#\\ #\|))
             (write-char #\\ p)
             (write-char c p)]
            [($fx< b 127) 
             (write-char c p)]
            [else 
             (write-inline-hex b p)]))
        (write-symbol-bar-esc-loop x ($fxadd1 i) n p))))
  
  (define write-symbol-bar-esc
    (lambda (x p)
      (write-char #\| p)
      (write-symbol-bar-esc-loop x 0 ($string-length x) p)
      (write-char #\| p)))
  
  (define-syntax ascii-map
    (lambda (x)
      ;;; r6rs prohibits bytevectors from being "datum"s
      ;;; oh well.
      (syntax-case x ()
        [(stx str) (string? (syntax->datum #'str))
         (let ([s (syntax->datum #'str)]
               [bv (make-bytevector 16 0)])
           (for-each
             (lambda (c)
               (let ([b (char->integer c)])
                 (let ([i (fxlogand b 7)]
                       [j (fxsra b 3)])
                   (bytevector-u8-set! bv j
                     (fxlogor (bytevector-u8-ref bv j)
                       (fxsll 1 i))))))
             (string->list s))
           (with-syntax ([bv (datum->syntax #'stx bv)])
             #'(quote bv)))])))

  (define subsequents-map 
   (ascii-map
     "!$%&*/:<=>?^_~+-.@abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
  (define initials-map
   (ascii-map
     "!$%&*/:<=>?^_~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))


  (define (in-map? byte map)
    (let ([i ($fxlogand byte 7)]
          [j ($fxsra byte 3)])
      (and 
        (fx< j ($bytevector-length map))
        (let ([mask ($fxsll 1 i)])
           (not ($fxzero? 
                  ($fxlogand mask
                    ($bytevector-u8-ref map j))))))))


  (define (write-subsequent* str i j p)
    (unless ($fx= i j) 
      (let* ([c ($string-ref str i)]
             [b ($char->fixnum c)])
        (cond
          [(in-map? b subsequents-map) 
           (write-char c p)]
          [($fx< b 128) 
           (write-inline-hex b p)]
          [(unicode-printable-char? c)
           (write-char c p)]
          [else 
           (write-inline-hex b p)]))
      (write-subsequent* str ($fxadd1 i) j p)))

  (define write-symbol-hex-esc
    (lambda (str p)
      (let ([n ($string-length str)])
        (cond
          [($fx= n 0) 
           (write-char #\| p) 
           (write-char #\| p)]
          [else
           (let* ([c0 ($string-ref str 0)]
                  [b0 ($char->fixnum c0)])
             (cond
               [(in-map? b0 initials-map) 
                (write-char c0 p)]
               [($fx< b0 128) (write-inline-hex b0 p)]
               [(unicode-printable-char? c0) (write-char c0 p)]
               [else (write-inline-hex b0 p)])
             (write-subsequent* str 1 n p))]))))


  (define (write-peculiar str p)
    (let ([n ($string-length str)])
      (cond
        [($fx= n 1) 
         (write-char ($string-ref str 0) p)]
        [(and ($fx>= n 2) 
              ($char= ($string-ref str 0) #\-)
              ($char= ($string-ref str 1) #\>))
         (write-char #\- p)
         (write-char #\> p)
         (write-subsequent* str 2 n p)]
        [(string=? str "...")
         (write-char #\. p)
         (write-char #\. p)
         (write-char #\. p)]
        [else (error 'write-peculiear "BUG")])))

  (define write-symbol
    (lambda (x p m)
      (write-symbol-string (symbol->string x) p m)))

  (define write-symbol-string
    (lambda (str p m)
      (if m
          (if (peculiar-symbol-string? str)
              (write-peculiar str p)
              (write-symbol-hex-esc str p))
          (write-char* str p))))
  
  (define write-gensym
    (lambda (x p m h i)
      (cond
        [(and m (print-gensym)) =>
         (lambda (gensym-how)
           (case gensym-how
             [(pretty)
              (let ([str (symbol->string x)])
                (write-char #\# p)
                (write-char #\: p)
                (write-symbol-string str p m))]
             [else
              (let ([str (symbol->string x)]
                    [ustr (gensym->unique-string x)])
                (write-char #\# p)
                (write-char #\{ p)
                (write-symbol-string str p m)
                (write-char #\space p)
                (write-symbol-bar-esc ustr p)
                (write-char #\} p))])
           i)]
        [else 
         (write-symbol x p m)
         i])))
  
  (define write-inline-hex
    (lambda (b p)
      (write-char #\\ p)
      (write-char #\x p)
      (if ($fxzero? b) 
          (write-char #\0 p)
          (write-positive-hex-fx b p))
      (write-char #\; p)))

  (define write-string-escape
    (lambda (x p)
      (define loop 
        (lambda (x i n p)
         (unless (fx= i n)
           (let* ([c (string-ref x i)]
                  [b ($char->fixnum c)])
             (cond
               [($fx< b 32) 
                (cond
                  [($fx< b 7) 
                   (write-inline-hex b p)]
                  [($fx< b 14)
                   (write-char #\\ p)
                   (write-char (string-ref "abtnvfr" ($fx- b 7)) p)]
                  [else 
                   (write-inline-hex b p)])]
               [(or ($char= #\" c) ($char= #\\ c))
                (write-char #\\ p)
                (write-char c p)]
               [($fx< b 127)
                (write-char c p)]
               [(unicode-printable-char? c) 
                (write-char c p)]
               [else (write-inline-hex b p)]))
           (loop x (fxadd1 i) n p)))) 
      (write-char #\" p)
      (loop x 0 (string-length x) p)
      (write-char #\" p)))
  
  (define write-string
    (lambda (x p m)
      (if m
          (write-string-escape x p)
          (write-char* x p))))
  
  (define write-fixnum
    (lambda (x p)
      (define loop
        (lambda (x p)
          (unless (fxzero? x)
            (loop (fxquotient x 10) p)
            (write-char 
               ($fixnum->char
                  ($fx+ (fxremainder x 10)
                        ($char->fixnum #\0)))
               p))))
      (cond
       [(fxzero? x) (write-char #\0 p)]
       [(fx< x 0)
        (write-char #\- p)
        (if (fx= x -536870912)
            (write-char* "536870912" p)
            (loop (fx- 0 x) p))]
       [else (loop x p)])))
  
  (define write-char*
    (lambda (x p)
      (define loop 
        (lambda (x i n p)
         (unless (fx= i n)
           (write-char (string-ref x i) p)
           (loop x (fxadd1 i) n p)))) 
      (loop x 0 (string-length x) p)))
  
  (define macro
    (lambda (x h)
      (define macro-forms
        '([quote .             "'"]
          [quasiquote .        "`"]
          [unquote .           ","]
          [unquote-splicing .  ",@"]
          [syntax .            "#'"]
          [quasisyntax .       "#`"]
          [unsyntax .          "#,"]
          [unsyntax-splicing . "#,@"]
          [|#primitive| .     "#%"]))
      (and (pair? x)
           (let ([d ($cdr x)])
             (and (pair? d)
                  (null? ($cdr d))
                  (not (hashtable-ref h x #f))))
           (assq ($car x) macro-forms))))
  
  (define write-pair
    (lambda (x p m h i)
      (cond
        [(macro x h) =>
         (lambda (a) 
           (display (cdr a) p) 
           (writer (cadr x) p m h i))]
        [else
         (write-char #\( p)
         (let ([i (writer (car x) p m h i)])
           (let ([i (write-list (cdr x) p m h i)])
             (write-char #\) p)
             i))])))
  
  (define write-ref
    (lambda (n p)
      (write-char #\# p)
      (write-fixnum (fx- -1 n) p)
      (write-char #\# p)))
  
  (define write-mark
    (lambda (n p)
      (write-char #\# p)
      (write-fixnum (fx- -1 n) p)
      (write-char #\= p)))
  
  (define write-shareable
    (lambda (x p m h i k)
      (cond
        [(hashtable-ref h x #f) =>
         (lambda (n)
           (cond
             [(fx< n 0) 
              (write-ref n p)
              i]
             [(fx= n 0)
              (k x p m h i)]
             [else
              (let ([i (fx- i 1)])
                (hashtable-set! h x i)
                (write-mark i p)
                (k x p m h i))]))]
        [else (k x p m h i)])))
  
  (define writer
    (lambda (x p m h i)
      (cond
        [(pair? x) 
         (write-shareable x p m h i write-pair)]
        [(symbol? x)
         (if (gensym? x)
             (write-gensym x p m h i)
             (begin (write-symbol x p m) i))]
        [(fixnum? x)
         (write-fixnum x p)
         i]
        [(string? x)
         (write-string x p m)
         i]
        [(boolean? x)
         (write-char* (if x "#t" "#f") p)
         i]
        [(char? x)
         (write-character x p m)
         i]
        [(procedure? x)
         (cond
           [(let ([name (procedure-annotation x)])
              (and (symbol? name) name)) =>
            (lambda (name) 
              (write-char* "#<procedure " p) 
              (write-char* (symbol->string name) p)
              (write-char* ">" p))]
           [else (write-char* "#<procedure>" p)])
         i]
        [(output-port? x)
         (write-char* "#<output-port " p)
         (let ([i (writer (output-port-name x) p #t h i)])
           (write-char #\> p)
           i)]
        [(input-port? x)
         (write-char* "#<input-port " p)
         (let ([i (writer (input-port-name x) p #t h i)])
           (write-char #\> p)
           i)]
        [(vector? x)
         (write-shareable x p m h i write-vector)]
        [(bytevector? x)
         (write-shareable x p m h i write-bytevector)]
        [(null? x) 
         (write-char #\( p)
         (write-char #\) p)
         i]
        [(eq? x (void)) 
         (write-char* "#<void>" p)
         i]
        [(eof-object? x)
         (write-char* "#!eof" p)
         i]
        [(bwp-object? x)
         (write-char* "#!bwp" p)
         i]
        [(hashtable? x)
         (write-char* "#<hashtable>" p)
         i]
        [(struct? x)
         (let ([printer (struct-printer x)])
           (if (procedure? printer)
               (begin (printer x p) i)
               (write-shareable x p m h i write-struct)))]
        [(code? x)
         (write-char* "#<code>" p)]
        [($unbound-object? x)
         (write-char* "#<unbound-object>" p)
         i]
       ;;; [($forward-ptr? x) FIXME reinstate
       ;;;  (write-char* "#<forward-ptr>" p)
       ;;;  i]
        [(number? x)
         (write-char* (number->string x) p)
         i]
        [($transcoder? x) 
         (write-char* "#<transcoder " p)
         (let ([n ($transcoder->data x)])
           (write-char* (number->string n) p))
         (write-char* ">" p)]
        [else 
         (write-char* "#<unknown>" p)
         i])))
  
  (define print-graph (make-parameter #f))
  
  (define (hasher x h)
    (define (vec-graph x i j h)
      (unless (fx= i j)
        (graph (vector-ref x i) h)
        (vec-graph x (fxadd1 i) j h)))
    (define (vec-dynamic x i j h)
      (unless (fx= i j)
        (dynamic (vector-ref x i) h)
        (vec-dynamic x (fxadd1 i) j h))) 
    (define (graph x h)
      (cond
        [(pair? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (graph (car x) h)
            (graph (cdr x) h)])]
        [(vector? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (vec-graph x 0 (vector-length x) h)])]
        [(gensym? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (hashtable-set! h x (fxadd1 n)))])]))
    (define (dynamic x h)
      (cond
        [(pair? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (dynamic (car x) h)
            (dynamic (cdr x) h)
            (when (and (hashtable-ref h x #f)
                       (fxzero? (hashtable-ref h x #f)))
              (hashtable-set! h x #f))])]
        [(vector? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (vec-dynamic x 0 (vector-length x) h)
            (when (and (hashtable-ref h x #f)
                       (fxzero? (hashtable-ref h x #f)))
              (hashtable-set! h x #f))])])) 
    (if (print-graph) 
        (graph x h)
        (dynamic x h)))
  
  (define (write-to-port x p)
    (let ([h (make-eq-hashtable)])
      (hasher x h)
      (writer x p #t h 0))
    (flush-output-port p))
  
  (define (display-to-port x p)
    (let ([h (make-eq-hashtable)])
      (hasher x h)
      (writer x p #f h 0))
    (flush-output-port p))
  
  (define formatter
    (lambda (who p fmt args)
      (let f ([i 0] [args args])
        (unless (fx= i (string-length fmt))
          (let ([c (string-ref fmt i)])
            (cond
              [($char= c #\~)
               (let ([i (fxadd1 i)])
                 (when (fx= i (string-length fmt))
                   (error who "invalid ~ at end of format string" fmt))
                 (let ([c (string-ref fmt i)])
                  (cond
                    [($char= c #\~) 
                     (write-char #\~ p)
                     (f (fxadd1 i) args)]
                    [($char= c #\%) 
                     (write-char #\newline p)
                     (f (fxadd1 i) args)] 
                    [($char= c #\a)
                     (when (null? args)
                       (error who "insufficient arguments"))
                     (display-to-port (car args) p)
                     (f (fxadd1 i) (cdr args))]
                    [($char= c #\s)
                     (when (null? args)
                       (error who "insufficient arguments"))
                     (write-to-port (car args) p)
                     (f (fxadd1 i) (cdr args))]
                    [(assv c '([#\b . 2] [#\o . 8] [#\x . 16] [#\d . 10]))
                     =>
                     (lambda (x)
                       (when (null? args)
                         (error who "insufficient arguments"))
                       (let ([a (car args)])
                         (cond
                           [(or (fixnum? a) (bignum? a) (ratnum? a))
                            (display-to-port (number->string a (cdr x)) p)]
                           [(flonum? a)
                            (unless (eqv? c #\d) 
                              (error who 
                                (format "flonums cannot be printed with ~~~a" c)))
                            (display-to-port (number->string a) p)]
                           [else 
                            (error who "not a number" a)]))
                       (f (fxadd1 i) (cdr args)))]
                    [else
                     (error who "invalid sequence character after ~" c)])))]
              [else 
               (write-char c p)
               (f (fxadd1 i) args)]))))
      (flush-output-port p)))
  
  (define fprintf
    (lambda (port fmt . args)
      (unless (output-port? port) 
        (error 'fprintf "not an output port" port))
      (unless (string? fmt)
        (error 'fprintf "not a string" fmt))
      (formatter 'fprintf port fmt args)))

  (define display-error
    (lambda (errname who fmt args)
      (unless (string? fmt)
        (error 'print-error "not a string" fmt))
      (let ([p (standard-error-port)])
        (if who
            (fprintf p "~a in ~a: " errname who)
            (fprintf p "~a: " errname))
        (formatter 'print-error p fmt args)
        (write-char #\. p)
        (newline p))))
  
  (define format
    (lambda (fmt . args)
      (unless (string? fmt)
        (error 'format "not a string" fmt))
      (let ([p (open-output-string)])
        (formatter 'format p fmt args)
        (get-output-string p))))
   
  (define printf 
    (lambda (fmt . args)
      (unless (string? fmt)
        (error 'printf "not a string" fmt))
      (formatter 'printf (current-output-port) fmt args)))
  
  (define write 
    (case-lambda
      [(x) (write-to-port x (current-output-port))]
      [(x p)
       (unless (output-port? p) 
         (error 'write "not an output port" p))
       (write-to-port x p)]))

  (define display 
    (case-lambda
      [(x) (display-to-port x (current-output-port))]
      [(x p)
       (unless (output-port? p) 
         (error 'display "not an output port" p))
       (display-to-port x p)]))

  (define print-error 
    (lambda (who fmt . args)
      (display-error "Error" who fmt args)))

  (define warning 
    (lambda (who fmt . args)
      (display-error "Warning" who fmt args)))

  )

