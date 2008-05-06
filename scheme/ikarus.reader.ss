;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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


(library (ikarus reader)
  (export read read-initial read-token comment-handler get-datum
          read-annotated read-script-annotated annotation?
          annotation-expression annotation-source
          annotation-stripped 
          read-library-source-file read-script-source-file)
  (import
    (ikarus system $chars)
    (ikarus system $fx)
    (ikarus system $pairs)
    (ikarus system $bytevectors)
    (only (io-spec) open-string-input-port/id)
    (only (ikarus unicode-data) unicode-printable-char?) 
    (except (ikarus) read-char read read-token comment-handler get-datum
      read-annotated read-script-annotated annotation?
      annotation-expression annotation-source annotation-stripped))

  (define (die/pos p off who msg arg*) 
    (define-condition-type &lexical-position &condition
        make-lexical-position-condition lexical-position?
        (file-name lexical-position-filename)
        (character lexical-position-character))
    (raise 
      (condition
        (make-lexical-violation) 
        (make-message-condition msg)
        (if (null? arg*) 
            (condition)
            (make-irritants-condition arg*))
        (make-lexical-position-condition 
          (port-id p) 
          (let ([pos (input-port-byte-position p)])
            (and pos (+ pos off)))))))
        
  (define (die/p p who msg . arg*)
    (die/pos p 0 who msg arg*))
  (define (die/p-1 p who msg . arg*)
    (die/pos p -1 who msg arg*))


  (define (checked-integer->char n ac p) 
    (define (valid-integer-char? n)
      (cond
        [(<= n #xD7FF)   #t]
        [(< n #xE000)    #f]
        [(<= n #x10FFFF) #t]
        [else            #f]))
    (if (valid-integer-char? n) 
        ($fixnum->char n)
        (die/p p 'tokenize 
          "invalid numeric value for character"
          (list->string (reverse ac)))))

  (define-syntax read-char 
    (syntax-rules ()
      [(_ p) (get-char p)]))

  (define delimiter?
    (lambda (c)
      (or (char-whitespace? c)
          (memq c '(#\( #\) #\[ #\] #\" #\# #\;)))))
  (define digit?
    (lambda (c)
      (and ($char<= #\0 c) ($char<= c #\9))))
  (define char->num
    (lambda (c)
      (fx- ($char->fixnum c) ($char->fixnum #\0))))
  (define initial?
    (lambda (c)
      (cond
        [($char<= c ($fixnum->char 127))
         (or (letter? c) (special-initial? c))]
        [else (unicode-printable-char? c)])))
  (define letter? 
    (lambda (c)
      (or (and ($char<= #\a c) ($char<= c #\z))
          (and ($char<= #\A c) ($char<= c #\Z)))))
  (define af? 
    (lambda (c)
      (or (and ($char<= #\a c) ($char<= c #\f))
          (and ($char<= #\A c) ($char<= c #\F)))))
  (define af->num
    (lambda (c)
      (if (and ($char<= #\a c) ($char<= c #\f))
          (fx+ 10 (fx- ($char->fixnum c) ($char->fixnum #\a)))
          (fx+ 10 (fx- ($char->fixnum c) ($char->fixnum #\A))))))
  (define special-initial?
    (lambda (c)
      (memq c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))))
  (define subsequent?
    (lambda (c)
      (or (initial? c) (digit? c) (special-subsequent? c))))
  (define special-subsequent?
    (lambda (c)
      (memq c '(#\+ #\- #\. #\@))))
  (define tokenize-identifier
    (lambda (ls p)
      (let ([c (peek-char p)])
        (cond
         [(eof-object? c) ls]
         [(subsequent? c)
          (tokenize-identifier (cons (read-char p) ls) p)]
         [(delimiter? c)
          ls]
         [(char=? c #\\)
          (read-char p)
          (tokenize-backslash ls p)]
         [else
          (die/p p 'tokenize "invalid identifier syntax" 
            (list->string (reverse (cons c ls))))]))))
  (define (tokenize-string ls p)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) 
         (die/p p 'tokenize "invalid eof inside string")]
        [else (tokenize-string-char ls p c)])))
  (define (tokenize-string-char ls p c)
    (define (intraline-whitespace? c)
      (or (eqv? c #\x9) 
          (eq? (char-general-category c) 'Zs)))
    (define (tokenize-string-continue ls p c)
      (cond
        [(eof-object? c) 
         (die/p p 'tokenize "invalid eof inside string")]
        [(intraline-whitespace? c)
         (let f ()
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (die/p p 'tokenize "invalid eof inside string")]
               [(intraline-whitespace? c) (f)]
               [else (tokenize-string-char ls p c)])))]
        [else (tokenize-string-char ls p c)]))
    (cond
     [($char= #\" c) ls]
     [($char= #\\ c)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) 
           (die/p p 'tokenize "invalid eof after string escape")]
          [($char= #\a c) (tokenize-string (cons #\x7 ls) p)]
          [($char= #\b c) (tokenize-string (cons #\x8 ls) p)]
          [($char= #\t c) (tokenize-string (cons #\x9 ls) p)]
          [($char= #\n c) (tokenize-string (cons #\xA ls) p)]
          [($char= #\v c) (tokenize-string (cons #\xB ls) p)]
          [($char= #\f c) (tokenize-string (cons #\xC ls) p)]
          [($char= #\r c) (tokenize-string (cons #\xD ls) p)]
          [($char= #\" c) (tokenize-string (cons #\x22 ls) p)]
          [($char= #\\ c) (tokenize-string (cons #\x5C ls) p)]
          [($char= #\x c) ;;; unicode escape \xXXX;
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (die/p p 'tokenize "invalid eof inside string")]
               [(hex c) =>
                (lambda (n)
                  (let f ([n n] [ac (cons c '(#\x))])
                    (let ([c (read-char p)])
                      (cond
                        [(eof-object? n) 
                         (die/p p 'tokenize "invalid eof inside string")]
                        [(hex c) =>
                         (lambda (v) (f (+ (* n 16) v) (cons c ac)))]
                        [($char= c #\;) 
                         (tokenize-string
                           (cons (checked-integer->char n ac p) ls) p)]
                        [else
                         (die/p-1 p 'tokenize
                           "invalid char in escape sequence"
                           (list->string (reverse (cons c ac))))]))))]
               [else 
                (die/p-1 p 'tokenize
                  "invalid char in escape sequence" c)]))]
          [(intraline-whitespace? c)
           (let f ()
             (let ([c (read-char p)])
               (cond
                 [(eof-object? c) 
                  (die/p p 'tokenize "invalid eof inside string")]
                 [(intraline-whitespace? c) (f)]
                 [(memv c '(#\xA #\x85 #\x2028)) 
                  (tokenize-string-continue ls p (read-char p))]
                 [(memv c '(#\xD)) 
                  (let ([c (read-char p)])
                    (cond
                      [(memv c '(#\xA #\x85))
                       (tokenize-string-continue ls p (read-char p))]
                      [else 
                       (tokenize-string-continue ls p c)]))]
                 [else
                  (die/p-1 p 'tokenize 
                    "non-whitespace character after escape")])))]
          [(memv c '(#\xA #\x85 #\x2028))
           (tokenize-string-continue ls p (read-char p))]
          [(memv c '(#\xD)) 
           (let ([c (read-char p)])
             (cond
               [(memv c '(#\xA #\x85))
                (tokenize-string-continue ls p (read-char p))]
               [else 
                (tokenize-string-continue ls p c)]))]
          [else (die/p-1 p 'tokenize "invalid string escape" c)]))]
     [(memv c '(#\xA #\x85 #\x2028))
      (tokenize-string (cons #\linefeed ls) p)]
     [(memv c '(#\xD))
      (let ([c (peek-char p)])
        (when (memv c '(#\xA #\x85))
          (read-char p))
        (tokenize-string (cons #\linefeed ls) p))]
     [else
      (tokenize-string (cons c ls) p)]))
  (define skip-comment
    (lambda (p)
      (let ([c (read-char p)])
        (unless (eof-object? c)
          (let ([i ($char->fixnum c)])
            (unless (or (fx= i 10) (fx= i 13))
              (skip-comment p)))))))
  (define tokenize-dot
    (lambda (p)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) 'dot]
          [(delimiter? c)  'dot]
          [($char= c #\.)  ; this is second dot
           (read-char p)
           (let ([c (peek-char p)])
             (cond
               [(eof-object? c) 
                (die/p p 'tokenize "invalid syntax .. near end of file")]
               [($char= c #\.) ; this is the third
                (read-char p)
                (let ([c (peek-char p)])
                  (cond
                    [(eof-object? c) '(datum . ...)]
                    [(delimiter? c)  '(datum . ...)]
                    [else 
                     (die/p p 'tokenize "invalid syntax"
                       (string-append "..." (string c)))]))]
               [else
                (die/p p 'tokenize "invalid syntax"
                  (string-append ".." (string c)))]))]
          [else 
           (cons 'datum 
             (tokenize-decimal-no-digits p '(#\.) #f))]))))
  (define tokenize-char* 
    (lambda (i str p d)
      (cond
       [(fx= i (string-length str))
        (let ([c (peek-char p)])
          (cond
           [(eof-object? c) d]
           [(delimiter? c)  d]
           [else (die/p p 'tokenize "invalid character after sequence"
                    (string-append str (string c)))]))]
       [else
        (let ([c (read-char p)])
          (cond
            [(eof-object? c) 
             (die/p p 'tokenize "invalid eof in the middle of expected sequence" str)]
            [($char= c (string-ref str i))
             (tokenize-char* (fxadd1 i) str p d)]
            [else 
             (die/p-1 p 'tokenize
                "invalid char while scanning string"
                 c str)]))])))
  (define tokenize-char-seq
    (lambda (p str d)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) (cons 'datum (string-ref str 0))]
          [(delimiter? c)  (cons 'datum (string-ref str 0))]
          [($char= (string-ref str 1) c) 
           (read-char p)
           (tokenize-char*  2 str p d)]
          [else (die/p p 'tokenize "invalid syntax" 
                  (string-ref str 0) c)]))))
  (define tokenize-char
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c)
           (die/p p 'tokenize "invalid #\\ near end of file")]
          [(eqv? #\n c) 
           (let ([c (peek-char p)])
             (cond
               [(eof-object? c)
                (read-char p)
                '(datum . #\n)]
               [(eqv? #\u c) 
                (read-char p)
                (tokenize-char-seq p "ul" '(datum . #\x0))]
               [(eqv? #\e c) 
                (read-char p)
                (tokenize-char-seq p "ewline" '(datum . #\xA))]
               [(delimiter? c)
                '(datum . #\n)]
               [else 
                (die/p p 'tokenize "invalid syntax"
                  (string #\# #\\ #\n c))]))]
          [(eqv? #\a c) 
           (tokenize-char-seq p "alarm" '(datum . #\x7))]
          [(eqv? #\b c) 
           (tokenize-char-seq p "backspace" '(datum . #\x8))]
          [(eqv? #\t c)
           (tokenize-char-seq p "tab" '(datum . #\x9))]
          [(eqv? #\l c) 
           (tokenize-char-seq p "linefeed" '(datum . #\xA))]
          [(eqv? #\v c) 
           (tokenize-char-seq p "vtab" '(datum . #\xB))]
          [(eqv? #\p c) 
           (tokenize-char-seq p "page" '(datum . #\xC))]
          [(eqv? #\r c) 
           (tokenize-char-seq p "return" '(datum . #\xD))]
          [(eqv? #\e c) 
           (tokenize-char-seq p "esc" '(datum . #\x1B))]
          [(eqv? #\s c) 
           (tokenize-char-seq p "space" '(datum . #\x20))]
          [(eqv? #\d c)
           (tokenize-char-seq p "delete" '(datum . #\x7F))]
          [(eqv? #\x c) 
           (let ([n (peek-char p)])
             (cond
               [(or (eof-object? n) (delimiter? n))
                '(datum . #\x)]
               [(hex n) =>
                (lambda (v)
                  (read-char p)
                  (let f ([v v] [ac (cons n '(#\x))])
                    (let ([c (peek-char p)])
                      (cond
                        [(eof-object? c)
                         (cons 'datum (checked-integer->char v ac p))]
                        [(delimiter? c)
                         (cons 'datum (checked-integer->char v ac p))]
                        [(hex c) =>
                         (lambda (v0)
                           (read-char p)
                           (f (+ (* v 16) v0) (cons c ac)))]
                        [else
                         (die/p p 'tokenize 
                           "invalid character sequence"
                           (list->string (reverse (cons c ac))))]))))]
               [else
                (die/p p 'tokenize "invalid character sequence"
                       (string-append "#\\" (string n)))]))]
          [else
           (let ([n (peek-char p)])
             (cond
               [(eof-object? n) (cons 'datum c)]
               [(delimiter? n)  (cons 'datum c)]
               [else 
                (die/p p 'tokenize "invalid syntax"
                  (string-append "#\\" (string c n)))]))]))))
  (define (hex x)
    (cond
      [(and ($char<= #\0 x) ($char<= x #\9))
       ($fx- ($char->fixnum x) ($char->fixnum #\0))]
      [(and ($char<= #\a x) ($char<= x #\f))
       ($fx- ($char->fixnum x) 
             ($fx- ($char->fixnum #\a) 10))]
      [(and ($char<= #\A x) ($char<= x #\F))
       ($fx- ($char->fixnum x)
             ($fx- ($char->fixnum #\A) 10))]
      [else #f]))
  (define multiline-error
    (lambda (p)
      (die/p p 'tokenize
         "end of file encountered while inside a #|-style comment")))
  (define apprev
    (lambda (str i ac)
      (cond
        [(fx= i (string-length str)) ac]
        [else
         (apprev str (fx+ i 1) (cons (string-ref str i) ac))])))
  (define multiline-comment
    (lambda (p)
      (define f 
        (lambda (p ac)
          (let ([c (read-char p)])
            (cond
              [(eof-object? c) (multiline-error p)]
              [($char= #\| c) 
               (let g ([c (read-char p)] [ac ac])
                 (cond
                   [(eof-object? c) (multiline-error p)]
                   [($char= #\# c) ac]
                   [($char= #\| c)
                    (g (read-char p) (cons c ac))]
                   [else (f p (cons c ac))]))]
              [($char= #\# c)
               (let ([c (read-char p)])
                 (cond
                   [(eof-object? c) (multiline-error p)]
                   [($char= #\| c)
                    (let ([v (multiline-comment p)])
                      (if (string? v)
                          (f p (apprev v 0 ac))
                          (f p ac)))]
                   [else 
                    (f p (cons c (cons #\# ac)))]))]
              [else (f p (cons c ac))]))))
      (let ([ac (f p '())])
        ((comment-handler)
          (list->string (reverse ac))))))
  (define tokenize-hash
    (lambda (p)
      (tokenize-hash/c (read-char p) p)))
  (define (skip-whitespace p caller)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c)
         (die/p p 'tokenize "invalid eof inside" caller)]
        [(char-whitespace? c)
         (skip-whitespace p caller)]
        [else c])))
  (define tokenize-hash/c
    (lambda (c p)
      (cond
        [(eof-object? c) (die/p p 'tokenize "invalid # near end of file")]
        [(memq c '(#\t #\T)) 
         (let ([c (peek-char p)])
           (cond
             [(eof-object? c) '(datum . #t)]
             [(delimiter? c)  '(datum . #t)]
             [else (die/p p 'tokenize 
                     (format "invalid syntax near #~a" c))]))]
        [(memq c '(#\f #\F)) 
         (let ([c (peek-char p)])
           (cond
             [(eof-object? c) '(datum . #f)]
             [(delimiter? c)  '(datum . #f)]
             [else (die/p p 'tokenize 
                     (format "invalid syntax near #~a" c))]))]
        [($char= #\\ c) (tokenize-char p)]
        [($char= #\( c) 'vparen]
        [($char= #\' c) '(macro . syntax)]
        [($char= #\` c) '(macro . quasisyntax)]
        [($char= #\, c)
         (let ([c (peek-char p)])
           (cond
             [(eqv? c #\@) (read-char p)
              '(macro . unsyntax-splicing)]
             [else '(macro . unsyntax)]))]
        [($char= #\! c) 
         (let ([e (read-char p)])
           (when (eof-object? e)
             (die/p p 'tokenize "invalid eof near #!"))
           (case e
             [(#\e) 
              (when (eq? (port-mode p) 'r6rs-mode)
                (die/p-1 p 'tokenize "invalid syntax: #!e"))
              (read-char* p '(#\e) "of" "eof sequence" #f #f)
              (cons 'datum (eof-object))]
             [(#\r) 
              (read-char* p '(#\r) "6rs" "#!r6rs comment" #f #f)
              (set-port-mode! p 'r6rs-mode)
              (tokenize/1 p)]
             [(#\i) 
              (read-char* p '(#\i) "karus" "#!ikarus comment" #f #f)
              (set-port-mode! p 'ikarus-mode)
              (tokenize/1 p)]
             [else
              (die/p-1 p 'tokenize
                (format "invalid syntax near #!~a" e))]))]
        [(digit? c) 
         (when (eq? (port-mode p) 'r6rs-mode)
           (die/p-1 p 'tokenize "graph syntax is invalid in #!r6rs mode"
              (format "#~a" c)))
         (tokenize-hashnum p (char->num c))]
        [($char= #\: c)
         (when (eq? (port-mode p) 'r6rs-mode)
           (die/p-1 p 'tokenize "gensym syntax is invalid in #!r6rs mode"
              (format "#~a" c)))
         (let* ([c (skip-whitespace p "gensym")]
                [id0 
                 (cond
                   [(initial? c)
                    (list->string
                      (reverse (tokenize-identifier (cons c '()) p)))]
                   [($char= #\| c)
                    (list->string 
                      (reverse (tokenize-bar p '())))]
                   [else 
                    (die/p-1 p 'tokenize
                      "invalid char inside gensym" c)])])
              (cons 'datum (gensym id0)))]
        [($char= #\{ c)
         (when (eq? (port-mode p) 'r6rs-mode)
           (die/p-1 p 'tokenize "gensym syntax is invalid in #!r6rs mode"
              (format "#~a" c)))
         (let* ([c (skip-whitespace p "gensym")]
                [id0 
                 (cond
                   [(initial? c)
                    (list->string
                      (reverse (tokenize-identifier (cons c '()) p)))]
                   [($char= #\| c)
                    (list->string 
                      (reverse (tokenize-bar p '())))]
                   [else 
                    (die/p-1 p 'tokenize
                      "invalid char inside gensym" c)])]
                [c (skip-whitespace p "gensym")])
           (cond
             [($char= #\} c)
              (cons 'datum 
                (foreign-call "ikrt_strings_to_gensym" #f id0))]
             [else
              (let ([id1
                     (cond
                       [(initial? c)
                        (list->string
                         (reverse
                           (tokenize-identifier 
                             (cons c '()) p)))]
                       [($char= #\| c)
                        (list->string 
                          (reverse (tokenize-bar p '())))]
                       [else 
                        (die/p-1 p 'tokenize
                          "invalid char inside gensym" c)])])
                (let ([c (skip-whitespace p "gensym")])
                  (cond
                    [($char= #\} c)
                     (cons 'datum
                      (foreign-call "ikrt_strings_to_gensym" 
                        id0 id1))]
                    [else
                     (die/p-1 p 'tokenize
                        "invalid char inside gensym" c)])))]))]
        [($char= #\v c)
         (let ([c (read-char p)])
           (cond
             [($char= #\u c)
              (let ([c (read-char p)])
                (cond
                  [($char= c #\8)
                   (let ([c (read-char p)])
                     (cond
                       [($char= c #\() 'vu8]
                       [(eof-object? c) 
                        (die/p p 'tokenize "invalid eof object after #vu8")]
                       [else (die/p-1 p 'tokenize 
                               (format "invalid sequence #vu8~a" c))]))]
                  [(eof-object? c) 
                   (die/p p 'tokenize "invalid eof object after #vu")]
                  [else (die/p-1 p 'tokenize 
                           (format "invalid sequence #vu~a" c))]))]
             [(eof-object? c) 
              (die/p p 'tokenize "invalid eof object after #v")]
             [else (die/p p 'tokenize
                     (format "invalid sequence #v~a" c))]))]
        [(memq c '(#\e #\E)) 
         (cons 'datum (tokenize-exactness-mark p (list c #\#) 'e))]
        [(memq c '(#\i #\I)) 
         (cons 'datum (tokenize-exactness-mark p (list c #\#) 'i))]
        [(memq c '(#\b #\B)) 
         (cons 'datum (tokenize-radix-mark p (list c #\#) 2))]
        [(memq c '(#\x #\X)) 
         (cons 'datum (tokenize-radix-mark p (list c #\#) 16))]
        [(memq c '(#\o #\O)) 
         (cons 'datum (tokenize-radix-mark p (list c #\#) 8))]
        [(memq c '(#\d #\D)) 
         (cons 'datum (tokenize-radix-mark p (list c #\#) 10))]
        [($char= #\@ c)
         (when (eq? (port-mode p) 'r6rs-mode)
           (die/p-1 p 'tokenize "fasl syntax is invalid in #!r6rs mode"
              (format "#~a" c)))
         (die/p-1 p 'read "FIXME: fasl read disabled")
         '(cons 'datum ($fasl-read p))]
        [else 
         (die/p-1 p 'tokenize 
            (format "invalid syntax #~a" c))])))
  (define (tokenize-exactness-mark p ls exact?)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (num-error p "eof object" ls)]
        [(radix-digit c 10) =>
         (lambda (d) 
           (tokenize-integer p (cons c ls) exact? 10 d))]
        [(char=? c #\.) 
         (tokenize-decimal-no-digits p (cons c ls) exact?)]
        [(char=? c #\-) 
         (- (tokenize-integer-no-digits p (cons c ls) exact? 10))]
        [(char=? c #\+)
         (tokenize-integer-no-digits p (cons c ls) exact? 10)]
        [(char=? c #\#)
         (let ([c1 (read-char p)])
           (cond
             [(eof-object? c1) 
              (num-error p "eof object" (cons c ls))]
             [(memv c1 '(#\b #\B)) 
              (tokenize-radix/exactness-marks p (cons* c1 c ls) exact? 2)]
             [(memv c1 '(#\x #\X)) 
              (tokenize-radix/exactness-marks p (cons* c1 c ls) exact? 16)]
             [(memv c1 '(#\o #\O)) 
              (tokenize-radix/exactness-marks p (cons* c1 c ls) exact? 8)]
             [(memv c1 '(#\d #\D)) 
              (tokenize-radix/exactness-marks p (cons* c1 c ls) exact? 10)]
             [else (num-error p "invalid sequence" (cons* c1 c ls))]))]
        [else (num-error p "invalid sequence" (cons c ls))]))) 
  (define (tokenize-radix-mark p ls radix)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (num-error p "eof object" ls)]
        [(radix-digit c radix) =>
         (lambda (d)
           (tokenize-integer p (cons c ls) #f radix d))]
        [(char=? c #\.) 
         (unless (= radix 10)
           (num-error p "invalid decimal" (cons c ls)))
         (tokenize-decimal-no-digits p (cons c ls) #f)]
        [(char=? c #\-)
         (- (tokenize-integer-no-digits p (cons c ls) #f radix))]
        [(char=? c #\+)
         (tokenize-integer-no-digits p (cons c ls) #f radix)]
        [(char=? c #\#)
         (let ([c1 (read-char p)])
           (cond
             [(eof-object? c1) 
              (num-error p "eof object" (cons c ls))]
             [(memv c1 '(#\e #\E)) 
              (tokenize-radix/exactness-marks p (cons c1 (cons c ls))
                'e radix)]
             [(memv c1 '(#\i #\I)) 
              (tokenize-radix/exactness-marks p (cons c1 (cons c ls))
                'i radix)]
             [else (num-error p "invalid sequence" (cons* c1 c ls))]))]
        [else (num-error p "invalid sequence" (cons c ls))])))
  (define (tokenize-radix/exactness-marks p ls exact? radix) 
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (num-error p "eof object" ls)]
        [(radix-digit c radix) =>
         (lambda (d) 
           (tokenize-integer p (cons c ls) exact? radix d))]
        [(char=? c #\.) 
         (unless (= radix 10)
           (num-error p "invalid decimal" (cons c ls)))
         (tokenize-decimal-no-digits p (cons c ls) exact?)]
        [(char=? c #\-) 
         (- (tokenize-integer-no-digits p (cons c ls) exact? radix))]
        [(char=? c #\+)
         (tokenize-integer-no-digits p (cons c ls) exact? radix)]
        [else (num-error p "invalid sequence" (cons c ls))])))
  (define (tokenize-integer p ls exact? radix ac) 
    (define (tokenize-denom-start p ls exact? radix num)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (num-error p "eof object" ls)]
          [(radix-digit c radix) =>
           (lambda (d) 
             (tokenize-denom p (cons c ls) exact? radix num d))]
          [(char=? c #\-)
           (tokenize-denom-no-digits p (cons c ls) exact? radix (- num))]
          [(char=? c #\+)
           (tokenize-denom-no-digits p (cons c ls) exact? radix num)]
          [else (num-error p "invalid sequence" (cons c ls))])))
    (define (tokenize-denom-no-digits p ls exact? radix num)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (num-error p "eof object" ls)]
          [(radix-digit c radix) =>
           (lambda (d) 
             (tokenize-denom p (cons c ls) exact? radix num d))]
          [else (num-error p "invalid sequence" (cons c ls))])))
    (define (tokenize-denom p ls exact? radix num ac)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) 
           (read-char p)
           (if (= ac 0) 
               (num-error p "zero denominator" ls)
               (convert/exact exact? (/ num ac)))]
          [(radix-digit c radix) =>
           (lambda (d) 
             (read-char p)
             (tokenize-denom p (cons c ls) exact? radix num 
                (+ (* radix ac) d)))]
          [(delimiter? c) 
           (if (= ac 0)
               (num-error p "zero denominator" ls)
               (convert/exact exact? (/ num ac)))]
          [else (num-error p "invalid sequence" (cons c ls))])))
    (let ([c (peek-char p)])
      (cond
        [(eof-object? c) (convert/exact exact? ac)]
        [(radix-digit c radix) =>
         (lambda (d)
           (read-char p)
           (tokenize-integer p (cons c ls) exact? radix 
             (+ (* ac radix) d)))]
        [(char=? c #\.)
         (unless (= radix 10)
           (num-error p "invalid decimal" (cons c ls)))
         (read-char p)
         (tokenize-decimal p (cons c ls) exact? ac 0)]
        [(char=? c #\/)
         (read-char p)
         (tokenize-denom-start p (cons #\/ ls) exact? radix ac)]
        [(memv c '(#\e #\E)) ; exponent
         (read-char p)
         (unless (= radix 10)
           (num-error p "invalid decimal" (cons c ls)))
         (let ([ex (tokenize-exponent-start p (cons c ls))])
           (convert/exact (or exact? 'i)
             (* ac (expt radix ex))))]
        [(delimiter? c)
         (convert/exact exact? ac)]
        [else (num-error p "invalid sequence" (cons c ls))])))
  (define (tokenize-exponent-start p ls)
    (define (tokenize-exponent-no-digits p ls)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (num-error p "eof object" ls)]
          [(radix-digit c 10) =>
           (lambda (d) 
             (tokenize-exponent p (cons c ls) d))]
          [else (num-error p "invalid sequence" (cons c ls))])))
    (define (tokenize-exponent p ls ac)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) ac]
          [(radix-digit c 10) =>
           (lambda (d) 
             (read-char p)
             (tokenize-exponent p (cons c ls) 
               (+ (* ac 10) d)))]
          [(delimiter? c) ac]
          [else (num-error p "invalid sequence" (cons c ls))])))
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (num-error p "eof object" ls)]
        [(radix-digit c 10) =>
         (lambda (d) 
           (tokenize-exponent p (cons c ls) d))]
        [(char=? c #\-)
         (- (tokenize-exponent-no-digits p (cons c ls)))]
        [(char=? c #\+)
         (tokenize-exponent-no-digits p (cons c ls))]
        [else (num-error p "invalid sequence" (cons c ls))])))
  (define (tokenize-decimal p ls exact? ac exp)
    (let ([c (peek-char p)])
      (cond
        [(eof-object? c) 
         (let ([ac (* ac (expt 10 exp))])
           (convert/exact (or exact? 'i) ac))]
        [(radix-digit c 10) =>
         (lambda (d) 
           (read-char p)
           (tokenize-decimal p (cons c ls) exact? 
             (+ (* ac 10) d) (- exp 1)))]
        [(memv c '(#\e #\E)) 
         (read-char p)
         (let ([ex (tokenize-exponent-start p (cons c ls))])
           (let ([ac (* ac (expt 10 (+ exp ex)))])
             (convert/exact (or exact? 'i) ac)))]
        [(delimiter? c) 
         (let ([ac (* ac (expt 10 exp))])
           (convert/exact (or exact? 'i) ac))]
        [else (num-error p "invalid sequence" (cons c ls))])))
  (define (tokenize-decimal-no-digits p ls exact?)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (num-error p "eof object" ls)]
        [(radix-digit c 10) =>
         (lambda (d) 
           (tokenize-decimal p (cons c ls) exact? d -1))]
        [else (num-error p "invalid sequence" (cons c ls))])))
  (define (convert/exact exact? n)
    (if (eq? exact? 'i) 
        (exact->inexact n)
        n))
  (define (radix-digit c radix)
    (case radix
      [(10) 
       (cond
         [(char<=? #\0 c #\9) 
          (fx- (char->integer c) (char->integer #\0))]
         [else #f])]
      [(16) 
       (cond
         [(char<=? #\0 c #\9) 
          (fx- (char->integer c) (char->integer #\0))]
         [(char<=? #\a c #\f) 
          (fx- (char->integer c) (fx- (char->integer #\a) 10))]
         [(char<=? #\A c #\F) 
          (fx- (char->integer c) (fx- (char->integer #\A) 10))]
         [else #f])]
      [(8) 
       (cond
         [(char<=? #\0 c #\7) 
          (fx- (char->integer c) (char->integer #\0))]
         [else #f])]
      [(2) 
       (case c
         [(#\0) 0]
         [(#\1) 1]
         [else #f])]
      [else (die 'radix-digit "invalid radix" radix)]))
  (define (read-char* p ls str who ci? delimited?)
    (let f ([i 0] [ls ls])
      (cond
        [(fx= i (string-length str)) 
         (when delimited?
           (let ([c (peek-char p)])
             (when (and (not (eof-object? c)) (not (delimiter? c)))
               (die/p p 'tokenize 
                 (format "invalid ~a: ~s" who 
                   (list->string (reverse (cons c ls))))))))]
        [else
         (let ([c (read-char p)])
           (cond
             [(eof-object? c) 
              (die/p p 'tokenize
                (format "invalid eof inside ~a" who))]
             [(or (and (not ci?) (char=? c (string-ref str i)))
                  (and ci? (char=? (char-downcase c) (string-ref str i))))
              (f (add1 i) (cons c ls))]
             [else 
              (die/p-1 p 'tokenize 
                (format "invalid ~a: ~s" who
                  (list->string (reverse (cons c ls)))))]))])))
  (define (tokenize-integer/nan/inf-no-digits p ls)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (num-error p "invalid eof" ls)]
        [(radix-digit c 10) =>
         (lambda (d)
           (tokenize-integer p (cons c ls) #f 10 d))]
        [(char=? c #\.) 
         (tokenize-decimal-no-digits p (cons c ls) #f)]
        [(memv c '(#\i #\I)) 
         (read-char* p (cons #\i ls) "nf.0" "number sequence" #t #t)
         +inf.0]
        [(memv c '(#\n #\N))
         (read-char* p (cons #\i ls) "an.0" "number sequence" #t #t)
         +nan.0]
        [else (num-error p "invalid sequence" (cons c ls))]))) 
  (define (tokenize-integer-no-digits p ls exact? radix?)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (num-error p "invalid eof" ls)]
        [(radix-digit c (or radix? 10)) =>
         (lambda (d) 
           (tokenize-integer p (cons c ls) exact? (or radix? 10) d))]
        [(char=? c #\.) 
         (when (and radix? (not (= radix? 10)))
           (num-error p "invalid decimal" (cons c ls)))
         (tokenize-decimal-no-digits p (cons c ls) exact?)]
        [else (num-error p "invalid sequence" (cons c ls))])))
  (define (num-error p str ls)
    (die/p-1 p 'read "invalid numeric sequence"
      (list->string (reverse ls))))
  (define (tokenize-hashnum p n)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) 
         (die/p p 'tokenize "invalid eof inside #n mark/ref")]
        [($char= #\= c) (cons 'mark n)]
        [($char= #\# c) (cons 'ref n)]
        [(digit? c)
         (tokenize-hashnum p (fx+ (fx* n 10) (char->num c)))]
        [else
         (die/p-1 p 'tokenize "invalid char while inside a #n mark/ref" c)])))
  (define tokenize-bar
    (lambda (p ac)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) 
           (die/p p 'tokenize "unexpected eof while reading symbol")]
          [($char= #\\ c)
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (die/p p 'tokenize "unexpected eof while reading symbol")]
               [else (tokenize-bar p (cons c ac))]))]
          [($char= #\| c) ac]
          [else (tokenize-bar p (cons c ac))]))))
  (define (tokenize-backslash main-ac p)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) 
         (die/p p 'tokenize "invalid eof after symbol escape")]
        [($char= #\x c) 
         (let ([c (read-char p)])
           (cond
             [(eof-object? c) 
              (die/p p 'tokenize "invalid eof after \\x")]
             [(hex c) => 
              (lambda (v)
                (let f ([v v] [ac `(,c #\x #\\)])
                  (let ([c (read-char p)])
                    (cond
                      [(eof-object? c) 
                       (die/p p 'tokenize 
                         (format "invalid eof after ~a"
                           (list->string (reverse ac))))]
                      [($char= #\; c)
                       (tokenize-identifier 
                         (cons (checked-integer->char v ac p) main-ac)
                         p)]
                      [(hex c) =>
                       (lambda (v0)
                         (f (+ (* v 16) v0) (cons c ac)))]
                      [else 
                       (die/p-1 p 'tokenize "invalid sequence"
                         (list->string (cons c (reverse ac))))]))))]
             [else
              (die/p-1 p 'tokenize 
                 (format "invalid sequence \\x~a" c))]))]
        [else 
         (die/p-1 p 'tokenize
           (format "invalid sequence \\~a" c))])))
  (define tokenize/c
    (lambda (c p)
      (cond
        [(eof-object? c)
         (error 'tokenize/c "hmmmm eof")
         (eof-object)]
        [($char= #\( c)   'lparen]
        [($char= #\) c)   'rparen]
        [($char= #\[ c)   'lbrack]
        [($char= #\] c)   'rbrack]
        [($char= #\' c)   '(macro . quote)]
        [($char= #\` c)   '(macro . quasiquote)]
        [($char= #\, c) 
         (let ([c (peek-char p)])
           (cond
            [(eof-object? c) '(macro . unquote)]
            [($char= c #\@)
             (read-char p)
             '(macro . unquote-splicing)]
            [else '(macro . unquote)]))]
        [($char= #\# c) (tokenize-hash p)]
        [(radix-digit c 10) =>
         (lambda (d) 
           (cons 'datum
             (tokenize-integer p (list c) #f 10 d)))]
        [(initial? c)
         (let ([ls (reverse (tokenize-identifier (cons c '()) p))])
           (cons 'datum (string->symbol (list->string ls))))]
        [($char= #\" c)
         (let ([ls (tokenize-string '() p)])
           (cons 'datum (list->string (reverse ls))))]
        [(memq c '(#\+))
         (let ([c (peek-char p)])
           (cond
             [(eof-object? c) '(datum . +)]
             [(delimiter? c)  '(datum . +)]
             [else
              (cons 'datum
                (tokenize-integer/nan/inf-no-digits p '(#\+)))]))]
        [(memq c '(#\-))
         (let ([c (peek-char p)])
           (cond
             [(eof-object? c) '(datum . -)]
             [(delimiter? c)  '(datum . -)]
             [($char= c #\>)
              (read-char p)
              (let ([ls (tokenize-identifier '() p)])
                (let ([str (list->string (cons* #\- #\> (reverse ls)))])
                  (cons 'datum (string->symbol str))))]
             [else
              (cons 'datum
                (- (tokenize-integer/nan/inf-no-digits p '(#\-))))]))]
        [($char= #\. c)
         (tokenize-dot p)]
        [($char= #\| c)
         (when (eq? (port-mode p) 'r6rs-mode)
           (die 'tokenize "|symbol| syntax is invalid in #!r6rs mode"))
         (let ([ls (reverse (tokenize-bar p '()))])
           (cons 'datum (string->symbol (list->string ls))))]
        [($char= #\\ c)
         (cons 'datum 
            (string->symbol
              (list->string
                (reverse (tokenize-backslash '() p)))))]
        [else
         (die/p-1 p 'tokenize "invalid syntax" c)])))

  (define tokenize/1
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (eof-object)]
          [(eqv? c #\;)
           (skip-comment p)
           (tokenize/1 p)]
          [(eqv? c #\#)
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (die/p p 'tokenize "invalid eof after #")]
               [(eqv? c #\;)
                (read-as-comment p) 
                (tokenize/1 p)]
               [(eqv? c #\|) 
                (multiline-comment p)
                (tokenize/1 p)]
               [else
                (tokenize-hash/c c p)]))]
          [(char-whitespace? c) (tokenize/1 p)]
          [else (tokenize/c c p)]))))

  (define tokenize/1+pos
    (lambda (p)
      (let ([pos (input-port-byte-position p)])
        (let ([c (read-char p)])
          (cond
            [(eof-object? c) (values (eof-object) pos)]
            [(eqv? c #\;)
             (skip-comment p)
             (tokenize/1+pos p)]
            [(eqv? c #\#)
             (let ([pos (input-port-byte-position p)])
               (let ([c (read-char p)])
                 (cond
                   [(eof-object? c) 
                    (die/p p 'tokenize "invalid eof after #")]
                   [(eqv? c #\;)
                    (read-as-comment p)
                    (tokenize/1+pos p)]
                   [(eqv? c #\|) 
                    (multiline-comment p)
                    (tokenize/1+pos p)]
                   [else
                    (values (tokenize-hash/c c p) pos)])))]
            [(char-whitespace? c) (tokenize/1+pos p)]
            [else 
             (values (tokenize/c c p) pos)])))))

  (define tokenize-script-initial
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) c]
          [(eqv? c #\;)
           (skip-comment p)
           (tokenize/1 p)]
          [(eqv? c #\#) 
           (let ([c (read-char p)])
             (cond
               [(eof-object? c)
                (die/p p 'tokenize "invalid eof after #")]
               [(eqv? c #\!)
                (skip-comment p)
                (tokenize/1 p)]
               [(eqv? c #\;)
                (read-as-comment p)
                (tokenize/1 p)]
               [(eqv? c #\|) 
                (multiline-comment p)
                (tokenize/1 p)]
               [else
                (tokenize-hash/c c p)]))]
          [(char-whitespace? c) (tokenize/1 p)]
          [else (tokenize/c c p)]))))

  (define tokenize-script-initial+pos
    (lambda (p)
      (let ([pos (input-port-byte-position p)])
        (let ([c (read-char p)])
          (cond
            [(eof-object? c) (values (eof-object) pos)]
            [(eqv? c #\;)
             (skip-comment p)
             (tokenize/1+pos p)]
            [(eqv? c #\#) 
             (let ([pos (input-port-byte-position p)])
               (let ([c (read-char p)])
                 (cond
                   [(eof-object? c)
                    (die/p p 'tokenize "invalid eof after #")]
                   [(eqv? c #\!)
                    (skip-comment p)
                    (tokenize/1+pos p)]
                   [(eqv? c #\;)
                    (read-as-comment p)
                    (tokenize/1+pos p)]
                   [(eqv? c #\|) 
                    (multiline-comment p)
                    (tokenize/1+pos p)]
                   [else
                    (values (tokenize-hash/c c p) pos)])))]
            [(char-whitespace? c) (tokenize/1+pos p)]
            [else (values (tokenize/c c p) pos)])))))

  (define-struct loc (value value^ set?))

  ;;; this is reverse engineered from psyntax.ss
  (define-struct annotation (expression source stripped))
  ;;; - source is a pair of file-name x char-position
  ;;; - stripped is an s-expression with no annotations
  ;;; - expression is a list/vector/id/whathaveyou that 
  ;;;   may contain further annotations.


  (module (read-expr read-expr-script-initial)
    (define-syntax tokenize/1 syntax-error)
    (define (annotate-simple datum pos p)
      (make-annotation datum (cons (port-id p) pos) datum))
    (define (annotate stripped expression pos p) 
      (make-annotation expression (cons (port-id p) pos) stripped))
    (define read-list
      (lambda (p locs k end mis init?)
        (let-values ([(t pos) (tokenize/1+pos p)])
          (cond
            [(eof-object? t)
             (die/p p 'read "end of file encountered while reading list")]
            [(eq? t end) (values '() '() locs k)]
            [(eq? t mis)
             (die/p-1 p 'read "paren mismatch")]
            [(eq? t 'dot)
             (when init?
               (die/p-1 p 'read "invalid dot while reading list"))
             (let-values ([(d d^ locs k) (read-expr p locs k)])
               (let-values ([(t pos^) (tokenize/1+pos p)])
                 (cond
                  [(eq? t end) (values d d^ locs k)]
                  [(eq? t mis)
                   (die/p-1 p 'read "paren mismatch")]
                  [(eq? t 'dot)
                   (die/p-1 p 'read "cannot have two dots in a list")]
                  [else
                   (die/p-1 p 'read 
                     (format "expecting ~a, got ~a" end t))])))]
            [else
             (let-values ([(a a^ locs k) (parse-token p locs k t pos)])
               (let-values ([(d d^ locs k) (read-list p locs k end mis #f)])
                 (let ([x (cons a d)] [x^ (cons a^ d^)])
                   (values x x^ locs (extend-k-pair x x^ a d k)))))]))))
    (define extend-k-pair
      (lambda (x x^ a d k) 
        (cond 
          [(or (loc? a) (loc? d))
           (lambda ()
             (let ([a (car x)])
               (when (loc? a)
                 (set-car! x (loc-value a))
                 (set-car! x^ (loc-value^ a))))
             (let ([d (cdr x)])
               (when (loc? d)
                 (set-cdr! x (loc-value d)) 
                 (set-cdr! x^ (loc-value^ d))))
             (k))]
          [else k])))
    (define vector-put
      (lambda (v v^ k i ls ls^)
        (cond
          [(null? ls) k]
          [else
           (let ([a (car ls)])
             (vector-set! v i a)
             (vector-set! v^ i (car ls^))
             (vector-put v v^ 
                (if (loc? a)
                    (lambda ()
                      (vector-set! v i (loc-value a))
                      (vector-set! v^ i (loc-value^ a))
                      (k))
                    k)
                (fxsub1 i) 
                (cdr ls)
                (cdr ls^)))])))
    (define bytevector-put
      (lambda (v k i ls ls^)
        (cond
          [(null? ls) k]
          [else
           (let ([a (car ls)])
             (cond
               [(fixnum? a)
                (unless (and (fx<= 0 a) (fx<= a 255))
                  (die 'read ;;; FIXME: pos
                    (format "invalid value ~s in a bytevector" a)))
                (bytevector-u8-set! v i a)
                (bytevector-put v k (fxsub1 i) (cdr ls) (cdr ls^))]
               [else (die 'read "invalid value inside a bytevector" a)]))])))
    (define read-vector
      (lambda (p locs k count ls ls^)
        (let-values ([(t pos) (tokenize/1+pos p)])
          (cond
            [(eof-object? t) 
             (die/p p 'read "end of file encountered while reading a vector")]
            [(eq? t 'rparen) 
             (let ([v (make-vector count)] [v^ (make-vector count)])
               (let ([k (vector-put v v^ k (fxsub1 count) ls ls^)])
                 (values v v^ locs k)))]
            [(eq? t 'rbrack)
             (die/p-1 p 'read "unexpected ] while reading a vector")]
            [(eq? t 'dot)
             (die/p-1 p 'read "unexpected . while reading a vector")]
            [else
             (let-values ([(a a^ locs k) (parse-token p locs k t pos)])
                (read-vector p locs k (fxadd1 count) 
                  (cons a ls) (cons a^ ls^)))]))))
    (define read-bytevector
      (lambda (p locs k count ls ls^)
        (let-values ([(t pos) (tokenize/1+pos p)])
          (cond
            [(eof-object? t) 
             (die/p p 'read "end of file encountered while reading a bytevector")]
            [(eq? t 'rparen) 
             (let ([v ($make-bytevector count)])
               (let ([k (bytevector-put v k (fxsub1 count) ls ls^)])
                 (values v v locs k)))]
            [(eq? t 'rbrack)
             (die/p-1 p 'read "unexpected ] while reading a bytevector")]
            [(eq? t 'dot)
             (die/p-1 p 'read "unexpected . while reading a bytevector")]
            [else
             (let-values ([(a a^ locs k) (parse-token p locs k t pos)])
                (read-bytevector p locs k (fxadd1 count) 
                  (cons a ls) (cons a^ ls^)))]))))
    (define parse-token
      (lambda (p locs k t pos)
        (cond
          [(eof-object? t)
           (values (eof-object) 
             (annotate-simple (eof-object) pos p) locs k)]
          [(eq? t 'lparen)
           (let-values ([(ls ls^ locs k) 
                         (read-list p locs k 'rparen 'rbrack #t)])
             (values ls (annotate ls ls^ pos p) locs k))]
          [(eq? t 'lbrack) 
           (let-values ([(ls ls^ locs k) 
                         (read-list p locs k 'rbrack 'rparen #t)])
             (values ls (annotate ls ls^ pos p) locs k))]
          [(eq? t 'vparen) 
           (let-values ([(v v^ locs k) 
                         (read-vector p locs k 0 '() '())])
             (values v (annotate v v^ pos p) locs k))]
          [(eq? t 'vu8)
           (let-values ([(v v^ locs k) 
                         (read-bytevector p locs k 0 '() '())])
             (values v (annotate v v^ pos p) locs k))]
          [(pair? t)
           (cond
             [(eq? (car t) 'datum) 
              (values (cdr t) 
                (annotate-simple (cdr t) pos p) locs k)]
             [(eq? (car t) 'macro)
              (let ([macro (cdr t)])
                (define (read-macro)
                  (let-values ([(t pos) (tokenize/1+pos p)])
                    (cond
                      [(eof-object? t) 
                       (die/p p 'read 
                         (format "invalid eof after ~a read macro"
                         macro))]
                      [else (parse-token p locs k t pos)])))
                (let-values ([(expr expr^ locs k) (read-macro)])
                  (let ([d (list expr)] [d^ (list expr^)])
                    (let ([x (cons macro d)] 
                          [x^ (cons (annotate-simple macro pos p) d^)])
                      (values x (annotate x x^ pos p) locs
                        (extend-k-pair d d^ expr '() k))))))]
             [(eq? (car t) 'mark) 
              (let ([n (cdr t)])
                (let-values ([(expr expr^ locs k) 
                              (read-expr p locs k)])
                  (cond
                    [(assq n locs) =>
                     (lambda (x)
                       (let ([loc (cdr x)])
                         (when (loc-set? loc) ;;; FIXME: pos
                           (die 'read "duplicate mark" n))
                         (set-loc-value! loc expr)
                         (set-loc-value^! loc expr^)
                         (set-loc-set?! loc #t)
                         (values expr expr^ locs k)))]
                    [else
                     (let ([loc (make-loc expr 'unused #t)])
                       (let ([locs (cons (cons n loc) locs)])
                         (values expr expr^ locs k)))])))]
             [(eq? (car t) 'ref)
              (let ([n (cdr t)])
                (cond
                  [(assq n locs) =>
                   (lambda (x)
                     (values (cdr x) 'unused locs k))]
                  [else
                   (let ([loc (make-loc #f 'unused #f)])
                     (let ([locs (cons (cons n loc) locs)])
                       (values loc 'unused locs k)))]))]
             [else (die 'read "invalid token" t)])]
          [else
           (die/p-1 p 'read 
             (format "unexpected ~s found" t))])))
    (define read-expr
      (lambda (p locs k)
        (let-values ([(t pos) (tokenize/1+pos p)])
          (parse-token p locs k t pos))))
    (define read-expr-script-initial
      (lambda (p locs k)
        (let-values ([(t pos) (tokenize-script-initial+pos p)])
          (parse-token p locs k t pos)))))


  (define reduce-loc!
    (lambda (x)
       (let ([loc (cdr x)])
         (unless (loc-set? loc)
           (die 'read "referenced mark is not set" (car x)))
         (when (loc? (loc-value loc))
           (let f ([h loc] [t loc])
             (if (loc? h)
                 (let ([h1 (loc-value h)])
                   (if (loc? h1)
                       (begin
                         (when (eq? h1 t)
                           (die 'read "circular marks"))
                         (let ([v (f (loc-value h1) (loc-value t))])
                           (set-loc-value! h1 v)
                           (set-loc-value! h v)
                           v))
                       (begin
                         (set-loc-value! h h1)
                         h1)))
                 h))))))
  
  (define (read-as-comment p)
    (begin (read-expr p '() void) (void)))

  (define (return-annotated x)
    (cond
      [(and (annotation? x) (eof-object? (annotation-expression x)))
       (eof-object)]
      [else x]))

  (define my-read
    (lambda (p)
      (let-values ([(expr expr^ locs k) (read-expr p '() void)])
        (cond
          [(null? locs) expr]
          [else
           (for-each reduce-loc! locs)
           (k)
           (if (loc? expr)
               (loc-value expr)
               expr)]))))

  (define read-initial
    (lambda (p)
      (let-values ([(expr expr^ locs k) (read-expr-script-initial p '() void)])
        (cond
          [(null? locs) expr]
          [else
           (for-each reduce-loc! locs)
           (k)
           (if (loc? expr)
               (loc-value expr)
               expr)]))))

  (define read-annotated
    (case-lambda
      [(p)
       (unless (input-port? p) 
         (error 'read-annotated "not an input port" p))
       (let-values ([(expr expr^ locs k) (read-expr p '() void)])
         (cond
           [(null? locs) (return-annotated expr^)]
           [else
            (for-each reduce-loc! locs)
            (k)
            (if (loc? expr)
                (loc-value^ expr)
                (return-annotated expr^))]))]
      [() (read-annotated (current-input-port))]))

  (define read-script-annotated
    (lambda (p)
      (let-values ([(expr expr^ locs k) (read-expr-script-initial p '() void)])
        (cond
          [(null? locs) (return-annotated expr^)]
          [else
           (for-each reduce-loc! locs)
           (k)
           (if (loc? expr)
               (loc-value^ expr)
               (return-annotated expr^))]))))

  (define read-token
    (case-lambda
      [() (tokenize/1 (current-input-port))]
      [(p)
       (if (input-port? p)
           (tokenize/1 p)
           (die 'read-token "not an input port" p))]))

  (define read
    (case-lambda
      [() (my-read (current-input-port))]
      [(p)
       (if (input-port? p)
           (my-read p)
           (die 'read "not an input port" p))]))

  (define (get-datum p)
    (unless (input-port? p) 
      (die 'get-datum "not an input port"))
    (my-read p))

  (define comment-handler
    ;;; this is stale, maybe delete
    (make-parameter
      (lambda (x) (void))
      (lambda (x)
        (unless (procedure? x)
          (die 'comment-handler "not a procedure" x))
        x)))

  (define (annotated-port file-name)
    (open-string-input-port/id 
      (with-input-from-file file-name 
        (lambda () (get-string-all (current-input-port))))
      file-name))

  (define (read-library-source-file file-name)
    (read-annotated (annotated-port file-name)))

  (define (read-script-source-file file-name)
    (let ([p (annotated-port file-name)])
      (let ([x (read-script-annotated p)])
        (if (eof-object? x)
            (begin (close-input-port p) '())
            (cons x 
              (let f ()
                (let ([x (read-annotated p)])
                  (cond
                    [(eof-object? x) 
                     (close-input-port p)
                     '()]
                    [else (cons x (f))]))))))))
  )

