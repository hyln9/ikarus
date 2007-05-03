
(library (ikarus tokenizer)
  (export)
  (import (scheme))

  (define delimiter?
    (lambda (c)
      (or (char-whitespace? c)
          (memq c '(#\( #\) #\[ #\] #\{ #\} #\' #\` #\, #\")))))
  (define digit?
    (lambda (c)
      (and ($char<= #\0 c) ($char<= c #\9))))
  (define char->num
    (lambda (c)
      (fx- ($char->fixnum c) ($char->fixnum #\0))))
  (define initial?
    (lambda (c)
      (or (letter? c) (special-initial? c))))
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
  (define tokenize-number
    (lambda (n p)
      (let ([c (read-char p)])
        (cond
         [(eof-object? c) n]
         [(digit? c)
          (tokenize-number (+ (* n 10) (char->num c)) p)]
         [($char= c #\.)
          (tokenize-flonum/with-digits n p)]
         [(delimiter? c)
          (unread-char c p)
          n]
         [else
          (unread-char c p)
          (error 'tokenize "invalid number syntax: ~a~a" n c)]))))
  (define tokenize-hex
    (lambda (n p)
      (let ([c (read-char p)])
        (cond
         [(eof-object? c) n]
         [(digit? c)
          (tokenize-hex (+ (* n 16) (char->num c)) p)]
         [(af? c)
          (tokenize-hex (+ (* n 16) (af->num c)) p)]
         [(delimiter? c)
          (unread-char c p)
          n]
         [else
          (unread-char c p)
          (error 'tokenize "invalid hex number sequence: ~a~a" n c)]))))
  (define tokenize-hex-init
    (lambda (p)
      (let ([c (read-char p)])
        (cond
         [(eof-object? c) 
          (unread-char c p) 
          (error 'tokenize "invalid #x near end of file")]
         [(digit? c)
          (cons 'datum (tokenize-hex (char->num c) p))]
         [(af? c)
          (cons 'datum (tokenize-hex (af->num c) p))]
         [($char= c #\-)
          (cons 'datum (* -1 (tokenize-hex 0 p)))]
         [($char= c #\+)
          (cons 'datum (tokenize-hex 0 p))]
         [else
          (unread-char c p)
          (error 'tokenize "invalid number syntax: #x~a" c)]))))
  (define tokenize-identifier
    (lambda (ls p)
      (let ([c (read-char p)])
        (cond
         [(eof-object? c) ls]
         [(subsequent? c)
          (tokenize-identifier (cons c ls) p)]
         [(delimiter? c)
          (unread-char c p)
          ls]
         [else
          (unread-char c p)
          (error 'tokenize "invalid identifier syntax: ~a" 
                 (list->string (reverse (cons c ls))))]))))
  (define tokenize-string
    (lambda (ls p)
      (let ([c (read-char p)])
        (cond
         [(eof-object? c) 
          (error 'tokenize "end-of-file while inside a string")]
         [($char= #\" c)  ls]
         [($char= #\\ c) 
          (let ([c (read-char p)])
            (cond
              [($char= #\" c) (tokenize-string (cons #\" ls) p)]
              [($char= #\\ c) (tokenize-string (cons #\\ ls) p)]
              [($char= #\n c) (tokenize-string (cons #\newline ls) p)]
              [($char= #\r c) (tokenize-string (cons #\return ls) p)]
              [($char= #\t c) (tokenize-string (cons #\tab ls) p)]
              [else (error 'tokenize "invalid string escape \\~a" c)]))]
         [else
          (tokenize-string (cons c ls) p)]))))
  (define skip-comment
    (lambda (p)
      (let ([c (read-char p)])
        (unless (eof-object? c)
          (let ([i ($char->fixnum c)])
            (unless (or (fx= i 10) (fx= i 13))
              (skip-comment p)))))))
  (define (ls->flonum ls pos?)
    (let ([str (if pos?
                   (list->string 
                     (cons #\. (reverse ls)))
                   (list->string
                     (list* #\- #\. (reverse ls))))])
       (string->flonum str)))
  (define (tokenize-flonum ls pos? p)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (ls->flonum ls pos?)]
        [(digit? c) (tokenize-flonum (cons c ls) pos? p)]
        [(delimiter? c)
         (unread-char c p)
         (ls->flonum ls pos?)]
        [else 
         (unread-char c p)
         (error 'tokenize "invalid char ~a after flonum" c)])))
  (define (tokenize-flonum/no-digits pos? p)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) 
         (error 'tokenize "invalid eof")]
        [(digit? c)
         (tokenize-flonum (list c) pos? p)]
        [else 
         (unread-char c p)
         (error 'tokenize "invalid char ~a after decimal point" c)])))
  (define (tokenize-flonum/with-digits n p)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) (+ n (string->flonum "0.0"))]
        [(digit? c)
         (+ n (tokenize-flonum (list c) #t p))]
        [(delimiter? c)
         (unread-char c p)
         (+ n (string->flonum "0.0"))]
        [else 
         (unread-char c p)
         (error 'tokenize "invalid char ~a after decimal point" c)])))
  (define tokenize-plus
    (lambda (p)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) '(datum . +)]
          [(delimiter? c)  '(datum . +)]
          [(digit? c) 
           (read-char p)
           (cons 'datum (tokenize-number (char->num c) p))]
          [($char= c #\.)
           (read-char p)
           (cons 'datum (tokenize-flonum/no-digits #t p))]
          [else (error 'tokenize "invalid sequence +~a" c)]))))
  (define tokenize-minus
    (lambda (p)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) '(datum . -)]
          [(delimiter? c)  '(datum . -)]
          [(digit? c) 
           (read-char p)
           (cons 'datum (* -1 (tokenize-number (char->num c) p)))]
          [($char= c #\.)
           (read-char p)
           (cons 'datum (tokenize-flonum/no-digits #f p))]
          [($char= c #\>)
           (read-char p)
           (let ([ls (tokenize-identifier '() p)])
             (let ([str (list->string (list* #\- #\> (reverse ls)))])
               (cons 'datum (string->symbol str))))]
          [else (error 'tokenize "invalid sequence -~a" c)]))))
  (define tokenize-dot
    (lambda (p)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) 'dot]
          [(delimiter? c)  'dot]
          [($char= c #\.)  ; this is second dot
           (read-char p)
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (error 'tokenize "invalid syntax .. near end of file")]
               [($char= c #\.) ; this is the third
                (let ([c (peek-char p)])
                  (cond
                    [(eof-object? c) '(datum . ...)]
                    [(delimiter? c)  '(datum . ...)]
                    [else 
                     (error 'tokenize "invalid syntax ...~a" c)]))]
               [else
                (unread-char c p)
                (error 'tokenize "invalid syntax ..~a" c)]))]
          [(digit? c)
           (read-char p)
           (cons 'datum (tokenize-flonum (list c) #t p))]
          [else
           (error 'tokenize "invalid syntax .~a" c)]))))
  (define tokenize-char* 
    (lambda (i str p d)
      (cond
       [(fx= i (string-length str))
        (let ([c (peek-char p)])
          (cond
           [(eof-object? c) d]
           [(delimiter? c)  d]
           [else (error 'tokenize "invalid character after #\\~a" str)]))]
       [else
        (let ([c (read-char p)])
          (cond
            [(eof-object? c) 
             (error 'tokenize "invalid eof in the middle of #\\~a" str)]
            [($char= c (string-ref str i))
             (tokenize-char* (fxadd1 i) str p d)]
            [else 
             (error 'tokenize
                    "invalid char ~a while scanning #\\~a" c str)]))])))
  (define tokenize-char-seq
    (lambda (p str d)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) (cons 'datum (string-ref str 0))]
          [(delimiter? c)  (cons 'datum (string-ref str 0))]
          [($char= (string-ref str 1) c) 
           (read-char p)
           (tokenize-char*  2 str p d)]
          [else (error 'tokenize "invalid syntax near #\\~a~a" 
                       (string-ref str 0) c)]))))
  (define tokenize-char
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c)
           (error 'tokenize "invalid #\\ near end of file")]
          [($char= #\s c) 
           (tokenize-char-seq p "space" '(datum . #\space))]
          [($char= #\n c) 
           (tokenize-char-seq p "newline" '(datum . #\newline))]
          [($char= #\t c) 
           (tokenize-char-seq p "tab" '(datum . #\tab))]
          [($char= #\r c) 
           (tokenize-char-seq p "return" '(datum . #\return))]
          [else
           (let ([n (peek-char p)])
             (cond
               [(eof-object? n) (cons 'datum c)]
               [(delimiter? n)  (cons 'datum c)]
               [else 
                (error 'tokenize "invalid syntax #\\~a~a" c n)]))]))))
  (define multiline-error
    (lambda ()
      (error 'tokenize
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
              [(eof-object? c) (multiline-error)]
              [($char= #\| c) 
               (let ([c (read-char p)])
                 (cond
                   [(eof-object? c) (multiline-error)]
                   [($char= #\# c) ac]
                   [else (f p (cons c ac))]))]
              [($char= #\# c)
               (let ([c (read-char p)])
                 (cond
                   [(eof-object? c) (multiline-error)]
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
  (define read-binary
    (lambda (ac chars p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) ac]
          [($char= #\0 c) (read-binary (* ac 2) (cons c chars) p)]
          [($char= #\1 c) (read-binary (+ (* ac 2) 1) (cons c chars) p)]
          [(delimiter? c) (unread-char c p) ac]
          [else 
           (unread-char c p)
           (error 'tokenize "invalid syntax #b~a"
                  (list->string (reverse (cons c chars))))]))))
  (define tokenize-hash
    (lambda (p)
      (tokenize-hash/c (read-char p) p)))
  (define (skip-whitespace p caller)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c)
         (error 'tokenize "invalid eof insize ~a" caller)]
        [(char-whitespace? c)
         (skip-whitespace p caller)]
        [else c])))
  (define tokenize-hash/c
    (lambda (c p)
      (cond
        [(eof-object? c) (error 'tokenize "invalid # near end of file")]
        [($char= c #\t) 
         (let ([c (peek-char p)])
           (cond
             [(eof-object? c) '(datum . #t)]
             [(delimiter? c)  '(datum . #t)]
             [else (error 'tokenize "invalid syntax near #t")]))]
        [($char= c #\f) 
         (let ([c (peek-char p)])
           (cond
             [(eof-object? c) '(datum . #f)]
             [(delimiter? c)  '(datum . #f)]
             [else (error 'tokenize "invalid syntax near #f")]))]
        [($char= #\\ c) (tokenize-char p)]
        [($char= #\( c) 'vparen]
        [($char= #\x c) (tokenize-hex-init p)]
        [($char= #\' c) '(macro . syntax)]
        [($char= #\; c) 'hash-semi]
        [($char= #\% c) '(macro . |#primitive|)]
        [($char= #\| c) (multiline-comment p) (tokenize p)]
        [($char= #\b c) 
         (let ([c (read-char p)])
           (cond
             [(eof-object? c) 
              (error 'tokenize "invalid eof while reading #b")]
             [($char= #\- c)
              (let ([c (read-char p)])
                (cond
                  [(eof-object? c)
                   (error 'tokenize "invalid eof while reading #b-")]
                  [($char= #\0 c)
                   (cons 'datum
                     (* -1 (read-binary 0 '(#\0 #\-) p)))]
                  [($char= #\1 c)
                   (cons 'datum
                     (* -1 (read-binary 1 '(#\1 #\-) p)))]
                  [else 
                   (unread-char c p)
                   (error 'tokenize "invalid binary syntax #b-~a" c)]))]
             [($char= #\0 c)
              (cons 'datum (read-binary 0 '(#\0) p))]
             [($char= #\1 c)
              (cons 'datum (read-binary 1 '(#\1) p))]
             [else 
              (unread-char c p)
              (error 'tokenize "invalid syntax #b~a" c)]
             ))]
        [($char= #\! c) 
         (let ([e (read-char p)])
           (when (eof-object? e)
             (error 'tokenize "invalid eof near #!"))
           (unless ($char= #\e e)
             (error 'tokenize "invalid syntax near #!~a" e))
           (let ([o (read-char p)])
             (when (eof-object? o)
               (error 'tokenize "invalid eof near #!e"))
             (unless ($char= #\o o)
               (error 'tokenize "invalid syntax near #!e~a" o))
             (let ([f (read-char p)])
               (when (eof-object? f)
                 (error 'tokenize "invalid syntax near #!eo"))
               (unless ($char= #\f f)
                 (error 'tokenize "invalid syntax near #!eo~a" f))
               (cons 'datum (eof-object)))))]
        [(digit? c) 
         (tokenize-hashnum p (char->num c))]
        [($char= #\: c)
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
                    (error 'tokenize
                      "invalid char ~a inside gensym" c)])])
              (cons 'datum (gensym id0)))]
        [($char= #\{ c)
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
                    (error 'tokenize
                      "invalid char ~a inside gensym" c)])]
                [c (skip-whitespace p "gensym")])
           (cond
             [($char= #\} c)
              (cons 'datum (gensym id0))]
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
                        (error 'tokenize
                          "invalid char ~a inside gensym" c)])])
                (let ([c (skip-whitespace p "gensym")])
                  (cond
                    [($char= #\} c)
                     (cons 'datum
                      (foreign-call "ikrt_strings_to_gensym" 
                        id0 id1))]
                    [else
                     (error 'tokenize
                        "invalid char ~a inside gensym" c)])))]))]
        [($char= #\@ c)
         (error 'read "FIXME: fasl read disabled")
         '(cons 'datum ($fasl-read p))]
        [else 
         (unread-char c p)
         (error 'tokenize "invalid syntax #~a" c)])))
  (define (tokenize-hashnum p n)
    (let ([c (read-char p)])
      (cond
        [(eof-object? c) 
         (error 'tokenize "invalid eof inside #n mark/ref")]
        [($char= #\= c) (cons 'mark n)]
        [($char= #\# c) (cons 'ref n)]
        [(digit? c)
         (tokenize-hashnum p (fx+ (fx* n 10) (char->num c)))]
        [else
         (unread-char c p)
         (error 'tokenize "invalid char ~a while inside a #n mark/ref" c)])))
  (define tokenize-bar
    (lambda (p ac)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) 
           (error 'tokenize "unexpected eof while reading symbol")]
          [($char= #\\ c)
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (error 'tokenize "unexpected eof while reading symbol")]
               [else (tokenize-bar p (cons c ac))]))]
          [($char= #\| c) ac]
          [else (tokenize-bar p (cons c ac))]))))
  (define tokenize/c
    (lambda (c p)
      (cond
        [(eof-object? c) (eof-object)]
        [(char-whitespace? c) (tokenize p)]
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
        [(digit? c) 
         (cons 'datum (tokenize-number (char->num c) p))]
        [(initial? c) 
         (let ([ls (reverse (tokenize-identifier (cons c '()) p))])
           (cons 'datum (string->symbol (list->string ls))))]
        [($char= #\" c) 
         (let ([ls (tokenize-string '() p)])
           (cons 'datum (list->string (reverse ls))))]
        [($char= #\; c)
         (skip-comment p)
         (tokenize p)]
        [($char= #\+ c)
         (tokenize-plus p)]
        [($char= #\- c)
         (tokenize-minus p)]
        [($char= #\. c)
         (tokenize-dot p)]
        [($char= #\| c)
         (let ([ls (reverse (tokenize-bar p '()))])
           (cons 'datum (string->symbol (list->string ls))))]
        [else
         (unread-char c p) 
         (error 'tokenize "invalid syntax ~a" c)])))

  (define tokenize
    (lambda (p)
      (tokenize/c (read-char p) p)))

  (define tokenize-initial
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) c]
          [($char= #\# c) 
           (let ([c (read-char p)])
             (cond
               [(eof-object? c)
                (error 'tokenize "invalid eof after #")]
               [($char= #\! c)
                (skip-comment p)
                (tokenize p)]
               [else
                (tokenize-hash/c c p)]))]
          [else (tokenize/c c p)]))))


  ;;;
  ;;;--------------------------------------------------------------* READ *---
  ;;;
  (define read-list-rest
    (lambda (p locs k end mis)
      (let ([t (read-token p)])
        (cond
         [(eof-object? t)
          (error 'read "end of file encountered while reading list")]
         [(eq? t end) (values '() locs k)]
         [(eq? t mis) 
          (error 'read "paren mismatch")]
         [(eq? t 'dot)
          (let-values ([(d locs k) (read-expr p locs k)])
            (let ([t (read-token p)])
              (cond
               [(eq? t end) (values d locs k)]
               [(eq? t mis)
                (error 'read "paren mismatch")]
               [(eq? t 'dot)
                (error 'read "cannot have two dots in a list")]
               [else
                (error 'read "expecting ~a, got ~a" end t)])))]
         [(eq? t 'hash-semi)
          (let-values ([(ignored locs k) (read-expr p locs k)])
            (read-list-rest p locs k end mis))]
         [else
          (let-values ([(a locs k) (parse-token p locs k t)])
            (let-values ([(d locs k) (read-list-rest p locs k end mis)])
               (let ([x (cons a d)])
                 (values x locs 
                         (if (or (loc? a) (loc? d))
                             (extend-k-pair x k)
                             k)))))]))))
  (define read-list-init
    (lambda (p locs k end mis)
      (let ([t (read-token p)])
       (cond
         [(eof-object? t)
          (error 'read "end of file encountered while reading list")]
         [(eq? t end) (values '() locs k)]
         [(eq? t mis) 
          (error 'read "paren mismatch")]
         [(eq? t 'dot)
          (error 'read "invalid dot while reading list")]
         [(eq? t 'hash-semi)
          (let-values ([(ignored locs k) (read-expr p locs k)])
            (read-list-init p locs k end mis))]
         [else
          (let-values ([(a locs k) (parse-token p locs k t)])
            (let-values ([(d locs k) (read-list-rest p locs k end mis)])
              (let ([x (cons a d)])
                 (values x locs 
                    (if (or (loc? a) (loc? d))
                        (extend-k-pair x k)
                        k)))))]))))
  (define extend-k-pair
    (lambda (x k)
      (lambda ()
        (let ([a (car x)])
          (when (loc? a)
            (set-car! x (loc-value a))))
        (let ([d (cdr x)])
          (when (loc? d)
            (set-cdr! x (loc-value d))))
        (k))))
  (define vector-put
    (lambda (v k i ls)
      (cond
        [(null? ls) k]
        [else
         (let ([a (car ls)])
           (vector-set! v i a)
           (vector-put v  
              (if (loc? a)
                  (lambda ()
                    (vector-set! v i (loc-value (vector-ref v i)))
                    (k))
                  k)
              (fxsub1 i) (cdr ls)))])))
  (define read-vector
    (lambda (p locs k count ls)
      (let ([t (read-token p)])
        (cond
          [(eof-object? t) 
           (error 'read "end of file encountered while reading a vector")]
          [(eq? t 'rparen) 
           (let ([v (make-vector count)])
             (let ([k (vector-put v k (fxsub1 count) ls)])
               (values v locs k)))]
          [(eq? t 'rbrack)
           (error 'read "unexpected ] while reading a vector")]
          [(eq? t 'dot)
           (error 'read "unexpected . while reading a vector")]
          [(eq? t 'hash-semi)
           (let-values ([(ignored locs k) (read-expr p locs k)])
             (read-vector p locs k count ls))]
          [else
           (let-values ([(a locs k) (parse-token p locs k t)])
              (read-vector p locs k (fxadd1 count) (cons a ls)))]))))
  (define-record loc (value set?))
  (define parse-token
    (lambda (p locs k t)
      (cond
        [(eof-object? t) (values (eof-object) locs k)]
        [(eq? t 'lparen) (read-list-init p locs k 'rparen 'rbrack)]
        [(eq? t 'lbrack) (read-list-init p locs k 'rbrack 'rparen)]
        [(eq? t 'vparen) (read-vector p locs k 0 '())]
        [(eq? t 'hash-semi)
         (let-values ([(ignored locs k) (read-expr p locs k)])
           (read-expr p locs k))]
        [(pair? t)
         (cond
           [(eq? (car t) 'datum) (values (cdr t) locs k)]
           [(eq? (car t) 'macro)
            (let-values ([(expr locs k) (read-expr p locs k)])
              (let ([x (list expr)])
                (values (cons (cdr t) x) locs
                        (if (loc? expr)
                            (lambda ()
                              (set-car! x (loc-value expr))
                              (k))
                            k))))]
           [(eq? (car t) 'mark) 
            (let ([n (cdr t)])
              (let-values ([(expr locs k) (read-expr p locs k)])
                (cond
                  [(assq n locs) =>
                   (lambda (x)
                     (let ([loc (cdr x)])
                       (when (loc-set? loc)
                         (error 'read "duplicate mark ~s" n))
                       (set-loc-value! loc expr)
                       (set-loc-set?! loc #t)
                       (values expr locs k)))]
                  [else
                   (let ([loc (make-loc expr #t)])
                     (let ([locs (cons (cons n loc) locs)])
                       (values expr locs k)))])))]
           [(eq? (car t) 'ref)
            (let ([n (cdr t)])
              (cond
                [(assq n locs) =>
                 (lambda (x)
                   (values (cdr x) locs k))]
                [else
                 (let ([loc (make-loc #f #f)])
                   (let ([locs (cons (cons n loc) locs)])
                     (values loc locs k)))]))]
           [else (error 'read "invalid token! ~s" t)])]
        [else
         (error 'read "unexpected ~s found" t)])))

  (define read-expr
    (lambda (p locs k)
      (parse-token p locs k (read-token p))))
  
  (define read-expr-initial
    (lambda (p locs k)
      (parse-token p locs k (tokenize-initial p))))

  (define reduce-loc!
    (lambda (x)
       (let ([loc (cdr x)])
         (unless (loc-set? loc)
           (error 'read "referenced mark ~s not set" (car x)))
         (when (loc? (loc-value loc))
           (let f ([h loc] [t loc])
             (if (loc? h)
                 (let ([h1 (loc-value h)])
                   (if (loc? h1)
                       (begin
                         (when (eq? h1 t)
                           (error 'read "circular marks"))
                         (let ([v (f (loc-value h1) (loc-value t))])
                           (set-loc-value! h1 v)
                           (set-loc-value! h v)
                           v))
                       (begin
                         (set-loc-value! h h1)
                         h1)))
                 h))))))
       
  (define my-read
    (lambda (p)
      (let-values ([(expr locs k) (read-expr p '() void)])
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
      (let-values ([(expr locs k) (read-expr-initial p '() void)])
        (cond
          [(null? locs) expr]
          [else
           (for-each reduce-loc! locs)
           (k)
           (if (loc? expr)
               (loc-value expr)
               expr)]))))


  ;;;      
  ;;;--------------------------------------------------------------* INIT *---
  ;;; 
  (primitive-set! 'read-token
    (case-lambda
      [() (tokenize (current-input-port))]
      [(p)
       (if (input-port? p)
           (tokenize p)
           (error 'read-token "~s is not an input port" p))]))
  (primitive-set! 'read
    (case-lambda
      [() (my-read (current-input-port))]
      [(p)
       (if (input-port? p)
           (my-read p)
           (error 'read "~s is not an input port" p))]))
  (primitive-set! 'comment-handler
    (make-parameter
      (lambda (x) (void))
      (lambda (x)
        (unless (procedure? x)
          (error 'comment-handler "~s is not a procedure" x))
        x)))
  (let ()
    (define load-handler
      (lambda (x)
        (chi-top-library x)))
    (define read-and-eval
      (lambda (p eval-proc)
        (let ([x (my-read p)])
          (unless (eof-object? x)
            (eval-proc x)
            (read-and-eval p eval-proc)))))
    (primitive-set! 'load
      (case-lambda
        [(x) (load x load-handler)]
        [(x eval-proc)
         (unless (string? x)
           (error 'load "~s is not a string" x))
         (unless (procedure? eval-proc)
           (error 'load "~s is not a procedure" eval-proc))
         (let ([p (open-input-file x)])
           (let ([x (read-initial p)])
             (unless (eof-object? x)
               (eval-proc x)
               (read-and-eval p eval-proc)))
           (close-input-port p))])))
  )

