(let ()
  (define char-whitespace?
    (lambda (c)
      (or (char= #\space c)
          (memq ($char->fixnum c) '(9 10 11 12 13))))) 
  (define delimiter?
    (lambda (c)
      (or (char-whitespace? c)
          (memq c '(#\( #\) #\[ #\] #\' #\` #\, #\")))))
  (define digit?
    (lambda (c)
      (and (char<= #\0 c) (char<= c #\9))))
  (define char->num
    (lambda (c)
      (fx- ($char->fixnum c) ($char->fixnum #\0))))
  (define initial?
    (lambda (c)
      (or (letter? c) (special-initial? c))))
  (define letter? 
    (lambda (c)
      (or (and (char<= #\a c) (char<= c #\z))
          (and (char<= #\A c) (char<= c #\Z)))))
  (define af? 
    (lambda (c)
      (or (and (char<= #\a c) (char<= c #\f))
          (and (char<= #\A c) (char<= c #\F)))))
  (define af->num
    (lambda (c)
      (if (and (char<= #\a c) (char<= c #\f))
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
          (tokenize-number (fx+ (fx* n 10) (char->num c)) p)]
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
          (tokenize-hex (fx+ (fx* n 16) (char->num c)) p)]
         [(af? c)
          (tokenize-hex (fx+ (fx* n 16) (af->num c)) p)]
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
         [(char= c #\-)
          (cons 'datum (fx- 0 (tokenize-hex 0 p)))]
         [(char= c #\+)
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
         [(char= #\" c)  ls]
         [(char= #\\ c) 
          (let ([c (read-char p)])
            (cond
              [(char= #\" c) (tokenize-string (cons #\" ls) p)]
              [(char= #\\ c) (tokenize-string (cons #\\ ls) p)]
              [(char= #\n c) (tokenize-string (cons #\newline ls) p)]
              [(char= #\t c) (tokenize-string (cons #\tab ls) p)]
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
  (define tokenize-plus
    (lambda (p)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) '(datum . +)]
          [(delimiter? c)  '(datum . +)]
          [(digit? c) 
           (read-char p)
           (cons 'datum (tokenize-number (char->num c) p))]
          [else (error 'tokenize "invalid sequence +~a" c)]))))
  (define tokenize-minus
    (lambda (p)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) '(datum . -)]
          [(delimiter? c)  '(datum . -)]
          [(digit? c) 
           (read-char p)
           (cons 'datum (fx- 0 (tokenize-number (char->num c) p)))]
          [else (error 'tokenize "invalid sequence -~a" c)]))))
  (define tokenize-dot
    (lambda (p)
      (let ([c (peek-char p)])
        (cond
          [(eof-object? c) 'dot]
          [(delimiter? c)  'dot]
          [(char= c #\.)  ; this is second dot
           (read-char p)
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (error 'tokenize "invalid syntax .. near end of file")]
               [(char= c #\.) ; this is the third
                (let ([c (peek-char p)])
                  (cond
                   [(eof-object? c) '(datum . ...)]
                   [(delimiter? c)  '(datum . ...)]
                   [else 
                    (error 'tokenize "invalid syntax ...~a" c)]))]
               [else
                (unread-char c)
                (error 'tokenize "invalid syntax ..~a" c)]))]
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
            [(char= c (string-ref str i))
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
          [(char= (string-ref str 1) c) 
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
          [(char= #\s c) 
           (tokenize-char-seq p "space" '(datum . #\space))]
          [(char= #\n c) 
           (tokenize-char-seq p "newline" '(datum . #\newline))]
          [(char= #\t c) 
           (tokenize-char-seq p "tab" '(datum . #\tab))]
          [(char= #\r c) 
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
  (define multiline-comment
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (multiline-error)]
          [(char= #\| c) 
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) (multiline-error)]
               [(char= #\# c)   (void)]
               [else (multiline-comment p)]))]
          [(char= #\# c)
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) (multiline-error)]
               [(char= #\| c) 
                (multiline-comment p)
                (multiline-comment p)]
               [else 
                (multiline-comment p)]))]
          [else (multiline-comment p)]))))
  (define read-binary
    (lambda (ac chars p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) ac]
          [(char= #\0 c) (read-binary (fxsll ac 1) (cons c chars) p)]
          [(char= #\1 c) (read-binary (fx+ (fxsll ac 1) 1) (cons c chars) p)]
          [(delimiter? c) (unread-char c p) ac]
          [else 
           (unread-char c)
           (error 'tokenize "invalid syntax #b~a"
                  (list->string (reverse (cons c chars))))]))))
  (define tokenize-hash
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (error 'tokenize "invalid # near end of file")]
          [(char= c #\t) 
           (let ([c (peek-char p)])
             (cond
               [(eof-object? c) '(datum . #t)]
               [(delimiter? c)  '(datum . #t)]
               [else (error 'tokenize "invalid syntax near #t")]))]
          [(char= c #\f) 
           (let ([c (peek-char p)])
             (cond
               [(eof-object? c) '(datum . #f)]
               [(delimiter? c)  '(datum . #f)]
               [else (error 'tokenize "invalid syntax near #f")]))]
          [(char= #\\ c) (tokenize-char p)]
          [(char= #\( c) 'vparen]
          [(char= #\x c) (tokenize-hex-init p)]
          [(char= #\' c) '(macro . syntax)]
          [(char= #\; c) 'hash-semi]
          [(char= #\% c) '(macro . |#primitive|)]
          [(char= #\| c) (multiline-comment p) (tokenize p)]
          [(char= #\b c) 
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (error 'tokenize "invalid eof while reading #b")]
               [(char= #\- c)
                (let ([c (read-char p)])
                  (cond
                    [(eof-object? c)
                     (error 'tokenize "invalid eof while reading #b-")]
                    [(char= #\0 c)
                     (cons 'datum
                       (fx- 0 (read-binary 0 '(#\0 #\-) p)))]
                    [(char= #\1 c)
                     (cons 'datum
                       (fx- 0 (read-binary 1 '(#\1 #\-) p)))]
                    [else 
                     (unread-char c p)
                     (error 'tokenize "invalid binary syntax #b-~a" c)]))]
               [(char= #\0 c)
                (cons 'datum (read-binary 0 '(#\0) p))]
               [(char= #\1 c)
                (cons 'datum (read-binary 1 '(#\1) p))]
               [else 
                (unread-char c p)
                (error 'tokenize "invalid syntax #b~a" c)]
               ))]
          [(char= #\! c) 
           (let ([e (read-char p)])
             (when (eof-object? e)
               (error 'tokenize "invalid eof near #!"))
             (unless (char= #\e e)
               (error 'tokenize "invalid syntax near #!~a" e))
             (let ([o (read-char p)])
               (when (eof-object? o)
                 (error 'tokenize "invalid eof near #!e"))
               (unless (char= #\o o)
                 (error 'tokenize "invalid syntax near #!e~a" o))
               (let ([f (read-char p)])
                 (when (eof-object? f)
                   (error 'tokenize "invalid syntax near #!eo"))
                 (unless (char= #\f f)
                   (error 'tokenize "invalid syntax near #!eo~a" f))
                 (cons 'datum (eof-object)))))]
          [else 
           (unread-char c p)
           (error 'tokenize "invalid syntax #~a" c)]))))
  (define tokenize-bar
    (lambda (p ac)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) 
           (error 'tokenize "unexpected eof while reading symbol")]
          [(char= #\\ c)
           (let ([c (read-char p)])
             (cond
               [(eof-object? c) 
                (error 'tokenize "unexpected eof while reading symbol")]
               [else (tokenize-bar p (cons c ac))]))]
          [(char= #\| c) ac]
          [else (tokenize-bar p (cons c ac))]))))
  (define tokenize
    (lambda (p)
      (let ([c (read-char p)])
        (cond
          [(eof-object? c) (eof-object)]
          [(char-whitespace? c) (tokenize p)]
          [(char= #\( c)   'lparen]
          [(char= #\) c)   'rparen]
          [(char= #\[ c)   'lbrack]
          [(char= #\] c)   'rbrack]
          [(char= #\' c)   '(macro . quote)]
          [(char= #\` c)   '(macro . quasiquote)]
          [(char= #\, c) 
           (let ([c (peek-char p)])
             (cond
              [(eof-object? c) '(macro . unquote)]
              [(char= c #\@) 
               (read-char p)
               '(macro . unquote-splicing)]
              [else '(macro . unquote)]))]
          [(char= #\# c)   (tokenize-hash p)]
          [(digit? c) 
           (cons 'datum (tokenize-number (char->num c) p))]
          [(initial? c) 
           (let ([ls (reverse (tokenize-identifier (cons c '()) p))])
             (cons 'datum (string->symbol (list->string ls))))]
          [(char= #\" c) 
           (let ([ls (tokenize-string '() p)])
             (cons 'datum (list->string (reverse ls))))]
          [(char= #\; c)
           (skip-comment p)
           (tokenize p)]
          [(char= #\+ c)
           (tokenize-plus p)]
          [(char= #\- c)
           (tokenize-minus p)]
          [(char= #\. c)
           (tokenize-dot p)]
          [(char= #\| c)
           (let ([ls (reverse (tokenize-bar p '()))])
             (cons 'datum (string->symbol (list->string ls))))]
          [else
           (unread-char c p) 
           (error 'tokenize "invalid syntax ~a" c)]))))

  ;;;
  ;;;--------------------------------------------------------------* READ *---
  ;;;
  (define read-list-rest
    (lambda (p end mis)
      (let ([t (read-token p)])
        (cond
         [(eof-object? t)
          (error 'read "end of file encountered while reading list")]
         [(eq? t end) '()]
         [(eq? t mis) 
          (error 'read "paren mismatch")]
         [(eq? t 'dot)
          (let ([d (read p)])
            (let ([t (read-token p)])
              (cond
               [(eq? t end) d]
               [(eq? t mis) 
                (error 'read "paren mismatch")]
               [(eq? t 'dot)
                (error 'read "cannot have two dots in a list")]
               [else
                (error 'read "expecting ~a, got ~a" end t)])))]
         [(eq? t 'hash-semi)
          (read p)
          (read-list-rest p end mis)]
         [else
          (let ([a (parse-token p t)])
            (let ([d (read-list-rest p end mis)])
              (cons a d)))]))))
  (define read-list-init
    (lambda (p end mis)
      (let ([t (read-token p)])
       (cond
         [(eof-object? t)
          (error 'read "end of file encountered while reading list")]
         [(eq? t end) '()]
         [(eq? t mis) 
          (error 'read "paren mismatch")]
         [(eq? t 'dot)
          (error 'read "invalid dot while reading list")]
         [(eq? t 'hash-semi)
          (read p)
          (read-list-init p end mis)]
         [else
          (let ([a (parse-token p t)])
            (cons a (read-list-rest p end mis)))]))))
  (define vector-put!
    (lambda (v i ls)
      (cond
        [(null? ls) v]
        [else
         (vector-set! v i (car ls))
         (vector-put! v (fxsub1 i) (cdr ls))])))
  (define read-vector
    (lambda (p count ls)
      (let ([t (read-token p)])
        (cond
         [(eof-object? t) 
          (error 'read "end of file encountered while reading a vector")]
         [(eq? t 'rparen) 
          (let ([v (make-vector count)])
            (vector-put! v (fxsub1 count) ls))]
         [(eq? t 'rbrack)
          (error 'read "unexpected ] while reading a vector")]
         [(eq? t 'dot)
          (error 'read "unexpected . while reading a vector")]
         [(eq? t 'hash-semi)
          (read p)
          (read-vector p count ls)]
         [else
          (let ([a (parse-token p t)])
            (read-vector p (fxadd1 count) (cons a ls)))]))))
  (define parse-token
    (lambda (p t)
      (cond
        [(eof-object? t) (eof-object)]
        [(eq? t 'lparen) (read-list-init p 'rparen 'rbrack)]
        [(eq? t 'lbrack) (read-list-init p 'rbrack 'rparen)]
        [(eq? t 'vparen) (read-vector p 0 '())]
        [(eq? t 'hash-semi) 
         (read p) ; ignored expression
         (read p)]
        [(pair? t)
         (cond
           [(eq? (car t) 'datum) (cdr t)]
           [(eq? (car t) 'macro)
            (cons (cdr t) (cons (read p) '()))]
           [else (error 'read "invalid token! ~s" t)])]
        [else
         (error 'read "unexpected ~s found" t)])))
  (define read
    (lambda (p) (parse-token p (read-token p))))
          
  ;;;      
  ;;;--------------------------------------------------------------* INIT *---
  ;;; 
  ($pcb-set! read-token
    (lambda p
      (if (null? p)
          (tokenize (current-input-port))
          (if (null? (cdr p))
              (let ([a (car p)])
                (if (input-port? a)
                    (tokenize a)
                    (error 'read-token
                      "not an input port: ~s ~s ~s"
                      (vector? a) (vector-length a) a)))
              (error 'read-token "too many arguments")))))
  ($pcb-set! read
    (lambda p
      (if (null? p)
          (read (current-input-port))
          (if (null? (cdr p))
              (let ([a (car p)])
                (if (input-port? a)
                    (read a)
                    (error 'read "not an input port: ~s" a)))
              (error 'read "too many arguments")))))
  
  (let ()
    (define read-and-eval
      (lambda (p)
        (let ([x (read p)])
          (unless (eof-object? x)
            (eval x)
            (read-and-eval p)))))
    ($pcb-set! load
      (lambda (x)
        (unless (string? x)
          (error 'load "~s is not a string" x))
        (let ([p (open-input-file x)])
          (read-and-eval p)
          (close-input-port p)))))
  )

