
;;;  Copied From CSUG7:

;;; procedure: (pretty-format sym) 
;;; returns: see below 
;;; procedure: (pretty-format sym fmt) 
;;; returns: unspecified

;;; By default, the pretty printer uses a generic algorithm for printing
;;; each form. This procedure is used to override this default and guide
;;; the pretty-printers treatment of specific forms. The symbol sym
;;; names a syntactic form or procedure. With just one argument,
;;; pretty-format returns the current format associated with sym, or #f
;;; if no format is associated with sym.

;;; In the two-argument case, the format fmt is associated with sym for
;;; future invocations of the pretty printer. fmt must be in the
;;; formatting language described below.


;;; <fmt>    ::= (quote symbol)
;;;            | var
;;;            | symbol
;;;            | (read-macro string symbol)
;;;            | (meta)
;;;            | (bracket . fmt-tail)
;;;            | (alt fmt fmt*)
;;;            | fmt-tail
;;; fmt-tail ::= ()
;;;            | (tab fmt ...)
;;;            | (fmt tab ...)
;;;            | (tab fmt . fmt-tail)
;;;            | (fmt ...)
;;;            | (fmt . fmt-tail)
;;;            | (fill tab fmt ...)
;;; tab      ::= int
;;;            | #f

;;; Some of the format forms are used for matching when there are
;;; multiple alternatives, while others are used for matching and
;;; control indentation or printing. A description of each fmt is given
;;; below.


;;; (quote symbol):
;;; This matches only the symbol symbol.

;;; var:
;;; This matches any symbol.

;;; symbol:
;;; This matches any input.

;;; (read-macro string symbol):
;;; This is used for read macros like quote and syntax. It matches any
;;; input of the form (symbol subform). For forms that match, the pretty
;;; printer prints string immediately followed by subform.

;;; (meta):
;;; This is a special case used for the meta keyword (Section 10.7)
;;; which is used as a keyword prefix of another form.

;;; (alt fmt fmt*):
;;; This compares the input against the specified formats and uses the
;;; one that is the closest match. Most often, one of the formats will
;;; match exactly, but in other cases, as when input is malformed or
;;; appears in abstract form in the template of a syntactic abstraction,
;;; none of the formats will match exactly.

;;; (bracket . fmt-tail):
;;; This matches any list-structured input and prints the input enclosed
;;; in square brackets, i.e., [ and ], rather than parentheses.

;;; fmt-tail:
;;; This matches any list-structured input.  Indentation of
;;; list-structured forms is determined via the fmt-tail specifier used
;;; to the last two cases above. A description of each fmt-tail is given
;;; below.


;;; ():
;;; This matches an empty list tail.

;;; (tab fmt ...):
;;; This matches the tail of any proper list; if the tail is nonempty
;;; and the list does not fit entirely on the current line, a line break
;;; is inserted before the first subform of the tail and tab (see below)
;;; determines the amount by which this and all subsequent subforms are
;;; indented.

;;; (fmt tab ...):
;;; This matches the tail of any proper list; if the tail is nonempty
;;; and the list does not fit entirely on the current line, a line break
;;; is inserted after the first subform of the tail and tab (see below)
;;; determines the amount by which all subsequent subforms are indented.

;;; (tab fmt . fmt-tail):
;;; This matches a nonempty tail if the tail of the tail matches
;;; fmt-tail. If the list does not fit entirely on the current line, a
;;; line break is inserted before the first subform of the tail and tab
;;; (see below) determines the amount by which the subform is indented.

;;; (fmt ...):
;;; This matches the tail of any proper list and specified that no line
;;; breaks are to be inserted before or after the current or subsequent
;;; subforms.

;;; (fmt . fmt-tail):
;;; This matches a nonempty tail if the tail of the tail matches
;;; fmt-tail and specifies that no line break is to be inserted before
;;; or after the current subform.

;;; (fill tab fmt ...):
;;; This matches the tail of any proper list and invokes a fill mode in
;;; which the forms are packed with as many as will fit on each line.  A
;;; tab determines the amount by which a list subform is indented. If
;;; tab is a nonnegative exact integer int, the subform is indented int
;;; spaces in from the character position just after the opening
;;; parenthesis or bracket of the parent form. If tab is #f, the
;;; standard indentation is used. The standard indentation can be
;;; determined or changed via the parameter pretty-standard-indent,
;;; which is described later in this section.

;;; In cases where a format is given that doesn't quite match, the
;;; pretty printer tries to use the given format as far as it can. For
;;; example, if a format matches a list-structured form with a specific
;;; number of subforms, but more or fewer subform are given, the pretty
;;; printer will discard or replicate subform formats as necessary.

;;; Here is an example showing the formatting of let might be specified.

;;; (pretty-format 'let
;;;   '(alt (let ([bracket var x] 0 ...) #f e #f e ...)
;;;         (let var ([bracket var x] 0 ...) #f e #f e ...)))

;;; Since let comes in two forms, named and unnamed, two alternatives
;;; are specified. In either case, the bracket fmt is used to enclose
;;; the bindings in square brackets, with all bindings after the first
;;; appearing just below the first (and just after the enclosing opening
;;; parenthesis), if they don't all fit on one line. Each body form is 
;;; indented by the standard indentation.

;;; parameter: pretty-line-length 
;;; parameter: pretty-one-line-limit
;;; The value of each of these parameters must be a positive fixnum.

;;; The parameters pretty-line-length and pretty-one-line-limit control
;;; the output produced by pretty-print. pretty-line-length determines
;;; after which character position (starting from the first) on a line
;;; the pretty printer attempts to cut off output. This is a soft limit
;;; only; if necessary, the pretty-printer will go beyond
;;; pretty-line-length.

;;; pretty-one-line-limit is similar to pretty-line-length, except that
;;; it is relative to the first nonblank position on each line of
;;; output. It is also a soft limit.

;;; parameter: pretty-initial-indent
;;; The value of this parameter must be a nonnegative fixnum.

;;; The parameter pretty-initial-indent is used to tell pretty-print
;;; where on an output line it has been called. If pretty-initial-indent
;;; is zero (the default), pretty-print assumes that the first line of
;;; output it produces will start at the beginning of the line. If set
;;; to a nonzero value n, pretty-print assumes that the first line will
;;; appear at character position n and will adjust its printing of
;;; subsequent lines.

;;; parameter: pretty-standard-indent
;;; The value of this parameter must be a nonnegative fixnum.

;;; This determines the amount by which pretty-print indents
;;; subexpressions of most forms, such as let expressions, from the
;;; form's keyword or first subexpression.

;;; parameter: pretty-maximum-lines
;;; The parameter pretty-maximum-lines controls how many lines
;;; pretty-print prints when it is called. If set to #f (the default),
;;; no limit is imposed; if set to a nonnegative fixnum n, at most n
;;; lines are printed.

(let ()

  (begin ;;; symbol printing helpers
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
    (define valid-symbol-string?
      (lambda (str)
        (or (let ([n ($string-length str)])
              (and ($fx>= n 1)
                   (initial? ($string-ref str 0))
                   (subsequent*? str 1 n)))
            (string=? str "+")
            (string=? str "-")
            (string=? str "..."))))
  
    (define write-symbol-esc-loop
      (lambda (x i n p)
        (unless ($fx= i n)
          (let ([c ($string-ref x i)])
            (when (memq c '(#\\ #\|))
              (write-char #\\ p))
            (write-char c p))
          (write-symbol-esc-loop x ($fxadd1 i) n p))))
    (define write-symbol-esc
      (lambda (x p)
        (write-char #\| p)
        (write-symbol-esc-loop x 0 ($string-length x) p)
        (write-char #\| p)))
    (define write-symbol
      (lambda (x p)
        (let ([str (symbol->string x)])
          (if (valid-symbol-string? str)
              (write-char* str p)
              (write-symbol-esc str p))))))
  (define (symbol-output symbol) ;-> var output box
    (define (symbol-output-length str esc?)
      (define (subs s n i)
        (cond
          [(fx= i n) n]
          [(subsequent? (string-ref s i))
           (subs s n (fxadd1 i))]
          [else
           (esc s n i i)]))
      (define (esc s n i len)
        (cond
          [(fx= i n) (fx+ len 2)] ; for the bars
          [(memv (string-ref s i) '(#\\ #\|))
           (esc s n (fx+ i 1) (fx+ len 2))] ;;; for \ escape
          [else
           (esc s n (fx+ i 1) (fx+ len 1))]))
      (define (init s n)
        (cond
          [(fx= n 0) 2] ; ||
          [else
           (case (string-ref s 0)
             [(#\+ #\-) 
              (if (fx= n 1)
                  1
                  (esc s n 1 1))]
             [(#\.)
              (if (and (fx= n 3) 
                       (char=? (string-ref s 1) #\.)
                       (char=? (string-ref s 2) #\.))
                  3
                  (esc s n 1 1))]
             [else
              (if (initial? (string-ref s 0))
                  (subs s n 1)
                  (esc s n 1 1))])]))
      (if esc?
          (esc str (string-length str) 0 0)
          (init str (string-length str))))
    (if (gensym? symbol)
        `#(var ,symbol
               ,(symbol-output-length (gensym->string symbol) #f)
               ,(symbol-output-length (gensym->unique-string symbol) #t))
        `#(var ,symbol 
               ,(symbol-output-length (symbol->string symbol) #f))))
  (define (number-output number) ; -> string
    (number->string number))
  (define (char-output char) ; -> string
    (let ([i (char->integer char)])
      (cond
        [(fx< i (vector-length char-table)) 
         (string-append "#\\" (vector-ref char-table i))]
        [(fx< i 127)
         (string-append "#\\" (string char))]
        [(fx= i 127) "#\\del"]
        [else (string-append "#\\+" (number->string i))])))
  (define char-table ; first nonprintable chars
    '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel" "bs" "tab" "newline"
       "vt" "ff" "return" "so" "si" "dle" "dc1" "dc2" "dc3" "dc4" "nak" 
       "syn" "etb" "can" "em" "sub" "esc" "fs" "gs" "rs" "us" "space"))
  (define (string-output str) ; -> string | append
    (define (f s n i len)
      (cond
        [(fx= i n) len]
        [(memv (string-ref s i) '(#\newline #\" #\\ #\tab #\return))
         (f s n (fxadd1 i) (fx+ len 2))]
        [else 
         (f s n (fxadd1 i) (fxadd1 len))]))
    (let ([n (string-length str)])
      (let ([m (f str (string-length str) 0 0)])
        (if (fx= n m)
            `#(append "\"" ,str "\"")
            `#(string ,m ,str)))))

  (define (make-output x h i) ;-> output, i
    (cond
      [(or (pair? x) (vector? x))
       (cond
         [(get-hash-table h x #f) =>
          (lambda (n)
            (cond
              [(fx< n 0) ;;; shared and already printed
               (values (ref-output n) i)]
              [(fx= n 0) ;;; not shared
               (if (pair? x)
                   (pair-output x h i)
                   (vector-output x h i))]
              [else ;;; shared and this is the first ref
               (let ([i (fx- i 1)])
                 (put-hash-table! h x i)
                 (let-values ([(no ni) 
                               (if (pair? x)
                                   (pair-output x h i)
                                   (vector-output x h i))])
                   (values `#(append ,(mark-output i) ,no) ni)))]))]
         [else
          (if (pair? x)
              (pair-output x h i)
              (vector-output x h i))])]
      [(symbol? x) (values (symbol-output x) i)]
      [(number? x) (values (number-output x) i)]
      [(char? x) (values (char-output x) i)]
      [(boolean? x) (values (if x "#t" "#f") i)]
      [(null? x) (values "()" i)]
      [(eq? x (void)) (values "#<void>" i)] 
      [(eof-object? x) (values "#!eof" i)]
      [(bwp-object? x) (values "#!bwp" i)]
      [(hash-table? x) (values "#<hash-table>" i)]
      [($unbound-object? x) (values "#<unbound-object>" i)]
      [($forward-ptr? x) (values "#<forward-pointer>" i)]
      [else (values "#<unknown>" i)]))
  (define (vector-output x h i)
    (let* ([n (vector-length x)]
           [v (make-vector n)])
      (let f ([idx 0] [i i])
        (cond
          [(fx= idx n) 
           (values `#(vector ,v) i)]
          [else
           (let-values ([(o i)
                         (make-output (vector-ref x idx) i)])
             (vector-set! v idx o)
             (f (fxadd1 idx) i))]))))
  (define (make-pair-output x h i)
    ;;; first cut, assume no special formatting
      (errrrrrrrrrrrrrr))



  (define write-list
    (lambda (x p h i)
      (cond
       [(and (pair? x) 
             (or (not (get-hash-table h x #f))
                 (fxzero? (get-hash-table h x 0))))
        (write-char #\space p)
        (write-list (cdr x) p h 
          (writer (car x) p h i))]
       [(null? x) i]
       [else
        (write-char #\space p)
        (write-char #\. p)
        (write-char #\space p)
        (writer x p h i)])))
  (define write-vector
    (lambda (x p h i)
      (write-char #\# p)
      (write-char #\( p)
      (let ([n (vector-length x)])
        (let ([i 
               (cond
                 [(fx> n 0)
                  (let f ([idx 1] [i (writer (vector-ref x 0) p h i)])
                    (cond
                      [(fx= idx n)
                       i]
                      [else
                       (write-char #\space p)
                       (f (fxadd1 idx)
                          (writer (vector-ref x idx) p h i))]))]
                 [else i])])
           (write-char #\) p)
           i))))
  (define write-record
    (lambda (x p h i)
      (write-char #\# p)
      (write-char #\[ p)
      (let ([i (writer (record-name x) p h i)])
        (let ([n (record-length x)])
          (let f ([idx 0] [i i])
            (cond
              [(fx= idx n)
               (write-char #\] p)
               i]
              [else 
               (write-char #\space p)
               (f (fxadd1 idx) 
                  (writer (record-ref x idx) p h i))]))))))
  (define macro
    (lambda (x)
      (define macro-forms
        '([quote .            "'"]
          [quasiquote .       "`"]
          [unquote .          ","]
          [unquote-splicing . ",@"]
          [syntax .           "#'"]
          [|#primitive| .     "#%"]))
      (and (pair? x)
           (let ([d ($cdr x)])
             (and (pair? d)
                  (null? ($cdr d))))
           (assq ($car x) macro-forms))))
  (define write-pair
    (lambda (x p h i)
      (write-char #\( p)
      (let ([i (writer (car x) p h i)])
        (let ([i (write-list (cdr x) p h i)])
          (write-char #\) p)
          i))))
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
  (define writer
    (lambda (x p h i)
      (cond
        [(pair? x)
         (pretty-pair x p h i)]
        [(symbol? x)
         (if (gensym? x)
             (write-gensym x p h i)
             (begin (write-symbol x p) i))]
        [(fixnum? x)
         (write-fixnum x p)
         i]
        [(string? x)
         (write-string x p)
         i]
        [(boolean? x)
         (write-char* (if x "#t" "#f") p)
         i]
        [(char? x)
         (write-character x p)
         i]
        [(procedure? x)
         (write-char* "#<procedure>" p)
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
         (write-shareable x p h i write-vector)]
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
        [(record? x)
         (let ([printer (record-printer x)])
           (if (procedure? printer)
               (begin (printer x p) i)
               (write-shareable x p h i write-record)))]
        [(hash-table? x)
         (write-char* "#<hash-table>" p)
         i]
        [($unbound-object? x)
         (write-char* "#<unbound-object>" p)
         i]
        [($forward-ptr? x)
         (write-char* "#<forward-ptr>" p)
         i]
        [(number? x)
         (write-char* (number->string x) p)
         i]
        [else 
         (write-char* "#<unknown>" p)
         i])))


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
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (graph (car x) h)
            (graph (cdr x) h)])]
        [(vector? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (vec-graph x 0 (vector-length x) h)])]
        [(gensym? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))])]))
    (define (dynamic x h)
      (cond
        [(pair? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (dynamic (car x) h)
            (dynamic (cdr x) h)
            (when (and (get-hash-table h x #f)
                       (fxzero? (get-hash-table h x #f)))
              (put-hash-table! h x #f))])]
        [(vector? x)
         (cond
           [(get-hash-table h x #f) =>
            (lambda (n)
              (put-hash-table! h x (fxadd1 n)))]
           [else
            (put-hash-table! h x 0)
            (vec-dynamic x 0 (vector-length x) h)
            (when (and (get-hash-table h x #f)
                       (fxzero? (get-hash-table h x #f)))
              (put-hash-table! h x #f))])])) 
    (if (print-graph) 
        (graph x h)
        (dynamic x h)))

  (define (write x p)
    (let ([h (make-hash-table)])
      (hasher x h)
      (writer x p h 0))
    (flush-output-port p))



(let ()
;;; <fmt>    ::= (quote symbol)
;;;            | var
;;;            | symbol
;;;            | (read-macro string symbol)
;;;            | (meta)
;;;            | (bracket . fmt-list)
;;;            | (alt fmt fmt*)
;;;            | fmt-list
;;; fmt-list ::= ()
;;;            | (tab fmt ...)
;;;            | (fmt tab ...)
;;;            | (tab fmt . fmt-list)
;;;            | (fmt ...)
;;;            | (fmt . fmt-list)
;;;            | (fill tab fmt ...)
;;; tab      ::= int
;;;            | #f


  )

(pretty-format 
  '([letrec    (_ ((x _) ...) #t _ _ ...)]
    [let       (_ x ([x _] 0 ...) #t _ _ ...)]
    [cond      (_ (or (_ 0 _) (_ '=> 0 _)) ...)]

