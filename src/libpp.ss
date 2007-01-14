(let ()
  (define (pretty-width) 70)
  (define-record cbox (length boxes))
  (define-record lbox (length boxes))
  (define-record sbox (length string))
  (define-record pbox (length ls last))
  (define-record mbox (length str val))
  (define-record vbox (length ls))
  (define (box-length x)
    (cond
      [(string? x) (string-length x)]
      [(cbox? x)   (cbox-length x)]
      [(lbox? x)   (lbox-length x)]
      [(sbox? x)   (sbox-length x)]
      [(pbox? x)   (pbox-length x)]
      [(mbox? x)   (mbox-length x)]
      [(vbox? x)   (vbox-length x)]
      [else (error 'boxify "invalid box ~s" x)]))
  (define (boxify x)
    (define (conc . a*)
      (let ([n 
             (let f ([a* a*] [len 0])
               (cond
                 [(null? a*) len]
                 [else
                  (f (cdr a*) (fx+ len (box-length (car a*))))]))])
        (make-cbox n a*)))
    (define (boxify-list ls)
      (define (boxify-list-generic ls)
        (let ([ls (map boxify ls)])
          (let ([n 
                 (let f ([ls ls] [n 1])
                   (cond
                     [(null? ls) n]
                     [else
                      (f (cdr ls)
                         (fx+ (box-length (car ls)) (fxadd1 n)))]))])
            (make-lbox n ls))))
      (define (boxify-reader-macro x)
        (define (macro-string x)
          (cdr (getprop x '*pretty-format*)))
        (let ([str (macro-string (car x))]
              [v (boxify (cadr x))])
          (make-mbox (fx+ (string-length str) (box-length v))
                     str v)))
      (define (reader-macro? x)
        (and (pair? x)
             (let ([a (car x)] [d (cdr x)])
               (and (symbol? a)
                    (pair? d)
                    (null? (cdr d))
                    (let ([p (getprop a '*pretty-format*)])
                      (and p (eq? (car p) 'reader-macro)))))))
      (cond
        [(reader-macro? ls) 
         (boxify-reader-macro ls)]
        [else (boxify-list-generic ls)]))
    (define (boxify-string x)
      (define (count s i j n)
        (cond
          [(fx= i j) n]
          [else
           (let ([c (string-ref s i)])
             (let ([int (char->integer c)])
               (cond
                 [(assv int string-esc-table) =>
                  (lambda (t)
                    (count s (fxadd1 i) j 
                           (fx+ (fx+ n 1) (string-length (cdr t)))))]
                 [(and (fx<= 32 int) (fx<= int 127))
                  (count s (fxadd1 i) j (fxadd1 n))]
                 [else 
                  (count s (fxadd1 i) j (fx+ n 3))])))]))
      (let ([n (string-length x)])
        (let ([m (count x 0 n 0)])
          (if (fx= n m)
              (conc "\"" x "\"")
              (make-sbox (fx+ m 2) x)))))
    (define (boxify-pair x)
      (let-values ([(ls last)
                    (let f ([x x])
                      (cond
                        [(pair? x)
                         (let ([a (boxify (car x))])
                           (let-values ([(ls last) (f (cdr x))])
                             (values (cons a ls) last)))]
                        [else
                         (values '() (boxify x))]))])
        (let ([n 
               (let f ([ls ls] [n 4])
                 (cond
                   [(null? ls) n]
                   [else 
                    (f (cdr ls) 
                       (fx+ (fxadd1 n) (box-length (car ls))))]))])
          (make-pbox (fx+ n (box-length last)) ls last))))

    (define (boxify-vector x)
      (let ([ls (map boxify (vector->list x))])
        (let ([n
               (let f ([ls ls] [n 0])
                 (cond
                   [(null? ls) n]
                   [else
                    (f (cdr ls) (fx+ n (box-length (car ls))))]))])
          (make-vbox (fx+ (fx+ n 2) (vector-length x)) ls))))
    (cond
      [(string? x)    (boxify-string x)]
      [(null? x)      "()"]
      [(vector? x)    (boxify-vector x)]
      [(list? x)      (boxify-list x)]
      [(pair? x)      (boxify-pair x)]
      [else           (format "~s" x)]))

  (define string-esc-table
    '((7 . "a")
      (8 . "b")
      (9 . "t")
      (10 . "n")
      (11 . "v")
      (12 . "f")
      (13 . "r")
      (34 . "\"")
      (92 . "\\")))

  (define (hexify n)
    (cond
      [(fx< n 10) (integer->char (fx+ n (char->integer #\0)))]
      [else (integer->char (fx+ (fx- n 10) (char->integer #\A)))]))
  (define (output x p) 
    (define (output-sbox x p col)
      (display  #\" p)
      (let ([str (sbox-string x)])
        (let f ([i 0] [n (string-length str)] [str str] [p p] [col col])
          (cond
            [(fx= i n)
             (display #\" p)
             (fx+ col 2)]
            [else
             (let ([c (string-ref str i)])
               (let ([int (char->integer c)])
                 (cond
                   [(assv int string-esc-table) =>
                    (lambda (t)
                      (display #\\ p)
                      (display (cdr t) p)
                      (f (fxadd1 i) n str p 
                         (fx+ col (fxadd1 (string-length (cdr t))))))]
                   [(and (fx<= 32 int) (fx<= int 127))
                    (display c p)
                    (f (fxadd1 i) n str p (fxadd1 col))]
                   [else 
                    (display #\\ p)
                    (display (hexify (fxquotient int 16)) p)
                    (display (hexify (fxremainder int 16)) p)
                    (f (fxadd1 i) n str p 
                       (fx+ col 3))])))]))))
    (define (output-cbox x p col)
      (let g ([ls (cbox-boxes x)] [p p] [col col])
        (cond
          [(null? ls) col]
          [else
           (g (cdr ls) p 
              (f (car ls) p col))])))
    (define (tab col p)
      (newline p)
      (let f ([col col] [p p])
        (unless (fxzero? col)
          (display #\space p)
          (f (fxsub1 col) p))))
    (define (output-lbox x p col)
      (define (lbox-one-line x p col ls)
        (display "(" p)
        (let g ([ls (cdr ls)] [p p] 
                [col (f (car ls) p (fx+ col 1))]) 
          (cond
            [(null? ls)
             (display ")" p)
             (fx+ col 1)]
            [else
             (display " " p)
             (g (cdr ls) p
                (f (car ls) p (fxadd1 col)))])))
      (define (lbox-multi-line x p col ls)
        (display "(" p)
        (let ([col (fx+ col 1)])
          (f (car ls) p col)
          (let g ([ls (cdr ls)] [p p] [col col])
            (cond
              [(null? ls) (display ")" p) col]
              [else
               (tab col p)
               (f (car ls) p col)
               (g (cdr ls) p col)]))))
      (define (lbox-multi-fill x p col ls)
        (display "(" p)
        (let g ([ls (cdr ls)] [p p] 
                [start-col (fx+ col 2)]
                [where #f]
                [col (f (car ls) p (fx+ col 1))])
          (cond
            [(null? ls) (display ")" p) (fx+ col 1)]
            [where
             (case where
               [(end) 
                (tab start-col p)
                (g ls p start-col 'start start-col)]
               [(start)
                (g (cdr ls) p start-col 
                   (if (fx>= (fx+ start-col (box-length (car ls)))
                             (pretty-width))
                       'end #f)
                   (f (car ls) p start-col))])]
            [(fx<= (fx+ (fx+ col 1) (box-length (car ls)))
                   (pretty-width))
             ; fits in the rest of the current line
             (display " " p)
             (g (cdr ls) p start-col 
                #f
                (f (car ls) p (fx+ col 1)))]
            [else 
             (g ls p start-col 'end col)]
            #;[else
             ; too big, give it a new line
             (tab start-col p)
             (f (car ls) p start-col)
             (g (cdr ls) p start-col 'end start-col)])))
      (let ([ls (lbox-boxes x)])
        (cond
          [(null? ls) (display "()" p) (fx+ col 2)]
          [(fx<= (fx+ (fx+ col 2) (lbox-length x)) 
                 (pretty-width))
           (lbox-one-line x p col ls)]
          [else
           (lbox-multi-fill x p col ls)])))
    (define (output-pbox x p col)
      (define (pbox-one-line x p col)
        (display "(" p)
        (let g ([ls (pbox-ls x)]
                [p p]
                [col (fx+ col 1)]
                [last (pbox-last x)]) 
          (cond
            [(null? ls)
             (display ". " p)
             (let ([col (f last p (fx+ col 2))])
               (display ")" p)
               (fx+ col 1))]
            [else
             (let ([col (f (car ls) p col)])
               (display " " p)
               (g (cdr ls) p (fx+ col 1) last))])))
      (define (pbox-multi-fill x p col)
        (display "(" p)
        (let g ([ls (cdr (pbox-ls x))]
                [p p] 
                [start-col (fx+ col 1)]
                [col (f (car (pbox-ls x)) p (fx+ col 1))]
                [last (pbox-last x)])
          (cond
            [(null? ls)
             (let ([n (box-length last)])
               (let ([col 
                      (cond
                        [(fx<= (fx+ (fx+ col n) 4) (pretty-width))
                         (display " . " p)
                         (fx+ col 3)]
                        [else
                         (tab start-col p)
                         (display ". " p)
                         (fx+ start-col 2)])])
                  (let ([col (f last p col)])
                    (display ")" p)
                    (fx+ col 1))))]
            [(fx<= (fx+ (fx+ col 1) (box-length (car ls)))
                   (pretty-width))
             (display " " p)
             (g (cdr ls) p start-col 
                (f (car ls) p (fx+ col 1))
                last)]
            [else
             (tab start-col p)
             (g (cdr ls) p start-col 
                (f (car ls) p start-col)
                last)])))
      (cond
        [(fx<= (fx+ col (pbox-length x)) (pretty-width))
         (pbox-one-line x p col)]
        [else
         (pbox-multi-fill x p col)]))
    (define (output-mbox x p col)
      (display (mbox-str x) p)
      (f (mbox-val x) p (fx+ col (string-length (mbox-str x)))))
    (define (output-vbox x p col)
      (let ([ls (vbox-ls x)])
        (cond
          [(null? ls)
           (display "#()" p)
           (fx+ col 3)]
          [else
           (display "#(" p)
           (let g ([ls (cdr ls)] [p p]
                   [col (f (car ls) p (fx+ col 2))]
                   [start (fx+ col 2)])
             (cond
               [(null? ls)
                (display ")" p)
                (fx+ col 1)]
               [(fx<= (fx+ (fx+ col 1) (box-length (car ls))) (pretty-width))
                (display " " p)
                (g (cdr ls) p 
                   (f (car ls) p (fx+ col 1))
                   start)]
               [else
                (tab start p)
                (g (cdr ls) p 
                   (f (car ls) p start)
                   start)]))])))
    (define (f x p col)
      (cond
        [(string? x) 
         (display x p)
         (fx+ col (string-length x))]
        [(cbox? x)   (output-cbox x p col)]
        [(lbox? x)   (output-lbox x p col)]
        [(sbox? x)   (output-sbox x p col)]
        [(pbox? x)   (output-pbox x p col)]
        [(mbox? x)   (output-mbox x p col)]
        [(vbox? x)   (output-vbox x p col)]
        [else (error 'pretty-print-output "invalid ~s" x)]))
    (f x p 0)
    (newline p))
  ;;;
  (define (pretty x p)
    (let ([x (boxify x)])
      (output x p)))
  ;;;
  (define (set-fmt! name fmt)
    (putprop name '*pretty-format* fmt))
  (primitive-set! 'pretty-print 
    (case-lambda
      [(x) (pretty x (current-output-port))]
      [(x p)
       (if (output-port? p)
           (pretty x p)
           (error 'pretty-print "~s is not an output port" p))]))
  (set-fmt! 'quote '(reader-macro . "'"))
  (set-fmt! 'unquote '(reader-macro . ","))
  (set-fmt! 'unquote-splicing '(reader-macro . ",@"))
  (set-fmt! 'quasiquote '(reader-macro . "`"))
  (set-fmt! 'syntax '(reader-macro . "#'"))
  (set-fmt! '|#primitive| '(reader-macro . "#%"))
  (set-fmt! 'define '(_ name tab e tab e ...))
  )

#!eof

(define (test x)
  (pretty-print x)
  (printf "====================================\n"))

(test 12)
(test #t)
(test #f)
(test (if #f #f))
(test '())
(test "string")
(test "\n")
(test "\r")
(test (string (integer->char 0)))
(test (string (integer->char 240)))
(test 'hello)
(test '(foo bar))
(test '
  (define pp 
    (case-lambda
      [(x) (pretty x (current-output-port))]
      [(x p)
       (if (output-port? p)
           (pretty x p)
           (error 'pretty-print "~s is not an output port" p))])))

(test '(384 7384 83947 893478 9137489 3894789 134789314 79817238
        97314897 318947138974 981374 89137489 1374897 13498713
        894713894 137894 89137489 1374 891348314 12 17 9000000 . 17))

(test '(',,@#''(quote (syntax unquote-splicing . 2) 2)))

(test '#(1 2 3))

(test '#(384 7384 83947 893478 9137489 3894789 134789314 79817238
         97314897 318947138974 981374 89137489 1374897 13498713
         894713894 137894 89137489))


(define (test-file x)
  (printf "testing file ~s ...\n" x)
  (with-input-from-file x 
    (lambda ()
      (let f ([i 0])
        (let ([x (read)] [fname (format "tmp.~a.pp" i)])
          (unless (eof-object? x)
            (let ([y
                   (begin
                     (call-with-output-file fname
                        (lambda (p) 
                          (pretty-print x p))
                        'replace)
                     (with-input-from-file fname read))])
              (if (equal? x y)
                  (f (fxadd1 i))
                  (error 'test-file "mismatch ~s ~s" x y)))))))))

(for-each test-file 
  '("fact.ss"
    "libhash.ss"
    "foo.ss"
    "libintelasm.ss"
    "libassembler.ss"
    "libnumerics.ss"
    "libcafe.ss"
    "libposix.ss"
    "libchezio.ss"
    "librecord.ss"
    "libcollect.ss"
    "libtimers.ss"
    "libcompile.ss"
    "libtokenizer.ss"
    "libcontrol.ss"
    "libtoplevel.ss"
    "libcore.ss"
    "libtrace.ss"
    "libcxr.ss"
    "libwriter.ss"
    "libengines.ss" 
    "libfasl.ss" 
    "libguardians.ss"
    "libpp.ss"
    "self-exporting-module.ss"
    "libhandlers.ss"
    "set-operations.ss"
    "psyntax-7.1.ss"
    ))
