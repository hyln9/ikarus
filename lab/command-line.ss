

;;; WORK IN PROGRESS, NOT FOR CONSUMPTION
;;; TODO: long options
;;;       multiple options in one go, e.g., -XYZ
;;;       concat value with option, e.g., -Xxvalue
;;;       usage error message
;;;       -h --help

(import (ikarus))

(define (dispatch-opts arguments data* proc*) 
  (define (print-usage) 
    (error 'usage "TODO" arguments))
        
  (define (option? x) 
    (and (string? x) 
         (>= (string-length x) 2)
         (char=? (string-ref x 0) #\-)
         (not (char=? (string-ref x 1) #\-))))

  (define (fill-char-opt c ls fields k)
    ;;; field = [c required    ] 
    ;;;       | [c flag        ]
    ;;;       | [c zero-plus   ]
    ;;;       | [c optional . str] 
    (let f ([fields fields] [k k])
      (and (pair? fields) 
           (let ([field (car fields)])
             (if (char=? c (car field))
                 (let ([t (cadr field)])
                   (case t
                     [(required optional) 
                      (and (not (null? ls)) 
                           (let ([val (car ls)] [ls (cdr ls)])
                             (k (cons 
                                  (cons* c 'ok val)
                                  (cdr fields))
                                ls)))]
                     [(flag) 
                      (k (cons (cons* c 'ok #t) (cdr fields)) ls)]
                     [(zero-plus)
                      (and (not (null? ls))
                           (let ([val (car ls)])
                             (k (cons (cons* c 'zero-plus
                                        (cons val (cddr field)))
                                      (cdr fields))
                                (cdr ls))))]
                     [else #f]))
                 (f (cdr fields)
                    (lambda (fields ls) 
                      (k (cons field fields) ls))))))))
          
  (define (fill-option a ls fields k)
    (if (= (string-length a) 2)
        (let ([char (string-ref a 1)])
          (fill-char-opt char ls fields 
            (lambda (fields ls) 
              (match-fields fields ls k))))
        (error 'fill-option "not yet")))

  (define (match-fields fields ls k) 
    (if (null? ls) 
        (k fields ls)
        (let ([a (car ls)])
          (if (option? a)
              (fill-option a (cdr ls) fields k)
              (k fields ls)))))

  (define (match-args args ls k) 
    (if (null? args)
        (k '() ls)
        (and (not (null? ls))
             (let ([a (car ls)])
               (and (not (option? a))
                    (match-args (cdr args) (cdr ls)
                      (lambda (a* ls)
                        (k (cons a a*) ls))))))))

  (define (match-args-rest a/f ls k)
    (if a/f
        (let f ([ls ls] [k (lambda (a ls) (k (list a) ls))])
          (if (null? ls)
              (k '() ls)
              (let ([a (car ls)])
                (if (string=? a "--")
                    (k '() ls)
                    (and (not (option? a)) 
                         (f (cdr ls)
                            (lambda (a* ls) 
                              (k (cons a a*) ls))))))))
        (and (or (null? ls) (string=? (car ls) "--"))
             (k '() ls))))
  (define (match-dash-rest a/f ls k)
    (if a/f
        (if (null? ls) 
            (k '(()))
            (and (string=? (car ls) "--")
                 (k (list (cdr ls)))))
        (and (null? ls) (k '()))))

  (define (fix-fields ls k)
    (cond
      [(null? ls) (k '())]
      [else
       (let ([a (car ls)] [k (lambda (a)
                               (fix-fields (cdr ls) 
                                  (lambda (ls) 
                                    (k (cons a ls)))))])
         (let ([type (cadr a)] [value (cddr a)])
           (case type
             [(ok flag optional) (k value)]
             [(zero-plus) (k (reverse value))]
             [else #f])))]))


  (define (match _help fields _field-ids args args-rest dash-rest)
    (let ([prog (car arguments)])
      (match-fields fields (cdr arguments)
        (lambda (fields ls)
          (fix-fields fields
            (lambda (fields)
              (match-args args ls
                (lambda (args ls)
                  (match-args-rest args-rest ls 
                    (lambda (args-rest ls) 
                      (match-dash-rest dash-rest ls 
                        (lambda (dash-rest) 
                          (cons prog 
                            (append fields
                                    args args-rest
                                    dash-rest))))))))))))))

  (let f ([data* data*] [proc* proc*])
    (if (null? data*) 
        (print-usage)
        (let ([opts (apply match (car data*))])
          (if opts 
              (apply (car proc*) opts)
              (f (cdr data*) (cdr proc*)))))))


(define-syntax parse-command-line
  (lambda (stx)
    (define (parse-format stx)
      (define (err str x)
        (syntax-violation #f str stx x))
      (define (prep-str stx)
        (let* ([str (syntax->datum stx)]
               [n (string-length str)]
               [err (lambda (why) (err why stx))])
          (when (< n 2) (err "invalid option string"))
          (unless (char=? (string-ref str 0) #\-)
            (err "option string must start with a dash: -"))
          (cons (string-ref str 1)
            (cond
               [(= n 2) #'(required)]
               [else
                (case (string-ref str 2)
                  [(#\?) #'(flag . #f)]
                  [(#\*) #'(zero-plus . ())]
  ;                [(#\+) #'(one-plus)]
                  [(#\=) 
                   (cons #'optional (substring str 3 n))]
                  [else (err "invalid option")])]))))
      (define (dots? x) 
        (and (identifier? x)
             (free-identifier=? x #'(... ...))))
      (define (id? x)
        (and (identifier? x) (not (dots? x))))
      (define (parse-command-line ls)
        (define (str? x) 
          (let ([d (syntax->datum x)])
            (and (string? d) (not (string=? d "--")))))
        (define (parse-head x)
          (syntax-case x ()
            [(prog . rest) 
             (if (id? #'prog) 
                 (values #'prog #'rest)
                 (err "pattern head is not an identifier" #'prog))]
            [_ (err "invalid pattern" x)]))
        (define (parse-opts x)
          (syntax-case x ()
            [(str id . rest)
             (and (id? #'id) (str? #'str))
             (let-values ([(opt-strs opt-ids rest) (parse-opts #'rest)])
               (values (cons (prep-str #'str) opt-strs)
                       (cons #'id opt-ids) rest))]
            [_ (values '() '() x)]))
        (define (parse-args x)
          (syntax-case x ()
            [(id dots . rest)
             (and (id? #'id) (dots? #'dots))
             (values '() #'id #'rest)]
            [(id . rest) (id? #'id)
             (let-values ([(args args-rest rest) (parse-args #'rest)])
               (values (cons #'id args) args-rest rest))]
            [_ (values '() #f x)]))
        (define (parse-tail x)
          (syntax-case x ()
            [("--" id) (id? #'id) #'id]
            [() #f]
            [_ (err "invalid pattern segment" x)]))
        (let-values ([(prog ls) (parse-head ls)])
          (let-values ([(opts opt-ids ls) (parse-opts ls)])
            (let-values ([(args args-rest ls) (parse-args ls)])
              (values prog opts opt-ids args args-rest (parse-tail ls))))))
      (define (get-fmls x ls1 ls2 m1 m2) 
        (define (cons? x ls) (if x (cons x ls) ls))
        (define (bound-id-member? x ls)
          (and (pair? ls)
               (or (bound-identifier=? x (car ls))
                   (bound-id-member? x (cdr ls)))))
        (let ([ls (cons x (append ls1 ls2 (cons? m1 (cons? m2 '()))))])
          (let f ([x (car ls)] [ls (cdr ls)])
            (unless (null? ls) 
              (if (bound-id-member? x ls)
                  (err "duplicate identifier" x)
                  (f (car ls) (cdr ls)))))
          ls))
      (let-values ([(prog opt-strs opt-ids args args-rest dash-rest)
                    (parse-command-line stx)])
        (list (get-fmls prog opt-ids args args-rest dash-rest)
          opt-strs opt-ids args args-rest dash-rest)))
    (define (parse-clause stx)
      (syntax-case stx ()
        [(format help-str body body* ...)
         (string? (syntax->datum #'help-str))
         (with-syntax ([((fmls ...) . args)
                        (parse-format #'format)])
           (list #'(lambda (fmls ...) body body* ...) 
                 #'(help-str . args)))]
        [(format body body* ...)
         (parse-clause #'(format "" body body* ...))]))
    (syntax-case stx ()
      [(_ expr clause* ...) 
       (with-syntax ([((proc* data*) ...) 
                      (map parse-clause #'(clause* ...))])
       #'(dispatch-opts expr '(data* ...) (list proc* ...)))])))


(define-syntax test 
  (syntax-rules ()
    [(_ command ls expected) 
     (begin
       (printf "testing ~s => ~s ... " '(command ls) 'expected)
       (let ([a1 (command 'ls)]
             [a2 'expected])
         (unless (equal? a1 a2)
           (error #f "failed/got/expected" a1 'expected))
         (printf "OK\n")))]))

(define (command1 ls)
  (parse-command-line ls
    [(p)          "Help0" (list 0 p)]
    [(p p1)       "Help1" (list 1 p p1)]
    [(p p1 p2)    "Help2" (list 2 p p1 p2)]
    [(p p1 p2 p3) "Help3" (list 3 p p1 p2 p3)]))

(test command1 ("p")                 (0 "p"))
(test command1 ("p" "p1")            (1 "p" "p1"))
(test command1 ("p" "p1" "p2")       (2 "p" "p1" "p2"))
(test command1 ("p" "p1" "p2" "p3")  (3 "p" "p1" "p2" "p3"))

(define (command2 ls)
  (parse-command-line ls
    [(p p1 p2 p3) "Help3" (list 3 p p1 p2 p3)]
    [(p p1 p2 ps ...) "Help2" (list 2 p p1 p2 ps)]
    [(p p1 ps ...) "Help1" (list 1 p p1 ps)]
    [(p ps ...) "Help0" (list 0 p ps)]))

(test command2 ("p")                 (0 "p" ()))
(test command2 ("p" "a")             (1 "p" "a" ()))
(test command2 ("p" "a" "b")         (2 "p" "a" "b" ()))
(test command2 ("p" "a" "b" "c")     (3 "p" "a" "b" "c"))
(test command2 ("p" "a" "b" "c" "d") (2 "p" "a" "b" ("c" "d")))

(define (command3 ls)
  (parse-command-line ls
    [(p "-X" xopt "-Y" yopt) (list 'xy p xopt yopt)]
    [(p "-X" xopt) (list 'x p xopt)]
    [(p "-Y" yopt) (list 'y p yopt)]))

(test command3 ("p" "-X" "xopt") (x "p" "xopt"))
(test command3 ("p" "-Y" "yopt") (y "p" "yopt"))
(test command3 ("p" "-X" "xopt" "-Y" "yopt") (xy "p" "xopt" "yopt"))
(test command3 ("p" "-Y" "yopt" "-X" "xopt") (xy "p" "xopt" "yopt"))

(define (command4 ls)
  (parse-command-line ls
    [(p "-X?" xopt "-Y?" yopt) (list p xopt yopt)]
    [(p "-X?" xopt rest ...)   (list p xopt rest)]))

(test command4 ("p")           ("p" #f #f))
(test command4 ("p" "-X")      ("p" #t #f))
(test command4 ("p" "-Y")      ("p" #f #t))
(test command4 ("p" "-X" "-Y") ("p" #t #t))
(test command4 ("p" "-Y" "-X") ("p" #t #t))
(test command4 ("p" "-X" "a")  ("p" #t ("a")))
(test command4 ("p" "a")       ("p" #f ("a")))

(define (command5 ls)
  (parse-command-line ls
    [(p "-X=default" xopt) (list p xopt)]))

(test command5 ("p")              ("p" "default"))
(test command5 ("p" "-X" "hello") ("p" "hello"))

(define (command6 ls)
  (parse-command-line ls
    [(p "-X*" xopts) (list p xopts)]
    [(p "-X*" xopts "-Y*" yopts) (list p xopts yopts)]))

(test command6 ("p")              ("p" ()))
(test command6 ("p" "-X" "a" "-X" "b") ("p" ("a" "b")))
(test command6 ("p" "-X" "a" "-Y" "b") ("p" ("a") ("b")))
(test command6 ("p" "-Y" "b") ("p" () ("b")))
(test command6 ("p" "-X" "a" "-Y" "b" "-X" "c" "-Y" "d")
      ("p" ("a" "c") ("b" "d")))



#!eof
(define (real-test ls)
  (command-line-interface ls
    (options:
      [("-O" "--optimize-level")
       "Specify optimization level"
       numeric-option
       (lambda (n err)
         (if (memv n '(0 1 2))
             (begin (optimize-level n) n)
             (err "valid options are 0, 1, and 2")))]
      [("-L" "--library-path")
       "Adds path to library search path"
       (lambda (s err)
         (library-path (cons s (library-path))))])
    [(program "-O1" "-L" libdirs ... "-l" library-files ...
              "--r6rs-script" script-file args ...)
     "Run <script-file> in R6RS-script mode."
     (list 1 program libdirs library-files script-file args)]
    [(program "-O2" "-L" libdirs ... 
              "--compile-dependencies" script-file)
     "Compile all libraries that <script-file> depends on"
     (list 2 program libdirs script-file)]
    [(program "-O0" "-L" libdirs ... "-l" library-files ...
              init-files ... "--script" script-file args ...)
     "Run <script-file> in R5RS-script mode 
      Each of <library-files> must contain a library and are 
      installed before <init-files> are loaded and
      <script-file> is run."
     (list 3 program libdirs library-files init-files script-file args)]
    [(program "-O0" "-L" libdirs ... "-l" library-files init-files ... 
              "--" args ...)
     "Run Ikarus in interactive mode.  Each of <library-files>
     must contain a library and are installed before the
     <init-files> are loaded"
     (list 4 program libdirs library-files init-files args)]))
