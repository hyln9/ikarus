

;;; WORK IN PROGRESS, NOT FOR CONSUMPTION
;;; TODO: long options
;;;       multiple options in one go, e.g., -XYZ
;;;       concat value with option, e.g., -Xxvalue  
;;;       usage error message [ok?]
;;;       -h --help should not be user-defined
;;;       check duplicate options

(import (ikarus))

(define (dispatch-opts arguments data* proc*) 
  (define (print-usage detailed?) 
    (define-record-type f (fields id char type def))
    (define (mkf x id)
      (make-f id (car x) (cadr x) (cddr x)))
    (define (get type ls)
      (filter (lambda (x) (eq? (f-type x) type)) ls))
    (define (fmt-req x)
      (format " -~a <~a>" (f-char x) (f-id x)))
    (define (fmt-req-no-value x)
      (format " -~a" (f-char x)))
    (define (fmt-z c)
      (lambda (x)
        (format " [-~a <~a>]~a" (f-char x) (f-id x) c)))
    (define (fmt-<> x)
      (format " <~a>" x))
    (define (synopsis f* args args-rest)
      (let ([opt*   (get 'optional f*)]
            [flag*  (get 'flag f*)]
            [req0*   (get 'required0 f*)]
            [req1*   (get 'required1 f*)]
            [z0*    (get 'zero-plus f*)]
            [z1*    (get 'one-plus f*)])
        (let-values ([(p e) (open-string-output-port)])
          (display (car arguments) p)
          (display (apply string-append (map fmt-req-no-value req0*)) p)
          (unless (null? flag*) 
            (fprintf p " [-~a]" 
              (list->string (map f-char flag*))))
          (display (apply string-append (map (fmt-z "") opt*)) p)
          (display (apply string-append (map (fmt-z "*") z0*)) p)
          (display (apply string-append (map (fmt-z "+") z1*)) p)
          (display (apply string-append (map fmt-req req1*)) p)
          (display (apply string-append (map fmt-<> args)) p)
          (when args-rest
            (display (string-append (fmt-<> args-rest) " ...") p))
          (e))))
    (define (print-usage-line help fields field-ids args args-rest dash-rest)
      (let ([f* (map mkf fields field-ids)])
        (display "  ")
        (display (synopsis f* args args-rest))
        (newline)
        (unless (string=? help "")
          (display "    ")
          (display help)
          (newline))
        (when detailed?
          (let ([def* (filter f-def (get 'optional f*))])
            (unless (null? def*)
              (for-each
                (lambda (x)
                  (printf "     -~a defaults to ~a\n" (f-char x)
                          (f-def x)))
                def*)))
          (newline))))
    (printf "\nUsage:\n")
    (for-each (lambda (x) (apply print-usage-line x)) data*)
    (print-usage-line "Display this help message" 
        '([#\h required0 . #f])
        '(dummy)
        '()
        #f
        #f)
    #f)
  (define (arguments-match)
    (define-condition-type &help &condition
      make-help-condition help-condition?
      (extended? help-extended?))
    (define-condition-type &unmatched &condition
      make-unmatched-condition unmatched-condition?)
    (define (help x) 
      (raise (make-help-condition x)))
    (define (unmatched) 
      (raise (make-unmatched-condition)))
  

    (define (option? x) 
      (or (equal? x "--help") ;;; hack
          (and (string? x) 
               (>= (string-length x) 2)
               (char=? (string-ref x 0) #\-)
               (not (char=? (string-ref x 1) #\-)))))
  
    (define (fill-char-opt c ls fields)
      ;;; field = [c required0   . _]      ; requires 0 args 
      ;;;       | [c required1   . _]      ; requires 1 arg
      ;;;       | [c flag       . default] ; toggles default to #t
      ;;;       | [c zero-plus  . reversed-list]
      ;;;       | [c one-plus   . reversed-list]
      ;;;       | [c optional   . default] ; overridden by value
      ;;;       | [c ok         . value]   ; already used,  not on input
      (let f ([fields fields])
        (when (null? fields) (unmatched))
        (let ([field (car fields)])
          (if (char=? c (car field))
              (let ([t (cadr field)])
                (case t
                  [(required1 optional) 
                   (when (null? ls) (unmatched))
                   (let ([val (car ls)] [ls (cdr ls)])
                     (values (cons (cons* c 'ok val) (cdr fields)) ls))]
                  [(flag) 
                   (values (cons (cons* c 'ok #t) (cdr fields)) ls)]
                  [(zero-plus one-plus)
                   (when (null? ls) (unmatched))
                   (let ([val (car ls)])
                     (values
                       (cons (cons* c 'zero-plus (cons val (cddr field)))
                             (cdr fields))
                       (cdr ls)))]
                  [else (unmatched)]))
              (let-values ([(fields ls) (f (cdr fields))])
                (values (cons field fields) ls))))))
            
    (define (fill-option a ls fields)
      (when (string=? a "--help") (help #t))
      (if (= (string-length a) 2)
          (let ([char (string-ref a 1)])
            (when (char=? char #\h) (help #f))
            (fill-char-opt char ls fields))
          (error 'fill-option "not yet")))
  
    (define (match-fields fields ls)
      (if (null? ls) 
          (values fields ls)
          (let ([a (car ls)])
            (if (option? a)
                (let-values ([(fields ls) (fill-option a (cdr ls) fields)])
                  (match-fields fields ls))
                (values fields ls)))))
  
    (define (match-args args ls)
      (cond
        [(null? args) (values '() ls)]
        [(null? ls)   (unmatched)]
        [else
         (let ([a (car ls)])
           (when (option? a) (unmatched))
           (let-values ([(a* ls) (match-args (cdr args) (cdr ls))])
             (values (cons a a*) ls)))]))
  
    (define (match-args-rest a/f ls)
      (if a/f
          (let-values ([(x ls) 
                        (let f ([ls ls])
                          (if (null? ls)
                              (values '() ls)
                              (let ([a (car ls)])
                                (if (string=? a "--")
                                    (values '() ls)
                                    (if (option? a)
                                        (unmatched)
                                        (let-values ([(a* ls) (f (cdr ls))])
                                          (values (cons a a*) ls)))))))])
            (values (list x) ls))
          (if (or (null? ls) (string=? (car ls) "--"))
              (values '() ls)
              (unmatched))))
  
    (define (match-dash-rest a/f ls)
      (if a/f
          (if (null? ls) 
              '(())
              (if (string=? (car ls) "--")
                  (list (cdr ls))
                  (unmatched)))
          (if (null? ls) '() (unmatched))))
  
    (define (fix-field x)
      (let ([type (cadr x)] [value (cddr x)])
        (case type
          [(ok flag optional) value]
          [(zero-plus) (reverse value)]
          [else (unmatched)])))
  
    (define (match _help fields _field-ids args args-rest dash-rest)
      (cons (car arguments)
        (let*-values ([(fields ls)    (match-fields fields (cdr arguments))]
                      [(fields)       (map fix-field fields)]
                      [(args ls)      (match-args args ls)]
                      [(args-rest ls) (match-args-rest args-rest ls)]
                      [(dash-rest)    (match-dash-rest dash-rest ls)])
          (append fields args args-rest dash-rest))))
    
    (guard (con
             [(help-condition? con) 
              (print-usage (help-extended? con))])
      (let f ([data* data*] [proc* proc*])
        (if (null? data*) 
            (help #f)
            (guard (con
                     [(unmatched-condition? con)
                      (f (cdr data*) (cdr proc*))])
               (apply (car proc*) (apply match (car data*))))))))
  (arguments-match))

(define-syntax command-line-interface
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
               [(= n 2) #'(required1)]
               [else
                (case (string-ref str 2)
                  [(#\?) #'(flag . #f)]
                  [(#\*) #'(zero-plus . ())]
                  [(#\+) #'(one-plus . ())]
                  [(#\=) 
                   (cons #'optional (substring str 3 n))]
                  [else (err "invalid option")])]))))
      (define (dots? x) 
        (and (identifier? x)
             (free-identifier=? x #'(... ...))))
      (define (id? x)
        (and (identifier? x) (not (dots? x))))
      (define (command-line-interface ls)
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
                    (command-line-interface stx)])
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
  (command-line-interface ls
    [(p)          "Help0" (list 0 p)]
    [(p p1)       "Help1" (list 1 p p1)]
    [(p p1 p2)    "Help2" (list 2 p p1 p2)]
    [(p p1 p2 p3) "Help3" (list 3 p p1 p2 p3)]))

(test command1 ("p")                 (0 "p"))
(test command1 ("p" "p1")            (1 "p" "p1"))
(test command1 ("p" "p1" "p2")       (2 "p" "p1" "p2"))
(test command1 ("p" "p1" "p2" "p3")  (3 "p" "p1" "p2" "p3"))
(test command1 ("./prog" "p1" "p2" "p3" "p4")  #f)

(define (command2 ls)
  (command-line-interface ls
    [(p p1 p2 p3) "Help3" (list 3 p p1 p2 p3)]
    [(p p1 p2 ps ...) "Help2" (list 2 p p1 p2 ps)]
    [(p p1 ps ...) "Help1" (list 1 p p1 ps)]
    [(p ps ...) "Help0" (list 0 p ps)]))

(test command2 ("p")                 (0 "p" ()))
(test command2 ("p" "a")             (1 "p" "a" ()))
(test command2 ("p" "a" "b")         (2 "p" "a" "b" ()))
(test command2 ("p" "a" "b" "c")     (3 "p" "a" "b" "c"))
(test command2 ("p" "a" "b" "c" "d") (2 "p" "a" "b" ("c" "d")))
(test command2 ("./prog" "-h") #f)

(define (command3 ls)
  (command-line-interface ls
    [(p "-X" xopt "-Y" yopt) (list 'xy p xopt yopt)]
    [(p "-X" xopt) (list 'x p xopt)]
    [(p "-Y" yopt) (list 'y p yopt)]))

(test command3 ("p" "-X" "xopt") (x "p" "xopt"))
(test command3 ("p" "-Y" "yopt") (y "p" "yopt"))
(test command3 ("p" "-X" "xopt" "-Y" "yopt") (xy "p" "xopt" "yopt"))
(test command3 ("p" "-Y" "yopt" "-X" "xopt") (xy "p" "xopt" "yopt"))
(test command3 ("./prog") #f)
(test command3 ("./prog" "-h") #f)

(define (command4 ls)
  (command-line-interface ls
    [(p "-X?" xopt "-Y?" yopt) (list p xopt yopt)]
    [(p "-X?" xopt rest ...)   (list p xopt rest)]))

(test command4 ("p")           ("p" #f #f))
(test command4 ("p" "-X")      ("p" #t #f))
(test command4 ("p" "-Y")      ("p" #f #t))
(test command4 ("p" "-X" "-Y") ("p" #t #t))
(test command4 ("p" "-Y" "-X") ("p" #t #t))
(test command4 ("p" "-X" "a")  ("p" #t ("a")))
(test command4 ("p" "a")       ("p" #f ("a")))
(test command4 ("./prog" "-h")      #f)

(define (command5 ls)
  (command-line-interface ls
    [(p "-X=default" xopt) (list p xopt)]))

(test command5 ("p")              ("p" "default"))
(test command5 ("p" "-X" "hello") ("p" "hello"))
(test command5 ("./prog" "-h")         #f)

(define (command6 ls)
  (command-line-interface ls
    [(p "-X*" xopts) (list p xopts)]
    [(p "-X*" xopts "-Y*" yopts) (list p xopts yopts)]))

(test command6 ("p")              ("p" ()))
(test command6 ("p" "-X" "a" "-X" "b") ("p" ("a" "b")))
(test command6 ("p" "-X" "a" "-Y" "b") ("p" ("a") ("b")))
(test command6 ("p" "-Y" "b") ("p" () ("b")))
(test command6 ("p" "-X" "a" "-Y" "b" "-X" "c" "-Y" "d")
      ("p" ("a" "c") ("b" "d")))
(test command6 ("./prog" "-Q" "12") #f)
(test command6 ("./prog" "-h") #f)

(define (command7 ls)
  (command-line-interface ls
    [(p "-X+" xopts) (list p xopts)]
    [(p "-X*" xopts "-Y+" yopts) (list p xopts yopts)]))

(test command7 ("p" "-X" "a") ("p" ("a")))
(test command7 ("p" "-X" "a" "-X" "b") ("p" ("a" "b")))
(test command7 ("p" "-X" "a" "-Y" "b") ("p" ("a") ("b")))
(test command7 ("p" "-Y" "b") ("p" () ("b")))
(test command7 ("p" "-X" "a" "-Y" "b" "-X" "c" "-Y" "d")
      ("p" ("a" "c") ("b" "d")))
(test command7 ("./prog") #f)
(test command7 ("./prog" "-h") #f)

(define (command8 ls)
  (command-line-interface ls
    [(p "-Q=foobar" q "-R=blabla" r "-X?" xopts "-Y?" yopt "-L*" libs "-f" file file* ...) 
     "Does something nice" 
     #t]))

(test command8 ("./prog") #f)
(test command8 ("./prog" "-h") #f)
(test command8 ("./prog" "--help") #f)


(define (ls-command ls)
  (command-line-interface ls
    [(ls "-A?" A "-B?" B "-C?" C "-F?" F "-G?" G "-H?" H "-L?" L
         "-P?" P "-R?" R "-S?" S "-T?" T "-W?" W "-Z?" Z "-a?" a
         "-b?" b "-c?" c "-d?" d "-e?" e "-f?" f "-g?" g "-i?" i
         "-k?" k "-l?" l "-m?" m "-n?" n "-o?" o "-p?" p "-q?" q
         "-r?" r "-s?" s "-t?" t "-u?" u "-w?" w "-x?" x "-1?" o1 
         files ...)
     #t]))

(test ls-command ("ls" "-h") #f)


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
     "Run <script-file> in R5RS-script mode"
      "Each of <library-files> must contain a library and are"
      "installed before <init-files> are loaded and"
      "<script-file> is run."
     (list 3 program libdirs library-files init-files script-file args)]
    [(program "-O0" "-L" libdirs ... "-l" library-files init-files ... 
              "--" args ...)
     "Run Ikarus in interactive mode."
     "Each of <library-files> must contain a library and are"
     "installed before the <init-files> are loaded"
     (list 4 program libdirs library-files init-files args)]))
