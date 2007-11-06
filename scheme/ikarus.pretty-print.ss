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


(library (ikarus pretty-print)
  (export pretty-print pretty-width)
  (import 
    (rnrs hashtables)
    (except (ikarus) pretty-print pretty-width))
  (define (map1ltr f ls)
    ;;; ltr so that gensym counts get assigned properly
    (cond
      [(null? ls) '()]
      [else
       (let ([a (f (car ls))])
         (cons a (map1ltr f (cdr ls))))]))

  (define pretty-width
    (make-parameter 60
      (lambda (x) 
        (unless (and (exact? x) (integer? x) (> x 0))
          (error 'pretty-width "invalid argument" x))
        x)))

  (define (pretty-indent) 1)
  (define-struct cbox (length boxes))
  (define-struct pbox (length ls last))
  (define-struct mbox (length str val))
  (define-struct vbox (length ls))
  (define-struct fbox (length box* sep*))
  (define (box-length x)
    (cond
      [(string? x) (string-length x)]
      [(cbox? x)   (cbox-length x)]
      [(pbox? x)   (pbox-length x)]
      [(mbox? x)   (mbox-length x)]
      [(vbox? x)   (vbox-length x)]
      [(fbox? x)   (fbox-length x)]
      [else (error 'boxify "invalid box" x)]))
  (define (boxify x)
    (define (conc . a*)
      (let ([n 
             (let f ([a* a*] [len 0])
               (cond
                 [(null? a*) len]
                 [else
                  (f (cdr a*) (fx+ len (box-length (car a*))))]))])
        (make-cbox n a*)))
    (define (boxify-list ls alt-fmt*)
      (define (sum-box* ls)
        (cond
          [(null? (cdr ls)) 
           (fx+ (box-length (car ls)) 2)]
          [else 
           (fx+ (box-length (car ls)) 
                (fxadd1 (sum-box* (cdr ls))))]))
      (define (gensep*-default ls)
        (cond
          [(null? (cdr ls)) '()]
          [else
           (cons (pretty-indent) (gensep*-default (cdr ls)))]))
      (define (tab-value x)
        (cond
          [(eq? x 'tab) (pretty-indent)]
          [(fixnum? x) x]
          [else #f]))
      (define (select-alt alt-fmt* ls)
        (define (good-match? fmt ls)
          (cond
            [(not (pair? fmt)) #t]
            [(eq? (car fmt) 'read-macro)
             (and (list? ls) (fx= (length ls) 2))]
            [else
             (let ([a (car fmt)] [fmt (cdr fmt)])
               (cond
                 [(or (eq? a 'tab) (fixnum? a))
                  (good-match? fmt ls)]
                 [(and (pair? fmt) (eq? (car fmt) '...))
                  (and (list? ls)
                       (andmap (lambda (x) (good-match? a x)) ls))]
                 [(pair? ls)
                  (and (good-match? a (car ls))
                       (good-match? fmt (cdr ls)))]
                 [else #f]))]))
         (ormap (lambda (fmt) (and (good-match? fmt ls) fmt)) 
                alt-fmt*))
      (define (applicable-formats a alt-fmt*)
        (cond
          [(and (symbol? a) (getprop a *pretty-format*)) =>
           (lambda (fmt) 
             (cond
               [(and (pair? fmt) (eq? (car fmt) 'alt))
                (append alt-fmt* (cdr fmt))]
               [else
                (append alt-fmt* (list fmt))]))]
          [(null? alt-fmt*) #f]
          [else       alt-fmt*]))
      (define (return sep* box*)
        (let ([n (sum-box* box*)])
          (make-fbox n box* sep*)))
      (let ([a (car ls)])
        (cond
          [(applicable-formats a alt-fmt*) =>
           (lambda (fmt*)
             (let ([fmt (select-alt fmt* ls)])
               (module (fmt-dots? skip-fmt fmt-tab sub-fmt)
                 (define (parse-fmt x)
                   (define (parse-dots tab fmt x)
                     (cond
                       [(and (pair? x) (eq? (car x) '...))
                        (values tab fmt #t (cdr x))]
                       [else
                        (values tab fmt #f x)]))
                   (define (parse-tab tab x)
                     (cond
                       [(pair? x)
                        (parse-dots tab (car x) (cdr x))]
                       [else (values tab #f #f #f)]))
                   (cond
                     [(pair? x)
                      (let ([a0 (car x)])
                        (cond
                          [(eq? a0 'tab) 
                           (parse-tab (pretty-indent) (cdr x))]
                          [(fixnum? a0) 
                           (parse-tab a0 (cdr x))]
                          [else (parse-tab #f x)]))]
                     [else (values (pretty-indent) #f #f #f)]))
                 (define (fmt-dots? x)
                   (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                      dots))
                 (define (fmt-tab x)
                   (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                      tab))
                 (define (sub-fmt x)
                   (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                      subfmt))
                 (define (skip-fmt x)
                   (let-values ([(tab subfmt dots fmt) (parse-fmt x)])
                      fmt)))
               ;(import M)
               (define (boxify/fmt fmt x) 
                 (cond
                   [(and (pair? fmt) (pair? x) (list? x))
                    (boxify-list x 
                      (if (eq? (car fmt) 'alt)
                          (cdr fmt)
                          (list fmt)))]
                   [else (boxify x)]))
               (define (read-macro? x)
                 (and (pair? x) (eq? (car x) 'read-macro)))
               (cond
                 [(read-macro? fmt) 
                  (conc (cdr fmt) (boxify (cadr ls)))]
                 [(fmt-dots? fmt) 
                  (return (fmt-tab fmt) 
                          (map1ltr (lambda (x) (boxify/fmt (sub-fmt fmt) x))
                               ls))]
                 [else
                  (let-values ([(sep* ls)
                                (let f ([fmt (skip-fmt fmt)] [ls (cdr ls)])
                                  (cond
                                    [(null? ls) 
                                     (values '() '())]
                                    [(fmt-dots? fmt) 
                                     (values (fmt-tab fmt) 
                                             (map1ltr (lambda (x)
                                                    (boxify/fmt (sub-fmt fmt) x))
                                                  ls))]
                                    [else
                                     (let-values ([(f^ l^) 
                                                   (f (skip-fmt fmt) (cdr ls))])
                                       (values (cons (fmt-tab fmt) f^)
                                               (cons (boxify/fmt
                                                       (sub-fmt fmt)
                                                       (car ls))
                                                     l^)))]))])
                    (return sep* (cons (boxify/fmt (sub-fmt fmt) a) ls)))])))]
            [else 
             (return (gensep*-default ls) (map1ltr boxify ls))])))
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
      (let ([ls (map1ltr boxify (vector->list x))])
        (let ([n
               (let f ([ls ls] [n 0])
                 (cond
                   [(null? ls) n]
                   [else
                    (f (cdr ls) (fx+ n (box-length (car ls))))]))])
          (make-vbox (fx+ (fx+ n 2) (vector-length x)) ls))))
    (cond
      [(null? x)      "()"]
      [(vector? x)    (boxify-vector x)]
      [(list? x)      (boxify-list x '())]
      [(pair? x)      (boxify-pair x)]
      [(setbox? x) 
       (let ([i (format "#~a=" (setbox-idx x))]
             [b (boxify (setbox-data x))])
         (make-cbox (+ (string-length i) (box-length b))
           (list i b)))]
      [(refbox? x) (format "#~a#" (refbox-idx x))]
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
    (define (output-fbox x p col)
      (define (output-rest-cont box* sep* p col left)
        (cond
          [(null? box*) col]
          [(pair? sep*) 
           (let* ([box (car box*)]
                  [sep (car sep*)]
                  [w (box-length box)])
             (cond
               [(fx<= (fx+ (fxadd1 w) col) (pretty-width))
                (display " " p)
                (output-rest-cont (cdr box*) (cdr sep*) p 
                  (f box p (fxadd1 col)) left)]
               [(not sep)
                (display " " p)
                (output-rest-multi (cdr box*) (cdr sep*) p 
                   (f box p (fxadd1 col)) left)]
               [else
                (let ([col (fx+ left sep)])
                  (tab col p)
                  (cond
                    [(fx<= (fx+ w col) (pretty-width))
                     (output-rest-cont (cdr box*) (cdr sep*) p 
                       (f box p col) left)]
                    [else
                     (output-rest-multi (cdr box*) (cdr sep*) p
                       (f box p col) left)]))]))]
          [else 
           (output-last-cont box* sep* p col left)]))
      (define (output-last-cont box* sep p col left)
        (define (sum ls)
          (cond
            [(null? ls) 0]
            [else (fx+ (box-length (car ls)) 
                       (fxadd1 (sum (cdr ls))))]))
        (cond
          [(not sep) 
           (output-rest-cont box* '(#f . #f) p col left)]
          [(fx<= (fx+ (sum box*) col) (pretty-width))
           (let g ([box* box*] [p p] [col col])
             (cond
               [(null? box*) col]
               [else
                (display " " p)
                (g (cdr box*) p (f (car box*) p (fxadd1 col)))]))]
          [else 
           (let g ([box* box*] [p p] [left (fx+ left sep)] [col col])
             (cond
               [(null? box*) col]
               [else
                (tab left p)
                (g (cdr box*) p left 
                   (f (car box*) p left))]))]))
      (define (output-last-multi box* sep p col left)
        (define (sum ls)
          (cond
            [(null? ls) 0]
            [else (fx+ (box-length (car ls)) 
                       (fxadd1 (sum (cdr ls))))]))
        (cond
          [(not sep) 
           (output-rest-multi box* '(#f . #f) p col left)]
          [else 
           (let g ([box* box*] [p p] [left (fx+ left sep)] [col col])
             (cond
               [(null? box*) col]
               [else
                (tab left p)
                (g (cdr box*) p left 
                   (f (car box*) p left))]))]))
      (define (output-rest-multi box* sep* p col left)
        (cond
          [(null? box*) col]
          [(pair? sep*) 
           (let* ([box (car box*)]
                  [sep (car sep*)]
                  [w (box-length box)])
             (cond
               [(not sep)
                (display " " p)
                (output-rest-multi (cdr box*) (cdr sep*) p 
                   (f box p (fxadd1 col)) left)]
               [else
                (let ([col (fx+ left sep)])
                  (tab col p)
                  (cond
                    [(fx<= (fx+ w col) (pretty-width))
                     (output-rest-cont (cdr box*) (cdr sep*) p 
                       (f box p col) left)]
                    [else
                     (output-rest-multi (cdr box*) (cdr sep*) p
                       (f box p col) left)]))]))]
          [else (output-last-multi box* sep* p col left)]))                
      (define (output-box-init box box* sep* p left)
        (let ([w (box-length box)])
          (cond
            [(fx<= (fx+ w left) (pretty-width))
             (let ([col (f box p left)])
               (output-rest-cont box* sep* p col left))]
            [else
             (let ([col (f box p left)])
               (output-rest-multi box* sep* p col left))])))
      (display "(" p)
      (let ([col (fx+ col 1)]
            [box* (fbox-box* x)]
            [sep* (fbox-sep* x)])
        (let ([col (output-box-init (car box*) (cdr box*) sep* p col)])
          (display ")" p)
          (fx+ col 1))))
    (define (f x p col)
      (cond
        [(string? x) 
         (display x p)
         (fx+ col (string-length x))]
        [(cbox? x)   (output-cbox x p col)]
        [(pbox? x)   (output-pbox x p col)]
        [(mbox? x)   (output-mbox x p col)]
        [(vbox? x)   (output-vbox x p col)]
        [(fbox? x)   (output-fbox x p col)]
        [else (error 'pretty-print-output "invalid" x)]))
    (f x p 0)
    (newline p))
  ;;;

  (define (hasher x h)
    (define (vec-graph x i j)
      (unless (fx= i j)
        (graph (vector-ref x i))
        (vec-graph x (fxadd1 i) j h)))
    (define (vec-dynamic x i j)
      (unless (fx= i j)
        (dynamic (vector-ref x i))
        (vec-dynamic x (fxadd1 i) j)))
    (define rv #f)
    (define (graph x)
      (cond
        [(pair? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (set! rv #t)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (graph (car x))
            (graph (cdr x))])]
        [(vector? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (set! rv #t)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (vec-graph x 0 (vector-length x))])]
        [(gensym? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (set! rv #t)
              (hashtable-set! h x (fxadd1 n)))])]))
    (define (dynamic x)
      (cond
        [(pair? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (set! rv #t)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (dynamic (car x))
            (dynamic (cdr x))
            (when (and (hashtable-ref h x #f)
                       (fxzero? (hashtable-ref h x #f)))
              (hashtable-set! h x #f))])]
        [(vector? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n)
              (set! rv #t)
              (hashtable-set! h x (fxadd1 n)))]
           [else
            (hashtable-set! h x 0)
            (vec-dynamic x 0 (vector-length x))
            (when (and (hashtable-ref h x #f)
                       (fxzero? (hashtable-ref h x #f)))
              (hashtable-set! h x #f))])])) 
    (if (print-graph) 
        (graph x)
        (dynamic x))
    rv)

  (define-struct setbox (idx data))
  (define-struct refbox (idx))

  (define (rewrite-shared x h)
    (define counter 0)
    (let f ([x x])
      (cond
        [(pair? x) 
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n) 
              (cond
                [(setbox? n)
                 (make-refbox (setbox-idx n))] 
                [(and (fixnum? n) (fx> n 0))
                 (let ([box (make-setbox counter #f)])
                   (set! counter (add1 counter))
                   (hashtable-set! h x box)
                   (let* ([a (f (car x))]
                          [d (f (cdr x))])
                     (set-setbox-data! box (cons a d))
                     box))]
                [else
                 (let* ([a (f (car x))]
                        [d (f (cdr x))])
                   (if (and (eq? a (car x))
                            (eq? d (cdr x)))
                       x
                       (cons a d)))]))]
           [else
            (let* ([a (f (car x))]
                   [d (f (cdr x))])
              (if (and (eq? a (car x))
                       (eq? d (cdr x)))
                  x
                  (cons a d)))])]
        [(vector? x)
         (cond
           [(hashtable-ref h x #f) =>
            (lambda (n) 
              (cond
                [(setbox? n)
                 (make-refbox (setbox-idx n))]
                [(and (fixnum? n) (fx> n 0))
                 (let ([box (make-setbox counter #f)])
                   (set! counter (add1 counter))
                   (hashtable-set! h x box)
                   (set-setbox-data! box
                     (list->vector
                       (map1ltr f (vector->list x))))
                   box)]
                [else 
                 (list->vector (map1ltr f (vector->list x)))]))]
           [else
            (list->vector (map1ltr f (vector->list x)))])]
        [else x])))

  (define (unshare x) 
    (let ([h (make-eq-hashtable)])
      (if (hasher x h)
          (rewrite-shared x h)
          x)))
  ;;;
  (define (pretty x p)
    (output (boxify (unshare x)) p))
  ;;;
  (define *pretty-format* '*pretty-format*)
  (define (set-fmt! name fmt)
    (putprop name *pretty-format* fmt))
  (define pretty-print 
    (case-lambda
      [(x) (pretty x (current-output-port))]
      [(x p)
       (if (output-port? p)
           (pretty x p)
           (error 'pretty-print "not an output port" p))]))
  ;;; standard formats
  (set-fmt! 'quote '(read-macro . "'"))
  (set-fmt! 'unquote '(read-macro . ","))
  (set-fmt! 'unquote-splicing '(read-macro . ",@"))
  (set-fmt! 'quasiquote '(read-macro . "`"))
  (set-fmt! 'syntax '(read-macro . "#'"))
  (set-fmt! 'quasisyntax '(read-macro . "#`"))
  (set-fmt! 'unsyntax '(read-macro . "#,"))
  (set-fmt! 'unsyntax-splicing '(read-macro . "#,@"))
  (set-fmt! '|#primitive| '(read-macro . "#%"))
  (set-fmt! 'let '(alt 
                    (_ (0 [e 0 e] ...) tab e ...)
                    (_ x (0 [e 0 e] ...) tab e ...)))
  (set-fmt! 'letrec '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let-syntax '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'letrec-syntax '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let* '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let-values '(_ (0 [e 0 e] ...) tab e tab e* ...))
  (set-fmt! 'cond '(_ tab [0 e ...] ...))
  (set-fmt! 'define '(_ name tab e ...))
  (set-fmt! 'case-lambda 
     '(_ tab [0 e ...] ...))
  (set-fmt! 'struct-case 
     '(_ e tab [e 0 e ...] ...))
  (set-fmt! 'if '(_ test 3 e ...))
  (set-fmt! 'and '(and test 4 e ...))
  (set-fmt! 'or '(or test 3 e ...))
  (set-fmt! 'begin '(_ tab e ...))
  (set-fmt! 'lambda '(_ fmls tab e tab e* ...))
  (set-fmt! 'case '(_ e tab [e 0 e] ...))
  (set-fmt! 'syntax-rules '(_ kwd* tab [e 0 e] ...))
  (set-fmt! 'syntax-case '(_ expr kwd*  
                             tab (e 0 e 0 e ...) ...))
  (set-fmt! 'module '(alt (_ (fill ...) tab e ...)
                          (_ name (fill ...) tab e ...)))
  (set-fmt! 'library '(_ name tab e ...))
  (set-fmt! 'import '(_ tab e ...))

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
           (error 'pretty-print "not an output port" p))])))

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
                  (error 'test-file "mismatch" x y)))))))))

