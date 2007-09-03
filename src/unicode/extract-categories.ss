#!/usr/bin/env ikarus --r6rs-script

(import 
  (ikarus)
  (unicode-data))


(define (codes-in-cats ls cats)
  (let f ([ls ls] [ac '()])
    (cond
      [(null? ls) (reverse ac)]
      [(memq (cdar ls) cats) 
       (f (cdr ls) (cons (caar ls) ac))]
      [else (f (cdr ls) ac)])))

(define (make-xonxoff ls)
  ;;; makes a list where if your index is at an odd
  ;;; position, then you're ON.  If your index is not in 
  ;;; the list, then look for the index before you.
  (cons 0
    (let f ([i 1] [on? #f] 
            [ls (if (= (car ls) 0)
                    (error 'make-xonxoff "first is on")
                    ls)])
      (cond
        [(null? ls) (list i)]
        [(= i (car ls)) 
         (if on?
             (f (+ i 1) #t (cdr ls))
             (cons i (f (+ i 1) #t (cdr ls))))]
        [else
         (if on?
             (cons i (f (+ i 1) #f ls))
             (f (+ i 1) #f ls))]))))


(define (search-on? n v)
  (let ([k (- (vector-length v) 1)])
    (let f ([i 0] [k k])
      (cond
        [(fx= i k) (odd? i)]
        [else
         (let ([j (fxsra (+ i k 1) 1)])
           (cond
             [(<= (vector-ref v j) n) (f j k)]
             [else (f i (- j 1))]))]))))


(define (verify vec ls) 
  (let f ([i 0] [ls ls])
    (unless (> i #x10FFFF)
      (let-values ([(on? ls) 
                    (cond
                      [(null? ls) (values #f '())]
                      [(= i (car ls)) (values #t (cdr ls))]
                      [else (values #f ls)])])
        (unless (equal? on? (search-on? i vec))
          (error #f "did not pass on ~s" i))
        (f (+ i 1) ls)))))
                      

(define (cat fields)
  (let ([num (car fields)]
        [cat (caddr fields)])
     (cons
       (read (open-input-string (format "#x~a" num)))
       (string->symbol cat))))

(define categories
  ;;; 30 categories
  '([Lu      "Letter, Uppercase"]
    [Ll      "Letter, Lowercase"]
    [Lt      "Letter, Titlecase"]
    [Lm      "Letter, Modifier"]
    [Lo      "Letter, Other"]
    [Mn      "Mark, Nonspacing"]
    [Mc      "Mark, Spacing Combining"]
    [Me      "Mark, Enclosing"]
    [Nd      "Number, Decimal Digit"]
    [Nl      "Number, Letter"]
    [No      "Number, Other"]
    [Pc      "Punctuation, Connector"]
    [Pd      "Punctuation, Dash"]
    [Ps      "Punctuation, Open"]
    [Pe      "Punctuation, Close"]
    [Pi      "Punctuation, Initial quote"]
    [Pf      "Punctuation, Final quote"]
    [Po      "Punctuation, Other"]
    [Sm      "Symbol, Math"]
    [Sc      "Symbol, Currency"]
    [Sk      "Symbol, Modifier"]
    [So      "Symbol, Other"]
    [Zs      "Separator, Space"]
    [Zl      "Separator, Line"]
    [Zp      "Separator, Paragraph"]
    [Cc      "Other, Control"]
    [Cf      "Other, Format"]
    [Cs      "Other, Surrogate"]
    [Co      "Other, Private Use"]
    [Cn      "Other, Not Assigned"]
  ))

(define (category-index x)
  (let f ([ls categories] [i 0])
    (cond
      [(null? ls) (error 'category-index "invalid cat ~s" x)]
      [(eq? x (caar ls)) i]
      [else (f (cdr ls) (add1 i))])))

      
(define (insert-missing ls)
  (let f ([ls ls] [i 0] [ac '()])
    (cond
      [(> i #x10FFFF) (reverse ac)]
      [(null? ls) 
       (f ls (+ i 1) (cons (cons i 'Cn) ac))]
      [(= i (caar ls)) 
       (f (cdr ls) (+ i 1) (cons (car ls) ac))]
      [else
       (f ls (+ i 1) (cons (cons i 'Cn) ac))])))

(define (make-cats-table ls) 
  (map 
    (lambda (x)
      (cons (car x) 
        (cons (cadr x) (category-index (cddr x)))))
    (let f ([i 1] [st (car ls)] [ls (cdr ls)] [ac '()])
      (cond
        [(null? ls) (reverse (cons (cons i st) ac))]
        [(eq? (cdar ls) (cdr st)) (f (add1 i) st (cdr ls) ac)]
        [else
         (f 1 (car ls) (cdr ls) (cons (cons i st) ac))]))))


(define (merge-sequences ls) 
  (define (split ls) 
    (cond
      [(null? ls) (values '() '())]
      [(= (caar ls) 1) 
       (let-values ([(chain no-chain) (split (cdr ls))])
         (values (cons (cdar ls) chain) no-chain))]
      [else 
       (values '() ls)]))
  (define (mk-chain a chain) 
    (cond
      [(null? chain) a]
      [else 
       (cons (car a)
         (list->vector 
           (cons (cdr a) 
             (map cdr chain))))]))
  (cond
    [(null? ls) '()]
    [(= (caar ls) 1)
     (let-values ([(chain no-chain) (split (cdr ls))])
       (cons (mk-chain (cdar ls) chain)
         (merge-sequences no-chain)))]
    [else (cons (cdar ls) (merge-sequences (cdr ls)))]))




(let ([ls (map cat (get-unicode-data "UNIDATA/UnicodeData.txt"))])
  (let ([wanted 
         (codes-in-cats ls
           '(Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pd Pc Po Sc Sm Sk So Co))]
        [cats-table (merge-sequences
                      (make-cats-table
                        (insert-missing ls)))])
    (let ([xonxoff (list->vector (make-xonxoff wanted))])
      (verify xonxoff wanted)
      (with-output-to-file "unicode-constituents.ss"
        (lambda ()
          (printf ";;; DO NOT EDIT\n")
          (printf ";;; automatically generated\n")
          (printf ";;; ~s elements in vector\n\n" (vector-length xonxoff))
          (pretty-print 
            `(define unicode-constituents-vector ',xonxoff))
          (printf ";;; ~s elements in cats\n" (length cats-table))
          (pretty-print 
            `(define unicode-categories-lookup-vector 
               ',(list->vector (map car cats-table))))
          (pretty-print 
            `(define unicode-categories-values-vector 
               ',(list->vector (map cdr cats-table))))
          (pretty-print 
            `(define unicode-categories-name-vector 
               ',(list->vector (map car categories)))))
        'replace))))


(printf "Happy Happy Joy Joy\n")
