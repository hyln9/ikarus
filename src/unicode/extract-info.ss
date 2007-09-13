#!/usr/bin/env ikarus --r6rs-script

;;; this file is a mess.

(import 
  (ikarus)
  (unicode-data))


(define (hex-string->number str)
  (or (string->number (string-append "#x" str)) 
      (error 'hex-string->number "invalid ~s" str)))

(define (find-char c s)
  (let f ([i 0] [n (string-length s)])
    (cond
      [(= i n) #f]
      [(char=? (string-ref s i) c) i]
      [else (f (add1 i) n)])))

(define (extract-range str)
 (cond
   [(find-char #\. str)
    =>
    (lambda (i) 
      (cons (hex-string->number (substring str 0 i))
            (hex-string->number (substring str (+ i 2) (string-length str)))))]
   [else 
    (let ([n (hex-string->number str)])
      (cons n n))]))




(define constituent-property  #x010000)
(define uppercase-property    #x020000)
(define lowercase-property    #x040000)
(define titlecase-property    #x080000)
(define alphabetic-property   #x100000)
(define numeric-property      #x200000)
(define whitespace-property   #x400000)

;;; Uppercase = Lu + Other_Uppercase
;;; Lowercase = Ll + Other_Lowercase
;;; Titlecase = Lt
;;; Alphabetic = Lu + Ll + Lt + Lm + Lo + Nl + Other_Alphabetic 
;;; Numeric = ???
;;; White_Space = 

(define proplist-properties
  `(["Other_Uppercase"  ,uppercase-property]
    ["Other_Lowercase"  ,lowercase-property]
    ["Other_Alphabetic" ,alphabetic-property]
    ["White_Space"      ,whitespace-property]))

(define categories
  ;;; 30 categories
  `([Lu ,(+ 00 constituent-property uppercase-property alphabetic-property) "Letter, Uppercase"]
    [Ll ,(+ 01 constituent-property lowercase-property alphabetic-property) "Letter, Lowercase"]
    [Lt ,(+ 02 constituent-property titlecase-property alphabetic-property) "Letter, Titlecase"]
    [Lm ,(+ 03 constituent-property alphabetic-property)  "Letter, Modifier"]
    [Lo ,(+ 04 constituent-property alphabetic-property)  "Letter, Other"]
    [Mn ,(+ 05 constituent-property)  "Mark, Nonspacing"]
    [Mc ,(+ 06 constituent-property)  "Mark, Spacing Combining"]
    [Me ,(+ 07 constituent-property)  "Mark, Enclosing"]
    [Nd ,(+ 08 constituent-property)  "Number, Decimal Digit"]
    [Nl ,(+ 09 constituent-property alphabetic-property)  "Number, Letter"]
    [No ,(+ 10 constituent-property)  "Number, Other"]
    [Pc ,(+ 11 constituent-property)  "Punctuation, Connector"]
    [Pd ,(+ 12 constituent-property)  "Punctuation, Dash"]
    [Ps ,(+ 13 )                      "Punctuation, Open"]
    [Pe ,(+ 14 )                      "Punctuation, Close"]
    [Pi ,(+ 15 )                      "Punctuation, Initial quote"]
    [Pf ,(+ 16 )                      "Punctuation, Final quote"]
    [Po ,(+ 17 constituent-property)  "Punctuation, Other"]
    [Sm ,(+ 18 constituent-property)  "Symbol, Math"]
    [Sc ,(+ 19 constituent-property)  "Symbol, Currency"]
    [Sk ,(+ 20 constituent-property)  "Symbol, Modifier"]
    [So ,(+ 21 constituent-property)  "Symbol, Other"]
    [Zs ,(+ 22 )                      "Separator, Space"]
    [Zl ,(+ 23 )                      "Separator, Line"]
    [Zp ,(+ 24 )                      "Separator, Paragraph"]
    [Cc ,(+ 25 )                      "Other, Control"]
    [Cf ,(+ 26 )                      "Other, Format"]
    [Cs ,(+ 27 )                      "Other, Surrogate"]
    [Co ,(+ 28 constituent-property)  "Other, Private Use"]
    [Cn ,(+ 29 )                      "Other, Not Assigned"]
  ))


(define (category-index x)
  (cond
    [(assq x categories) => cadr]
    [else (error 'category-index "invalid cat ~s" x)]))


(define (make-cats-table ls) 
  (let f ([i 1] [st (car ls)] [ls (cdr ls)] [ac '()])
    (cond
      [(null? ls) (reverse (cons (cons i st) ac))]
      [(equal? (cdar ls) (cdr st)) (f (add1 i) st (cdr ls) ac)]
      [else
       (f 1 (car ls) (cdr ls) (cons (cons i st) ac))])))


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

(define (iota i n)
  (let f ([i i] [n n] [ac '()])
    (cond
      [(= i n) ac]
      [else (f i (sub1 n) (cons (sub1 n) ac))])))

;;; first, make a big vector for all characters
;;; place all in category Cn, unless proven otherwise
(let ([v (make-vector (+ #x10FFFF 1) (category-index 'Cn))])
  (let ([ls (get-unicode-data "UNIDATA/UnicodeData.txt")])
    ;;; interesting parts of each element in ls are:
    ;;; field0: the character index, numeric
    ;;; field2: the category, symbolic
    ;;; field8: if set, then the char has the numeric property
    (for-each 
      (lambda (x) 
        (let ([idx (hex-string->number (list-ref x 0))]
              [cat (category-index (string->symbol (list-ref x 2)))]
              [num? (list-ref x 8)])
          (vector-set! v idx 
            (if (string=? num? "")
                cat
                (fxlogor cat numeric-property)))))
      ls))
  ;;; every element of v now maps to the category-index.
  (let ([ls (get-unicode-data "UNIDATA/PropList.txt")])
    ;;; field0 is a range
    ;;; field1 is a property name
    (for-each
      (lambda (x)
        (let ([range (extract-range (car x))]
              [name (cadr x)])
          (cond
            [(assoc name proplist-properties) =>
             (lambda (a) 
               (let ([n (cadr a)])
                 (let f ([i (car range)] [j (cdr range)])
                   (unless (> i j) 
                     (vector-set! v i (fxlogor (vector-ref v i) n))
                     (f (add1 i) j)))))])))
      ls))
  (let ([table 
         (merge-sequences 
           (make-cats-table
             (map cons 
                  (iota 0 (vector-length v))
                  (vector->list v))))])
    (with-output-to-file "unicode-charinfo.ss"
      (lambda () 
         (printf ";;; DO NOT EDIT\n")
         (printf ";;; automatically generated\n")
         (printf ";;; ~s elements in vectors\n\n" (length table))
         (pretty-print
           `(begin
              (define constituent-property  ,constituent-property ) 
              (define uppercase-property    ,uppercase-property   ) 
              (define lowercase-property    ,lowercase-property   ) 
              (define titlecase-property    ,titlecase-property   ) 
              (define alphabetic-property   ,alphabetic-property  ) 
              (define numeric-property      ,numeric-property     ) 
              (define whitespace-property   ,whitespace-property)))
         (pretty-print 
           `(define unicode-categories-lookup-vector 
              ',(list->vector (map car table))))
         (pretty-print 
           `(define unicode-categories-values-vector 
              ',(list->vector (map cdr table))))
         (pretty-print 
           `(define unicode-categories-name-vector 
              ',(list->vector (map car categories)))))
      'replace)))


(printf "Happy Happy Joy Joy\n")
