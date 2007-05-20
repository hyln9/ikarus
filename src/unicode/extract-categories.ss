#!/usr/bin/env ikarus --r6rs-script

(import (ikarus))


(define (read-line)
  (let f ([ac '()])
    (let ([x (read-char)])
      (cond
        [(eof-object? x)
         (if (null? ac)
             (eof-object) 
             (list->string (reverse ac)))]
        [(char=? x #\newline)
         (if (null? ac) (f) (list->string (reverse ac)))]
        [else (f (cons x ac))]))))

(define (find-semi str i n)
  (cond
    [(or (fx= i n)
         (char=? (string-ref str i) #\;)) i]
    [else (find-semi str (+ i 1) n)]))

(define (split str)
  (let f ([i 0] [n (string-length str)])
    (cond
      [(= i n) '()]
      [else
       (let ([j (find-semi str i n)])
         (cond
           [(= j n) (list (substring str i j))]
           [else
            (cons (substring str i j)
                  (f (+ j 1) n))]))])))

(define (extract-uni-data)
  (let f ([ls '()])
    (let ([line (read-line)])
      (cond
        [(eof-object? line)
         (reverse ls)]
        [else
         (let ([fields (split line)])
           (let ([num (car fields)]
                 [cat (caddr fields)])
             (f (cons
                  (cons
                    (read 
                      (open-input-string (format "#x~a" num)))
                    (string->symbol cat))
                  ls))))]))))

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

(define (odd? n) (= (fxlogand n 1) 1))

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
                      



(let ([ls
       (with-input-from-file 
         "UNIDATA/UnicodeData.txt"
         extract-uni-data)])
  (let ([wanted 
         (codes-in-cats ls
           '(Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pd Pc Po Sc Sm Sk So Co))])
    (let ([xonxoff (list->vector (make-xonxoff wanted))])
      (verify xonxoff wanted)
      (with-output-to-file "unicode-constituents.ss"
        (lambda ()
          (printf ";;; DO NOT EDIT\n")
          (printf ";;; automatically generated\n")
          (printf ";;; ~s elements in vector\n\n" (vector-length xonxoff))
          (pretty-print 
            `(define unicode-constituents-vector ',xonxoff)))
        'replace))))


(printf "Happy Happy Joy Joy\n")
