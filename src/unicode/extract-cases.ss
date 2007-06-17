#!/usr/bin/env ikarus --r6rs-script

(import 
  (ikarus)
  (unicode-data))

(define (hex->num x)
  (read (open-input-string (format "#x~a" x))))

(define data-case
  (lambda (fields)
    (let ([num (car fields)]
          [uc (list-ref fields uc-index)]
          [lc (list-ref fields lc-index)]
          [tc (list-ref fields tc-index)])
      (let ([n (hex->num num)])
        (define (f x)
          (if (string=? x "") 0 (- (hex->num x) n)))
        (cons n (vector (f uc) (f lc) (f tc) #f))))))

(define (remove-dups ls)
  (let f ([ls ls] [last #f]) 
    (cond
      [(null? ls) '()]
      [(equal? (cdar ls) last) (f (cdr ls) last)]
      [else
       (cons (car ls) (f (cdr ls) (cdar ls)))])))

(define (compute-foldcase ls)
  (define (find-vec idx)
    (cond
      [(assq idx ls) => cdr]
      [else (error 'find-vec "~s is missing" idx)]))
  (define (upper i)
    (+ i (vector-ref (find-vec i) 0)))
  (define (lower i)
    (+ i (vector-ref (find-vec i) 1)))
  (define (set-folder! i j)
    (vector-set! (find-vec i) 3 (- j i)))
  (for-each 
    (lambda (x)
      (let ([idx (car x)] [vec (cdr x)])
        (vector-set! vec 3 
          (- (lower (upper idx)) idx))))
    ls)
  (for-each
    (lambda (idx) 
      (let ([vec (find-vec idx)])
        (vector-set! vec 3 0)))
    ;; turkic chars 
    '(#x130 #x131))
  ls)

(define uc-index 12)
(define lc-index 13)
(define tc-index 14)

(let ([ls 
       (remove-dups
         (compute-foldcase
           (map data-case 
             (get-unicode-data))))])
  (define (p name idx) 
    (pretty-print 
      `(define ,name 
         ',(list->vector (map (lambda (x) (vector-ref (cdr x) idx)) ls)))))
  (let ([v0 (list->vector (map car ls))])
    (with-output-to-file "unicode-char-cases.ss"
      (lambda ()
        (printf ";;; DO NOT EDIT\n;;; automatically generated\n")
        (printf ";;; ~s entries in table\n" (vector-length v0))
        (pretty-print `(define charcase-search-vector ',v0))
        (p 'char-upcase-adjustment-vector 0)
        (p 'char-downcase-adjustment-vector 1)
        (p 'char-titlecase-adjustment-vector 2)
        (p 'char-foldcase-adjustment-vector 3))
      'replace)))
    

(printf "Happy Happy Joy Joy\n")
