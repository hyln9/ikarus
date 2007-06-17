

(library (unicode-data)
  (export get-unicode-data)
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
        [(= i n) '("")]
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
             (f (cons fields ls)))]))))

  (define (get-unicode-data)
    (with-input-from-file 
       "UNIDATA/UnicodeData.txt"
       extract-uni-data)))
