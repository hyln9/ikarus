

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
           (if (null? ac) (f '()) (list->string (reverse ac)))]
          [else (f (cons x ac))]))))
  
  (define (find-semi/hash str i n)
    (cond
      [(or (fx= i n) (memv (string-ref str i) '(#\; #\#))) i]
      [else (find-semi/hash str (+ i 1) n)]))
  
  (define (cleanup str)
    (let ([lo 
           (let f ([i 0] [n (string-length str)])
             (cond
               [(= i n) n]
               [(char=? #\space (string-ref str i)) 
                (f (add1 i) n)]
               [else i]))]
          [hi
           (let f ([i (sub1 (string-length str))])
             (cond
               [(< i 0) i]
               [(char=? #\space (string-ref str i)) 
                (f (sub1 i))]
               [else i]))])
      (if (> hi lo) 
          (substring str lo (+ hi 1))
          "")))

  (define (split str)
    (let f ([i 0] [n (string-length str)])
      (cond
        [(or (= i n) (memv (string-ref str i) '(#\#)))
         '("")]
        [else
         (let ([j (find-semi/hash str i n)])
           (cond
             [(or (= j n) (memv (string-ref str i) '(#\#)))
              (list (cleanup (substring str i j)))]
             [else
              (cons (cleanup (substring str i j))
                    (f (+ j 1) n))]))])))
  
  (define (extract-uni-data)
    (let f ([ls '()])
      (let ([line (read-line)])
        (cond
          [(eof-object? line)
           (reverse ls)]
          [else
           (let ([fields (split line)])
             (if (or (null? fields) (equal? fields '("")))
                 (f ls)
                 (f (cons fields ls))))]))))

  (define (get-unicode-data filename)
    (with-input-from-file 
       filename
       extract-uni-data)))
