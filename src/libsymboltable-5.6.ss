(let ()
;;; (define hash-loop
;;;   (lambda (str i j h)
;;;     (cond
;;;       [($fx= i j)  h]
;;;;        ($fxlogxor h ($fxsra h 15))]
;;;       [else
;;;        (hash-loop str ($fxadd1 i) j 
;;;          ($fxlogxor 
;;;            ($char->fixnum ($string-ref str i))
;;;            ($fxlogxor 
;;;              ($fxsll h 5)
;;;              ($fxsra h 23))))])))
 (define hash-loop
   (lambda (str i j h)
     (cond
       [($fx= i j) 
        (let* ([h ($fx+ h ($fxsll h 3))]
               [h ($fxlogxor h ($fxsra h 11))]
               [h ($fx+ h ($fxsll h 15))])
          h)]
       [else
        (hash-loop str ($fxadd1 i) j
          (let ([h ($fx+ h ($char->fixnum ($string-ref str i)))])
            (let ([h ($fx+ h ($fxsll h 10))])
              ($fxlogxor h ($fxsra h 6)))))])))
 (define hash-function
   (lambda (str)
     (let ([n ($string-length str)])
       (hash-loop str 0 n 0))))
 (define str=
    (lambda (s1 s2 i n)
      (or ($fx= i n)
          (and ($char= ($string-ref s1 i) ($string-ref s2 i))
               (str= s1 s2 ($fxadd1 i) n)))))
  (define bucket-lookup
    (lambda (str strlen ls)
      (if (null? ls)
           '#f
           (let ([a ($car ls)])
             (let ([str2 ($symbol-string a)])
               (if (and ($fx= ($string-length str2) strlen)
                        (str= str str2 0 strlen))
                   a
                   (bucket-lookup str strlen ($cdr ls))))))))
  (define intern
    (lambda (str htable)
      (let ([h (hash-function str)])
        (let ([idx ($fxlogand h ($fx- ($vector-length htable) 1))])
          (let ([bucket ($vector-ref htable idx)])
            (or (bucket-lookup str ($string-length str) bucket)
                (let ([sym ($make-symbol str)])
                  ($vector-set! htable idx (cons sym bucket))
                  ($set-symbol-unique-string! sym #f)
                  sym)))))))
  (define old-intern
    (lambda (str htable)
      (or (bucket-lookup str ($string-length str) ($vector-ref htable 0))
          (let ([sym ($make-symbol str)])
            ($vector-set! htable 0 (cons sym ($vector-ref htable 0)))
            sym))))
  (define init-vec
    (lambda (v i n)
      (unless ($fx= i n)
        ($vector-set! v i '())
        (init-vec v ($fxadd1 i) n))))
  
  (define revappend
    (lambda (ls ac)
      (cond
        [(null? ls) ac]
        [else (revappend ($cdr ls) (cons ($car ls) ac))])))

  (define vec->list
    (lambda (v i j ls)
      (cond
        [($fx= i j) ls]
        [else
         (vec->list v ($fxadd1 i) j
            (revappend ($vector-ref v i) ls))])))

  (define hash-vec ($make-vector 4096))
  
  (init-vec hash-vec 0 4096)

  ($pcb-set! $intern
    (lambda (str)
      (intern str hash-vec)))
  
  ($pcb-set! oblist
    (lambda ()
      (vec->list hash-vec 0 4096 '()))))

