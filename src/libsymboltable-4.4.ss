;;; require make-symbol and symbol->string
(let ([table '()])
  (letrec ([str=
            (lambda (s1 s2)
              (let ([n ($string-length s1)])
                (and ($fx= n ($string-length s2))
                     (loop s1 s2 0 n))))]
           [loop
            (lambda (s1 s2 i n)
              (or ($fx= i n)
                  (and ($char= ($string-ref s1 i) ($string-ref s2 i))
                       (loop s1 s2 ($fx+ i 1) n))))]
           [lookup
            (lambda (str ls)
              (cond
               [(null? ls) #f]
               [(str= str ($symbol-string ($car ls))) ($car ls)]
               [else (lookup str ($cdr ls))]))])
    ($pcb-set! $intern 
      (lambda (str)
        (or (lookup str table)
            (let ([s ($make-symbol str)])
              (set! table (cons s table))
              s))))
    ($pcb-set! oblist 
      (lambda () table))))
