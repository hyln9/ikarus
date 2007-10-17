
(library (ikarus.sort)
  (export list-sort vector-sort vector-sort!)
  (import 
    (ikarus system $fx)
    (except (ikarus) list-sort vector-sort vector-sort!))
  

  (define (merge1 <? a1 ls1 ls2) 
    (cond
      [(null? ls2) ls1]
      [else
       (let ([a2 (car ls2)])
         (cond
           [(<? a2 a1) 
            (cons a2 (merge1 <? a1 ls1 (cdr ls2)))]
           [else
            (cons a1 (merge2 <? a2 (cdr ls1) ls2))]))]))
  
  (define (merge2 <? a2 ls1 ls2) 
    (cond
      [(null? ls1) ls2]
      [else
       (let ([a1 (car ls1)])
         (cond
           [(<? a2 a1) 
            (cons a2 (merge1 <? a1 ls1 (cdr ls2)))]
           [else
            (cons a1 (merge2 <? a2 (cdr ls1) ls2))]))]))

  (define (merge <? ls1 ls2) 
    (cond
      [(null? ls2) ls1]
      [else 
       (let ([a1 (car ls1)] [a2 (car ls2)])
         (cond
           [(<? a2 a1) 
            (cons a2 (merge1 <? a1 ls1 (cdr ls2)))]
           [else
            (cons a1 (merge2 <? a2 (cdr ls1) ls2))]))]))

  (define (sort-head <? ls n)
    (cond
      [($fx= n 0) (values '() ls)]
      [($fx= n 1)
       (values (cons (car ls) '()) (cdr ls))]
      [else
       (let-values ([(sorted-head tail) 
                     (sort-head <? ls ($fxsra n 1))])
         (let-values ([(sorted-tail tail) 
                       (sort-head <? tail ($fx- n ($fxsra n 1)))])
           (values (merge <? sorted-head sorted-tail) tail)))]))

  (define (sort-tail <? ls n) 
    (cond
      [($fx<= n 1) ls]
      [else
       (let-values ([(sorted-head tail) 
                     (sort-head <? ls ($fxsra n 1))])
          (merge <? sorted-head (sort-tail <? tail ($fx- n ($fxsra n 1)))))]))

  (define (list-sort <? ls) 
    (unless (procedure? <?)
      (error 'list-sort "~s is not a procedure" <?))
    (sort-tail <? ls (length ls)))

  (define (vector-sort <? v)
    ;;; FIXME: improve
    (unless (procedure? <?)
      (error 'vector-sort "~s is not a procedure" <?))
    (unless (vector? v)
      (error 'vector-sort "~s is not a vector" v))
    (list->vector 
      (sort-tail <? (vector->list v) (vector-length v))))

  (define (vector-sort! <? v) 
    (unless (procedure? <?)
      (error 'vector-sort! "~s is not a procedure" <?))
    (unless (vector? v)
      (error 'vector-sort! "~s is not a vector" v))
    (let f ([i 0] [v v] 
            [ls (sort-tail <? (vector->list v) (vector-length v))])
      (unless (null? ls) 
        (vector-set! v i (car ls))
        (f (fx+ i 1) v (cdr ls)))))
    
  )


