
(let ()
  (define k* '())
  
  (define display-prefix
    (lambda (ls t)
      (unless (null? ls)
        (display (if t "|" " "))
        (display-prefix (cdr ls) (not t)))))
  
  (define display-trace
    (lambda (k* v)
      (display-prefix k* #t)
      (write v)
      (newline)))

  (define make-traced-procedure
    (lambda (name proc)
      (lambda args
        (call/cf
          (lambda (f)
            (cond
              [(memq f k*) => 
               (lambda (ls)
                 (display-trace ls (cons name args))
                 (apply proc args))]
              [else
               (display-trace (cons 1 k*) (cons name args))
               (dynamic-wind
                 (lambda () (set! k* (cons f k*)))
                 (lambda () 
                   (let ([v 
                          (call/cf
                            (lambda (nf)
                              (set! f nf)
                              (set-car! k* nf)
                              (apply proc args)))])
                     (display-trace k* v)
                     v))
                 (lambda () (set! k* (cdr k*))))]))))))
      
  (define traced-symbols '())

  (define trace-symbol!
    (lambda (s)
      (cond
        [(assq s traced-symbols) =>
         (lambda (pr)
           (let ([a (cdr pr)] [v (top-level-value s)])
             (unless (eq? (cdr a) v)
               (unless (procedure? v)
                 (error 'trace
                        "the top-level value of ~s is ~s (not a procedure)"
                     s v))
               (let ([p (make-traced-procedure s v)])
                 (set-car! a v)
                 (set-cdr! a p)
                 (set-top-level-value! s p)))))]
        [else
         (unless (top-level-bound? s)
           (error 'trace "~s is unbound" s))
         (let ([v (top-level-value s)])
           (unless (procedure? v)
             (error 'trace "the top-level value of ~s is ~s (not a procedure)"
                    s v))
           (let ([p (make-traced-procedure s v)])
             (set! traced-symbols 
               (cons (cons s (cons v p)) traced-symbols))
             (set-top-level-value! s p)))])))
  
  (define untrace-symbol!
    (lambda (s)
      (define loop
        (lambda (ls)
          (cond
            [(null? ls) '()]
            [(eq? s (caar ls))
             (let ([a (cdar ls)])
               (when (eq? (cdr a) (top-level-value s))
                 (set-top-level-value! s (car a)))
               (cdr ls))]
            [else (cons (car ls) (loop (cdr ls)))])))
      (set! traced-symbols (loop traced-symbols))))

  ($pcb-set! make-traced-procedure make-traced-procedure)
  ($pcb-set! trace-symbol! trace-symbol!)
  ($pcb-set! untrace-symbol! untrace-symbol!))


