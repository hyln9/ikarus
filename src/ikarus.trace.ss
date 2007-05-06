
(library (ikarus trace)
  (export make-traced-procedure trace-symbol! untrace-symbol!)
  (import
    (only (scheme) top-level-bound? set-top-level-value!
          top-level-value)
    (ikarus))

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
                   (call-with-values
                     (lambda ()
                       (call/cf
                         (lambda (nf)
                           (set! f nf)
                           (set-car! k* nf)
                           (apply proc args))))
                     (lambda v*
                       (display-prefix k* #t)
                       (unless (null? v*)
                         (write (car v*))
                         (let f ([v* (cdr v*)])
                           (cond
                             [(null? v*) (newline)]
                             [else
                              (write-char #\space)
                              (write (car v*))
                              (f (cdr v*))])))
                       (apply values v*))))
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

  )


#!eof


Try:

(trace-define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (sub1 n))))))
(fact 5)

(trace-define (fact n m)
  (cond
    [(zero? n) m]
    [else (fact (sub1 n) (* n m))]))
(fact 5 1)


(trace-define (fact n m k)
  (cond
    [(zero? n) (k m)]
    [else (begin (fact (sub1 n) (* n m) k) 0)]))
(call/cc
  (lambda (k)
    (fact 6 1
      (trace-lambda escape (v) (k v)))))

(trace-define (infinite-loop n)
  (infinite-loop (add1 n)))
(infinite-loop 0)

