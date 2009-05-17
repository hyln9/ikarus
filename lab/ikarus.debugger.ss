
(import (ikarus))

(define (with-output-to-string/limit x len)
  (define n 0)
  (define str (make-string len))
  (call/cc
    (lambda (k)
      (define p
        (make-custom-textual-output-port 
          "*limited-port*"
          (lambda (buf i count)
            (let f ([i i] [count count])
              (unless (zero? count)
                (if (= n len)
                    (k str)
                    (begin
                      (string-set! str n (string-ref buf i))
                      (set! n (+ n 1))
                      (f (+ i 1) (- count 1))))))
            count)
          #f #f #f))
      (parameterize ([print-graph #f])
        (write x p)
        (flush-output-port p))
      (substring str 0 n))))



(define (make-annotated-procedure ann proc)
  (import (ikarus system $codes))
  ($make-annotated-procedure ann proc))

(define-struct trace (src expr rator rands))

(module (get-traces debug-call)

  (define outer-ring-size 30)
  (define inner-ring-size 10)

  (define end-marker -1)

  (define-struct icell (prev next num content))
  (define-struct ocell (prev next num cf icell))

  (define (make-ring n cell-prev cell-next cell-prev-set! cell-next-set! make-cell)
    (let ([ring (make-cell)])
      (cell-prev-set! ring ring)
      (cell-next-set! ring ring)
      (do ((n n (- n 1)))
        ((<= n 1))
        (let ([cell (make-cell)]
              [next (cell-next ring)])
          (cell-prev-set! cell ring)
          (cell-next-set! cell next)
          (cell-prev-set! next cell)
          (cell-next-set! ring cell)))
      ring))
  
  (define (make-double-ring n m)
    (make-ring n
      ocell-prev ocell-next set-ocell-prev! set-ocell-next!
      (lambda ()
        (make-ocell #f #f end-marker #f 
          (make-ring m 
            icell-prev icell-next set-icell-prev! set-icell-next!
            (lambda () (make-icell #f #f end-marker (lambda () #f))))))))

  (define (ring->list x cell-num cell-prev cell-content)
    (let f ([x x] [orig #f])
      (if (or (eq? x orig) (eqv? (cell-num x) end-marker))
          '()
           (cons (cons (cell-num x) (cell-content x))
             (f (cell-prev x) (or orig x))))))
   
  (define (get-traces)
    (ring->list step-ring ocell-num ocell-prev
      (lambda (x)
        (ring->list (ocell-icell x) icell-num icell-prev icell-content))))

  (define step-ring 
    (make-double-ring outer-ring-size inner-ring-size))

  (define (debug-call src expr rator . rands)
    (call/cf
      (lambda (cf)
        (if (eq? cf (ocell-cf step-ring))
            (reduce src expr rator rands)
            (let ([cf #f] [pcf #f])
              (dynamic-wind
                (lambda () 
                  (let ([prev step-ring])
                    (let ([next (ocell-next prev)])
                      (set! pcf (ocell-cf prev))
                      (set-ocell-num! next (+ (ocell-num prev) 1))
                      (set-icell-num! (ocell-icell next) end-marker)
                      (set! step-ring next)
                      (set-ocell-cf! step-ring cf))))
                (lambda ()
                  (call/cf
                    (lambda (cf2)
                      (set! cf cf2)
                      (set-ocell-cf! step-ring cf)
                      (reduce src expr rator rands))))
                (lambda ()
                  (let ([next step-ring])
                    (let ([prev (ocell-prev next)])
                      (set-ocell-num! prev (- (ocell-num next) 1))
                      (set-ocell-num! next end-marker)
                      (set-icell-num! (ocell-icell next) end-marker)
                      (set-ocell-cf! prev pcf)
                      (set! step-ring prev))))))))))

  (define (reduce src expr rator rands)
    (define (mark-reduction! x)
      (let ([prev (ocell-icell step-ring)])
        (let ([next (icell-next prev)])
          (set-icell-content! next x)
          (set-icell-num! next (+ (icell-num prev) 1))
          (set-ocell-icell! step-ring next))))
    (mark-reduction! (make-trace src expr rator rands))
    (apply rator rands))

)


(define (operator? x)
 (and (pair? x)
      (eq? (car x) 'primitive)
      (guard (con [(assertion-violation? con) #t])
        (system-value (cadr x))
        #f)))


(define (get-src/expr ae)
  (if (annotation? ae)
      (values (annotation-source ae) (annotation-stripped ae))
      (values #f (syntax->datum ae))))


(define (add-debug-calls expr)
  (define who 'add-debug-calls)
  (define (direct-call? op rands)
    (define n (length rands))
    (define (test cls*)
      (and (pair? cls*)
           (or
             (let ([fmls (caar cls*)])
               (and (list? fmls) (= (length fmls) n)))
             (test (cdr cls*)))))
    (and (pair? op)
         (case (car op)
           [(lambda) (test (list (cdr op)))]
           [(case-lambda) (test (cdr op))]
           [(annotated-case-lambda) (test (cddr op))]
           [else #f])))
  (define (E-call src expr op rands)
    (cond
      [(or (operator? op) (direct-call? op rands))
       `(,op ,@(map E rands))]
      [else
        `(',debug-call ',src ',expr ,(E op) ,@(map E rands))]))
  (define (E expr)
    (cond
      [(symbol? expr) expr]
      [(and (pair? expr) (list? expr))
       (let ([a (car expr)] [d (cdr expr)])
         (case a
           [(quote) expr]
           [(primitive) expr]
           [(set!) `(set! ,(car d) ,(E (cadr d)))]
           [(if) `(if ,(E (car d)) ,(E (cadr d)) ,(E (caddr d)))]
           [(begin) (cons 'begin (map E d))]
           [(lambda) (list 'lambda (car d) (E (cadr d)))]
           [(case-lambda)
            (cons 'case-lambda
              (map (lambda (x) (list (car x) (E (cadr x)))) d))]
           [(annotated-case-lambda)
            (cons* 'annotated-case-lambda (car d)
              (map (lambda (x) (list (car x) (E (cadr x)))) (cdr d)))]
           [(letrec letrec*)
            (list a 
              (map (lambda (x) (list (car x) (E (cadr x)))) (car d))
              (E (cadr d)))]
           [(foreign-call) 
            (cons* 'foreign-call (car d) (map E (cdr d)))]
           [(library-letrec*)
            (list a 
              (map (lambda (x) (list (car x) (cadr x) (E (caddr x)))) (car d))
              (E (cadr d)))]
           [(annotated-call)
            (let-values ([(src expr) (get-src/expr (car d))])
              (E-call src expr (cadr d) (cddr d)))]
           [else (E-call #f #f a d)]))]
      [else
       (die who "invalid expression" expr)]))
  (E expr))


(define (print-trace x)
  (define (chop x)
    (if (> (string-length x) 60)
        (format "~a#..." (substring x 0 56))
        x))
  (let ([n (car x)] [x (cdr x)])
    (printf " [~a] ~s\n" n (trace-expr x))
    (let ([src (trace-src x)])
      (when (pair? src)
        (printf "     source: char ~a of ~a\n" (cdr src) (car src))))
    (printf "     operator: ~s\n" (trace-rator x))
    (printf "     operands: ")
    (let ([ls (map (lambda (x) 
                     (with-output-to-string/limit x 80))
                   (trace-rands x))])
      (if (< (apply + 1 (length ls) (map string-length ls)) 60)
          (write (trace-rands x))
          (begin
            (display "(")
            (let f ([a (car ls)] [ls (cdr ls)])
              (display (chop a))
              (if (null? ls)
                  (display ")")
                  (begin 
                    (display "\n                ") 
                    (f (car ls) (cdr ls))))))))
    (newline)))

(define (print-step x)
  (let ([n (car x)] [ls (cdr x)])
    (unless (null? ls)
      (printf "FRAME ~s:\n" n)
      (for-each print-trace (reverse ls)))))

(define (print-all-traces)
  (let ([ls (reverse (get-traces))])
    (printf "CALL FRAMES:\n")
    (for-each print-step ls)))

(define (guarded-start proc)
  (with-exception-handler
    (lambda (con)
      (define (help)
        (printf "Condition trapped by debugger.\n")
        (print-condition con)
        (printf "~a\n" 
          (string-append 
            "[t] Trace. "
            "[r] Reraise condition. "
            "[c] Continue "
            "[q] Quit "
            "[?] Help. ")))
      (help)
      ((call/cc
         (lambda (k)
           (new-cafe
             (lambda (x)
               (case x
                [(R r) (k (lambda () (raise-continuable con)))]
                [(Q q) (exit 0)]
                [(T t) (print-all-traces)]
                [(C c) (k void)]
                [(?)   (help)]
                [else (printf "invalid option\n")])))
           void))))
    proc))

(define (start-repl)
  (display "Ikarus Interpreter\n\n")
  (new-cafe
    (lambda (x)
      (guarded-start
        (lambda ()
          (eval x (interaction-environment)))))))

(define (start-script script-name args)
  (command-line-arguments (cons script-name args))
  (guarded-start
    (lambda ()
      (load-r6rs-script script-name #f #t))))
 
(current-core-eval
  (let ([ev (current-core-eval)])
    (lambda (x)
      (ev (add-debug-calls x)))))

(apply
  (case-lambda
    [(interpreter flag script-name . rest) 
     (if (string=? flag "--r6rs-script") 
         (start-script script-name rest)
         (error interpreter "invalid args" (cons* flag script-name rest)))]
    [(interpreter) (start-repl)]
    [(interpreter . rest)
     (error interpreter "invalid args" rest)])
  (command-line-arguments))


 


#!eof

(print-graph #t)

;(write (make-double-rib 5 5))
(write (make-ring 10 (lambda () #f)))
(newline)
