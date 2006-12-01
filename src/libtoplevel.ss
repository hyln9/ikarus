
(for-each
  (lambda (x)
    ($set-symbol-value! x (primitive-ref x)))
  (public-primitives))

(let ()
  (define add-prim 
    (lambda (x)
      (let ([g (gensym (symbol->string x))])
        (putprop x '|#system| g)
        (putprop g '*sc-expander* (cons 'core-primitive x)))))
  (for-each add-prim (public-primitives))
  (for-each add-prim (system-primitives)))

(for-each
  (lambda (x)
    (cond
      [(getprop x '*sc-expander*) =>
       (lambda (p)
         (let ([g (gensym (symbol->string x))])
           (putprop x '|#system| g)
           (putprop g '*sc-expander* p)))]
      [(getprop x '|#system|) =>
       (lambda (g)
         (let ([p (getprop g '*sc-expander*)])
           (putprop x '*sc-expander* p)))]
      [else (error #f "~s is not a macro" x)]))
  (macros))

(let ([gsys (gensym "#system")] [gsch (gensym "*scheme*")])
  (define (make-stx x)
    (vector 'syntax-object x 
            (list '(top) 
                  (vector 'ribcage 
                          (vector x)
                          (vector '(top))
                          (vector (getprop x '|#system|))))))
  (define (make-module stx* name)
    `($module . #(interface (top) ,(list->vector stx*) ,name)))
  (putprop '|#system| '|#system| gsys)
  (putprop 'scheme  '|#system| gsch)
  (putprop 'scheme '*scheme* gsch)
  (let* ([schls (append '(scheme) (public-primitives) (macros))]
         [sysls (append '(|#system|) (system-primitives) schls)])
    (let ([sysmod (make-module (map make-stx sysls) '|#system|)]
          [schmod (make-module (map make-stx schls) '*scheme*)])
      (for-each 
        (lambda (x)
          (putprop x '*scheme* (getprop x '|#system|)))
        schls)
      (putprop gsch '*sc-expander* schmod)
      (putprop gsys '*sc-expander* sysmod)
      (putprop '|#system| '*sc-expander* sysmod)
      (putprop 'scheme '*sc-expander* schmod))))

(let-values ([(files args)
              (let f ([args (command-line-arguments)])
                (cond
                  [(null? args) (values '() '())]
                  [(string=? (car args) "--")
                   (values '() (cdr args))]
                  [else
                   (let-values ([(f* a*) (f (cdr args))])
                     (values (cons (car args) f*) a*))]))])
  (current-eval compile)
  (command-line-arguments args)
  (printf "Petite Ikarus Scheme (Build ~a)\n" (compile-time-date-string))
  (display "Copyright (c) 2006 Abdulaziz Ghuloum\n\n")
  (for-each load files)
  (new-cafe))


