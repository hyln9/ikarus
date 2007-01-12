
;;; this file is one big hack that initializes the whole system.


;;; first, it defines all public primitives to their primref values.
;;;       (cross your fingers they're all defined in code)
(for-each
  (lambda (x)
    ($set-symbol-value! x (primitive-ref x)))
  (public-primitives))

;;; second, it hacks a |#system| module by defining all system and
;;; public primitives to be (core-primitive . name) syntaxes.
(let ()
  (define add-prim 
    (lambda (x)
      (let ([g (gensym (symbol->string x))])
        (putprop x '|#system| g)
        (putprop g '*sc-expander* (cons 'core-primitive x)))))
  (for-each add-prim (public-primitives))
  (for-each add-prim (system-primitives)))

;;; third, all macros that are defined in the compiler |#system| are
;;;  added to the top-level, and those defined in the top-level are
;;;  added to the |#system|.
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

;;; Now we hack the read #system and scheme modules by forging
;;; interfaces and putting property lists.
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


;;; Finally, we're ready to evaluate the files and enter the cafe.
(let-values ([(files script args)
              (let f ([args (command-line-arguments)])
                (cond
                  [(null? args) (values '() #f '())]
                  [(string=? (car args) "--")
                   (values '() #f (cdr args))]
                  [(string=? (car args) "--script")
                   (let ([d (cdr args)])
                     (cond
                       [(null? d) 
                        (error #f "--script requires a script name")]
                       [else
                        (values '() (car d) (cdr d))]))]
                  [else
                   (let-values ([(f* script a*) (f (cdr args))])
                     (values (cons (car args) f*) script a*))]))])
  (current-eval compile)
  (cond
    [script ; no greeting, no cafe
     (command-line-arguments (cons script args))
     (for-each load files)
     (load script)
     (exit 0)]
    [else
     (printf "Ikarus Scheme (Build ~a)\n" (compile-time-date-string))
     (display "Copyright (c) 2006-2007 Abdulaziz Ghuloum\n\n")
     (command-line-arguments args)
     (for-each load files)
     (new-cafe)
     (exit 0)]))


