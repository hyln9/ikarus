(define primitive-set! set-top-level-value!)
(define chez-expand sc-expand)
(define-syntax |#primitive|
  (lambda (x)
    (syntax-case x ()
      [(_ n) #'n])))

(printf "loading psyntax.pp ...\n")
(load "psyntax-7.1.pp")

(current-expand 
  (lambda (x . args)
    (apply chez-expand (sc-expand x) args)))

(printf "loading psyntax.ss ...\n")
(load "psyntax-7.1.ss")
(current-expand
  (lambda (x . args)
    (apply chez-expand (sc-expand x) args)))

(printf "making xpsyntax.pp ...\n")

(with-output-to-file "xpsyntax.pp"
  (lambda ()
    (load "psyntax-7.1.ss"
      (lambda (x)
        (parameterize ([print-gensym #f]
                       [print-graph #f]
                       [expand-mode 'bootstrap]
                       [print-vector-length #f])
          (pretty-print (sc-expand x))
          (newline)))))
  'replace)

