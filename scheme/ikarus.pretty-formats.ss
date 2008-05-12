(library (ikarus.pretty-formats)
  (export get-fmt pretty-format)
  (import (except (ikarus) pretty-format))

  (define *pretty-format* '*pretty-format*)
  (define (get-fmt name)
    (getprop name *pretty-format*))
  (define (set-fmt! name fmt)
    (putprop name *pretty-format* fmt))


  (define pretty-format
    (lambda (x)
      (unless (symbol? x)
        (die 'pretty-format "not a symbol" x))
      (case-lambda
        [() (getprop x *pretty-format*)]
        [(v) (putprop x *pretty-format* v)])))

  ;;; standard formats
  (set-fmt! 'quote '(read-macro . "'"))
  (set-fmt! 'unquote '(read-macro . ","))
  (set-fmt! 'unquote-splicing '(read-macro . ",@"))
  (set-fmt! 'quasiquote '(read-macro . "`"))
  (set-fmt! 'syntax '(read-macro . "#'"))
  (set-fmt! 'quasisyntax '(read-macro . "#`"))
  (set-fmt! 'unsyntax '(read-macro . "#,"))
  (set-fmt! 'unsyntax-splicing '(read-macro . "#,@"))
  ;(set-fmt! '|#primitive| '(read-macro . "#%"))
  (set-fmt! 'let '(alt 
                    (_ (0 [e 0 e] ...) tab e ...)
                    (_ x (0 [e 0 e] ...) tab e ...)))
  (set-fmt! 'letrec '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'letrec* '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let-syntax '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'letrec-syntax '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let* '(_ (0 [e 0 e] ...) tab e ...))
  (set-fmt! 'let-values '(_ (0 [e 0 e] ...) tab e tab e* ...))
  (set-fmt! 'cond '(_ tab [0 e ...] ...))
  (set-fmt! 'define '(_ name tab e ...))
  (set-fmt! 'case-lambda 
     '(_ tab [0 e ...] ...))
  (set-fmt! 'struct-case 
     '(_ e tab [e 0 e ...] ...))
  (set-fmt! 'if '(_ test 3 e ...))
  (set-fmt! 'and '(and test 4 e ...))
  (set-fmt! 'or '(or test 3 e ...))
  (set-fmt! 'begin '(_ tab e ...))
  (set-fmt! 'lambda '(_ fmls tab e tab e* ...))
  (set-fmt! 'case '(_ e tab [e 0 e] ...))
  (set-fmt! 'syntax-rules '(_ kwd* tab [e 0 e] ...))
  (set-fmt! 'syntax-case '(_ expr kwd*  
                             tab (e 0 e 0 e ...) ...))
  (set-fmt! 'module '(alt (_ (fill ...) tab e ...)
                          (_ name (fill ...) tab e ...)))
  (set-fmt! 'library '(_ name tab e ...))
  (set-fmt! 'import '(_ tab e ...))

)
