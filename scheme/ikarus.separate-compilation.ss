
(library (ikarus separate-compilation)
  (export compile-library-to-port install-library-from-file)
  (import 
    (except (ikarus) library 
      compile-library-to-port
      install-library-from-file)
    (only (ikarus.compiler) compile-core-expr)
    (only (psyntax library-manager)
      install-library current-library-expander))

  (define-struct library (id name ver imp* vis* inv* 
    export-subst export-env visit-code invoke-code visible?))

  (define (install-library-from-file filename)
    (let ([p (open-file-input-port filename)])
      (let ([L (fasl-read p)])
        (unless (library? L) 
          (error 'install-library "file does not contain a library"
                 filename))
        (printf "L=~s\n" L)
        (install-library (library-id L) (library-name L) 
          (library-ver L) (library-imp* L) (library-vis* L)
          (library-inv* L) (library-export-subst L)
          (library-export-env L) (library-visit-code L)
          (library-invoke-code L) (library-visible? L)))))

  (define (compile-library-to-port x p)
    (let-values (((id name ver imp* vis* inv* 
                   invoke-code visit-code export-subst export-env)
                  ((current-library-expander) x)))
      (let ([L (make-library id name ver imp* vis* inv*
                  export-subst export-env 
                  (compile-core-expr visit-code)
                  (compile-core-expr invoke-code)
                  #t)])
        (printf "L=~s\n" L)
        (fasl-write L p)))))

