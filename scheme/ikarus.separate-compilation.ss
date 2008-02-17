
(library (ikarus separate-compilation)
  (export compile-library-to-port)
  (import 
    (except (ikarus) compile-library-to-port)
    (only (ikarus.compiler) compile-core-expr-to-port)
    (only (psyntax library-manager) current-library-expander))

  (define (compile-library-to-port x p)
    (let-values (((id name ver imp* vis* inv* 
                   invoke-code visit-code export-subst export-env)
                  ((current-library-expander) x)))
      (printf "id=~s name=~s ver=~s imp*=~s vis*=~s inv*=~s\n"
              id name ver imp* vis* inv*)
      (fasl-write (list id name ver imp* vis* inv* export-subst export-env) p)
      (compile-core-expr-to-port visit-code p)
      (compile-core-expr-to-port invoke-code p))))

