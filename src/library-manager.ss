
(library (ikarus library-manager)
  (export)
  (import (scheme))

  (define-record library 
    (id name ver imp* vis* inv* exp-subst exp-env visit-state invoke-state))

  (define (find-dependencies ls)
    (cond
      [(null? ls) '()]
      [else (error 'find-dependencies "cannot handle deps yet")]))

  (define *all-libraries* '())

  (define (find-library-by pred)
    (let f ([ls *all-libraries*])
      (cond
        [(null? ls) #f]
        [(pred (car ls)) (car ls)]
        [else (f (cdr ls))])))

  (define (find-library-by-name name)
    (find-library-by
      (lambda (x) (equal? (library-name x) name))))

  (define (find-library-by-name/die name)
    (or (find-library-by-name name)
        (error #f "cannot find library ~s" name)))

  (define (lm:install-library id name ver
             imp* vis* inv* exp-subst exp-env visit-code invoke-code)
    (let ([imp-lib* (map find-library-by-name/die imp*)]
          [vis-lib* (map find-library-by-name/die vis*)]
          [inv-lib* (map find-library-by-name/die inv*)])
      (unless (and (symbol? id) (list? name) (list? ver))
        (error 'install-library "invalid spec ~s ~s ~s" id name ver))
      (when (find-library-by-name name)
        (error 'install-library "~s is already installed" name))
      (let ([lib (make-library id name ver imp-lib* vis-lib* inv-lib* 
                    exp-subst exp-env visit-code invoke-code)])
        (set! *all-libraries* (cons lib *all-libraries*)))))
  (lm:install-library (gensym "null") ;;; id
                      '(null)     '() ;;; name/version
                      '() ;;; import libs 
                      '() ;;; visit libs
                      '() ;;; invoke libs
                      '() ;;; subst
                      '() ;;; env
                      void void)
  ;(printf "ALL LIBRARIES:\n~s\n" *all-libraries*)
  (primitive-set! 'install-library lm:install-library)
  )
