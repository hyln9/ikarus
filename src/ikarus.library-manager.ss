
#| current-library-collection   procedure
  Calling (current-library-collection) returns a procedure that:
    - when called with no arguments, it returns a list of the set
      of
      libraries in the collection.
    - when called with a single argument, it adds that library to
      the set of libraries in the collection.
  Calling (current-library-collection f) sets the current library 
   collection to be the procedure f which must follow the protocol
   above.
|#



(library (ikarus library-manager)
  (export imported-label->binding library-subst/env
          current-library-collection installed-libraries
          find-library-by-name imported-loc->library install-library
          library-spec invoke-library)
  (import (except (ikarus) current-library-collection))

  (define (make-collection)
    (let ([set '()])
      (define (set-cons x ls)
        (cond
          [(memq x ls) ls]
          [else (cons x ls)]))
      (case-lambda
        [() set]
        [(x) (set! set (set-cons x set))])))

  (define current-library-collection
    (make-parameter (make-collection) 
      (lambda (x)
        (unless (procedure? x)
          (error 'current-library-collection 
            "~s is not a procedure" x))
        x)))

  (define-record library 
    (id name ver imp* vis* inv* subst env visit-state invoke-state))

  (define (find-dependencies ls)
    (cond
      [(null? ls) '()]
      [else (error 'find-dependencies "cannot handle deps yet")]))

  (define (find-library-by pred)
    (let f ([ls ((current-library-collection))])
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

  (define (find-library-by-spec/die spec)
    (let ([id (car spec)])
      (or (find-library-by
            (lambda (x) (eq? id (library-id x))))
          (error #f "cannot find library with spec ~s" spec))))

  (define label->binding-table (make-hash-table))

  (define (install-library id name ver
             imp* vis* inv* exp-subst exp-env visit-code invoke-code)
    (let ([imp-lib* (map find-library-by-spec/die imp*)]
          [vis-lib* (map find-library-by-spec/die vis*)]
          [inv-lib* (map find-library-by-spec/die inv*)])
      (unless (and (symbol? id) (list? name) (list? ver))
        (error 'install-library "invalid spec ~s ~s ~s" id name ver))
      (when (find-library-by-name name)
        (error 'install-library "~s is already installed" name))
      (let ([lib (make-library id name ver imp-lib* vis-lib* inv-lib* 
                    exp-subst exp-env visit-code invoke-code)])
        (for-each 
          (lambda (x) 
            (put-hash-table! label->binding-table (car x) (cdr x)))
          exp-env)
        ((current-library-collection) lib))))

  (define (imported-label->binding lab)
    (get-hash-table label->binding-table lab #f))

  (define (imported-loc->library loc)
    (define (loc-in-env? ls)
      (and (pair? ls)
           (let ([a (car ls)])
             (let ([binding (cdr a)])
               (or (and (eq? (car binding) 'global)
                        (eq? (cdr binding) loc))
                   (loc-in-env? (cdr ls)))))))
    (let f ([ls ((current-library-collection))])
      (cond
        [(null? ls) #f]
        [(loc-in-env? (library-env (car ls))) (car ls)]
        [else (f (cdr ls))])))

  (define (invoke-library lib)
    (let ([invoke (library-invoke-state lib)])
      (when (procedure? invoke)
        (set-library-invoke-state! lib 
          (lambda () (error 'invoke "circularity detected for ~s" lib)))
        (for-each invoke-library (library-inv* lib))
        (set-library-invoke-state! lib 
          (lambda () (error 'invoke "first invoke did not return for ~s" lib)))
        (invoke)
        (set-library-invoke-state! lib #t))))

  (define (invoke-library-by-spec spec)
    (invoke-library (find-library-by-spec/die spec)))


  (define installed-libraries 
    (lambda () ((current-library-collection))))
  (define library-subst/env
    (lambda (x) 
      (unless (library? x)
        (error 'library-subst/env "~s is not a library" x))
      (values (library-subst x) (library-env x))))
  (define library-spec       
    (lambda (x) 
      (unless (library? x)
        (error 'library-spec "~s is not a library" x))
      (list (library-id x) (library-name x) (library-ver x))))

  ;;; init

  ((record-field-mutator (record-type-descriptor (type-descriptor library)) 'printer)
    (type-descriptor library)
    (lambda (x p)
      (unless (library? x)
        (error 'record-type-printer "not a library"))
      (display 
        (format "#<library ~s>" (append (library-name x) (library-ver x)))
        p)))

  )

