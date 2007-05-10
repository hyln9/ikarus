


(library (ikarus library-manager)
  (export imported-label->binding library-subst
          installed-libraries visit-library
          find-library-by-name install-library
          library-spec invoke-library 
          extend-library-subst! extend-library-env!)
  (import (except (ikarus) installed-libraries))

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
    ;;; this works now because make-collection is a lambda
    ;;; binding and this turns into a complex binding as far
    ;;; as letrec is concerned.  It will be more ok once we do
    ;;; letrec*.
    (make-parameter (make-collection)
      (lambda (x)
        (unless (procedure? x)
          (error 'current-library-collection "~s is not a procedure" x))
        x)))

  (define-record library 
    (id name ver imp* vis* inv* subst env visit-state invoke-state
        visible?))

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

  (define (install-library-by-name name)
    #f)

  (define (find-library-by-name name)
    (find-library-by
       (lambda (x) (equal? (library-name x) name))))

  (define (library-exists? name)
    (and (find-library-by
           (lambda (x) (equal? (library-name x) name)))
         #t))

  (define (find-library-by-spec/die spec)
    (let ([id (car spec)])
      (or (find-library-by
            (lambda (x) (eq? id (library-id x))))
          (error #f "cannot find library with spec ~s" spec))))

  (define label->binding-table (make-hash-table))

  (define (install-library id name ver imp* vis* inv* 
            exp-subst exp-env visit-code invoke-code visible?)
    (let ([imp-lib* (map find-library-by-spec/die imp*)]
          [vis-lib* (map find-library-by-spec/die vis*)]
          [inv-lib* (map find-library-by-spec/die inv*)])
      (unless (and (symbol? id) (list? name) (list? ver))
        (error 'install-library "invalid spec ~s ~s ~s" id name ver))
      (when (library-exists? name)
        (error 'install-library "~s is already installed" name))
      (let ([lib (make-library id name ver imp-lib* vis-lib* inv-lib* 
                    exp-subst exp-env visit-code invoke-code 
                    visible?)])
        (for-each 
          (lambda (x) 
            (let ([label (car x)] [binding (cdr x)])
              (let ([binding 
                     (case (car binding)
                       [(global) 
                        (cons 'global (cons lib (cdr binding)))]
                       [(global-macro)
                        (cons 'global-macro (cons lib (cdr binding)))]
                       [else binding])])
                (put-hash-table! label->binding-table label binding))))
          exp-env)
        ((current-library-collection) lib))))

  (define extend-library-subst!
    (lambda (lib sym label)
      (set-library-subst! lib 
        (cons (cons sym label) (library-subst lib)))))

  (define extend-library-env!
    (lambda (lib label binding)
      (set-library-env! lib
        (cons (cons label binding) (library-env lib)))
      (put-hash-table! label->binding-table label binding)))

  (define (imported-label->binding lab)
    (get-hash-table label->binding-table lab #f))

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


  (define (visit-library lib)
    (let ([visit (library-visit-state lib)])
      (when (procedure? visit)
        (set-library-visit-state! lib 
          (lambda () (error 'visit "circularity detected for ~s" lib)))
        (for-each invoke-library (library-vis* lib))
        (set-library-visit-state! lib 
          (lambda () (error 'invoke "first visit did not return for ~s" lib)))
        (visit)
        (set-library-visit-state! lib #t))))


  (define (invoke-library-by-spec spec)
    (invoke-library (find-library-by-spec/die spec)))

  (define installed-libraries 
    (case-lambda
      [(all?)
       (let f ([ls ((current-library-collection))])
         (cond
           [(null? ls) '()]
           [(or all? (library-visible? (car ls)))
            (cons (car ls) (f (cdr ls)))]
           [else (f (cdr ls))]))]
      [() (installed-libraries #f)]))

  (define library-spec       
    (lambda (x) 
      (unless (library? x)
        (error 'library-spec "~s is not a library" x))
      (list (library-id x) (library-name x) (library-ver x))))

  ;;; init
  (set-rtd-printer! (type-descriptor library)
    (lambda (x p)
      (unless (library? x)
        (error 'record-type-printer "not a library"))
      (display 
        (format "#<library ~s>" (append (library-name x) (library-ver x)))
        p)))

  )

