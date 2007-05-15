


(library (ikarus library-manager)
  (export imported-label->binding library-subst
          installed-libraries visit-library
          library-name
          find-library-by-name install-library
          library-spec invoke-library 
          extend-library-subst! extend-library-env!
          current-library-expander current-library-collection)
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

  (define library-path
    (make-parameter
      '(".")
      (lambda (x)
        (if (and (list? x) (andmap string? x))
            (map values x)
            (error 'library-path "~s is not a list of strings" x)))))

  (define (library-name->file-name x) 
    (with-output-to-string
      (lambda ()
        (define (display-hex n)
          (cond
            [(<= 0 n 9) (display n)]
            [else (display 
                    (integer->char 
                      (+ (char->integer #\A)
                         (- n 10))))]))
        (let f ([ls x])
          (cond
            [(null? ls) (display ".ss")]
            [else
             (display "/")
             (for-each
               (lambda (c)
                 (cond
                   [(or (char<=? #\a c #\z) 
                        (char<=? #\A c #\Z)
                        (char<=? #\0 c #\9) 
                        (memv c '(#\- #\. #\_ #\~)))
                    (display c)]
                   [else
                    (display "%")
                    (let ([n (char->integer c)])
                      (display-hex (quotient n 16))
                      (display-hex (remainder n 16)))]))
               (string->list 
                 (symbol->string (car ls))))
             (f (cdr ls))])))))
  (define file-locator
    (make-parameter
      (lambda (x)
        (let ([str (library-name->file-name x)])
          (let f ([ls (library-path)])
            (and (pair? ls)
                 (let ([name (string-append (car ls) str)])
                   (if (file-exists? name)
                       name
                       (f (cdr ls))))))))
      (lambda (f)
        (if (procedure? f)
            f
            (error 'file-locator 
               "~s is not a procedure" f)))))

  (define library-locator
    (make-parameter
      (lambda (x)
        (let ([file-name ((file-locator) x)])
          (and (string? file-name)
               (with-input-from-file file-name read))))
      (lambda (f)
        (if (procedure? f)
            f
            (error 'library-locator 
                   "~s is not a procedure" f)))))

  (define current-library-expander
    (make-parameter
      (lambda (x)
        (error 'library-expander "not initialized"))
      (lambda (f)
        (if (procedure? f)
            f
            (error 'library-expander 
                   "~s is not a procedure" f)))))

  (define external-pending-libraries 
    (make-parameter '()))

  (define (find-external-library name)
    (when (member name (external-pending-libraries))
      (error #f "circular attempt to import library ~s detected"
             name))
    (parameterize ([external-pending-libraries
                    (cons name (external-pending-libraries))])
      (let ([lib-expr ((library-locator) name)])
        (unless lib-expr 
          (error #f "cannot find library ~s" name))
        ((current-library-expander) lib-expr)
        (or (find-library-by
              (lambda (x) (equal? (library-name x) name)))
            (error #f "handling external library of ~s did not yield the currect library" name)))))
          
  (define (find-library-by-name name)
    (or (find-library-by
          (lambda (x) (equal? (library-name x) name)))
        (find-external-library name)))

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

  (define (install-library-record lib)
    (let ([exp-env (library-env lib)])
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
        exp-env))
    ((current-library-collection) lib))

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
        (install-library-record lib))))

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

