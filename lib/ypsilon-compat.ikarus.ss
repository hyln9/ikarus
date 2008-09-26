
(library (ypsilon-compat)
  (export on-windows on-darwin on-linux on-freebsd on-posix
          load-shared-object c-argument c-function 
          microsecond usleep library-pointer
          (rename (ypsilon:format format)))
  (import 
    (ikarus system $foreign)
    (except (ikarus) library))

  (define (microsecond)
    (let ([t (current-time)])
      (+ (* (time-second t) 1000000)
         (div (time-nanosecond t) 1000))))

  (define (usleep . args) (error '#f "invalid args" args))

  (define (ypsilon:format what str . args)
    (cond
      [(eq? what #t) 
       (apply printf str args)]
      [(eq? what #f) 
       (apply format str args)]
      [else (error 'ypsion:format "invalid what" what)]))


  (define (architecture-feature what)
    (case what
      [(operating-system) (host-info)]
      [else (error 'architecture-feature "invalid args" what)]))

  (define (string-contains text s)
    (define (starts-at? i)
      (let f ([i i] [j 0])
        (cond
          [(= j (string-length s)) #t]
          [(= i (string-length text)) #f]
          [else
           (and (char=? (string-ref text i) (string-ref s j))
                (f (+ i 1) (+ j 1)))])))
    (let f ([i 0])
      (cond
        [(= i (string-length text)) #f]
        [(starts-at? i) #t]
        [else (f (+ i 1))])))

  (define on-windows (and (string-contains (architecture-feature 'operating-system) "windows") #t))
  (define on-darwin  (and (string-contains (architecture-feature 'operating-system) "darwin")  #t))
  (define on-linux   (and (string-contains (architecture-feature 'operating-system) "linux")   #t))
  (define on-freebsd (and (string-contains (architecture-feature 'operating-system) "freebsd") #t))
  (define on-posix   (not on-windows))

  
  (define-record-type library (fields name pointer))

  (define (load-shared-object libname) 
    (make-library libname
      (or (dlopen libname)
          (error 'load-shared-object (dlerror) libname))))
  
  (define (int? x) (or (fixnum? x) (bignum? x)))

  (define (check-int who x)
    (cond
      [(int? x) x]
      [else (die who "not an int" x)]))
  
  (define (vector-andmap f v)
    (andmap f (vector->list v)))

  (define (check-int* who x)
    (cond
      [(and (vector? x) (vector-andmap int? x))
       (let ([n (vector-length x)])
         (let ([p (malloc (* n 4))])
           (let f ([i 0])
             (cond
               [(= i n) p]
               [else
                (pointer-set-int p (* i 4) (vector-ref x i))
                (f (+ i 1))]))))]
      [else (die who "not an int*" x)]))

  (define (check-char* who x)
    (cond
      [(string? x)
       (check-byte* who (string->utf8 x))]
      [else (die who "not a char*" x)]))
      
  (define pointer-size
    (cond
      [(<= (fixnum-width) 32) 4]
      [else                   8]))

  (define (check-char** who x)
    (cond
      [(and (vector? x) (vector-andmap string? x))
       (let ([n (vector-length x)])
         (let ([p (malloc (* n pointer-size))])
           (let f ([i 0])
             (cond
               [(= i n) p]
               [else
                (pointer-set-int p (* i pointer-size)
                  (pointer->integer (check-char* who (vector-ref x i))))
                (f (+ i 1))]))))]
      [else (die who "not a char**" x)]))

  (define (check-byte* who x)
    (cond
      [(bytevector? x)
       (let ([n (bytevector-length x)])
         (let ([p (malloc (+ n 1))])
           (pointer-set-char p n 0)
           (let f ([i 0])
             (cond
               [(= i n) p]
               [else
                (pointer-set-char p i (bytevector-u8-ref x i))
                (f (+ i 1))]))))]
      [else (die who "not a byte*" x)]))
  
  (define (check-float who x)
    (cond
      [(flonum? x) x]
      [else (die who "not a flonum" x)]))

  (define (check-double who x)
    (cond
      [(flonum? x) x]
      [else (die who "not a double" x)]))

  (define-syntax check-callback
    (lambda (x)
      (syntax-case x ()
        [(_ foreign-name val return-type (arg-type* ...))
         #'(let ([t val])
             (if (procedure? t)
                 ((make-callback
                    (convert-type return-type)
                    (list (convert-type arg-type*) ...))
                  t)
                 (error 'foreign-name "not a procedure" t)))])))

  (define-syntax todo
    (syntax-rules ()
      [(_ name* ...)
       (begin
         (define (name* . args) (error 'name* "not implemented"))
         ...)]))

  (define (check-void* who x)
    (cond
      [(pointer? x) x]
      [else (die who "not a void*" x)]))

  (define-syntax convert-arg
    (lambda (x)
      (syntax-case x (int char* byte* c-callback float double void*)
        [(_ form foreign-name val char*)
         #'(check-char* 'foreign-name val)]
        [(_ form foreign-name val byte*)
         #'(check-byte* 'foreign-name val)]
        [(_ form foreign-name val void*)
         #'(check-void* 'foreign-name val)]
        [(_ form foreign-name val int)
         #'(check-int 'foreign-name val)]
        [(_ form foreign-name val float)
         #'(check-float 'foreign-name val)]
        [(_ form foreign-name val double)
         #'(check-double 'foreign-name val)]
        [(_ form foreign-name val [int])
         #'(check-int* 'foreign-name val)]
        [(_ form foreign-name val [char*])
         #'(check-char** 'foreign-name val)]
        [(_ form foreign-name val [c-callback return-type (arg-types ...)])
         #'(check-callback foreign-name val return-type (arg-types ...))]
        [(_ form foreign-name val arg-type)
         (syntax-violation 'c-function "invalid argument type"
                           #'form #'arg-type)])))


  (define (char*->string who x)
    (define (strlen x)
      (let f ([i 0])
        (cond
          [(= 0 (pointer-ref-uchar x i)) i]
          [else (f (+ i 1))])))
    (let ([n (strlen x)])
      (let ([s (make-string n)])
        (let f ([i 0])
          (if (= i n) 
              s
              (begin
                (string-set! s i (integer->char (pointer-ref-uchar x i)))
                (f (+ i 1))))))))

  (define-syntax convert-return
    (lambda (x)
      (syntax-case x (char*)
        [(_ form foreign-name val char*)
         #'(char*->string 'foreign-name val)]
        [(_ form foreign-name val other) 
         #'val])))


  (define-syntax convert-type
    (lambda (x)
      (define ls
        '([void    void]
          [char*   pointer]
          [float   float]
          [double  double]
          [void*   pointer]
          [byte*   pointer]
          [int     sint32]))
      (define (valid x)
        (cond
          [(and (list? x) (= (length x) 3) (eq? (car x) 'c-callback))
           (and (valid (cadr x))
                (andmap valid (caddr x))
                'pointer)]
          [(list? x)
           (and (andmap valid x) 'pointer)]
          [(assq x ls) => cadr]
          [else #f]))
      (syntax-case x (void)
        [(ctxt t)
         (cond
           [(valid (syntax->datum #'t)) => 
            (lambda (t)
              (with-syntax ([t (datum->syntax #'ctxt t)])
                #'(quote t)))]
           [else (syntax-violation #f "invalid type" #'t)])])))

  (define (lookup-shared-object lib name) 
    (define who 'lookup-shared-object)
    (unless (symbol? name) (die who "not a symbol" name))
    (unless (library? lib) (die who "not a library" lib))
    (or (dlsym (library-pointer lib) (symbol->string name))
      (error who 
        (format "cannot find object ~a in library ~a" 
                name (library-name lib)))))


  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
        [(_ lib lib-name return-type conv foreign-name (arg-type* ...))
         (with-syntax ([x x]
                       [(t* ...) (generate-temporaries #'(arg-type* ...))])
         #'(let ([callout 
                   ((make-ffi 
                      (convert-type return-type)
                      (list (convert-type arg-type*) ...))
                    (lookup-shared-object lib 'foreign-name))])
             (lambda (t* ...)
               (let ([t* (convert-arg x foreign-name t* arg-type*)] ...)
                 (let ([v (callout t* ...)])
                   (convert-return x foreign-name v return-type))))))])))

  (define-syntax c-argument
    (lambda (x)
      (syntax-case x ()
        [(_ function-name argnum argtype argval)
         (begin
           ;(printf "syntax ~s\n" (syntax->datum x))
           #'(void))])))

)
