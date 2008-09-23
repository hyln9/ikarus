
(library (core)
  (export iota string-contains architecture-feature format
          flonum->float string->cstring load-shared-object
          lookup-shared-object
          stdcall-shared-object->void
          stdcall-shared-object->int
          stdcall-shared-object->intptr
          stdcall-shared-object->double
          make-callback
          usleep microsecond)
  (import (except (ikarus) library format)
          (except (ikarus system $foreign) make-callback))

  (define (format what str . args)
    (import (ikarus))
    (cond
      [(eq? what #f) 
       (apply printf str args)]
      [else 
       (apply format str args)]))

  (define (iota n i)
    (cond
      [(= n 0) '()]
      [else (cons i (iota (- n 1) (+ i 1)))]))

  (define (architecture-feature what)
    (case what
      [(operating-system) "darwin"]
      [(alignof:int)             4]
      [(sizeof:int)              4]
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

  (define-record-type library (fields name pointer))

  (define (load-shared-object libname) 
    (make-library libname
      (or (dlopen libname)
          (error 'load-shared-object (dlerror) libname))))
  
  (define-record-type shared-object 
    (fields name lib [mutable proc] pointer))
  
  (define (lookup-shared-object lib name) 
    (define who 'lookup-shared-object)
    (unless (symbol? name) (die who "not a symbol" name))
    (unless (library? lib) (die who "not a library" lib))
    (make-shared-object name lib #f
      (or (dlsym (library-pointer lib) (symbol->string name))
        (error who 
          (format #f "cannot find object ~a in library ~a" 
                  name (library-name lib))))))


  (define get-ffi
    (let ([ffis '()])
      (lambda (return-type arg-types)
        (let ([x (cons return-type arg-types)])
          (cond
            [(assoc x ffis) => cdr]
            [else
             (let ([ffi (make-ffi return-type arg-types)])
               (set! ffis (cons (cons x ffi) ffis))
               ffi)])))))

  (define (arg->arg-type who)
    (define (err who x)
      (error 'arg->arg-type 
             (format #f "cannot handle conversion for procedure ~a" who)
             x))
    (lambda (arg)
      (cond
        [(bytevector? arg) 'pointer]
        [(vector? arg)     'pointer]
        [(cstring? arg)    'pointer]
        [(pointer? arg)    'pointer]
        [(cfloat? arg)     'float]
        [(flonum? arg)     'double]
        [(or (fixnum? arg) (bignum? arg)) 'sint32]
        [else (err who arg)])))


  (define (convert-incoming type result)
    (define who 'convert-incoming)
    (case type
      [(void) (void)]
      [(sint32) result]
      [(pointer) result]
      [else (error who "unhandled type" type)]))

  (define (convert-outgoing type arg)
    (define who 'convert-outgoing)
    (case type
      [(pointer)
       (cond
         [(bytevector? arg) (bytevector->blob arg)]
         [(vector? arg) (vector->blob arg)]
         [(cstring? arg) (cstring-pointer arg)]
         [(pointer? arg) arg]
         [else (error who "no converter for" type arg)])]
      [(sint32)
       (cond
         [(or (fixnum? arg) (bignum? arg)) arg]
         [else (error who "no converter for" type arg)])]
      [(float)
       (cond
         [(cfloat? arg) (cfloat-value arg)]
         [else (error who "no converter for" type arg)])]
      [(double)
       (cond
         [(flonum? arg) arg]
         [else (error who "no converter for" type arg)])] 
      [else (error who "unhandled type" type)]))

  (define (wrap-conversion who return-type arg-types f)
    (lambda args
      (unless (= (length args) (length arg-types))
        (error who "incorrect number of arguments" args))
      (let ([args (map convert-outgoing arg-types args)])
        (let ([result (apply f args)])
          (convert-incoming return-type result)))))


  (define (make-proc who obj return-type args)
    (let ([arg-types (map (arg->arg-type who) args)])
      (let ([ffi (get-ffi return-type arg-types)])
        (wrap-conversion 
          (shared-object-name obj) 
          return-type arg-types
          (ffi (shared-object-pointer obj))))))


  (define (get-proc who obj return-type args)
    (unless (shared-object? obj)
      (error who "not a shared object" obj))
    (or (shared-object-proc obj)
        (let ([p (make-proc who obj return-type args)])
          (shared-object-proc-set! obj p)
          p)))


  (define-record-type cstring (fields pointer))


  (define (string->cstring str)
    (let ([bv (string->utf8 str)])
      (let ([n (bytevector-length bv)])
        (let ([p (malloc (+ n 1))])
          (let f ([i 0])
            (unless (= i n)
              (pointer-set-char p i (bytevector-u8-ref bv i))
              (f (+ i 1))))
          (pointer-set-char p n 0)
          (make-cstring p)))))

  
  (define (bytevector->blob bv)
    (let ([n (bytevector-length bv)])
      (let ([p (malloc n)])
        (let f ([i 0] [j (- n 1)])
          (unless (= i n)
            (pointer-set-char p i (bytevector-u8-ref bv j))
            (f (+ i 1) (- j 1))))
        p)))


  #;
  (define (bytevector->blob bv)
    (let ([n (bytevector-length bv)])
      (let ([p (malloc n)])
        (let f ([i 0])
          (unless (= i n)
            (pointer-set-char p i (bytevector-u8-ref bv i))
            (f (+ i 1))))
        p)))


  (define (object->long x)
    (cond
      [(integer? x) x]
      [(cstring? x) (pointer->integer (cstring-pointer x))]
      [else (error 'object->pointer "cannot handle" x)]))

  (define (vector->blob v)
    (let ([n (vector-length v)])
      (let ([p (malloc (* n 4))])
        (let f ([i 0] [j (- n 1)])
          (unless (= i n)
            (pointer-set-long p (* i 4)
              (object->long (vector-ref v j)))
            (f (+ i 1) (- j 1))))
        p)))

  (define-record-type cfloat (fields value))

  (define (flonum->float x)
    (cond
      [(flonum? x) (make-cfloat x)]
      [else (error 'flonum->float "invalid arg" x)]))


  (define (stdcall-shared-object->void proc . args) 
    (apply (get-proc 'stdcall-shared-object->void proc 'void args) args))


  (define (stdcall-shared-object->int proc . args) 
    (apply (get-proc 'stdcall-shared-object->int proc 'sint32 args) args))
                
  (define (stdcall-shared-object->intptr proc . args) 
    (apply (get-proc 'stdcall-shared-object->intptr proc 'pointer args) args))

  (define (make-callback conv argcount proc)
    (import (ikarus system $foreign))
    (define who 'make-callback)
    (unless (procedure? proc) (error who "not a procedure" proc))
    (unless (eqv? 0 conv) (error who "invalid calling convention" conv))
    ((make-callback 'sint32 (make-list argcount 'sint32)) proc))


  (define (stdcall-shared-object->double . args) (error '#f "invalid args" args))
  (define (usleep . args) (error '#f "invalid args" args))

  (define (microsecond)
    (let ([t (current-time)])
      (+ (* (time-second t) 1000000)
         (div (time-nanosecond t) 1000))))
  
  ;(define-record-type carray (fields pointer))

  ;(define make-binary-array-of-int
  ;  (lambda argv
  ;    (let ((step (architecture-feature 'alignof:int))
  ;          (proc (case (architecture-feature 'sizeof:int)
  ;                  ((4) pointer-set-int)
  ;                  ((8) pointer-set-long)
  ;                  (else
  ;                   (syntax-violation 'make-binary-array-of-int "byte size of int not defined")))))
  ;      (let ((p (malloc (* step (length argv)))))
  ;        (let loop ((offset 0) (arg argv))
  ;          (cond ((null? arg) (make-carray p))
  ;                (else
  ;                 (let ((value (car arg)))
  ;                   (proc p offset value)
  ;                   (loop (+ offset step) (cdr arg))))))))))
  ;

  ;(define make-binary-array-of-char*
  ;  (lambda (ref . argv)
  ;    (assert (= ref 0))
  ;    (let ((step (architecture-feature 'alignof:int))
  ;          (proc (case (architecture-feature 'sizeof:int)
  ;                  ((4) pointer-set-int)
  ;                  ((8) pointer-set-long)
  ;                  (else
  ;                   (syntax-violation 'make-binary-array-of-char* "byte size of int not defined")))))
  ;      (let ((p (malloc (* step (length argv)))))
  ;        (let loop ((offset 0) (arg argv))
  ;          (cond ((null? arg) (make-carray p))
  ;                (else
  ;                 (let ((value (car arg)))
  ;                   (proc p offset 
  ;                         (pointer->integer 
  ;                           (cstring-pointer (string->cstring value))))
  ;                   (loop (+ offset step) (cdr arg))))))))))

)

