
(library (ikarus.pointers)
  (export pointer? integer->pointer pointer->integer 
          dlopen dlerror dlclose dlsym malloc free
          pointer-ref-signed-char 
          pointer-ref-signed-short
          pointer-ref-signed-int 
          pointer-ref-signed-long
          pointer-ref-unsigned-char
          pointer-ref-unsigned-short 
          pointer-ref-unsigned-int 
          pointer-ref-unsigned-long
          pointer-set-char pointer-set-short pointer-set-int pointer-set-long
          pointer-set-pointer pointer-ref-pointer
          pointer-set-float pointer-ref-float
          pointer-set-double pointer-ref-double
          make-callout make-callback)
  (import 
    (except (ikarus) 
      pointer? 
      integer->pointer pointer->integer
      dlopen dlerror dlclose dlsym malloc free))

  ;;; pointer manipulation procedures 

  (define (pointer? x)
    (foreign-call "ikrt_isapointer" x))

  (define (integer->pointer x)
    (cond
      [(fixnum? x) 
       (foreign-call "ikrt_fx_to_pointer" x)]
      [(bignum? x)
       (foreign-call "ikrt_bn_to_pointer" x)]
      [else 
       (die 'integer->pointer "not an integer" x)]))

  (define (pointer->integer x)
    (cond
      [(pointer? x) 
       (foreign-call "ikrt_pointer_to_int" x)]
      [else 
       (die 'pointer->integer "not a pointer" x)]))

  ;;; dynamic loading procedures 

  (define dlerror 
    (lambda ()
      (let ([p (foreign-call "ikrt_dlerror")])
        (and p (utf8->string p)))))

  (define dlopen 
    (let ()
      (define (open x lazy? global?)
        (foreign-call "ikrt_dlopen" x lazy? global?))
      (case-lambda 
        [()  
         (open #f #f #f)]
        [(x)
         (dlopen x #f #f)]
        [(x lazy? global?)
         (cond
           [(string? x) (open (string->utf8 x) lazy? global?)]
           [else (die 'dlopen "library name must be a string" x)])])))

  (define dlclose
    (lambda (x)
      (if (pointer? x)
          (foreign-call "ikrt_dlclose" x)
          (die 'dlclose "not a pointer" x))))

  (define dlsym
    (lambda (handle name)
      (define who 'dlsym)
      (if (pointer? handle)
          (if (string? name) 
              (foreign-call "ikrt_dlsym" handle (string->utf8 name))
              (die who "invalid symbol name" name))
          (die who "handle is not a pointer" handle))))

  ;;; explicit memory management

  (define (malloc len) 
    (if (and (fixnum? len) (fx>? len 0))
        (foreign-call "ikrt_malloc" len)
        (die 'malloc "not a positive fixnum" len)))

  (define (free x)
    (if (pointer? x)
        (foreign-call "ikrt_free" x)
        (die 'free "not a pointer" x)))

  ;;; getters and setters

  (define-syntax define-getter
    (syntax-rules ()
      [(_ name foreign-name) 
       (define name
         (lambda (p i)
           (if (pointer? p)
               (if (fixnum? i)
                   (foreign-call foreign-name p i)
                   (die 'name "index is not a fixnum" i))
               (die 'name "not a pointer" p))))]))

  (define-syntax define-setter
    (syntax-rules ()
      [(_ name pred? foreign-name) 
       (define name
         (lambda (p i v)
           (if (pointer? p)
               (if (fixnum? i)
                   (if (pred? v)
                       (foreign-call foreign-name p i v)
                       (die 'name 
                          (format "value must satisfy the predicate ~a" 'pred?)
                          v))
                   (die 'name "index is not a fixnum" i))
               (die 'name "not a pointer" p))))]))

  (define (int? x) (or (fixnum? x) (bignum? x)))

  (define-getter pointer-ref-signed-char    "ikrt_ref_char")
  (define-getter pointer-ref-signed-short   "ikrt_ref_short")
  (define-getter pointer-ref-signed-int     "ikrt_ref_int")
  (define-getter pointer-ref-signed-long    "ikrt_ref_long")
  (define-getter pointer-ref-unsigned-char  "ikrt_ref_uchar")
  (define-getter pointer-ref-unsigned-short "ikrt_ref_ushort")
  (define-getter pointer-ref-unsigned-int   "ikrt_ref_uint")
  (define-getter pointer-ref-unsigned-long  "ikrt_ref_ulong")
  (define-getter pointer-ref-float          "ikrt_ref_float")
  (define-getter pointer-ref-double         "ikrt_ref_double")
  (define-getter pointer-ref-pointer        "ikrt_ref_pointer")

  (define-setter pointer-set-char    int?     "ikrt_set_char")
  (define-setter pointer-set-short   int?     "ikrt_set_short")
  (define-setter pointer-set-int     int?     "ikrt_set_int")
  (define-setter pointer-set-long    int?     "ikrt_set_long")
  (define-setter pointer-set-float   flonum?  "ikrt_set_float")
  (define-setter pointer-set-double  flonum?  "ikrt_set_double")
  (define-setter pointer-set-pointer pointer? "ikrt_set_pointer")

  ;;; libffi interface

  (define (checker who)
    (define (checker t)
      (cond
        [(vector? t)
         (let ([t* (vector-map checker t)])
           (lambda (v)
             (and (vector? v)
                  (let ([n (vector-length v)])
                    (and (= n (vector-length t))
                         (let f ([i 0])
                           (or (= i n)
                               (and ((vector-ref t* i) (vector-ref v i))
                                    (f (+ i 1))))))))))]
        [else
         (case t
           [(unsigned-char)   int?]
           [(signed-char)     int?]
           [(unsigned-short)  int?]
           [(signed-short)    int?]
           [(unsigned-int)    int?]
           [(signed-int)      int?]
           [(unsigned-long)   int?]
           [(signed-long)     int?]
           [(float)           flonum?]
           [(double)          flonum?]
           [(pointer)         pointer?]
           [else (die who "invalid type" t)])]))
    checker)
 


  (define (ffi-prep-cif rtype argtypes)
    (define who 'ffi-prep-cif)
    (define (convert x)
      (cond
        [(vector? x) (vector-map convert x)]
        [else
         (case x 
           [(void)            1]
           [(unsigned-char)   2]
           [(signed-char)     3]
           [(unsigned-short)  4]
           [(signed-short)    5]
           [(unsigned-int)    6]
           [(signed-int)      7]
           [(unsigned-long)   8]
           [(signed-long)     9]
           [(float)          10]
           [(double)         11]
           [(pointer)        12]
           [else (die who "invalid type" x)])]))
    (unless (list? argtypes)
      (die who "arg types is not a list" argtypes))
    (let ([argtypes-n (vector-map convert (list->vector argtypes))]
          [rtype-n (convert rtype)])
      (values (or (foreign-call "ikrt_ffi_prep_cif" rtype-n argtypes-n)
                  (die who "failed to initialize" rtype argtypes))
              argtypes-n
              rtype-n)))

  (define (make-callout rtype argtypes)
    (define who 'make-callout)
    (let-values ([(cif argtypes-n rtype-n)
                  (ffi-prep-cif rtype argtypes)])
      (let* ([argtypes-vec (list->vector argtypes)]
             [checkers (vector-map (checker who) argtypes-vec)])
        (lambda (cfun)
          (define data (vector cif cfun argtypes-n rtype-n))
          (unless (pointer? cfun)
            (die who "not a pointer" cfun))
          (lambda args
            (let ([argsvec (list->vector args)])
              (unless (= (vector-length argsvec) 
                         (vector-length argtypes-vec))
                (error 'callout-procedure "arg length mismatch" 
                       (vector->list argtypes-vec)
                       args))
              (vector-for-each
                (lambda (p? t x)
                  (unless (p? x)
                    (die 'callout-procedure 
                         (format "argument does not match type ~a" t)
                         x)))
                checkers argtypes-vec argsvec)
              (foreign-call "ikrt_ffi_call" data argsvec)))))))

  (define (make-callback rtype argtypes)
    (let-values ([(cif argtypes-n rtype-n)
                  (ffi-prep-cif rtype argtypes)])
      (lambda (proc)
        (define who 'make-callback)
        (unless (procedure? proc)
          (die who "not a procedure"))
        (let ([proc 
               (cond
                 [(eq? rtype 'void) proc]
                 [else
                  (let ([p? ((checker who) rtype)])
                    (lambda args 
                      (let ([v (apply proc args)])
                        (unless (p? v)
                          (die 'callback
                             (format "returned value does not match type ~a"
                                     rtype)
                             v))
                        v)))])])
          (let ([data (vector cif proc argtypes-n rtype-n)])
            (or (foreign-call "ikrt_prepare_callback" data)
                (die who "cannot prepare foreign callback")))))))

  )


 
