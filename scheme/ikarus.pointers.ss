
(library (ikarus.pointers)
  (export pointer? integer->pointer pointer->integer 
          dlopen dlerror dlclose dlsym malloc free
          pointer-ref-char pointer-ref-short pointer-ref-int pointer-ref-long
          pointer-ref-uchar pointer-ref-ushort pointer-ref-uint pointer-ref-ulong
          pointer-set-char pointer-set-short pointer-set-int pointer-set-long
          ffi-prep-cif)
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
    (case-lambda 
      [(x) (dlopen x #t #t)]
      [(x lazy? global?) 
       (define (open x)
         (foreign-call "ikrt_dlopen" x lazy? global?))
       (cond
         [(not x) (open #f)]
         [(string? x) (open (string->utf8 x))]
         [else (die 'dlopen "name should be a string or #f" x)])]))

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
      [(_ name foreign-name) 
       (define name
         (lambda (p i v)
           (if (pointer? p)
               (if (fixnum? i)
                   (if (or (fixnum? v) (bignum? v))
                       (foreign-call foreign-name p i v)
                       (die 'name "value must be a fixnum or bignum" v))
                   (die 'name "index is not a fixnum" i))
               (die 'name "not a pointer" p))))]))

  (define-getter pointer-ref-char   "ikrt_ref_char")
  (define-getter pointer-ref-short  "ikrt_ref_short")
  (define-getter pointer-ref-int    "ikrt_ref_int")
  (define-getter pointer-ref-long   "ikrt_ref_long")
  
  (define-getter pointer-ref-uchar  "ikrt_ref_uchar")
  (define-getter pointer-ref-ushort "ikrt_ref_ushort")
  (define-getter pointer-ref-uint   "ikrt_ref_uint")
  (define-getter pointer-ref-ulong  "ikrt_ref_ulong")

  (define-setter pointer-set-char   "ikrt_set_char")
  (define-setter pointer-set-short  "ikrt_set_short")
  (define-setter pointer-set-int    "ikrt_set_int")
  (define-setter pointer-set-long   "ikrt_set_long")

  ;;; libffi interface

  (define (ffi-prep-cif rtype argtypes)
    (define who 'ffi-prep-cif)
    (define (convert x)
      (case x 
        [(void)       1]
        [(uint8)      2]
        [(sint8)      3]
        [(uint16)     4]
        [(sint16)     5]
        [(uint32)     6]
        [(sint32)     7]
        [(uint64)     8]
        [(sint64)     9]
        [(float)     10]
        [(double)    11]
        [(pointer)   12]
        [else (die who "invalid type" x)]))
    (unless (list? argtypes) 
      (die who "arg types is not a list" argtypes))
    (let ([argtypes-n (vector-map convert (list->vector argtypes))]
          [rtype-n (convert rtype)])
      (let ([cif (or (foreign-call "ikrt_ffi_prep_cif" rtype-n argtypes-n)
                     (die who "failed to initialize" rtype argtypes))])
        (lambda (cfun)
          (define data (vector cif cfun argtypes-n rtype-n))
          (unless (pointer? cfun) 
            (die 'ffi "not a pointer" cfun))
          (lambda args
            (let ([argsvec (list->vector args)])
              (unless (= (vector-length argsvec) 
                         (vector-length argtypes-n))
                (error 'ffi "args mismatch" argtypes args))
              (foreign-call "ikrt_ffi_call" data argsvec)))))))

  )


 
