
(library (ikarus.pointers)
  (export pointer? integer->pointer pointer->integer 
          dlopen dlerror dlclose dlsym malloc free)
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


  )


