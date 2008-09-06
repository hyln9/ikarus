
(library (ikarus.pointers)
  (export pointer? integer->pointer pointer->integer)
  (import (except (ikarus) pointer? integer->pointer pointer->integer))

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
       (die 'pointer->integer "not a pointer" x)])))


