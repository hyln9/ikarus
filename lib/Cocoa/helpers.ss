(library (Cocoa helpers)
  (export make-app)
  (import (ikarus) (ikarus system $foreign))

  (define (make-app)
    (define kProcessTransformToForegroundApplication 1)
    (define self (dlopen #f))
    (define get-current-process
      ((make-ffi 'void '(pointer))
       (dlsym self "GetCurrentProcess")))
    (define transform-process-type
      ((make-ffi 'void '(pointer sint32)) 
       (dlsym self "TransformProcessType")))
    (define set-front-process
      ((make-ffi 'void '(pointer)) 
       (dlsym self "SetFrontProcess")))
    (let ([p (malloc 16)])
      (get-current-process p)
      (transform-process-type p kProcessTransformToForegroundApplication)
      (set-front-process p)
      (free p)))
)


