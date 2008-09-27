(library (Cocoa helpers)
  (export make-app nsstring)
  (import (ikarus) (Cocoa) (objc) (ikarus system $foreign))

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


  (define (nsstring x)
    [$ [$ NSString alloc]
       initWithCharactersNoCopy: x
       length: (string-length x)
       freeWhenDone: #t])

)


