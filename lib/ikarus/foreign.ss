
(library (ikarus foreign)

  (export 
          pointer-set-c-char!
          pointer-set-c-short!
          pointer-set-c-int!
          pointer-set-c-long!
          pointer-set-c-pointer!
          pointer-set-c-float!
          pointer-set-c-double!
          pointer-ref-c-signed-char
          pointer-ref-c-signed-short
          pointer-ref-c-signed-int
          pointer-ref-c-signed-long
          pointer-ref-c-unsigned-char 
          pointer-ref-c-unsigned-short
          pointer-ref-c-unsigned-int
          pointer-ref-c-unsigned-long
          pointer-ref-c-pointer
          pointer-ref-c-float 
          pointer-ref-c-double
          malloc free 
          pointer->integer integer->pointer pointer? dlopen dlsym
          dlclose dlerror
          make-c-callout make-c-callback)

  (import (ikarus system $foreign)))

