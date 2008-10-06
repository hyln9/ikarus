
(library (ikarus foreign)

  (export malloc free pointer-set-char pointer-set-short
          pointer-set-int pointer-set-long pointer-set-pointer
          pointer-set-float pointer-set-double
          pointer-ref-signed-char pointer-ref-signed-short
          pointer-ref-signed-int pointer-ref-signed-long
          pointer-ref-unsigned-char pointer-ref-unsigned-short
          pointer-ref-unsigned-int pointer-ref-unsigned-long
          pointer-ref-pointer pointer-ref-float pointer-ref-double
          pointer->integer integer->pointer pointer? dlopen dlsym
          dlclose dlerror
          make-callout make-callback)

  (import (ikarus system $foreign)))

