
(library (ypsilon-compat)
  (export on-windows on-darwin on-linux on-freebsd on-posix
          load-shared-object c-argument c-function 
          microsecond usleep format)
  (import (core) (ffi)))
