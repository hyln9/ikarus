
(library (ikarus codecs)
  (export latin-1-codec utf-8-codec utf-16-codec)
  (import (rnrs base))
  (define (latin-1-codec) 'latin-1-codec)
  (define (utf-8-codec)   'utf-8-codec)
  (define (utf-16-codec)  'utf-16-codec))

