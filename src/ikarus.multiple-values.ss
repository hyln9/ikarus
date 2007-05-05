
(library (ikarus multiple-values)
  (export call-with-values values)
  (import (except (scheme) call-with-values values))

  (define call-with-values 
    ($make-call-with-values-procedure))
  (define values 
    ($make-values-procedure)))
