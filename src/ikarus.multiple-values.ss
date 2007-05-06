
(library (ikarus multiple-values)
  (export call-with-values values)
  (import 
    (ikarus system $stack)
    (except (ikarus) call-with-values values))

  (define call-with-values 
    ($make-call-with-values-procedure))
  (define values 
    ($make-values-procedure)))
