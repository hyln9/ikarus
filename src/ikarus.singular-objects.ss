
(library (ikarus singular-objects)
  (export base-rtd eof-object void #;fixnum-width)
  (import 
    (rename (ikarus system $records) (base-rtd sys:base-rtd))
    (rename (ikarus)
            (void sys:void)
           #; (fixnum-width sys:fixnum-width)
            (eof-object sys:eof-object)))

  (define (void) (sys:void))
#;  (define (fixnum-width) (sys:fixnum-width))
  (define (eof-object) (sys:eof-object))
  (define (base-rtd) (sys:base-rtd)))

