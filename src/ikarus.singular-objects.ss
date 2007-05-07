
(library (ikarus singular-objects)
  (export base-rtd eof-object void)
  (import 
    (rename (ikarus system $records) (base-rtd sys:base-rtd))
    (rename (ikarus)
            (void sys:void)
            (eof-object sys:eof-object)))

  (define (void) (sys:void))
  (define (eof-object) (sys:eof-object))
  (define (base-rtd) (sys:base-rtd)))

