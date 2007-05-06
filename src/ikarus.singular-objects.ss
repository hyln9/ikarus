
(library (ikarus singular-objects)
  (export base-rtd eof-object)
  (import 
    (rename (ikarus system $records) (base-rtd sys:base-rtd))
    (rename (ikarus) (eof-object sys:eof-object)))

  (define (eof-object) (sys:eof-object))
  (define (base-rtd) (sys:base-rtd)))

