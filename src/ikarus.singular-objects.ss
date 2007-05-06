
(library (ikarus singular-objects)
  (export base-rtd eof-object)
  (import 
    (rename (ikarus)
      (eof-object sys:eof-object)
      (base-rtd sys:base-rtd)))

  (define (eof-object) (sys:eof-object))
  (define (base-rtd) (sys:base-rtd)))

