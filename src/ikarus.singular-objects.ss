
(library (ikarus singular-objects)
  (export base-rtd eof-object void fixnum-width least-fixnum
          greatest-fixnum)
  (import 
    (rename (ikarus system $records) (base-rtd sys:base-rtd))
    (rename (ikarus)
            (void sys:void)
            (fixnum-width sys:fixnum-width)
            (least-fixnum sys:least-fixnum)
            (greatest-fixnum sys:greatest-fixnum)
            (eof-object sys:eof-object)))

  (define (void) (sys:void))
  (define (fixnum-width) (sys:fixnum-width))
  (define (least-fixnum) (sys:least-fixnum))
  (define (greatest-fixnum) (sys:greatest-fixnum))
  (define (eof-object) (sys:eof-object))
  (define (base-rtd) (sys:base-rtd)))

