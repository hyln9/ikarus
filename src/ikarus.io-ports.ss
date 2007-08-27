
(library (ikarus io-ports)
  (export make-input-port make-output-port 
          port-handler
          port-input-buffer  port-output-buffer 
          port-input-index   set-port-input-index!
          port-input-size    set-port-input-size!
          port-output-index  set-port-output-index!
          port-output-size   set-port-output-size!)
  (import 
    (ikarus system $ports)
    (ikarus system $strings)
    (ikarus system $bytevectors)
    (ikarus system $fx)
    (except (ikarus)  
         make-input-port make-output-port 
         port-handler
         port-input-buffer  port-output-buffer
         port-input-index   set-port-input-index!
         port-input-size    set-port-input-size!
         port-output-index  set-port-output-index!
         port-output-size   set-port-output-size!))
  ;;; GENERIC PORTS: BASIC PRIMITIVES
  ;;;
  ;;; Exports: 
  ;;; * Constructors:
  ;;;   (make-input-port handler input-buffer)
  ;;;   (make-output-port handler output-buffer)
  ;;;
  ;;; * Predicates:
  ;;;   (port? x)
  ;;;   (input-port? x)
  ;;;   (output-port? x)
  ;;;
  ;;; * Accessors:
  ;;;   (port-handler port)
  ;;;   (port-buffer port)
  ;;;   (port-index port)
  ;;;   (port-size port)
  ;;;
  ;;; * Mutators:
  ;;;   (set-port-index! port fixnum)
  ;;;   (set-port-size! port fixnum)
  ;;;
  (define $make-input-port
    (lambda (handler buffer)
      ($make-port/input handler buffer 0 ($bytevector-length buffer))))
  ;;;
  (define make-input-port
    (lambda (handler buffer)
      (if (procedure? handler)
          (if (bytevector? buffer)
              ($make-input-port handler buffer)
              (error 'make-input-port "~s is not a bytevector" buffer))
          (error 'make-input-port "~s is not a procedure" handler))))
  ;;;
  (define $make-output-port
    (lambda (handler buffer)
      ($make-port/output handler buffer 0 ($bytevector-length buffer))))
  ;;;
  (define make-output-port
    (lambda (handler buffer)
      (if (procedure? handler)
          (if (bytevector? buffer)
              ($make-output-port handler buffer)
              (error 'make-output-port "~s is not a bytevector" buffer))
          (error 'make-output-port "~s is not a procedure" handler))))
  ;;;
  (define port-handler
    (lambda (x)
      (if (port? x)
          ($port-handler x)
          (error 'port-handler "~s is not a port" x))))
  ;;;
  (define port-input-buffer
    (lambda (x)
      (if (input-port? x)
          ($port-buffer x)
          (error 'port-input-buffer "~s is not an input-port" x))))
  ;;;
  (define port-input-index
    (lambda (x)
      (if (input-port? x)
          ($port-index x)
          (error 'port-input-index "~s is not an input-port" x))))
  ;;;
  (define port-input-size
    (lambda (x)
      (if (input-port? x)
          ($port-size x)
          (error 'port-input-size "~s is not an input-port" x))))
  ;;;
  (define port-output-buffer
    (lambda (x)
      (if (output-port? x)
          ($port-buffer x)
          (error 'port-output-buffer "~s is not an output-port" x))))
  ;;;
  (define port-output-index
    (lambda (x)
      (if (output-port? x)
          ($port-index x)
          (error 'port-output-index "~s is not an output-port" x))))
  ;;;
  (define port-output-size
    (lambda (x)
      (if (output-port? x)
          ($port-size x)
          (error 'port-output-size "~s is not an output-port" x))))
  ;;;
  (define set-port-input-index!
    (lambda (p i)
      (if (input-port? p)
          (if (fixnum? i)
              (if ($fx>= i 0)
                  (if ($fx<= i ($port-size p))
                      ($set-port-index! p i)
                      (error 'set-port-input-index! "index ~s is too big" i))
                  (error 'set-port-input-index! "index ~s is negative" i))
              (error 'set-port-input-index! "~s is not a valid index" i))
          (error 'set-port-input-index! "~s is not an input-port" p))))
  ;;;
  (define set-port-input-size!
    (lambda (p i)
      (if (input-port? p)
          (if (fixnum? i)
              (if ($fx>= i 0)
                  (if ($fx<= i ($bytevector-length ($port-buffer p)))
                      (begin
                        ($set-port-index! p 0)
                        ($set-port-size! p i))
                      (error 'set-port-input-size! "size ~s is too big" i))
                  (error 'set-port-input-size! "size ~s is negative" i))
              (error 'set-port-input-size! "~s is not a valid size" i))
          (error 'set-port-input-size! "~s is not an input-port" p))))
  ;;;
  (define set-port-output-index!
    (lambda (p i)
      (if (output-port? p)
          (if (fixnum? i)
              (if ($fx>= i 0)
                  (if ($fx<= i ($port-size p))
                      ($set-port-index! p i)
                      (error 'set-port-output-index! "index ~s is too big" i))
                  (error 'set-port-output-index! "index ~s is negative" i))
              (error 'set-port-output-index! "~s is not a valid index" i))
          (error 'set-port-output-index! "~s is not an output-port" p))))
  ;;;
  (define set-port-output-size!
    (lambda (p i)
      (if (output-port? p)
          (if (fixnum? i)
              (if ($fx>= i 0)
                  (if ($fx<= i ($bytevector-length ($port-buffer p)))
                      (begin
                        ($set-port-index! p 0)
                        ($set-port-size! p i))
                      (error 'set-port-output-size! "size ~s is too big" i))
                  (error 'set-port-output-size! "size ~s is negative" i))
              (error 'set-port-output-size! "~s is not a valid size" i))
          (error 'set-port-output-size! "~s is not an output-port" p)))))



