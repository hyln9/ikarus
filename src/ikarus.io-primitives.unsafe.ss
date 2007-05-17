
(library (ikarus io-primitives unsafe)
  (export $write-char $read-char $unread-char $peek-char
          $reset-input-port! $flush-output-port 
          $close-input-port $close-output-port)
  (import
    (ikarus)
    (ikarus system $ports)
    (ikarus system $strings)
    (ikarus system $chars)
    (ikarus system $bytevectors)
    (ikarus system $fx))

  (define $write-char
    (lambda (c p)
      (let ([idx (port-output-index p)])
        (if ($fx< idx ($port-output-size p))
            (begin
              (string-set! ($port-output-buffer p) idx c)
              ($set-port-output-index! p ($fxadd1 idx)))
            (($port-handler p) 'write-char c p)))))

  (define $read-char
    (lambda (p)
      (let ([idx ($port-input-index p)])
        (if ($fx< idx ($port-input-size p))
            (let ([b ($bytevector-u8-ref ($port-input-buffer p) idx)])
              (cond
                [($fx<= b 127) 
                 ($set-port-input-index! p ($fxadd1 idx))
                 ($fixnum->char b)]
                [else (($port-handler p) 'read-char p)]))
            (($port-handler p) 'read-char p)))))

  (define $peek-char
    (lambda (p)
      (let ([idx ($port-input-index p)])
        (if ($fx< idx ($port-input-size p))
            (let ([b ($bytevector-u8-ref ($port-input-buffer p) idx)])
                (cond
                  [($fx<= b 127) 
                   ($fixnum->char b)]
                  [else (($port-handler p) 'peek-char p)]))
            (($port-handler p) 'peek-char p)))))

  (define $unread-char
    (lambda (c p)
      (let ([idx ($fxsub1 ($port-input-index p))]
            [b ($char->fixnum c)])
        (if (and ($fx<= b 127)
                 ($fx>= idx 0)
                 ($fx< idx ($port-input-size p)))
            (begin
              ($set-port-input-index! p idx)
              ($bytevector-set! ($port-input-buffer p) idx b))
            (($port-handler p) 'unread-char c p)))))

  (define $reset-input-port!
    (lambda (p)
      ($set-port-input-size! p 0)))

  (define $close-input-port
    (lambda (p)
      (($port-handler p) 'close-port p)))

  (define $close-output-port
    (lambda (p)
      (($port-handler p) 'close-port p)))

  (define $flush-output-port
    (lambda (p)
      (($port-handler p) 'flush-output-port p))))
