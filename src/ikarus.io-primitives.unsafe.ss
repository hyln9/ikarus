
(library (ikarus io-primitives unsafe)
  (export $write-char $read-char $unread-char $peek-char
          $reset-input-port! $flush-output-port 
          $close-input-port $close-output-port)
  (import
    (ikarus)
    (only (scheme) $port-handler $port-output-buffer
          $set-port-output-index! $port-output-size
          $set-port-input-index! $port-input-buffer $port-input-size
          $set-port-input-size!
          $port-input-index
          $fxadd1 $fxsub1 $fx< $fx>= $string-ref
          $string-length $string-set!))

  (define $write-char
    (lambda (c p)
      (let ([idx (port-output-index p)])
        (if ($fx< idx ($port-output-size p))
            (begin
              ($string-set! ($port-output-buffer p) idx c)
              ($set-port-output-index! p ($fxadd1 idx)))
            (($port-handler p) 'write-char c p)))))

  (define $read-char
    (lambda (p)
      (let ([idx ($port-input-index p)])
        (if ($fx< idx ($port-input-size p))
            (begin
              ($set-port-input-index! p ($fxadd1 idx))
              ($string-ref ($port-input-buffer p) idx))
            (begin
              (($port-handler p) 'read-char p))))))

  (define $peek-char
    (lambda (p)
      (let ([idx ($port-input-index p)])
        (if ($fx< idx ($port-input-size p))
            ($string-ref ($port-input-buffer p) idx)
            (($port-handler p) 'peek-char p)))))

  (define $unread-char
    (lambda (c p)
      (let ([idx ($fxsub1 ($port-input-index p))])
        (if (and ($fx>= idx 0)
                 ($fx< idx ($port-input-size p)))
            (begin
              ($set-port-input-index! p idx)
              ($string-set! ($port-input-buffer p) idx c))
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
