
(library (tests input-ports)
  (export test-input-ports)
  (import (ikarus) (tests framework))


  (define-tests test-input-ports
    [eof-object?
     (get-line (open-string-input-port ""))]
    [(lambda (x) (equal? x "abcd"))
     (get-line (open-string-input-port "abcd"))]
    [(lambda (x) (equal? x ""))
     (get-line (open-string-input-port "\nabcd"))]
    [(lambda (x) (equal? x "abcd"))
     (get-line (open-string-input-port "abcd\nefg"))]))

