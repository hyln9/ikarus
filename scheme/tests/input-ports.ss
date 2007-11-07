
(library (tests input-ports)
  (export test-input-ports)
  (import (ikarus) (tests framework))


  (define-tests test-input-ports
    [eof-object?
     (get-line (open-input-string ""))]
    [(lambda (x) (equal? x "abcd"))
     (get-line (open-input-string "abcd"))]
    [(lambda (x) (equal? x ""))
     (get-line (open-input-string "\nabcd"))]
    [(lambda (x) (equal? x "abcd"))
     (get-line (open-input-string "abcd\nefg"))]))

