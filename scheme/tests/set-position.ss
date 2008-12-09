
(library (tests set-position)
  (export run-tests)
  (import (ikarus))

  (define fname "temp-test-file")

  (define (test-setting-position-for-binary-output-files)
    (let ([pos-list '([500 12] [720 34] [12  180] [400 4])])
      (when (file-exists? fname) (delete-file fname))
      (let ([p (open-file-output-port fname)])
        (for-each
          (lambda (x)
            (set-port-position! p (car x))
            (put-u8 p (cadr x)))
          pos-list)
        (close-output-port p))
      (let ([bv 
             (let ([p (open-file-input-port fname)])
               (let ([bv (get-bytevector-all p)])
                 (close-input-port p)
                 bv))])
        (assert (= (bytevector-length bv)
                   (add1 (apply max (map car pos-list)))))
        (for-each 
          (lambda (x)
            (assert (= (bytevector-u8-ref bv (car x)) (cadr x))))
          pos-list))
      (delete-file fname)))

  (define (run-tests)
    (test-setting-position-for-binary-output-files)))

