
(library (ikarus not-yet-implemented)
  (export 
    angle make-polar 
    bitwise-copy-bit-field bitwise-reverse-bit-field
    bitwise-rotate-bit-field bitwise-if fxreverse-bit-field
    fxrotate-bit-field bytevector->string string->bytevector
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port output-port-buffer-mode
    port-has-set-port-position!?
    set-port-position!  make-eqv-hashtable
    hashtable-hash-function make-hashtable
    hashtable-equivalence-function equal-hash
    string-downcase string-normalize-nfc string-normalize-nfd
    string-normalize-nfkc string-normalize-nfkd string-titlecase
    string-upcase)

  (import (except (ikarus) 
    angle make-polar 
    bitwise-copy-bit-field bitwise-reverse-bit-field
    bitwise-rotate-bit-field bitwise-if fxreverse-bit-field
    fxrotate-bit-field bytevector->string string->bytevector
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port output-port-buffer-mode
    port-has-set-port-position!?
    set-port-position!  make-eqv-hashtable
    hashtable-hash-function make-hashtable
    hashtable-equivalence-function equal-hash
    string-downcase string-normalize-nfc string-normalize-nfd
    string-normalize-nfkc string-normalize-nfkd string-titlecase
    string-upcase))
  
  (define-syntax not-yet
    (syntax-rules ()
      [(_ x* ...) 
       (begin 
         (define (bug op)
           (define-condition-type &url &condition
             make-url-condition
             url-condition?
            (url condition-url))
           (raise 
             (condition 
               (make-error)
               (make-who-condition 'ikarus)
               (make-message-condition "primitive not supported yet")
               (make-message-condition
                 "please file a bug report to help us prioritize our goals")
               (make-url-condition 
                 "https://bugs.launchpad.net/ikarus/+filebug")
               (make-irritants-condition (list op)))))
         (define (x* . args) (bug 'x*))
         ...)]))

  (not-yet 
    ;;; should be implemented
    bytevector->string string->bytevector
    string-downcase string-titlecase string-upcase
    angle make-polar 
    bitwise-if
    bitwise-rotate-bit-field bitwise-copy-bit-field bitwise-reverse-bit-field
    fxreverse-bit-field fxrotate-bit-field 
    ;;; not top priority at the moment
    make-eqv-hashtable make-hashtable equal-hash
    hashtable-hash-function hashtable-equivalence-function
    string-normalize-nfc string-normalize-nfd
    string-normalize-nfkc string-normalize-nfkd 
    ;;; won't be implemented
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port
    output-port-buffer-mode
    port-has-set-port-position!? set-port-position!  ))


