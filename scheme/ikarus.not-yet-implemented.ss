
(library (ikarus not-yet-implemented)
  (export 
    bitwise-reverse-bit-field
    bitwise-rotate-bit-field fxreverse-bit-field
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port 
    equal-hash)

  (import (except (ikarus) 
    bitwise-reverse-bit-field
    bitwise-rotate-bit-field fxreverse-bit-field
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port 
    equal-hash))
  
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
               (make-message-condition "primitive not supported")
               (make-message-condition
                 "Please visit the Ikarus FAQs page for more information")
               (make-url-condition 
                 "https://answers.launchpad.net/ikarus/+faqs")
               (make-irritants-condition (list op)))))
         (define (x* . args) (bug 'x*))
         ...)]))

  (not-yet 
    ;;; should be implemented
    bitwise-rotate-bit-field bitwise-reverse-bit-field
    fxreverse-bit-field 
    ;;; not top priority at the moment
    equal-hash
    ;;; won't be implemented
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port
    ))


