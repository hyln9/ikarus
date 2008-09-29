#!/usr/bin/env ikarus --r6rs-script

(import (ikarus) (objc))

(define-framework Cocoa)
(define-class NSObject)

(define IKFact 
  (create-class "IKFact" NSObject '() #f))

(class-add-class-method IKFact 'fact: 'int '(class selector int)
  (trace-lambda fact (self sel n)
    (if (zero? n)
        1
        (* n [$ self fact: (sub1 n)]))))

(printf "(fact 5) = ~s\n" [$ IKFact fact: 5])
