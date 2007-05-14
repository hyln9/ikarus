;;; procedural layer:

(make-record-type-descriptor name parent uid sealed? opaque? fields)
;;; name is a symbol, for informational purposes only
;;; parent is #f or an rtd
;;; uid is #f or symbol.
;;;    symbol => nongenerative (or interned by that name)
;;;    #f => generative        (can use a gensym for uid)
;;; if parent is sealed?, then it cannot be extended,
;;;   therefore, parent must not be sealed.
;;; if parent is opaque, then so is the rtd (overrides opaque?)
;;; if parent is not opaque, then use opaque?
;;; fields is a vector where each element is either:
;;;    (mutable <symbol>)   or   (immutable <symbol>)
;;;  symbols need not be unique, they're informational.

(record-type-descriptor? x)
;;; self explanatory.

(make-record-constructor-descriptor 
  rtd parent-constructor-descriptor protocol)
;;; returns a record constructor descriptor
;;; protocol is either a procedure or #f
;;;  if rtd has no parent, then protocol must be #f
;;;    typical protocol procedure is:
;;;      (lambda (new)
;;;         (lambda (bar baz) ;;; constructor of 2 args
;;;            ;;; for a record of three fields
;;;            (new bar baz (* bar baz))))
;;;  default protocol procedure is (lambda (new) new)
;;;  an extension protocol is typically:
;;;     (lambda (parent-new) 
;;;       (lambda (px py pz x y z)
;;;         (let ([new (parent-new px py pz)])
;;;           (new x y z))))
;;; or  (lambda (parent-new)
;;;       (lambda (px py pz x y z)
;;;         ((parent-new px py pz) x y z)))
;;;


(record-constructor rcd) 
;;; returns a procedure that constructs records.  The number of
;;; arguments that the procedure takes is the same number of
;;; arguments in the protocol's inner lambda.

(record-predicate rtd)  ;=> predicate procedure.
(record-accessor rtd k) ;=> accessor procedure.
(record-mutator rtd k)  ;=> mutator procedure.


