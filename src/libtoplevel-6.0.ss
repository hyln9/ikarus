
(for-each
  (lambda (x)
    (let ([v (primitive-ref x)])
      (when (procedure? v)
        (set-top-level-value! x v))))
  (oblist))

