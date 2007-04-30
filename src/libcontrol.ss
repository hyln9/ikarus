
(library (ikarus control)
  (export)
  (import (scheme))
;(let ()

(let ()
  (define call-with-current-frame
    (lambda (f)
      (if ($fp-at-base)
          (f ($current-frame))
          ($seal-frame-and-call f))))
  (primitive-set! 'call/cf call-with-current-frame))
 
(let () 
  (define primitive-call/cc
    (lambda (f)
      (call/cf
        (lambda (frm)
          (f ($frame->continuation frm))))))
  (primitive-set! '$primitive-call/cc primitive-call/cc))


(let ([winders '()])

  (define len
    (lambda (ls n)
      (if (null? ls)
          n
          (len (cdr ls) (fxadd1 n)))))

  (define list-tail
    (lambda (ls n)
      (if (fxzero? n)
          ls
          (list-tail (cdr ls) (fxsub1 n)))))

  (define drop-uncommon-heads 
    (lambda (x y)
      (if (eq? x y)
          x
          (drop-uncommon-heads (cdr x) (cdr y)))))

  (define common-tail
    (lambda (x y)
      (let ([lx (len x 0)] [ly (len y 0)])
        (let ([x (if (fx> lx ly) (list-tail x (fx- lx ly)) x)]
              [y (if (fx> ly lx) (list-tail y (fx- ly lx)) y)])
          (if (eq? x y)
              x
              (drop-uncommon-heads (cdr x) (cdr y)))))))

  (define unwind*
    (lambda (ls tail)
      (unless (eq? ls tail)
        (set! winders (cdr ls))
        ((cdar ls))
        (unwind* (cdr ls) tail))))

  (define rewind*
    (lambda (ls tail)
      (unless (eq? ls tail)
        (rewind* (cdr ls) tail)
        ((caar ls))
        (set! winders ls))))

  (define do-wind
    (lambda (new)
      (let ([tail (common-tail new winders)])
        (unwind* winders tail)
        (rewind* new tail))))

  (define call/cc
    (lambda (f)
      ($primitive-call/cc
        (lambda (k)
          (let ([save winders])
            (f (case-lambda
                 [(v) (unless (eq? save winders) (do-wind save)) (k v)]
                 [()  (unless (eq? save winders) (do-wind save)) (k)]
                 [(v1 v2 . v*)
                  (unless (eq? save winders) (do-wind save))
                  (apply k v1 v2 v*)])))))))

  (define dynamic-wind
    (lambda (in body out)
      (in)
      (set! winders (cons (cons in out) winders))
      (call-with-values
        body
        (case-lambda
          [(v) (set! winders (cdr winders)) (out) v]
          [()  (set! winders (cdr winders)) (out) (values)]
          [(v1 v2 . v*)
           (set! winders (cdr winders))
           (out)
           (apply values v1 v2 v*)]))))

  (primitive-set! 'call/cc call/cc)
  (primitive-set! 'dynamic-wind dynamic-wind)
  (void))
)
