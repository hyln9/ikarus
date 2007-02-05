#!/usr/bin/env ikarus --script
(define (racompile x)
  ;;;
  (define-syntax record-case
    (lambda (x)
      (define (enumerate fld* i)
        (syntax-case fld* ()
          [() #'()]
          [(x . x*) 
           (with-syntax ([i i] [i* (enumerate #'x* (fx+ i 1))])
             #'(i . i*))]))
      (define (generate-body ctxt cls*)
        (syntax-case cls* (else)
          [() (with-syntax ([x x]) #'(error #f "unmatched ~s in ~s" v #'x))]
          [([else b b* ...])  #'(begin b b* ...)]
          [([(rec-name rec-field* ...) b b* ...] . rest) (identifier? #'rec-name)
           (with-syntax ([altern (generate-body ctxt #'rest)]
                         [(id* ...) (enumerate #'(rec-field* ...) 0)]
                         [rtd #'(type-descriptor rec-name)])
            #'(if (#%$record/rtd? v rtd)
                  (let ([rec-field* (#%$record-ref v id*)] ...)
                    b b* ...)
                  altern))]))
      (syntax-case x ()
        [(_ expr cls* ...)
         (with-syntax ([body (generate-body #'_ #'(cls* ...))])
           #'(let ([v expr]) body))])))
  ;;;
  (define-record constant (val))
  (define (mkconst v) (make-constant v))
  (define-record int (val))
  (define (mkint v) (make-int v))
  (define-record set (lhs rhs))
  (define (mkset x v) (make-set x v))
  (define-record reg (name))
  (define (mkreg x) (make-reg x))
  (define-record primcall (op rand*))
  (define (mkprm op . rand*) (make-primcall op rand*))
  (define-record seq (e0 e1))
  (define (mkseq e0 e1) (make-seq e0 e1))
  ;;;
  (define (recordize x)
    (define who 'recordize)
    ;;;
    (define (E x r)
      (cond
        [(pair? x)
         (case (car x)
           [(quote) (mkconst (cadr x))]
           [else (error who "invalid expression ~s" x)])]
        [else (error who "invalid expression ~s" x)]))
    ;;;
    (E x '()))
  ;;;
  (define (specify-representation x)
    (define who 'specify-representation)
    ;;;
    (define fixnum-scale 4)
    (define true-object #x3F)
    (define false-object #x2F)
    (define void-object #x7F)
    (define bwp-object #x8F)
    (define eof-object #x5F)
    (define null-object #x4F)
    (define char-shift 8)
    (define char-tag #x0F)
    (define char-mask #xFF)
    ;;;
    (define (immediate? c)
      (or (fixnum? c)
          (boolean? c)
          (char? c)
          (null? c)
          (eq? c (void))
          (eof-object? c)
          (bwp-object? c)))
    ;;;
    (define (immediate-rep c)
      (cond
        [(fixnum? c) (mkint (* c fixnum-scale))]
        [(boolean? c) (mkint (if c true-object false-object))]
        [(char? c) 
         (mkint (fxlogor char-tag (fxsll (char->integer c) char-shift)))]
        [(null? c) (mkint null-object)]
        [(eof-object? c) (mkint eof-object)]
        [(eq? c (void)) (mkint void-object)]
        [(bwp-object? c) (mkint bwp-object)]
        [else (error 'immediate-rep "invalid ~s" c)]))
    ;;;
    (define (Tail x)
      (record-case x
        [(constant c)
         (if (immediate? c)
             (immediate-rep c)
             x)]
        [else (error who "invalid tail ~s" x)]))
    ;;;
    (Tail x))
  ;;;
  (define (impose-calling-convention x)
    (define who 'impose-calling-convention)
    ;;;
    (define rv-register (mkreg '%eax))
    ;;;
    (define (return x)
      (mkseq (mkset rv-register x)
             (mkprm 'return rv-register)))
    (define (Tail x)
      (record-case x
        [(constant) (return x)]
        [(int)      (return x)]
        [else (error who "invalid tail ~s" x)]))
    ;;;
    (Tail x))
  ;;;
  (define (linearize x)
    (define who 'linearize)
    ;;;
    (define (op x)
      (record-case x
        [(reg r) r]
        [(constant c) `(obj ,c)]
        [(int i) i]
        [else (error who "invalid op ~s" x)]))
    ;;;
    (define (Effect x ac)
      (record-case x
        [(seq e0 e1)
         (Effect e0 (Effect e1 ac))]
        [(set targ v)
         (cons `(movl ,(op v) ,(op targ)) ac)]
        [else (error who "invalid effect ~s" x)]))
    ;;;
    (define (Tail x ac)
      (record-case x
        [(seq e0 e1)
         (Effect e0 (Tail e1 ac))]
        [(primcall op rands) 
         (case op
           [(return) 
            (cons '(ret) ac)]
           [else (error who "invalid tail prim ~s" op)])]
        [else (error who "invalid tail ~s" x)]))
    ;;;
    (list (cons 0 (Tail x '()))))
  ;;;
  (define (compile x)
    (let* ([x (expand x)]
           [x (recordize x)]
           [x (specify-representation x)]
           [x (impose-calling-convention x)]
           [x* (linearize x)]
           [code (car (#%list*->code* 
                         (lambda (x) #f)
                         x*))])
      ((#%$code->closure code))))
  (compile x))



(define-syntax add-tests-with-string-output 
  (syntax-rules (=>)
    [(_ name [expr* => str*] ...)
     (begin
       (printf "SECTION ~a ...\n" 'name)
       (let ([str str*]
             [expr 'expr*])
         (printf "testing ~s\n" expr)
         (let ([r (with-output-to-string 
                    (lambda ()
                      (write (racompile expr))
                      (newline)))])
           (unless (string=? r str)
             (error #f "expected ~s, got ~s\n" str r)))) 
       ...)]))

(load "tests/tests-1.1-req.scm")
(load "tests/tests-1.2-req.scm")

(printf "ALL IS GOOD :-)\n")
