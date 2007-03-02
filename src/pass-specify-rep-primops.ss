(define (prm op . arg*)
  (make-primcall op arg*))

(define (nop) (make-primcall 'nop '()))

(define (K x) (make-constant x))


(define (tag-test x mask tag)
  (if mask
      (prm '= (prm 'logand x (K mask)) (K tag))
      (prm '= x (K tag))))

(define (sec-tag-test x pmask ptag smask stag)
  (make-conditional 
    (tag-test x pmask ptag)
    (tag-test (prm 'mref x (K (- ptag))) smask stag)
    (make-constant #f)))

(define (safe-ref x disp mask tag)
  (seq*
    (interrupt-unless (tag-test x mask tag))
    (prm 'mref x (K (- disp tag)))))


(define (dirty-vector-set address)
  (prm 'mset 
     (prm 'int+
          (prm 'mref pcr (K 28)) ;;; FIXME: make srl
          (prm 'sll (prm 'sra address (K pageshift)) (K wordshift)))
     (K 0)
     (K dirty-word)))

(define (smart-dirty-vector-set addr what)
  (record-case what
    [(constant t) 
     (if (or (fixnum? t) (immediate? t))
         (prm 'nop)
         (dirty-vector-set addr))]
    [else (dirty-vector-set addr)]))

(define (mem-assign v x i)
  (with-tmp ([t (prm 'int+ x (K i))])
    (make-seq 
      (prm 'mset t (K 0) (T v))
      (dirty-vector-set t))))

(define (smart-mem-assign what v x i)
  (record-case what
    [(constant t) 
     (if (or (fixnum? t) (immediate? t))
         (prm 'mset x (K i) v)
         (mem-assign v x i))]
    [else (mem-assign v x i)]))


(define (align-code unknown-amt known-amt)
  (prm 'sll 
     (prm 'sra
          (prm 'int+ unknown-amt
               (K (+ known-amt (sub1 object-alignment))))
          (K align-shift))
     (K align-shift)))


(define-syntax section
  (syntax-rules (/section)
    [(section e* ... /section) (begin e* ...)]))

(section ;;; simple objects section

(define-primop void safe
  [(V) (K void-object)]
  [(P) (K #t)]
  [(E) (prm 'nop)])

(define-primop nop unsafe
  [(E) (prm 'nop)])

(define-primop neq? unsafe
  [(P x y) (prm '!= (T x) (T y))]
  [(E x y) (nop)])

(define-primop eq? safe
  [(P x y) (prm '= (T x) (T y))]
  [(E x y) (nop)])

(define-primop null? safe
  [(P x) (prm '= (T x) (K nil))]
  [(E x) (nop)])

(define-primop eof-object safe
  [(V) (K eof)]
  [(P) (K #t)]
  [(E) (nop)])

(define-primop eof-object? safe
  [(P x) (prm '= (T x) (K eof))]
  [(E x) (nop)])

(define-primop $unbound-object? unsafe
  [(P x) (prm '= (T x) (K unbound))]
  [(E x) (nop)])

(define-primop immediate? safe
  [(P x)
   (make-conditional
     (tag-test (T x) fixnum-mask fixnum-tag)
     (make-constant #t)
     (tag-test (T x) 7 7))]
  [(E x) (nop)])

(define-primop boolean? safe
  [(P x) (tag-test (T x) bool-mask bool-tag)]
  [(E x) (nop)])

(define-primop bwp-object? safe
  [(P x) (prm '= (T x) (K bwp-object))]
  [(E x) (nop)])

(define-primop $forward-ptr? unsafe
  [(P x) (prm '= (T x) (K -1))]
  [(E x) (nop)])

(define-primop pointer-value unsafe
  [(V x) (prm 'logand (T x) (K (* -1 fixnum-scale)))]
  [(P x) (K #t)]
  [(E x) (nop)])

(define-primop $arg-list unsafe
  [(V) (prm 'mref pcr (K 32))] ;; PCB ARGS-LIST
  [(P) (K #t)]
  [(E) (nop)])

/section)

(section ;;; pairs 

(define-primop pair? safe
  [(P x) (tag-test (T x) pair-mask pair-tag)]
  [(E x) (nop)])

(define-primop cons safe
  [(V a d)
   (with-tmp ([t (prm 'alloc (K pair-size) (K pair-tag))])
     (prm 'mset t (K (- disp-car pair-tag)) (T a))
     (prm 'mset t (K (- disp-cdr pair-tag)) (T d))
     t)]
  [(P a d) (K #t)]
  [(E a d) (prm 'nop)])

(define-primop $car unsafe
  [(V x) (prm 'mref  (T x) (K (- disp-car pair-tag)))]
  [(E x) (nop)])

(define-primop $cdr unsafe
  [(V x) (prm 'mref  (T x) (K (- disp-cdr pair-tag)))]
  [(E x) (nop)])

(define-primop $set-car! unsafe
  [(E x v)
   (with-tmp ([x (T x)])
     (prm 'mset x (K (- disp-car pair-tag)) (T v))
     (smart-dirty-vector-set x v))])

(define-primop $set-cdr! unsafe
  [(E x v)
   (with-tmp ([x (T x)])
     (prm 'mset x (K (- disp-cdr pair-tag)) (T v))
     (smart-dirty-vector-set x v))])

(define-primop car safe
  [(V x)
   (safe-ref (T x) disp-car pair-mask pair-tag)]
  [(E x)
   (interrupt-unless (tag-test (T x) pair-mask pair-tag))])

(define-primop cdr safe
  [(V x)
   (safe-ref (T x) disp-cdr pair-mask pair-tag)]
  [(E x)
   (interrupt-unless (tag-test (T x) pair-mask pair-tag))])

(define-primop set-car! safe
  [(E x v)
   (with-tmp ([x (T x)])
     (interrupt-unless (tag-test x pair-mask pair-tag))
     (prm 'mset x (K (- disp-car pair-tag)) (T v))
     (smart-dirty-vector-set x v))])

(define-primop set-cdr! safe
  [(E x v)
   (with-tmp ([x (T x)])
     (interrupt-unless (tag-test x pair-mask pair-tag))
     (prm 'mset x (K (- disp-cdr pair-tag)) (T v))
     (smart-dirty-vector-set x v))])

(define-primop list safe
  [(V) (K nil)]
  [(V . arg*)
   (let ([n (length arg*)] [t* (map T arg*)])
     (with-tmp ([v (prm 'alloc (K (align (* n pair-size))) (K pair-tag))])
       (prm 'mset v (K (- disp-car pair-tag)) (car t*))
       (prm 'mset v
            (K (- (+ disp-cdr (* (sub1 n) pair-size)) pair-tag))
            (K nil))
       (let f ([t* (cdr t*)] [i pair-size])
         (cond
           [(null? t*) v]
           [else
            (with-tmp ([tmp (prm 'int+ v (K i))])
              (prm 'mset tmp (K (- disp-car pair-tag)) (car t*))
              (prm 'mset tmp (K (+ disp-cdr (- pair-size) (- pair-tag))) tmp)
              (f (cdr t*) (+ i pair-size)))]))))]
  [(P . arg*) (K #t)]
  [(E . arg*) (nop)])


(define-primop list* safe
  [(V) (interrupt)]
  [(V x) (T x)]
  [(V a . a*)
   (let ([t* (map T a*)] [n (length a*)])
     (with-tmp ([v (prm 'alloc (K (* n pair-size)) (K pair-tag))])
       (prm 'mset v (K (- disp-car pair-tag)) (T a))
       (let f ([t* t*] [i pair-size])
         (cond
           [(null? (cdr t*)) 
            (seq* (prm 'mset v (K (- i disp-cdr pair-tag)) (car t*)) v)]
           [else
            (with-tmp ([tmp (prm 'int+ v (K i))])
              (prm 'mset tmp (K (- disp-car pair-tag)) (car t*))
              (prm 'mset tmp (K (- (- disp-cdr pair-tag) pair-size)) tmp)
              (f (cdr t*) (+ i pair-size)))]))))]
  [(P) (interrupt)]
  [(P x) (P x)]
  [(P a . a*) (K #t)]
  [(E) (interrupt)]
  [(E . a*) (nop)])


/section)

(section ;;; vectors
(section ;;; helpers
  (define (vector-range-check x idx)
    (define (check-fx i)
      (seq*
         (interrupt-unless (tag-test (T x) vector-mask vector-tag))
         (with-tmp ([len (cogen-value-$vector-length x)])
           (interrupt-unless (prm 'u< (K (* i wordsize)) len))
           (interrupt-unless-fixnum len))))
    (define (check-? idx)
      (seq*
        (interrupt-unless (tag-test (T x) vector-mask vector-tag))
        (with-tmp ([len (cogen-value-$vector-length x)])
          (interrupt-unless (prm 'u< (T idx) len))
          (with-tmp ([t (prm 'logor len (T idx))])
            (interrupt-unless-fixnum t)))))
    (record-case idx
      [(constant i)
       (if (and (fixnum? i) (fx>= i 0)) 
           (check-fx i)
           (check-? idx))]
      [else (check-? idx)]))
  /section)

(define-primop vector? unsafe
  [(P x) (sec-tag-test (T x) vector-mask vector-tag fixnum-mask fixnum-tag)]
  [(E x) (nop)])

(define-primop $make-vector unsafe
  [(V len)
   (record-case len
     [(constant i)
      (unless (fixnum? i) (interrupt))
      (with-tmp ([v (prm 'alloc
                        (K (align (+ (* i wordsize) disp-vector-data)))
                        (K vector-tag))])
          (prm 'mset v 
               (K (- disp-vector-length vector-tag))
               (K (make-constant (* i fixnum-scale))))
          v)]
     [else
      (with-tmp ([alen (align-code (T len) disp-vector-data)])
        (with-tmp ([v (prm 'alloc alen (K vector-tag))])
            (prm 'mset v (K (- disp-vector-length vector-tag)) (T len))
            v))])]
  [(P len) (K #t)]
  [(E len) (nop)])

(define-primop $vector-ref unsafe
  [(V x i)
   (or 
     (record-case i
       [(constant i) 
        (and (fixnum? i) 
             (fx>= i 0)
             (prm 'mref (T x) 
                  (K (+ (* i wordsize) (- disp-vector-data vector-tag)))))]
       [else #f])
        (prm 'mref (T x) 
           (prm 'int+ (T i) (K (- disp-vector-data vector-tag)))))])

(define-primop $vector-length unsafe
  [(V x) (prm 'mref (T x) (K (- disp-vector-length vector-tag)))]
  [(E x) (prm 'nop)]
  [(P x) (K #t)])

(define-primop vector-length safe
  [(V x)
   (seq*
     (interrupt-unless (tag-test (T x) vector-mask vector-tag))
     (with-tmp ([t (cogen-value-$vector-length x)])
       (interrupt-unless-fixnum t)
       t))]
  [(E x)
   (seq*
     (interrupt-unless (tag-test (T x) vector-mask vector-tag))
     (with-tmp ([t (cogen-value-$vector-length x)])
       (interrupt-unless-fixnum t)))]
  [(P x) 
   (seq* (cogen-effect-vector-length x) (K #t))])

(define-primop vector-ref safe
  [(V x i)
   (seq*
     (vector-range-check x i)
     (cogen-value-$vector-ref x i))]
  [(E x i)
   (vector-range-check x i)])


(define-primop $vector-set! unsafe
  [(E x i v)
   (record-case i
     [(constant i) 
      (unless (fixnum? i) (interrupt)) 
      (mem-assign v (T x) 
         (+ (* i wordsize)
            (- disp-vector-data vector-tag)))]
     [else
      (mem-assign v 
         (prm 'int+ (T x) (T i))
         (- disp-vector-data vector-tag))])])

#;(define-primop vector-set! safe
  [(E x i v)
   (seq*
     (vector-range-check x i)
     (cogen-effect-$vector-set! x i v))])

(define-primop vector safe
  [(V . arg*)
   (with-tmp ([v (prm 'alloc
                   (K (align (+ disp-vector-data
                                (* (length arg*) wordsize))))
                   (K vector-tag))])
     (seq*
       (prm 'mset v (K (- disp-vector-length vector-tag))
            (K (* (length arg*) wordsize)))
       (let f ([t* (map T arg*)]
               [i (- disp-vector-data vector-tag)])
         (cond
           [(null? t*) v]
           [else
            (make-seq
              (prm 'mset v (K i) (car t*))
              (f (cdr t*) (+ i wordsize)))]))))]
  [(E . arg*) (prm 'nop)]
  [(P . arg*) (K #t)])

/section)

(section ;;; closures

(define-primop procedure? safe
  [(P x) (tag-test (T x) closure-mask closure-tag)])

(define-primop $cpref unsafe
  [(V x i) 
   (record-case i
     [(constant i) 
      (unless (fixnum? i) (interrupt))
      (prm 'mref (T x)
         (K (+ (- disp-closure-data closure-tag)
               (* i wordsize))))]
     [else (interrupt)])])

/section)

(section ;;; symbols

(define-primop symbol? safe
  [(P x) (tag-test (T x) symbol-mask symbol-tag)]
  [(E x) (nop)])

(define-primop $make-symbol unsafe
  [(V str)
   (with-tmp ([x (prm 'alloc (K (align symbol-size)) (K symbol-tag))])
     (prm 'mset x (K (- disp-symbol-string symbol-tag)) (T str))
     (prm 'mset x (K (- disp-symbol-unique-string symbol-tag)) (K 0))
     (prm 'mset x (K (- disp-symbol-value symbol-tag)) (K unbound))
     (prm 'mset x (K (- disp-symbol-plist symbol-tag)) (K nil))
     (prm 'mset x (K (- disp-symbol-system-value symbol-tag)) (K unbound))
     (prm 'mset x (K (- disp-symbol-function symbol-tag)) (K 0))
     (prm 'mset x (K (- disp-symbol-error-function symbol-tag)) (K 0)) 
     (prm 'mset x (K (- disp-symbol-unused symbol-tag)) (K 0))
     x)]
  [(P str) (K #t)]
  [(E str) (nop)])

(define-primop primitive-set! unsafe
  [(E x v) (mem-assign v (T x) (- disp-symbol-system-value symbol-tag))])

(define-primop primitive-ref unsafe
  [(V x) (prm 'mref (T x) (K (- disp-symbol-system-value symbol-tag)))]
  [(E x) (nop)])

(define-primop $symbol-string unsafe
  [(V x) (prm 'mref (T x) (K (- disp-symbol-string symbol-tag)))]
  [(E x) (nop)])

(define-primop $set-symbol-string! unsafe
  [(E x v) (mem-assign v (T x) (- disp-symbol-string symbol-tag))])

(define-primop $symbol-unique-string unsafe
  [(V x) (prm 'mref (T x) (K (- disp-symbol-unique-string symbol-tag)))]
  [(E x) (nop)])

(define-primop $set-symbol-unique-string! unsafe
  [(E x v) (mem-assign v (T x) (- disp-symbol-unique-string symbol-tag))])

(define-primop $symbol-plist unsafe
  [(V x) (prm 'mref (T x) (K (- disp-symbol-plist symbol-tag)))]
  [(E x) (nop)])

(define-primop $set-symbol-plist! unsafe
  [(E x v) (mem-assign v (T x) (- disp-symbol-plist symbol-tag))])

(define-primop $symbol-value unsafe
  [(V x) (prm 'mref (T x) (K (- disp-symbol-value symbol-tag)))]
  [(E x) (nop)])

(define-primop $set-symbol-value! unsafe
  [(E x v)
   (with-tmp ([x (T x)])
     (prm 'mset x (K (- disp-symbol-value symbol-tag)) (T v))
     (prm 'mset x (K (- disp-symbol-function symbol-tag))
          (prm 'mref x (K (- disp-symbol-error-function symbol-tag))))
     (dirty-vector-set x))])

(define-primop top-level-value safe
  [(V x)
   (record-case x
     [(constant s)
      (if (symbol? s)
          (with-tmp ([v (cogen-value-$symbol-value x)])
            (interrupt-when (cogen-pred-$unbound-object? v))
            v)
          (interrupt))]
     [else
      (with-tmp ([x (T x)])
        (interrupt-unless (cogen-pred-symbol? x))
        (with-tmp ([v (cogen-value-$symbol-value x)])
          (interrupt-when (cogen-pred-$unbound-object? v))
          v))])]
  [(E x)
   (record-case x
     [(constant s)
      (if (symbol? s)
          (with-tmp ([v (cogen-value-$symbol-value x)])
            (interrupt-when (cogen-pred-$unbound-object? v)))
          (interrupt))]
     [else
      (with-tmp ([x (T x)])
        (interrupt-unless (cogen-pred-symbol? x))
        (with-tmp ([v (cogen-value-$symbol-value x)])
          (interrupt-when (cogen-pred-$unbound-object? v))))])])

/section)

(section ;;; fixnums

(define-primop fixnum? safe
  [(P x) (tag-test (T x) fixnum-mask fixnum-tag)]
  [(E x) (nop)])

(define-primop $fxzero? unsafe
  [(P x) (prm '= (T x) (K 0))]
  [(E x) (nop)])

(define-primop $fx= unsafe
  [(P x y) (prm '= (T x) (T y))]
  [(E x y) (nop)])

(define-primop $fx< unsafe
  [(P x y) (prm '< (T x) (T y))]
  [(E x y) (nop)])

(define-primop $fx<= unsafe
  [(P x y) (prm '<= (T x) (T y))]
  [(E x y) (nop)])

(define-primop $fx> unsafe
  [(P x y) (prm '> (T x) (T y))]
  [(E x y) (nop)])

(define-primop $fx>= unsafe
  [(P x y) (prm '>= (T x) (T y))]
  [(E x y) (nop)])

(define-primop $fxadd1 unsafe
  [(V x) (cogen-value-$fx+ x (K 1))]
  [(P x) (K #t)]
  [(E x) (nop)])

(define-primop $fxsub1 unsafe
  [(V x) (cogen-value-$fx+ x (K -1))]
  [(P x) (K #t)]
  [(E x) (nop)])

(define-primop $fx+ unsafe
  [(V x y) (prm 'int+ (T x) (T y))]
  [(P x y) (K #t)]
  [(E x y) (nop)])

(define-primop $fx* unsafe
  [(V a b) 
   (record-case a
    [(constant a)
     (unless (fixnum? a) (interrupt))
     (prm 'int* (T b) (K a))]
    [else
     (record-case b
       [(constant b)
        (unless (fixnum? b) (interrupt))
        (prm 'int* (T a) (K b))]
       [else
        (prm 'int* (T a) (prm 'sra (T b) (K fixnum-shift)))])])]
  [(P x y) (K #t)]
  [(E x y) (nop)])

(define-primop $fxlognot unsafe
  [(V x) (cogen-value-$fxlogxor x (K -1))]
  [(P x) (K #t)]
  [(E x) (nop)])

(define-primop $fxlogand unsafe
  [(V x y) (prm 'logand (T x) (T y))]
  [(P x y) (K #t)]
  [(E x y) (nop)])

(define-primop $fxlogor unsafe
  [(V x y) (prm 'logor (T x) (T y))]
  [(P x y) (K #t)]
  [(E x y) (nop)])

(define-primop $fxlogxor unsafe
  [(V x y) (prm 'logxor (T x) (T y))]
  [(P x y) (K #t)]
  [(E x y) (nop)])

(define-primop $fx- unsafe
  [(V x y) (prm 'int- (T x) (T y))]
  [(P x y) (K #t)]
  [(E x y) (nop)])

(define-primop $fxsll unsafe
  [(V x i)
   (record-case i
     [(constant i) 
      (unless (fixnum? i) (interrupt))
      (prm 'sll (T x) (K i))]
     [else 
      (prm 'sll (T x) (prm 'sra (T i) (K fixnum-shift)))])]
  [(P x i) (K #t)]
  [(E x i) (nop)])

(define-primop $fxsra unsafe
  [(V x i)
   (record-case i
     [(constant i) 
      (unless (fixnum? i) (interrupt))
      (prm 'logand (prm 'sra (T x) (K i)) (K (* -1 fixnum-scale)))]
     [else 
      (prm 'logand
           (prm 'sra (T x) (prm 'sra (T i) (K fixnum-shift)))
           (K (* -1 fixnum-scale)))])]
  [(P x i) (K #t)]
  [(E x i) (nop)])

(define-primop $fxquotient unsafe
  [(V a b) 
   (with-tmp ([b (T b)])
    (prm 'sll (prm 'remainder (T a) b) (K fixnum-shift)))]
  [(P a b) (K #t)]
  [(E a b) (nop)])

(define-primop $fxmodulo unsafe
  [(V a b)
   (with-tmp ([b (T b)])
     (with-tmp ([c (prm 'logand b 
                      (prm 'sra (prm 'logxor b (T a))
                         (K (sub1 (* 8 wordsize)))))])
       (prm 'int+ c (prm 'quotient (T a) b))))]
  [(P a b) (K #t)]
  [(E a b) (nop)])

/section)

(section ;;; numbers

(define (non-fixnum? x)
  (record-case x
    [(constant i) (not (fixnum? i))]
    [else #f]))

(define (or* a a*)
  (cond
    [(null? a*) a]
    [else (or* (prm 'logor a (T (car a*))) (cdr a*))]))

(define (assert-fixnums a a*)
  (interrupt-unless (tag-test (or* (T a) a*) fixnum-mask fixnum-tag)))

(define (fold-p op a a*)
  (cond
    [(or (non-fixnum? a) (ormap non-fixnum? a*)) (interrupt)]
    [else
     (seq*
       (assert-fixnums a a*)
       (let f ([a a] [a* a*])
         (cond
           [(null? a*) (K #t)]
           [else
            (let ([b (car a*)])
              (make-conditional
                (prm op (T a) (T b))
                (f b (cdr a*))
                (K #f)))])))]))

(define (fold-e a a*)
  (cond
    [(or (non-fixnum? a) (ormap non-fixnum? a*)) (interrupt)]
    [else (assert-fixnums a a*)]))

(define-primop = safe
  [(P) (interrupt)]
  [(P a . a*) (fold-p '= a a*)]
  [(E) (interrupt)]
  [(E a . a*) (fold-e a a*)])

(define-primop < safe
  [(P) (interrupt)]
  [(P a . a*) (fold-p '< a a*)]
  [(E) (interrupt)]
  [(E a . a*) (fold-e a a*)])

(define-primop <= safe
  [(P) (interrupt)]
  [(P a . a*) (fold-p '<= a a*)]
  [(E) (interrupt)]
  [(E a . a*) (fold-e a a*)])

(define-primop > safe
  [(P) (interrupt)]
  [(P a . a*) (fold-p '> a a*)]
  [(E) (interrupt)]
  [(E a . a*) (fold-e a a*)])

(define-primop >= safe
  [(P) (interrupt)]
  [(P a . a*) (fold-p '>= a a*)]
  [(E) (interrupt)]
  [(E a . a*) (fold-e a a*)])

(define-primop - safe
  [(V a) 
   (cond
     [(non-fixnum? a) (interrupt)]
     [else
      (seq*
        (assert-fixnums a '())
        (prm 'int-/overflow (K 0) (T a)))])]
  [(V a . a*)
   (cond
     [(or (non-fixnum? a) (ormap non-fixnum? a*)) (interrupt)]
     [else
      (seq*
        (assert-fixnums a a*)
        (let f ([a (T a)] [a* a*])
          (cond
            [(null? a*) a]
            [else
             (f (prm 'int-/overflow a (T (car a*))) (cdr a*))])))])]
  [(P a . a*) (seq* (assert-fixnums a a*) (K #t))]
  [(E a . a*) (assert-fixnums a a*)])

(define-primop + safe
  [(V) (K 0)]
  [(V a . a*)
   (cond
     [(or (non-fixnum? a) (ormap non-fixnum? a*)) (interrupt)]
     [else
      (seq*
        (assert-fixnums a a*)
        (let f ([a (T a)] [a* a*])
          (cond
            [(null? a*) a]
            [else
             (f (prm 'int+/overflow a (T (car a*))) (cdr a*))])))])]
  [(P) (K #t)]
  [(P a . a*) (seq* (assert-fixnums a a*) (K #t))]
  [(E) (nop)]
  [(E a . a*) (assert-fixnums a a*)])


/section)

(section ;;; records

(define-primop $record? unsafe
  [(P x) (sec-tag-test (T x) vector-mask vector-tag vector-mask vector-tag)]
  [(E x) (nop)])

#;(define-primop $record/rtd? unsafe
  [(P x rtd)
   (make-conditional
     (tag-test (T x) vector-mask vector-tag)
     (prm '= (prm 'mref (T x) (K (- vector-tag))) (T rtd))
     (make-constant #f))]
  [(E x rtd) (nop)])

(define-primop $make-record unsafe
  [(V rtd len)
   (record-case len
     [(constant i) 
      (unless (fixnum? i) (interrupt))
      (with-tmp ([t (prm 'alloc
                         (K (align (+ (* i wordsize) disp-record-data)))
                         (K vector-tag))])
        (prm 'mset t (K (- disp-record-rtd vector-tag)) (T rtd))
        t)]
     [else
      (with-tmp ([ln (align-code len disp-record-data)])
        (with-tmp ([t (prm 'alloc ln (K vector-tag))])
          (prm 'mset t (K (- disp-record-rtd vector-tag)) (T rtd))
           t))])]
  [(P rtd len) (K #t)]
  [(E rtd len) (nop)])

(define-primop $record-rtd unsafe
  [(V x) 
   (prm 'mref (T x) (K (- disp-record-rtd vector-tag)))]
  [(E x) (nop)]
  [(P x) #t])

(define-primop $record-ref unsafe
  [(V x i) (cogen-value-$vector-ref x i)]
  [(E x i) (cogen-effect-$vector-ref x i)]
  [(P x i) (cogen-pred-$vector-ref x i)])

(define-primop $record-set! unsafe
  [(V x i v) (cogen-value-$vector-set! x i v)]
  [(E x i v) (cogen-effect-$vector-set! x i v)]
  [(P x i v) (cogen-pred-$vector-set! x i v)])

(define-primop $record unsafe
  [(V rtd . v*)
   (with-tmp ([t (prm 'alloc 
                     (K (align
                          (+ disp-record-data
                            (* (length v*) wordsize))))
                     (K vector-tag))])
     (prm 'mset t (K (- disp-record-rtd vector-tag)) (T rtd))
     (let f ([v* v*] 
             [i (- disp-record-data vector-tag)])
       (cond
         [(null? v*) t]
         [else
          (make-seq 
            (prm 'mset t (K i) (T (car v*)))
            (f (cdr v*) (+ i wordsize)))])))]
  [(P rtd . v*) (K #t)]
  [(E rtd . v*) (nop)])

/section)

(section ;;; characters

(define-primop char? safe
  [(P x) (tag-test (T x) char-mask char-tag)]
  [(E x) (nop)])

(define-primop $char= unsafe
  [(P x y) (prm '= (T x) (T y))]
  [(E x y) (nop)])

(define-primop $char< unsafe
  [(P x y) (prm '< (T x) (T y))]
  [(E x y) (nop)])

(define-primop $char<= unsafe
  [(P x y) (prm '<= (T x) (T y))]
  [(E x y) (nop)])

(define-primop $char> unsafe
  [(P x y) (prm '> (T x) (T y))]
  [(E x y) (nop)])

(define-primop $char>= unsafe
  [(P x y) (prm '>= (T x) (T y))]
  [(E x y) (nop)])

(define-primop $fixnum->char unsafe
  [(V x) 
   (prm 'logor
        (prm 'sll (T x) (K (- char-shift fixnum-shift)))
        (K char-tag))]
  [(P x) (K #t)]
  [(E x) (nop)])

(define-primop $char->fixnum unsafe
  [(V x) (prm 'sra (T x) (K (- char-shift fixnum-shift)))]
  [(P x) (K #t)]
  [(E x) (nop)])

/section)

(section ;;; strings
         
(define-primop string? safe
  [(P x) (tag-test (T x) string-mask string-tag)]
  [(E x) (nop)])

(define-primop $make-string unsafe
  [(V n)
   (record-case n
     [(constant n)
      (unless (fixnum? n) (interrupt))
      (with-tmp ([s (prm 'alloc 
                      (K (align (+ n 1 disp-string-data)))
                      (K string-tag))])
         (prm 'mset s
             (K (- disp-string-length string-tag))
             (K (* n fixnum-scale)))
         (prm 'bset/c s
             (K (+ n (- disp-string-data string-tag)))
             (K 0))
         s)]
     [else
      (with-tmp ([s (prm 'alloc 
                      (align-code 
                        (prm 'sra (T n) (K fixnum-shift))
                        (+ disp-string-data 1))
                      (K string-tag))])
          (prm 'mset s
            (K (- disp-string-length string-tag))
            (T n))
          (prm 'bset/c s
               (prm 'int+ 
                    (prm 'sra (T n) (K fixnum-shift))
                    (K (- disp-string-data string-tag)))
               (K 0))
          s)])]
  [(P n) (K #t)]
  [(E n) (nop)])

(define-primop $string-length unsafe
  [(V x) (prm 'mref (T x) (K (- disp-string-length string-tag)))]
  [(P x) (K #t)]
  [(E x) (nop)])


(define-primop $string-ref unsafe
  [(V s i)
   (record-case i
     [(constant i)
      (unless (fixnum? i) (interrupt))
      (prm 'logor
        (prm 'sll
          (prm 'logand 
             (prm 'mref (T s)
               (K (+ i (- disp-string-data string-tag))))
             (K 255))
          (K char-shift))
        (K char-tag))]
     [else
      (prm 'logor
        (prm 'sll
          (prm 'srl ;;; FIXME: bref
             (prm 'mref (T s)
                  (prm 'int+
                     (prm 'sra (T i) (K fixnum-shift))
                     ;;; ENDIANNESS DEPENDENCY
                     (K (- disp-string-data 
                           (- wordsize 1) 
                           string-tag))))
             (K (* (- wordsize 1) 8)))
          (K char-shift))
        (K char-tag))])]
  [(P s i) (K #t)]
  [(E s i) (nop)])

(define (assert-fixnum x)
  (record-case x
    [(constant i) 
     (if (fixnum? i) (nop) (interrupt))]
    [else (interrupt-unless (cogen-pred-fixnum? x))]))

(define (assert-string x)
  (record-case x
    [(constant s) (if (string? s) (nop) (interrupt))]
    [else (interrupt-unless (cogen-pred-string? x))]))

(define-primop string-ref safe
  [(V s i)
   (seq*
     (assert-fixnum i)
     (assert-string s)
     (interrupt-unless (prm 'u< (T i) (cogen-value-$string-length s)))
     (cogen-value-$string-ref s i))]
  [(P s i)
   (seq*
     (assert-fixnum i)
     (assert-string s)
     (interrupt-unless (prm 'u< (T i) (cogen-value-$string-length s)))
     (K #t))]
  [(E s i)
   (seq*
     (assert-fixnum i)
     (assert-string s)
     (interrupt-unless (prm 'u< (T i) (cogen-value-$string-length s))))])


(define-primop $string-set! unsafe
  [(E x i c)
   (record-case i
     [(constant i) 
      (unless (fixnum? i) (interrupt))
      (record-case c
        [(constant c)
         (unless (char? c) (interrupt))
         (prm 'bset/c (T x) 
              (K (+ i (- disp-string-data string-tag)))
              (K (char->integer c)))]
        [else
         (unless (= char-shift 8) (error 'cogen-$string-set! "BUG"))
         (prm 'bset/h (T x)
               (K (+ i (- disp-string-data string-tag)))
               (T c))])]
     [else
      (record-case c
        [(constant c)
         (unless (char? c) (interrupt))
         (prm 'bset/c (T x) 
              (prm 'int+ 
                   (prm 'sra (T i) (K fixnum-shift))
                   (K (- disp-string-data string-tag)))
              (K (char->integer c)))]
        [else
         (unless (= char-shift 8) (error 'cogen-$string-set! "BUG"))
         (prm 'bset/h (T x)
               (prm 'int+ 
                    (prm 'sra (T i) (K fixnum-shift))
                    (K (- disp-string-data string-tag)))
               (T c))])])])

/section)

(section ;;; ports

(define-primop port? safe
  [(P x) (sec-tag-test (T x) vector-mask vector-tag port-mask port-tag)]
  [(E x) (nop)])

(define-primop input-port? safe
  [(P x) (sec-tag-test (T x) vector-mask vector-tag #f input-port-tag)]
  [(E x) (nop)])

(define-primop output-port? safe
  [(P x) (sec-tag-test (T x) vector-mask vector-tag #f output-port-tag)]
  [(E x) (nop)])

(define (make-port handler buf/i idx/i sz/i buf/o idx/o sz/o tag)
  (with-tmp ([p (prm 'alloc (K (align port-size)) (K vector-tag))])
    (prm 'mset p (K (- vector-tag)) (K tag))
    (prm 'mset p (K (- disp-port-handler vector-tag)) (T handler))
    (prm 'mset p (K (- disp-port-input-buffer vector-tag)) (T buf/i))
    (prm 'mset p (K (- disp-port-input-index vector-tag)) (T idx/i))
    (prm 'mset p (K (- disp-port-input-size vector-tag)) (T sz/i))
    (prm 'mset p (K (- disp-port-output-buffer vector-tag)) (T buf/o))
    (prm 'mset p (K (- disp-port-output-index vector-tag)) (T idx/o))
    (prm 'mset p (K (- disp-port-output-size vector-tag)) (T sz/o))
    p))

(define-primop $make-port/input unsafe
  [(V handler buf/i idx/i sz/i buf/o idx/o sz/o) 
   (make-port handler buf/i idx/i sz/i buf/o idx/o sz/o
              input-port-tag)])
(define-primop $make-port/output unsafe
  [(V handler buf/i idx/i sz/i buf/o idx/o sz/o) 
   (make-port handler buf/i idx/i sz/i buf/o idx/o sz/o
              output-port-tag)])
(define-primop $make-port/both unsafe
  [(V handler buf/i idx/i sz/i buf/o idx/o sz/o) 
   (make-port handler buf/i idx/i sz/i buf/o idx/o sz/o
              input/output-port-tag)])

(define-primop $port-handler unsafe
  [(V x) (prm 'mref (T x) (K (- disp-port-handler vector-tag)))])
(define-primop $port-input-buffer unsafe
  [(V x) (prm 'mref (T x) (K (- disp-port-input-buffer vector-tag)))])
(define-primop $port-input-index unsafe
  [(V x) (prm 'mref (T x) (K (- disp-port-input-index vector-tag)))])
(define-primop $port-input-size unsafe
  [(V x) (prm 'mref (T x) (K (- disp-port-input-size vector-tag)))])
(define-primop $port-output-buffer unsafe
  [(V x) (prm 'mref (T x) (K (- disp-port-output-buffer vector-tag)))])
(define-primop $port-output-index unsafe
  [(V x) (prm 'mref (T x) (K (- disp-port-output-index vector-tag)))])
(define-primop $port-output-size unsafe
  [(V x) (prm 'mref (T x) (K (- disp-port-output-size vector-tag)))])

(define-primop $set-port-input-index! unsafe
  [(E x i) (prm 'mset (T x) (K (- disp-port-input-index vector-tag)) (T i))])
(define-primop $set-port-output-index! unsafe
  [(E x i) (prm 'mset (T x) (K (- disp-port-output-index vector-tag)) (T i))])

(define-primop $set-port-input-size! unsafe
  [(E x i) 
   (seq*
     (prm 'mset (T x) (K (- disp-port-input-index vector-tag)) (K 0))
     (prm 'mset (T x) (K (- disp-port-input-size vector-tag)) (T i)))])
(define-primop $set-port-output-size! unsafe
  [(E x i) 
   (seq*
     (prm 'mset (T x) (K (- disp-port-output-index vector-tag)) (K 0))
     (prm 'mset (T x) (K (- disp-port-output-size vector-tag)) (T i)))])



/section)

(section ;;; interrupts-and-engines

(define-primop $interrupted? unsafe
  [(P) (prm '!= (prm 'mref pcr (K 40)) (K 0))])

(define-primop $unset-interrupted! unsafe
  [(E) (prm 'mset pcr (K 40) (K 0))])

/section)

(section ;;; control operations

(define-primop $fp-at-base unsafe
  [(P) ;;; PCB FRAME-BASE
   (prm '= (prm 'int+ (prm 'mref pcr (K 12)) (K (- wordsize))) fpr)])

(define-primop $current-frame unsafe
  [(V) (prm 'mref pcr (K 20))]) ;; PCB NEXT-CONTINUATION


(define-primop $seal-frame-and-call unsafe
  [(V x) ;;; PCB NEXT CONT;;; PCB BASE
   (with-tmp ([k (prm 'alloc (K continuation-size) (K vector-tag))])
     (with-tmp ([base (prm 'int+ (prm 'mref pcr (K 12)) (K (- wordsize)))])
       (with-tmp ([underflow-handler (prm 'mref base (K 0))])
         (prm 'mset k (K (- vector-tag)) (K continuation-tag))
         (prm 'mset k (K (- disp-continuation-top vector-tag)) fpr)
         (prm 'mset k (K (- disp-continuation-next vector-tag)) 
              (prm 'mref pcr (K 20))) 
         (prm 'mset k (K (- disp-continuation-size vector-tag)) (prm 'int- base fpr))
         (prm 'mset pcr (K 20) k)
         (prm 'mset pcr (K 12) fpr)
         (prm '$call-with-underflow-handler underflow-handler (T x) k))))]
  [(E . args) (interrupt)]
  [(P . args) (interrupt)])

(define-primop $frame->continuation unsafe
  [(V x)
   (with-tmp ([t (prm 'alloc
                    (K (align (+ disp-closure-data wordsize)))
                    (K closure-tag))])
     (prm 'mset t (K (- disp-closure-code closure-tag))
          (K (make-code-loc SL_continuation_code)))
     (prm 'mset t (K (- disp-closure-data closure-tag))
          (T x))
     t)]
  [(P x) (K #t)]
  [(E x) (nop)])

(define-primop $make-call-with-values-procedure unsafe
  [(V) (K (make-closure (make-code-loc SL_call_with_values) '()))]
  [(P) (interrupt)]
  [(E) (interrupt)])

(define-primop $make-values-procedure unsafe
  [(V) (K (make-closure (make-code-loc SL_values) '()))]
  [(P) (interrupt)]
  [(E) (interrupt)])



/section)

(section ;;; hash table tcbuckets

(define-primop $make-tcbucket unsafe
  [(V tconc key val next)
   (with-tmp ([x (prm 'alloc (K (align tcbucket-size)) (K vector-tag))])
     (prm 'mset x (K (- disp-tcbucket-tconc vector-tag)) (T tconc))
     (prm 'mset x (K (- disp-tcbucket-key vector-tag)) (T key))
     (prm 'mset x (K (- disp-tcbucket-val vector-tag)) (T val))
     (prm 'mset x (K (- disp-tcbucket-next vector-tag)) (T next))
     x)])

(define-primop $tcbucket-key unsafe
  [(V x) (prm 'mref (T x) (K (- disp-tcbucket-key vector-tag)))])
(define-primop $tcbucket-val unsafe
  [(V x) (prm 'mref (T x) (K (- disp-tcbucket-val vector-tag)))])
(define-primop $tcbucket-next unsafe
  [(V x) (prm 'mref (T x) (K (- disp-tcbucket-next vector-tag)))])

(define-primop $set-tcbucket-key! unsafe
  [(E x v) (mem-assign v (T x) (- disp-tcbucket-key vector-tag))])
(define-primop $set-tcbucket-val! unsafe
  [(E x v) (mem-assign v (T x) (- disp-tcbucket-val vector-tag))])
(define-primop $set-tcbucket-next! unsafe
  [(E x v) (mem-assign v (T x) (- disp-tcbucket-next vector-tag))])
(define-primop $set-tcbucket-tconc! unsafe
  [(E x v) (mem-assign v (T x) (- disp-tcbucket-tconc vector-tag))])


/section)

(section ;;; codes

(define-primop $code? unsafe
  [(P x) (sec-tag-test (T x) vector-mask vector-tag #f code-tag)])

(define-primop $closure-code unsafe
  [(V x) 
   (prm 'int+ 
        (prm 'mref (T x) (K (- disp-closure-code closure-tag)))
        (K (- vector-tag disp-code-data)))])

(define-primop $code-freevars unsafe
  [(V x) (prm 'mref (T x) (K (- disp-code-freevars vector-tag)))])

(define-primop $code-reloc-vector unsafe
  [(V x) (prm 'mref (T x) (K (- disp-code-relocsize vector-tag)))])

(define-primop $code-size unsafe
  [(V x) (prm 'mref (T x) (K (- disp-code-instrsize vector-tag)))])

(define-primop $code->closure unsafe
  [(V x) 
   (with-tmp ([v (prm 'alloc
                    (K (align (+ 0 disp-closure-data)))
                    (K closure-tag))])
     (prm 'mset v 
          (K (- disp-closure-code closure-tag))
          (prm 'int+ (T x) 
            (K (- disp-code-data vector-tag))))
     v)])

(define-primop $code-ref unsafe
  [(V x i) 
   (prm 'sll
     (prm 'logand
          (prm 'mref (T x)
               (prm 'int+
                    (prm 'sra (T i) (K fixnum-shift))
                    (K (- disp-code-data vector-tag))))
          (K 255))
     (K fixnum-shift))])

(define-primop $code-set! unsafe
  [(E x i v)
   (prm 'bset/h (T x)
        (prm 'int+ 
             (prm 'sra (T i) (K fixnum-shift))
             (K (- disp-code-data vector-tag)))
        (prm 'sll (T v) (K (- 8 fixnum-shift))))])


/section)

#!eof


         [($init-symbol-function!)
          (tbind ([x (Value (car arg*))] [v (Value (cadr arg*))])
            (seq*
              (prm 'mset x (K (- disp-symbol-function symbol-tag)) v)
              (prm 'mset x (K (- disp-symbol-error-function symbol-tag)) v)
              (dirty-vector-set x)))]

         [(zero?) 
          (tbind ([x (Value (car arg*))])
            (make-conditional
              (tag-test x fixnum-mask fixnum-tag)
              (prm '= x (K 0))
              (prm '!=
                   (make-funcall (Value (make-primref 'zero?)) (list x))
                   (Value (K #f)))))]
         [($procedure-check)
          (tbind ([x (Value (car arg*))])
            (make-shortcut
              (make-seq
                (make-conditional 
                  (tag-test x closure-mask closure-tag)
                  (prm 'nop)
                  (prm 'interrupt))
                x)
              (Value 
                (make-funcall (make-primref 'error)
                  (list (make-constant 'apply)
                        (make-constant "~s is not a procedure")
                        x)))))]




















(include "libprimops.ss")

(define (specify-representation x)
  (define who 'specify-representation)
  ;;;
  (define fixnum-scale 4)
  (define fixnum-shift 2)
  (define fixnum-tag 0)
  (define fixnum-mask 3)
  (define pcb-dirty-vector-offset 28)
  ;;;
  (define nop (make-primcall 'nop '()))
  ;;;
  (define (Effect x)
       ]
      [(forcall op arg*)
       (make-forcall op (map Value arg*))]
      [(funcall rator arg*)
       (make-funcall (Function rator) (map Value arg*))]
      [(jmpcall label rator arg*)
       (make-jmpcall label (Value rator) (map Value arg*))]
      [(mvcall rator x)
       (make-mvcall (Value rator) (Clambda x Effect))]
      [else (error who "invalid effect expr ~s" x)]))
  ;;;
  ;;;
  ;;;
  ;;;
  ;;; value
  ;;;
  (define (ClambdaCase x k)
    (record-case x
      [(clambda-case info body)
       (make-clambda-case info (k body))]
      [else (error who "invalid clambda-case ~s" x)]))
  ;;;
  (define (Clambda x k)
    (record-case x
      [(clambda label case* free*)
       (make-clambda label 
          (map (lambda (x) (ClambdaCase x k)) case*)
          free*)]
      [else (error who "invalid clambda ~s" x)]))
  ;;;
  (define (error-codes)
    (define (code-list symbol)
      (define L1 (gensym))
      (define L2 (gensym))
      `(0
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) ,cp-register]
        [andl ,closure-mask ,cp-register]
        [cmpl ,closure-tag ,cp-register]
        [jne (label ,L1)]
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) ,cp-register]
        [movl ,cp-register (disp ,(- disp-symbol-function symbol-tag) (obj ,symbol))]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]
        [label ,L1]
        [movl (disp ,(- disp-symbol-value symbol-tag) (obj ,symbol)) %eax]
        [cmpl ,unbound %eax]
        [je (label ,L2)]
        [movl (obj apply) (disp -4 %esp)]
        [movl (obj "~s is not a procedure") (disp -8 %esp)]
        [movl %eax (disp -12 %esp)]
        [movl (obj error) ,cp-register]
        [movl (disp ,(- disp-symbol-system-value symbol-tag)
                    ,cp-register) ,cp-register]
        [movl ,(argc-convention 3) %eax]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]
        [label ,L2]
        [movl (obj ,symbol) (disp -4 %esp)]
        [movl (obj top-level-value) ,cp-register]
        [movl (disp ,(- disp-symbol-system-value symbol-tag)
                    ,cp-register) ,cp-register]
        [movl ,(argc-convention 1) %eax]
        [jmp (disp ,(- disp-closure-code closure-tag) ,cp-register)]))
    (let ([ls encountered-symbol-calls])
      (let ([c* (map code-list ls)])
        (let ([c* (list*->code* (lambda (x) #f) c*)])
          (let ([p* (map (lambda (x) ($code->closure x)) c*)])
            (let f ([ls ls] [p* p*])
              (cond
                [(null? ls) (prm 'nop)]
                [else 
                 (make-seq
                   (tbind ([p (Value (K (car p*)))] [s (Value (K (car ls)))])
                       (Effect (prm '$init-symbol-function! s p)))
                   (f (cdr ls) (cdr p*)))])))))))
  (define (Program x)
    (record-case x 
      [(codes code* body)
       (let ([code* (map (lambda (x) (Clambda x Value)) code*)]
             [body (Value body)])
         (make-codes code*
           (make-seq (error-codes) body)))]
      [else (error who "invalid program ~s" x)]))
  ;;;
  ;(print-code x)
  (Program x))

