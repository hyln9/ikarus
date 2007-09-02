
(library (ikarus hash-tables)
  (export hash-table? make-hash-table get-hash-table put-hash-table!)
  (import 
    (except (ikarus) hash-table? make-hash-table
            get-hash-table put-hash-table!))

  (define-syntax inthash
    (syntax-rules ()
      [(_ x) x]))

  (define-record hasht (vec count gckey))

  (define stretch
    (lambda (h v n)
      (set-hasht-gckey! h (collect-key))
      (let ([newv (make-vector (fx* n 2) '())]
            [mask (fx- (* n 2) 1)])
        (do ([i 0 (fx+ i 1)])
            ((fx= i n))
          (let f ([b (vector-ref v i)])
            (unless (null? b)
              (let ([idx (fxlogand (inthash (pointer-value (caar b))) mask)]
                    [next (cdr b)])
                (set-cdr! b (vector-ref newv idx))
                (vector-set! newv idx b)
                (f next)))))
        (set-hasht-vec! h newv))))

  (define rehash
    (lambda (h v)
      (set-hasht-gckey! h (collect-key))
      (let ([n (vector-length v)])
        (let f ([i 0])
          (if (fx= i n)
              (void)
              (let ([b (vector-ref v i)])
                (if (null? b)
                    (f (fx+ i 1))
                    (begin
                      (vector-set! v i '())
                      (let g ([i (fx+ i 1)] [loc (last-pair b)])
                        (if (fx= i n)
                            (let ([mask (fx- n 1)])
                              (void)
                              (let f ([b b])
                                (unless (null? b)
                                  (let ([idx (fxlogand (inthash (pointer-value (caar b))) mask)])
                                    (let ([next (cdr b)])
                                      (set-cdr! b (vector-ref v idx))
                                      (vector-set! v idx b)
                                      (f next))))))
                            (let ([b (vector-ref v i)])
                              (if (null? b)
                                  (g (fx+ i 1) loc)
                                  (begin
                                    (vector-set! v i '())
                                    (set-cdr! loc b)
                                    (g (fx+ i 1) (last-pair b)))))))))))))))

  (define get-hash
    (lambda (h x v)
      (let ([pv (pointer-value x)]
            [vec (hasht-vec h)])
        (let ([ih (inthash pv)])
          (let ([idx (fxlogand ih (fx- (vector-length vec) 1))])
            (let ([b (vector-ref vec idx)])
              (cond
                [(assq x b) => cdr]
                [(not (eq? (hasht-gckey h) (collect-key)))
                 (rehash h vec)
                 (get-hash h x v)]
                [else v])))))))
  
  (define put-hash!
    (lambda (h x v)
      (let ([pv (pointer-value x)]
            [vec (hasht-vec h)])
        (let ([ih (inthash pv)])
          (let ([idx (fxlogand ih (fx- (vector-length vec) 1))])
            (let ([b (vector-ref vec idx)])
              (cond
                [(assq x b) => (lambda (a) (set-cdr! a v))]
                [(not (eq? (hasht-gckey h) (collect-key)))
                 (rehash h vec)
                 (put-hash! h x v)]
                [else
                 (vector-set! vec idx (cons (cons x v) b))
                 (let ([ct (hasht-count h)])
                   (set-hasht-count! h (fxadd1 ct))
                   (let ([n (vector-length vec)])
                     (when (fx> ct n)
                       (stretch h vec n))))])))))))

  ;;; public interface
  (define (hash-table? x) (hasht? x))

  (define (make-hash-table)
    (make-hasht (make-vector 32 '()) 0 (collect-key)))

  (define get-hash-table
    (lambda (h x v)
      (if (hasht? h)
          (get-hash h x v)
          (error 'get-hash-table "~s is not a hash table" h))))

  (define put-hash-table!
    (lambda (h x v)
      (if (hasht? h)
          (put-hash! h x v)
          (error 'put-hash-table! "~s is not a hash table" h))))
)

#!eof

(import rht)

(define (test1)
  (printf "test1 ...\n")
  (let ([ls (let f ([i 100000] [ac '()])
              (cond
                [(fx= i 0) ac]
                [else (f (fx- i 1) (cons (cons i i) ac))]))])
    (let ([ht (make-hash-table)])
      (for-each (lambda (x) (put-hash-table! ht x x)) ls)
      (let f ([i 1000])
        (unless (fx= i 0)
          (collect)
          (f (fx- i 1))))
      (for-each 
        (lambda (x)
          (unless (eq? x (get-hash-table ht x #f))
            (error 'test1 "failed")))
        ls)))
  (printf "passed test1\n"))

(define (test2)
  (printf "test2 ...\n")
  (let ([ls (let f ([i 10000] [ac '()])
              (cond
                [(fx= i 0) ac]
                [else (f (fx- i 1) (cons (cons i i) ac))]))])
    (let ([ht (make-hash-table)])
      (for-each (lambda (x) (put-hash-table! ht x x)) ls)
      (for-each 
        (lambda (x)
          (collect)
          (unless (eq? x (get-hash-table ht x #f))
            (error 'test2 "failed")))
        ls)))
  (printf "passed test2\n"))

(define (test3)
  (printf "test3 ...\n")
  (let ([ls (let f ([i 10000] [ac '()])
              (cond
                [(fx= i 0) ac]
                [else (f (fx- i 1) (cons (cons i i) ac))]))])
    (let ([ht (make-hash-table)])
      (for-each (lambda (x) 
                  (collect)
                  (put-hash-table! ht x x))
                ls)
      (for-each 
        (lambda (x)
          (unless (eq? x (get-hash-table ht x #f))
            (error 'test3 "failed")))
        ls)))
  (printf "passed test3\n"))

(define (test-all)
  (test1)
  (test2)
  (test3))
