
(library (ikarus hash-tables)
  (export hash-table? make-hash-table get-hash-table put-hash-table!)
  (import 
    (ikarus system $vectors)
    (ikarus system $fx)
    (except (ikarus) hash-table? make-hash-table
            get-hash-table put-hash-table!))

  (define-syntax inthash
    (syntax-rules ()
      [(_ x) x]))


  (define-record ht (g v count threashold rehashed))
  (define-record lk (key val next))

  (define make-transport-guardian
    (lambda ()
      (define loop
        (lambda (m g)
          (and m
               (let ([x (car m)])
                 (if (bwp-object? x)
                     (loop (g) g)
                     (begin (g m) x))))))
      (let ([g (make-guardian)])
        (case-lambda
          [(x) (g (weak-cons x #f))]
          [() (loop (g) g)]))))

  (define initial-size 8)

  ;;; assq-like lookup
  (define direct-lookup 
    (lambda (x b)
      (if (fixnum? b)
          #f
          (if (eq? x (lk-key b))
              b
              (direct-lookup x (lk-next b))))))

  (define rehash-lookup
    (lambda (h g x)
      (cond
        [(g) =>
         (lambda (b)
           (re-add! h b)
           (if (eq? x (lk-key b))
               b
               (rehash-lookup h g x)))]
        [else #f])))

  (define get-bucket-index
    (lambda (b)
      (let ([next (lk-next b)])
        (if (fixnum? next)
            next
            (get-bucket-index next)))))

  (define replace!
    (lambda (lb x y)
      (let ([n (lk-next lb)])
        (cond
          [(eq? n x) 
           (set-lk-next! lb y)]
          [else
           (replace! n x y)]))))

  (define re-add! 
    (lambda (h b)
      (let ([vec (ht-v h)]
            [next (lk-next b)])
        ;;; first remove it from its old place
        (set-ht-rehashed! h (fx+ (ht-rehashed h) 1))
        (let ([idx 
               (if (fixnum? next)
                   next
                   (get-bucket-index next))])
          (let ([fst ($vector-ref vec idx)])
            (cond
              [(eq? fst b) 
               ($vector-set! vec idx next)]
              [else 
               (replace! fst b next)])))
        (let ([k (lk-key b)])
          (let ([ih (inthash (pointer-value k))])
            (let ([idx ($fxlogand ih ($fx- ($vector-length vec) 1))])
              (let ([n ($vector-ref vec idx)])
                (set-lk-next! b n)
                ($vector-set! vec idx b))))))))

  (define get-hash
    (lambda (h x v)
      (let ([pv (pointer-value x)]
            [vec (ht-v h)])
        (let ([ih (inthash pv)])
          (let ([idx ($fxlogand ih ($fx- ($vector-length vec) 1))])
            (let ([b ($vector-ref vec idx)])
              (cond
                [(or (direct-lookup x b) (rehash-lookup h (ht-g h) x))
                 =>
                 (lambda (b) 
                   (lk-val b))]
                [else v])))))))
  
  (define put-hash!
    (lambda (h x v)
      (let ([pv (pointer-value x)]
            [vec (ht-v h)])
        (let ([ih (inthash pv)])
          (let ([idx ($fxlogand ih ($fx- ($vector-length vec) 1))])
            (let ([b ($vector-ref vec idx)])
              (cond
                [(or (direct-lookup x b) (rehash-lookup h (ht-g h) x))
                 =>
                 (lambda (b) 
                   (set-lk-val! b v))]
                [else 
                 (let ([bucket (make-lk x v ($vector-ref vec idx))])
                   ((ht-g h) bucket)
                   (if ($fx= (pointer-value x) pv)
                       ($vector-set! vec idx bucket)
                       (let* ([ih (inthash (pointer-value x))]
                              [idx 
                               ($fxlogand ih ($fx- ($vector-length vec) 1))])
                         (set-lk-next! bucket ($vector-ref vec idx))
                         ($vector-set! vec idx bucket))))
                 (let ([ct (ht-count h)])
                   (set-ht-count! h ($fx+ 1 ct))
                   (when ($fx> ct ($vector-length vec))
                     (enlarge-table h)))])))))))

  (define insert-b
    (lambda (b vec mask)
      (let* ([x (lk-key b)]
             [pv (pointer-value x)]
             [ih (inthash pv)]
             [idx ($fxlogand ih mask)]
             [next (lk-next b)])
        (set-lk-next! b ($vector-ref vec idx))
        ($vector-set! vec idx b)
        (unless (fixnum? next)
          (insert-b next vec mask)))))

  (define move-all
    (lambda (vec1 i n vec2 mask)
      (unless ($fx= i n)
        (let ([b ($vector-ref vec1 i)])
          (unless (fixnum? b)
            (insert-b b vec2 mask))
          (move-all vec1 ($fx+ 1 i) n vec2 mask)))))

  (define enlarge-table
    (lambda (h)
      (let* ([vec1 (ht-v h)]
             [n1 ($vector-length vec1)]
             [n2 ($fxsll n1 1)]
             [vec2 (make-base-vec n2)])
        (move-all vec1 0 n1 vec2 ($fx- n2 1))
        (set-ht-v! h vec2))))

  (define make-base-vec
    (lambda (n)
      (init-vec (make-vector n) 0 n)))

  (define init-vec
    (lambda (v i n)
      (if ($fx= i n)
          v
          (begin
            ($vector-set! v i i)
            (init-vec v ($fx+ 1 i) n)))))

  ;;; public interface
  (define hash-table?
    (lambda (x) (ht? x)))

  (define make-hash-table
    (lambda ()
      (make-ht (make-transport-guardian)
               (init-vec (make-vector initial-size) 0 initial-size)
               0
               initial-size
               0)))

  (define get-hash-table
    (lambda (h x v)
      (if (ht? h)
          (get-hash h x v)
          (error 'get-hash-table "~s is not a hash table" h))))

  (define put-hash-table!
    (lambda (h x v)
      (if (ht? h)
          (put-hash! h x v)
          (error 'put-hash-table! "~s is not a hash table" h))))
  
  (define hasht-rehash-count
    (lambda (h)
      (if (ht? h)
          (ht-rehashed h)
          (error 'hasht-rehash-count "~s is not a hash table" h))))

  (define hasht-reset-count!
    (lambda (h)
      (if (ht? h)
          (set-ht-rehashed! h 0)
          (error 'hasht-rehash-count "~s is not a hash table" h))))
)

#!eof

(import ght)

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

