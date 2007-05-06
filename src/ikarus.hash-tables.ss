
(library (ikarus hash-tables)
   (export hash-table? make-hash-table get-hash-table put-hash-table!)
   (import 
     (ikarus system $pairs)
     (ikarus system $vectors)
     (ikarus system $tcbuckets)
     (ikarus system $fx)
     (except (ikarus) hash-table? make-hash-table get-hash-table
             put-hash-table!))

   (define-record hasht (vec count tc))

  ;;; directly from Dybvig's paper
  (define tc-pop
    (lambda (tc)
      (let ([x ($car tc)])
        (if (eq? x ($cdr tc))
            #f
            (let ([v ($car x)])
              ($set-car! tc ($cdr x))
              ($set-car! x #f)
              ($set-cdr! x #f)
              v)))))

  (define inthash
    (lambda (key)
      ;static int inthash(int key) { /* from Bob Jenkin's */
      ;  key += ~(key << 15);
      ;  key ^=  (key >> 10);
      ;  key +=  (key << 3);
      ;  key ^=  (key >> 6);
      ;  key += ~(key << 11);
      ;  key ^=  (key >> 16);
      ;  return key;
      ;}
      (let* ([key ($fx+ key ($fxlognot ($fxsll key 15)))]
             [key ($fxlogxor key ($fxsra key 10))]
             [key ($fx+ key ($fxsll key 3))]
             [key ($fxlogxor key ($fxsra key 6))]
             [key ($fx+ key ($fxlognot ($fxsll key 11)))]
             [key ($fxlogxor key ($fxsra key 16))])
        key)))

  ;;; assq-like lookup
  (define direct-lookup 
    (lambda (x b)
      (if (fixnum? b)
          #f
          (if (eq? x ($tcbucket-key b))
              b
              (direct-lookup x ($tcbucket-next b))))))

  (define rehash-lookup
    (lambda (h tc x)
      (cond
        [(tc-pop tc) =>
         (lambda (b)
           (if (eq? ($tcbucket-next b) #f)
               (rehash-lookup h tc x)
               (begin
                 (re-add! h b)
                 (if (eq? x ($tcbucket-key b))
                     b
                     (rehash-lookup h tc x)))))]
        [else #f])))

  (define get-bucket-index
    (lambda (b)
      (let ([next ($tcbucket-next b)])
        (if (fixnum? next)
            next
            (get-bucket-index next)))))

  (define replace!
    (lambda (lb x y)
      (let ([n ($tcbucket-next lb)])
        (cond
          [(eq? n x) 
           ($set-tcbucket-next! lb y)
           (void)]
          [else
           (replace! n x y)]))))

  (define re-add! 
    (lambda (h b)
      (let ([vec (hasht-vec h)]
            [next ($tcbucket-next b)])
        ;;; first remove it from its old place
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
        ;;; reset the tcbucket-tconc FIRST
        ($set-tcbucket-tconc! b (hasht-tc h))
        ;;; then add it to the new place
        (let ([k ($tcbucket-key b)])
          (let ([ih (inthash (pointer-value k))])
            (let ([idx ($fxlogand ih ($fx- ($vector-length vec) 1))])
              (let ([n ($vector-ref vec idx)])
                ($set-tcbucket-next! b n)
                ($vector-set! vec idx b)
                (void))))))))

  (define get-hash
    (lambda (h x v)
      (let ([pv (pointer-value x)]
            [vec (hasht-vec h)])
        (let ([ih (inthash pv)])
          (let ([idx ($fxlogand ih ($fx- ($vector-length vec) 1))])
            (let ([b ($vector-ref vec idx)])
              (cond
                [(or (direct-lookup x b) (rehash-lookup h (hasht-tc h) x))
                 =>
                 (lambda (b)
                   ($tcbucket-val b))]
                [else v])))))))
  
  (define put-hash!
    (lambda (h x v)
      (let ([pv (pointer-value x)]
            [vec (hasht-vec h)])
        (let ([ih (inthash pv)])
          (let ([idx ($fxlogand ih ($fx- ($vector-length vec) 1))])
            (let ([b ($vector-ref vec idx)])
              (cond
                [(or (direct-lookup x b) (rehash-lookup h (hasht-tc h) x))
                 =>
                 (lambda (b) 
                   ($set-tcbucket-val! b v)
                   (void))]
                [else 
                 (let ([bucket
                        ($make-tcbucket (hasht-tc h) x v ($vector-ref vec idx))])
                   (if ($fx= (pointer-value x) pv)
                       ($vector-set! vec idx bucket)
                       (let* ([ih (inthash (pointer-value x))]
                              [idx 
                               ($fxlogand ih ($fx- ($vector-length vec) 1))])
                         ($set-tcbucket-next! bucket ($vector-ref vec idx))
                         ($vector-set! vec idx bucket))))
                 (let ([ct (hasht-count h)])
                   (set-hasht-count! h ($fxadd1 ct))
                   (when ($fx> ct ($vector-length vec))
                     (enlarge-table h)))])))))))

  (define insert-b
    (lambda (b vec mask)
      (let* ([x ($tcbucket-key b)]
             [pv (pointer-value x)]
             [ih (inthash pv)]
             [idx ($fxlogand ih mask)]
             [next ($tcbucket-next b)])
        ($set-tcbucket-next! b ($vector-ref vec idx))
        ($vector-set! vec idx b)
        (unless (fixnum? next)
          (insert-b next vec mask)))))

  (define move-all
    (lambda (vec1 i n vec2 mask)
      (unless ($fx= i n)
        (let ([b ($vector-ref vec1 i)])
          (unless (fixnum? b)
            (insert-b b vec2 mask))
          (move-all vec1 ($fxadd1 i) n vec2 mask)))))

  (define enlarge-table
    (lambda (h)
      (let* ([vec1 (hasht-vec h)]
             [n1 ($vector-length vec1)]
             [n2 ($fxsll n1 1)]
             [vec2 (make-base-vec n2)])
        (move-all vec1 0 n1 vec2 ($fx- n2 1))
        (set-hasht-vec! h vec2))))

  (define init-vec
    (lambda (v i n)
      (if ($fx= i n)
          v
          (begin
            ($vector-set! v i i)
            (init-vec v ($fxadd1 i) n)))))

  (define make-base-vec
    (lambda (n)
      (init-vec (make-vector n) 0 n)))

  ;;; public interface
  (define (hash-table? x) (hasht? x))

  (define (make-hash-table)
    (let ([x (cons #f #f)])
      (let ([tc (cons x x)])
        (make-hasht (make-base-vec 32) 0 tc))))

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
