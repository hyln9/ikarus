
(library (objc)
  (export 
    define-framework
    define-class
    define-object
    $)
  (import 
    (ikarus)
    (ikarus system $foreign)
    (except (ypsilon-compat) format)
    )

(define ptrsize 4)



 
(define objc 
  (load-shared-object "libobjc.A.dylib"))
(define Cocoa 
  (load-shared-object "/System/Library/Frameworks/Cocoa.framework/Cocoa"))


(define-syntax define-function
  (syntax-rules ()
    ((_ ret name args)
     (define name 
       (c-function objc "Objective C Binding" ret __stdcall name args)))))


(define-function int objc_getClassList (void* int))
(define-function void* objc_getClass (char*))
(define-function void* sel_registerName (char*))
(define-function void* sel_getUid (char*))
(define-function void* class_getInstanceMethod (void* void*))
(define-function void* class_getClassMethod (void* void*))
(define-function void* class_nextMethodList (void* void*))



(define-record-type class (fields ptr))
(define-record-type object (fields ptr))
(define-record-type lazy-object (fields ptr))
(define-record-type selector (fields ptr))
(define-record-type method (fields ptr))

(define (pointer-ref addr offset)
  (assert (pointer? addr))
  (integer->pointer (pointer-ref-long addr offset)))

(define (char*len x)
  (let f ([i 0])
    (cond
      [(zero? (pointer-ref-uchar x i)) i]
      [else (f (+ i 1))])))

(define (char*->bv x)
  (let ([n (char*len x)])
    (let ([bv (make-bytevector n)])
      (let f ([i 0])
        (cond
          [(= i n) bv]
          [else
           (bytevector-u8-set! bv i (pointer-ref-uchar x i))
           (f (+ i 1))])))))

(define (char*->string x)
  (utf8->string (char*->bv x)))

(define-syntax check
  (syntax-rules ()
    [(_ who pred expr)
     (let ([t expr])
       (unless (pred t)
         (die who (format "not a ~a" 'pred) t)))]))

(define (class-name x)
  (check 'class-name class? x)
  (char*->string (pointer-ref (class-ptr x) (* ptrsize 2))))

(define (method-types x)
  (check 'method-types method? x)
  (char*->string (pointer-ref (method-ptr x) (* ptrsize 1))))

(define (method-pointer x)
  (check 'method-pointer method? x)
  (pointer-ref (method-ptr x) (* ptrsize 2)))


(define (method-selector x)
  (check 'method-selector method? x)
  (make-selector (pointer-ref (method-ptr x) (* ptrsize 0))))

(define (method-name x)
  (check 'method-name method? x)
  (string-append 
    (selector-name (method-selector x))
    "  "
    (method-types x)))



(define CLS_METHOD_ARRAY #x100)


(define (class-is? x what)
  (define alist
    '([method-array      #x100]
      [no-method-array  #x4000]))
  (check 'class-info class? x)
  (let ([mask
          (cond
            [(assq what alist) => cadr]
            [else (error 'class-is? "invalid what" what)])])
    (= mask (bitwise-and mask (pointer-ref-long (class-ptr x) (* ptrsize 4))))))

(define (class-methods x)
  (define (methods x)
    (let ([n (pointer-ref-int x ptrsize)]
          [array (integer->pointer (+ (pointer->integer x) (* 2 ptrsize)))])
      (let f ([i 0])
        (if (= i n)
            '()
            (let ([m (make-method 
                       (integer->pointer 
                         (+ (pointer->integer array)
                            (* 3 ptrsize i))))])
              (cons m (f (+ i 1))))))))
  (check 'class-methods class? x)
  (when (class-is? x 'method-array)
    (error 'class-methods "BUG: not yet for method arrays"))
  (let ([iterator (malloc ptrsize)])
    (pointer-set-long iterator 0 0)
    (let f ()
      (let ([methodlist (class_nextMethodList (class-ptr x) iterator)])
        (cond
          [(nil? methodlist)
           (free iterator) 
           '()]
          [else
           (let ([ls (methods methodlist)])
             (append ls (f)))])))))


(define (get-class-list)
  (let ([n (objc_getClassList (integer->pointer 0) 0)])
    (if (= n 0)
        '()
        (let ([buffer (malloc (* ptrsize n))])
          (let ([n (objc_getClassList buffer n)])
            (let f ([i 0] [ac '()])
              (if (= i n) 
                  (begin (free buffer) ac)
                  (f (+ i 1) 
                     (cons 
                       (make-class 
                         (integer->pointer
                           (pointer-ref-long buffer (* ptrsize i))))
                       ac)))))))))

(define (nil? x)
  (zero? (pointer->integer x)))

(define (get-class name)
  (check 'lookup-class string? name)
  (let ([v (objc_getClass name)])
    (cond
      [(nil? v) #f]
      [else (make-class v)])))

(define (get-selector name)
  (check 'lookup-selector string? name)
  (let ([v (sel_registerName name)])
    (cond
      [(nil? v) #f]
      [else (make-selector v)])))

(define (selector-name x)
  (check 'selector-name selector? x)
  (char*->string (selector-ptr x)))

(define (get-class-method class selector)
  (check 'get-class-method class? class)
  (check 'get-class-method selector? selector)
  (let ([v (class_getClassMethod 
             (class-ptr class)
             (selector-ptr selector))])
    (cond
      [(nil? v) #f]
      [else (make-method v)])))

(define (get-instance-method x selector)
  (check 'get-instance-method object? x)
  (check 'get-instance-method selector? selector)
  (let ([class (pointer-ref (object-ptr x) 0)])
    (let ([v (class_getInstanceMethod 
               class
               (selector-ptr selector))])
    (cond
      [(nil? v) #f]
      [else (make-method v)]))))


(define-syntax define-class 
  (syntax-rules ()
    [(_ name) 
     (define name 
       (or (get-class (symbol->string 'name))
           (error 'define-class "undefined class" 'name)))]))

(define-syntax define-selector 
  (syntax-rules ()
    [(_ name) 
     (define name 
       (or (get-selector (symbol->string 'name))
           (error 'define-selector "undefined selector" 'name)))]))

(define-syntax define-class-method
  (syntax-rules ()
    [(_ name class selector)
     (define name 
       (or (get-class-method class selector)
           (error 'define-class-method 
                  "class method not implemented"
                  'name)))]))


(define-class NSObject)
(define-class NSString)
(define-class NSAutoreleasePool)
(define-class NSWindow)
(define-selector alloc)
(define-selector allocWithZone:)
(define-selector init)

(define-class-method NSObject:alloc NSObject alloc)
(define-class-method NSObject:allocWithZone: NSObject allocWithZone:)
(define-class-method NSAutoreleasePool:alloc NSAutoreleasePool alloc)


(define (class-info x)
  `([name: ,(class-name x)]
    [methods: 
      ,(list-sort string<?
         (map method-name (class-methods x)))]))


(define-syntax define-framework
  (lambda (x)
    (syntax-case x ()
      [(_ name) (identifier? #'name)
       (let ([str (symbol->string (syntax->datum #'name))])
         (with-syntax ([framework-name
                        (string-append str ".framework/" str)])
           #'(define name 
               (load-shared-object framework-name))))])))

(define (load-object lib name)
  (let ([ptr 
         (or (dlsym (library-pointer lib) (symbol->string name))
             (error 'load-object "cannot find symbol" name))])
    (make-lazy-object ptr)))

(define-syntax define-object
  (lambda (x)
    (syntax-case x ()
      [(_ name lib)
       #'(define name (load-object lib 'name))])))

(define (symbol->selector x)
  (or (get-selector (symbol->string x))
      (error 'symbol->selector "undefined selector" x)))


(define (make-signature str)
  (define who 'make-signature)
  (let ([n (string-length str)])
    (define (scan i c)
      (cond
        [(= i n) (error who "cannot find " c)]
        [(char=? c (string-ref str i)) (+ i 1)]
        [else (scan (+ i 1) c)]))
    (define (parse i)
      (cond
        [(= i n) (error who "unterminated string")]
        [else
         (let ([c (string-ref str i)])
           (case c
             [(#\@) (values 'object (+ i 1))]
             [(#\:) (values 'selector (+ i 1))]
             [(#\v) (values 'void (+ i 1))]
             [(#\f) (values 'float (+ i 1))]
             [(#\i) (values 'int (+ i 1))]
             [(#\I) (values 'uint (+ i 1))]
             [(#\c) (values 'char (+ i 1))]
             [(#\{) ;;; struct
              (let ([i (scan (+ i 1) #\=)])
                (let-values ([(i ls)
                              (let f ([i i])
                                (let-values ([(x i) (parse i)])
                                  (cond
                                    [(>= i n) (error who "runaway")]
                                    [(char=? (string-ref str i) #\})
                                     (values (+ i 1) (list x))]
                                    [else
                                     (let-values ([(i ls) (f i)])
                                       (values i (cons x ls)))])))])
                  (values (list->vector ls) i)))]
             [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
              (values 'skip (+ i 1))]
             [else (error who "invalid char" c str)]))]))
    (define (cons/skip x y)
      (if (eq? x 'skip) y (cons x y)))
    (let f ([i 0])
      (cond
        [(= i n) '()]
        [else 
         (let-values ([(x i) (parse i)])
           (cons/skip x (f i)))]))))


(define (objc-type->ikarus-type x)
  (cond
    [(vector? x) 
     (vector-map objc-type->ikarus-type x)]
    [else
     (case x 
       [(selector) 'pointer]
       [(object)   'pointer]
       [(void)     'void]
       [(float)    'float]
       [(uint)     'uint32]
       [(int)      'sint32]
       [(char)     'sint8]
       [else (error 'objc-type->ikarus-type "invalid type" x)])]))



(define (convert-incoming t x)
  (case t
    [(object) (make-object x)]
    [(void)   (void)]
    [else (error 'convert-incoming "invalid type" t)]))

(define (convert-outgoing t x)
  (cond
    [(vector? t)
     (cond
       [(vector? x)
        (unless (= (vector-length x) (vector-length t))
          (error 'convert-outgoing "length mismatch" x t))
        (vector-map convert-outgoing t x)]
       [else (error 'convert-output "not a vector" x)])]
    [else
     (case t
       [(selector)
        (cond
          [(selector? x) (selector-ptr x)]
          [else (error 'convert-output "not a selector" x)])]
       [(object) 
        (cond
          [(object? x) (object-ptr x)]
          [(lazy-object? x) 
           (pointer-ref (lazy-object-ptr x) 0)]
          [(class? x) (class-ptr x)]
          [else (error 'convert-output "cannot convert to object" x)])]
       [(float) 
        (cond
          [(number? x) (inexact x)]
          [else (error 'convert-output "cannot convert to float" x)])]
       [(uint int char)
        (cond
          [(or (fixnum? x) (bignum? x)) x]
          [(boolean? x) (if x 1 0)]
          [else (error 'convert-output "cannot convert to int" x)])]
       [else (error 'convert-outgoing "invalid type" t)])]))


(define (call-with-sig sig mptr args)
  (let ([rtype (car sig)] [argtypes (cdr sig)])
    (unless (= (length args) (length argtypes))
      (error 'call-with-sig "incorrect number of args" args argtypes))
    (let ([ffi (make-ffi 
                 (objc-type->ikarus-type rtype)
                 (map objc-type->ikarus-type argtypes))])
      (let ([proc (ffi mptr)])
        (convert-incoming rtype 
          (apply proc (map convert-outgoing argtypes args)))))))

(define (send-message x method-name . args)
  (let ([selector (symbol->selector method-name)])
    (let ([method 
           (cond
             [(class? x) (get-class-method x selector)]
             [(object? x) (get-instance-method x selector)]
             [(lazy-object? x)
              (get-instance-method 
                (make-object (pointer-ref (lazy-object-ptr x) 0))
                selector)]
             [else (error 'send-message "not an object" x)])])
      (unless method
        (error 'send-message "undefined method" method-name))
      (let ([sig (make-signature (method-types method))]
            [mptr (method-pointer method)])
        (call-with-sig sig mptr (cons* x selector args))))))

(define-syntax $
  (lambda (x)
    (define (process-rest ls)
      (syntax-case ls ()
        [() (values "" '())]
        [(kwd val . rest) (identifier? #'kwd)
         (let-values ([(sel args) (process-rest #'rest)])
           (values 
             (string-append 
               (symbol->string (syntax->datum #'kwd))
               sel)
             (cons #'val args)))]))
    (define (process-args ls)
      (let-values ([(sel args) (process-rest ls)])
        (cons (datum->syntax #'here (string->symbol sel)) args)))
    (syntax-case x ()
      [(_ receiver kwd)
       (identifier? #'kwd)
       #'(send-message receiver 'kwd)]
      [(_ receiver kwd/arg* ...)
       (identifier? #'kwd)
       (with-syntax ([(sel-name arg* ...)
                      (process-args #'(kwd/arg* ...))])
         #'(send-message receiver 'sel-name arg* ...))])))


         

;(printf "Classes: ~s\n" 
;  (list-sort string<? (map class-name (get-class-list))))
;
;(printf "NSObject=~s\n" NSObject)
;(printf "alloc=~s\n" alloc)
;(printf "init=~s\n" init)
;(printf "NSObject:alloc=~s\n" NSObject:alloc)
;(printf "NSObject:allocWithZone=~s\n" NSObject:allocWithZone:)
;(printf "types alloc=~s\n" (method-types NSObject:alloc))
;(printf "types alloc=~s\n" (method-types NSAutoreleasePool:alloc))
;(printf "types allocWithZone=~s\n" (method-types NSObject:allocWithZone:))
;(for-each
;  (lambda (x)
;    (pretty-print (class-info x)))
;  (list NSObject NSString NSAutoreleasePool NSWindow))



) ; library
