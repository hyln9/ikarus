
(load "chez-compat.ss")

(load "libintelasm-5.8.ss")
(load "libfasl-6.0.ss")

(define-record code (code-size reloc-size closure-size code-vec reloc-vec))
(define make-code
  (let ([make-code make-code])
    (lambda (code-size reloc-size closure-size)
      (printf "reloc=~s\n" reloc-size)
      (let ([code-size (fxsll (fxsra (fx+ code-size 3) 2) 2)]) 
        (make-code 
          (fxsra code-size 2)
          (fxsra reloc-size 2)
          closure-size
          (make-vector code-size (cons 'byte 0))
          (make-vector (fxsra reloc-size 2)))))))

(define set-code-byte! 
  (lambda (code idx byte)
    (vector-set! (code-code-vec code) idx (cons 'byte byte))))


(define set-code-word!
  (lambda (code idx x)
    (cond
      [(fixnum? x) 
       (set-code-byte! code (fx+ idx 0) (fxsll (fxlogand x #x3F) 2))
       (set-code-byte! code (fx+ idx 1) (fxlogand (fxsra x 6) #xFF))
       (set-code-byte! code (fx+ idx 2) (fxlogand (fxsra x 14) #xFF))
       (set-code-byte! code (fx+ idx 3) (fxlogand (fxsra x 22) #xFF))]
      [else (error 'set-code-word! "unhandled ~s" x)])))

(define set-code-object!
  (lambda (code obj code-idx reloc-idx)
    (let ([v (code-reloc-vec code)])
      (vector-set! v reloc-idx (list 'object code-idx obj)))))

(define set-code-object+offset/rel! 
  (lambda (code obj code-idx obj-idx reloc-idx)
    (let ([v (code-reloc-vec code)])
      (vector-set! v reloc-idx 
        (list 'object+off/rel code-idx obj obj-idx))
      (vector-set! v (fxadd1 reloc-idx) '(skip)))))

(define set-code-object+offset!
  (lambda (code obj code-idx obj-idx reloc-idx)
    (let ([v (code-reloc-vec code)])
      (vector-set! v reloc-idx 
        (list 'object+off code-idx obj obj-idx))
      (vector-set! v (fxadd1 reloc-idx) '(skip)))))

(define make-code-executable!
  (lambda (x) (void)))



(define eval-code
  (lambda (code)
    (with-output-to-file "stst.fasl"
       (lambda ()
         (fasl-write code))
       'replace)
    (let ([rv (system "runtime/ikarus stst.fasl > stst.tmp")])
      (unless (zero? rv)
        (error 'eval-code "Failed to run: ~s" rv)))
    (with-input-from-file "stst.tmp" read)))




(let ()
  (define verbose #t)
  (define passed-tests 0)

  (define all-tests 0)

  (define test-code
    (lambda (code-ls val)
      (set! all-tests (fxadd1 all-tests))
      (when verbose (printf "Evaluating\n~s\n" code-ls))
      (let* ([code (car (list*->code* (list code-ls)))]
             [v (eval-code code)])
        (when verbose (printf "evaluated\n"))
        (cond
          [(equal? v val) 
           (set! passed-tests (fxadd1 passed-tests))
           (when verbose (printf "OK\n"))]
          [else
           (error 'test-code
                  "expected ~s, got ~s" val v)]))))

  (printf "testing ... \n")

  (test-code
    '([movl (int 0) %eax]
      [ret])
    0)


  (let ([L1 (gensym)])
    (test-code
      `([movl (obj 10) %eax]
        [jmp (label ,L1)]
        [byte 0]
        [byte 1]
        [byte 2]
        [byte 3]
        [byte 4]
        [byte 5]
        [byte 6]
        [byte 7]
        [byte 8]
        [byte 9]
        [label ,L1]
        [ret])
      10))

  (test-code
    '([movl (obj+ (1 2 3) 3) %eax]
      [movl (disp (int 0) %eax) %eax]
      [ret])
    '(2 3))




  (test-code 
    '([movl (int 40) %eax]
      [ret])
    10)

  (test-code 
    '([movl (obj 40) %eax]
      [ret])
    40)

  (test-code 
    '([movl (obj 40) %ebx]
      [movl %ebx %eax]
      [ret])
    40)

  (test-code 
    '([movl (obj (1 2 3)) %eax]
      [ret])
    '(1 2 3))

  (test-code 
    '([movl (obj (1 2 3)) %ebx]
      [movl (disp (int -1) %ebx) %eax]
      [ret])
    '1)

  (test-code 
    '([movl (obj (1 2 3)) %ebx]
      [movl (disp (int 3) %ebx) %eax]
      [ret])
    '(2 3))

  (test-code 
    '([movl (obj (1 2 3)) %ebx]
      [movl (int 120) %eax]
      [movl %eax (disp (int 3) %ebx)]
      [movl %ebx %eax]
      [ret])
    '(1 . 30))

  (test-code 
    '([movl (obj (1 2 3)) %eax]
      [movl (int 120) (disp (int -1) %eax)]
      [ret])
    '(30 2 3))

  (test-code 
    '([movl (obj (1 2 3)) %eax]
      [movl (int 120000) (disp (int -1) %eax)]
      [ret])
    '(30000 2 3))

  (test-code
    '([movl (int 40) %eax]
      [addl (int 80) %eax]
      [ret])
    30)

  (test-code
    '([movl (int 40) %eax]
      [addl (obj 20) %eax]
      [ret])
    30)

  (test-code
    '([movl (int 40) %eax]
      [movl (obj 20) %ebx]
      [addl %ebx %eax]
      [ret])
    30)

  (test-code 
    '([movl (obj (1 2 3)) %eax]
      [movl (obj 10) %ebx]
      [addl (disp (int -1) %eax) %ebx]
      [movl %ebx %eax]
      [ret])
    '11)

  (test-code 
    '([movl (obj (1 2 3)) %eax]
      [addl (int 1000) %eax]
      [movl (obj 10) %ebx]
      [addl (disp (int -1001) %eax) %ebx]
      [movl %ebx %eax]
      [ret])
    '11)

  (test-code
    '([movl (obj 10) %eax]
      [sall (int 1) %eax]
      [ret])
    20)



  (test-code
    '([movl (obj 10) %eax]
      [sall (int 3) %eax]
      [ret])
    80)

  (test-code
    '([movl (obj 10) %eax]
      [movl (int 3) %ecx]
      [sall %cl %eax]
      [ret])
    80)

  (test-code
    '([movl (obj #xF0) %eax]
      [sarl (int 1) %eax]
      [ret])
    #x78)

  (test-code
    '([movl (obj #xF0) %eax]
      [sarl (int 4) %eax]
      [ret])
    #x0F)

  (test-code
    '([movl (obj #xF0) %eax]
      [movl (int 4) %ecx]
      [sarl %cl %eax]
      [ret])
    #x0F)


  (test-code
    '([movl (obj #xFFFF) %eax]
      [andl (obj #xF0F0) %eax]
      [ret])
    #xF0F0)

  (test-code
    '([movl (obj #xFFFF) %eax]
      [movl (obj #x7654) %ebx]
      [andl %ebx %eax]
      [ret])
    #x7654)

  (test-code
    '([movl (obj #xFFFF) %eax]
      [andl (int #x3F) %eax]
      [ret])
    #xF)

  (test-code
    '([movl (obj #xFFFF) %eax]
      [movl (obj (#xF707F)) %ebx]
      [andl (disp (int -1) %ebx) %eax]
      [ret])
    #x707F)

  (test-code
    '([movl (obj #xFFFF) %eax]
      [movl (obj (#xF707F)) %ebx]
      [addl (int 1000) %ebx]
      [andl (disp (int -1001) %ebx) %eax]
      [ret])
    #x707F)

  (test-code
    '([movl (int 3) %eax]
      [notl %eax]
      [ret])
    -1)

  (test-code
    '([movl (obj 1942) %eax]
      [negl %eax]
      [ret])
    -1942)

  (test-code
    '([movl (obj 10) %eax]
      [jmp (int 10)]
      [byte 0]
      [byte 1]
      [byte 2]
      [byte 3]
      [byte 4]
      [byte 5]
      [byte 6]
      [byte 7]
      [byte 8]
      [byte 9]
      [ret])
    10)

  (test-code
    '([movl (obj 10) %eax]
      [jmp (int 10)]
      [byte 0]
      [byte 1]
      [byte 2]
      [byte 3]
      [byte 4]
      [byte 5]
      [ret]
      [byte 7]
      [byte 8]
      [byte 9]
      [jmp (int -9)])
    10)



  (let ([L1 (gensym)])
    (test-code
      `([movl (obj 10) %eax]
        [jmp (label ,L1)]
        [byte 0]
        [byte 1]
        [byte 2]
        [byte 3]
        [byte 4]
        [byte 5]
        [byte 6]
        [byte 7]
        [byte 8]
        [byte 9]
        [label ,L1]
        [ret])
      10))

  (let ([L2 (gensym)]
        [L3 (gensym)])
    (test-code
      `([movl (obj 10) %eax]
        [jmp (label ,L2)]
        [byte 0]
        [byte 1]
        [byte 2]
        [byte 3]
        [byte 4]
        [byte 5]
        [label ,L3]
        [ret]
        [byte 7]
        [byte 8]
        [byte 9]
        [label ,L2]
        [jmp (label ,L3)])
      10))




  (test-code
    '([movl (obj 10) (disp (int -4) %esp)]
      [movl (obj list) %eax]
      [ret])
    'list)

;;  (test-code
;;    '([movl (obj list) %eax]
;;      [movl (disp (int 6) %eax) %eax] ; symbol value
;;      [ret])
;;    list)

;;  (test-code
;;    '([movl (obj 10) (disp (int -4) %esp)]
;;      [movl (obj list) %eax]
;;      [movl (disp (int 6) %eax) %edi] ; symbol value
;;      [movl (obj -1) %eax] ; argc
;;      [jmp (disp (int -3) %edi)])
;;    '(10))

;;  (test-code
;;    '([movl (obj 10) (disp (int -4) %esp)]
;;      [movl (obj 20) %eax]
;;      [movl %eax (disp (int -8) %esp)]
;;      [movl (disp (int -8) %esp) %ebx]
;;      [movl %ebx (disp (int -12) %esp)]
;;      [movl (obj list) %eax]
;;      [movl (disp (int 6) %eax) %edi] ; symbol value
;;      [movl (obj -3) %eax] ; argc
;;      [jmp (disp (int -3) %edi)])
;;    '(10 20 20))

  (test-code
    '([movl (obj 10) %eax]
      [imull (int 3) %eax]
      [ret])
    30)

  (test-code
    '([movl (obj 10) %eax]
      [imull (obj 10) %eax]
      [ret])
    400)

  (test-code
    '([movl (obj 10) %eax]
      [movl (obj 20) %ebx]
      [imull %ebx %eax]
      [ret])
    800)

  (test-code
    '([movl (obj 10) %eax]
      [movl (obj 20) (disp (int -4) %esp)]
      [imull (disp (int -4) %esp) %eax]
      [ret])
    800)

  (test-code
    '([movl (obj 10) %eax]
      [cltd]
      [ret])
    10)

  (test-code
    '([movl (obj 10) %eax]
      [movl (obj 100) %edx]
      [cltd]
      [movl %edx %eax]
      [ret])
    0)

  (test-code
    '([movl (obj -10) %eax]
      [movl (obj 100) %edx]
      [cltd]
      [movl %edx %eax]
      [sall (int 2) %eax]
      [ret])
    -1)

  (let ([L1 (gensym)])
    (test-code
      `([movl (int 10) %eax]
        [cmpl (int 8) %eax]
        [jne (label ,L1)]
        [movl (obj 0) %eax]
        [ret]
        [label ,L1]
        [movl (obj 1) %eax]
        [ret])
      1))

  (let ([L1 (gensym)])
    (test-code
      `([movl (int 40) %eax]
        [cmpl (obj 10) %eax]
        [je (label ,L1)]
        [movl (obj 0) %eax]
        [ret]
        [label ,L1]
        [movl (obj 1) %eax]
        [ret])
      1))

  (let ([L1 (gensym)])
    (test-code
      `([movl (int 40) %eax]
        [movl (int 30) %ebx]
        [cmpl %ebx %eax]
        [jge (label ,L1)]
        [movl (obj 0) %eax]
        [ret]
        [label ,L1]
        [movl (obj 1) %eax]
        [ret])
      1))

  (let ([L1 (gensym)])
    (test-code
      `([movl (int 40) (disp (int -4) %esp)]
        [cmpl (int 70) (disp (int -4) %esp)]
        [jle (label ,L1)]
        [movl (obj 0) %eax]
        [ret]
        [label ,L1]
        [movl (obj 1) %eax]
        [ret])
      1))

  (test-code
    '([movl (int 40) (disp (int -4) %esp)]
      [addl (int 10) %esp]
      [movl (disp (int -14) %esp) %eax]
      [addl (int -10) %esp]
      [ret])
    10)

  (test-code
    '([movl (int 40) (disp (int -4) %esp)]
      [addl (int 1000) %esp]
      [movl (disp (int -1004) %esp) %eax]
      [addl (int -1000) %esp]
      [ret])
    10)

  (let ([L1 (gensym)])
    (test-code
      `([movl (int 40) (disp (int -4) %esp)]
        [addl (int 1000) %esp]
        [cmpl (int 70) (disp (int -1004) %esp)]
        [jle (label ,L1)]
        [addl (int -1000) %esp]
        [movl (obj 0) %eax]
        [ret]
        [label ,L1]
        [addl (int -1000) %esp]
        [movl (obj 1) %eax]
        [ret])
      1))

  (let ([L1 (gensym)])
    (test-code
      `([movl (int 4000) (disp (int -4) %esp)]
        [addl (int 1000) %esp]
        [cmpl (int 7000) (disp (int -1004) %esp)]
        [jle (label ,L1)]
        [addl (int -1000) %esp]
        [movl (obj 0) %eax]
        [ret]
        [label ,L1]
        [addl (int -1000) %esp]
        [movl (obj 1) %eax]
        [ret])
      1))

  (let ([L1 (gensym)])
    (test-code
      `([movl (int 40) (disp (int -4) %esp)]
        [movl (int 70) %ebx]
        [cmpl (disp (int -4) %esp) %ebx]
        [jge (label ,L1)]
        [movl (obj 0) %eax]
        [ret]
        [label ,L1]
        [movl (obj 1) %eax]
        [ret])
      1))


  (let ([L_fact (gensym)] [L1 (gensym)])
    (test-code
      `([movl (int 5) %eax]
        [call (label ,L_fact)]
        [sall (int 2) %eax]
        [ret]
        [label ,L_fact]
        [cmpl (int 0) %eax]
        [jne (label ,L1)]
        [movl (int 1) %eax]
        [ret]
        [label ,L1]
        [movl %eax (disp (int -4) %esp)]
        [addl (int -4) %esp]
        [addl (int -1) %eax]
        [call (label ,L_fact)]
        [addl (int 4) %esp]
        [imull (disp (int -4) %esp) %eax]
        [ret])
      120))

  (test-code
    '([movl (int 16) %eax]
      [cltd]
      [movl (int 4) %ebx]
      [idivl %ebx]
      [ret])
    1)

  (test-code
    '([movl (int 16) %eax]
      [cltd]
      [movl (obj (1)) %ebx]
      [idivl (disp (int -1) %ebx)]
      [ret])
    1)

  (test-code
    '([movl (int 16) %eax]
      [cltd]
      [movl (int 4) (disp (int -4) %esp)]
      [idivl (disp (int -4) %esp)]
      [ret])
    1)


  (test-code
    '([movl (int #x30) %ebx]
      [orl (int #x4) %ebx]
      [movl %ebx %eax]
      [ret])
    (fxsra #x34 2))

  (test-code
    '([movl (int #x30) %eax]
      [orl (int #x4) %eax]
      [ret])
    (fxsra #x34 2))

  (test-code
    '([movl (int #x30) %eax]
      [orl (obj #x1) %eax]
      [ret])
    (fxsra #x34 2))

  (test-code
    '([movl (int #x30) %ebx]
      [orl (obj #x1) %ebx]
      [movl %ebx %eax]
      [ret])
    (fxsra #x34 2))

  (test-code
    '([movl (obj (#xC)) %ebx]
      [movl (int #x4) %eax]
      [orl (disp (int -1) %ebx) %eax]
      [ret])
    (fxsra #x34 2))


  (test-code
    '([movl (int #x30) (disp (int -4) %esp)]
      [movl (int #x4) %eax]
      [orl (disp (int -4) %esp) %eax]
      [ret])
    (fxsra #x34 2))

  (test-code
    '([pushl (int 8)]
      [movl (disp (int 0) %esp) %eax]
      [addl (int 4) %esp]
      [ret])
    2)

  (test-code
    '([pushl (int 8000)]
      [movl (disp (int 0) %esp) %eax]
      [addl (int 4) %esp]
      [ret])
    2000)

  (test-code
    '([movl (int 8000) %ebx]
      [pushl %ebx]
      [movl (disp (int 0) %esp) %eax]
      [addl (int 4) %esp]
      [ret])
    2000)

  (test-code
    '([movl (obj (1 2 3)) %eax]
      [pushl (disp (int 3) %eax)]
      [addl (int 4) %esp]
      [movl (disp (int -4) %esp) %eax]
      [ret])
    '(2 3))

  (test-code
    '([movl (obj (1 2 3)) %eax]
      [addl (int -1000) %eax]
      [pushl (disp (int 1003) %eax)]
      [addl (int 4) %esp]
      [movl (disp (int -4) %esp) %eax]
      [ret])
    '(2 3))

  (test-code
    '([pushl (obj 100)]
      [popl %eax]
      [ret])
    100)

  (test-code
    '([pushl (obj 100)]
      [popl (disp (int -32) %esp)]
      [movl (disp (int -32) %esp) %eax]
      [ret])
    100)

  (test-code
    '([movl (int 4) %eax]
      [cmpl (int 5) %eax]
      [sete %al]
      [andl (int 1) %eax]
      [sall (int 2) %eax]
      [ret])
    0)

  (test-code
    '([movl (int 4) %eax]
      [cmpl (int 5) %eax]
      [setle %al]
      [andl (int 1) %eax]
      [sall (int 2) %eax]
      [ret])
    1)

  (test-code
    '([movl (obj+ (1 2 3) 3) %eax]
      [movl (disp (int 0) %eax) %eax]
      [ret])
    '(2 3))

  (let ([L_entry (gensym)] [L_no (gensym)])
    (test-code
      `([movl (obj 10) %eax]
        [ret]
        [label ,L_entry]
        [cmpl (int 1) %eax]
        [jne (label ,L_no)]
        [movl (obj foo) %eax]
        [ret]
        [label ,L_no]
        [movl (obj bar) %eax]
        [ret])
      10)
    (test-code
      `([movl (int 1) %eax]
        [jmp (label ,L_entry)])
      'foo)
    (test-code
      `([movl (int 0) %eax]
        [jmp (label ,L_entry)])
      'bar))

  (printf "Passed ~s/~s tests in assembler\n" passed-tests all-tests)
)
