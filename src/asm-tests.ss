#!/usr/bin/env ikarus --script 
(define (asm-test res ls)
  (printf "Testing:\n")
  (for-each (lambda (x)
              (printf "    ~s\n" x))
            ls)
  (let ([code 
         (car (#%list*->code* 
                (lambda (x) #f)
                `([0 (label ,(gensym)) . ,ls])))])
    (let ([proc (#%$code->closure code)])
      (let ([v (proc)])
        (unless (equal? v res)
          (printf "failed!\n")
          (error 'test-asm "expected ~s, got ~s" res v)))))
  (printf "OK\n\n"))

(asm-test 12
  '([movl 48 %eax]
    [ret]))

(asm-test 12
  '([movl 16 %eax]
    [orl 32 %eax]
    [ret]))

(asm-test 12
  '([movl 48 %eax]
    [movl %eax (disp -4 %esp)]
    [movl 0 %eax]
    [movl (disp -4 %esp) %eax]
    [ret]))

(asm-test 12
  '([movl 16 %eax]
    [movl %eax (disp -4 %esp)]
    [addl 32 (disp -4 %esp)]
    [movl (disp -4 %esp) %eax]
    [ret]))

(asm-test 1
  '([movl 1 (disp -4 %esp)]
    [sall 2 (disp -4 %esp)]
    [movl (disp -4 %esp) %eax]
    [ret]))

(asm-test 1
  '([movl 32 (disp -4 %esp)]
    [sarl 3 (disp -4 %esp)]
    [movl (disp -4 %esp) %eax]
    [ret]))

(asm-test 2
  '([movl 4 %ebx]
    [movl 4 (disp -8 %esp)]
    [addl %ebx (disp -8 %esp)]
    [movl (disp -8 %esp) %eax]
    [ret]))

(asm-test 2
  '([movl 4 %eax]
    [movl 4 (disp -8 %esp)]
    [addl %eax (disp -8 %esp)]
    [movl 0 %eax]
    [movl (disp -8 %esp) %eax]
    [ret]))



(printf "Happy Happy Joy Joy\n")
(exit)
