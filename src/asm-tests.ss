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
        (printf "running\n")
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

(asm-test 12
  '([movl 16 %eax]
    [movl %eax (disp -200 %esp)]
    [addl 32 (disp -200 %esp)]
    [movl (disp -200 %esp) %eax]
    [ret]))

(asm-test 1
  '([movl 8 %eax]
    [movl %eax (disp -4 %esp)]
    [movl 4 %eax]
    [subl %eax (disp -4 %esp)]
    [movl -4 %eax]
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

(asm-test 1
  '([movl 0 (disp -4 %esp)]
    [movl %esp %eax]
    [movl -4 %ebx]
    [movb 4 (disp %eax %ebx)]
    [movl (disp -4 %esp) %eax]
    [ret]))

(asm-test 2
  '([movl 12 (disp -8 %esp)] ;;; 12 = 001100
    [movl 24 %eax]           ;;; 24 = 011000
    [andl %eax (disp -8 %esp)]
    [movl (disp -8 %esp) %eax]
    [ret]))

(asm-test 3
  '([movl 4 (disp -4 %esp)]
    [orl 8 (disp -4 %esp)]
    [movl (disp -4 %esp) %eax]
    [ret]))

(asm-test 3
  '([movl -1 (disp -4 %esp)]
    [andl 12 (disp -4 %esp)]
    [movl (disp -4 %esp) %eax]
    [ret]))

(asm-test 1
  '([movl (obj (1 2)) (disp -4 %esp)]
    [movl (obj car) %eax]
    [movl (disp 14 %eax) %edi] ;;; symbol-value 
    [movl -4 %eax]
    [jmp (disp -3 %edi)])) 

(asm-test 1
  '([movl (obj (1 2)) (disp -4 %esp)]
    [movl (obj car) %eax]
    [movl (disp 14 %eax) %edi] ;;; symbol-value 
    [movl (disp -3 %edi) %eax]
    [movl %eax (disp 26 (obj car))]
    [movl -4 %eax]
    [jmp (disp 26 (obj car))]))

(asm-test 1
  '([movl (obj (1 2)) (disp -4 %esp)]
    [movl (obj car) %eax]
    [movl (disp 14 %eax) %eax] ;;; symbol-value 
    [movl (disp -3 %eax) %eax]
    [movl %eax (disp 26 (obj car))]
    [movl -4 %eax]
    [jmp (disp 26 (obj car))]))


(asm-test 1
  '([movl (obj (1 2)) (disp -4 %esp)]
    [movl -4 %eax]
    [jmp (disp 26 (obj car))]))

(asm-test 1
  '([movl (obj (1 2)) (disp -8 %esp)]
    [movl -4 %eax]
    [call (disp 26 (obj car))]
    [ret]))

(printf "Happy Happy Joy Joy\n")
(exit)
