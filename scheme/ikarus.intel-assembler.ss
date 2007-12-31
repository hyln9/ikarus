;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



(library (ikarus intel-assembler)
  (export assemble-sources code-entry-adjustment)
  (import 
    (ikarus)
    (rnrs bytevectors)
    (except (ikarus code-objects) procedure-annotation)
    (ikarus system $pairs))

(define fold
  (lambda (f init ls)
    (cond
      [(null? ls) init]
      [else
       (f (car ls) (fold f init (cdr ls)))])))

(define convert-instructions
  (lambda (ls)
    (fold convert-instruction '() ls)))

(define register-mapping
  '([%eax 32 0] 
    [%ecx 32 1]
    [%edx 32 2]
    [%ebx 32 3]
    [%esp 32 4]
    [%ebp 32 5]
    [%esi 32 6]
    [%edi 32 7]
    [%r8   64 0]
    [%r9   64 1]
    [%r10  64 2]
    [%r11  64 3]
    [%r12  64 4]
    [%r13  64 5]
    [%r14  64 6]
    [%r15  64 7]
    [%al   8 0]
    [%cl   8 1]
    [%dl   8 2]
    [%bl   8 3]
    [%ah   8 4]
    [%ch   8 5]
    [%dh   8 6]
    [%bh   8 7]
    [/0    0 0]
    [/1    0 1]
    [/2    0 2]
    [/3    0 3]
    [/4    0 4]
    [/5    0 5]
    [/6    0 6]
    [/7    0 7]
    [xmm0 xmm 0]
    [xmm1 xmm 1]
    [xmm2 xmm 2]
    [xmm3 xmm 3]
    [xmm4 xmm 4]
    [xmm5 xmm 5]
    [xmm6 xmm 6]
    [xmm7 xmm 7]
    ))
  
(define register-index
  (lambda (x)
    (cond
      [(assq x register-mapping) => caddr]
      [else (die 'register-index "not a register" x)])))

(define reg32?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (eqv? (cadr x) 32))]
      [else #f])))

(define reg8?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (eqv? (cadr x) 8))]
      [else #f])))

(define xmmreg?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (eqv? (cadr x) 'xmm))]
      [else #f])))

(define reg?
  (lambda (x)
    (assq x register-mapping)))


;(define with-args
;  (lambda (ls f)
;    (apply f (cdr ls))))

(define-syntax with-args
  (syntax-rules (lambda)
    [(_ x (lambda (a0 a1) b b* ...))
     (let ([t x])
       (if (pair? t) 
           (let ([t ($cdr t)])
             (if (pair? t)
                 (let ([a0 ($car t)] [t ($cdr t)])
                   (if (pair? t)
                        (let ([a1 ($car t)])
                           (if (null? ($cdr t)) 
                               (let () b b* ...)
                               (die 'with-args "too many args")))
                        (die 'with-args "too few args")))
                 (die 'with-args "too few args")))
           (die 'with-args "too few args")))]))



(define-syntax byte
  (syntax-rules ()
    [(_ x) (fxlogand x 255)]))


(define word
  (lambda (x)
    (cons 'word x)))

(define reloc-word
  (lambda (x)
    (cons 'reloc-word x)))

(define reloc-word+
  (lambda (x d)
    (cons* 'reloc-word+ x d)))

(define byte?
  (lambda (x)
    (and (fixnum? x)
         (fx<= x 127)
         (fx<= -128 x))))

(define mem?
  (lambda (x)
    (and (pair? x)
         (eq? (car x) 'disp))))

(define small-disp?
  (lambda (x)
    (and (mem? x)
         (byte? (cadr x)))))


(define CODE
  (lambda (n ac)
    (cons (byte n) ac)))

(define CODE+r
  (lambda (n r ac)
    (cons (byte (fxlogor n (register-index r))) ac)))

(define ModRM
  (lambda (mod reg r/m ac)
    (cons (byte (fxlogor 
                  (register-index r/m)
                  (fxlogor 
                    (fxsll (register-index reg) 3)
                    (fxsll mod 6))))
          (if (and (not (fx= mod 3)) (eq? r/m '%esp))
              (cons (byte #x24) ac)
              ac))))


(define IMM32
  (lambda (n ac)
    (cond
      [(int? n) 
       (if (fixnum? n)
           (cons* 
             (byte n)
             (byte (fxsra n 8))
             (byte (fxsra n 16))
             (byte (fxsra n 24))
             ac)
           (let* ([lo (remainder n 256)]
                  [hi (quotient (if (< n 0) (- n 255) n) 256)])
             (cons* 
               (byte lo)
               (byte hi)
               (byte (fxsra hi 8))
               (byte (fxsra hi 16))
               ac)))]
      [(obj? n)
       (let ([v (cadr n)])
         (if (immediate? v)
             (cons (word v) ac)
             (cons (reloc-word v) ac)))]
      [(obj+? n)
       (let ([v (cadr n)] [d (caddr n)])
         (cons (reloc-word+ v d) ac))]
      [(label-address? n)
       (cons (cons 'label-addr (label-name n)) ac)]
      [(foreign? n)
       (cons (cons 'foreign-label (label-name n)) ac)]
      [else (die 'IMM32 "invalid" n)])))


(define IMM8
  (lambda (n ac)
    (cond
      [(int? n) 
       (cons* (byte n) ac)]
      [else (die 'IMM8 "invalid" n)])))


(define imm?
  (lambda (x)
    (or (int? x)
        (obj? x)
        (obj+? x) 
        (label-address? x)
        (foreign? x))))

(define foreign?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'foreign-label))))


(define imm8?
  (lambda (x)
    (and (int? x) (byte? x))))

(define label?
  (lambda (x)
    (cond
      [(and (pair? x) (eq? (car x) 'label))
       (let ([d (cdr x)])
         (unless (and (null? (cdr d))
                      (symbol? (car d)))
           (die 'assemble "invalid label" x)))
       #t]
      [else #f])))

(define label-address?
  (lambda (x)
    (cond
      [(and (pair? x) (eq? (car x) 'label-address))
       (let ([d (cdr x)])
         (unless (and (null? (cdr d))
                      (or (symbol? (car d))
                          (string? (car d))))
           (die 'assemble "invalid label-address" x)))
       #t]
      [else #f])))

(define label-name
  (lambda (x) (cadr x)))

(define int? integer?)

(define obj?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'obj))))

(define obj+?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'obj+))))

(define CODErri
  (lambda (c d s i ac)
    (cond
      [(imm8? i)
       (CODE c (ModRM 1 d s (IMM8 i ac)))]
      [(imm? i)
       (CODE c (ModRM 2 d s (IMM32 i ac)))]
      [else (die 'CODErri "invalid i" i)])))

(define CODErr
  (lambda (c r1 r2 ac)
    (CODE c (ModRM 3 r1 r2 ac))))


(define RegReg
  (lambda (r1 r2 r3 ac)
    (cond
      [(eq? r3 '%esp) (die 'assembler "BUG: invalid src %esp")]
      [(eq? r1 '%ebp) (die 'assembler "BUG: invalid src %ebp")]
      [else 
       (cons*
         (byte (fxlogor 4 (fxsll (register-index r1) 3)))
         (byte (fxlogor (register-index r2) 
                        (fxsll (register-index r3) 3)))
         ac)])))


(define IMM32*2
  (lambda (i1 i2 ac)
    (cond
      [(and (int? i1) (obj? i2))
       (let ([d i1] [v (cadr i2)])
         (cons (reloc-word+ v d) ac))]
      [(and (int? i2) (obj? i1)) (IMM32*2 i2 i1 ac)]
      [(and (int? i1) (int? i2)) 
       ;FIXME
       (IMM32 i1 (IMM32 i2 ac))]
      [else (die 'assemble "invalid IMM32*2" i1 i2)])))

(define (SIB s i b ac)
  (cons (byte
          (fxlogor
            (register-index b)
            (fxlogor 
              (fxsll (register-index i) 3)
              (fxsll s 6))))
        ac))

(define CODErd
  (lambda (c r1 disp ac)
    (with-args disp
      (lambda (a1 a2)
        (cond
          [(and (reg32? a1) (reg32? a2))
           (CODE c (RegReg r1 a1 a2 ac))]
          [(and (imm? a1) (reg32? a2))
           (CODErri c r1 a2 a1 ac)]
          [(and (imm? a2) (reg32? a1))
           (CODErri c r1 a1 a2 ac)]
          [(and (imm? a1) (imm? a2))
           (CODE c 
             (ModRM 0 r1 '/5 
               (IMM32*2 a1 a2 ac)))]
          [else (die 'CODErd "unhandled" disp)])))))

;              81 /0 id    ADD r/m32,imm32            Valid Add imm32 to 
(define (CODE/digit c /d)
  (lambda (dst ac)
    (cond
      [(mem? dst) 
       (with-args dst
          (lambda (a0 a1)
            (cond
              [(and (imm8? a0) (reg32? a1))
               (CODE c (ModRM 1 /d a1 (IMM8 a0 ac)))]
              [(and (imm? a0) (reg32? a1))
               (CODE c (ModRM 2 /d a1 (IMM32 a0 ac)))]
              [(and (imm8? a1) (reg32? a0))
               (CODE c (ModRM 1 /d a0 (IMM8 a1 ac)))]
              [(and (imm? a1) (reg32? a0))
               (CODE c (ModRM 2 /d a0 (IMM32 a1 ac)))]
              [(and (reg32? a0) (reg32? a1))
               (CODE c (ModRM 1 /d '/4 (SIB 0 a0 a1 (IMM8 0 ac))))]
              [(and (imm? a0) (imm? a1))
               (CODE c (ModRM 0 /d '/5 (IMM32*2 a0 a1 ac)))]
              [else (die 'CODE/digit "unhandled" a0 a1)])))]
      [else (die 'CODE/digit "unhandled" dst)])))

(define *cogen* (gensym "*cogen*"))

(define-syntax add-instruction
  (syntax-rules ()
    [(_ (name instr ac args ...) b b* ...)
     (putprop 'name *cogen*
       (cons (length '(args ...))
             (lambda (instr ac args ...) b b* ...)))]))

(define-syntax add-instructions
  (syntax-rules ()
    [(_ instr ac [(name* arg** ...) b* b** ...] ...)
     (begin
       (add-instruction (name* instr ac arg** ...) b* b** ...) ...)]))

(define (convert-instruction a ac)
  (cond
    [(getprop (car a) *cogen*) =>
     (lambda (p)
       (let ([n (car p)] [proc (cdr p)] [args (cdr a)])
         (cond
           [(fx= n 2)
            (if (fx= (length args) 2)
                (proc a ac (car args) (cadr args))
                (die 'convert-instruction "incorrect args" a))]
           [(fx= n 1)
            (if (fx= (length args) 1)
                (proc a ac (car args))
                (die 'convert-instruction "incorrect args" a))]
           [(fx= n 0)
            (if (fx= (length args) 0)
                (proc a ac)
                (die 'convert-instruction "incorrect args" a))] 
           [else
            (if (fx= (length args) n)
                (apply proc a ac args)
                (die 'convert-instruction "incorrect args" a))])))]
    [else (die 'convert-instruction "unknown instruction" a)]))

(define (CODEri code r i ac)
  (CODE+r code r (IMM32 i ac)))

(define (CODEmi code m i ac)
  ((CODE/digit code '/0) m (IMM32 i ac)))

(define (CODEmi8 code m i8 ac)
  ((CODE/digit code '/0) m (IMM8 i8 ac)))

(define (CODEi code i ac)
  (CODE code (IMM32 i ac)))

(module ()
(define who 'assembler)

(define (conditional-set c dst ac)
  (cond
    [(reg8? dst)
     (CODE #x0F (CODE c (ModRM 3 '/0 dst ac)))]
    [else (die who "invalid condition-set" dst)]))

(define (conditional-jump c dst ac)
  (cond
    [(imm? dst)
     (CODE #x0F (CODE c (IMM32 dst ac)))]
    [(label? dst)
     (CODE #x0F (CODE c (cons (cons 'relative (label-name dst)) ac)))]
    [else (die who "invalid conditional jump target" dst)]))

(define (CRI32 c r i32 ac)
  (CODEri c r i32 ac))
(define (CMI32 c d i32 ac)
  (CODEmi c d i32 ac))
(define (CMI8 c d i8 ac)
  (CODEmi8 c d i8 ac))

(define (CRRI8 c r0 r1 i8 ac)
  (CODE c (ModRM 3 r0 r1 (IMM8 i8 ac))))
(define (CI8 c i8 ac)
  (CODE c (IMM8 i8 ac)))
(define (CI32 c i32 ac)
  (CODEi c i32 ac))
(define (CRRI32 c r0 r1 i32 ac)
  (CODE c (ModRM 3 r0 r1 (IMM32 i32 ac))))
(define (CRR c r0 r1 ac)
  (CODErr c r0 r1 ac))
(define (CRMI32 c r d i32 ac)
  ((CODE/digit c r) d (IMM32 i32 ac)))
(define (CRMI8 c r d i8 ac)
  ((CODE/digit c r) d (IMM8 i8 ac)))
(define (CRM c r d ac)
  ((CODE/digit c r) d ac))
(define (CCRR c0 c1 r0 r1 ac)
  (CODE c0 (CRR c1 r0 r1 ac)))
(define (CCRM c0 c1 r m ac)
  (CODE c0 (CRM c1 r m ac)))
(define (CCR c0 c1 r ac)
  (CODE c0 (CR c1 r ac)))
(define (CR c r ac) 
  (CODE+r c r ac))
(define (CL c lab ac)
  (CODE c (cons (cons 'relative (label-name lab)) ac)))


(add-instructions instr ac
   [(ret)                                 (CODE #xC3 ac)]
   [(cltd)                                (CODE #x99 ac)]
   [(movl src dst)
    (cond
      [(and (imm? src) (reg32? dst))      (CRI32 #xB8 dst src ac)]
      [(and (imm? src) (mem? dst))        (CMI32 #xC7 dst src ac)]
      [(and (reg32? src) (reg32? dst))    (CRR  #x89 src dst ac)]
      [(and (reg32? src) (mem? dst))   ;   (CRM  #x89 src dst ac)]
       (CODErd #x89 src dst ac)]
      [(and (mem? src) (reg32? dst))   ;   (CRM  #x8B dst src ac)]
       (CODErd #x8B dst src ac)]
      [else (die who "invalid" instr)])]
   [(movb src dst)
    (cond
      [(and (imm8? src) (mem? dst))       (CMI8 #xC6 dst src ac)]
      [(and (reg8? src) (mem? dst))       (CRM #x88 src dst ac)]
      [(and (mem? src) (reg8? dst))       (CRM #x8A dst src ac)]
      [else (die who "invalid" instr)])]
   [(addl src dst)
    (cond   
      [(and (imm8? src) (reg32? dst))     (CRRI8  #x83 '/0 dst src ac)]
      [(and (imm? src) (eq? dst '%eax))   (CI32   #x05 src ac)]
      [(and (imm? src) (reg32? dst))      (CRRI32 #x81 '/0 dst src ac)]
      [(and (reg32? src) (reg32? dst))    (CRR    #x01 src dst ac)]
      [(and (mem? src) (reg32? dst))      (CRM    #x03 dst src ac)]
      [(and (imm? src) (mem? dst))        (CRMI32 #x81 '/0 dst src ac)]
      [(and (reg32? src) (mem? dst))      (CRM    #x01 src dst ac)]
      [else (die who "invalid" instr)])]
   [(subl src dst)
    (cond   
      [(and (imm8? src) (reg32? dst))     (CRRI8  #x83 '/5 dst src ac)]
      [(and (imm? src) (eq? dst '%eax))   (CI32   #x2D src ac)]
      [(and (imm? src) (reg32? dst))      (CRRI32 #x81 '/5 dst src ac)]
      [(and (reg32? src) (reg32? dst))    (CRR    #x29 src dst ac)]
      [(and (mem? src) (reg32? dst))      (CRM    #x2B dst src ac)]
      [(and (imm? src) (mem? dst))        (CRMI32 #x81 '/5 dst src ac)]
      [(and (reg32? src) (mem? dst))      (CRM    #x29 src dst ac)]
      [else (die who "invalid" instr)])]
   [(sall src dst)
    (cond
      [(and (equal? 1 src) (reg32? dst))  (CRR   #xD1 '/4 dst ac)]
      [(and (imm8? src) (reg32? dst))     (CRRI8 #xC1 '/4 dst src ac)]
      [(and (imm8? src) (mem? dst))       (CRMI8 #xC1 '/4 dst src ac)]
      [(and (eq? src '%cl) (reg32? dst))  (CRR   #xD3 '/4 dst ac)]
      [(and (eq? src '%cl) (mem? dst))    (CRM   #xD3 '/4 dst ac)]
      [else (die who "invalid" instr)])]
   [(shrl src dst)
    (cond
      [(and (equal? 1 src) (reg32? dst))  (CRR   #xD1 '/5 dst ac)]
      [(and (imm8? src) (reg32? dst))     (CRRI8 #xC1 '/5 dst src ac)]
      [(and (eq? src '%cl) (reg32? dst))  (CRR   #xD3 '/5 dst ac)]
      [(and (imm8? src) (mem? dst))       (CRMI8 #xC1 '/5 dst src ac)]
      [(and (eq? src '%cl) (mem? dst))    (CRM   #xD3 '/5 dst ac)]
      [else (die who "invalid" instr)])]
   [(sarl src dst)
    (cond
      [(and (equal? 1 src) (reg32? dst))  (CRR   #xD1 '/7 dst ac)]
      [(and (imm8? src) (reg32? dst))     (CRRI8 #xC1 '/7 dst src ac)]
      [(and (imm8? src) (mem? dst))       (CRMI8 #xC1 '/7 dst src ac)]
      [(and (eq? src '%cl) (reg32? dst))  (CRR   #xD3 '/7 dst ac)]
      [(and (eq? src '%cl) (mem? dst))    (CRM   #xD3 '/7 dst ac)]
      [else (die who "invalid" instr)])]
   [(andl src dst)
    (cond
      [(and (imm? src) (mem? dst))        (CRMI32 #x81 '/4 dst src ac)]
      [(and (imm8? src) (reg32? dst))     (CRRI8  #x83 '/4 dst src ac)]
      [(and (imm? src) (eq? dst '%eax))   (CI32   #x25 src ac)]
      [(and (imm? src) (reg32? dst))      (CRRI32 #x81 '/4 dst src ac)]
      [(and (reg32? src) (reg32? dst))    (CRR    #x21 src dst ac)]
      [(and (reg32? src) (mem? dst))      (CRM    #x21 src dst ac)]
      [(and (mem? src) (reg32? dst))      (CRM    #x23 dst src ac)]
      [else (die who "invalid" instr)])]
   [(orl src dst) 
    (cond
      [(and (imm? src) (mem? dst))        (CRMI32 #x81 '/1 dst src ac)]
      [(and (reg32? src) (mem? dst))      (CRM    #x09 src dst ac)]
      [(and (imm8? src) (reg32? dst))     (CRRI8  #x83 '/1 dst src ac)]
      [(and (imm? src) (eq? dst '%eax))   (CI32   #x0D src ac)]
      [(and (imm? src) (reg32? dst))      (CRRI32 #x81 '/1 dst src ac)]
      [(and (reg32? src) (reg32? dst))    (CRR    #x09 src dst ac)]
      [(and (mem? src) (reg32? dst))      (CRM    #x0B dst src ac)]
      [else (die who "invalid" instr)])]
   [(xorl src dst) 
    (cond
      [(and (imm8? src) (reg32? dst))     (CRRI8 #x83 '/6 dst src ac)]
      [(and (imm8? src) (mem? dst))       (CRMI8 #x83 '/6 dst src ac)]
      [(and (imm? src) (eq? dst '%eax))   (CI32  #x35 src ac)]
      [(and (reg32? src) (reg32? dst))    (CRR   #x31 src dst ac)]
      [(and (mem? src) (reg32? dst))      (CRM   #x33 dst src ac)]
      [(and (reg32? src) (mem? dst))      (CRM   #x31 src dst ac)]
      [else (die who "invalid" instr)])]
   [(leal src dst)
    (cond
      [(and (mem? src) (reg32? dst))      (CRM #x8D dst src ac)]
      [else (die who "invalid" instr)])]
   [(cmpl src dst)
    (cond
      [(and (imm8? src) (reg32? dst))     (CRRI8  #x83 '/7 dst src ac)]
      [(and (imm? src) (eq? dst '%eax))   (CI32   #x3D src ac)]
      [(and (imm? src) (reg32? dst))      (CRRI32 #x81 '/7 dst src ac)]
      [(and (reg32? src) (reg32? dst))    (CRR    #x39 src dst ac)]
      [(and (mem? src) (reg32? dst))      (CRM    #x3B dst src ac)]
      [(and (imm8? src) (mem? dst))       (CRMI8  #x83 '/7 dst src ac)]
      [(and (imm? src) (mem? dst))        (CRMI32 #x81 '/8 dst src ac)]
      [else (die who "invalid" instr)])]
   [(imull src dst)
    (cond
      [(and (imm8? src) (reg32? dst))     (CRRI8 #x6B dst dst src ac)]
      [(and (imm? src) (reg32? dst))      (CRRI32 #x69 dst dst src ac)]
      [(and (reg32? src) (reg32? dst))    (CCRR #x0F #xAF dst src ac)]
      [(and (mem? src) (reg32? dst))      (CCRM #x0F #xAF dst src ac)]
      [else (die who "invalid" instr)])]
   [(idivl dst)
    (cond
      [(reg32? dst)                       (CRR #xF7 '/7 dst ac)]
      [(mem? dst)                         (CRM #xF7 '/7 dst ac)]
      [else (die who "invalid" instr)])]
   [(pushl dst)
    (cond
      [(imm8? dst)                        (CI8 #x6A dst ac)]
      [(imm? dst)                         (CI32 #x68 dst ac)]
      [(reg32? dst)                       (CR #x50 dst ac)]
      [(mem? dst)                         (CRM #xFF '/6 dst ac)]
      [else (die who "invalid" instr)])]
   [(popl dst)
    (cond
      [(reg32? dst)                      (CR #x58 dst ac)]
      [(mem? dst)                        (CRM #x8F '/0 dst ac)]
      [else (die who "invalid" instr)])] 
   [(notl dst)
    (cond
      [(reg32? dst)                     (CRR #xF7 '/2 dst ac)]
      [(mem? dst)                       (CRM #xF7 '/7 dst ac)]
      [else (die who "invalid" instr)])]
   [(bswap dst)
    (cond
      [(reg32? dst)                     (CCR #x0F #xC8 dst ac)]
      [else (die who "invalid" instr)])]
   [(negl dst)
    (cond
      [(reg32? dst)                    (CRR #xF7 '/3 dst ac)]
      [else (die who "invalid" instr)])]
   [(jmp dst)
    (cond
      [(label? dst)                   (CL #xE9 dst ac)]
      [(imm? dst)                     (CI32 #xE9 dst ac)]
      [(mem? dst)                     (CRM  #xFF '/4 dst ac)]
      [else (die who "invalid jmp target" dst)])]
   [(call dst)
    (cond
      [(label? dst)                   (CL #xE8 dst ac)]
      [(imm? dst)                     (CI32 #xE8 dst ac)]
      [(mem? dst)                     (CRM #xFF '/2 dst ac)]
      [(reg32? dst)                   (CRR #xFF '/2 dst ac)]
      [else (die who "invalid jmp target" dst)])]
   [(movsd src dst)
    (cond
      [(and (xmmreg? dst) (or (xmmreg? src) (mem? src)))
       (CODE #xF2 (CODE #x0F ((CODE/digit #x10 dst) src ac)))]
      [(and (xmmreg? src) (or (xmmreg? dst) (mem? dst)))
       (CODE #xF2 (CODE #x0F ((CODE/digit #x11 src) dst ac)))]
      [else (die who "invalid" instr)])]
   [(cvtsi2sd src dst)
    (cond
      [(and (xmmreg? dst) (reg32? src))
       (CODE #xF2 (CODE #x0F (CODE #x2A (ModRM 3 src dst ac))))]
      [(and (xmmreg? dst) (mem? src))
       (CODE #xF2 (CODE #x0F ((CODE/digit #x2A dst) src ac)))]
      [else (die who "invalid" instr)])] 
   [(cvtsd2ss src dst)
    (cond
      [(and (xmmreg? dst) (xmmreg? src))
       (CODE #xF2 (CODE #x0F (CODE #x5A (ModRM 3 src dst ac))))]
      [else (die who "invalid" instr)])]
   [(cvtss2sd src dst)
    (cond
      [(and (xmmreg? dst) (xmmreg? src))
       (CODE #xF3 (CODE #x0F (CODE #x5A (ModRM 3 src dst ac))))]
      [else (die who "invalid" instr)])]
   [(movss src dst)
    (cond
      [(and (xmmreg? dst) (or (xmmreg? src) (mem? src)))
       (CODE #xF3 (CODE #x0F ((CODE/digit #x10 dst) src ac)))]
      [(and (xmmreg? src) (or (xmmreg? dst) (mem? dst)))
       (CODE #xF3 (CODE #x0F ((CODE/digit #x11 src) dst ac)))]
      [else (die who "invalid" instr)])]
   [(addsd src dst)
    (cond
      [(and (xmmreg? dst) (or (xmmreg? src) (mem? src)))
       (CODE #xF2 (CODE #x0F ((CODE/digit #x58 dst) src ac)))]
      [else (die who "invalid" instr)])]
   [(subsd src dst)
    (cond
      [(and (xmmreg? dst) (or (xmmreg? src) (mem? src)))
       (CODE #xF2 (CODE #x0F ((CODE/digit #x5C dst) src ac)))]
      [else (die who "invalid" instr)])]
   [(mulsd src dst)
    (cond
      [(and (xmmreg? dst) (or (xmmreg? src) (mem? src)))
       (CODE #xF2 (CODE #x0F ((CODE/digit #x59 dst) src ac)))]
      [else (die who "invalid" instr)])] 
   [(divsd src dst)
    (cond
      [(and (xmmreg? dst) (or (xmmreg? src) (mem? src)))
       (CODE #xF2 (CODE #x0F ((CODE/digit #x5E dst) src ac)))]
      [else (die who "invalid" instr)])] 
   [(ucomisd src dst)
    (cond
      [(and (xmmreg? dst) (or (xmmreg? src) (mem? src)))
       (CODE #x66 (CODE #x0F ((CODE/digit #x2E dst) src ac)))]
      [else (die who "invalid" instr)])]
   [(seta dst)   (conditional-set  #x97 dst ac)]
   [(setae dst)  (conditional-set  #x93 dst ac)]
   [(setb dst)   (conditional-set  #x92 dst ac)]
   [(setbe dst)  (conditional-set  #x96 dst ac)]
   [(setg dst)   (conditional-set  #x9F dst ac)]
   [(setge dst)  (conditional-set  #x9D dst ac)]
   [(setl dst)   (conditional-set  #x9C dst ac)]
   [(setle dst)  (conditional-set  #x9E dst ac)]
   [(sete dst)   (conditional-set  #x94 dst ac)]
   [(setna dst)  (conditional-set  #x96 dst ac)]
   [(setnae dst) (conditional-set  #x92 dst ac)]
   [(setnb dst)  (conditional-set  #x93 dst ac)]
   [(setnbe dst) (conditional-set  #x97 dst ac)]
   [(setng dst)  (conditional-set  #x9E dst ac)]
   [(setnge dst) (conditional-set  #x9C dst ac)]
   [(setnl dst)  (conditional-set  #x9D dst ac)]
   [(setnle dst) (conditional-set  #x9F dst ac)]
   [(setne dst)  (conditional-set  #x95 dst ac)]
   [(ja dst)     (conditional-jump #x87 dst ac)]
   [(jae dst)    (conditional-jump #x83 dst ac)]
   [(jb dst)     (conditional-jump #x82 dst ac)]
   [(jbe dst)    (conditional-jump #x86 dst ac)]
   [(jg dst)     (conditional-jump #x8F dst ac)]
   [(jge dst)    (conditional-jump #x8D dst ac)]
   [(jl dst)     (conditional-jump #x8C dst ac)]
   [(jle dst)    (conditional-jump #x8E dst ac)]
   [(je dst)     (conditional-jump #x84 dst ac)]
   [(jna dst)    (conditional-jump #x86 dst ac)]
   [(jnae dst)   (conditional-jump #x82 dst ac)]
   [(jnb dst)    (conditional-jump #x83 dst ac)]
   [(jnbe dst)   (conditional-jump #x87 dst ac)]
   [(jng dst)    (conditional-jump #x8E dst ac)]
   [(jnge dst)   (conditional-jump #x8C dst ac)]
   [(jnl dst)    (conditional-jump #x8D dst ac)]
   [(jnle dst)   (conditional-jump #x8F dst ac)]
   [(jne dst)    (conditional-jump #x85 dst ac)]
   [(jo dst)     (conditional-jump #x80 dst ac)]
   [(jp dst)     (conditional-jump #x8A dst ac)]
   [(jnp dst)    (conditional-jump #x8B dst ac)]
   [(byte x)
    (unless (byte? x) (die who "not a byte" x))
    (cons (byte x) ac)]
   [(byte-vector x) (append (map (lambda (x) (byte x)) (vector->list x)) ac)]
   [(int a) (IMM32 a ac)]
   [(label L)
    (unless (symbol? L) (die who "label is not a symbol" L))
    (cons (cons 'label L) ac)]
   [(label-address L)
    (unless (symbol? L) (die who "label-address is not a symbol" L))
    (cons (cons 'label-addr L) ac)]
   [(current-frame-offset)
    (cons '(current-frame-offset) ac)]
   [(nop) ac]

))


(define compute-code-size 
  (lambda (ls)
    (fold (lambda (x ac)
            (if (fixnum? x)
                (fx+ ac 1)
                (case (car x)
                  [(byte) (fx+ ac 1)]
                  [(word reloc-word reloc-word+ label-addr foreign-label 
                    relative local-relative current-frame-offset)
                   (fx+ ac 4)]
                  [(label) ac]
                  [else (die 'compute-code-size "unknown instr" x)])))
          0 
          ls)))


(define set-label-loc!
  (lambda (x loc)
    (when (getprop x '*label-loc*)
      (die 'compile "label is already defined" x))
    (putprop x '*label-loc* loc)))

(define label-loc
  (lambda (x)
    (or (getprop x '*label-loc*)
        (die 'compile "undefined label" x))))


(define unset-label-loc!
  (lambda (x)
    (remprop x '*label-loc*)))


(define set-code-word!
  (lambda (code idx x)
    (cond
      [(fixnum? x) 
       (code-set! code (fx+ idx 0) (fxsll (fxlogand x #x3F) 2))
       (code-set! code (fx+ idx 1) (fxlogand (fxsra x 6) #xFF))
       (code-set! code (fx+ idx 2) (fxlogand (fxsra x 14) #xFF))
       (code-set! code (fx+ idx 3) (fxlogand (fxsra x 22) #xFF))]
      [else (die 'set-code-word! "unhandled" x)])))
 
(define (optimize-local-jumps ls)
  (define locals '())
  (define g (gensym))
  (for-each
    (lambda (x)
      (when (and (pair? x) (eq? (car x) 'label))
        (putprop (cdr x) g 'local)
        (set! locals (cons (cdr x) locals))))
    ls)
  (for-each
    (lambda (x)
      (when (and (pair? x)
                 (eq? (car x) 'relative)
                 (eq? (getprop (cdr x) g) 'local))
        (set-car! x 'local-relative)))
    ls)
  (for-each (lambda (x) (remprop x g)) locals)
  ls)



(define whack-instructions
  (lambda (x ls)
    (define f
      (lambda (ls idx reloc)
        (cond
          [(null? ls) reloc]
          [else
           (let ([a (car ls)])
             (if (fixnum? a)
                 (begin
                   (code-set! x idx a)
                   (f (cdr ls) (fxadd1 idx) reloc))
                 (case (car a)
                  [(byte) 
                   (code-set! x idx (cdr a))
                   (f (cdr ls) (fx+ idx 1) reloc)]
                  [(reloc-word reloc-word+)
                   (f (cdr ls) (fx+ idx 4) (cons (cons idx a) reloc))]
                  [(local-relative relative label-addr foreign-label) 
                   (f (cdr ls) (fx+ idx 4) (cons (cons idx a) reloc))]
                  [(word)
                   (let ([v (cdr a)])
                      (set-code-word! x idx v)
                      (f (cdr ls) (fx+ idx 4) reloc))]
                  [(current-frame-offset)
                   (set-code-word! x idx idx)
                   (f (cdr ls) (fx+ idx 4) reloc)]
                  [(label)
                   (set-label-loc! (cdr a) (list x idx))
                   (f (cdr ls) idx reloc)]
                  [else
                   (die 'whack-instructions "unknown instr" a)])))])))
    (f ls 0 '())))

(define wordsize 4)


(define compute-reloc-size 
  (lambda (ls)
    (fold (lambda (x ac)
            (if (fixnum? x)
                ac
                (case (car x)
                  [(reloc-word foreign-label)        (fx+ ac 2)]
                  [(relative reloc-word+ label-addr) (fx+ ac 3)]
                  [(word byte label current-frame-offset local-relative) ac]
                  [else (die 'compute-reloc-size "unknown instr" x)])))
          0 
          ls)))

(define foreign-string->bytevector
  (let ([mem '()])
    (lambda (x)
      (let f ([ls mem])
        (cond
          [(null? ls) 
           (let ([bv (string->utf8 x)])
             (set! mem (cons (cons x bv) mem))
             bv)]
          [(string=? x (caar ls)) (cdar ls)]
          [else (f (cdr ls))])))))


(define code-entry-adjustment
  (let ([v #f])
    (case-lambda
      [() (or v (die 'code-entry-adjustment "uninitialized"))]
      [(x) (set! v x)])))

(define whack-reloc
  (lambda (thunk?-label code vec)
    (define reloc-idx 0)
    (lambda (r)
      (let ([idx (car r)] [type (cadr r)]
            [v 
             (let ([v (cddr r)])
               (cond
                 [(thunk?-label v) =>
                  (lambda (label)
                    (let ([p (label-loc label)])
                      (cond
                        [(fx= (length p) 2)
                         (let ([code (car p)] [idx (cadr p)])
                           (unless (fx= idx 0)
                             (die 'whack-reloc 
                               "cannot create a thunk pointing"
                               idx))
                           (let ([thunk (code->thunk code)])
                             (set-cdr! (cdr p) (list thunk))
                             thunk))]
                        [else (caddr p)])))]
                 [else v]))])
        (case type
          [(reloc-word)
           (vector-set! vec reloc-idx (fxsll idx 2))
           (vector-set! vec (fx+ reloc-idx 1) v)
           (set! reloc-idx (fx+ reloc-idx 2))]
          [(foreign-label)
           ;;; FIXME: converted strings should be memoized.
           ;;;        wait for equal? hash tables.
           (let ([name 
                  (if (string? v)
                      (foreign-string->bytevector v)
                      (die 'whack-reloc "not a string" v))])
             (vector-set! vec reloc-idx (fxlogor 1 (fxsll idx 2)))
             (vector-set! vec (fx+ reloc-idx 1) name)
             (set! reloc-idx (fx+ reloc-idx 2)))]
          [(reloc-word+)
           (let ([obj (car v)] [disp (cdr v)])
             (vector-set! vec reloc-idx (fxlogor 2 (fxsll idx 2)))
             (vector-set! vec (fx+ reloc-idx 1) disp)
             (vector-set! vec (fx+ reloc-idx 2) obj)
             (set! reloc-idx (fx+ reloc-idx 3)))]
          [(label-addr)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (vector-set! vec reloc-idx (fxlogor 2 (fxsll idx 2)))
               (vector-set! vec (fx+ reloc-idx 1) 
                  (fx+ disp (code-entry-adjustment)))
               (vector-set! vec (fx+ reloc-idx 2) obj)))
           (set! reloc-idx (fx+ reloc-idx 3))]
          [(local-relative)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (unless (eq? obj code)
                 (die 'whack-reloc "local-relative differ"))
               (let ([rel (fx- disp (fx+ idx 4))])
                 (code-set! code (fx+ idx 0) (fxlogand rel #xFF))
                 (code-set! code (fx+ idx 1) (fxlogand (fxsra rel 8) #xFF))
                 (code-set! code (fx+ idx 2) (fxlogand (fxsra rel 16) #xFF))
                 (code-set! code (fx+ idx 3) (fxlogand (fxsra rel 24) #xFF)))))]
          [(relative)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (unless (and (code? obj) (fixnum? disp))
                 (die 'whack-reloc "invalid relative jump obj/disp" obj disp))
               (vector-set! vec reloc-idx (fxlogor 3 (fxsll idx 2)))
               (vector-set! vec (fx+ reloc-idx 1) 
                 (fx+ disp (code-entry-adjustment)))
               (vector-set! vec (fx+ reloc-idx 2) obj)))
           (set! reloc-idx (fx+ reloc-idx 3))]
          [else (die 'whack-reloc "invalid reloc type" type)]))
      )))


;;; (define list->code
;;;   (lambda (ls)
;;;     (let ([ls (convert-instructions ls)])
;;;       (let ([n (compute-code-size ls)]
;;;             [m (compute-reloc-size ls)])
;;;         (let ([x (make-code n m 1)])
;;;           (let ([reloc* (whack-instructions x ls)])
;;;             (for-each (whack-reloc x) reloc*))
;;;           (make-code-executable! x)
;;;           x)))))

  (define assemble-sources
    (lambda (thunk?-label ls*)
      (define (code-list ls)
        (if (let ([a (cadr ls)])
              (and (pair? a) (eq? (car a) 'name)))
            (cddr ls)
            (cdr ls)))
      (define (code-name ls)
        (let ([a (cadr ls)])
           (if (and (pair? a) (eq? (car a) 'name))
               (cadr a)
               #f)))
      (let ([closure-size* (map car ls*)]
            [code-name* (map code-name ls*)]
            [ls* (map code-list ls*)])
        (let* ([ls* (map convert-instructions ls*)]
               [ls* (map optimize-local-jumps ls*)])
          (let ([n* (map compute-code-size ls*)]
                [m* (map compute-reloc-size ls*)])
            (let ([code* (map make-code n* closure-size*)]
                  [relv* (map make-vector m*)])
              (let ([reloc** (map whack-instructions code* ls*)])
                (for-each
                  (lambda (foo reloc*)
                    (for-each (whack-reloc thunk?-label (car foo) (cdr foo)) reloc*))
                  (map cons code* relv*) reloc**)
                (for-each set-code-reloc-vector! code* relv*)
                (for-each (lambda (code name)
                            (when name
                              (set-code-annotation! code name)))
                          code* code-name*)
                code*)))))))
  

)

