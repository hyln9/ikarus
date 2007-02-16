
;;; 
;;; assuming the existence of a code manager, this file defines an assember
;;; that takes lists of assembly code and produces a list of code objects
;;;

   ;;;      add     
   ;;;      and
   ;;;      cmp
   ;;;      call
   ;;;      cltd
   ;;;      idiv
   ;;;      imull
   ;;;      ja
   ;;;      jae
   ;;;      jb
   ;;;      jbe
   ;;;      je
   ;;;      jg
   ;;;      jge
   ;;;      jl
   ;;;      jle
   ;;;      jne
   ;;;      jmp
   ;;;      movb
   ;;;      movl
   ;;;      negl
   ;;;      notl
   ;;;      orl
   ;;;      popl
   ;;;      pushl
   ;;;      ret
   ;;;      sall
   ;;;      sarl
   ;;;      shrl
   ;;;      sete
   ;;;      setg


(let ()

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
    ))
  
(define register-index
  (lambda (x)
    (cond
      [(assq x register-mapping) => caddr]
      [else (error 'register-index "not a register ~s" x)])))

(define reg32?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (fx= (cadr x) 32))]
      [else #f])))

(define reg8?
  (lambda (x)
    (cond
      [(assq x register-mapping) =>
       (lambda (x) (fx= (cadr x) 8))]
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
                               (error 'with-args "too many args")))
                        (error 'with-args "too few args")))
                 (error 'with-args "too few args")))
           (error 'with-args "too few args")))]))


;(define byte
;  (lambda (x)
;    (cons 'byte (fxlogand x 255))))

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
    (list* 'reloc-word+ x d)))

(define byte?
  (lambda (x)
    (and (fixnum? x)
         (fx<= x 127)
         (fx<= -128 x))))

(define mem?
  (lambda (x)
    (and (pair? x)
         ;(fx= (length x) 3)
         (eq? (car x) 'disp)
         ;(or (imm? (cadr x))
         ;    (reg? (cadr x)))
         ;(or (imm? (caddr x))
         ;    (reg? (caddr x)))
         )))

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
           (list* 
             (byte n)
             (byte (fxsra n 8))
             (byte (fxsra n 16))
             (byte (fxsra n 24))
             ac)
           (let* ([lo (remainder n 256)]
                  [hi (quotient (if (< n 0) (- n 255) n) 256)])
             (list* 
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
      [else (error 'IMM32 "invalid ~s" n)])))


(define IMM8
  (lambda (n ac)
    (cond
      [(int? n) 
       (list* (byte n) ac)]
      [else (error 'IMM8 "invalid ~s" n)])))


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
           (error 'assemble "invalid label ~s" x)))
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
           (error 'assemble "invalid label-address ~s" x)))
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
      [else (error 'CODErri "invalid i=~s" i)])))

(define CODErr
  (lambda (c d s ac)
    (CODE c (ModRM 3 d s ac))))

(define CODEri
  (lambda (c d i ac)
    (CODE+r c d (IMM32 i ac))))


(define RegReg
  (lambda (r1 r2 r3 ac)
    (cond
      [(eq? r3 '%esp) (error 'assembler "BUG: invalid src %esp")]
      [(eq? r1 '%ebp) (error 'assembler "BUG: invalid src %ebp")]
      [else 
       (list*
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
      [else (error 'assemble "IMM32*2 ~s ~s" i1 i2)])))


(define CODErd
  (lambda (c r1 disp ac)
    (with-args disp
      (lambda (a1 a2)
        (cond
          [(and (reg? a1) (reg? a2))
           (CODE c (RegReg r1 a1 a2 ac))]
          [(and (imm? a1) (reg? a2))
           (CODErri c r1 a2 a1 ac)]
          [(and (imm? a2) (reg? a1))
           (CODErri c r1 a1 a2 ac)]
          [(and (imm? a1) (imm? a2))
           (CODE c 
             (ModRM 0 r1 '/5 
               (IMM32*2 a1 a2 ac)))]
          [else (error 'CODErd "unhandled ~s" disp)])))))

(define CODEdi
  (lambda (c /? disp n ac)
    (with-args disp
      (lambda (a1 a2)
        (cond
          [(and (reg? a1) (reg? a2)) 
           (error 'CODEdi "unsupported1 ~s" disp)]
          [(and (imm? a1) (reg? a2))
           (CODErri c /? a2 a1 (IMM32 n ac))]
          [(and (imm? a2) (reg? a1))
           (CODErri c /? a1 a2 (IMM32 n ac))]
          [(and (imm? a1) (imm? a2))
           (error 'CODEdi "unsupported2")]
          [else (error 'CODEdi "unhandled ~s" disp)])))))

(define (SIB s i b ac)
  (cons (byte
          (fxlogor
            (register-index b)
            (fxlogor 
              (fxsll (register-index i) 3)
              (fxsll s 6))))
        ac))
;              81 /0 id    ADD r/m32,imm32            Valid Add imm32 to 
(define (CODE/digit c /d)
  (lambda (dst ac)
    (cond
      [(mem? dst) 
       (with-args dst
          (lambda (a0 a1)
            (cond
              [(and (imm8? a0) (reg? a1))
               (CODE c (ModRM 1 /d a1 (IMM8 a0 ac)))]
              [(and (imm? a0) (reg? a1))
               (CODE c (ModRM 2 /d a1 (IMM32 a0 ac)))]
              [(and (imm8? a1) (reg? a0))
               (CODE c (ModRM 1 /d a0 (IMM8 a1 ac)))]
              [(and (reg? a0) (reg? a1)) 
               (CODE c (ModRM 1 /d '/4 (SIB 0 a0 a1 (IMM8 0 ac))))]
              [else (error 'CODE/digit "unhandled ~s ~s" a0 a1)])))]
      [else (error 'CODE/digit "unhandled ~s" dst)])))

;              01 /r      ADD r/m32, r32        Valid Add r32 to r/m32.
;;;(define (CODE/r c /r)
;;;  (lambda (dst ac)
;;;    (cond
;;;      [(mem? dst) 
;;;       (with-args dst
;;;          (lambda (a0 a1)
;;;            (cond
;;;              [(and (imm8? a0) (reg? a1))
;;;               (CODE c (ModRM 1 /r a1 (IMM8 a0 ac)))]
;;;              [else (error 'CODE/r "unhandled ~s ~s" a0 a1)])))]
;;;      [else (error 'CODE/r "unhandled ~s" dst)])))





(define CODEid
  (lambda (c /? n disp ac)
    (with-args disp
      (lambda (a1 a2)
        (cond
          [(and (reg? a1) (reg? a2)) 
           (error 'CODEid "unsupported1 ~s" disp)]
          [(and (imm? a1) (reg? a2))
           (error 'CODEid "unsupported2")
           (CODErri c /? a2 a1 (IMM32 n ac))]
          [(and (imm? a2) (reg? a1))
           (error 'CODEid "unsupported3")
           (CODErri c /? a1 a2 (IMM32 n ac))]
          [(and (imm? a1) (imm? a2))
           (error 'CODEid "unsupported4")]
          [else (error 'CODEid "unhandled ~s" disp)])))))

(define CODEdi8
  (lambda (c /? disp n ac)
    (with-args disp
      (lambda (i r)
        (CODErri c /? r i (IMM8 n ac))))))

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
                (error 'convert-instruction "incorrect args in ~s" a))]
           [(fx= n 1)
            (if (fx= (length args) 1)
                (proc a ac (car args))
                (error 'convert-instruction "incorrect args in ~s" a))]
           [(fx= n 0)
            (if (fx= (length args) 0)
                (proc a ac)
                (error 'convert-instruction "incorrect args in ~s" a))] 
           [else
            (if (fx= (length args) n)
                (apply proc a ac args)
                (error 'convert-instruction "incorrect args in ~s" a))])))]
    [else (error 'convert-instruction "unknown instruction in ~s" a)]))



(define (instr/2 arg1 arg2 ac ircode imcode rrcode rmcode mrcode)
  (cond
    [(imm? arg1)
     (cond
       [(reg? arg2) (CODEri ircode arg2 arg1 ac)]
       [(mem? arg2) (CODEdi imcode '/0 arg2 arg1 ac)]
       [else (error 'instr/2 "invalid args ~s ~s" arg1 arg2)])]
    [(reg? arg1)
     (cond
       [(reg? arg2) (CODErr rrcode arg1 arg2 ac)]
       [(mem? arg2) (CODErd rmcode arg1 arg2 ac)]
       [else (error 'instr/2 "invalid args ~s ~s" arg1 arg2)])]
    [(mem? arg1)
     (cond
       [(reg? arg2) (CODErd mrcode arg2 arg1 ac)]
       [else (error 'instr/2 "invalid args ~s ~s" arg1 arg2)])]
    [else (error 'instr/2 "invalid args ~s ~s" arg1 arg2)]))

(module ()
(define who 'assembler)

(define (conditional-set c dst ac)
  (cond
    [(reg8? dst)
     (CODE #x0F (CODE c (ModRM 3 '/0 dst ac)))]
    [else (error who "invalid condition-set to ~s" dst)]))

(define (conditional-jump c dst ac)
  (cond
    [(imm? dst)
     (CODE #x0F (CODE c (IMM32 dst ac)))]
    [(label? dst)
     (CODE #x0F (CODE c (cons (cons 'relative (label-name dst)) ac)))]
    [else (error who "invalid conditional jump target ~s" dst)]))

(add-instructions instr ac
   [(ret)  (CODE #xC3 ac)]
   [(cltd) (CODE #x99 ac)]
   ;                         ircode imcode rrcode rmcode mrcode)
   [(movl src dst) (instr/2 src dst ac #xB8 #xC7 #x89 #x89 #x8B)]
   [(movb src dst)
    (cond
      [(and (imm8? src) (mem? dst))
       ((CODE/digit #xC6 '/0) dst (IMM8 src ac))]
      [(and (reg8? src) (mem? dst)) (CODErd #x88 src dst ac)]
      [(and (mem? src) (reg8? dst)) (CODErd #x8A dst src ac)]
      [else (error who "invalid ~s" instr)])]
   [(addl src dst)
    (cond   
      [(and (imm8? src) (reg? dst)) 
       (CODE #x83 (ModRM 3 '/0 dst (IMM8 src ac)))]
      [(and (imm? src) (eq? dst '%eax))
       (CODE #x05 (IMM32 src ac))]
      [(and (imm? src) (reg? dst))
       (CODE #x81 (ModRM 3 '/0 dst (IMM32 src ac)))]
      [(and (reg? src) (reg? dst))
       (CODE #x01 (ModRM 3 src dst ac))]
      [(and (mem? src) (reg? dst))
       (CODErd #x03 dst src ac)]
      [(and (imm? src) (mem? dst)) 
       ((CODE/digit #x81 '/0) dst (IMM32 src ac))]
      [(and (reg? src) (mem? dst))
       ((CODE/digit #x01 src) dst ac)]
      [else (error who "invalid ~s" instr)])]
   [(subl src dst)
    (cond   
      [(and (imm8? src) (reg? dst)) 
       (CODE #x83 (ModRM 3 '/5 dst (IMM8 src ac)))]
      [(and (imm? src) (eq? dst '%eax)) 
       (CODE #x2D (IMM32 src ac))]
      [(and (imm? src) (reg? dst))
       (CODE #x81 (ModRM 3 '/5 dst (IMM32 src ac)))]
      [(and (reg? src) (reg? dst))
       (CODE #x29 (ModRM 3 src dst ac))]
      [(and (mem? src) (reg? dst))
       (CODErd #x2B dst src ac)]
      [(and (reg? src) (mem? dst))
       ((CODE/digit #x29 src) dst ac)]
      [else (error who "invalid ~s" instr)])]
   [(sall src dst)
    (cond
      [(and (equal? 1 src) (reg? dst))
       (CODE #xD1 (ModRM 3 '/4 dst ac))]
      [(and (imm8? src) (reg? dst))
       (CODE #xC1 (ModRM 3 '/4 dst (IMM8 src ac)))]
      [(and (imm8? src) (mem? dst))
       ((CODE/digit #xC1 '/4) dst (IMM8 src ac))]
      [(and (eq? src '%cl) (reg? dst))
       (CODE #xD3 (ModRM 3 '/4 dst ac))]
      [else (error who "invalid ~s" instr)])]
   [(shrl src dst)
    (cond
      [(and (equal? 1 src) (reg? dst))
       (CODE #xD1 (ModRM 3 '/5 dst ac))]
      [(and (imm8? src) (reg? dst))
       (CODE #xC1 (ModRM 3 '/5 dst (IMM8 src ac)))]
      [(and (eq? src '%cl) (reg? dst))
       (CODE #xD3 (ModRM 3 '/5 dst ac))]
      [else (error who "invalid ~s" instr)])]
   [(sarl src dst)
    (cond
      [(and (equal? 1 src) (reg? dst))
       (CODE #xD1 (ModRM 3 '/7 dst ac))]
      [(and (imm8? src) (reg? dst))
       (CODE #xC1 (ModRM 3 '/7 dst (IMM8 src ac)))]
      [(and (imm8? src) (mem? dst))
       ((CODE/digit #xC1 '/7) dst (IMM8 src ac))] 
      [(and (eq? src '%cl) (reg? dst))
       (CODE #xD3 (ModRM 3 '/7 dst ac))]
      [else (error who "invalid ~s" instr)])]
   [(andl src dst) 
    (cond
      [(and (imm? src) (mem? dst))
       ((CODE/digit #x81 '/4) dst (IMM32 src ac))]
      [(and (imm8? src) (reg? dst)) 
       (CODE #x83 (ModRM 3 '/4 dst (IMM8 src ac)))]
      [(and (imm? src) (eq? dst '%eax)) 
       (CODE #x25 (IMM32 src ac))]
      [(and (imm? src) (reg? dst))
       (CODE #x81 (ModRM 3 '/4 dst (IMM32 src ac)))]
      [(and (reg? src) (reg? dst))
       (CODE #x21 (ModRM 3 src dst ac))]
      [(and (reg? src) (mem? dst))
       ((CODE/digit #x21 src) dst ac)]
      [(and (mem? src) (reg? dst))
       (CODErd #x23 dst src ac)]
      [else (error who "invalid ~s" instr)])]
   [(orl src dst) 
    (cond
      [(and (imm? src) (mem? dst))
       ((CODE/digit #x81 '/1) dst (IMM32 src ac))]
      [(and (imm8? src) (reg? dst)) 
       (CODE #x83 (ModRM 3 '/1 dst (IMM8 src ac)))]
      [(and (imm? src) (eq? dst '%eax)) 
       (CODE #x0D (IMM32 src ac))]
      [(and (imm? src) (reg? dst))
       (CODE #x81 (ModRM 3 '/1 dst (IMM32 src ac)))]
      [(and (reg? src) (reg? dst))
       (CODE #x09 (ModRM 3 src dst ac))]
      [(and (mem? src) (reg? dst))
       (CODErd #x0B dst src ac)]
      [else (error who "invalid ~s" instr)])]
   [(xorl src dst) 
    (cond
      [(and (imm8? src) (reg? dst)) 
       (CODE #x83 (ModRM 3 '/6 dst (IMM8 src ac)))]
      [(and (imm? src) (eq? dst '%eax)) 
       (CODE #x35 (IMM32 src ac))]
      [(and (reg? src) (reg? dst))
       (CODE #x31 (ModRM 3 src dst ac))]
      [(and (mem? src) (reg? dst))
       (CODErd #x33 dst src ac)]
      [else (error who "invalid ~s" instr)])]
   [(leal src dst) 
    (cond
      [(and (mem? src) (reg? dst))
       (CODErd #x8D dst src ac)]
      [else (error who "invalid ~s" instr)])]
   [(cmpl src dst)
    (cond
      [(and (imm8? src) (reg? dst)) 
       (CODE #x83 (ModRM 3 '/7 dst (IMM8 src ac)))]
      [(and (imm? src) (eq? dst '%eax)) 
       (CODE #x3D (IMM32 src ac))]
      [(and (imm? src) (reg? dst))
       (CODE #x81 (ModRM 3 '/7 dst (IMM32 src ac)))]
      [(and (reg? src) (reg? dst))
       (CODE #x39 (ModRM 3 src dst ac))]
      [(and (mem? src) (reg? dst))
       (CODErd #x3B dst src ac)]
      [(and (imm8? src) (mem? dst))
       (CODErd #x83 '/7 dst (IMM8 src ac))]
      [(and (imm? src) (mem? dst))
       (CODErd #x81 '/7 dst (IMM32 src ac))]
      [else (error who "invalid ~s" instr)])]
   [(imull src dst)
    (cond
      [(and (imm8? src) (reg? dst)) 
       (CODE #x6B (ModRM 3 dst dst (IMM8 src ac)))]
      [(and (imm? src) (reg? dst))
       (CODE #x69 (ModRM 3 dst dst (IMM32 src ac)))]
      [(and (reg? src) (reg? dst))
       (CODE #x0F (CODE #xAF (ModRM 3 dst src ac)))]
      [(and (mem? src) (reg? dst))
       (CODE #x0F (CODErd #xAF dst src ac))]
      [else (error who "invalid ~s" instr)])]
   [(idivl dst)
    (cond
      [(reg? dst)
       (CODErr #xF7 '/7 dst ac)]
      [(mem? dst)
       (CODErd #xF7 '/7 dst ac)]
      [else (error who "invalid ~s" instr)])]
   [(pushl dst)
    (cond
      [(imm8? dst) 
       (CODE #x6A (IMM8 dst ac))]
      [(imm? dst)
       (CODE #x68 (IMM32 dst ac))]
      [(reg? dst)
       (CODE+r #x50 dst ac)]
      [(mem? dst)
       (CODErd #xFF '/6 dst ac)]
      [else (error who "invalid ~s" instr)])] 
   [(popl dst)
    (cond
      [(reg? dst)
       (CODE+r #x58 dst ac)]
      [(mem? dst)
       (CODErd #x8F '/0 dst ac)]
      [else (error who "invalid ~s" instr)])] 
   [(notl dst)
    (cond
      [(reg? dst)
       (CODE #xF7 (ModRM 3 '/2 dst ac))]
      [(mem? dst)
       (CODErd #xF7 '/7 dst ac)]
      [else (error who "invalid ~s" instr)])]
   [(negl dst)
    (cond
      [(reg? dst)
       (CODE #xF7 (ModRM 3 '/3 dst ac))]
      [else (error who "invalid ~s" instr)])]
   [(jmp dst)
    (cond
      [(label? dst)
       (CODE #xE9 (cons (cons 'relative (label-name dst)) ac))]
      [(imm? dst) 
       (CODE #xE9 (IMM32 dst ac))]
      [(mem? dst)
       (CODErd #xFF '/4 dst ac)]
      [else (error who "invalid jmp target ~s" dst)])]
   [(call dst)
    (cond
      [(imm? dst) 
       (CODE #xE8 (IMM32 dst ac))]
      [(label? dst)
       (CODE #xE8 (cons (cons 'relative (label-name dst)) ac))]
      [(mem? dst)
       (CODErd #xFF '/2 dst ac)]
      [(reg? dst)
       (CODE #xFF (ModRM 3 '/2 dst ac))]
      [else (error who "invalid jmp target ~s" dst)])]
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
   [(byte x)
    (unless (byte? x) (error who "~s is not a byte" x))
    (cons (byte x) ac)]
   [(byte-vector x) (append (map (lambda (x) (byte x)) (vector->list x)) ac)]
   [(int a) (IMM32 a ac)]
   [(label L)
    (unless (symbol? L) (error who "label ~s is not a symbol" L))
    (cons (cons 'label L) ac)]
   [(label-address L)
    (unless (symbol? L) (error who "label-address ~s is not a symbol" L))
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
                  [else (error 'compute-code-size "unknown instr ~s" x)])))
          0 
          ls)))


(define set-label-loc!
  (lambda (x loc)
    (when (getprop x '*label-loc*)
      (error 'compile "label ~s is already defined" x))
    (putprop x '*label-loc* loc)))

(define label-loc
  (lambda (x)
    (or (getprop x '*label-loc*)
        (error 'compile "undefined label ~s" x))))


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
      [else (error 'set-code-word! "unhandled ~s" x)])))
 
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
                   (error 'whack-instructions "unknown instr ~s" a)])))])))
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
                  [(word byte label current-frame-offset local-relative)    ac]
                  [else (error 'compute-reloc-size "unknown instr ~s" x)])))
          0 
          ls)))

(define whack-reloc
  (lambda (thunk?-label code vec)
    (define reloc-idx 0)
    (lambda (r)
      ;(printf "r=~s\n" r)
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
                             (error 'whack-reloc 
                               "cannot create a thunk pointing at ~s"
                               idx))
                           (let ([thunk ($code->closure code)])
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
           (vector-set! vec reloc-idx (fxlogor 1 (fxsll idx 2)))
           (vector-set! vec (fx+ reloc-idx 1) v)
           (set! reloc-idx (fx+ reloc-idx 2))]
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
               (vector-set! vec (fx+ reloc-idx 1) (fx+ disp 11))
               (vector-set! vec (fx+ reloc-idx 2) obj)))
           (set! reloc-idx (fx+ reloc-idx 3))]
          [(local-relative)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (unless (eq? obj code)
                 (error 'whack-reloc "local-relative differ"))
               (let ([rel (fx- disp (fx+ idx 4))])
                 (code-set! code (fx+ idx 0) (fxlogand rel #xFF))
                 (code-set! code (fx+ idx 1) (fxlogand (fxsra rel 8) #xFF))
                 (code-set! code (fx+ idx 2) (fxlogand (fxsra rel 16) #xFF))
                 (code-set! code (fx+ idx 3) (fxlogand (fxsra rel 24) #xFF)))))]
          [(relative)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [disp (cadr loc)])
               (vector-set! vec reloc-idx (fxlogor 3 (fxsll idx 2)))
               (vector-set! vec (fx+ reloc-idx 1) (fx+ disp 11))
               (vector-set! vec (fx+ reloc-idx 2) obj)))
           (set! reloc-idx (fx+ reloc-idx 3))]
          [else (error 'whack-reloc "invalid reloc type ~s" type)]))
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

(define list*->code*
  (lambda (thunk?-label ls*)
    (let ([closure-size* (map car ls*)]
          [ls* (map cdr ls*)])
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
              code*)))))))

;(define list->code
;  (lambda (ls)
;    (car (list*->code* (list ls)))))

(primitive-set! 'list*->code* list*->code*)

)

