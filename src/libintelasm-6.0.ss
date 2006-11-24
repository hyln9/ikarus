
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
    [/0   0  0]
    [/1   0  1]
    [/2   0  2]
    [/3   0  3]
    [/4   0  4]
    [/5   0  5]
    [/6   0  6]
    [/7   0  7]
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

(define check-len
  (lambda (x)
    (define instr-len
      '([ret]
        [movl s d]
        [movb s d]
        [addl s d]
        [subl s d]
        [sall s d]
        [sarl s d]
        [andl s d]
        [xorl s d]
        [orl s d]
        [cmpl s d]
        [imull s d]
        [notl d]
        [negl d]
        [idivl d]
        [pushl d]
        [popl d]
        [jmp d]
        [call d]
        [ja d]
        [jae d]
        [jb d]
        [jbe d]
        [je d]
        [jg d]
        [jge d]
        [jl d]
        [jle d]
        [jna d]
        [jnae d]
        [jnb d]
        [jnbe d]
        [jne d]
        [jng d]
        [jnge d]
        [jnl d]
        [jnle d]
        [seta d]
        [setae d]
        [setb d]
        [setbe d]
        [sete d]
        [setg d]
        [setge d]
        [setl d]
        [setle d]
        [setna d]
        [setnae d]
        [setnb d]
        [setnbe d]
        [setne d]
        [setng d]
        [setnge d]
        [setnl d]
        [setnle d]
        [cltd]
        [nop]
        [byte x]
        [byte-vector x]
        [int x]
        [label x]
        [label-address x]
        [current-frame-offset]
        ))
    (cond
      [(assq (car x) instr-len) =>
       (lambda (p) 
         (unless (fx= (length x) (length p))
           (error 'assembler "invalid instruction format ~s" x)))]
      [else (error 'assembler "unknown instruction ~s" x)])))

(define with-args
  (lambda (ls f)
    (apply f (cdr ls))))

(define byte
  (lambda (x)
    (cons 'byte (fxlogand x 255))))


(define word
  (lambda (x)
    (cons 'word x)))

(define reloc-word
  (lambda (x)
    (cons 'reloc-word x)))

(define reloc-word+
  (lambda (x d)
    (list* 'reloc-word+ x d)))

(define list*-aux
  (lambda (ls ls*)
    (cond
      [(null? ls*) ls]
      [else (cons ls (list*-aux (car ls*) (cdr ls*)))])))

(define list*
  (lambda (ls . ls*)
    (list*-aux ls ls*)))

(define byte?
  (lambda (x)
    (and (fixnum? x)
         (fx<= x 127)
         (fx<= -128 x))))

(define mem?
  (lambda (x)
    (and (list? x)
         (fx= (length x) 3)
         (eq? (car x) 'disp)
         (or (imm? (cadr x))
             (reg? (cadr x)))
         (or (imm? (caddr x))
             (reg? (caddr x))))))

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
       (let ([n (cadr n)])
         (list* (byte n)
           (byte (fxsra n  8))
           (byte (fxsra n 16))
           (byte (fxsra n 24))
           ac))]
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
       (let ([n (cadr n)])
         (list* (byte n) ac))]
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
    (and (int? x) (byte? (cadr x)))))

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

(define int?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'int))))

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
      [(reg? i) 
       (CODE c (ModRM i d s ac))]
      [else
       (CODE c (ModRM 2 d s (IMM32 i ac)))])))

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
;;;       (parameterize ([print-radix 16])
;;;          (printf "REGREG ~s ~s ~s\n" r1 r2 r3)
;;;          (printf "REGREG ~s ~s\n" 
;;;            (byte (fxlogor 4 (fxsll (register-index r1) 3)))
;;;            (byte (fxlogor (register-index r2) 
;;;                           (fxsll (register-index r3) 3)))))
       (list*
         (byte (fxlogor 4 (fxsll (register-index r1) 3)))
         (byte (fxlogor (register-index r2) 
                        (fxsll (register-index r3) 3)))
         ac)])))


#;(define CODErd
  (lambda (c r1 disp ac)
    (with-args disp
      (lambda (i/r r2)
        (if (reg? i/r)
            (CODE c (RegReg r1 i/r r2 ac))
            (CODErri c r1 r2 i/r ac))))))


(define IMM32*2
  (lambda (i1 i2 ac)
    (cond
      [(and (int? i1) (obj? i2))
       (let ([d (cadr i1)] [v (cadr i2)])
         (cons (reloc-word+ v d) ac))]
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
          [(and (imm? a1) (imm? a2))
           (CODE c 
             (ModRM 0 r1 '/5 
               (IMM32*2 a1 a2 ac)))]
          [else (error 'CODErd "unhandled ~s" disp)])))))

(define CODEdi
  (lambda (c disp n ac)
    (with-args disp
      (lambda (i r)
        (CODErri c '/0 r i (IMM32 n ac))))))

(define CODEdi8
  (lambda (c disp n ac)
    (with-args disp
      (lambda (i r)
        (CODErri c '/0 r i (IMM8 n ac))))))




(define convert-instruction
  (lambda (a ac)
    (define who 'assemble)
    (check-len a)
    (case (car a)
      [(ret) (CODE #xC3 ac)]
      [(cltd) (CODE #x99 ac)]
      [(movl)
       (with-args a
         (lambda (src dst)
           (cond
             [(and (imm? src) (reg? dst))  (CODEri #xB8 dst src ac)]
             [(and (imm? src) (mem? dst))  (CODEdi #xC7 dst src ac)]
             [(and (reg? src) (reg? dst))  (CODErr #x89 src dst ac)]
             [(and (reg? src) (mem? dst))  (CODErd #x89 src dst ac)]
             [(and (mem? src) (reg? dst))  (CODErd #x8B dst src ac)] 
             [else (error who "invalid ~s" a)])))]
      [(movb)
       (with-args a
         (lambda (src dst)
           (cond
             ;[(and (imm8? src) (reg8? dst))  (CODEri #xB0 dst src ac)]
             [(and (imm8? src) (mem? dst)) (CODEdi8 #xC6 dst src ac)]
             ;[(and (reg8? src) (reg8? dst))  (CODErr #x88 src dst ac)]
             [(and (reg8? src) (mem? dst))   (CODErd #x88 src dst ac)]
             [(and (mem? src) (reg8? dst))   (CODErd #x8A dst src ac)] 
             [else (error who "invalid ~s" a)])))]
      [(addl)
       (with-args a
         (lambda (src dst)
           (cond   
             ;;; add imm -> reg
             [(and (imm8? src) (reg? dst)) 
              (CODE #x83 (ModRM 3 '/0 dst (IMM8 src ac)))]
             [(and (imm? src) (eq? dst '%eax)) 
              (CODE #x05 (IMM32 src ac))]
             [(and (imm? src) (reg? dst))
              (CODE #x81 (ModRM 3 '/0 dst (IMM32 src ac)))]
             ;;; add reg -> reg
             [(and (reg? src) (reg? dst))
              (CODE #x01 (ModRM 3 src dst ac))]
             ;;; add mem -> reg
             [(and (mem? src) (reg? dst))
              (CODErd #x03 dst src ac)]
             ;;; add imm -> mem (not needed)
             ;;; add reg -> mem (not needed)
             [else (error who "invalid ~s" a)])))]
      [(subl)
       (with-args a
         (lambda (src dst)
           (cond   
             ;;; imm -> reg
             [(and (imm8? src) (reg? dst)) 
              (CODE #x83 (ModRM 3 '/5 dst (IMM8 src ac)))]
             [(and (imm? src) (eq? dst '%eax)) 
              (CODE #x2D (IMM32 src ac))]
             [(and (imm? src) (reg? dst))
              (CODE #x81 (ModRM 3 '/5 dst (IMM32 src ac)))]
             ;;; reg -> reg
             [(and (reg? src) (reg? dst))
              (CODE #x29 (ModRM 3 src dst ac))]
             ;;; mem -> reg
             [(and (mem? src) (reg? dst))
              (CODErd #x2B dst src ac)]
             ;;; imm -> mem (not needed)
             ;;; reg -> mem (not needed)
             [else (error who "invalid ~s" a)])))] 
      [(sall)
       (with-args a
         (lambda (src dst)
           (cond
             [(and (equal? '(int 1) src) (reg? dst))
              (CODE #xD1 (ModRM 3 '/4 dst ac))]
             [(and (imm8? src) (reg? dst))
              (CODE #xC1 (ModRM 3 '/4 dst (IMM8 src ac)))]
             [(and (eq? src '%cl) (reg? dst))
              (CODE #xD3 (ModRM 3 '/4 dst ac))]
             [else (error who "invalid ~s" a)])))]
      [(sarl)
       (with-args a
         (lambda (src dst)
           (cond
             [(and (equal? '(int 1) src) (reg? dst))
              (CODE #xD1 (ModRM 3 '/7 dst ac))]
             [(and (imm8? src) (reg? dst))
              (CODE #xC1 (ModRM 3 '/7 dst (IMM8 src ac)))]
             [(and (eq? src '%cl) (reg? dst))
              (CODE #xD3 (ModRM 3 '/7 dst ac))]
             [else (error who "invalid ~s" a)])))]
      [(andl)  ; similar to add
       (with-args a
         (lambda (src dst)
           (cond
             ;;; and imm -> reg
             [(and (imm8? src) (reg? dst)) 
              (CODE #x83 (ModRM 3 '/4 dst (IMM8 src ac)))]
             [(and (imm? src) (eq? dst '%eax)) 
              (CODE #x25 (IMM32 src ac))]
             [(and (imm? src) (reg? dst))
              (CODE #x81 (ModRM 3 '/4 dst (IMM32 src ac)))]
             ;;; and reg -> reg
             [(and (reg? src) (reg? dst))
              (CODE #x21 (ModRM 3 src dst ac))]
             ;;; and mem -> reg
             [(and (mem? src) (reg? dst))
              (CODErd #x23 dst src ac)]
             [else (error who "invalid ~s" a)])))]
      [(orl)  ; similar to add
       (with-args a
         (lambda (src dst)
           (cond
             ;;; or imm -> reg
             [(and (imm8? src) (reg? dst)) 
              (CODE #x83 (ModRM 3 '/1 dst (IMM8 src ac)))]
             [(and (imm? src) (eq? dst '%eax)) 
              (CODE #x0D (IMM32 src ac))]
             [(and (imm? src) (reg? dst))
              (CODE #x81 (ModRM 3 '/1 dst (IMM32 src ac)))]
             ;;; or reg -> reg
             [(and (reg? src) (reg? dst))
              (CODE #x09 (ModRM 3 src dst ac))]
             ;;; or mem -> reg
             [(and (mem? src) (reg? dst))
              (CODErd #x0B dst src ac)]
             [else (error who "invalid ~s" a)])))]
      [(xorl)  ; similar to add
       (with-args a
         (lambda (src dst)
           (cond
             ;;; or imm -> reg
             ;[(and (imm8? src) (reg? dst)) 
             ; (CODE #x83 (ModRM 3 '/1 dst (IMM8 src ac)))]
             ;[(and (imm? src) (eq? dst '%eax)) 
             ; (CODE #x0D (IMM32 src ac))]
             ;[(and (imm? src) (reg? dst))
             ; (CODE #x81 (ModRM 3 '/1 dst (IMM32 src ac)))]
             ;;; or reg -> reg
             [(and (reg? src) (reg? dst))
              (CODE #x31 (ModRM 3 src dst ac))]
             ;;; or mem -> reg
             [(and (mem? src) (reg? dst))
              (CODErd #x33 dst src ac)]
             [else (error who "invalid ~s" a)])))]
      [(cmpl)
       (with-args a
         (lambda (src dst)
           (cond
             [(and (imm8? src) (reg? dst)) 
              (CODE #x83 (ModRM 3 '/7 dst (IMM8 src ac)))]
             [(and (imm? src) (eq? dst '%eax)) 
              (CODE #x3D (IMM32 src ac))]
             [(and (reg? src) (reg? dst))
              (CODE #x39 (ModRM 3 src dst ac))]
             [(and (mem? src) (reg? dst))
              (CODErd #x3B dst src ac)]
             [(and (imm8? src) (mem? dst))
              (CODErd #x83 '/7 dst (IMM8 src ac))]
             [(and (imm? src) (mem? dst))
              (CODErd #x81 '/7 dst (IMM32 src ac))]
             [else (error who "invalid ~s" a)])))]
      [(imull)
       (with-args a
         (lambda (src dst)
           (cond
             [(and (imm8? src) (reg? dst)) 
              (CODE #x6B (ModRM 3 dst dst (IMM8 src ac)))]
             [(and (imm? src) (reg? dst))
              (CODE #x69 (ModRM 3 dst dst (IMM32 src ac)))]
             [(and (reg? src) (reg? dst))
              (CODE #x0F (CODE #xAF (ModRM 3 dst src ac)))]
             [(and (mem? src) (reg? dst))
              (CODE #x0F (CODErd #xAF dst src ac))]
             [else (error who "invalid ~s" a)])))]
      [(idivl)
       (with-args a
         (lambda (dst)
           (cond
             [(reg? dst)
              (CODErr #xF7 '/7 dst ac)]
             [(mem? dst)
              (CODErd #xF7 '/7 dst ac)]
             [else (error who "invalid ~s" a)])))]
      [(pushl)
       (with-args a
         (lambda (dst)
           (cond
             [(imm8? dst) 
              (CODE #x6A (IMM8 dst ac))]
             [(imm? dst)
              (CODE #x68 (IMM32 dst ac))]
             [(reg? dst)
              (CODE+r #x50 dst ac)]
             [(mem? dst)
              (CODErd #xFF '/6 dst ac)]
             [else (error who "invalid ~s" a)])))] 
      [(popl)
       (with-args a
         (lambda (dst)
           (cond
             [(reg? dst)
              (CODE+r #x58 dst ac)]
             [(mem? dst)
              (CODErd #x8F '/0 dst ac)]
             [else (error who "invalid ~s" a)])))] 
      [(notl)
       (with-args a
         (lambda (dst)
           (cond
             [(reg? dst)
              (CODE #xF7 (ModRM 3 '/2 dst ac))]
             [(mem? dst)
              (CODErd #xF7 '/7 dst ac)]
             [else (error who "invalid ~s" a)])))]
      [(negl)
       (with-args a
         (lambda (dst)
           (cond
             [(reg? dst)
              (CODE #xF7 (ModRM 3 '/3 dst ac))]
             [else (error who "invalid ~s" a)])))]
      [(jmp)
       (with-args a
         (lambda (dst)
           (cond
             [(label? dst)
              (CODE #xE9 (cons (cons 'relative (label-name dst)) ac))]
             [(imm? dst) 
              (CODE #xE9 (IMM32 dst ac))]
             [(mem? dst)
              (CODErd #xFF '/4 dst ac)]
             [else (error who "invalid jmp in ~s" a)])))]
      [(call)
       (with-args a
         (lambda (dst)
           (cond
             [(imm? dst) 
              (CODE #xE8 (IMM32 dst ac))]
             [(label? dst)
              (CODE #xE8 (cons (cons 'relative (label-name dst)) ac))]
             [(mem? dst)
              (CODErd #xFF '/2 dst ac)]
             [(reg? dst)
              (CODE #xFF (ModRM 3 '/2 dst ac))]
             [else (error who "invalid jmp in ~s" a)])))]
      [(seta  setae  setb  setbe  sete  setg  setge  setl  setle
        setna setnae setnb setnbe setne setng setnge setnl setnle)
       (let* ([table
               '([seta  #x97] [setna  #x96] 
                 [setae #x93] [setnae #x92]
                 [setb  #x92] [setnb  #x93]
                 [setbe #x96] [setnbe #x97]
                 [setg  #x9F] [setng  #x9E]
                 [setge #x9D] [setnge #x9C]
                 [setl  #x9C] [setnl  #x9D]
                 [setle #x9E] [setnle #x9F]
                 [sete  #x94] [setne  #x95])]
              [lookup
               (lambda (x)
                 (cond
                   [(assq x table) => cadr]
                   [else (error who "invalid cset ~s" x)]))])
         (with-args a
           (lambda (dst)
             (cond
               [(reg8? dst)
                (CODE #x0F 
                  (CODE (lookup (car a)) 
                    (ModRM 3 '/0 dst ac)))]
               [else (error who "invalid ~s" a)]))))]
      [(ja  jae  jb  jbe  je  jg  jge  jl  jle
        jna jnae jnb jnbe jne jng jnge jnl jnle)
       (let* ([table 
               '([je #x84]  [jne #x85]
                 [ja #x87]  [jna #x86]
                 [jae #x83] [jnae #x82] 
                 [jb #x82]  [jnb #x83]
                 [jbe #x86] [jnbe #x87] 
                 [jg #x8F]  [jng #x8E] 
                 [jge #x8D] [jnge #x8C]
                 [jl #x8C]  [jnl #x8D]
                 [jle #x8E] [jnle #x8F])]
              [lookup
               (lambda (x)
                 (cond
                   [(assq x table) => cadr]
                   [else (error who "invalid cmp ~s" x)]))])
         (with-args a
           (lambda (dst)
             (cond
               [(imm? dst)
                (CODE #x0F (CODE (lookup (car a)) (IMM32 dst ac)))]
               [(label? dst)
                (CODE #x0F 
                  (CODE (lookup (car a)) 
                    (cons (cons 'relative (label-name dst)) ac)))]
               [else (error who "invalid ~s" a)]))))]
      [(byte) 
       (with-args a
         (lambda (x)
           (unless (byte? x) (error who "invalid instruction ~s" a))
           (cons (byte x) ac)))]
      [(byte-vector)
       (with-args a
         (lambda (x) (append (map byte (vector->list x)) ac)))]
      [(int) (IMM32 a ac)] 
      [(label)
       (with-args a 
         (lambda (L)
           (unless (symbol? L) (error who "invalid instruction ~s" a))
           (cons (cons 'label L) ac)))]
      [(label-address)
       (with-args a 
         (lambda (L)
           (unless (symbol? L) (error who "invalid instruction ~s" a))
           (cons (cons 'label-addr L) ac)))]
      [(current-frame-offset)
       (cons '(current-frame-offset) ac)]
      [(nop) ac]
      [else
       (error who "unknown instruction ~s" a)])))

(define diff
  (lambda (ls x)
    (cond
      [(eq? ls x) '()]
      [else (cons (car ls) (diff (cdr ls) x))])))

(define hex-table
  '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
     #\8 #\9 #\A #\B #\C #\D #\E #\F))

(define write/x
  (lambda (x)
    (case (car x)
      [(byte)
       (display "0x")
       (display (vector-ref hex-table (fxsra (cdr x) 4)))
       (display (vector-ref hex-table (fxlogand (cdr x) 15)))
       (display " ")]
      [else (write x)])))


(define compute-code-size 
  (lambda (ls)
    (fold (lambda (x ac)
            (case (car x)
              [(byte) (fx+ ac 1)]
              [(word reloc-word reloc-word+ label-addr foreign-label 
                relative current-frame-offset)
               (fx+ ac 4)]
              [(label) ac]
              [else (error 'compute-code-size "unknown instr ~s" x)]))
          0 
          ls)))


(define compute-reloc-size 
  (lambda (ls)
    (fold (lambda (x ac)
            (case (car x)
              [(reloc-word )       (fx+ ac 4)]
              [(reloc-word+)      (fx+ ac 8)]
              [(relative label-addr foreign-label)         (fx+ ac 8)]
              [(word byte label current-frame-offset)   ac]
              [else (error 'compute-reloc-size "unknown instr ~s" x)]))
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


(define whack-instructions
  (lambda (x ls)
    (define f
      (lambda (ls idx reloc)
        (cond
          [(null? ls) reloc]
          [else
           (let ([a (car ls)])
             (case (car a)
               [(byte) 
                (set-code-byte! x idx (cdr a))
                (f (cdr ls) (fx+ idx 1) reloc)]
               [(reloc-word reloc-word+)
                (f (cdr ls) (fx+ idx 4) (cons (cons idx a) reloc))]
               [(relative label-addr foreign-label) 
                (f (cdr ls) (fx+ idx 4) (cons (cons idx a) reloc))]
               [(word)
                (let ([v (cdr a)])
                   (set-code-word! x idx v)
                   (f (cdr ls) (fx+ idx 4) reloc))]
               [(current-frame-offset)
                (set-code-word! x idx idx)
                (f (cdr ls) (fx+ idx 4) reloc)]
               [(label)
                (set-label-loc! (cdr a) (cons x idx))
                (f (cdr ls) idx reloc)]
               [else
                (error 'whack-instructions "unknown instr ~s" a)]))])))
    (f ls 0 '())))

(define wordsize 4)

(define whack-reloc
  (lambda (code)
    (define reloc-idx 0)
    (lambda (r)
      (let ([idx (car r)] [type (cadr r)] [v (cddr r)])
        (case type
          [(reloc-word)
           (set-code-object! code v idx reloc-idx)
           (set! reloc-idx (fxadd1 reloc-idx))]
          [(foreign-label)
           (set-code-foreign-object! code v idx reloc-idx)
           (set! reloc-idx (fx+ reloc-idx 2))]
          [(reloc-word+)
           (let ([obj (car v)] [disp (cdr v)])
             (set-code-object+offset! code obj idx disp reloc-idx)
             (set! reloc-idx (fx+ reloc-idx 2)))]
          [(label-addr)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [off (cdr loc)])
               (set-code-object+offset! 
                 code obj idx (fx+ off 11) reloc-idx)))
           (set! reloc-idx (fx+ reloc-idx 2))]
          [(relative)
           (let ([loc (label-loc v)])
             (let ([obj (car loc)] [off (cdr loc)])
               (set-code-object+offset/rel! 
                 code obj idx (fx+ off 11) reloc-idx)))
           (set! reloc-idx (fx+ reloc-idx 2))]
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
  (lambda (ls*)
    (let ([closure-size* (map car ls*)]
          [ls* (map cdr ls*)])
      (let ([ls* (map convert-instructions ls*)])
        (let ([n* (map compute-code-size ls*)]
              [m* (map compute-reloc-size ls*)])
          (let ([code* (map (lambda (n m c) (make-code n m c))
                            n* 
                            m*
                            closure-size*)])
            (let ([reloc** (map whack-instructions code* ls*)])
              (for-each
                (lambda (code reloc*)
                  (for-each (whack-reloc code) reloc*))
                code* reloc**)
              (for-each make-code-executable! code*)
              code*)))))))

(define list->code
  (lambda (ls)
    (car (list*->code* (list ls)))))

(primitive-set! 'list*->code* list*->code*)
)
