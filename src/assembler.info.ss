
;;; Instruction format:
;;; 0,1,2,3,4 byte prefixes 
;;; 1,2,3 byte opcode
;;; 0,1 byte ModR/M
;;; 0,1 byte SIB
;;; 0,1,2,4 bytes address displacement
;;; 0,1,2,4 bytes immediate 
;;;
;;; Prefixes:
;;;   0 to 4 prefixes are permitted.  Up to one prefix from each of the
;;;   following groups is permitted (in any order)
;;;   Group 1: Lock and Repeat
;;;     0xF0 -- LOCK
;;;     0xF2 -- REPNE/REPNZ (for string instructions)
;;;     0xF3 -- REPE/REPX (for string instructions) 
;;;   Group 2: Segment override and branch hints
;;;     0x2E -- CS segment override
;;;     0x36 -- SS 
;;;     0x3E -- DS
;;;     0x26 -- ES
;;;     0x64 -- FS
;;;     0x65 -- GS
;;;   Group 3: 
;;;     0x66 -- Operand-size override
;;;   Group 4:
;;;     0x67 -- Address-size override
;;;
;;; Opcodes:
;;;   * Either 1 byte opcode
;;;   * Or 2-bytes formed by 0x0F escape opcode followed by a second opcode
;;;   * Or 3-bytes formed by 0x66,0xF2,0xF3 prefix followed by escape opcode,
;;;     then a second opcode
;;;
;;; Mod/RM:  1 byte
;;;           ._________._____________.___________.
;;;     Bits: |  7   6  |  5   4   3  |  2  1  0  |
;;;           | mod     | reg/opcode  | R/M       |
;;;           `~~~~~~~~~^~~~~~~~~~~~~~^~~~~~~~~~~~'
;;;  Refer to table 2-2 Page 39 from IA32 Vol2A instruction set reference
;;;
;;;  Mod: 
;;;    0b00 -- direct dereference (i.e.  [EAX], [ECX], ... , sib, disp32)
;;;    0b01 -- deref + 8-bit disp (i.e.  [EAX]+disp8, ...)
;;;    0b10 -- deref + 32-bit disp
;;;    0b11 -- register name (i.e. EAX, ECX, ...)
;;;
;;;  R/M: In general, the register names are as follows:
;;;     0b000 -- eax
;;;     0b001 -- ecx
;;;     0b010 -- edx
;;;     0b011 -- ebx
;;;     0b100 -- esp
;;;     0b101 -- ebp
;;;     0b110 -- esi
;;;     0b111 -- edi
;;;   Exceptions:
;;;     If mod is 0b00, 0b01 or 0b10:
;;;       then esp is invalid and 0b100 is used to denote the presence
;;;       of an SIB field
;;;     If mod is 0b00:
;;;       then ebp is invalid and 0b101 is used to denote a disp32 field
;;;       that follows the Mod/RM byte and (or the SIB byte if present).
;;;
;;;  /r: The /r denoted the register operand, the numbers are the same
;;;      as above.
;;;
;;;
;;; SIB:  1 byte
;;;           ._________._____________.___________.
;;;     Bits: |  7   6  |  5   4   3  |  2  1  0  |
;;;           | scale   | index       | base      |
;;;           `~~~~~~~~~^~~~~~~~~~~~~~^~~~~~~~~~~~'
;;;  Refer to table 2-3 Page 40 from IA32 Vol2A instruction set reference
;;;
;;;   Scale:
;;;     0b00: multiply index register by 1  (no scale)
;;;     0b01: multiply index register by 2
;;;     0b10: multiply index register by 4
;;;     0b11: multiply index register by 8
;;;
;;;   Index: a register number 
;;;      (esp or 0b100 is invalid as an index)
;;;
;;;   Base: a register number 
;;;      ebp or 0b101 as a base is interpreted as follows:
;;;      If mod == 0b00, then EA = scaled index + disp32    (no base)
;;;      If mod == 0b01, then EA = scaled index + disp8 + ebp
;;;      If mod == 0b10, then EA = scaled index + disp32 + ebp
;;;      If mod == 0b11, then I DON'T KNOW
;;;
;;;
;;;


(define-instr (TMPL1 primary secondary d s)
  (cases (d s)
    [(AL        imm8)        => (logor primary #b00000100)  s] ; 04 ib
    [(EAX       imm32)       => (logor primary #b00000101)  s] ; 05 id
    [(reg/mem8  imm8)        => #b10000000 secondary ib] ; 80 /0 ib
    [(reg/mem32 imm32)       => #b10000001 secondary id] ; 81 /0 id
    [(reg/mem32 imm8)        => #b10000011 secondary ib] ; 83 /0 ib  (sign ext.)
    [(reg/mem8  reg8)        => (logor primary #b00000000) /r ] ; 00 /r
    [(reg/mem32 reg32)       => (logor primary #b00000001) /r ] ; 01 /r
    [(reg8      reg/mem8)    => (logor primary #b00000010) /r ] ; 02 /r
    [(reg32     reg/mem32)   => (logor primary #b00000011) /r ] ; 03 /r
  ))                 

(define-insrt (ADD d s) (TMPL1 #b00000000 /0 d s))

(define-insrt (AND d s) (TMPL1 #b00100000 /4 d s))

(define-instr (CMP d s) (TMPL1 #b00111000 /7 d s))

(define-insrt (CALL d)
  (cases (d)
    [(rel32of)     => #b11101000 id] ; E8 id
    [(reg/mem32)   => #b11111111 /2] ; FF /2
  ))

(define-instr (CLTD) ; convert long to double
  (cases ()
    [() =>  #b10011001] ; 99
  ))

(define-insrt (IDIV s)
  (cases (s)
    [(reg/mem8)    => #b11110110 /7] ; F6 /7
    [(reg/mem32)   => #b11110111 /7] ; F7 /7
  ))

imull
ja
jae
jb
jbe
je
jg
jge
jl
jle
jmp
jne
movb
movl
movswl
movzbl
negl
notl
orl
pop
popl
push
pushl
ret
sall
sarl
sete
setg
setge
setl
setle
shll
shrl
subl
xorl


)


