
;;;
;;; the interface for creating and managing code objects
;;;

($pcb-set! make-code
  (lambda (code-size reloc-size closure-size)
    (unless (and (fixnum? code-size) (fx> code-size 0))
      (error 'make-code "invalid code size ~s" code-size))
    (unless (and (fixnum? reloc-size) (fx>= reloc-size 0))
      (error 'make-code "invalid relocation table size ~s" reloc-size))
    (unless (and (fixnum? closure-size) (fx>= closure-size 0))
      (error 'make-code "invalid closure size ~s" closure-size))
    (foreign-call "S_make_code" code-size reloc-size closure-size)))

($pcb-set! make-code-executable!
   (lambda (x)
     (unless (code? x) (error 'make-code-executable! "~s is not a code" x))
     (unless (foreign-call "S_make_code_executable" x)
       (error 'make-code-executable "Failed!"))))

($pcb-set! code-instr-size
   (lambda (x)
     (unless (code? x)
       (error 'code-instr-size "~s is not a code" x))
     ($code-instr-size x)))

($pcb-set! code-reloc-size
   (lambda (x)
     (unless (code? x)
       (error 'code-reloc-size "~s is not a code" x))
     ($code-reloc-size x)))

($pcb-set! code-closure-size
   (lambda (x)
     (unless (code? x)
       (error 'code-closure-size "~s is not a code" x))
     ($code-closure-size x)))

($pcb-set! code?
   (lambda (x)
     (code? x)))

($pcb-set! code->closure
   (lambda (x)
     (unless (code? x) (error 'code->closure "~s is not a code"))
     (unless ($fx= ($code-closure-size x) 1) 
       (error 'code->closure "code contains free variables"))
     ($code->closure x)))


($pcb-set! set-code-byte!
   (lambda (x i b)
     (unless (code? x) (error 'set-code-byte! "~s is not a code" x))
     (unless (and (fixnum? i) ($fx>= i 0))
       (error 'set-code-byte! "~s is not a valid index" i))
     (unless (and (fixnum? b) ($fx>= b 0) ($fx<= b 255))
       (error 'set-code-byte! "~s is not a valid byte" b))
     (unless ($fx< i ($code-instr-size x))
       (error 'set-code-byte! "~s is out of range for a code of size ~s"
              i
              ($code-instr-size x)))
     ($set-code-byte! x i b)))

($pcb-set! set-code-word!
   (lambda (x i w)
     (unless (code? x) (error 'set-code-word! "~s is not a code" x))
     (unless (and (fixnum? i) ($fx>= i 0))
       (error 'set-code-word! "~s is not a valid index" i))
     (unless (and ($fx< i ($code-instr-size x))
                  ($fx< ($fx+ i 3) ($code-instr-size x)))
       (error 'set-code-word! "~s is out of range for a code of size ~s"
              i
              ($code-instr-size x)))
     ($set-code-word! x i w)))


($pcb-set! set-code-object!
   (lambda (code object code-offset reloc-index)
     (unless (code? code)
       (error 'set-code-object! "~s is not a code" code))
     (unless (and (fixnum? code-offset)
                  ($fx> code-offset 0)
                  ($fx< code-offset ($code-instr-size code))
                  ($fx< ($fx+ code-offset 3) ($code-instr-size code)))
       (error 'set-code-object! "~s is not a valid code offset" code-offset))
     (unless (and (fixnum? reloc-index)
                  ($fx>= reloc-index 0)
                  ($fx< reloc-index ($code-reloc-size code)))
       (error 'set-code-object! "~s is not a valid reloc index" reloc-index))
     ($set-code-object! code object code-offset reloc-index)))

($pcb-set! set-code-object+offset!
   (lambda (code object code-offset object-offset reloc-index)
     (unless (code? code)
       (error 'set-code-object+offset! "~s is not a code" code))
     (unless (and (fixnum? code-offset)
                  ($fx> code-offset 0)
                  ($fx< code-offset ($code-instr-size code))
                  ($fx< ($fx+ code-offset 3) ($code-instr-size code)))
       (error 'set-code-object+offset!
              "~s is not a valid code offset" code-offset))
     (unless (and (fixnum? reloc-index)
                  ($fx>= reloc-index 0)
                  ($fx< reloc-index ($fx- ($code-reloc-size code) 1)))
       (error 'set-code-object+offset!
              "~s is not a valid reloc index" reloc-index))
     ($set-code-object+offset! code object 
        code-offset object-offset reloc-index)))


($pcb-set! set-code-object+offset/rel!
   (lambda (code object code-offset object-offset reloc-index)
     (unless (code? code)
       (error 'set-code-object+offset/rel! "~s is not a code" code))
     (unless (and (fixnum? code-offset)
                  ($fx> code-offset 0)
                  ($fx< code-offset ($code-instr-size code))
                  ($fx< ($fx+ code-offset 3) ($code-instr-size code)))
       (error 'set-code-object+offset/rel!
              "~s is not a valid code offset" code-offset))
     (unless (and (fixnum? reloc-index)
                  ($fx>= reloc-index 0)
                  ($fx< reloc-index ($fx- ($code-reloc-size code) 1)))
       (error 'set-code-object+offset/rel!
              "~s is not a valid reloc index" reloc-index))
     ($set-code-object+offset/rel! code object 
        code-offset object-offset reloc-index)))


($pcb-set! set-code-object/reloc/relative!
   (lambda args (error 'set-code-object/reloc/relative! "not yet")))
