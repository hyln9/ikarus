
;;; libcompile should provide: compile-core->asm
;;; it takes one expression in core scheme, and produces a list of 
;;; assembly codes (each is a list of instructions).
;;; the resulting lists can then either be fed to the gas backend to
;;; produce assembly files, or to the online assembler to produce
;;; code.
;;;
;;; complications:
;;;  * The gas backend does not support 3D objects.  The online
;;;    assembler does.  We provide a parameter, assembler-backend,
;;;    that when set to 'online, suppresses removing complex constants
;;;    and when set to 'offline, suppresses proucing 3D objects.
;;;

($pcb-set! assembler-backend
  (make-parameter
    'online
    (lambda (x)
      (unless (memq x '(online offline))
        (error 'assembler-backend "invalid backend ~s" x))
      x)))

