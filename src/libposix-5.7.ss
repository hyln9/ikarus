
($pcb-set! posix-fork
  (lambda ()
    (foreign-call "S_fork")))

($pcb-set! fork
  (lambda (parent-proc child-proc)
    (let ([pid (posix-fork)])
      (cond
        [(fx= pid 0) (child-proc)]
        [(fx= pid -1) 
         (error 'fork "failed")]
        [else (parent-proc pid)]))))

($pcb-set! system
  (lambda (x)
    (unless (string? x)
      (error 'system "~s is not a string" x))
    (let ([rv (foreign-call "S_system" x)])
      (if (fx= rv -1)
          (error 'system "failed")
          rv))))

