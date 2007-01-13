
(primitive-set! 'posix-fork
  (lambda ()
    (foreign-call "ikrt_fork")))

(primitive-set! 'fork
  (lambda (parent-proc child-proc)
    (let ([pid (posix-fork)])
      (cond
        [(fx= pid 0) (child-proc)]
        [(fx= pid -1) 
         (error 'fork "failed")]
        [else (parent-proc pid)]))))

(primitive-set! 'waitpid
  (lambda (pid)
    (unless (fixnum? pid)
      (error 'waitpid "~s is not a fixnum" pid))
    (foreign-call "ikrt_waitpid" pid)))

(primitive-set! 'system
  (lambda (x)
    (unless (string? x)
      (error 'system "~s is not a string" x))
    (let ([rv (foreign-call "ik_system" x)])
      (if (fx= rv -1)
          (error 'system "failed")
          rv))))

(primitive-set! 'file-exists?
  (lambda (x)
    (unless (string? x)
      (error 'file-exists? "filename ~s is not a string" x))
    (let ([v (foreign-call "ikrt_file_exists" x)])
      (cond
        [(boolean? v) v]
        [else
         (error 'file-exists?
                (case v
                  [(1) "the path ~s contains a non-directory"]
                  [(2) "the path ~s is too long"]
                  [(3) "the path ~s is not accessible"]
                  [(4) "the path ~s contains too many symbolic links"]
                  [(5) "internal access error while accessing ~s"]
                  [(6) "IO error encountered while accessing ~s"]
                  [else "Unknown error in ~s"])
                x)]))))

(primitive-set! 'delete-file
  (lambda (x)
    (unless (string? x)
      (error 'delete-file "filename ~s is not a string" x))
    (let ([v (foreign-call "ikrt_delete_file" x)])
      (case v
        [(0) (void)]
        [else
         (error 'delete-file
                (case v
                  [(1) "the path ~s contains a non-directory"]
                  [(2) "the path ~s is too long"]
                  [(3) "the file ~s does not exist"]
                  [(4) "the path ~s is not accessible"]
                  [(5) "the path ~s contains too many symbolic links"]
                  [(6) "you do not have permissions to delete ~s"]
                  [(7) "device ~s is busy"]
                  [(8) "IO error encountered while deleting ~s"]
                  [(9) "~s is in a read-only file system"]
                  [(10) "internal access error while deleting ~s"]
                  [else "Unknown error while deleting ~s"])
                x)]))))

(primitive-set! 'env
  (let ()
    (define env
      (case-lambda
        [(key) 
         (if (string? key)
             (foreign-call "ikrt_getenv" key)
             (error 'env "the key: ~s is not a string" key))]
        [(key val) (env key val #t)]
        [(key val overwrite?)
         (if (string? key)
             (if (string? val)
                 (unless (foreign-call "ikrt_setenv" key val overwrite?)
                   (error 'env "failed to set ~s to ~s" key val))
                 (error 'env "the value: ~s is not a string" val))
             (error 'env "the key: ~s is not a string" key))]))
    env))

(primitive-set! 'environ
  (lambda ()
    (map 
      (lambda (s)
        (define (loc= s i n)
          (cond
            [(fx= i n) i]
            [(char=? (string-ref s i) #\=) i]
            [else (loc= s (fx+ i 1) n)]))
        (let ([n (string-length s)])
          (let ([i (loc= s 0 n)])
            (cons (substring s 0 i)
                  (substring s (fxadd1 i) n)))))
      (foreign-call "ikrt_environ"))))
