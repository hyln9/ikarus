
;;; ($pcb-set! posix-fork
;;;   (lambda ()
;;;     (foreign-call "S_fork")))
;;; 
;;; ($pcb-set! fork
;;;   (lambda (parent-proc child-proc)
;;;     (let ([pid (posix-fork)])
;;;       (cond
;;;         [(fx= pid 0) (child-proc)]
;;;         [(fx= pid -1) 
;;;          (error 'fork "failed")]
;;;         [else (parent-proc pid)]))))

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




