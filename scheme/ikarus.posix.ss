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


(library (ikarus posix)
  (export posix-fork fork waitpid system file-exists? delete-file
          getenv env environ)
  (import 
    (rnrs bytevectors)
    (except (ikarus)
       posix-fork fork waitpid system file-exists? delete-file
       getenv env environ))

  (define posix-fork
    (lambda ()
      (foreign-call "ikrt_fork")))

  (define fork
    (lambda (parent-proc child-proc)
      (let ([pid (posix-fork)])
        (cond
          [(fx= pid 0) (child-proc)]
          [(fx= pid -1) 
           (error 'fork "failed")]
          [else (parent-proc pid)]))))

  (define waitpid
    (lambda (pid)
      (unless (fixnum? pid)
        (error 'waitpid "not a fixnum" pid))
      (foreign-call "ikrt_waitpid" pid)))

  (define system
    (lambda (x)
      (unless (string? x)
        (error 'system "not a string" x))
      (let ([rv (foreign-call "ik_system"
                  (string->utf8 x))])
        (if (fx= rv -1)
            (error 'system "failed")
            rv))))

  (define file-exists?
    (lambda (x)
      (unless (string? x)
        (error 'file-exists? "filename is not a string" x))
      (let ([v (foreign-call "ikrt_file_exists" 
                  (string->utf8 x))])
        (cond
          [(boolean? v) v]
          [else
           (error 'file-exists?
                  (case v
                    [(1) "the path contains a non-directory"]
                    [(2) "the path is too long"]
                    [(3) "the path is not accessible"]
                    [(4) "the path contains too many symbolic links"]
                    [(5) "internal access error while accessing"]
                    [(6) "IO error encountered while accessing"]
                    [else "Unknown error"])
                  x)]))))

  (define delete-file
    (lambda (x)
      (unless (string? x)
        (error 'delete-file "filename is not a string" x))
      (let ([v (foreign-call "ikrt_delete_file"
                 (string->utf8 x))])
        (case v
          [(0) (void)]
          [else
           (error 'delete-file
                  (case v
                    [(1) "the path contains a non-directory"]
                    [(2) "the path is too long"]
                    [(3) "the file does not exist"]
                    [(4) "the path is not accessible"]
                    [(5) "the path contains too many symbolic links"]
                    [(6) "you do not have permissions to delete file"]
                    [(7) "device is busy"]
                    [(8) "IO error encountered while deleting"]
                    [(9) "is in a read-only file system"]
                    [(10) "internal access error while deleting"]
                    [else "Unknown error while deleting"])
                  x)]))))

  (define ($getenv-bv key)
    (foreign-call "ikrt_getenv" key))
  (define ($getenv-str key) 
    (utf8->string ($getenv-bv (string->utf8 key))))

  (define (getenv key)
    (if (string? key)
        ($getenv-str key)
        (error 'getenv "the key is not a string" key)))

  (define env
    (let ()
      (define env
        (case-lambda
          [(key) 
           (if (string? key)
               (foreign-call "ikrt_getenv" key)
               (error 'env "the key is not a string" key))]
          [(key val) (env key val #t)]
          [(key val overwrite?)
           (if (string? key)
               (if (string? val)
                   (unless (foreign-call "ikrt_setenv" key val overwrite?)
                     (error 'env "failed" key val))
                   (error 'env "the value is not a string" val))
               (error 'env "the key is not a string" key))]))
      (define busted (lambda args (error 'env "BUG: busted!")))
      busted))


  (define environ (lambda args (error 'environ "busted!")))
  (define environ^
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
                    (if (fx< (fxadd1 i) n)
                        (substring s (fxadd1 i) n)
                        "")))))
        (foreign-call "ikrt_environ"))))
  )
