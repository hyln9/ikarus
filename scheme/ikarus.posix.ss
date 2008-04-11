;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
          nanosleep getenv env environ file-ctime)
  (import 
    (rnrs bytevectors)
    (except (ikarus)
       nanosleep
       posix-fork fork waitpid system file-exists? delete-file
       getenv env environ file-ctime))

  (define posix-fork
    (lambda ()
      (foreign-call "ikrt_fork")))

  (define fork
    (lambda (parent-proc child-proc)
      (let ([pid (posix-fork)])
        (cond
          [(fx= pid 0) (child-proc)]
          [(fx= pid -1) 
           (die 'fork "failed")]
          [else (parent-proc pid)]))))

  (define waitpid
    (lambda (pid)
      (unless (fixnum? pid)
        (die 'waitpid "not a fixnum" pid))
      (foreign-call "ikrt_waitpid" pid)))

  (define system
    (lambda (x)
      (unless (string? x)
        (die 'system "not a string" x))
      (let ([rv (foreign-call "ik_system"
                  (string->utf8 x))])
        (if (fx= rv -1)
            (die 'system "failed")
            rv))))

  (define file-exists?
    (lambda (x)
      (unless (string? x)
        (die 'file-exists? "filename is not a string" x))
      (let ([v (foreign-call "ikrt_file_exists" 
                  (string->utf8 x))])
        (cond
          [(boolean? v) v]
          [else
           (raise
             (condition
               (make-who-condition 'file-exists?)
               (make-message-condition
                 (case v
                   [(1) "file path contains a non-directory"]
                   [(2) "file path is too long"]
                   [(3) "file path is not accessible"]
                   [(4) "file path contains too many symbolic links"]
                   [(5) "internal access error while accessing file"]
                   [(6) "IO error encountered while accessing file"]
                   [else "Unknown error while testing file"]))
               (make-i/o-filename-error x)))]))))

  (define delete-file
    (lambda (x)
      (unless (string? x)
        (die 'delete-file "filename is not a string" x))
      (let ([v (foreign-call "ikrt_delete_file"
                 (string->utf8 x))])
        (case v
          [(0) (void)]
          [else
           (raise
             (condition
               (make-who-condition 'delete-file)
               (make-message-condition
                 (case v
                   [(1) "file path contains a non-directory"]
                   [(2) "file path is too long"]
                   [(3) "file does not exist"]
                   [(4) "file path is not accessible"]
                   [(5) "file path contains too many symbolic links"]
                   [(6) "you do not have permissions to delete file"]
                   [(7) "device is busy"]
                   [(8) "IO error encountered while deleting"]
                   [(9) "file is in a read-only file system"]
                   [(10) "internal access error while deleting"]
                   [else "Unknown error while deleting file"]))
               (make-i/o-filename-error x)))]))))

  (define (file-ctime x)
    (define who 'file-ctime)
    (unless (string? x) 
      (die who "not a string" x))
    (let ([p (cons #f #f)])
      (let ([v (foreign-call "ikrt_file_ctime" (string->utf8 x) p)])
        (case v
          [(0) (+ (* (car p) #e1e9) (cdr p))]
          [else 
           (raise
             (condition 
               (make-who-condition who)
               (make-message-condition "cannot stat a file")
               (make-i/o-filename-error x)))]))))


  (define ($getenv-bv key)
    (foreign-call "ikrt_getenv" key))
  (define ($getenv-str key) 
    (utf8->string ($getenv-bv (string->utf8 key))))

  (define (getenv key)
    (if (string? key)
        ($getenv-str key)
        (die 'getenv "the key is not a string" key)))

  (define env
    (let ()
      (define env
        (case-lambda
          [(key) 
           (if (string? key)
               (foreign-call "ikrt_getenv" key)
               (die 'env "the key is not a string" key))]
          [(key val) (env key val #t)]
          [(key val overwrite?)
           (if (string? key)
               (if (string? val)
                   (unless (foreign-call "ikrt_setenv" key val overwrite?)
                     (die 'env "failed" key val))
                   (die 'env "the value is not a string" val))
               (die 'env "the key is not a string" key))]))
      (define busted (lambda args (die 'env "BUG: busted!")))
      busted))


  (define environ (lambda args (die 'environ "busted!")))
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

  (define (nanosleep secs nsecs)
    (import (ikarus system $fx))
    (unless (cond
              [(fixnum? secs) ($fx>= secs 0)]
              [(bignum? secs) (<= 0 secs (- (expt 2 32) 1))]
              [else (die 'nanosleep "not an exact integer" secs)])
      (die 'nanosleep "seconds must be a nonnegative integer <=" secs))
    (unless (cond
              [(fixnum? nsecs) ($fx>= nsecs 0)]
              [(bignum? nsecs) (<= 0 nsecs 999999999)]
              [else (die 'nanosleep "not an exact integer" nsecs)])
      (die 'nanosleep "nanoseconds must be an integer \
                       in the range 0..999999999" nsecs))
    (let ([rv (foreign-call "ikrt_nanosleep" secs nsecs)])
      (unless (eq? rv 0)
        (error 'nanosleep "failed"))))

  )
