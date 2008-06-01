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


(library (ikarus.posix)
  (export posix-fork fork waitpid system file-exists? delete-file
          nanosleep getenv env environ file-ctime current-directory
          file-regular? file-directory? file-symbolic-link? make-symbolic-link
          directory-list make-directory delete-directory change-mode
          strerror)
  (import 
    (rnrs bytevectors)
    (except (ikarus)
       nanosleep
       posix-fork fork waitpid system file-exists? delete-file
       getenv env environ file-ctime current-directory
       file-regular? file-directory? file-symbolic-link? make-symbolic-link
       directory-list make-directory delete-directory change-mode
       strerror))

  (define posix-fork
    (lambda ()
      (foreign-call "ikrt_fork")))

  (define fork
    (lambda (parent-proc child-proc)
      (let ([pid (posix-fork)])
        (cond
          [(fx= pid 0) (child-proc)]
          [(fx< pid 0) (raise/strerror 'fork pid)]
          [else (parent-proc pid)]))))

  (define waitpid
    (lambda (pid)
      (unless (fixnum? pid)
        (die 'waitpid "not a fixnum" pid))
      (let ([r (foreign-call "ikrt_waitpid" pid)])
        (if (fx< r 0)
            (raise/strerror 'waitpid r)
            r))))

  (define system
    (lambda (x)
      (unless (string? x)
        (die 'system "not a string" x))
      (let ([rv (foreign-call "ik_system"
                  (string->utf8 x))])
        (if (fx< rv 0)
            (raise/strerror 'system rv)
            rv))))
  
  (define stat
    (lambda (path follow who)
      (unless (string? path)
        (die who "not a string" path))
      (let ([r (foreign-call "ikrt_stat" (string->utf8 path) follow)])
        (case r
          [(0) 'unknown]
          [(1) 'regular]
          [(2) 'directory]
          [(3) 'symlink]
          [(-45) #f]  ;; from ikarus-errno.c: ENOENT -- path does not exist
          [else (raise/strerror who r path)]))))

  (define file-exists?
    (case-lambda 
      [(path) (file-exists? path #t)]
      [(path follow)
       (and (stat path follow 'file-exists?) #t)]))
  
  (define file-regular?
    (case-lambda
      [(path) (file-regular? path #t)]
      [(path follow)
       (eq? 'regular (stat path follow 'file-regular?))]))
  
  (define file-directory?
    (case-lambda
      [(path) (file-directory? path #t)]
      [(path follow)
       (eq? 'directory (stat path follow 'file-directory?))]))
  
  (define file-symbolic-link?
    (lambda (path)
      (eq? 'symlink (stat path #f 'file-symbolic-link?))))

  (define delete-file
    (lambda (x)
      (define who 'delete-file)
      (unless (string? x)
        (die who "filename is not a string" x))
      (let ([v (foreign-call "ikrt_delete_file"
                 (string->utf8 x))])
        (unless (eq? v #t)
          (raise/strerror who v x)))))
  
  (define directory-list
    (lambda (path)
      (define who 'directory-list)
      (unless (string? path)
        (die who "not a string" path))
      (let ([r (foreign-call "ikrt_directory_list" (string->utf8 path))])
        (if (fixnum? r)
            (raise/strerror who r path)
            (map utf8->string (reverse r))))))
  
  (define make-directory
    (case-lambda 
      [(path) (make-directory path #o755)]
      [(path mode)
       (define who 'make-directory)
       (unless (string? path)
         (die who "not a string" path))
       (unless (fixnum? mode)
         (die who "not a fixnum" mode))
       (let ([r (foreign-call "ikrt_mkdir" (string->utf8 path) mode)])
         (unless (eq? r #t)
           (raise/strerror who r path)))]))
  
  (define delete-directory
    (case-lambda
      [(path) (delete-directory path #f)]
      [(path want-error?)
       (define who 'delete-directory)
       (unless (string? path)
         (die who "not a string" path))
       (let ([r (foreign-call "ikrt_rmdir" (string->utf8 path))])
         (if want-error?
             (unless (eq? r #t) (raise/strerror who r path))
             (eq? r #t)))]))
  
  (define change-mode
    (lambda (path mode)
      (define who 'change-mode)
      (unless (string? path)
        (die who "not a string" path))
      (unless (fixnum? mode)
        (die who "not a fixnum" mode))
      (let ([r (foreign-call "ikrt_chmod" (string->utf8 path) mode)])
        (unless (eq? r #t)
          (raise/strerror who r path)))))
  
  (define make-symbolic-link
    (lambda (to path)
      (define who 'make-symbolic-link)
      (unless (and (string? to) (string? path))
        (die who "not a string" (if (string? to) path to)))
      (let ([r (foreign-call "ikrt_symlink" 
                 (string->utf8 to) (string->utf8 path))])
        (unless (eq? r #t)
          (raise/strerror who r path)))))

  (define (file-ctime x)
    (define who 'file-ctime)
    (unless (string? x) 
      (die who "not a string" x))
    (let ([p (cons #f #f)])
      (let ([v (foreign-call "ikrt_file_ctime" (string->utf8 x) p)])
        (case v
          [(0) (+ (* (car p) #e1e9) (cdr p))]
          [else (raise/strerror who v x)]))))


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


  (define current-directory
    (case-lambda
      [() 
       (let ([v (foreign-call "ikrt_getcwd")])
         (if (bytevector? v)
             (utf8->string v)
             (raise/strerror 'current-directory v)))]
      [(x) 
       (if (string? x) 
           (let ([rv (foreign-call "ikrt_chdir" (string->utf8 x))])
             (unless (eq? rv #t)
               (raise/strerror 'current-directory rv x)))
           (die 'current-directory "not a string" x))]))
  
  (define raise/strerror 
    (case-lambda
      [(who errno-code) 
       (raise/strerror who errno-code #f)]
      [(who errno-code filename)
       (raise
         (condition
           (make-who-condition who)
           (make-message-condition (strerror errno-code))
           (if filename 
               (make-i/o-filename-error filename)
               (condition))))]))
  
  (define strerror
    (lambda (errno-code)
      (define who 'strerror)
      (unless (fixnum? errno-code)
        (die who "not a fixnum" errno-code))
      (let ([emsg (foreign-call "ikrt_strerror" errno-code)])
        (if emsg
            (let ([errno-name 
                   (foreign-call "ikrt_errno_code_to_name" errno-code)])
              (assert errno-name)
              (format "~a: ~a" 
                (utf8->string errno-name)
                (utf8->string emsg)))
            (format "Ikarus's ~a: don't know Ikarus errno code ~s" 
              who errno-code)))))

  )
