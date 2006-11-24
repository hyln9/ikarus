

;;; 6.2: * side-effects now modify the dirty-vector
;;;      * added bwp-object?
;;;      * added pointer-value
;;;      * added tcbuckets
;;; 6.1: * added case-lambda, dropped lambda
;;; 6.0: * basic compiler

(when (eq? "" "")
  (load "chez-compat.ss")
  (set! primitive-ref top-level-value)
  (set! primitive-set! set-top-level-value!)
  (set! chez-expand sc-expand)
  (set! chez-current-expand current-expand)
  (printf "loading psyntax.pp ...\n")
  (load "psyntax-7.1.pp")
  (chez-current-expand
    (lambda (x . args)
      (apply chez-expand (sc-expand x) args)))
  (printf "loading psyntax.ss ...\n")
  (load "psyntax-7.1-6.5.ss")
  (chez-current-expand
    (lambda (x . args)
      (apply chez-expand (sc-expand x) args)))
  (printf "ok\n")
  (load "libassembler-compat-6.0.ss") ; defines make-code etc.
  (load "libintelasm-6.0.ss") ; uses make-code, etc.
  (load "libfasl-6.0.ss") ; uses code? etc.
  (load "libcompile-6.5.ss") ; uses fasl-write
)


(define scheme-library-files
  '(["libhandlers-6.0.ss"    "libhandlers.fasl"]
    ["libcontrol-6.1.ss"     "libcontrol.fasl"]
    ["libcollect-6.1.ss"     "libcollect.fasl"]
    ["librecord-6.4.ss"      "librecord.fasl"]
    ["libcxr-6.0.ss"         "libcxr.fasl"]
    ["libcore-6.2.ss"        "libcore.fasl"]
    ["libio-6.1.ss"          "libio.fasl"]
    ["libwriter-6.2.ss"      "libwriter.fasl"]
    ["libtokenizer-6.1.ss"   "libtokenizer.fasl"]
    ["libassembler-compat-6.0.ss" "libassembler-compat.ss"]
    ["libintelasm-6.4.ss"    "libintelasm.fasl"]
    ["libfasl-6.0.ss"        "libfasl.fasl"]
    ["libcompile-6.5.ss"     "libcompile.fasl"]
    ["psyntax-7.1-6.5.ss"        "psyntax.fasl"]
    ["libinterpret-6.5.ss"   "libinterpret.fasl"]
    ["libcafe-6.1.ss"        "libcafe.fasl"]
;    ["libtrace-5.3.ss"       "libtrace-5.3.s"       "libtrace"      ]
    ["libposix-6.0.ss"       "libposix.fasl"]
    ["libhash-6.2.ss"        "libhash.fasl"]
    ["libtoplevel-6.0.ss"    "libtoplevel.fasl"]
    ))



(define (compile-library ifile ofile)
  (parameterize ([assembler-output #f] [expand-mode 'bootstrap])
      (printf "compiling ~a ...\n" ifile)
      (compile-file ifile ofile 'replace)))

(for-each 
  (lambda (x)
    (compile-library (car x) (cadr x)))
  scheme-library-files)

(system "rm -f ikarus.fasl")

(for-each
  (lambda (x)
    (system (format "cat ~a >> ikarus.fasl" (cadr x))))
  scheme-library-files)


(define (get-date)
  (system "date +\"%F\" > build-date.tmp")
  (let ([ip (open-input-file "build-date.tmp")])
    (list->string
      (let f ()
        (let ([x (read-char ip)])
          (if (char=? x #\newline)
              '()
              (cons x (f))))))))
  
(with-output-to-file "petite-ikarus.ss"
  (lambda ()
    (write 
      `(begin
         (display ,(format "Petite Ikarus Scheme (Build ~a)\n" (get-date)))
         (display "Copyright (c) 2006 Abdulaziz Ghuloum\n\n")
         (new-cafe))))
  'replace)

(compile-library "petite-ikarus.ss" "petite-ikarus.fasl")
