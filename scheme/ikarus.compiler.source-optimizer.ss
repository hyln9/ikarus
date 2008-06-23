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


;;; Work in progress

;;; Oscar Waddell. "Extending the Scope of Syntactic Abstraction". PhD.
;;; Thesis. Indiana University Computer Science Department. August 1999.
;;; Available online: 
;;;   http://www.cs.indiana.edu/~owaddell/papers/thesis.ps.gz

(module (source-optimize 
         cp0-effort-limit cp0-size-limit)
  (define-syntax define*
    (lambda (x)
      (import (ikarus))
      (syntax-case x ()
        [(_ (name args ...) b b* ...) 
         (with-syntax ([caller (datum->syntax #'name 'caller)]
                       [who (datum->syntax #'name 'who)]
                       [(a* ...) (generate-temporaries #'(args ...))])
            #'(begin
                (define (name-aux args ... caller)
                  (define who 'name)
                  b b* ...)
                (define-syntax name 
                  (lambda (x)
                    (syntax-case x ()
                      [(ctxt a* ...) 
                       (not 
                         (bound-identifier=?
                            (datum->syntax #'here 'who)
                            (datum->syntax #'ctxt 'who)))
                       (with-syntax ([caller (datum->syntax #'ctxt 'who)])
                         #'(name-aux a* ... caller))])))))])))
  (define who 'source-optimize)
  ;;; this define-structure definition for compatibility with the
  ;;; notation used in Oscar's thesis.
  (define-syntax define-structure
    (lambda (stx) 
      (define (fmt ctxt)
        (lambda (str . args) 
          (datum->syntax ctxt 
            (string->symbol 
              (apply format str (map syntax->datum args))))))
      (syntax-case stx ()
        [(_ (name fields ...)) 
         #'(define-struct name (fields ...))]
        [(_ (name fields ...) ([others defaults] ...))
         (with-syntax ([(pred maker (getters ...) (setters ...))
                        (let ([fmt (fmt #'name)])
                          (list (fmt "~s?" #'name)
                                (fmt "make-~s" #'name)
                                (map (lambda (x) (fmt "~s-~s" #'name x))
                                     #'(fields ... others ...))
                                (map (lambda (x) (fmt "set-~s-~s!" #'name x))
                                     #'(fields ... others ...))))])
           #'(module (name pred getters ... setters ... maker)
               (module P (name pred getters ... setters ... maker)
                 (define-struct name (fields ... others ...)))
               (module (maker)
                 (define (maker fields ...)
                   (import P)
                   (maker fields ... defaults ...)))
               (module (name pred getters ... setters ...)
                 (import P))))])))
  ;;;
  (define-structure (prelex operand)
    ([source-referenced?   #f]
     [source-assigned?     #f]
     [residual-referenced? #f]
     [residual-assigned?   #f]))
  ;;;
  (define-structure (app rand* ctxt)
    ([inlined #f]))
  ;;;
  (define-structure (operand expr env ec)
    ([value                  #f]
     [residualize-for-effect #f]
     [size                    0]
     [inner-pending          #f]
     [outer-pending          #f]))
  ;;;
  (define-structure (counter value ctxt k))
  ;;;
  (define (passive-counter)
    (make-counter (greatest-fixnum) #f
      (lambda args 
        (error 'passive-counter "invalid abort"))))
  ;;;
  (define (passive-counter-value x)
    (- (greatest-fixnum) (counter-value x)))
  ;;;
  (define (active-counter? x)
    (and (counter? x) (counter-ctxt x)))
  ;;;
  (define (decrement x amt)
    (let ([n (- (counter-value x) amt)])
      (set-counter-value! x n)
      (when (< n 0)
        (reset-integrated! (counter-ctxt x))
        ((counter-k x) #f))))
  ;;;
  (define (reset-integrated! ctxt)
    (set-app-inlined! ctxt #f)
    (let ([ctxt (app-ctxt ctxt)])
      (when (app? ctxt)
        (reset-integrated! ctxt))))
  ;;;
  ;;;
  (module (init-var! var-prelex)
    (define (init-var! x)
      (set-var-index! x (make-prelex #f)))
    (define (var-prelex x)
      (let ([v (var-index x)])
        (if (prelex? v)
            v
            (error 'var-prelex "not initialized" x)))))
  (module (with-extended-env)
    (define (copy-var x)
      (let ([xi (var-prelex x)])
        (let ([y (unique-var (var-name x))]
              [yi (make-prelex #f)])
          (set-var-index! y yi)
          (set-prelex-source-referenced?! yi
            (prelex-source-referenced? xi))
          (set-prelex-source-assigned?! yi
            (prelex-source-assigned? xi))
          (let ([loc (var-global-loc x)])
            (when loc
              (set-var-global-loc! y loc)
              (set-prelex-source-referenced?! yi #t)
              (set-prelex-residual-referenced?! yi #t)))
          y)))
    (define (extend env lhs* rands) 
      (let ([nlhs* (map copy-var lhs*)])
        (when rands 
          (for-each 
            (lambda (lhs rhs) 
              (assert (operand? rhs))
              (set-prelex-operand! (var-prelex lhs) rhs))
            nlhs* rands))
        (values (vector lhs* nlhs* env) nlhs*)))
    (define-syntax with-extended-env
      (syntax-rules ()
        [(_ ((e2 args2) (e1 args1 rands)) b b* ...)
         (let-values ([(e2 args2) (extend e1 args1 rands)])
           b b* ...)])))
  ;;; purpose of prepare:
  ;;; 1. attach an info struct to every bound variable
  ;;; 2. set the plref and plset flags to indicate whether
  ;;;    there is a reference/assignment to the variable.
  ;;; 3. verify well-formness of the input.
  (define (prepare x)
    (define who 'prepare)
    (define (L x)
      (for-each 
        (lambda (x)
          (struct-case x
            [(clambda-case info body)
             (for-each init-var! (case-info-args info))
             (E body)]))
        (clambda-cases x)))
    (define (E x) 
      (struct-case x 
        [(constant) (void)]
        [(var) (set-prelex-source-referenced?! (var-prelex x) #t)]
        [(primref)  (void)]
        [(clambda)  (L x)]
        [(seq e0 e1) (E e0) (E e1)]
        [(conditional e0 e1 e2) 
         (E e0) (E e1) (E e2)]
        [(assign x val) 
         (set-prelex-source-assigned?! (var-prelex x) #t)
         (E val)]
        [(bind lhs* rhs* body)
         (for-each E rhs*)
         (for-each init-var! lhs*)
         (E body)]
        [(fix lhs* rhs* body)
         (for-each init-var! lhs*)
         (for-each L rhs*)
         (E body)
         (for-each ;;; sanity check
           (lambda (x)
             (assert (not (prelex-source-assigned? (var-prelex x))))) 
           lhs*)]
        [(funcall rator rand*) 
         (for-each E rand*)
         (E rator)]
        [(forcall name rand*)
         (for-each E rand*)]
        [else (error who "invalid expr in prepare" x)]))
    (E x))

  #;
  (define (uncover-global-locations x)
    (define who 'uncover-global-locations)
    (define (make-global-assign loc val)
      (make-funcall 
        (make-primref '$init-symbol-value!)
        (list (make-constant loc) val)))
    (define (bind-globals ls e)
      (cond
        [(null? ls) e]
        [else 
         (let ([x (car ls)] [e (bind-globals (cdr ls) e)])
           (cond
             [(var-global-loc x) =>
              (lambda (loc)
                (set-var-global-loc! x #f)
                (set-prelex-source-referenced?! (var-prelex x) #t)
                (make-seq (make-global-assign loc x) e))]
             [else e]))]))
    (define (L x)
      (struct-case x 
        [(clambda g cases cp free name) 
         (make-clambda g 
           (map (lambda (x)
                  (struct-case x 
                    [(clambda-case info body)
                     (make-clambda-case info 
                       (bind-globals (case-info-args info) 
                         (E body)))]))
                cases)
           cp free name)]))
    (define (E x)
      (struct-case x 
        [(constant) x]
        [(var)      x]
        [(primref)  x]
        [(clambda)  (L x)]
        [(seq e0 e1) (make-seq (E e0) (E e1))]
        [(conditional e0 e1 e2) 
         (make-conditional (E e0) (E e1) (E e2))]
        [(assign x val) 
         (cond
           [(var-global-loc x) =>
            (lambda (loc)
              (make-seq
                (make-assign x (E val))
                (make-global-assign loc x)))]
           [else (make-assign x (E val))])]
        [(bind lhs* rhs* body)
         (make-bind lhs* (map E rhs*) (bind-globals lhs* (E body)))]
        [(fix lhs* rhs* body)
         (make-fix lhs* (map E rhs*) (bind-globals lhs* (E body)))]
        [(funcall rator rand*)
         (make-funcall (E rator) (map E rand*))]
        [(forcall name rand*)
         (make-forcall name (map E rand*))]
        [else (error who "invalid expr" x)]))
    (E x))


  (define cp0-effort-limit (make-parameter 100))
  (define cp0-size-limit (make-parameter 10))
  ;(define cp0-size-limit (make-parameter 0))

  ;;; TODO
  (define primitive-info
    '(
      [make-list ]
      [last-pair ]
      [bwp-object? ]
      [weak-cons ]
      [weak-pair? ]
      [uuid ]
      [date-string ]
      [andmap ]
      [ormap ]
      [fx< ]
      [fx<= ]
      [fx> ]
      [fx>= ]
      [fx= ]
      [fxadd1 ]
      [fxsub1 ]
      [fxquotient ]
      [fxremainder ]
      [fxmodulo ]
      [fxsll ]
      [fxsra ]
      [sra ]
      [sll ]
      [fxlogand ]
      [fxlogxor ]
      [fxlogor ]
      [fxlognot ]
      [fixnum->string ]
      [string->flonum ]
      [add1 ]
      [sub1 ]
      [bignum? ]
      [ratnum? ]
      [compnum? ]
      [cflonum? ]
      [flonum-parts ]
      [flonum-bytes ]
      [flonum->string ]
      [random ]
      [gensym? ]
      [gensym->unique-string ]
      [unicode-printable-char? ]
      [struct? ]
      [$car ]
      [$cdr ]
      [$memq ]
      [$memv ]
      [$char? ]
      [$char= ]
      [$char< ]
      [$char> ]
      [$char<= ]
      [$char>= ]
      [$char->fixnum ]
      [$fixnum->char ]
      [$make-string ]
      [$string-ref ]
      [$string-set! ]
      [$string-length ]
      [$make-bytevector ]
      [$bytevector-length ]
      [$bytevector-s8-ref ]
      [$bytevector-u8-ref ]
      [$bytevector-set! ]
      [$bytevector-ieee-double-native-ref ]
      [$bytevector-ieee-double-native-set! ]
      [$bytevector-ieee-double-nonnative-ref ]
      [$bytevector-ieee-double-nonnative-set! ]
      [$bytevector-ieee-single-native-ref ]
      [$bytevector-ieee-single-native-set! ]
      [$bytevector-ieee-single-nonnative-ref ]
      [$bytevector-ieee-single-nonnative-set! ]
      [$flonum-u8-ref ]
      [$make-flonum ]
      [$flonum-set! ]
      [$flonum-signed-biased-exponent ]
      [$flonum-rational? ]
      [$flonum-integer? ]
      [$fl+ ]
      [$fl- ]
      [$fl* ]
      [$fl/ ]
      [$fl= ]
      [$fl< ]
      [$fl<= ]
      [$fl> ]
      [$fl>= ]
      [$fixnum->flonum ]
      [$flonum-sbe ]
      [$make-bignum ]
      [$bignum-positive? ]
      [$bignum-size ]
      [$bignum-byte-ref ]
      [$bignum-byte-set! ]
      [$make-ratnum ]
      [$ratnum-n ]
      [$ratnum-d ]
      [$make-compnum ]
      [$compnum-real ]
      [$compnum-imag ]
      [$make-cflonum ]
      [$cflonum-real ]
      [$cflonum-imag ]
      [$make-vector ]
      [$vector-length ]
      [$vector-ref ]
      [$vector-set! ]
      [$fxzero? ]
      [$fxadd1 ]
      [$fxsub1 ]
      [$fx>= ]
      [$fx<= ]
      [$fx> ]
      [$fx< ]
      [$fx= ]
      [$fxsll ]
      [$fxsra ]
      [$fxquotient ]
      [$fxmodulo ]
      [$fxlogxor ]
      [$fxlogor ]
      [$fxlognot ]
      [$fxlogand ]
      [$fx+ ]
      [$fx* ]
      [$fx- ]
      [$fxinthash ]
      [$make-symbol ]
      [$symbol-unique-string ]
      [$symbol-value ]
      [$symbol-string ]
      [$symbol-plist ]
      [$set-symbol-value! ]
      [$set-symbol-proc! ]
      [$set-symbol-string! ]
      [$set-symbol-unique-string! ]
      [$set-symbol-plist! ]
      [$init-symbol-value! ]
      [$unbound-object? ]
      [base-rtd ]
      [$struct-set! ]
      [$struct-ref ]
      [$struct-rtd ]
      [$struct ]
      [$make-struct ]
      [$struct? ]
      [$struct/rtd? ]
      [$closure-code ]
      [$code->closure ]
      [$code-reloc-vector ]
      [$code-freevars ]
      [$code-size ]
      [$code-annotation ]
      [$code-ref ]
      [$code-set! ]
      [$set-code-annotation! ]
      [procedure-annotation ]
      [$make-tcbucket ]
      [$tcbucket-key ]
      [$tcbucket-val ]
      [$tcbucket-next ]
      [$set-tcbucket-val! ]
      [$set-tcbucket-next! ]
      [$set-tcbucket-tconc! ]
      [$arg-list ]
      [$collect-key ]
      [$$apply ]
      [$fp-at-base ]
      [$primitive-call/cc ]
      [$frame->continuation ]
      [$current-frame ]
      [$seal-frame-and-call ]
      [$make-call-with-values-procedure ]
      [$make-values-procedure ]
      [$interrupted? ]
      [$unset-interrupted! ]
      [$swap-engine-counter! ]
      [interrupted-condition? ]
      [make-interrupted-condition ]
      [$apply-nonprocedure-error-handler ]
      [$incorrect-args-error-handler ]
      [$multiple-values-error ]
      [$debug ]
      [$underflow-misaligned-error ]
      [top-level-value-error ]
      [car-error ]
      [cdr-error ]
      [fxadd1-error ]
      [fxsub1-error ]
      [cadr-error ]
      [fx+-type-error ]
      [fx+-types-error ]
      [fx+-overflow-error ]
      [$do-event ]
      [do-overflow ]
      [do-overflow-words ]
      [do-vararg-overflow ]
      [collect ]
      [collect-key ]
      [do-stack-overflow ]
      [make-promise ]
      [make-traced-procedure ]
      [error@fx+ ]
      [error@fxarithmetic-shift-left ]
      [error@fx* ]
      [error@fx- ]
      [error@add1 ]
      [error@sub1 ]
      [error@fxadd1 ]
      [error@fxsub1 ]
      [< ]
      [<= ]
      [= ]
      [> ]
      [>= ]
      [+ ]
      [- ]
      [* ]
      [/ ]
      [abs ]
      [acos ]
      [angle ]
      [append ]
      [asin ]
      [atan ]
      [boolean=? ]
      [boolean? ]
      [car ]
      [cdr ]
      [caar ]
      [cadr ]
      [cdar ]
      [cddr ]
      [caaar ]
      [caadr ]
      [cadar ]
      [caddr ]
      [cdaar ]
      [cdadr ]
      [cddar ]
      [cdddr ]
      [caaaar ]
      [caaadr ]
      [caadar ]
      [caaddr ]
      [cadaar ]
      [cadadr ]
      [caddar ]
      [cadddr ]
      [cdaaar ]
      [cdaadr ]
      [cdadar ]
      [cdaddr ]
      [cddaar ]
      [cddadr ]
      [cdddar ]
      [cddddr ]
      [ceiling ]
      [char->integer ]
      [char<=? ]
      [char<? ]
      [char=? ]
      [char>=? ]
      [char>? ]
      [char? ]
      [complex? ]
      [cons ]
      [cos ]
      [denominator ]
      [div ]
      [mod ]
      [div0 ]
      [mod0 ]
      [dynamic-wind ]
      [eq? ]
      [equal? ]
      [eqv? ]
      [even? ]
      [exact ]
      [exact-integer-sqrt ]
      [exact? ]
      [exp ]
      [expt ]
      [finite? ]
      [floor ]
      [gcd ]
      [imag-part ]
      [inexact ]
      [inexact? ]
      [infinite? ]
      [integer->char ]
      [integer-valued? ]
      [integer? ]
      [lcm ]
      [length ]
      [list ]
      [list->string ]
      [list->vector ]
      [list-ref ]
      [list-tail ]
      [list? ]
      [log ]
      [magnitude ]
      [make-polar ]
      [make-rectangular ]
      [$make-rectangular ]
      [make-string ]
      [make-vector ]
      [map ]
      [max ]
      [min ]
      [nan? ]
      [negative? ]
      [not ]
      [null? ]
      [number->string ]
      [number? ]
      [numerator ]
      [odd? ]
      [pair? ]
      [positive? ]
      [procedure? ]
      [rational-valued? ]
      [rational? ]
      [rationalize ]
      [real-part ]
      [real-valued? ]
      [real? ]
      [reverse ]
      [round ]
      [sin ]
      [sqrt ]
      [string ]
      [string->list ]
      [string->number ]
      [string->symbol ]
      [string-append ]
      [string-copy ]
      [string-for-each ]
      [string-length ]
      [string-ref ]
      [string<=? ]
      [string<? ]
      [string=? ]
      [string>=? ]
      [string>? ]
      [string? ]
      [substring ]
      [symbol->string ]
      [symbol=? ]
      [symbol? ]
      [tan ]
      [truncate ]
      [vector ]
      [vector->list ]
      [vector-fill! ]
      [vector-for-each ]
      [vector-length ]
      [vector-map ]
      [vector-ref ]
      [vector? ]
      [zero? ]
      [bitwise-arithmetic-shift ]
      [bitwise-arithmetic-shift-left ]
      [bitwise-arithmetic-shift-right ]
      [bitwise-not ]
      [bitwise-and ]
      [bitwise-ior ]
      [bitwise-xor ]
      [bitwise-bit-count ]
      [bitwise-bit-field ]
      [bitwise-bit-set? ]
      [bitwise-copy-bit ]
      [bitwise-copy-bit-field ]
      [bitwise-first-bit-set ]
      [bitwise-if ]
      [bitwise-length ]
      [bitwise-reverse-bit-field ]
      [bitwise-rotate-bit-field ]
      [fixnum? ]
      [fixnum-width ]
      [least-fixnum ]
      [greatest-fixnum ]
      [fx* ]
      [fx*/carry ]
      [fx+ ]
      [fx+/carry ]
      [fx- ]
      [fx-/carry ]
      [fx<=? ]
      [fx<? ]
      [fx=? ]
      [fx>=? ]
      [fx>? ]
      [fxand ]
      [fxarithmetic-shift ]
      [fxarithmetic-shift-left ]
      [fxarithmetic-shift-right ]
      [fxbit-count ]
      [fxbit-field ]
      [fxbit-set? ]
      [fxcopy-bit ]
      [fxcopy-bit-field ]
      [fxdiv ]
      [fxdiv-and-mod ]
      [fxdiv0 ]
      [fxdiv0-and-mod0 ]
      [fxeven? ]
      [fxfirst-bit-set ]
      [fxif ]
      [fxior ]
      [fxlength ]
      [fxmax ]
      [fxmin ]
      [fxmod ]
      [fxmod0 ]
      [fxnegative? ]
      [fxnot ]
      [fxodd? ]
      [fxpositive? ]
      [fxreverse-bit-field ]
      [fxrotate-bit-field ]
      [fxxor ]
      [fxzero? ]
      [fixnum->flonum ]
      [fl* ]
      [fl+ ]
      [fl- ]
      [fl/ ]
      [fl<=? ]
      [fl<? ]
      [fl=? ]
      [fl>=? ]
      [fl>? ]
      [flabs ]
      [flacos ]
      [flasin ]
      [flatan ]
      [flceiling ]
      [flcos ]
      [fldenominator ]
      [fldiv ]
      [fldiv0 ]
      [fleven? ]
      [flexp ]
      [flexpt ]
      [flfinite? ]
      [flfloor ]
      [flinfinite? ]
      [flinteger? ]
      [fllog ]
      [flmax ]
      [flmin ]
      [flmod ]
      [flmod0 ]
      [flnan? ]
      [flnegative? ]
      [flnumerator ]
      [flodd? ]
      [flonum? ]
      [flpositive? ]
      [flround ]
      [flsin ]
      [flsqrt ]
      [fltan ]
      [fltruncate ]
      [flzero? ]
      [real->flonum ]
      [bytevector->sint-list ]
      [bytevector->u8-list ]
      [bytevector->uint-list ]
      [bytevector-copy ]
      [bytevector-ieee-double-native-ref ]
      [bytevector-ieee-double-ref ]
      [bytevector-ieee-single-native-ref ]
      [bytevector-ieee-single-ref ]
      [bytevector-length ]
      [bytevector-s16-native-ref ]
      [bytevector-s16-ref ]
      [bytevector-s32-native-ref ]
      [bytevector-s32-ref ]
      [bytevector-s64-native-ref ]
      [bytevector-s64-ref ]
      [bytevector-s8-ref ]
      [bytevector-sint-ref ]
      [bytevector-u16-native-ref ]
      [bytevector-u16-ref ]
      [bytevector-u32-native-ref ]
      [bytevector-u32-ref ]
      [bytevector-u64-native-ref ]
      [bytevector-u64-ref ]
      [bytevector-u8-ref ]
      [bytevector-uint-ref ]
      [bytevector=? ]
      [bytevector? ]
      [endianness ]
      [native-endianness ]
      [sint-list->bytevector ]
      [string->utf16 ]
      [string->utf32 ]
      [string->utf8 ]
      [u8-list->bytevector ]
      [uint-list->bytevector ]
      [utf8->string ]
      [utf16->string ]
      [utf32->string ]
      [bytevector->string ]
      [assoc ]
      [assp ]
      [assq ]
      [assv ]
      [cons* ]
      [member ]
      [memp ]
      [memq ]
      [memv ]
      [exact->inexact ]
      [inexact->exact ]
      [modulo ]
      [remainder ]
      [quotient ]
      [make-bytevector ]
      [string->bytevector ]
      [eof-object ]
      [eof-object? ]
      [record-field-mutable? ]
      [record-rtd ]
      [record-type-field-names ]
      [record-type-generative? ]
      [record-type-name ]
      [record-type-opaque? ]
      [record-type-parent ]
      [record-type-sealed? ]
      [record-type-uid ]
      [record? ]
      [make-record-constructor-descriptor ]
      [make-record-type-descriptor ]
      [record-accessor ]
      [record-constructor ]
      [record-mutator ]
      [record-predicate ]
      [record-type-descriptor? ]
      [bound-identifier=? ]
      [datum->syntax ]
      [syntax ]
      [syntax->datum ]
      [syntax-case ]
      [unsyntax ]
      [unsyntax-splicing ]
      [quasisyntax ]
      [with-syntax ]
      [free-identifier=? ]
      [generate-temporaries ]
      [identifier? ]
      [make-variable-transformer ]
      [char-alphabetic? ]
      [char-ci<=? ]
      [char-ci<? ]
      [char-ci=? ]
      [char-ci>=? ]
      [char-ci>? ]
      [char-downcase ]
      [char-foldcase ]
      [char-titlecase ]
      [char-upcase ]
      [char-general-category ]
      [char-lower-case? ]
      [char-numeric? ]
      [char-title-case? ]
      [char-upper-case? ]
      [char-whitespace? ]
      [string-ci<=? ]
      [string-ci<? ]
      [string-ci=? ]
      [string-ci>=? ]
      [string-ci>? ]
      [string-downcase ]
      [string-foldcase ]
      [string-normalize-nfc ]
      [string-normalize-nfd ]
      [string-normalize-nfkc ]
      [string-normalize-nfkd ]
      [string-titlecase ]
      [string-upcase ]
      [void ]
      [gensym ]
      ))
      


  ;;; business
  (define-syntax ctxt-case
    (lambda (stx)
      (define (test x)
        (case (syntax->datum x)
          [(p)   #'(eq? t 'p)]
          [(v)   #'(eq? t 'v)]
          [(e)   #'(eq? t 'e)]
          [(app) #'(app? t)]
          [else (syntax-violation stx "invalid ctxt" x)]))
      (define (extract cls*)
        (syntax-case cls* (else)
          [() #'(error 'extract "unmatched ctxt")]
          [([else e e* ...]) #'(begin e e* ...)]
          [([(t* ...) e e* ...] rest ...) 
           (with-syntax ([(t* ...) (map test #'(t* ...))]
                         [body (extract #'(rest ...))])
             #'(if (or t* ...) 
                   (begin e e* ...)
                   body))]))
      (syntax-case stx ()
        [(_ expr cls* ...)
         (with-syntax ([body (extract #'(cls* ...))])
           #'(let ([t expr])
               body))])))
  (define (mkseq e0 e1)
    ;;; returns a (seq e0 e1) with a seq-less e1 if both 
    ;;; e0 and e1 are constructed properly.
    (if (simple? e0)
        e1
        (let ([e0 (struct-case e0
                    [(seq e0a e0b) (if (simple? e0b) e0a e0)]
                    [else e0])])
          (struct-case e1
            [(seq e1a e1b) (make-seq (make-seq e0 e1a) e1b)]
            [else (make-seq e0 e1)]))))
  ;;; simple?: check quickly whether something is effect-free
  (define (simple? x) 
    (struct-case x
      [(constant) #t]
      [(var)      #t]
      [(primref)  #t]
      [(clambda)  #t]
      [else       #f]))
  ;;; result returns the "last" value of an expression
  (define (result-expr x)
    (struct-case x
      [(seq e0 e1) e1]
      [else        x]))
  ;;;
  (define (records-equal? x y ctxt)
    (struct-case x
      [(constant kx)
       (struct-case y
         [(constant ky)
          (ctxt-case ctxt
            [(e) #t]
            [(p) (if kx ky (not ky))]
            [else (eq? kx ky)])]
         [else #f])]
      [else #f]))
  ;;;
  (define* (residualize-operands e rand* sc)
    (cond
      [(null? rand*) e]
      [(not (operand-residualize-for-effect (car rand*)))
       (residualize-operands e (cdr rand*) sc)]
      [else
       (let ([opnd (car rand*)])
         (let ([e1 (or (operand-value opnd)
                       (struct-case opnd
                         [(operand expr env ec)
                          (E expr 'effect env ec sc)]))])
           (if (simple? e1)
               (residualize-operands e (cdr rand*) sc)
               (begin
                 (decrement sc (operand-size opnd))
                 (mkseq e1 (residualize-operands e (cdr rand*) sc))))))]))
  (define* (value-visit-operand! rand)
    (or (operand-value rand)
        (let ([sc (passive-counter)])
          (let ([e (struct-case rand
                     [(operand expr env ec) 
                      (E expr 'v env sc ec)])])
            (set-operand-value! rand e)
            (set-operand-size! rand (passive-counter-value sc))
            e))))
  (define* (score-value-visit-operand! rand sc)
    (let ([val (value-visit-operand! rand)])
      (decrement sc (operand-size rand))
      val))
  (define* (E-call rator rand* env ctxt ec sc)
    (let ([ctxt (make-app rand* ctxt)])
      (let ([rator (E rator ctxt env ec sc)])
        (if (app-inlined ctxt)
            (residualize-operands rator rand* sc)
            (make-funcall rator 
              (map (lambda (x) (score-value-visit-operand! x sc)) rand*))))))
  ;;;
  (define* (E-var x ctxt env ec sc)
    (ctxt-case ctxt
      [(e) (make-constant (void))]
      [else 
       (let ([x (lookup x env)])
         (let ([opnd (prelex-operand (var-prelex x))])
           (if (and opnd (not (operand-inner-pending opnd)))
               (begin
                 (dynamic-wind
                   (lambda () (set-operand-inner-pending! opnd #t))
                   (lambda () (value-visit-operand! opnd))
                   (lambda () (set-operand-inner-pending! opnd #f)))
                 (if (prelex-source-assigned? (var-prelex x))
                     (residualize-ref x sc)
                     (copy x opnd ctxt ec sc)))
               (residualize-ref x sc))))]))
  ;;;
  (define* (copy x opnd ctxt ec sc)
    (let ([rhs (result-expr (operand-value opnd))])
      (struct-case rhs
        [(constant) rhs]
        [(var)
         (if (prelex-source-assigned? (var-prelex rhs))
             (residualize-ref x sc)
             (let ([opnd (prelex-operand (var-prelex rhs))])
               (if (and opnd (operand-value opnd))
                   (copy2 rhs opnd ctxt ec sc)
                   (residualize-ref rhs sc))))]
        [else (copy2 x opnd ctxt ec sc)])))
  ;;;
  (define* (copy2 x opnd ctxt ec sc)
    (let ([rhs (result-expr (operand-value opnd))])
      (struct-case rhs
        [(clambda) 
         (ctxt-case ctxt
           [(v) (residualize-ref x sc)]
           [(p) (make-constant #t)]
           [(e) (make-constant (void))]
           [(app) 
            (or (and (not (operand-outer-pending opnd))
                     (dynamic-wind
                       (lambda () (set-operand-outer-pending! opnd #t))
                       (lambda ()
                         (call/cc
                           (lambda (abort)
                             (inline rhs ctxt empty-env ;(operand-env opnd) ; empty-env 
                               (if (active-counter? ec)
                                   ec
                                   (make-counter
                                     (cp0-effort-limit)
                                     ctxt abort))
                               (make-counter 
                                 (if (active-counter? sc)
                                     (counter-value sc)
                                     (cp0-size-limit))
                                 ctxt abort)))))
                       (lambda () (set-operand-outer-pending! opnd #f))))
                (residualize-ref x sc))])]
        [(primref p)
         (ctxt-case ctxt
           [(v) rhs]
           [(p) (make-constant #t)]
           [(e) (make-constant (void))]
           [(app) (fold-prim p ctxt ec sc)])]
        [else (residualize-ref x sc)])))
  ;;;
  (define* (inline proc ctxt env ec sc)
    (define (get-case cases rand*)
      (define (compatible? x)
        (struct-case (clambda-case-info x) 
          [(case-info label args proper)
           (cond
             [proper (= (length rand*) (length args))]
             [else #|FIXME|# #f])]))
      (cond
        [(memp compatible? cases) => car]
        [else #f]))
    (struct-case proc
      [(clambda g cases cp free name)
       (let ([rand* (app-rand* ctxt)])
         (struct-case (get-case cases rand*)
           [(clambda-case info body)
            (struct-case info
              [(case-info label args proper) 
               (with-extended-env ((env args) (env args rand*))
                 (let ([body (E body (app-ctxt ctxt) env ec sc)])
                   (let ([result (make-let-binding args rand* body sc)])
                     (set-app-inlined! ctxt #t)
                     result)))])]
           [else ((counter-k ec) #f)]))]))
  ;;;
  (define* (do-bind lhs* rhs* body ctxt env ec sc)
    (E (make-funcall 
         (make-clambda (gensym) 
           (list
             (make-clambda-case
               (make-case-info (gensym) lhs* #t)
               body))
           #f #f #f)
         rhs*)
       ctxt env ec sc))
  ;;;
  (define* (make-let-binding var* rand* body sc)
    (define (process1 var rand ef* lhs* rhs*)
      (cond
        [(prelex-residual-referenced? (var-prelex var))
         (values ef* 
            (cons var lhs*) 
            (cons (score-value-visit-operand! rand sc) rhs*))]
        [(prelex-residual-assigned? (var-prelex var))
         (values ef*
            (cons var lhs*)
            (cons (make-constant (void)) rhs*))]
        [else
         (set-operand-residualize-for-effect! rand #t)
         (values (cons rand ef*) lhs* rhs*)]))
    (define (process var* rand*)
      (cond
        [(null? var*) (values '() '() '())]
        [else
         (let ([var (car var*)] [rand (car rand*)])
           (let-values ([(ef* lhs* rhs*) (process (cdr var*) (cdr rand*))])
             (process1 var rand ef* lhs* rhs*)))]))
    (let-values ([(ef* lhs* rhs*) (process var* rand*)])
      (residualize-operands 
        (if (null? lhs*) body (make-bind lhs* rhs* body)) 
        ef*
        sc)))
  ;;;
  (define* (fold-prim p ctxt ec sc)
    ;;; TODO
    (make-primref p))
  ;;;
  (define* (residualize-ref x sc)
    (decrement sc 1)
    (set-prelex-residual-referenced?! (var-prelex x) #t)
    x)
  ;;;
  (define indent-level (make-parameter 0))
  (define (printf/indent s . args)
    (let f ([i (indent-level)])
      (unless (zero? i)
        (printf " |")
        (f (- i 1))))
    (apply printf s args))
  (define (pretty-print/indent x)
    (let ([p
           (open-string-input-port
             (let-values ([(p e) (open-string-output-port)])
               (parameterize ([pretty-width 200])
                 (pretty-print x p))
               (e)))])
      (let f ()
        (let ([x (get-line p)])
          (unless (eof-object? x)
            (printf/indent "~a\n" x)
            (f))))))

  (define* (E^ x ctxt env ec sc)
     (define (env-list env)
       (cond
         [(null? env) '()]
         [else (append (vector-ref env 0) (env-list (vector-ref env 2)))]))
     (printf/indent "[ENV: ~s ~s\n" (if (app? ctxt) 'app ctxt) (map unparse (env-list env)))
     (pretty-print/indent (unparse x))
     (let ([v 
            (parameterize ([indent-level (add1 (indent-level))])
              (E^ x ctxt env ec sc))])
       (printf/indent "=>\n")
       (pretty-print/indent (unparse v))
       (printf/indent "=================================]\n")
       v))
  (define* (E x ctxt env ec sc)
    (decrement ec 1)
    (struct-case x
      [(constant) x]
      [(var) (E-var x ctxt env ec sc)]
      [(seq e0 e1)
       (mkseq (E e0 'e env ec sc) (E e1 ctxt env ec sc))]
      [(conditional e0 e1 e2)
       (let ([e0 (E e0 'p env ec sc)])
         (struct-case (result-expr e0)
           [(constant k) 
            (mkseq e0 (E (if k e1 e2) ctxt env ec sc))]
           [else 
            (let ([ctxt (ctxt-case ctxt [(app) 'v] [else ctxt])])
              (let ([e1 (E e1 ctxt env ec sc)]
                    [e2 (E e2 ctxt env ec sc)])
                (if (records-equal? e1 e2 ctxt)
                    (mkseq e0 e1)
                    (begin 
                      (decrement sc 1)
                      (make-conditional e0 e1 e2)))))]))]
      [(assign x v)
       (mkseq
         (let ([x (lookup x env)])
           (let ([xi (var-prelex x)])
             (cond
               [(not (prelex-source-referenced? xi))
                ;;; dead on arrival
                (E v 'e env ec sc)]
               [else
                (decrement sc 1)
                (set-prelex-residual-assigned?! xi #t)
                (make-assign x (E v 'v env ec sc))])))
         (make-constant (void)))]
      [(funcall rator rand*)
       (E-call rator 
        (map (lambda (x) (make-operand x env ec)) rand*)
        env ctxt ec sc)]
      [(forcall name rand*)
       (make-forcall name (map (lambda (x) (E x 'v env ec sc)) rand*))]
      [(primref name)
       (ctxt-case ctxt
         [(app) (fold-prim name ctxt ec sc)]
         [(v) x]
         [else (make-constant #t)])]
      [(clambda g cases cp free name) 
       (ctxt-case ctxt
         [(app) (inline x ctxt env ec sc)]
         [(p e) (make-constant (void))]
         [else
          (decrement sc 1)
          (make-clambda (gensym g)
            (map 
              (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (struct-case info
                     [(case-info label args proper) 
                      (with-extended-env ((env args) (env args #f))
                        (make-clambda-case 
                          (make-case-info (gensym label) args proper)
                          (E body 'v env ec sc)))])]))
              cases)
            cp free name)])]
      [(bind lhs* rhs* body) 
       (do-bind lhs* rhs* body ctxt env ec sc)]
      [(fix lhs* rhs* body)
       (with-extended-env ((env lhs*) (env lhs* #f))
         (for-each
           (lambda (lhs rhs)
             (assert (not (prelex-operand (var-prelex lhs))))
             (set-prelex-operand! (var-prelex lhs)
               (make-operand rhs env ec)))
            lhs* rhs*)
         (let ([body (E body ctxt env ec sc)])
           (let ([lhs* (remp 
                         (lambda (x)
                           (not (prelex-residual-referenced?
                                  (var-prelex x))))
                         lhs*)])
             (cond
               [(null? lhs*) body]
               [else
                (make-fix lhs* 
                  (map (lambda (x)
                         (value-visit-operand! 
                           (prelex-operand (var-prelex x))))
                       lhs*)
                  body)]))))]
      [else (error who "invalid expression" caller x)]))
  (define empty-env '())
  (define* (lookup x orig-env)
    (define (lookup env)
      (cond
        [(vector? env)
         (let f ([lhs* (vector-ref env 0)] [rhs* (vector-ref env 1)])
           (cond
             [(null? lhs*) (lookup (vector-ref env 2))]
             [(eq? x (car lhs*)) (car rhs*)]
             [else (f (cdr lhs*) (cdr rhs*))]))]
        [else x]))
    (lookup orig-env))
  (define debug-optimizer (make-parameter #f))
  (define (source-optimize expr)
    (prepare expr)
    (when (debug-optimizer)
      (newline)
      (pretty-print (unparse expr)))
    (let ([expr (E expr 'v empty-env 
                   (passive-counter)
                   (passive-counter))])
      (when (debug-optimizer)
        (printf "=>\n")
        (pretty-print (unparse expr))
        (printf "============================================\n"))
      expr))
)




