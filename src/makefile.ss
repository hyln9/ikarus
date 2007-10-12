#!/usr/bin/env ikarus -b ikarus.boot --r6rs-script


(import (except (ikarus) assembler-output)
        (ikarus compiler)
        (except (psyntax system $bootstrap)
                eval-core
                current-primitive-locations
                compile-core-expr-to-port))


(define scheme-library-files
  ;;; Listed in the order in which they're loaded.
  ;;;
  ;;; Loading of the boot file may segfault if a library is
  ;;; loaded before its dependencies are loaded first.
  ;;;
  ;;; reason is that the base libraries are not a hierarchy of
  ;;; dependencies but rather an eco system in which every
  ;;; part depends on the other.
  ;;;
  ;;; For example, the printer may call error if it finds
  ;;;  an error (e.g. "not an output port"), while the error
  ;;;  procedure may call the printer to display the message.
  ;;;  This works fine as long as error does not itself cause
  ;;;  an error (which may lead to the infamous Error: Error: 
  ;;;  Error: Error: Error: Error: Error: Error: Error: ...).
  ;;;
  '("ikarus.singular-objects.ss"
    "ikarus.handlers.ss"
    "ikarus.multiple-values.ss"
    "ikarus.control.ss"
    "ikarus.collect.ss"
    "ikarus.apply.ss"
    "ikarus.predicates.ss"
    "ikarus.pairs.ss"
    "ikarus.lists.ss"
    "ikarus.fixnums.ss"
    "ikarus.chars.ss"
    "ikarus.records.ss"
    "ikarus.strings.ss"
    "ikarus.transcoders.ss"
    "ikarus.date-string.ss"
    "ikarus.symbols.ss"
    "ikarus.vectors.ss"
    "ikarus.numerics.ss"
    "ikarus.guardians.ss"
    "ikarus.command-line.ss"
    "ikarus.codecs.ss"
    "ikarus.io-ports.ss"
    "ikarus.io-primitives.unsafe.ss"
    "ikarus.io-primitives.ss"
    "ikarus.io.input-files.ss"
    "ikarus.io.output-files.ss"
    "ikarus.io.input-strings.ss"
    "ikarus.io.output-strings.ss"
    "ikarus.hash-tables.ss"
    "ikarus.unicode-data.ss"
    "ikarus.writer.ss"
    "ikarus.reader.ss"
    "ikarus.code-objects.ss"
    "ikarus.intel-assembler.ss"
    "ikarus.trace.ss"
    "ikarus.fasl.write.ss"
    "ikarus.fasl.ss"
    "ikarus.compiler.ss"
    "psyntax.compat.ss"
    "psyntax.library-manager.ss"
    "psyntax.internal.ss"
    "psyntax.config.ss"
    "psyntax.builders.ss"
    "psyntax.expander.ss"
    "ikarus.load.ss"
    "ikarus.pretty-print.ss"
    "ikarus.cafe.ss"
    "ikarus.posix.ss"
    "ikarus.timer.ss"
    "ikarus.bytevectors.ss"
    "ikarus.sort.ss"
    "ikarus.promises.ss"
    "ikarus.main.ss"))

(define ikarus-system-macros
  '([define            (define)]
    [define-syntax     (define-syntax)]
    [module            (module)]
    [begin             (begin)]
    [import            (import)]
    [set!              (set!)]
    [let-syntax        (let-syntax)]
    [letrec-syntax     (letrec-syntax)]
    [foreign-call      (core-macro . foreign-call)]
    [quote             (core-macro . quote)]
    [syntax-case       (core-macro . syntax-case)]
    [syntax            (core-macro . syntax)]
    [lambda            (core-macro . lambda)]
    [case-lambda       (core-macro . case-lambda)]
    [type-descriptor   (core-macro . type-descriptor)]
    [letrec            (core-macro . letrec)]
    [letrec*           (core-macro . letrec*)]
    [if                (core-macro . if)]
    [when              (core-macro . when)]         
    [unless            (core-macro . unless)]
    [parameterize      (core-macro . parameterize)]
    [case              (core-macro . case)]
    [let-values        (core-macro . let-values)]
    [define-record     (macro . define-record)]
    [include           (macro . include)]
    [syntax-rules      (macro . syntax-rules)]
    [quasiquote        (macro . quasiquote)]
    [quasisyntax       (macro . quasisyntax)]
    [with-syntax       (macro . with-syntax)]
    [identifier-syntax (macro . identifier-syntax)]
    [let               (macro . let)]
    [let*              (macro . let*)]
    [cond              (macro . cond)]
    [do                (macro . do)]
    [and               (macro . and)]
    [or                (macro . or)]
    [time              (macro . time)]
    [delay             (macro . delay)]
    [endianness        (macro . endianness)]
    [assert            (macro . assert)]
    [...               (macro . ...)]
    [=>                (macro . =>)]
    [else              (macro . else)]
    [_                 (macro . _)]
    [unquote           (macro . unquote)]
    [unquote-splicing  (macro . unquote-splicing)]
    [unsyntax          (macro . unsyntax)]
    [unsyntax-splicing (macro . unsyntax-splicing)]
    [trace-lambda      (macro . trace-lambda)]
    [trace-define      (macro . trace-define)]
    [eol-style         (macro . eol-style)]
    ))

(define library-legend
  ;; abbr.       name                             visible? required?
  '([i           (ikarus)                              #t     #t]
    [cm          (chez modules)                        #t     #t]
    [symbols     (ikarus symbols)                      #t     #t]
    [parameters  (ikarus parameters)                   #t     #t]
    [interaction (ikarus interaction)                  #t     #t]
    [ne          (ikarus null-environment-5)           #t     #t]
    [se          (ikarus scheme-report-environment-5)  #t     #t]
    [r           (rnrs)                                #t     #t]
    [r5          (rnrs r5rs)                           #t     #t]
    [ct          (rnrs control)                        #t     #t]
    [ev          (rnrs eval)                           #t     #t]
    [mp          (rnrs mutable-pairs)                  #t     #t]
    [ms          (rnrs mutable-strings)                #t     #t]
    [pr          (rnrs programs)                       #t     #t]
    [sc          (rnrs syntax-case)                    #t     #t]
    [fi          (rnrs files)                          #t     #t]
    [sr          (rnrs sorting)                        #t     #t]
    [ba          (rnrs base)                           #t     #t]
    [ls          (rnrs lists)                          #t     #t]
    [is          (rnrs io simple)                      #t     #t]
    [bv          (rnrs bytevectors)                    #t     #t]
    [uc          (rnrs unicode)                        #t     #t]
    [ex          (rnrs exceptions)                     #t     #t]
    [bw          (rnrs arithmetic bitwise)             #t     #t]
    [fx          (rnrs arithmetic fixnums)             #t     #t]
    [fl          (rnrs arithmetic flonums)             #t     #t]
    [ht          (rnrs hashtables)                     #t     #t]
    [ip          (rnrs io ports)                       #t     #t]
    [en          (rnrs enums)                          #t     #t]
    [co          (rnrs conditions)                     #t     #t]
    [ri          (rnrs records inspection)             #t     #t]
    [rp          (rnrs records procedural)             #t     #t]
    [rs          (rnrs records syntactic)              #t     #t]
    [$pairs      (ikarus system $pairs)                #f     #t]
    [$lists      (ikarus system $lists)                #f     #t]
    [$chars      (ikarus system $chars)                #f     #t]
    [$strings    (ikarus system $strings)              #f     #t]
    [$vectors    (ikarus system $vectors)              #f     #t]
    [$flonums    (ikarus system $flonums)              #f     #t]
    [$bignums    (ikarus system $bignums)              #f     #t]
    [$bytes      (ikarus system $bytevectors)          #f     #t]
    [$fx         (ikarus system $fx)                   #f     #t]
    [$rat        (ikarus system $ratnums)              #f     #t]
    [$symbols    (ikarus system $symbols)              #f     #t]
    [$records    (ikarus system $records)              #f     #t]
    [$ports      (ikarus system $ports)                #f     #t]
    [$codes      (ikarus system $codes)                #f     #t]
    [$tcbuckets  (ikarus system $tcbuckets)            #f     #t]
    [$io         (ikarus system $io)                   #f     #t]
    [$arg-list   (ikarus system $arg-list)             #f     #t]
    [$stack      (ikarus system $stack)                #f     #t]
    [$interrupts (ikarus system $interrupts)           #f     #t]
    [$all        (psyntax system $all)                 #f     #t]
    [$boot       (psyntax system $bootstrap)           #f     #t]
    ))

(define identifier->library-map
  '(
    [import                                      i]
    [foreign-call                                i]
    [type-descriptor                             i]
    [parameterize                                i parameters]
    [define-record                               i]
    [include                                     i r]
    [time                                        i]
    [trace-lambda                                i]
    [trace-define                                i]
    [make-list                                   i r]
    [last-pair                                   i r]
    [bwp-object?                                 i]
    [weak-cons                                   i]
    [weak-pair?                                  i]
    [uuid                                        i]
    [date-string                                 i]
    [string->utf8-bytevector                     i]
    [utf8-bytevector->string                     i]
    [$two-bignums                                i]
    [andmap                                      i]
    [ormap                                       i]
    [fx<                                         i]
    [fx<=                                        i]
    [fx>                                         i]
    [fx>=                                        i]
    [fx=                                         i]
    [fxadd1                                      i]
    [fxsub1                                      i]
    [fxquotient                                  i]
    [fxremainder                                 i]
    [fxmodulo                                    i]
    [fxsll                                       i]
    [fxsra                                       i]
    [sra                                         i]
    [sll                                         i]
    [fxlogand                                    i]
    [logand                                      i]
    [fxlogxor                                    i]
    [fxlogor                                     i]
    [fxlognot                                    i]
    [fixnum->string                              i]
    [string->flonum                              i]
    [add1                                        i]
    [sub1                                        i]
    [bignum?                                     i]
    [ratnum?                                     i]
    [flonum-parts                                i]
    [flonum-bytes                                i]
    [quotient+remainder                          i r]
    [flonum->string                              i]
    [random                                      i]
    [gensym?                                     i symbols]
    [getprop                                     i symbols]
    [putprop                                     i symbols]
    [remprop                                     i symbols]
    [property-list                               i symbols]
    [gensym->unique-string                       i symbols]
    [symbol-bound?                               i symbols]
    [top-level-value                             i symbols]
    [reset-symbol-proc!                          i symbols]
    [make-guardian                               i]
    [make-input-port                             i]
    [make-output-port                            i]
    [port-output-index                           i]
    [port-output-size                            i]
    [port-output-buffer                          i]
    [set-port-output-index!                      i]
    [set-port-output-size!                       i]
    [port-input-buffer                           i]
    [port-input-index                            i]
    [port-input-size                             i]
    [set-port-input-index!                       i]
    [set-port-input-size!                        i]
    [port-name                                   i]
    [input-port-name                             i]
    [output-port-name                            i]
    [with-input-from-string                      i]
    [open-output-string                          i]
    [open-input-string                           i r]
    [get-output-string                           i]
    [with-output-to-string                       i]
    [console-input-port                          i]
    [console-output-port                         i]
    [reset-input-port!                           i]
    [write-byte                                  i]
    [read-token                                  i]
    [unread-char                                 i]
    [printf                                      i]
    [format                                      i]
    [comment-handler                             i]
    [print-gensym                                i symbols]
    [print-graph                                 i]
    [print-unicode                               i]
    [gensym-count                                i symbols]
    [gensym-prefix                               i symbols]
    [make-parameter                              i parameters]
    [call/cf                                     i]
    [print-error                                 i]
    [error-handler                               i]
    [interrupt-handler                           i]
    [assembler-output                            i]
    [new-cafe                                    i]
    [expand                                      i]
    [environment?                                i]
    [time-it                                     i]
    [command-line-arguments                      i]
    [make-record-type                            i]
    [record-type-symbol                          i]
    [set-rtd-printer!                            i]
    [record-name                                 i]
    [record-length                               i]
    [record-printer                              i]
    [record-ref                                  i]
    [record-set!                                 i]
    [record-field-accessor                       i]
    [record-field-mutator                        i]
    [code?                                       i]
    [immediate?                                  i]
    [pointer-value                               i]
    [system                                      i]
    [installed-libraries                         i]
    [current-primitive-locations                 $boot]
    [boot-library-expand                         $boot]
    [current-library-collection                  $boot]
    [library-name                                $boot]
    [find-library-by-name                        $boot]
    [$car                                        $pairs]
    [$cdr                                        $pairs]
    [$set-car!                                   $pairs]
    [$set-cdr!                                   $pairs]
    [$memq                                       $lists]
    [$memv                                       $lists]
    [$char?                                      $chars]
    [$char=                                      $chars]
    [$char<                                      $chars]
    [$char>                                      $chars]
    [$char<=                                     $chars]
    [$char>=                                     $chars]
    [$char->fixnum                               $chars]
    [$fixnum->char                               $chars]
    [$make-string                                $strings]
    [$string-ref                                 $strings]
    [$string-set!                                $strings]
    [$string-length                              $strings]
    [$make-bytevector                            $bytes]
    [$bytevector-length                          $bytes]
    [$bytevector-s8-ref                          $bytes]
    [$bytevector-u8-ref                          $bytes]
    [$bytevector-set!                            $bytes]
    [$flonum-u8-ref                              $flonums]
    [$make-flonum                                $flonums]
    [$flonum-set!                                $flonums]
    [$flonum-signed-biased-exponent              $flonums]
    [$flonum-rational?                           $flonums]
    [$flonum-integer?                            $flonums]
    [$fl+                                        $flonums]
    [$fl-                                        $flonums]
    [$fl*                                        $flonums]
    [$fl/                                        $flonums]
    [$fl=                                        $flonums]
    [$fl<                                        $flonums]
    [$fl<=                                       $flonums]
    [$fl>                                        $flonums]
    [$fl>=                                       $flonums]
    [$fixnum->flonum                             $flonums]
    [$make-bignum                                $bignums]
    [$bignum-positive?                           $bignums]
    [$bignum-size                                $bignums]
    [$bignum-byte-ref                            $bignums]
    [$bignum-byte-set!                           $bignums]
    [$make-ratnum                                $rat]
    [$ratnum-n                                   $rat]
    [$ratnum-d                                   $rat]
    [$make-vector                                $vectors]
    [$vector-length                              $vectors]
    [$vector-ref                                 $vectors]
    [$vector-set!                                $vectors]
    [$fxzero?                                    $fx]
    [$fxadd1                                     $fx]
    [$fxsub1                                     $fx]
    [$fx>=                                       $fx]
    [$fx<=                                       $fx]
    [$fx>                                        $fx]
    [$fx<                                        $fx]
    [$fx=                                        $fx]
    [$fxsll                                      $fx]
    [$fxsra                                      $fx]
    [$fxquotient                                 $fx]
    [$fxmodulo                                   $fx]
    [$fxlogxor                                   $fx]
    [$fxlogor                                    $fx]
    [$fxlognot                                   $fx]
    [$fxlogand                                   $fx]
    [$fx+                                        $fx]
    [$fx*                                        $fx]
    [$fx-                                        $fx]
    [$fxinthash                                  $fx]
    [$make-symbol                                $symbols]
    [$symbol-unique-string                       $symbols]
    [$symbol-value                               $symbols]
    [$symbol-string                              $symbols]
    [$symbol-plist                               $symbols]
    [$set-symbol-value!                          $symbols]
    [$set-symbol-proc!                           $symbols]
    [$set-symbol-string!                         $symbols]
    [$set-symbol-unique-string!                  $symbols]
    [$set-symbol-plist!                          $symbols]
    [$init-symbol-value!                         ]
    [$unbound-object?                            $symbols]
    [base-rtd                                    $records]
    [$record-set!                                $records]
    [$record-ref                                 $records]
    [$record-rtd                                 $records]
    [$record                                     $records]
    [$make-record                                $records]
    [$record?                                    $records]
    [$record/rtd?                                $records]
    [$make-port/input                            $ports]
    [$make-port/output                           $ports]
    [$port-handler                               $ports]
    [$port-buffer                                $ports]
    [$port-index                                 $ports]
    [$port-size                                  $ports]
    [$set-port-index!                            $ports]
    [$set-port-size!                             $ports]
    [$closure-code                               $codes]
    [$code->closure                              $codes]
    [$code-reloc-vector                          $codes]
    [$code-freevars                              $codes]
    [$code-size                                  $codes]
    [$code-annotation                            $codes]
    [$code-ref                                   $codes]
    [$code-set!                                  $codes]
    [$set-code-annotation!                       $codes]
    [procedure-annotation                        i]
    [$make-tcbucket                              $tcbuckets]
    [$tcbucket-key                               $tcbuckets]
    [$tcbucket-val                               $tcbuckets]
    [$tcbucket-next                              $tcbuckets]
    [$set-tcbucket-val!                          $tcbuckets]
    [$set-tcbucket-next!                         $tcbuckets]
    [$set-tcbucket-tconc!                        $tcbuckets]
    [$flush-output-port                          $io]
    [$reset-input-port!                          $io]
    [$close-input-port                           $io]
    [$close-output-port                          $io]
    [$write-char                                 $io]
    [$write-byte                                 $io]
    [$read-char                                  $io]
    [$peek-char                                  $io]
    [$unread-char                                $io]
    [$arg-list                                   $arg-list]
    [$collect-key                                $arg-list]
    [$$apply                                     $stack]
    [$fp-at-base                                 $stack]
    [$primitive-call/cc                          $stack]
    [$frame->continuation                        $stack]
    [$current-frame                              $stack]
    [$seal-frame-and-call                        $stack]
    [$make-call-with-values-procedure            $stack]
    [$make-values-procedure                      $stack]
    [$interrupted?                               $interrupts]
    [$unset-interrupted!                         $interrupts]
    [$apply-nonprocedure-error-handler           ]
    [$incorrect-args-error-handler               ]
    [$multiple-values-error                      ]
    [$debug                                      ]
    [$underflow-misaligned-error                 ]
    [top-level-value-error                       ]
    [car-error                                   ]
    [cdr-error                                   ]
    [fxadd1-error                                ]
    [fxsub1-error                                ]
    [cadr-error                                  ]
    [fx+-type-error                              ]
    [fx+-types-error                             ]
    [fx+-overflow-error                          ]
    [$do-event                                   ]
    [do-overflow                                 ]
    [do-overflow-words                           ]
    [do-vararg-overflow                          ]
    [collect                                     i]
    [collect-key                                 i]
    [do-stack-overflow                           ]
    [make-promise                                ]
    [make-traced-procedure                       i]
    [error@fx+                                   ]
    [fasl-write                                  i]
    [lambda                                      i r ba se ne]
    [and                                         i r ba se ne]
    [begin                                       i r ba se ne]
    [case                                        i r ba se ne]
    [cond                                        i r ba se ne]
    [define                                      i r ba se ne]
    [define-syntax                               i r ba se ne]
    [identifier-syntax                           i r ba]
    [if                                          i r ba se ne]
    [let                                         i r ba se ne]
    [let*                                        i r ba se ne]
    [let*-values                                 r ba]
    [let-syntax                                  i r ba se ne]
    [let-values                                  i r ba]
    [letrec                                      i r ba se ne]
    [letrec*                                     i r ba]
    [letrec-syntax                               i r ba se ne]
    [or                                          i r ba se ne]
    [quasiquote                                  i r ba se ne]
    [quote                                       i r ba se ne]
    [set!                                        i r ba se ne]
    [syntax-rules                                i r ba se ne]
    [unquote                                     i r ba se ne]
    [unquote-splicing                            i r ba se ne]
    [<                                           i r ba se]
    [<=                                          i r ba se]
    [=                                           i r ba se]
    [>                                           i r ba se]
    [>=                                          i r ba se]
    [+                                           i r ba se]
    [-                                           i r ba se]
    [*                                           i r ba se]
    [/                                           i r ba se]
    [abs                                         i r ba se]
    [acos                                        i r ba se]
    [angle                                       r ba se]
    [append                                      i r ba se]
    [apply                                       i r ba se]
    [asin                                        i r ba se]
    [assert                                      i r ba]
    [assertion-violation                         r ba]
    [atan                                        i r ba se]
    [boolean=?                                   i r ba]
    [boolean?                                    i r ba se]
    [car                                         i r ba se]
    [cdr                                         i r ba se]
    [caar                                        i r ba se]
    [cadr                                        i r ba se]
    [cdar                                        i r ba se]
    [cddr                                        i r ba se]
    [caaar                                       i r ba se]
    [caadr                                       i r ba se]
    [cadar                                       i r ba se]
    [caddr                                       i r ba se]
    [cdaar                                       i r ba se]
    [cdadr                                       i r ba se]
    [cddar                                       i r ba se]
    [cdddr                                       i r ba se]
    [caaaar                                      i r ba se]
    [caaadr                                      i r ba se]
    [caadar                                      i r ba se]
    [caaddr                                      i r ba se]
    [cadaar                                      i r ba se]
    [cadadr                                      i r ba se]
    [caddar                                      i r ba se]
    [cadddr                                      i r ba se]
    [cdaaar                                      i r ba se]
    [cdaadr                                      i r ba se]
    [cdadar                                      i r ba se]
    [cdaddr                                      i r ba se]
    [cddaar                                      i r ba se]
    [cddadr                                      i r ba se]
    [cdddar                                      i r ba se]
    [cddddr                                      i r ba se]
    [call-with-current-continuation              i r ba se]
    [call/cc                                     i r ba]
    [call-with-values                            i r ba se]
    [ceiling                                     i r ba se]
    [char->integer                               i r ba se]
    [char<=?                                     i r ba se]
    [char<?                                      i r ba se]
    [char=?                                      i r ba se]
    [char>=?                                     i r ba se]
    [char>?                                      i r ba se]
    [char?                                       i r ba se]
    [complex?                                    r ba se]
    [cons                                        i r ba se]
    [cos                                         i r ba se]
    [denominator                                 i r ba se]
    [div                                         i r ba]
    [mod                                         i r ba]
    [div-and-mod                                 i r ba]
    [div0                                        i r ba]
    [mod0                                        i r ba]
    [div0-and-mod0                               i r ba]
    [dynamic-wind                                i r ba se]
    [eq?                                         i r ba se]
    [equal?                                      i r ba se]
    [eqv?                                        i r ba se]
    [error                                       i r ba]
    [even?                                       i r ba se]
    [exact                                       i r ba]
    [exact-integer-sqrt                          i r ba]
    [exact?                                      i r ba se]
    [exp                                         i r ba se]
    [expt                                        i r ba se]
    [finite?                                     i r ba]
    [floor                                       i r ba se]
    [for-each                                    i r ba se]
    [gcd                                         i r ba se]
    [imag-part                                   r ba se]
    [inexact                                     i r ba]
    [inexact?                                    i r ba se]
    [infinite?                                   i r ba]
    [integer->char                               i r ba se]
    [integer-valued?                             i r ba]
    [integer?                                    i r ba se]
    [lcm                                         i r ba se]
    [length                                      i r ba se]
    [list                                        i r ba se]
    [list->string                                i r ba se]
    [list->vector                                i r ba se]
    [list-ref                                    i r ba se]
    [list-tail                                   i r ba se]
    [list?                                       i r ba se]
    [log                                         i r ba se]
    [magnitude                                   r ba se]
    [make-polar                                  r ba se]
    [make-rectangular                            r ba se]
    [make-string                                 i r ba se]
    [make-vector                                 i r ba se]
    [map                                         i r ba se]
    [max                                         i r ba se]
    [min                                         i r ba se]
    [nan?                                        i r ba]
    [negative?                                   i r ba se]
    [not                                         i r ba se]
    [null?                                       i r ba]
    [number->string                              i r ba se]
    [number?                                     i r ba se]
    [numerator                                   i r ba se]
    [odd?                                        i r ba se]
    [pair?                                       i r ba se]
    [positive?                                   i r ba se]
    [procedure?                                  i r ba se]
    [rational-valued?                            i r ba]
    [rational?                                   i r ba se]
    [rationalize                                 i r ba se]
    [real-part                                   r ba se]
    [real-valued?                                i r ba]
    [real?                                       i r ba se]
    [reverse                                     i r ba se]
    [round                                       i r ba se]
    [sin                                         i r ba se]
    [sqrt                                        i r ba se]
    [string                                      i r ba se]
    [string->list                                i r ba se]
    [string->number                              i r ba se]
    [string->symbol                              i symbols r ba se]
    [string-append                               i r ba se]
    [string-copy                                 i r ba se]
    [string-for-each                             i r ba]
    [string-length                               i r ba se]
    [string-ref                                  i r ba se]
    [string<=?                                   i r ba se]
    [string<?                                    i r ba se]
    [string=?                                    i r ba se]
    [string>=?                                   i r ba se]
    [string>?                                    i r ba se]
    [string?                                     i r ba se]
    [substring                                   i r ba se]
    [symbol->string                              i symbols r ba se]
    [symbol=?                                    i symbols r ba]
    [symbol?                                     i symbols r ba se]
    [tan                                         i r ba se]
    [truncate                                    i r ba se]
    [values                                      i r ba se]
    [vector                                      i r ba se]
    [vector->list                                i r ba se]
    [vector-fill!                                i r ba se]
    [vector-for-each                             i r ba]
    [vector-length                               i r ba se]
    [vector-map                                  i r ba]
    [vector-ref                                  i r ba se]
    [vector-set!                                 i r ba se]
    [vector?                                     i r ba se]
    [zero?                                       i r ba se]
    [...                                         i ne r ba sc]
    [=>                                          i ne r ba ex]
    [_                                           i ne r ba sc]
    [else                                        i ne r ba ex]
    [bitwise-arithmetic-shift                    i r bw]
    [bitwise-arithmetic-shift-left               i r bw]
    [bitwise-arithmetic-shift-right              i r bw]
    [bitwise-not                                 r bw]
    [bitwise-and                                 r bw]
    [bitwise-ior                                 r bw]
    [bitwise-xor                                 r bw]
    [bitwise-bit-count                           r bw]
    [bitwise-bit-field                           r bw]
    [bitwise-bit-set?                            r bw]
    [bitwise-copy-bit                            r bw]
    [bitwise-copy-bit-field                      r bw]
    [bitwise-first-bit-set                       r bw]
    [bitwise-if                                  r bw]
    [bitwise-length                              r bw]
    [bitwise-reverse-bit-field                   r bw]
    [bitwise-rotate-bit-field                    r bw]
    [fixnum?                                     i r fx]
    [fixnum-width                                i r fx]
    [least-fixnum                                i r fx]
    [greatest-fixnum                             i r fx]
    [fx*                                         i r fx]
    [fx*/carry                                   i r fx]
    [fx+                                         i r fx]
    [fx+/carry                                   i r fx]
    [fx-                                         i r fx]
    [fx-/carry                                   i r fx]
    [fx<=?                                       i r fx]
    [fx<?                                        i r fx]
    [fx=?                                        i r fx]
    [fx>=?                                       i r fx]
    [fx>?                                        i r fx]
    [fxand                                       i r fx]
    [fxarithmetic-shift                          i r fx]
    [fxarithmetic-shift-left                     i r fx]
    [fxarithmetic-shift-right                    i r fx]
    [fxbit-count                                 r fx]
    [fxbit-field                                 r fx]
    [fxbit-set?                                  r fx]
    [fxcopy-bit                                  r fx]
    [fxcopy-bit-field                            r fx]
    [fxdiv                                       r fx]
    [fxdiv-and-mod                               r fx]
    [fxdiv0                                      r fx]
    [fxdiv0-and-mod0                             r fx]
    [fxeven?                                     i r fx]
    [fxfirst-bit-set                             r fx]
    [fxif                                        i r fx]
    [fxior                                       i r fx]
    [fxlength                                    r fx]
    [fxmax                                       i r fx]
    [fxmin                                       i r fx]
    [fxmod                                       r fx]
    [fxmod0                                      r fx]
    [fxnegative?                                 i r fx]
    [fxnot                                       i r fx]
    [fxodd?                                      i r fx]
    [fxpositive?                                 i r fx]
    [fxreverse-bit-field                         r fx]
    [fxrotate-bit-field                          r fx]
    [fxxor                                       i r fx]
    [fxzero?                                     i r fx]
    [fixnum->flonum                              i r fl]
    [fl*                                         i r fl]
    [fl+                                         i r fl]
    [fl-                                         i r fl]
    [fl/                                         i r fl]
    [fl<=?                                       i r fl]
    [fl<?                                        i r fl]
    [fl=?                                        i r fl]
    [fl>=?                                       i r fl]
    [fl>?                                        i r fl]
    [flabs                                       i r fl]
    [flacos                                      i r fl]
    [flasin                                      i r fl]
    [flatan                                      i r fl]
    [flceiling                                   i r fl]
    [flcos                                       i r fl]
    [fldenominator                               i r fl]
    [fldiv                                       r fl]
    [fldiv-and-mod                               r fl]
    [fldiv0                                      r fl]
    [fldiv0-and-mod0                             r fl]
    [fleven?                                     i r fl]
    [flexp                                       i r fl]
    [flexpt                                      i r fl]
    [flfinite?                                   i r fl]
    [flfloor                                     i r fl]
    [flinfinite?                                 i r fl]
    [flinteger?                                  i r fl]
    [fllog                                       i r fl]
    [flmax                                       i r fl]
    [flmin                                       i r fl]
    [flmod                                       r fl]
    [flmod0                                      r fl]
    [flnan?                                      i r fl]
    [flnegative?                                 i r fl]
    [flnumerator                                 i r fl]
    [flodd?                                      i r fl]
    [flonum?                                     i r fl]
    [flpositive?                                 i r fl]
    [flround                                     i r fl]
    [flsin                                       i r fl]
    [flsqrt                                      i r fl]
    [fltan                                       i r fl]
    [fltruncate                                  i r fl]
    [flzero?                                     i r fl]
    [real->flonum                                r fl]
    [make-no-infinities-violation                r fl]
    [make-no-nans-violation                      r fl]
    [&no-infinities                              r fl]
    [no-infinities-violation?                    r fl]
    [&no-nans                                    r fl]
    [no-nans-violation?                          r fl]
    [bytevector->sint-list                       i r bv]
    [bytevector->u8-list                         i r bv]
    [bytevector->uint-list                       i r bv]
    [bytevector-copy                             i r bv]
    [bytevector-copy!                            i r bv]
    [bytevector-fill!                            i r bv]
    [bytevector-ieee-double-native-ref           r bv]
    [bytevector-ieee-double-native-set!          r bv]
    [bytevector-ieee-double-ref                  r bv]
    [bytevector-ieee-single-native-ref           r bv]
    [bytevector-ieee-single-native-set!          r bv]
    [bytevector-ieee-single-ref                  r bv]
    [bytevector-length                           i r bv]
    [bytevector-s16-native-ref                   i r bv]
    [bytevector-s16-native-set!                  i r bv]
    [bytevector-s16-ref                          i r bv]
    [bytevector-s16-set!                         i r bv]
    [bytevector-s32-native-ref                   i r bv]
    [bytevector-s32-native-set!                  i r bv]
    [bytevector-s32-ref                          i r bv]
    [bytevector-s32-set!                         i r bv]
    [bytevector-s64-native-ref                   r bv]
    [bytevector-s64-native-set!                  r bv]
    [bytevector-s64-ref                          r bv]
    [bytevector-s64-set!                         r bv]
    [bytevector-s8-ref                           i r bv]
    [bytevector-s8-set!                          i r bv]
    [bytevector-sint-ref                         i r bv]
    [bytevector-sint-set!                        i r bv]
    [bytevector-u16-native-ref                   i r bv]
    [bytevector-u16-native-set!                  i r bv]
    [bytevector-u16-ref                          i r bv]
    [bytevector-u16-set!                         i r bv]
    [bytevector-u32-native-ref                   i r bv]
    [bytevector-u32-native-set!                  i r bv]
    [bytevector-u32-ref                          i r bv]
    [bytevector-u32-set!                         i r bv]
    [bytevector-u64-native-ref                   r bv]
    [bytevector-u64-native-set!                  r bv]
    [bytevector-u64-ref                          r bv]
    [bytevector-u64-set!                         r bv]
    [bytevector-u8-ref                           i r bv]
    [bytevector-u8-set!                          i r bv]
    [bytevector-uint-ref                         i r bv]
    [bytevector-uint-set!                        i r bv]
    [bytevector=?                                i r bv]
    [bytevector?                                 i r bv]
    [endianness                                  i r bv]
    [native-endianness                           i r bv]
    [sint-list->bytevector                       i r bv]
    [string->utf16                               r bv]
    [string->utf32                               r bv]
    [string->utf8                                r bv]
    [u8-list->bytevector                         i r bv]
    [uint-list->bytevector                       i r bv]
    [utf8->string                                r bv]
    [utf16->string                               r bv]
    [utf32->string                               r bv]
    [condition?                                  r co]
    [&assertion                                  r co]
    [assertion-violation?                        r co]
    [&condition                                  r co]
    [condition                                   r co]
    [condition-accessor                          r co]
    [condition-irritants                         r co]
    [condition-message                           r co]
    [condition-predicate                         r co]
    [condition-who                               r co]
    [define-condition-type                       r co]
    [&error                                      r co]
    [error?                                      r co]
    [&implementation-restriction                 r co]
    [implementation-restriction-violation?       r co]
    [&irritants                                  r co]
    [irritants-condition?                        r co]
    [&lexical                                    r co]
    [lexical-violation?                          r co]
    [make-assertion-violation                    r co]
    [make-error                                  r co]
    [make-implementation-restriction-violation   r co]
    [make-irritants-condition                    r co]
    [make-lexical-violation                      r co]
    [make-message-condition                      r co]
    [make-non-continuable-violation              r co]
    [make-serious-condition                      r co]
    [make-syntax-violation                       r co]
    [make-undefined-violation                    r co]
    [make-violation                              r co]
    [make-warning                                r co]
    [make-who-condition                          r co]
    [&message                                    r co]
    [message-condition?                          r co]
    [&non-continuable                            r co]
    [non-continuable-violation?                  r co]
    [&serious                                    r co]
    [serious-condition?                          r co]
    [simple-conditions                           r co]
    [&syntax                                     r co]
    [syntax-violation                            r co sc]
    [syntax-violation-form                       r co]
    [syntax-violation-subform                    r co]
    [syntax-violation?                           r co]
    [&undefined                                  r co]
    [undefined-violation?                        r co]
    [&violation                                  r co]
    [violation?                                  r co]
    [&warning                                    r co]
    [warning?                                    r co]
    [&who                                        r co]
    [who-condition?                              r co]
    [case-lambda                                 i r ct]
    [do                                          i r ct se ne]
    [unless                                      i r ct]
    [when                                        i r ct]
    [define-enumeration                          r en]
    [enum-set->list                              r en]
    [enum-set-complement                         r en]
    [enum-set-constructor                        r en]
    [enum-set-difference                         r en]
    [enum-set-indexer                            r en]
    [enum-set-intersection                       r en]
    [enum-set-member?                            r en]
    [enum-set-projection                         r en]
    [enum-set-subset?                            r en]
    [enum-set-union                              r en]
    [enum-set-universe                           r en]
    [enum-set=?                                  r en]
    [make-enumeration                            r en]
    [environment                                 i ev]
    [eval                                        i ev se]
    [raise                                       r ex]
    [raise-continuable                           r ex]
    [with-exception-handler                      r ex]
    [guard                                       r ex]
    [binary-port?                                r ip]
    [buffer-mode                                 r ip]
    [buffer-mode?                                r ip]
    [bytevector->string                          r ip]
    [call-with-bytevector-output-port            r ip]
    [call-with-port                              r ip]
    [call-with-string-output-port                r ip]
    [assoc                                       i r ls se]
    [assp                                        i r ls]
    [assq                                        i r ls se]
    [assv                                        i r ls se]
    [cons*                                       i r ls]
    [filter                                      i r ls]
    [find                                        i r ls]
    [fold-left                                   i r ls]
    [fold-right                                  i r ls]
    [for-all                                     i r ls]
    [exists                                      i r ls]
    [member                                      i r ls se]
    [memp                                        i r ls]
    [memq                                        i r ls se]
    [memv                                        i r ls se]
    [partition                                   i r ls]
    [remq                                        i r ls]
    [remp                                        i r ls]
    [remv                                        i r ls]
    [remove                                      i r ls]
    [set-car!                                    i mp se]
    [set-cdr!                                    i mp se]
    [string-set!                                 i r ms se]
    [string-fill!                                i r ms se]
    [command-line                                i r pr]
    [exit                                        i r pr]
    [delay                                       i r5 se ne]
    [exact->inexact                              i r r5 se]
    [force                                       i r5 se]
    [inexact->exact                              i r r5 se]
    [modulo                                      i r r5 se]
    [remainder                                   i r r5 se]
    [null-environment                            i r5 se]
    [quotient                                    i r r5 se]
    [scheme-report-environment                   r5 se]
    [close-port                                  r ip]
    [eol-style                                   i r ip]
    [error-handling-mode                         r ip]
    [file-options                                r ip]
    [flush-output-port                           i r ip]
    [get-bytevector-all                          r ip]
    [get-bytevector-n                            r ip]
    [get-bytevector-n!                           r ip]
    [get-bytevector-some                         r ip]
    [get-char                                    r ip]
    [get-datum                                   r ip]
    [get-line                                    r ip]
    [get-string-all                              r ip]
    [get-string-n                                r ip]
    [get-string-n!                               r ip]
    [get-u8                                      r ip]
    [&i/o                                        r ip is fi]
    [&i/o-decoding                               r ip]
    [i/o-decoding-error?                         r ip]
    [&i/o-encoding                               r ip]
    [i/o-encoding-error-char                     r ip]
    [i/o-encoding-error?                         r ip]
    [i/o-error-filename                          r ip is fi]
    [i/o-error-port                              r ip is fi]
    [i/o-error?                                  r ip is fi]
    [&i/o-file-already-exists                    r ip is fi]
    [i/o-file-already-exists-error?              r ip is fi]
    [&i/o-file-does-not-exist                    r ip is fi]
    [i/o-file-does-not-exist-error?              r ip is fi]
    [&i/o-file-is-read-only                      r ip is fi]
    [i/o-file-is-read-only-error?                r ip is fi]
    [&i/o-file-protection                        r ip is fi]
    [i/o-file-protection-error?                  r ip is fi]
    [&i/o-filename                               r ip is fi]
    [i/o-filename-error?                         r ip is fi]
    [&i/o-invalid-position                       r ip is fi]
    [i/o-invalid-position-error?                 r ip is fi]
    [&i/o-port                                   r ip is fi]
    [i/o-port-error?                             r ip is fi]
    [&i/o-read                                   r ip is fi]
    [i/o-read-error?                             r ip is fi]
    [&i/o-write                                  r ip is fi]
    [i/o-write-error?                            r ip is fi]
    [lookahead-char                              r ip]
    [lookahead-u8                                r ip]
    [make-bytevector                             i r bv]
    [make-custom-binary-input-port               r ip]
    [make-custom-binary-input/output-port        r ip]
    [make-custom-binary-output-port              r ip]
    [make-custom-textual-input-port              r ip]
    [make-custom-textual-input/output-port       r ip]
    [make-custom-textual-output-port             r ip]
    [make-i/o-decoding-error                     r ip]
    [make-i/o-encoding-error                     r ip]
    [make-i/o-error                              r ip is fi]
    [make-i/o-file-already-exists-error          r ip is fi]
    [make-i/o-file-does-not-exist-error          r ip is fi]
    [make-i/o-file-is-read-only-error            r ip is fi]
    [make-i/o-file-protection-error              r ip is fi]
    [make-i/o-filename-error                     r ip is fi]
    [make-i/o-invalid-position-error             r ip is fi]
    [make-i/o-port-error                         r ip is fi]
    [make-i/o-read-error                         r ip is fi]
    [make-i/o-write-error                        r ip is fi]
    [latin-1-codec                               i r ip]
    [make-transcoder                             r ip]
    [native-eol-style                            i r ip]
    [native-transcoder                           r ip]
    [open-bytevector-input-port                  r ip]
    [open-bytevector-output-port                 r ip]
    [open-file-input-port                        r ip]
    [open-file-input/output-port                 r ip]
    [open-file-output-port                       r ip]
    [open-string-input-port                      r ip]
    [open-string-output-port                     r ip]
    [output-port-buffer-mode                     r ip]
    [port-eof?                                   r ip]
    [port-has-port-position?                     r ip]
    [port-has-set-port-position!?                r ip]
    [port-position                               r ip]
    [port-transcoder                             r ip]
    [port?                                       i r ip]
    [put-bytevector                              r ip]
    [put-char                                    r ip]
    [put-datum                                   r ip]
    [put-string                                  r ip]
    [put-u8                                      r ip]
    [set-port-position!                          r ip]
    [standard-error-port                         i r ip]
    [standard-input-port                         i r ip]
    [standard-output-port                        i r ip]
    [string->bytevector                          r ip]
    [textual-port?                               r ip]
    [transcoded-port                             r ip]
    [transcoder-codec                            r ip]
    [transcoder-eol-style                        r ip]
    [transcoder-error-handling-mode              r ip]
    [utf-16-codec                                i r ip]
    [utf-8-codec                                 i r ip]
    [input-port?                                 i r is ip se]
    [output-port?                                i r is ip se]
    [current-input-port                          i r ip is se]
    [current-output-port                         i r ip is se]
    [current-error-port                          r ip is]
    [eof-object                                  i r ip is se]
    [eof-object?                                 i r ip is]
    [close-input-port                            i r is se]
    [close-output-port                           i r is se]
    [display                                     i r is se]
    [newline                                     i r is se]
    [open-input-file                             i r is se]
    [open-output-file                            i r is se]
    [peek-char                                   i r is se]
    [read                                        i r is se]
    [read-char                                   i r is se]
    [with-input-from-file                        i r is se]
    [with-output-to-file                         i r is se]
    [write                                       i r is se]
    [write-char                                  i r is se]
    [call-with-input-file                        i r is se]
    [call-with-output-file                       i r is se]
    [hashtable-clear!                            r ht]
    [hashtable-contains?                         r ht]
    [hashtable-copy                              r ht]
    [hashtable-delete!                           r ht]
    [hashtable-entries                           r ht]
    [hashtable-keys                              r ht]
    [hashtable-mutable?                          r ht]
    [hashtable-ref                               r ht]
    [hashtable-set!                              r ht]
    [hashtable-size                              r ht]
    [hashtable-update!                           r ht]
    [hashtable?                                  r ht]
    [make-eq-hashtable                           r ht]
    [make-eqv-hashtable                          r ht]
    [hashtable-hash-function                     r ht]
    [make-hashtable                              r ht]
    [hashtable-equivalence-function              r ht]
    [equal-hash                                  r ht]
    [string-hash                                 r ht]
    [string-ci-hash                              r ht]
    [symbol-hash                                 r ht]
    [list-sort                                   i r sr]
    [vector-sort                                 i r sr]
    [vector-sort!                                i r sr]
    [file-exists?                                i r fi]
    [delete-file                                 i r fi]
    [define-record-type                          r rs]
    [fields                                      r rs]
    [immutable                                   r rs]
    [mutable                                     r rs]
    [opaque                                      r rs]
    [parent                                      r rs]
    [parent-rtd                                  r rs]
    [protocol                                    r rs]
    [record-constructor-descriptor               r rs]
    [record-type-descriptor                      i r rs]
    [sealed                                      r rs]
    [nongenerative                               r rs]
    [record-field-mutable?                       r ri]
    [record-rtd                                  r ri]
    [record-type-field-names                     i r ri]
    [record-type-generative?                     r ri]
    [record-type-name                            i r ri]
    [record-type-opaque?                         r ri]
    [record-type-parent                          r ri]
    [record-type-sealed?                         r ri]
    [record-type-uid                             r ri]
    [record?                                     i r ri]
    [make-record-constructor-descriptor          r rp]
    [make-record-type-descriptor                 r rp]
    [record-accessor                             r rp]
    [record-constructor                          i r rp]
    [record-mutator                              r rp]
    [record-predicate                            i r rp]
    [record-type-descriptor?                     r rp]
    [bound-identifier=?                          i r sc]
    [datum->syntax                               i r sc]
    [syntax                                      i r sc]
    [syntax->datum                               i r sc]
    [syntax-case                                 i r sc]
    [unsyntax                                    i r sc]
    [unsyntax-splicing                           i r sc]
    [quasisyntax                                 i r sc]
    [with-syntax                                 i r sc]
    [free-identifier=?                           i r sc]
    [generate-temporaries                        i r sc]
    [identifier?                                 i r sc]
    [make-variable-transformer                   i r sc]
    [char-alphabetic?                            i r uc se]
    [char-ci<=?                                  i r uc se]
    [char-ci<?                                   i r uc se]
    [char-ci=?                                   i r uc se]
    [char-ci>=?                                  i r uc se]
    [char-ci>?                                   i r uc se]
    [char-downcase                               i r uc se]
    [char-foldcase                               i r uc]
    [char-titlecase                              i r uc]
    [char-upcase                                 i r uc se]
    [char-general-category                       i r uc]
    [char-lower-case?                            i r uc se]
    [char-numeric?                               i r uc se]
    [char-title-case?                            i r uc]
    [char-upper-case?                            i r uc se]
    [char-whitespace?                            i r uc se]
    [string-ci<=?                                i r uc se]
    [string-ci<?                                 i r uc se]
    [string-ci=?                                 i r uc se]
    [string-ci>=?                                i r uc se]
    [string-ci>?                                 i r uc se]
    [string-downcase                             r uc]
    [string-foldcase                             i r uc]
    [string-normalize-nfc                        r uc]
    [string-normalize-nfd                        r uc]
    [string-normalize-nfkc                       r uc]
    [string-normalize-nfkd                       r uc]
    [string-titlecase                            r uc]
    [string-upcase                               r uc]
    [char-ready?                                 ]
    [interaction-environment                     ]
    [load                                        i]
    [void                                        i $boot]
    [gensym                                      i symbols $boot]
    [symbol-value                                i symbols $boot]
    [set-symbol-value!                           i symbols $boot]
    [eval-core                                   $boot]
    [pretty-print                                i $boot]
    [module                                      i cm]
    [syntax-dispatch                             ]
    [syntax-error                                i sc]
  ))

(define (macro-identifier? x) 
  (and (assq x ikarus-system-macros) #t))

(define (procedure-identifier? x)
  (not (macro-identifier? x)))

(define bootstrap-collection
  (let ([ls 
         (let f ([ls library-legend])
           (define required? cadddr)
           (define library-name cadr)
           (cond
             [(null? ls) '()]
             [(required? (car ls)) 
              (cons (find-library-by-name (library-name (car ls)))
                    (f (cdr ls)))]
             [else (f (cdr ls))]))])
    (case-lambda
      [() ls]
      [(x) (unless (memq x ls) 
             (set! ls (cons x ls)))])))

(define (verify-map)
  (define (f x)
    (for-each 
      (lambda (x) 
        (unless (assq x library-legend)
          (error 'verify "~s is not in the libraries list" x)))
      (cdr x)))
  (for-each f identifier->library-map))

(define (make-collection)
  (let ([set '()])
    (case-lambda
      [() set]
      [(x) (set! set (cons x set))])))

(define (make-system-data subst env)
  (define who 'make-system-data)
  (let ([export-subst    (make-collection)] 
        [export-env      (make-collection)]
        [export-primlocs (make-collection)])
    (for-each
      (lambda (x)
        (let ([name (car x)] [binding (cadr x)])
          (let ([label (gensym)])
            (export-subst (cons name label))
            (export-env   (cons label binding)))))
      ikarus-system-macros)
    (for-each
      (lambda (x)
        (when (procedure-identifier? x)
          (cond
            [(assq x (export-subst))
             (error who "ambiguous export of ~s" x)]
            [(assq x subst) =>
             ;;; primitive defined (exported) within the compiled libraries
             (lambda (p)
               (let ([label (cdr p)])
                 (cond
                   [(assq label env) =>
                    (lambda (p)
                      (let ([binding (cdr p)])
                        (case (car binding)
                          [(global) 
                           (export-subst (cons x label))
                           (export-env   (cons label (cons 'core-prim x)))
                           (export-primlocs (cons x (cdr binding)))]
                          [else 
                           (error #f "invalid binding ~s for ~s" p x)])))]
                   [else (error #f "cannot find binding for ~s ~s" x
                                label)])))]
            [else 
             ;;; core primitive with no backing definition, assumed to
             ;;; be defined in other strata of the system
             (let ([label (gensym)])
               (export-subst (cons x label))
               (export-env (cons label (cons 'core-prim x))))])))
      (map car identifier->library-map))
    (values (export-subst) (export-env) (export-primlocs))))

(define (get-export-subset key subst)
  (let f ([ls subst])
    (cond
      [(null? ls) '()]
      [else
       (let ([x (car ls)])
         (let ([name (car x)])
           (cond
             [(assq name identifier->library-map)
              =>
              (lambda (q)
                (cond
                  [(memq key (cdr q)) 
                   (cons x (f (cdr ls)))]
                  [else (f (cdr ls))]))]
             [else 
              ;;; not going to any library?
              (f (cdr ls))])))])))

(define (build-system-library export-subst export-env primlocs)
  (define (build-library legend-entry)
    (let ([key (car legend-entry)]
          [name (cadr legend-entry)]
          [visible? (caddr legend-entry)]) 
      (let ([id     (gensym)]
            [name       name]
            [version     '()]
            [import-libs '()]
            [visit-libs  '()]
            [invoke-libs '()])
        (let-values ([(subst env)
                      (if (equal? name '(psyntax system $all)) 
                          (values export-subst export-env)
                          (values
                            (get-export-subset key export-subst)
                            '()))])
          `(install-library 
             ',id ',name ',version ',import-libs ',visit-libs ',invoke-libs
             ',subst ',env void void ',visible?)))))
  (let ([code `(library (ikarus primlocs)
                  (export) ;;; must be empty
                  (import 
                    (only (psyntax library-manager)
                          install-library)
                    (only (ikarus compiler)
                          current-primitive-locations)
                    (ikarus))
                  (current-primitive-locations 
                    (lambda (x) 
                      (cond
                        [(assq x ',primlocs) => cdr]
                        [else #f])))
                  ,@(map build-library library-legend))])
    (let-values ([(code empty-subst empty-env)
                  (boot-library-expand code)])
       code)))

;;; the first code to run on the system is one that initializes
;;; the value and proc fields of the location of $init-symbol-value!
;;; Otherwise, all subsequent inits to any global variable will
;;; segfault.  

(define (make-init-code)
  (define proc (gensym))
  (define loc (gensym))
  (define label (gensym))
  (define sym (gensym))
  (define val (gensym))
  (define args (gensym))
  (values 
    (list
      `((case-lambda 
          [(,proc) (,proc ',loc ,proc)])
        (case-lambda
          [(,sym ,val)
           (begin
             ((primitive $set-symbol-value!) ,sym ,val)
             (if ((primitive procedure?) ,val) 
                 ((primitive $set-symbol-proc!) ,sym ,val)
                 ((primitive $set-symbol-proc!) ,sym
                    (case-lambda 
                      [,args
                       ((primitive error)
                         'apply 
                         '"~s is not a procedure"
                         ((primitive $symbol-value) ,sym))]))))])))
    `([$init-symbol-value! . ,label])
    `([,label . (global . ,loc)])))

(define (expand-all files)
  ;;; remove all re-exported identifiers (those with labels in
  ;;; subst but not binding in env).
  (define (prune-subst subst env)
    (cond 
      ((null? subst) '()) 
      ((not (assq (cdar subst) env)) (prune-subst (cdr subst) env)) 
      (else (cons (car subst) (prune-subst (cdr subst) env)))))
  (let-values (((code* subst env) (make-init-code)))
    (for-each
      (lambda (file)
        (printf "expanding ~s\n" file)
        (load file
          (lambda (x) 
            (let-values ([(code export-subst export-env)
                          (boot-library-expand x)])
               (set! code* (cons code code*))
               (set! subst (append export-subst subst))
               (set! env (append export-env env))))))
      files)
    (let-values ([(export-subst export-env export-locs)
                  (make-system-data (prune-subst subst env) env)])
      (let ([code (build-system-library export-subst export-env export-locs)])
        (values 
          (reverse (cons* (car code*) code (cdr code*)))
          export-locs)))))

(verify-map)

(time-it "the entire bootstrap process"
  (lambda ()
    (let-values ([(core* locs) 
                  (time-it "macro expansion"
                    (lambda () 
                      (parameterize ([current-library-collection
                                       bootstrap-collection])
                        (expand-all scheme-library-files))))])
        (current-primitive-locations
          (lambda (x)
            (cond
              [(assq x locs) => cdr]
              [else 
               (error 'bootstrap "no location for ~s" x)])))
        (let ([p (open-output-file "ikarus.boot" 'replace)])
          (for-each 
            (lambda (x) 
              (compile-core-expr-to-port x p))
            core*)
          (close-output-port p)))))

(printf "Happy Happy Joy Joy\n")


;;; vim:syntax=scheme
