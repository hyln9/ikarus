#!/usr/bin/env scheme-script
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


(import (ikarus))

;;; library names:

(define library-names
  '(
    [se (scheme-report-environment)]
    [r5 (rnrs r5rs (6))]
    [ct (rnrs control (6))]
    [ev (rnrs eval (6))]
    [mp (rnrs mutable-pairs (6))]
    [ms (rnrs mutable-strings (6))]
    [pr (rnrs programs (6))]
    [sc (rnrs syntax-case (6))]
    [fi (rnrs files (6))]
    [ne (null-environment)]
    [sr (rnrs sorting (6))]
    [ex (rnrs exceptions (6))]
    [ls (rnrs lists (6))]
    [ri (rnrs records inspection (6))]
    [rp (rnrs records procedural (6))]
    [rs (rnrs records syntactic (6))]
    [co (rnrs conditions (6))]
    [en (rnrs enums (6))]
    [is (rnrs io simple (6))]
    [fl (rnrs arithmetic flonums (6))]
    [ba (rnrs base (6))]
    [bv (rnrs bytevectors (6))]
    [uc (rnrs unicode (6))]
    [bw (rnrs arithmetic bitwise (6))]
    [fx (rnrs arithmetic fixnums (6))]
    [ht (rnrs hashtables (6))]
    [ip (rnrs io ports (6))]
    ))

(define status-names
  '(
    [S scheduled]
    [C completed]
    ))

(define identifier-names
  '(
    ;;;
    [lambda                                     C ba se ne]
    [and                                        C ba se ne]
    [begin                                      C ba se ne]
    [case                                       C ba se ne]
    [cond                                       C ba se ne]
    [define                                     C ba se ne]
    [define-syntax                              C ba se ne]
    [identifier-syntax                          C ba]
    [if                                         C ba se ne]
    [let                                        C ba se ne]
    [let*                                       C ba se ne]
    [let*-values                                C ba]
    [let-syntax                                 C ba se ne]
    [let-values                                 C ba]
    [letrec                                     C ba se ne]
    [letrec*                                    C ba]
    [letrec-syntax                              C ba se ne]
    [or                                         C ba se ne]
    [quasiquote                                 C ba se ne]
    [quote                                      C ba se ne]
    [set!                                       C ba se ne]
    [syntax-rules                               C ba se ne]
    [unquote                                    C ba se ne]
    [unquote-splicing                           C ba se ne]
    [<                                          C ba se]
    [<=                                         C ba se]
    [=                                          C ba se]
    [>                                          C ba se]
    [>=                                         C ba se]
    [+                                          C ba se]
    [-                                          C ba se]
    [*                                          C ba se]
    [/                                          C ba se]
    [abs                                        C ba se]
    [acos                                       C ba se]
    [angle                                      S ba se]
    [append                                     C ba se]
    [apply                                      C ba se]
    [asin                                       C ba se]
    [assert                                     C ba]
    [assertion-violation                        C ba]
    [atan                                       C ba se]
    [boolean=?                                  C ba]
    [boolean?                                   C ba se]
    [car                                        C ba se]
    [cdr                                        C ba se]
    [caar                                       C ba se]
    [cadr                                       C ba se]
    [cdar                                       C ba se]
    [cddr                                       C ba se]
    [caaar                                      C ba se]
    [caadr                                      C ba se]
    [cadar                                      C ba se]
    [caddr                                      C ba se]
    [cdaar                                      C ba se]
    [cdadr                                      C ba se]
    [cddar                                      C ba se]
    [cdddr                                      C ba se]
    [caaaar                                     C ba se]
    [caaadr                                     C ba se]
    [caadar                                     C ba se]
    [caaddr                                     C ba se]
    [cadaar                                     C ba se]
    [cadadr                                     C ba se]
    [caddar                                     C ba se]
    [cadddr                                     C ba se]
    [cdaaar                                     C ba se]
    [cdaadr                                     C ba se]
    [cdadar                                     C ba se]
    [cdaddr                                     C ba se]
    [cddaar                                     C ba se]
    [cddadr                                     C ba se]
    [cdddar                                     C ba se]
    [cddddr                                     C ba se]
    [call-with-current-continuation             C ba se]
    [call/cc                                    C ba]
    [call-with-values                           C ba se]
    [ceiling                                    C ba se]
    [char->integer                              C ba se]
    [char<=?                                    C ba se]
    [char<?                                     C ba se]
    [char=?                                     C ba se]
    [char>=?                                    C ba se]
    [char>?                                     C ba se]
    [char?                                      C ba se]
    [complex?                                   C ba se]
    [cons                                       C ba se]
    [cos                                        C ba se]
    [denominator                                C ba se]
    [div                                        C ba]
    [mod                                        C ba]
    [div-and-mod                                C ba]
    [div0                                       C ba]
    [mod0                                       C ba]
    [div0-and-mod0                              C ba]
    [dynamic-wind                               C ba se]
    [eq?                                        C ba se]
    [equal?                                     C ba se]
    [eqv?                                       C ba se]
    [error                                      C ba]
    [even?                                      C ba se]
    [exact                                      C ba]
    [exact-integer-sqrt                         C ba]
    [exact?                                     C ba se]
    [exp                                        C ba se]
    [expt                                       C ba se]
    [finite?                                    C ba]
    [floor                                      C ba se]
    [for-each                                   C ba se]
    [gcd                                        C ba se]
    [imag-part                                  C ba se]
    [inexact                                    C ba]
    [inexact?                                   C ba se]
    [infinite?                                  C ba]
    [integer->char                              C ba se]
    [integer-valued?                            C ba]
    [integer?                                   C ba se]
    [lcm                                        C ba se]
    [length                                     C ba se]
    [list                                       C ba se]
    [list->string                               C ba se]
    [list->vector                               C ba se]
    [list-ref                                   C ba se]
    [list-tail                                  C ba se]
    [list?                                      C ba se]
    [log                                        C ba se]
    [magnitude                                  S ba se]
    [make-polar                                 S ba se]
    [make-rectangular                           S ba se]
    [make-string                                C ba se]
    [make-vector                                C ba se]
    [map                                        C ba se]
    [max                                        C ba se]
    [min                                        C ba se]
    [nan?                                       C ba]
    [negative?                                  C ba se]
    [not                                        C ba se]
    [null?                                      C ba]
    [number->string                             C ba se]
    [number?                                    C ba se]
    [numerator                                  C ba se]
    [odd?                                       C ba se]
    [pair?                                      C ba se]
    [positive?                                  C ba se]
    [procedure?                                 C ba se]
    [rational-valued?                           C ba]
    [rational?                                  C ba se]
    [rationalize                                C ba se]
    [real-part                                  C ba se]
    [real-valued?                               C ba]
    [real?                                      C ba se]
    [reverse                                    C ba se]
    [round                                      C ba se]
    [sin                                        C ba se]
    [sqrt                                       C ba se]
    [string                                     C ba se]
    [string->list                               C ba se]
    [string->number                             C ba se]
    [string->symbol                             C ba se]
    [string-append                              C ba se]
    [string-copy                                C ba se]
    [string-for-each                            C ba]
    [string-length                              C ba se]
    [string-ref                                 C ba se]
    [string<=?                                  C ba se]
    [string<?                                   C ba se]
    [string=?                                   C ba se]
    [string>=?                                  C ba se]
    [string>?                                   C ba se]
    [string?                                    C ba se]
    [substring                                  C ba se]
    [symbol->string                             C ba se]
    [symbol=?                                   C ba]
    [symbol?                                    C ba se]
    [tan                                        C ba se]
    [truncate                                   C ba se]
    [values                                     C ba se]
    [vector                                     C ba se]
    [vector->list                               C ba se]
    [vector-fill!                               C ba se]
    [vector-for-each                            C ba]
    [vector-length                              C ba se]
    [vector-map                                 C ba]
    [vector-ref                                 C ba se]
    [vector-set!                                C ba se]
    [vector?                                    C ba se]
    [zero?                                      C ba se]
    [...                                        C ba sc]
    [=>                                         C ba ex]
    [_                                          C ba sc]
    [else                                       C ba ex]
    ;;;
    [bitwise-arithmetic-shift                   C bw]
    [bitwise-arithmetic-shift-left              C bw]
    [bitwise-arithmetic-shift-right             C bw]
    [bitwise-not                                C bw]
    [bitwise-and                                C bw]
    [bitwise-ior                                S bw]
    [bitwise-xor                                S bw]
    [bitwise-bit-count                          C bw]
    [bitwise-bit-field                          S bw]
    [bitwise-bit-set?                           S bw]
    [bitwise-copy-bit                           S bw]
    [bitwise-copy-bit-field                     S bw]
    [bitwise-first-bit-set                      C bw]
    [bitwise-if                                 S bw]
    [bitwise-length                             S bw]
    [bitwise-reverse-bit-field                  S bw]
    [bitwise-rotate-bit-field                   S bw]
    ;;;
    [fixnum?                                    C fx]
    [fixnum-width                               C fx] 
    [least-fixnum                               C fx] 
    [greatest-fixnum                            C fx] 
    [fx*                                        C fx]
    [fx*/carry                                  C fx]
    [fx+                                        C fx]
    [fx+/carry                                  C fx]
    [fx-                                        C fx]
    [fx-/carry                                  C fx]
    [fx<=?                                      C fx]
    [fx<?                                       C fx]
    [fx=?                                       C fx]
    [fx>=?                                      C fx]
    [fx>?                                       C fx]
    [fxand                                      C fx]
    [fxarithmetic-shift                         C fx]
    [fxarithmetic-shift-left                    C fx]
    [fxarithmetic-shift-right                   C fx]
    [fxbit-count                                C fx]
    [fxbit-field                                C fx]
    [fxbit-set?                                 C fx]
    [fxcopy-bit                                 C fx]
    [fxcopy-bit-field                           C fx]
    [fxdiv                                      C fx]
    [fxdiv-and-mod                              C fx]
    [fxdiv0                                     C fx]
    [fxdiv0-and-mod0                            C fx]
    [fxeven?                                    C fx]
    [fxfirst-bit-set                            C fx]
    [fxif                                       C fx]
    [fxior                                      C fx]
    [fxlength                                   C fx]
    [fxmax                                      C fx]
    [fxmin                                      C fx]
    [fxmod                                      C fx]
    [fxmod0                                     C fx]
    [fxnegative?                                C fx]
    [fxnot                                      C fx]
    [fxodd?                                     C fx]
    [fxpositive?                                C fx]
    [fxreverse-bit-field                        S fx]
    [fxrotate-bit-field                         S fx]
    [fxxor                                      C fx]
    [fxzero?                                    C fx]
    ;;;
    [fixnum->flonum                             C fl]
    [fl*                                        C fl]
    [fl+                                        C fl]
    [fl-                                        C fl]
    [fl/                                        C fl]
    [fl<=?                                      C fl]
    [fl<?                                       C fl]
    [fl=?                                       C fl]
    [fl>=?                                      C fl]
    [fl>?                                       C fl]
    [flabs                                      C fl]
    [flacos                                     C fl]
    [flasin                                     C fl]
    [flatan                                     C fl]
    [flceiling                                  C fl]
    [flcos                                      C fl]
    [fldenominator                              C fl]
    [fldiv                                      C fl]
    [fldiv-and-mod                              C fl]
    [fldiv0                                     C fl]
    [fldiv0-and-mod0                            C fl]
    [fleven?                                    C fl]
    [flexp                                      C fl]
    [flexpt                                     C fl]
    [flfinite?                                  C fl]
    [flfloor                                    C fl]
    [flinfinite?                                C fl]
    [flinteger?                                 C fl]
    [fllog                                      C fl]
    [flmax                                      C fl]
    [flmin                                      C fl]
    [flmod                                      C fl]
    [flmod0                                     C fl]
    [flnan?                                     C fl]
    [flnegative?                                C fl]
    [flnumerator                                C fl]
    [flodd?                                     C fl]
    [flonum?                                    C fl]
    [flpositive?                                C fl]
    [flround                                    C fl]
    [flsin                                      C fl]
    [flsqrt                                     C fl]
    [fltan                                      C fl]
    [fltruncate                                 C fl]
    [flzero?                                    C fl]
    [real->flonum                               C fl]
    [make-no-infinities-violation               C fl]
    [make-no-nans-violation                     C fl]
    [&no-infinities                             C fl]
    [no-infinities-violation?                   C fl]
    [&no-nans                                   C fl]
    [no-nans-violation?                         C fl]
    ;;;
    [bytevector->sint-list                      C bv]
    [bytevector->u8-list                        C bv]
    [bytevector->uint-list                      C bv]
    [bytevector-copy                            C bv]
    [bytevector-copy!                           C bv]
    [bytevector-fill!                           C bv]
    [bytevector-ieee-double-native-ref          C bv]
    [bytevector-ieee-double-native-set!         C bv]
    [bytevector-ieee-double-ref                 C bv]
    [bytevector-ieee-double-set!                C bv]
    [bytevector-ieee-single-native-ref          C bv]
    [bytevector-ieee-single-native-set!         C bv]
    [bytevector-ieee-single-ref                 C bv]
    [bytevector-ieee-single-set!                C bv]
    [bytevector-length                          C bv]
    [bytevector-s16-native-ref                  C bv]
    [bytevector-s16-native-set!                 C bv]
    [bytevector-s16-ref                         C bv]
    [bytevector-s16-set!                        C bv]
    [bytevector-s32-native-ref                  C bv]
    [bytevector-s32-native-set!                 C bv]
    [bytevector-s32-ref                         C bv]
    [bytevector-s32-set!                        C bv]
    [bytevector-s64-native-ref                  C bv]
    [bytevector-s64-native-set!                 C bv]
    [bytevector-s64-ref                         C bv]
    [bytevector-s64-set!                        C bv]
    [bytevector-s8-ref                          C bv]
    [bytevector-s8-set!                         C bv]
    [bytevector-sint-ref                        C bv]
    [bytevector-sint-set!                       C bv]
    [bytevector-u16-native-ref                  C bv]
    [bytevector-u16-native-set!                 C bv]
    [bytevector-u16-ref                         C bv]
    [bytevector-u16-set!                        C bv]
    [bytevector-u32-native-ref                  C bv]
    [bytevector-u32-native-set!                 C bv]
    [bytevector-u32-ref                         C bv]
    [bytevector-u32-set!                        C bv]
    [bytevector-u64-native-ref                  C bv]
    [bytevector-u64-native-set!                 C bv]
    [bytevector-u64-ref                         C bv]
    [bytevector-u64-set!                        C bv]
    [bytevector-u8-ref                          C bv]
    [bytevector-u8-set!                         C bv]
    [bytevector-uint-ref                        C bv]
    [bytevector-uint-set!                       C bv]
    [bytevector=?                               C bv]
    [bytevector?                                C bv]
    [endianness                                 C bv]
    [native-endianness                          C bv]
    [sint-list->bytevector                      C bv]
    [string->utf16                              S bv]
    [string->utf32                              S bv]
    [string->utf8                               C bv]
    [u8-list->bytevector                        C bv]
    [uint-list->bytevector                      C bv]
    [utf8->string                               C bv]
    [utf16->string                              S bv]
    [utf32->string                              S bv]
    ;;;
    [condition?                                 C co]
    [&assertion                                 C co]
    [assertion-violation?                       C co]
    [&condition                                 C co]
    [condition                                  C co]
    [condition-accessor                         C co]
    [condition-irritants                        C co]
    [condition-message                          C co]
    [condition-predicate                        C co]
    [condition-who                              C co]
    [define-condition-type                      C co]
    [&error                                     C co]
    [error?                                     C co]
    [&implementation-restriction                C co]
    [implementation-restriction-violation?      C co]
    [&irritants                                 C co]
    [irritants-condition?                       C co]
    [&lexical                                   C co]
    [lexical-violation?                         C co]
    [make-assertion-violation                   C co]
    [make-error                                 C co]
    [make-implementation-restriction-violation  C co]
    [make-irritants-condition                   C co]
    [make-lexical-violation                     C co]
    [make-message-condition                     C co]
    [make-non-continuable-violation             C co]
    [make-serious-condition                     C co]
    [make-syntax-violation                      C co]
    [make-undefined-violation                   C co]
    [make-violation                             C co]
    [make-warning                               C co]
    [make-who-condition                         C co]
    [&message                                   C co]
    [message-condition?                         C co]
    [&non-continuable                           C co]
    [non-continuable-violation?                 C co]
    [&serious                                   C co]
    [serious-condition?                         C co]
    [simple-conditions                          C co]
    [&syntax                                    C co]
    [syntax-violation                           C co sc]
    [syntax-violation-form                      C co]
    [syntax-violation-subform                   C co]
    [syntax-violation?                          C co]
    [&undefined                                 C co]
    [undefined-violation?                       C co]
    [&violation                                 C co]
    [violation?                                 C co]
    [&warning                                   C co]
    [warning?                                   C co]
    [&who                                       C co]
    [who-condition?                             C co]
    ;;;
    [case-lambda                                C ct]
    [do                                         C ct se ne]
    [unless                                     C ct]
    [when                                       C ct]
    ;;;
    [define-enumeration                         C en]
    [enum-set->list                             C en]
    [enum-set-complement                        C en]
    [enum-set-constructor                       C en]
    [enum-set-difference                        C en]
    [enum-set-indexer                           C en]
    [enum-set-intersection                      C en]
    [enum-set-member?                           C en]
    [enum-set-projection                        C en]
    [enum-set-subset?                           C en]
    [enum-set-union                             C en]
    [enum-set-universe                          C en]
    [enum-set=?                                 C en]
    [make-enumeration                           C en]
    ;;;
    [environment                                C ev]
    [eval                                       C ev se]
    ;;;
    [raise                                      C ex]
    [raise-continuable                          C ex]
    [with-exception-handler                     C ex]
    [guard                                      C ex]
    ;;;
    [binary-port?                               S ip]
    [buffer-mode                                C ip]
    [buffer-mode?                               C ip]
    [bytevector->string                         S ip]
    [call-with-bytevector-output-port           S ip]
    [call-with-port                             S ip]
    [call-with-string-output-port               S ip]
    ;;;
    [assoc                                      C ls se]
    [assp                                       C ls]
    [assq                                       C ls se]
    [assv                                       C ls se]
    [cons*                                      C ls]
    [filter                                     C ls]
    [find                                       C ls]
    [fold-left                                  C ls]
    [fold-right                                 C ls]
    [for-all                                    C ls]
    [exists                                     C ls]
    [member                                     C ls se]
    [memp                                       C ls]
    [memq                                       C ls se]
    [memv                                       C ls se]
    [partition                                  C ls]
    [remq                                       C ls]
    [remp                                       C ls]
    [remv                                       C ls]
    [remove                                     C ls]
    ;;;
    [set-car!                                   C mp se]
    [set-cdr!                                   C mp se]
    ;;;
    [string-set!                                C ms se]
    [string-fill!                               C ms se]
    ;;;
    [command-line                               C pr]
    [exit                                       C pr]
    ;;;
    [delay                                      C r5 se ne]
    [exact->inexact                             C r5 se]
    [force                                      C r5 se]
    [inexact->exact                             C r5 se]
    [modulo                                     C r5 se]
    [remainder                                  C r5 se]
    [null-environment                           C r5 se]
    [quotient                                   C r5 se]
    [scheme-report-environment                  C r5 se]
    ;;;
    [close-port                                 S ip]
    [eol-style                                  C ip]
    [error-handling-mode                        C ip]
    [file-options                               C ip]
    [flush-output-port                          C ip]
    [get-bytevector-all                         S ip]
    [get-bytevector-n                           S ip]
    [get-bytevector-n!                          S ip]
    [get-bytevector-some                        S ip]
    [get-char                                   C ip]
    [get-datum                                  S ip]
    [get-line                                   C ip]
    [get-string-all                             S ip]
    [get-string-n                               S ip]
    [get-string-n!                              S ip]
    [get-u8                                     C ip]
    [&i/o                                       C ip is fi]
    [&i/o-decoding                              C ip]
    [i/o-decoding-error?                        C ip]
    [&i/o-encoding                              C ip]
    [i/o-encoding-error-char                    C ip]
    [i/o-encoding-error?                        C ip]
    [i/o-error-filename                         C ip is fi]
    [i/o-error-port                             C ip is fi]
    [i/o-error?                                 C ip is fi]
    [&i/o-file-already-exists                   C ip is fi]
    [i/o-file-already-exists-error?             C ip is fi]
    [&i/o-file-does-not-exist                   C ip is fi]
    [i/o-file-does-not-exist-error?             C ip is fi]
    [&i/o-file-is-read-only                     C ip is fi]
    [i/o-file-is-read-only-error?               C ip is fi]
    [&i/o-file-protection                       C ip is fi]
    [i/o-file-protection-error?                 C ip is fi]
    [&i/o-filename                              C ip is fi]
    [i/o-filename-error?                        C ip is fi]
    [&i/o-invalid-position                      C ip is fi]
    [i/o-invalid-position-error?                C ip is fi]
    [&i/o-port                                  C ip is fi]
    [i/o-port-error?                            C ip is fi]
    [&i/o-read                                  C ip is fi]
    [i/o-read-error?                            C ip is fi]
    [&i/o-write                                 C ip is fi]
    [i/o-write-error?                           C ip is fi]
    [lookahead-char                             S ip]
    [lookahead-u8                               S ip]
    [make-bytevector                            C bv]
    [make-custom-binary-input-port              S ip]
    [make-custom-binary-input/output-port       S ip]
    [make-custom-binary-output-port             S ip]
    [make-custom-textual-input-port             S ip]
    [make-custom-textual-input/output-port      S ip]
    [make-custom-textual-output-port            S ip]
    [make-i/o-decoding-error                    C ip]
    [make-i/o-encoding-error                    C ip]
    [make-i/o-error                             C ip is fi]
    [make-i/o-file-already-exists-error         C ip is fi]
    [make-i/o-file-does-not-exist-error         C ip is fi]
    [make-i/o-file-is-read-only-error           C ip is fi]
    [make-i/o-file-protection-error             C ip is fi]
    [make-i/o-filename-error                    C ip is fi]
    [make-i/o-invalid-position-error            C ip is fi]
    [make-i/o-port-error                        C ip is fi]
    [make-i/o-read-error                        C ip is fi]
    [make-i/o-write-error                       C ip is fi]
    [latin-1-codec                              C ip]
    [make-transcoder                            C ip]
    [native-eol-style                           C ip]
    [native-transcoder                          C ip]
    [open-bytevector-input-port                 S ip]
    [open-bytevector-output-port                S ip]
    [open-file-input-port                       S ip]
    [open-file-input/output-port                S ip]
    [open-file-output-port                      S ip]
    [open-string-input-port                     C ip]
    [open-string-output-port                    C ip]
    [output-port-buffer-mode                    S ip]
    [port-eof?                                  S ip]
    [port-has-port-position?                    S ip]
    [port-has-set-port-position!?               S ip]
    [port-position                              S ip]
    [port-transcoder                            S ip]
    [port?                                      C ip]
    [put-bytevector                             S ip]
    [put-char                                   C ip]
    [put-datum                                  S ip]
    [put-string                                 S ip]
    [put-u8                                     C ip]
    [set-port-position!                         S ip]
    [standard-error-port                        S ip]
    [standard-input-port                        S ip]
    [standard-output-port                       S ip]
    [string->bytevector                         S ip]
    [textual-port?                              S ip]
    [transcoded-port                            S ip]
    [transcoder-codec                           C ip]
    [transcoder-eol-style                       C ip]
    [transcoder-error-handling-mode             C ip]
    [utf-16-codec                               C ip]
    [utf-8-codec                                C ip]
    ;;;
    [input-port?                                C is ip se]
    [output-port?                               C is ip se]
    [current-input-port                         C ip is se]
    [current-output-port                        C ip is se]
    [current-error-port                         C ip is]
    [eof-object                                 C ip is se]
    [eof-object?                                C ip is]
    [close-input-port                           C is se]
    [close-output-port                          C is se]
    [display                                    C is se]
    [newline                                    C is se]
    [open-input-file                            C is se]
    [open-output-file                           C is se]
    [peek-char                                  C is se]
    [read                                       C is se]
    [read-char                                  C is se]
    [with-input-from-file                       C is se]
    [with-output-to-file                        C is se]
    [write                                      C is se]
    [write-char                                 C is se]
    [call-with-input-file                       C is se]
    [call-with-output-file                      C is se]
    ;;;
    [hashtable-clear!                           C ht]
    [hashtable-contains?                        C ht]
    [hashtable-copy                             C ht]
    [hashtable-delete!                          C ht]
    [hashtable-entries                          C ht]
    [hashtable-keys                             C ht]
    [hashtable-mutable?                         C ht]
    [hashtable-ref                              C ht]
    [hashtable-set!                             C ht]
    [hashtable-size                             C ht]
    [hashtable-update!                          C ht]
    [hashtable?                                 C ht]
    [make-eq-hashtable                          C ht]
    [make-eqv-hashtable                         S ht]
    [hashtable-hash-function                    S ht]
    [make-hashtable                             S ht]
    [hashtable-equivalence-function             S ht]
    [equal-hash                                 S ht]
    [string-hash                                S ht]
    [string-ci-hash                             S ht]
    [symbol-hash                                S ht]
    ;;;
    [list-sort                                  C sr]
    [vector-sort                                C sr]
    [vector-sort!                               C sr]
    ;;;
    [file-exists?                               C fi]
    [delete-file                                C fi]
    ;;;
    [define-record-type                         C rs]
    [fields                                     C rs]
    [immutable                                  C rs]
    [mutable                                    C rs]
    [opaque                                     C rs]
    [parent                                     C rs]
    [parent-rtd                                 C rs]
    [protocol                                   C rs]
    [record-constructor-descriptor              C rs]
    [record-type-descriptor                     C rs]
    [sealed                                     C rs]
    [nongenerative                              C rs]
    ;;;
    [record-field-mutable?                      C ri]
    [record-rtd                                 C ri]
    [record-type-field-names                    C ri]
    [record-type-generative?                    C ri]
    [record-type-name                           C ri]
    [record-type-opaque?                        C ri]
    [record-type-parent                         C ri]
    [record-type-sealed?                        C ri]
    [record-type-uid                            C ri]
    [record?                                    C ri]
    ;;;
    [make-record-constructor-descriptor         C rp]
    [make-record-type-descriptor                C rp]
    [record-accessor                            C rp]
    [record-constructor                         C rp]
    [record-mutator                             C rp]
    [record-predicate                           C rp]
    [record-type-descriptor?                    C rp]
    ;;;
    [bound-identifier=?                         C sc]
    [datum->syntax                              C sc]
    [syntax                                     C sc]
    [syntax->datum                              C sc]
    [syntax-case                                C sc]
    [unsyntax                                   C sc]
    [unsyntax-splicing                          C sc]
    [quasisyntax                                C sc]
    [with-syntax                                C sc]
    [free-identifier=?                          C sc]
    [generate-temporaries                       C sc]
    [identifier?                                C sc]
    [make-variable-transformer                  C sc]
    ;;;
    [char-alphabetic?                           C uc se]
    [char-ci<=?                                 C uc se]
    [char-ci<?                                  C uc se]
    [char-ci=?                                  C uc se]
    [char-ci>=?                                 C uc se]
    [char-ci>?                                  C uc se]
    [char-downcase                              C uc se]
    [char-foldcase                              C uc]
    [char-titlecase                             C uc]
    [char-upcase                                C uc se]
    [char-general-category                      C uc]
    [char-lower-case?                           C uc se]
    [char-numeric?                              C uc se]
    [char-title-case?                           C uc]
    [char-upper-case?                           C uc se]
    [char-whitespace?                           C uc se]
    [string-ci<=?                               C uc se]
    [string-ci<?                                C uc se]
    [string-ci=?                                C uc se]
    [string-ci>=?                               C uc se]
    [string-ci>?                                C uc se]
    [string-downcase                            S uc]
    [string-foldcase                            S uc]
    [string-normalize-nfc                       S uc]
    [string-normalize-nfd                       S uc]
    [string-normalize-nfkc                      S uc]
    [string-normalize-nfkd                      S uc]
    [string-titlecase                           S uc]
    [string-upcase                              S uc]
    ;;;
    [char-ready?                                S ]
    [interaction-environment                    S ]
    [load                                       S ]
    ;;;
    ))


(define (no-dups ls)
  (unless (null? ls)
    (when (memq (car ls) (cdr ls))
      (error #f "duplicate identifier" (car ls)))
    (no-dups (cdr ls))))

(define (assert-id x)
  (unless (and (>= (length x) 2)
               (let ([name (car x)]
                     [status (cadr x)]
                     [libs (cddr x)])
                 (no-dups libs)
                 (and (assq status status-names)
                      (andmap (lambda (x) 
                                (assq x library-names))
                        libs))))
    (error #f "invalid identifier" x)))


(define (filter* ls) 
  (filter 
    (lambda (x) 
      (not (null? (filter 
                    (lambda (x) 
                      (memq x ls))
                    (cdr x)))))
    identifier-names))

(define (count-status x)
  (length (filter* (list x))))

(define (join s ls) 
  (cond
    [(null? ls) ""]
    [(null? (cdr ls)) (format "~a" (car ls))]
    [else
     (format "~a~a~a" (car ls) s (join s (cdr ls)))]))

(define (status-str x)
  (cond
    [(assq x identifier-names) 
     =>
     (lambda (x) 
       (let ([st (cadr x)] [libs (cddr x)])
         (format "(~a ~a)" st (join "," libs))))]
    [else (error #f "invalid identifier" x)]))

(define (print-ids ls) 
  (define (split ls n) 
    (cond
      [(null? ls) (values '() '())]
      [(> (string-length (car ls)) n) 
       (values '() ls)]
      [else
       (let-values ([(fst rest) 
                     (split (cdr ls) 
                        (- n 
                           (string-length (car ls))))])
         (values (cons (car ls) fst) rest))]))
  (define (print-ids ls)
    (unless (null? ls) 
      (let-values ([(ls rest) 
                    (split ls 80)])
        (for-each display ls)
        (newline)
        (print-ids rest))))
  (print-ids 
    (map (lambda (x) (format "~s ~a   " x (status-str x))) ls)))


(define (split p ls) 
  (cond
    [(null? ls) (values '() '())]
    [else
     (let-values ([(ls1 ls2) 
                   (split p (cdr ls))])
       (if (p (car ls))
           (values (cons (car ls) ls1) ls2)
           (values ls1 (cons (car ls) ls2))))]))

(define (null-intersection? ls1 ls2)
  (cond
    [(null? ls1) #t]
    [(memq (car ls1) ls2) #f]
    [else (null-intersection? (cdr ls1) ls2)]))


(define (library-info lib) 
  (let ([inf (map (lambda (x) (cons (car x) 0)) status-names)])
    (for-each 
      (lambda (x) 
        (let ([s (cadr x)]
              [l* (cddr x)]) 
          (cond
            [(and (memq lib l*) (assq s inf)) =>
             (lambda (x) 
               (set-cdr! x (add1 (cdr x))))])))
      identifier-names)
    (join " " 
       (map (lambda (x) 
              (format "~a=~a" (car x) (cdr x)))
            (filter 
              (lambda (x) 
                (not (zero? (cdr x))))
              inf)))))

(no-dups (map car identifier-names))
(no-dups (map car library-names))
(no-dups (map car status-names))
(for-each assert-id identifier-names)


(let ([args (cdr (command-line-arguments))]
      [exe (car (command-line-arguments))])
  (cond
    [(null? args) 
     (printf "usage:  ~a (<status>|<libname>)*\n\n" exe)
     (printf "Library Names:\n")
     (for-each 
       (lambda (x) 
         (printf "   ~a  ~a   ~a\n" (car x) (cadr x)
                 (library-info (car x))))
       library-names)
     (printf "Status Codes:\n")
     (let* ([s* (map 
                  (lambda (x)
                    (count-status (car x)))
                  status-names)]
            [all (apply + s*)])
       (for-each 
         (lambda (x s) 
           (printf "   ~a  ~a  (~s ids == ~s%)\n"
                   (car x) (cadr x) 
                   s
                   (/ (round (* (/ s all) 10000)) 100.0)))
         status-names s*))]
    [else
     (let-values ([(s* l*)
                   (split 
                     (lambda (x) 
                       (cond
                         [(assq x status-names)  #t]
                         [(assq x library-names) #f]
                         [else (error #f "invalid argument" x)]))
                     (map string->symbol args))])
       (let ([ls (filter
                   (lambda (x) 
                     (let ([s (cadr x)]
                           [libs (cddr x)])
                       (cond
                         [(null? l*) (memq s s*)]
                         [(null? s*)
                          (not (null-intersection? l* libs))]
                         [else 
                          (and (memq s s*) 
                               (not (null-intersection? l* libs)))])))
                   identifier-names)])
         (printf "~s identifiers\n" (length ls))
         (print-ids (map car ls))))]
    ))

