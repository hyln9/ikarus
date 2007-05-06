
#| current-library-collection   procedure
  Calling (current-library-collection) returns a procedure that:
    - when called with no arguments, it returns a list of the set
      of
      libraries in the collection.
    - when called with a single argument, it adds that library to
      the set of libraries in the collection.
  Calling (current-library-collection f) sets the current library 
   collection to be the procedure f which must follow the protocol
   above.
|#



(library (ikarus library-manager)
  (export imported-label->binding library-subst/env
          current-library-collection
          installed-libraries
          find-library-by-name imported-loc->library install-library
          library-spec invoke-library)
  (import (except (ikarus) imported-label->binding library-subst/env
          current-library-collection
          installed-libraries
          find-library-by-name imported-loc->library install-library
          library-spec invoke-library))



  (define (make-collection)
    (let ([set '()])
      (define (set-cons x ls)
        (cond
          [(memq x ls) ls]
          [else (cons x ls)]))
      (case-lambda
        [() set]
        [(x) (set! set (set-cons x set))])))

  (define current-library-collection
    (make-parameter (make-collection) 
      (lambda (x)
        (unless (procedure? x)
          (error 'current-library-collection 
            "~s is not a procedure" x))
        x)))

  (define-record library 
    (id name ver imp* vis* inv* subst env visit-state invoke-state))

  (define (find-dependencies ls)
    (cond
      [(null? ls) '()]
      [else (error 'find-dependencies "cannot handle deps yet")]))

  (define (find-library-by pred)
    (let f ([ls ((current-library-collection))])
      (cond
        [(null? ls) #f]
        [(pred (car ls)) (car ls)]
        [else (f (cdr ls))])))

  (define (find-library-by-name name)
    (find-library-by
      (lambda (x) (equal? (library-name x) name))))

  (define (find-library-by-name/die name)
    (or (find-library-by-name name)
        (error #f "cannot find library ~s" name)))

  (define (find-library-by-spec/die spec)
    (let ([id (car spec)])
      (or (find-library-by
            (lambda (x) (eq? id (library-id x))))
          (error #f "cannot find library with spec ~s" spec))))

  (define (install-library id name ver
             imp* vis* inv* exp-subst exp-env visit-code invoke-code)
    (let ([imp-lib* (map find-library-by-spec/die imp*)]
          [vis-lib* (map find-library-by-spec/die vis*)]
          [inv-lib* (map find-library-by-spec/die inv*)])
      (unless (and (symbol? id) (list? name) (list? ver))
        (error 'install-library "invalid spec ~s ~s ~s" id name ver))
      (when (find-library-by-name name)
        (error 'install-library "~s is already installed" name))
      (let ([lib (make-library id name ver imp-lib* vis-lib* inv-lib* 
                    exp-subst exp-env visit-code invoke-code)])
        ((current-library-collection) lib))))

  (define scheme-env ; the-env
    '([define        define-label        (define)]
      [define-syntax define-syntax-label (define-syntax)]
      [module        module-label        (module)]
      [begin         begin-label         (begin)]
      [set!          set!-label          (set!)]
      [define-record define-record-label (macro . define-record)]
      [include       include-label       (macro . include)]
      [syntax-rules  syntax-rules-macro  (macro . syntax-rules)]
      [quasiquote    quasiquote-macro    (macro . quasiquote)]
      [with-syntax   with-syntax-label   (macro . with-syntax)]
      [let           let-label           (macro . let)]
      [let*          let*-label          (macro . let*)]
      [cond          cond-label          (macro . cond)]
      [and           and-label           (macro . and)]
      [or            or-label            (macro . or)]
      [case          case-label          (core-macro .  case)]
      [foreign-call  foreign-call-label  (core-macro .  foreign-call)]
      [quote         quote-label         (core-macro . quote)]
      [syntax-case   syntax-case-label   (core-macro . syntax-case)]
      [syntax        syntax-label        (core-macro . syntax)]
      [lambda        lambda-label        (core-macro . lambda)]
      [case-lambda   case-lambda-label   (core-macro . case-lambda)]
      [let-values    let-values-label    (core-macro . let-values)]
      [type-descriptor           type-descriptor-label           (core-macro . type-descriptor)]
      [letrec        letrec-label        (core-macro . letrec)]
      [if            if-label            (core-macro . if)]
      [when          when-label          (core-macro . when)]
      [unless        unless-label        (core-macro . unless)]
      [parameterize  parameterize-label  (core-macro . parameterize)]
      ;;; prims
      [void         void-label        (core-prim . void)]
      [not          not-label         (core-prim . not)]
      [boolean?     boolean-label     (core-prim . boolean?)]
      [null?        null?-label       (core-prim . null?)]
      [procedure?   procedure?-label  (core-prim . procedure?)]
      [eof-object?  eof-object?-label (core-prim . eof-object?)]
      [eof-object   eof-object-label  (core-prim . eof-object)]
      ;;; comparison
      [eq?        eq?-label        (core-prim . eq?)]
      [eqv?       eqv?-label       (core-prim . eqv?)]
      [equal?     equal?-label     (core-prim . equal?)]
      ;;; pairs/lists
      [cons       cons-label       (core-prim . cons)]
      [pair?      pair?-label      (core-prim . pair?)]
      [car        car-label        (core-prim . car)]
      [cdr        cdr-label        (core-prim . cdr)]
      [set-car!   set-car!-label   (core-prim . set-car!)]
      [set-cdr!   set-cdr!-label   (core-prim . set-cdr!)]
      [caar       caar-label       (core-prim . caar)]
      [cdar       cdar-label       (core-prim . cdar)]
      [cadr       cadr-label       (core-prim . cadr)]
      [cddr       cddr-label       (core-prim . cddr)]
      [caaar       caaar-label       (core-prim . caaar)]
      [cdaar       cdaar-label       (core-prim . cdaar)]
      [cadar       cadar-label       (core-prim . cadar)]
      [cddar       cddar-label       (core-prim . cddar)]
      [caadr       caadr-label       (core-prim . caadr)]
      [cdadr       cdadr-label       (core-prim . cdadr)]
      [caddr       caddr-label       (core-prim . caddr)]
      [cdddr       cdddr-label       (core-prim . cdddr)]
      [caaaar       caaaar-label       (core-prim . caaaar)]
      [cdaaar       cdaaar-label       (core-prim . cdaaar)]
      [cadaar       cadaar-label       (core-prim . cadaar)]
      [cddaar       cddaar-label       (core-prim . cddaar)]
      [caadar       caadar-label       (core-prim . caadar)]
      [cdadar       cdadar-label       (core-prim . cdadar)]
      [caddar       caddar-label       (core-prim . caddar)]
      [cdddar       cdddar-label       (core-prim . cdddar)]
      [caaadr       caaadr-label       (core-prim . caaadr)]
      [cdaadr       cdaadr-label       (core-prim . cdaadr)]
      [cadadr       cadadr-label       (core-prim . cadadr)]
      [cddadr       cddadr-label       (core-prim . cddadr)]
      [caaddr       caaddr-label       (core-prim . caaddr)]
      [cdaddr       cdaddr-label       (core-prim . cdaddr)]
      [cadddr       cadddr-label       (core-prim . cadddr)]
      [cddddr       cddddr-label       (core-prim . cddddr)]
      [list       list-label       (core-prim . list)]
      [list-ref       list-ref-label       (core-prim . list-ref)]
      [make-list make-list-label (core-prim . make-list)]
      [list*       list*-label       (core-prim . list*)]
      [list?      list?-label      (core-prim . list?)]
      [append     append-label     (core-prim . append)]
      [last-pair     last-pair-label     (core-prim . last-pair)]
      [reverse     reverse-label     (core-prim . reverse)]
      [length     length-label     (core-prim . length)]
      [assq       assq-label       (core-prim . assq)]
      [assv       assv-label       (core-prim . assv)]
      [assoc      assoc-label      (core-prim . assoc)]
      [memq       memq-label       (core-prim . memq)]
      [memv       memv-label       (core-prim . memv)]
      [member     member-label     (core-prim . member)]
      [$car       $car-label       (core-prim . $car)]
      [$cdr       $cdr-label       (core-prim . $cdr)]
      [$set-car!       $set-car!-label       (core-prim . $set-car!)]
      [$set-cdr!       $set-cdr!-label       (core-prim . $set-cdr!)]
      [$memq       $memq-label       (core-prim . $memq)]
      [$memv       $memv-label       (core-prim . $memv)]
      ;;; weak conses
      [bwp-object?  bwp-object?-label (core-prim . bwp-object?)]
      [weak-cons       weak-cons-label       (core-prim . weak-cons)]
      [weak-pair?       weak-pair?-label       (core-prim . weak-pair?)]
      ;;; chars
      [char?     char?-label     (core-prim . char?)]
      [char=?     char=?-label     (core-prim . char=?)]
      [char<?     char<?-label     (core-prim . char<?)]
      [char>?     char>?-label     (core-prim . char>?)]
      [char<=?     char<=?-label     (core-prim . char<=?)]
      [char>=?     char>=?-label     (core-prim . char>=?)]
      [integer->char integer->char-label (core-prim . integer->char)]
      [char->integer char->integer-label (core-prim . char->integer)]
      [char-whitespace? char-whitespace?-label (core-prim . char-whitespace?)]
      [$char?     $char?-label     (core-prim . $char?)]
      [$char=    $char=-label    (core-prim . $char=)]
      [$char<    $char<-label    (core-prim . $char<)]
      [$char>    $char>-label    (core-prim . $char>)]
      [$char<=    $char<=-label    (core-prim . $char<=)]
      [$char>=    $char>=-label    (core-prim . $char>=)]
      [$char->fixnum $char->fixnum-label (core-prim . $char->fixnum)]
      [$fixnum->char $fixnum->char-label (core-prim . $fixnum->char)]
      ;;; strings
      [string?    string?-label    (core-prim . string?)]
      [string    string-label    (core-prim . string)]
      [make-string make-string-label (core-prim . make-string)]
      [string-ref string-ref-label (core-prim . string-ref)]
      [string-set! string-set!-label (core-prim . string-set!)]
      [string-length string-length-label (core-prim . string-length)]
      [string=?   string=?-label   (core-prim . string=?)]
      [substring  substring-label  (core-prim . substring)]
      [string-append string-append-label (core-prim . string-append)]
      [string->list string->list-label (core-prim . string->list)]
      [list->string list->string-label (core-prim . list->string)]
      [uuid    uuid-label    (core-prim . uuid)]
      [date-string    date-string-label    (core-prim . date-string)]
      [$make-string $make-string-label (core-prim . $make-string)]
      [$string-ref $string-ref-label (core-prim . $string-ref)]
      [$string-set! $string-set!-label (core-prim . $string-set!)]
      [$string-length $string-length-label (core-prim . $string-length)]
      ;;; vectors
      [vector      vector-label      (core-prim . vector)]
      [make-vector make-vector-label (core-prim . make-vector)]
      [vector-ref  vector-ref-label  (core-prim . vector-ref)]
      [vector-set! vector-set!-label (core-prim . vector-set!)]
      [vector?     vector?-label     (core-prim . vector?)]
      [vector-length vector-length-label (core-prim . vector-length)]
      [list->vector list->vector-label (core-prim . list->vector)]
      [vector->list vector->list-label (core-prim . vector->list)]
      [$make-vector $make-vector-label (core-prim . $make-vector)]
      [$vector-length $vector-length-label (core-prim . $vector-length)]
      [$vector-ref $vector-ref-label (core-prim . $vector-ref)]
      [$vector-set! $vector-set!-label (core-prim . $vector-set!)]
      ;;; iterators
      [for-each   for-each-label   (core-prim . for-each)]
      [map        map-label        (core-prim . map)]
      [andmap     andmap-label     (core-prim . andmap)]
      [ormap      ormap-label      (core-prim . ormap)]
      ;;; fixnums
      [fixnum?    fixnum-label     (core-prim . fixnum?)]
      [fx<        fx<-label        (core-prim . fx<)]
      [fx<=       fx<=-label       (core-prim . fx<=)]
      [fx>        fx>-label        (core-prim . fx>)]
      [fx>=       fx>=-label       (core-prim . fx>=)]
      [fx=        fx=-label        (core-prim . fx=)]
      [fx-        fx--label        (core-prim . fx-)]
      [fx+        fx+-label        (core-prim . fx+)]
      [fx*        fx*-label        (core-prim . fx*)]
      [fxzero?    fxzero?-label    (core-prim . fxzero?)]
      [fxadd1     fxadd1-label     (core-prim . fxadd1)]
      [fxsub1     fxsub1-label     (core-prim . fxsub1)]
      [fxquotient fxquotient-label (core-prim . fxquotient)]
      [fxremainder fxremainder-label (core-prim . fxremainder)]
      [fxmodulo fxmodulo-label (core-prim . fxmodulo)]
      [fxsll       fxsll-label       (core-prim . fxsll)]
      [fxsra       fxsra-label       (core-prim . fxsra)]
      [fxlogand    fxlogand-label    (core-prim . fxlogand)]
      [fxlogxor    fxlogxor-label    (core-prim . fxlogxor)]
      [fxlogor    fxlogor-label    (core-prim . fxlogor)]
      [fxlognot    fxlognot-label    (core-prim . fxlognot)]
      [fixnum->string fixnum->string-label (core-prim . fixnum->string)]
      [$fxzero?    $fxzero?-label    (core-prim . $fxzero?)]
      [$fxadd1        $fxadd1-label        (core-prim . $fxadd1)]
      [$fxsub1        $fxsub1-label        (core-prim . $fxsub1)]
      [$fx>=       $fx>=-label       (core-prim . $fx>=)]
      [$fx<=       $fx<=-label       (core-prim . $fx<=)]
      [$fx>       $fx>-label       (core-prim . $fx>)]
      [$fx<       $fx<-label       (core-prim . $fx<)]
      [$fx=        $fx=-label        (core-prim . $fx=)]
      [$fxsll        $fxsll-label        (core-prim . $fxsll)]
      [$fxsra        $fxsra-label        (core-prim . $fxsra)]
      [$fxquotient $fxquotient-label (core-prim . $fxquotient)]
      [$fxmodulo $fxmodulo-label (core-prim . $fxmodulo)]
      [$fxlogxor        $fxlogxor-label        (core-prim . $fxlogxor)]
      [$fxlogor        $fxlogor-label        (core-prim . $fxlogor)]
      [$fxlognot        $fxlognot-label        (core-prim . $fxlognot)]
      [$fxlogand        $fxlogand-label        (core-prim . $fxlogand)]
      [$fx+        $fx+-label        (core-prim . $fx+)]
      [$fx*        $fx*-label        (core-prim . $fx*)]
      [$fx-        $fx--label        (core-prim . $fx-)]
      ;;; flonum
      [string->flonum string->flonum-label (core-prim . string->flonum)]
      ;;; generic arithmetic
      [-          minus-label    (core-prim . -)]
      [=          =-label        (core-prim . =)]
      [<          <-label        (core-prim . <)]
      [>          >-label        (core-prim . >)]
      [<=          <=-label      (core-prim . <=)]
      [>=          >=-label      (core-prim . >=)]
      [*          *-label        (core-prim . *)]
      [+          plus-label     (core-prim . +)]
      [add1        add1-label    (core-prim . add1)]
      [sub1        sub1-label    (core-prim . sub1)]
      [number?   number?-label   (core-prim . number?)]
      [bignum?   bignum?-label   (core-prim . bignum?)]
      [integer?   integer?-label   (core-prim . integer?)]
      [flonum?   flonum?-label   (core-prim . flonum?)]
      [quotient   quotient-label   (core-prim . quotient)]
      [remainder   remainder-label   (core-prim . remainder)]
      [quotient+remainder   quotient+remainder-label   (core-prim . quotient+remainder)]
      [number->string   number->string-label   (core-prim . number->string)]
      [string->number   string->number-label   (core-prim . string->number)]
      ;;; other numerics
      [flonum->string   flonum->string-label   (core-prim . flonum->string)]
      [string->flonum   string->flonum-label   (core-prim . string->flonum)]
      ;;; symbols/gensyms
      [symbol?    symbol?-label    (core-prim . symbol?)]
      [gensym?    gensym?-label    (core-prim . gensym?)]
      [gensym     gensym-label     (core-prim . gensym)]
      [getprop    getprop-label    (core-prim . getprop)]
      [putprop    putprop-label    (core-prim . putprop)]
      [remprop    remprop-label    (core-prim . remprop)]
      [property-list    property-list-label    (core-prim . property-list)]
      [string->symbol string->symbol-label (core-prim . string->symbol)]
      [symbol->string symbol->string-label (core-prim . symbol->string)]
      [gensym->unique-string gensym->unique-string-label (core-prim . gensym->unique-string)]
      [$make-symbol     $make-symbol-label     (core-prim . $make-symbol)]
      [$symbol-unique-string $symbol-unique-string-label (core-prim . $symbol-unique-string)]
      [$symbol-value $symbol-value-label (core-prim . $symbol-value)]
      [$symbol-string $symbol-string-label (core-prim . $symbol-string)]
      [$symbol-plist $symbol-plist-label (core-prim . $symbol-plist)]
      [$set-symbol-value! $set-symbol-value!-label (core-prim . $set-symbol-value!)]
      [$set-symbol-string! $set-symbol-string!-label (core-prim . $set-symbol-string!)]
      [$set-symbol-unique-string! $set-symbol-unique-string!-label (core-prim . $set-symbol-unique-string!)]
      [$set-symbol-plist! $set-symbol-plist!-label (core-prim . $set-symbol-plist!)]
      ;;; top-level
      [top-level-bound?     top-level-bound-label      (core-prim . top-level-bound?)]
      [top-level-value      top-level-value-label      (core-prim .  top-level-value)]
      [set-top-level-value! set-top-level-value!-label (core-prim .  set-top-level-value!)]
      ;;; guardians
      [make-guardian     make-guardian-label     (core-prim . make-guardian)]
      ;;; IO/low-level
      [$make-port/input        $make-port/input-label        (core-prim .  $make-port/input)]
      [$make-port/output        $make-port/output-label        (core-prim .  $make-port/output)]
      [$make-port/both        $make-port/both-label        (core-prim .  $make-port/both)]
      [$port-handler        $port-handler-label        (core-prim .  $port-handler)]
      [$port-input-buffer        $port-input-buffer-label        (core-prim .  $port-input-buffer)]
      [$port-input-index        $port-input-index-label        (core-prim .  $port-input-index)]
      [$port-input-size        $port-input-size-label        (core-prim .  $port-input-size)]
      [$port-output-buffer        $port-output-buffer-label        (core-prim .  $port-output-buffer)]
      [$port-output-index        $port-output-index-label        (core-prim .  $port-output-index)]
      [$port-output-size        $port-output-size-label        (core-prim .  $port-output-size)]
      [$set-port-input-index!        $set-port-input-index!-label        (core-prim .  $set-port-input-index!)]
      [$set-port-input-size!        $set-port-input-size!-label        (core-prim .  $set-port-input-size!)]
      [$set-port-output-index!        $set-port-output-index!-label        (core-prim .  $set-port-output-index!)]
      [$set-port-output-size!        $set-port-output-size!-label        (core-prim .  $set-port-output-size!)]
      [make-input-port        make-input-port-label        (core-prim .  make-input-port)]
      [make-output-port        make-output-port-label        (core-prim .  make-output-port)]
      [make-input/output-port        make-input/output-port-label        (core-prim .  make-input/output-port)]
      [$make-input-port        $make-input-port-label        (core-prim .  $make-input-port)]
      [$make-output-port        $make-output-port-label        (core-prim .  $make-output-port)]
      [$make-input/output-port        $make-input/output-port-label        (core-prim .  $make-input/output-port)]
      [port-output-index        port-output-index-label        (core-prim .  port-output-index)]
      [port-output-size        port-output-size-label        (core-prim .  port-output-size)]
      [port-output-buffer        port-output-buffer-label        (core-prim .  port-output-buffer)]
      [set-port-output-index!        set-port-output-index!-label        (core-prim .  set-port-output-index!)]
      [set-port-output-size!        set-port-output-size!-label        (core-prim .  set-port-output-size!)]
      [port-input-buffer        port-input-buffer-label        (core-prim .  port-input-buffer)]
      [port-input-index        port-input-index-label        (core-prim .  port-input-index)]
      [port-input-size        port-input-size-label        (core-prim .  port-input-size)]
      [set-port-input-index!        set-port-input-index!-label        (core-prim .  set-port-input-index!)]
      [set-port-input-size!        set-port-input-size!-label        (core-prim .  set-port-input-size!)]
      [*standard-input-port*        *standard-input-port*-label        (core-prim .  *standard-input-port*)]
      [*standard-output-port*        *standard-output-port*-label        (core-prim .  *standard-output-port*)]
      [*standard-error-port*        *standard-error-port*-label        (core-prim .  *standard-error-port*)]
      [*current-input-port*        *current-input-port*-label        (core-prim .  *current-input-port*)]
      [*current-output-port*        *current-output-port*-label        (core-prim .  *current-output-port*)]
      ;;; IO/ports
      [output-port?        output-port?-label        (core-prim .  output-port?)]
      [input-port?        input-port?-label        (core-prim .  input-port?)]
      [port?        port?-label        (core-prim .  port?)]
      [port-name        port-name-label        (core-prim .  port-name)]
      [input-port-name        input-port-name-label        (core-prim .  input-port-name)]
      [output-port-name        output-port-name-label        (core-prim .  output-port-name)]
      [open-input-file  open-input-file-label  (core-prim .  open-input-file)]
      [with-input-from-file  with-input-from-file-label  (core-prim .  with-input-from-file)]
      [with-output-to-file  with-output-to-file-label  (core-prim .  with-output-to-file)]
      [open-output-file  open-output-file-label  (core-prim .  open-output-file)]
      [open-output-string  open-output-string-label  (core-prim .  open-output-string)]
      [get-output-string  get-output-string-label  (core-prim .  get-output-string)]
      [close-input-port  close-input-port-label  (core-prim .  close-input-port)]
      [close-output-port  close-output-port-label  (core-prim .  close-output-port)]
      [console-input-port  console-input-port-label  (core-prim .  console-input-port)]
      [console-output-port console-output-port-label (core-prim .  console-output-port)]
      [current-input-port  current-input-port-label  (core-prim .  current-input-port)]
      [current-output-port current-output-port-label (core-prim .  current-output-port)]
      [standard-input-port  standard-input-port-label  (core-prim .  standard-input-port)]
      [standard-output-port standard-output-port-label (core-prim .  standard-output-port)]
      [standard-error-port  standard-error-port-label  (core-prim .  standard-error-port)]
      [flush-output-port   flush-output-port-label   (core-prim .  flush-output-port)]
      [reset-input-port!   reset-input-port!-label   (core-prim .  reset-input-port!)]
      [$flush-output-port   $flush-output-port-label   (core-prim .  $flush-output-port)]
      [$reset-input-port!   $reset-input-port!-label   (core-prim .  $reset-input-port!)]
      [$close-input-port  $close-input-port-label  (core-prim .  $close-input-port)]
      [$close-output-port  $close-output-port-label  (core-prim .  $close-output-port)]
      ;;; IO/high-level
      [display    display-label    (core-prim . display)]
      [write      write-label      (core-prim . write)]
      [write-char write-char-label (core-prim . write-char)]
      [read       read-label       (core-prim . read)]
      [read-char  read-char-label  (core-prim . read-char)]
      [read-token  read-token-label  (core-prim . read-token)]
      [peek-char  peek-char-label  (core-prim . peek-char)]
      [unread-char  unread-char-label  (core-prim . unread-char)]
      [newline    newline-label    (core-prim . newline)]
      [printf     printf-label     (core-prim . printf)]
      [format     format-label     (core-prim . format)]
      [pretty-print pretty-print-label (core-prim . pretty-print)]
      [comment-handler comment-handler-label (core-prim . comment-handler)]
      [print-gensym print-gensym-label (core-prim . print-gensym)]
      [gensym-count       gensym-count-label       (core-prim . gensym-count)]
      [gensym-prefix       gensym-prefix-label       (core-prim . gensym-prefix)]
      [$write-char $write-char-label (core-prim . $write-char)]
      [$read-char  $read-char-label  (core-prim . $read-char)]
      [$peek-char  $peek-char-label  (core-prim . $peek-char)]
      [$unread-char  $unread-char-label  (core-prim . $unread-char)]
      ;;; hash tables
      [make-hash-table make-hash-table-label (core-prim . make-hash-table)]
      [hash-table?      hash-table?-label      (core-prim . hash-table?)]
      [get-hash-table get-hash-table-label (core-prim . get-hash-table)]
      [put-hash-table! put-hash-table!-label (core-prim . put-hash-table!)]
      ;;; evaluation / control
      [make-parameter      make-parameter-label      (core-prim . make-parameter)]
      [apply      apply-label      (core-prim . apply)]
      [values     values-label     (core-prim . values)]
      [call-with-values cwv-label  (core-prim . call-with-values)]
      [call/cc     call/cc-label   (core-prim . call/cc)]
      [call/cf     call/cf-label   (core-prim . call/cf)]
      [dynamic-wind dynamic-wind-label (core-prim . dynamic-wind)]
      [error      error-label      (core-prim . error)]
      [print-error print-error-label (core-prim . print-error)]
      [error-handler error-handler-label (core-prim .  error-handler)]
      [interrupt-handler interrupt-handler-label (core-prim .  interrupt-handler)]
      [exit       exit-label       (core-prim . exit)]
      [compile-core-expr-to-port  compile-core-expr-to-port-label (core-prim . compile-core-expr-to-port)]
      [eval-core  eval-core-label  (core-prim . eval-core)]
      [load       load-label       (core-prim . load)]
      [assembler-output       assembler-output-label       (core-prim . assembler-output)]
      [fasl-write       fasl-write-label       (core-prim . fasl-write)]
      [new-cafe   new-cafe-label   (core-prim . new-cafe)]
      [command-line-arguments command-line-arguments-label (core-prim .  command-line-arguments)]
      [list*->code*       list*->code*-label       (core-prim . list*->code*)]
      [eval-top-level   eval-top-level-label   (core-prim . eval-top-level)]
      [current-primitive-locations       current-primitive-locations-label       (core-prim . current-primitive-locations)]
      ;;; record/mid-level
      [record?                 record?-label                 (core-prim . record?)]
      [make-record-type        make-record-type-label        (core-prim . make-record-type)]
      [record-type-descriptor  record-type-descriptor-label  (core-prim . record-type-descriptor)]
      [record-type-field-names record-type-field-names-label (core-prim . record-type-field-names)]
      [record-type-symbol      record-type-symbol-label      (core-prim . record-type-symbol)]
      [record-type-name        record-type-name-label        (core-prim . record-type-name)]
      [record-name             record-name-label             (core-prim . record-name)]
      [record-constructor      record-constructor-label      (core-prim . record-constructor)]
      [record-predicate        record-predicate-labe         (core-prim . record-predicate)]
      [record-length           record-length-label           (core-prim . record-length)]
      [record-printer          record-printer-label          (core-prim . record-printer)]
      [record-ref              record-ref-label              (core-prim . record-ref)]
      [record-field-accessor   record-field-accessor-label   (core-prim . record-field-accessor)]
      [record-field-mutator    record-field-mutator-label    (core-prim . record-field-mutator)]
      ;;; records/low-level 
      [$base-rtd    $base-rtd-label    (core-prim . $base-rtd)]
      [$record-set! $record-set!-label (core-prim . $record-set!)]
      [$record-ref  $record-ref-label  (core-prim . $record-ref)]
      [$record-rtd  $record-rtd-label  (core-prim . $record-rtd)]
      [$record      $record-label      (core-prim . $record)]
      [$make-record $make-record-label (core-prim . $make-record)]
      [$record?     $record?-label     (core-prim . $record?)]
      [$record/rtd? $record/rtd?-label (core-prim . $record/rtd?)]
      ;;; syntax-case
      [identifier?          identifier?-label          (core-prim . identifier?)]
      [syntax-error             syntax-error-label             (core-prim . syntax-error)]
      [generate-temporaries generate-temporaries-label (core-prim . generate-temporaries)]
      [free-identifier=?    free-identifier=?-label    (core-prim . free-identifier=?)]
      [boot-library-expand boot-library-expand-label (core-prim . boot-library-expand)]
      ;;; codes
      [$closure-code  $closure-code-label (core-prim . $closure-code)]
      [$code? $code?-label (core-prim . $code?)]
      [$code->closure $code->closure-label (core-prim . $code->closure)]
      [$code-reloc-vector $code-reloc-vector-label (core-prim . $code-reloc-vector)]
      [$code-freevars $code-freevars-label (core-prim . $code-freevars)]
      [$code-size $code-size-label (core-prim . $code-size)]
      [$code-ref $code-ref-label (core-prim . $code-ref)]
      [$code-set! $code-set!-label (core-prim . $code-set!)]
      [code? code?-label (core-prim . code?)]
      [make-code make-code-label (core-prim . make-code)]
      [code-reloc-vector code-reloc-vector-label (core-prim . code-reloc-vector)]
      [set-code-reloc-vector! set-code-reloc-vector!-label (core-prim . set-code-reloc-vector!)]
      [code-size code-size-label (core-prim . code-size)]
      [code-freevars code-freevars-label (core-prim . code-freevars)]
      [code-ref code-ref-label (core-prim . code-ref)]
      [code-set! code-set!-label (core-prim . code-set!)]
      ;;; tcbuckets
      [$make-tcbucket     $make-tcbucket-label    (core-prim . $make-tcbucket)]
      [$tcbucket-key     $tcbucket-key-label    (core-prim . $tcbucket-key)]
      [$tcbucket-val     $tcbucket-val-label    (core-prim . $tcbucket-val)]
      [$tcbucket-next     $tcbucket-next-label    (core-prim . $tcbucket-next)]
      [$set-tcbucket-val! $set-tcbucket-val!-label (core-prim . $set-tcbucket-val!)]
      [$set-tcbucket-next! $set-tcbucket-next!-label (core-prim . $set-tcbucket-next!)]
      [$set-tcbucket-tconc! $set-tcbucket-tconc!-label (core-prim . $set-tcbucket-tconc!)]
      ;;; misc
      [immediate?     immediate?-label    (core-prim . immediate?)]
      [pointer-value     pointer-value-label    (core-prim . pointer-value)]
      [$forward-ptr?     $forward-ptr?-label    (core-prim . $forward-ptr?)]
      ;;; junk that should go away
      [$unbound-object?     $unbound-object?-label    (core-prim . $unbound-object?)]
      [$make-call-with-values-procedure $make-cwv-procedure (core-prim . $make-call-with-values-procedure)]
      [$make-values-procedure $make-values-procedure (core-prim . $make-values-procedure)]
      [$$apply     $$apply-label    (core-prim . $$apply)]
      [$arg-list     $arg-list-label    (core-prim . $arg-list)]
      [$interrupted? $interrupted?-label (core-prim .  $interrupted?)]
      [$unset-interrupted!     $unset-interrupted!-label    (core-prim . $unset-interrupted!)]
      [$fp-at-base     $fp-at-base-label    (core-prim . $fp-at-base)]
      [$primitive-call/cc     $primitive-call/cc-label    (core-prim . $primitive-call/cc)]
      [$frame->continuation     $frame->continuation-label    (core-prim . $frame->continuation)]
      [$current-frame     $current-frame-label    (core-prim . $current-frame)]
      [$seal-frame-and-call     $seal-frame-and-call-label    (core-prim . $seal-frame-and-call)]
      [installed-libraries     installed-libraries-label (core-prim . installed-libraries)]
      [library-subst/env     library-subst/env-label (core-prim . library-subst/env)]
      [find-library-by-name     find-library-by-name-label (core-prim . find-library-by-name)]
      [imported-label->binding  imported-label->binding-label (core-prim . imported-label->binding)] 
      [imported-loc->library  imported-loc->library-label (core-prim . imported-loc->library)] 
      [library-spec library-spec-label (core-prim . library-spec)]
      [current-library-collection current-library-collection-label (core-prim . current-library-collection)]
      [invoke-library invoke-library-label (core-prim . invoke-library)]
      ))
  (define (imported-label->binding lab)
    (let f ([ls ((current-library-collection))])
      (cond
        [(null? ls) #f]
        [(assq lab (library-env (car ls))) => cdr]
        [else (f (cdr ls))])))

  (define (imported-loc->library loc)
    (define (loc-in-env? ls)
      (and (pair? ls)
           (let ([a (car ls)])
             (let ([binding (cdr a)])
               (or (and (eq? (car binding) 'global)
                        (eq? (cdr binding) loc))
                   (loc-in-env? (cdr ls)))))))
    (let f ([ls ((current-library-collection))])
      (cond
        [(null? ls) #f]
        [(loc-in-env? (library-env (car ls))) (car ls)]
        [else (f (cdr ls))])))

  (define (invoke-library lib)
    (let ([invoke (library-invoke-state lib)])
      (when (procedure? invoke)
        (set-library-invoke-state! lib 
          (lambda () (error 'invoke "circularity detected for ~s" lib)))
        (for-each invoke-library (library-inv* lib))
        (set-library-invoke-state! lib 
          (lambda () (error 'invoke "first invoke did not return for ~s" lib)))
        (invoke)
        (set-library-invoke-state! lib #t))))

  (define (invoke-library-by-spec spec)
    (invoke-library (find-library-by-spec/die spec)))


  (define installed-libraries 
    (lambda () ((current-library-collection))))
  (define library-subst/env
    (lambda (x) 
      (unless (library? x)
        (error 'library-subst/env "~s is not a library" x))
      (values (library-subst x) (library-env x))))
  (define library-spec       
    (lambda (x) 
      (unless (library? x)
        (error 'library-spec "~s is not a library" x))
      (list (library-id x) (library-name x) (library-ver x))))

  ;;; init
  (let ([subst
         (map (lambda (x) (cons (car x) (cadr x))) scheme-env)]
        [env 
         (map (lambda (x) (cons (cadr x) (caddr x))) scheme-env)])
    (install-library 'scheme-id   ;;; id
                     '(scheme)    ;;; name
                     '()          ;;; version
                     '() '() '()  ;;; req
                     subst env
                     void void))

  ((record-field-mutator (record-type-descriptor (type-descriptor library)) 'printer)
    (type-descriptor library)
    (lambda (x p)
      (unless (library? x)
        (error 'record-type-printer "not a library"))
      (display 
        (format "#<library ~s>" (append (library-name x) (library-ver x)))
        p)))

  )

