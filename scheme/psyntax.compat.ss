
(library (psyntax compat)
  (export define-record make-parameter parameterize format gensym
          eval-core symbol-value set-symbol-value!
          file-options-spec make-struct-type)
  (import 
    (only (ikarus compiler) eval-core)
    (ikarus))

  (define-syntax define-record
    (syntax-rules ()
      [(_ name (field* ...) printer)
       (begin
         (define-struct name (field* ...))
         (module ()
            (set-rtd-printer! (type-descriptor name)
              printer)))]
      [(_ name (field* ...))
       (define-struct name (field* ...))])))

