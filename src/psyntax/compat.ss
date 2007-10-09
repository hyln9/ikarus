
(library (psyntax compat)
  (export define-record make-parameter parameterize format)
  (import 
    (rename (ikarus) (define-record sys.define-record)))

  (define-syntax define-record
    (syntax-rules ()
      [(_ name (field* ...) printer)
       (begin
         (sys.define-record name (field* ...))
         (module ()
            (set-rtd-printer! (type-descriptor name)
              printer)))]
      [(_ name (field* ...))
       (sys.define-record name (field* ...))])))

