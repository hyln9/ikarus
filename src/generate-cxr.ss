
(define generate-cxr-definitions
  (lambda ()
    (define gen-body
      (lambda (name arg ls)
        (cond
          [(null? (cdr ls)) 
           `(if (pair? ,arg)
                (,(car ls) ,arg)
                (err ',name orig))]
          [else
           (let ([a (car ls)])
             `(if (pair? ,arg)
                  (let ([x (,a ,arg)])
                    ,(gen-body name 'x (cdr ls)))
                  (err ',name orig)))])))
    (define gen-cxr
      (lambda (name ls)
        `(primitive-set! ',name (lambda (orig) ,(gen-body name 'orig ls)))))
    (define gen-names-n
      (lambda (n)
        (cond
          [(fx= n 0) '(())]
          [else
           (let ([ls (gen-names-n (fx- n 1))])
             (append 
               (map (lambda (x) (cons #\a x)) ls)
               (map (lambda (x) (cons #\d x)) ls)))])))
    (define gen-names
      (lambda (n)
        (cond
          [(fx= n 0) '()]
          [else (append (gen-names (fx- n 1)) (gen-names-n n))])))
    (define ls->name
      (lambda (ls)
        (string->symbol (list->string (append '(#\c) ls '(#\r))))))
    (define ls->functions
      (lambda (ls)
        (reverse 
          (map (lambda (c) (string->symbol (format "$c~ar" c))) ls))))
    `(let ([err
            (lambda (who x)
              (error who "invalid list structure ~s" x))])
       ,@(map
           (lambda (ls) (gen-cxr (ls->name ls) (ls->functions ls)))
           (gen-names 4)))))

(with-output-to-file "libcxr-6.0.ss"
  (lambda ()
    (pretty-print (generate-cxr-definitions)))
  'replace)

