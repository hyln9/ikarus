
(library (ikarus.apropos)
  (export apropos)
  (import 
    (except (ikarus) apropos)
    (only (psyntax library-manager) library-subst library-name))

  (define (compose f g)
    (lambda (x) (f (g x))))

  (define (match-maker s1)
    (let ([n1 (string-length s1)])
      (lambda (s2)
        (let ([m (fx- (string-length s2) n1)])
          (let f ([i2 0])
            (and (fx<=? i2 m)
                 (or (let g ([i1 0] [i2 i2])
                       (or (fx= i1 n1)
                           (and (char=? (string-ref s1 i1)
                                        (string-ref s2 i2))
                                (g (fx+ i1 1) (fx+ i2 1)))))
                     (f (fx+ i2 1)))))))))

  (define ($apropos-list name who)
    (let ([name
           (cond
             [(string? name) name]
             [(symbol? name) (symbol->string name)]
             [else
              (die who "not a string or symbol" name)])])
      (define matcher 
        (compose (match-maker name) 
          (compose symbol->string car)))
      (fold-right 
        (lambda (lib rest)
          (define (symbol<? s1 s2) 
            (string<? (symbol->string s1) (symbol->string s2)))
          (let ([ls (filter matcher (library-subst lib))])
            (if (null? ls) 
                rest
                (let ([ls (list-sort symbol<? (map car ls))])
                  (cons (cons (library-name lib) ls) rest)))))
        '()
        (list-sort
          (lambda (lib1 lib2)
            (let f ([ls1 (library-name lib1)] [ls2 (library-name lib2)])
              (and (pair? ls2)
                   (or (null? ls1)
                       (let ([s1 (symbol->string (car ls1))]
                             [s2 (symbol->string (car ls2))])
                         (or (string<? s1 s2)
                             (and (string=? s1 s2)
                                  (f (cdr ls1) (cdr ls2)))))))))
          (installed-libraries)))))

  (define (apropos name)
    (for-each 
      (lambda (x) (printf "~a:\n   ~a\n" (car x) (cdr x)))
      ($apropos-list name 'apropos))))


