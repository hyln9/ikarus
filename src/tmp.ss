(expand '(parameterize ([foo bar] [quux klan]) 1 2))

((lambda (g0 g1 g2 g3)
   ((lambda (g4) 
      (#%dynamic-wind g4 (lambda () (begin '1 '2)) g4))
    (lambda () 
      (begin
        ((lambda (g5) (begin (g0 g2) (set! g2 g5))) (g0))
        ((lambda (g6) (begin (g1 g3) (set! g3 g6))) (g1))))))
 (top-level-value 'foo) 
 (top-level-value 'quux)
 (top-level-value 'bar)
 (top-level-value 'klan))

       


(expand '(parameterize ([foo bar] [quux klan]) 1 2)) 
((lambda (g0 g1 g2 g3)
   ((lambda (swap)
      (#2%dynamic-wind #t swap (lambda () 1 2) swap))
     (lambda ()
       ((lambda (t) (g0 g2) (set! g2 t)) (g0))
       ((lambda (t) (g1 g3) (set! g3 t)) (g1)))))
  foo
  quux
  bar
  klan)
