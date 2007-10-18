
(library (ikarus command-line)
  (export command-line-arguments command-line)
  (import 
    (ikarus system $arg-list)
    (except (ikarus) command-line command-line-arguments))

  (define (command-line) (command-line-arguments))
  (define command-line-arguments
    (make-parameter ($arg-list)
      (lambda (x)
        (if (and (list? x) (andmap string? x))
            x
            (error 'command-list 
              "invalid command-line-arguments ~s\n" x))))))

