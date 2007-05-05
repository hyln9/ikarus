
(library (ikarus io-primitives)
  (export read-char unread-char peek-char write-char newline
          port-name input-port-name output-port-name
          close-input-port reset-input-port! 
          flush-output-port close-output-port)
  (import 
    (only (scheme) $write-char $flush-output-port $port-handler
          $read-char $unread-char $peek-char $write-char
          $reset-input-port! $close-input-port $close-output-port)
    (except (ikarus) read-char unread-char peek-char write-char
            newline port-name input-port-name output-port-name
            close-input-port reset-input-port!  flush-output-port
            close-output-port))

  (define write-char
    (case-lambda
      [(c)
       (if (char? c)
           ($write-char c (current-output-port))
           (error 'write-char "~s is not a character" c))]
      [(c p)
       (if (char? c)
           (if (output-port? p)
               ($write-char c p)
               (error 'write-char "~s is not an output-port" p))
           (error 'write-char "~s is not a character" c))]))
  ;;;
  (define newline
    (case-lambda
      [() 
       ($write-char #\newline (current-output-port))
       ($flush-output-port (current-output-port))]
      [(p) 
       (if (output-port? p)
           (begin
             ($write-char #\newline p)
             ($flush-output-port p))
           (error 'newline "~s is not an output port" p))]))
  ;;;
  (define port-name
    (lambda (p)
      (if (port? p) 
          (($port-handler p) 'port-name p)
          (error 'port-name "~s is not a port" p))))

  (define input-port-name
    (lambda (p)
      (if (port? p) 
          (($port-handler p) 'port-name p)
          (error 'input-port-name "~s is not a port" p))))

  (define output-port-name
    (lambda (p)
      (if (port? p) 
          (($port-handler p) 'port-name p)
          (error 'output-port-name "~s is not a port" p))))

  (define read-char
    (case-lambda
      [() ($read-char *current-input-port*)]
      [(p)
       (if (input-port? p)
           ($read-char p)
           (error 'read-char "~s is not an input-port" p))]))
  ;;;
  (define unread-char
    (case-lambda
      [(c) (if (char? c)
               ($unread-char c (current-input-port))
               (error 'unread-char "~s is not a character" c))]
      [(c p)
       (if (input-port? p)
           (if (char? c)
               ($unread-char c p)
               (error 'unread-char "~s is not a character" c))
           (error 'unread-char "~s is not an input-port" p))]))
  ;;;
  (define peek-char
    (case-lambda
      [() ($peek-char (current-input-port))]
      [(p)
       (if (input-port? p)
           ($peek-char p)
           (error 'peek-char "~s is not an input-port" p))]))
  ;;;
  (define reset-input-port!
    (case-lambda
      [() ($reset-input-port! (current-input-port))]
      [(p)
       (if (input-port? p)
           ($reset-input-port! p)
           (error 'reset-input-port! "~s is not an input-port" p))]))
  ;;;
  (define close-input-port
    (case-lambda
      [() ($close-input-port (current-input-port))]
      [(p)
       (if (input-port? p)
           ($close-input-port p)
           (error 'close-input-port! "~s is not an input-port" p))]))
  ;;;
  (define close-output-port
    (case-lambda
      [() ($close-output-port (current-output-port))]
      [(p)
       (if (output-port? p)
           ($close-output-port p)
           (error 'close-output-port "~s is not an output-port" p))]))
  ;;;
  (define flush-output-port
    (case-lambda
      [() ($flush-output-port (current-output-port))]
      [(p)
       (if (output-port? p)
           ($flush-output-port p)
           (error 'flush-output-port "~s is not an output-port" p))])))

