#!/Users/aghuloum/.opt/bin/ikarus --script

(define get-comments
  (lambda (file-name)
    (define comments '())
    (parameterize ([comment-handler
                    (lambda (str)
                      (set! comments (cons str comments)))])
      (load file-name (lambda (x) (void))))
    (reverse comments)))

(define filter
  (lambda (f ls)
    (cond
      [(null? ls) '()]
      [(f (car ls)) (cons (car ls) (filter f (cdr ls)))]
      [else (filter f (cdr ls))])))

(define matcher
  (lambda (tok)
    (let ([n (string-length tok)])
      (lambda (str)
        (and (fx>= (string-length str) n)
             (string=? (substring str 0 n) tok))))))

(define get-matching
  (lambda (token file-name)
    (filter (matcher token) (get-comments file-name))))

#|Usage:
    find.ss token files ...
  Prints all the comments matching Hash-Bar-token that are found 
  in the files.
|#
(define usage
  (lambda (me?)
    (printf "Usage: ~a token files ...\n" (or me? "find"))
    (printf " Prints all multiline comments matching #|token<comment>|# from the files\n")
    (exit 0)))

(define print-comments
  (lambda (file ls)
    (unless (null? ls)
      (printf "From ~a:\n" file)
      (for-each 
        (lambda (x)
          (display x)
          (newline))
        ls))))

(let ([args (command-line-arguments)])
  (when (null? args) (usage #f))
  (let ([me (car args)] [args (cdr args)])
    (when (null? args) (usage me))
    (let ([tok (car args)] [files (cdr args)])
      (for-each
        (lambda (file)
          (print-comments file (get-matching tok file)))
        files))))
