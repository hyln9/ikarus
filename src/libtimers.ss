
(let ()
  (define-record stats 
    (user-secs user-usecs sys-secs sys-usecs real-secs real-usecs))

  (define (mk-stats)
    (make-stats #f #f #f #f #f #f))

  (define (set-stats! t)
    (foreign-call "ikrt_stats_now" t))

  (define (print-stats message t1 t0)
    (define (print-time msg secs usecs)
      (if (fx< usecs 0)
          (print-time msg (fx- secs 1) (fx+ usecs 1000000))
          (printf "    ~a.~a~a~as ~a time\n"
                  secs
                  (fxremainder (fxquotient usecs 100000) 10)
                  (fxremainder (fxquotient usecs 10000) 10)
                  (fxremainder (fxquotient usecs 1000) 10)
                  msg)))
    (if message
        (printf "running stats for ~a:\n" message)
        (printf "running stats:\n"))
    (print-time "user" 
        (fx- (stats-user-secs t1) (stats-user-secs t0))
        (fx- (stats-user-usecs t1) (stats-user-usecs t0)))
    (print-time "system" 
        (fx- (stats-sys-secs t1) (stats-sys-secs t0))
        (fx- (stats-sys-usecs t1) (stats-sys-usecs t0)))
    (print-time "real" 
        (fx- (stats-real-secs t1) (stats-real-secs t0))
        (fx- (stats-real-usecs t1) (stats-real-usecs t0))))

  (define time-it
    (case-lambda
      [(proc) 
       (time-it proc #f)]
      [(proc message)
       (unless (procedure? proc)
         (error 'time-it "~s is not a procedure" proc))
       (let* ([t1 (mk-stats)]
              [t0 (mk-stats)])
         (set-stats! t0)
         (call-with-values proc
           (case-lambda
             [(v)
              (set-stats! t1)
              (print-stats message t1 t0)
              v]
             [v*
              (set-stats! t1)
              (print-stats message t1 t0)
              (apply values v*)])))]))

  (primitive-set! 'time-it time-it)
  
)
