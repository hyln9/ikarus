(library (tests reader)
  (export test-reader test-char-syntax)
  (import (ikarus) (tests framework))

  (define t
    (lambda (str)
      (lambda (n?)
        (and (number? n?)
             (= (with-input-from-string str read) n?)))))

  (define-syntax reader-tests
    (syntax-rules ()
      [(_ name str* ...)
       (define-tests name 
         [(t str*) (string->number str*)] ...)]))
        
  (reader-tests test-reader
    "12"
    "+12"
    "3427384783264876238746784234"
    "0"
    "+0"
    "-12"
    "-3498738947983748939478347834"
    "-0"
    "#x-238973897AAAAAFFFFbb00bbdddcc"
    "#x238973897AAAAA000FFFFbbbbdddcc"
    "#x+07edf387"
    "#x+0"
    "#x-0"
    "#x0"
    "#b01010101010000000111111111110000"
    "#b-01010101010000000111111111110000"
    "#b+01010101010000000111111111110000"
    "#b+0"
    "#b-0"
    "#b0"
    "#d2398128321308912830912830912839"
    "#d-2398128321308912830912830912839"
    "#d+2398128321308912830912830912839"
    "#d+0"
    "#d-0"
    "#d0"
    "#o237612036721631263126371263712"
    "#o-2376120036721631263126371263712"
    "#o+23761236721631263126371263712"
    "#o+0"
    "#o-0"
    "#o0"
    
    "#X-238973897AAAAAFFFFbb00bbdddcc"
    "#X238973897AAAAA000FFFFbbbbdddcc"
    "#X+07edf387"
    "#X+0"
    "#X-0"
    "#X0"
    "#B01010101010000000111111111110000"
    "#B-01010101010000000111111111110000"
    "#B+01010101010000000111111111110000"
    "#B+0"
    "#B-0"
    "#B0"
    "#D2398128321308912830912830912839"
    "#D-2398128321308912830912830912839"
    "#D+2398128321308912830912830912839"
    "#D+0"
    "#D-0"
    "#D0"
    "#O237612036721631263126371263712"
    "#O-2376120036721631263126371263712"
    "#O+23761236721631263126371263712"
    "#O+0"
    "#O-0"
    "#O0")
  
  (define-tests test-char-syntax
    [(lambda (x) (= (char->integer x) #x0)) 
     (read (open-input-string "#\\nul"))]
    [(lambda (x) (= (char->integer x) #x7)) 
     (read (open-input-string "#\\alarm"))]
    [(lambda (x) (= (char->integer x) #x8)) 
     (read (open-input-string "#\\backspace"))]
    [(lambda (x) (= (char->integer x) #x9)) 
     (read (open-input-string "#\\tab"))]
    [(lambda (x) (= (char->integer x) #xA)) 
     (read (open-input-string "#\\linefeed"))]
    [(lambda (x) (= (char->integer x) #xA)) 
     (read (open-input-string "#\\newline"))]
    [(lambda (x) (= (char->integer x) #xB)) 
     (read (open-input-string "#\\vtab"))]
    [(lambda (x) (= (char->integer x) #xC)) 
     (read (open-input-string "#\\page"))]
    [(lambda (x) (= (char->integer x) #xD)) 
     (read (open-input-string "#\\return"))]
    [(lambda (x) (= (char->integer x) #x1B)) 
     (read (open-input-string "#\\esc"))]
    [(lambda (x) (= (char->integer x) #x20)) 
     (read (open-input-string "#\\space"))]
    [(lambda (x) (= (char->integer x) #x7F)) 
     (read (open-input-string "#\\delete"))])

  )

