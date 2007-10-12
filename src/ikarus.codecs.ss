
(library (ikarus codecs)
  (export latin-1-codec utf-8-codec utf-16-codec native-eol-style
          make-transcoder native-transcoder)
  (import 
    (except (ikarus) latin-1-codec utf-8-codec utf-16-codec 
      native-eol-style make-transcoder native-transcoder)
    (ikarus system $transcoders))
  (define (latin-1-codec) 'latin-1-codec)
  (define (utf-8-codec)   'utf-8-codec)
  (define (utf-16-codec)  'utf-16-codec)
  (define (native-eol-style) 'none)
  
  (define error-handling-mode-alist
    '([ignore .  #b01]
      [raise .   #b10]
      [replace . #b11]))

  (define eol-style-alist
    '([none .   #b00000]
      [lf .     #b00100]
      [cr .     #b01000]
      [crlf .   #b01100]
      [nel .    #b10000]
      [crnel .  #b10100]
      [ls .     #b11000]))

  (define codec-alist
    '([latin-1-codec . #b0100000]
      [utf-8-codec .   #b1000000]
      [utf-16-codec .  #b1100000]))

  (define (codec->fixnum x who)
    (cond
      [(assq x codec-alist) => cdr]
      [else (error who "~s is not a valid coded" x)]))

  (define (eol-style->fixnum x who)
    (cond
      [(assq x eol-style-alist) => cdr]
      [else (error who "~s is not a valid eol-style" x)]))

  (define (error-handling-mode->fixnum x who)
    (cond
      [(assq x error-handling-mode-alist) => cdr]
      [else (error who "~s is not a valid error-handling mode" x)]))

  (define make-transcoder
    (case-lambda
      [(codec eol-style handling-mode) 
       ($data->transcoder 
         (fxior 
           (error-handling-mode->fixnum handling-mode 'make-transcoder)
           (eol-style->fixnum eol-style 'make-transcoder)
           (codec->fixnum codec 'make-transcoder)))]
      [(codec eol-style) 
       (make-transcoder codec eol-style 'replace)]
      [(codec) 
       (make-transcoder codec 'none 'replace)]))

  (define (native-transcoder) 
    (make-transcoder 'utf-8-codec 'none 'replace))

  )

