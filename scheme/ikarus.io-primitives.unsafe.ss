;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus io-primitives unsafe)
  (export $write-char 
          $write-byte
          $read-char
          $get-u8 
          $lookahead-u8
          $peek-char
          $reset-input-port!
          $flush-output-port 
          $close-input-port
          $close-output-port
          )
  (import
    (ikarus)
    (ikarus system $ports)
    (ikarus system $strings)
    (ikarus system $chars)
    (ikarus system $bytevectors)
    (ikarus system $fx))

  (define $write-char
    (lambda (c p)
      (let ([b ($char->fixnum c)])
        (cond
          [($fx<= b #x7F)
           ($write-byte b p)]
          [($fx<= b #x7FF) 
           ($write-byte 
             ($fxlogor #b11000000 ($fxsra b 6)) p)
           ($write-byte
             ($fxlogor #b10000000 ($fxlogand b #b111111)) p)]
          [($fx<= b #xFFFF)
           ($write-byte 
             ($fxlogor #b11100000 ($fxsra b 12)) p)
           ($write-byte 
             ($fxlogor #b10000000 ($fxlogand ($fxsra b 6) #b111111)) p)
           ($write-byte 
             ($fxlogor #b10000000 ($fxlogand b #b111111)) p)]
          [else 
           ($write-byte
             ($fxlogor #b11110000 ($fxsra b 18)) p)
           ($write-byte 
             ($fxlogor #b10000000 ($fxlogand ($fxsra b 12) #b111111)) p)
           ($write-byte 
             ($fxlogor #b10000000 ($fxlogand ($fxsra b 6) #b111111)) p)
           ($write-byte 
             ($fxlogor #b10000000 ($fxlogand b #b111111)) p)]))))

  (define $write-byte
    (lambda (b p)
      (let ([idx (port-output-index p)])
        (if ($fx< idx ($port-size p))
            (begin
              ($bytevector-set! ($port-buffer p) idx b)
              ($set-port-index! p ($fxadd1 idx)))
            (($port-handler p) 'write-byte b p)))))

  (define $read-char
    (lambda (p)
      (let ([idx ($port-index p)])
        (if ($fx< idx ($port-size p))
            (let ([b ($bytevector-u8-ref ($port-buffer p) idx)])
              (cond
                [($fx<= b 127) 
                 ($set-port-index! p ($fxadd1 idx))
                 ($fixnum->char b)]
                [else (($port-handler p) 'read-char p)]))
            (($port-handler p) 'read-char p)))))

  (define $get-u8
    (lambda (p)
      (let ([idx ($port-index p)])
        (if ($fx< idx ($port-size p))
            (let ([b ($bytevector-u8-ref ($port-buffer p) idx)])
              ($set-port-index! p ($fxadd1 idx))
              b)
            (($port-handler p) 'get-u8 p)))))

  (define $lookahead-u8
    (lambda (p)
      (let ([idx ($port-index p)])
        (if ($fx< idx ($port-size p))
            ($bytevector-u8-ref ($port-buffer p) idx)
            (($port-handler p) 'lookahead-u8 p)))))

  (define $peek-char
    (lambda (p)
      (let ([idx ($port-index p)])
        (if ($fx< idx ($port-size p))
            (let ([b ($bytevector-u8-ref ($port-buffer p) idx)])
                (cond
                  [($fx<= b 127) 
                   ($fixnum->char b)]
                  [else (($port-handler p) 'peek-char p)]))
            (($port-handler p) 'peek-char p)))))

  (define $reset-input-port!
    (lambda (p)
      ($set-port-size! p 0)))

  (define $close-input-port
    (lambda (p)
      (($port-handler p) 'close-port p)))

  (define $close-output-port
    (lambda (p)
      (($port-handler p) 'close-port p)))

  (define $flush-output-port
    (lambda (p)
      (($port-handler p) 'flush-output-port p))))
