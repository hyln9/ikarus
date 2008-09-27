
(library (Cocoa)
  (export) ; below
  (import (ikarus) (objc))

  (define-syntax define-and-export
    (syntax-rules ()
      [(_ (def* name* . rest*) ...)
       (begin
         (def* name* . rest*) ...
         (export name* ...))]))

  (define-and-export
    (define-framework Cocoa)
    (define-class NSAutoreleasePool)
    (define-class NSWindow)
    (define-class NSApplication)
    (define-class NSString)
    (define-class NSMenu)
    (define-class NSMenuItem)
    (define-object NSApp Cocoa)
    
    (define NSBorderlessWindowMask         #b000000000)
    (define NSTitledWindowMask             #b000000001)
    (define NSClosableWindowMask           #b000000010)
    (define NSMiniaturizableWindowMask     #b000000100)
    (define NSResizableWindowMask          #b000001000)
    (define NSTexturedBackgroundWindowMask #b100000000)
    
    (define NSBackingStoreRetained     0)
    (define NSBackingStoreNonretained  1)
    (define NSBackingStoreBuffered     2)
    
    (define NSASCIIStringEncoding 1)   ; /* 0..127 only */
    (define NSNEXTSTEPStringEncoding 2)
    (define NSJapaneseEUCStringEncoding 3)
    (define NSUTF8StringEncoding 4)
    (define NSISOLatin1StringEncoding 5)
    (define NSSymbolStringEncoding 6)
    (define NSNonLossyASCIIStringEncoding 7)
    (define NSShiftJISStringEncoding 8)
    (define NSISOLatin2StringEncoding 9)
    (define NSUnicodeStringEncoding 10)
    (define NSWindowsCP1251StringEncoding 11) ;   /* Cyrillic; same as AdobeStandardCyrillic */
    (define NSWindowsCP1252StringEncoding 12) ;   /* WinLatin1 */
    (define NSWindowsCP1253StringEncoding 13) ;   /* Greek */
    (define NSWindowsCP1254StringEncoding 14) ;   /* Turkish */
    (define NSWindowsCP1250StringEncoding 15) ;   /* WinLatin2 */
    (define NSISO2022JPStringEncoding 21)     ;    /* ISO 2022 Japanese encoding for e-mail */
    (define NSMacOSRomanStringEncoding 30)
    (define NSProprietaryStringEncoding 65536)  ;  /* Installation-specific encoding */
    
    (define NSAlphaShiftKeyMask (sll 1 16))
    (define NSShiftKeyMask (sll 1 17))
    (define NSControlKeyMask (sll 1 18))
    (define NSAlternateKeyMask (sll 1 19))
    (define NSCommandKeyMask (sll 1 20))
    (define NSNumericPadKeyMask (sll 1 21))
    (define NSHelpKeyMask (sll 1 22))
    (define NSFunctionKeyMask (sll 1 23))
    (define NSDeviceIndependentModifierFlagsMask #xffff0000)


  ))




