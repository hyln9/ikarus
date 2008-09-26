#!/usr/bin/env ikarus --r6rs-script

(import (ikarus) (objc) (Cocoa))

(define pool [$ [$ NSAutoreleasePool alloc] init])

[$ NSApplication sharedApplication]

(define style
  (bitwise-ior 
    NSTitledWindowMask
    NSClosableWindowMask 
    NSResizableWindowMask
    NSMiniaturizableWindowMask))

(define backing NSBackingStoreBuffered)

(define win 
  [$ [$ NSWindow alloc]
     initWithContentRect: '#(#(50 50) #(600 400))
     styleMask: style
     backing: backing
     defer: #f])

(define (nsstring x)
  [$ [$ NSString alloc]
     initWithCharactersNoCopy: x
     length: (string-length x)
     freeWhenDone: #t])

[$ win setTitle: (nsstring "Hello Ikarus")]

[$ win makeKeyAndOrderFront: win]

[$ NSApp run]
[$ pool release]
