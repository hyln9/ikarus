#!/usr/bin/env ikarus --r6rs-script
;;; vim:syntax=scheme
(import (ikarus) (objc))

(define-framework Cocoa)
(define-class NSAutoreleasePool)
(define-class NSWindow)
(define-class NSApplication)
(define-object NSApp Cocoa)


(define pool [$ [$ NSAutoreleasePool alloc] init])
[$ NSApplication sharedApplication]

(define NSBorderlessWindowMask         #b000000000)
(define NSTitledWindowMask             #b000000001)
(define NSClosableWindowMask           #b000000010)
(define NSMiniaturizableWindowMask     #b000000100)
(define NSResizableWindowMask          #b000001000)
(define NSTexturedBackgroundWindowMask #b100000000)

(define NSBackingStoreRetained     0)
(define NSBackingStoreNonretained  1)
(define NSBackingStoreBuffered     2)

(define style 
  (bitwise-ior 
    NSClosableWindowMask 
    NSResizableWindowMask
    NSTexturedBackgroundWindowMask
    NSTitledWindowMask
    NSMiniaturizableWindowMask))

(define backing NSBackingStoreBuffered)

(define win [$ [$ NSWindow alloc]
               initWithContentRect: '#(#(50 50) #(600 400))
               styleMask: style
               backing: backing
               defer: #f])

[$ win makeKeyAndOrderFront: win]

[$ NSApp run]
[$ pool release]


