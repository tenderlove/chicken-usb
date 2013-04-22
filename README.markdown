# libusb bindings for Chicken scheme

This egg exposes libusb to Chicken!  It is an extremely low level interface,
so you should know the API for [libusb](http://libusb.sourceforge.net/api-1.0/modules.html)
before proceeding too far.  You shouldn't need to do any memory management,
but you should know how to use libusb.

## Synopsis

```scheme
(use usb srfi-1)

; Create a context
(define usb-ctx (usb-make-context))

; Find a list of devices you want to interact with
(define devices (filter (lambda (dev)
                          (let ((desc (usb-device-descriptor dev)))
                            (and (= #x27B8 (usb-device.idVendor desc))
                                 (= #x01ED (usb-device.idProduct desc)))))
                        (usb-devices usb-ctx)))


; Open a handle
(define handle (usb-open (car devices)))

; Claim the device so we can write to it
(usb-claim-interface! handle)

; Prepare some data to write
(define (pack buf)
  (with-output-to-string (lambda ()
      (for-each (lambda (byte) (write-char (integer->char byte))) buf))))

(define write-request (bitwise-ior usb::request-type-class
                                   usb::recipient-interface
                                   usb::endpoint-out))
(define set-report #x09)
(define value 769)
(define interface-number 0)
(define timeout 1000)
(define payload (pack (list 1 #x6e 255 0 0 0 0 0 0)))

; Write the data
(usb-control-transfer handle
                      write-request
                      set-report
                      value
                      interface-number
                      payload
                      timeout)

; Release the handle
(usb-release-interface! handle)

; Close the handle
(usb-close handle)
```

## Current problems

Currently this does not wrap the Async I/O API or the Polling and timing API.

