;;;; usb.scm
;;;; Bindings to libusb

(module usb
  (usb-find-devices
   usb-first-bus
   usb-each-bus
   usb-bus-each-device
   usb-device-idVendor
   usb-device-idProduct
   usb-next-bus)

(import scheme chicken foreign ports)

#>
#include <usb.h>
#include <errno.h>
<#

(foreign-code #<<EOF
usb_init();
usb_find_busses();
usb_find_devices();
EOF
)

;;; support routines

;; devices

(define-record-type usb-device
  (usb-wrap-device context)
  usb-device?
  (context usb-unwrap-device))

(define (usb-bus-first-device bus)
  (let ((dev (usb_bus->devices (usb-unwrap-bus bus))))
    (if dev (usb-wrap-device dev) #f)))

(define (usb-bus-each-device cb bus)
  (let loop ((dev (usb-bus-first-device bus)))
    (if dev (begin (cb dev) (loop (usb-next-device dev))))))

(define (usb-next-device dev)
  (let ((next (usb_device->next (usb-unwrap-device dev))))
    (if next (usb-wrap-device next) #f)))

(define (usb-device-idVendor dev)
  (usb_device->idVendor (usb-unwrap-device dev)))

(define (usb-device-idProduct dev)
  (usb_device->idProduct (usb-unwrap-device dev)))

;; busses

(define-record-type usb-bus
  (usb-wrap-bus context)
  usb-bus?
  (context usb-unwrap-bus))

(define (usb-each-bus cb)
  (let loop ((bus (usb-first-bus)))
    (if bus (begin (cb bus) (loop (usb-next-bus bus))) #f)))

(define (usb-next-bus bus)
  (let ((bus (usb_bus->next (usb-unwrap-bus bus))))
    (if bus (usb-wrap-bus bus) #f)))

(define usb-find-devices (foreign-lambda int "usb_find_devices"))
(define (usb-first-bus) (usb-wrap-bus (usb_get_busses)))

;; C integration

;; devices
(define usb_device->next
 (foreign-lambda*
   (c-pointer usb_device) ((c-pointer dev))
   "C_return(((struct usb_device *)dev)->next);\n"))

(define usb_device->idVendor
 (foreign-lambda*
   int ((c-pointer dev))
   "C_return(((struct usb_device *)dev)->descriptor.idVendor);\n"))

(define usb_device->idProduct
 (foreign-lambda*
   int ((c-pointer dev))
   "C_return(((struct usb_device *)dev)->descriptor.idProduct);\n"))

;; busses
(define usb_bus->next
 (foreign-lambda*
   (c-pointer usb_bus) ((c-pointer bus))
   "C_return(((struct usb_bus *)bus)->next);\n"))

(define usb_bus->devices
 (foreign-lambda*
   (c-pointer usb_device) ((c-pointer bus))
   "C_return(((struct usb_bus *)bus)->devices);\n"))

(define usb_get_busses (foreign-lambda (c-pointer usb_bus) "usb_get_busses"))
)
