;;;; usb.scm
;;;; Bindings to libusb

(module usb
  (usb-find-devices
   usb-first-bus
   usb-each-bus
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

(define-record-type usb-bus
  (usb-wrap-bus context)
  usb-bus?
  (context usb-unwrap-bus))

(define (usb-each-bus cb)
  (let loop ((bus (usb-first-bus)))
    (if bus (begin (cb bus) (loop (usb-next-bus bus))))))

(define (usb-next-bus bus)
  (let ((bus (usb_bus->next (usb-unwrap-bus bus))))
    (if bus (usb-wrap-bus bus) #f)))

(define usb-find-devices (foreign-lambda int "usb_find_devices"))
(define (usb-first-bus) (usb-wrap-bus (usb_get_busses)))

(define usb_bus->next
 (foreign-lambda*
   (c-pointer usb_bus) ((c-pointer bus))
   "C_return(((struct usb_bus *)bus)->next);\n"))

(define usb_get_busses (foreign-lambda (c-pointer usb_bus) "usb_get_busses"))
)
