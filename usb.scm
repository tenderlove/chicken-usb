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

(define (usb-each-bus cb)
  (let loop ((bus (usb-first-bus)))
    (if bus (begin (cb bus) (loop (usb-next-bus bus))))))

(define usb-next-bus
 (foreign-lambda*
   (c-pointer usb_bus) ((c-pointer bus))
   "C_return(((struct usb_bus *)bus)->next);\n"))
(define usb-find-devices (foreign-lambda int "usb_find_devices"))
(define usb-first-bus (foreign-lambda (c-pointer usb_bus) "usb_get_busses"))
)
