;;;; usb.scm
;;;; Bindings to libusb

(module usb
  (usb-find-devices)

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

(define usb-find-devices (foreign-lambda int "usb_find_devices"))
)
