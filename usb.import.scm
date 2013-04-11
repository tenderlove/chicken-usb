;;;; usb.import.scm - GENERATED BY CHICKEN 4.8.0.1 -*- Scheme -*-

(eval '(import scheme chicken foreign ports))
(##sys#register-compiled-module
  'usb
  (list)
  '((usb-find-devices . usb#usb-find-devices)
    (usb-first-bus . usb#usb-first-bus)
    (usb-each-bus . usb#usb-each-bus)
    (usb-bus-each-device . usb#usb-bus-each-device)
    (usb-device-idVendor . usb#usb-device-idVendor)
    (usb-device-idProduct . usb#usb-device-idProduct)
    (usb-next-bus . usb#usb-next-bus))
  (list)
  (list))

;; END OF FILE
