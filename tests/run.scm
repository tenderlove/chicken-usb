(use usb test)

(test-begin "usb")

(test-group "handle"
  (test-assert (usb-make-context))
  (test-assert (usb-set-debug! (usb-make-context) 3))
)

(define handle (usb-make-context))
(define devices (usb-devices handle))
(define dev-handle (usb-open (car devices)))

(test-group "devices"
  (test-assert (< 0 (length devices)))
  (test-assert dev-handle)
  (test-assert (map usb-device-descriptor devices))
  (test-assert (map usb-device.idVendor
                    (map usb-device-descriptor devices)))
  (test-assert (map usb-device.idProduct
                    (map usb-device-descriptor devices)))
  (test-assert (map usb-device-bus-number devices))
  (test-assert (map usb-device-address devices))
  (test-assert (usb-close dev-handle))
)

(test-end)
(test-exit)
