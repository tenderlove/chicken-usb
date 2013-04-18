(use usb test)

(test-begin "usb")

(test-group "handle"
  (test-assert (usb-init))
  (test-assert (usb-set-debug! (usb-init) 3))
)

(define handle (usb-init))
(define devices (usb-devices handle))

(test-group "devices"
  (test-assert (< 0 (length devices)))
  (test-assert (usb-open (car devices)))
  (test-assert (map usb-device-descriptor devices))
  (test-assert (map usb-device-descriptor.idVendor
                    (map usb-device-descriptor devices)))
  (test-assert (map usb-device-descriptor.idProduct
                    (map usb-device-descriptor devices)))
  (test-assert (map usb-device-bus-number devices))
  (test-assert (map usb-device-address devices))
)

(test-end)
(test-exit)
