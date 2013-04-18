(use usb test)

(test-begin "usb")

(test-group "handle"
  (test-assert (usb-init))
  (test-assert (usb-set-debug! (usb-init) 3))
)

(define handle (usb-init))

(test-group "with handle"
  (test-assert (< 0 (length (usb-devices handle))))
  (test-assert (usb-open (car (usb-devices handle))))
  (test-assert (map usb-device-descriptor (usb-devices handle)))
  (test-assert (map usb-device-descriptor.idVendor
                    (map usb-device-descriptor (usb-devices handle))))
  (test-assert (map usb-device-descriptor.idProduct
                    (map usb-device-descriptor (usb-devices handle))))
)

(test-end)
(test-exit)
