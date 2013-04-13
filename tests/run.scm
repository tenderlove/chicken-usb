(use usb test)

(test-begin "usb")

(test-group "USB"
  (test-assert (< 0 (length (usb-busses))))
  (test-assert (< 0 (length (flatten (map usb-devices (usb-busses))))))
  (test-assert (usb-init))
  (test-assert (usb-exit (usb-init)))
)

(test-end)
(test-exit)
