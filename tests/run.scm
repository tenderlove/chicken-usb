(use usb test)

(test-begin "usb")

(test-group "USB"
  (test-assert (< 0 (length (usb-devices (usb-init)))))
  (test-assert (usb-init))
  (test-assert (usb-set-debug! (usb-init) 3))
)

(test-end)
(test-exit)
