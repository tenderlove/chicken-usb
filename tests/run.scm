(use usb test)

(test-begin "usb")

(test-group "USB"
  ; (test-assert (< 0 (length (usb-busses))))
  ; (test-assert (< 0 (length (flatten (map usb-devices (usb-busses))))))
  (test-assert (usb-init))
  (test-assert (usb-set-debug! (usb-init) 3))
)

(test-end)
(test-exit)
