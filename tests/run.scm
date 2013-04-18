(use usb test)

(test-begin "usb")

(test-group "handle"
  (test-assert (usb-init))
  (test-assert (usb-set-debug! (usb-init) 3))
)

(test-group "with handle"
  (let ((handle (usb-init)))
    (test-assert (< 0 (length (usb-devices handle))))
    (test-assert (usb-open (car (usb-devices handle))))))

(test-end)
(test-exit)
