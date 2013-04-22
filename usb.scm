;;;; usb.scm
;;;; Bindings to libusb

(module usb
  (usb-devices
   usb-make-context
   usb::request-type-standard
   usb::request-type-class
   usb::request-type-vendor
   usb::request-type-reserved
   usb::recipient-device
   usb::recipient-interface
   usb::recipient-endpoint
   usb::recipient-other
   usb::endpoint-out
   usb::endpoint-in
   usb::request-get-status
   usb::request-clear-feature
   usb::request-set-feature
   usb::request-set-address
   usb::request-get-descriptor
   usb::request-set-descriptor
   usb::request-get-configuration
   usb::request-set-configuration
   usb::request-get-interface
   usb::request-set-interface
   usb::request-synch-frame
   usb-set-debug!
   usb-open
   usb-control-transfer
   usb-device-descriptor
   usb-device-bus-number
   usb-device-address
   usb-claim-interface!
   usb-device.idVendor
   usb-device.idProduct
   usb-device.iSerialNumber
   usb-device.iManufacturer
   usb-release-interface!)

(import scheme chicken foreign ports)

(foreign-declare "#include <libusb.h>")

;; constants

(define usb::request-get-status
  (foreign-value "LIBUSB_REQUEST_GET_STATUS" int))
(define usb::request-clear-feature
  (foreign-value "LIBUSB_REQUEST_CLEAR_FEATURE" int))
(define usb::request-set-feature
  (foreign-value "LIBUSB_REQUEST_SET_FEATURE" int))
(define usb::request-set-address
  (foreign-value "LIBUSB_REQUEST_SET_ADDRESS" int))
(define usb::request-get-descriptor
  (foreign-value "LIBUSB_REQUEST_GET_DESCRIPTOR" int))
(define usb::request-set-descriptor
  (foreign-value "LIBUSB_REQUEST_SET_DESCRIPTOR" int))
(define usb::request-get-configuration
  (foreign-value "LIBUSB_REQUEST_GET_CONFIGURATION" int))
(define usb::request-set-configuration
  (foreign-value "LIBUSB_REQUEST_SET_CONFIGURATION" int))
(define usb::request-get-interface
  (foreign-value "LIBUSB_REQUEST_GET_INTERFACE" int))
(define usb::request-set-interface
  (foreign-value "LIBUSB_REQUEST_SET_INTERFACE" int))
(define usb::request-synch-frame
  (foreign-value "LIBUSB_REQUEST_SYNCH_FRAME" int))

(define usb::request-type-standard
  (foreign-value "LIBUSB_REQUEST_TYPE_STANDARD" int))
(define usb::request-type-class
  (foreign-value "LIBUSB_REQUEST_TYPE_CLASS" int))
(define usb::request-type-vendor
  (foreign-value "LIBUSB_REQUEST_TYPE_VENDOR" int))
(define usb::request-type-reserved
  (foreign-value "LIBUSB_REQUEST_TYPE_RESERVED" int))

(define usb::recipient-device
  (foreign-value "LIBUSB_RECIPIENT_DEVICE" int))
(define usb::recipient-interface
  (foreign-value "LIBUSB_RECIPIENT_INTERFACE" int))
(define usb::recipient-endpoint
  (foreign-value "LIBUSB_RECIPIENT_ENDPOINT" int))
(define usb::recipient-other
  (foreign-value "LIBUSB_RECIPIENT_OTHER" int))

(define usb::endpoint-out
  (foreign-value "LIBUSB_ENDPOINT_OUT" int))
(define usb::endpoint-in
  (foreign-value "LIBUSB_ENDPOINT_IN" int))

(define-foreign-type libusb_device (c-pointer "libusb_device"))
(define-foreign-type libusb_device_handle (c-pointer "libusb_device_handle"))
(define-foreign-type libusb_context (c-pointer "libusb_context"))

;; context

(define-record-type usb-context
  (usb-wrap-context context)
  usb-context
  (context usb-unwrap-context))

(define (usb-make-context)
  (set-finalizer!
    (usb-wrap-context (libusb_init)) usb-exit))

(define (usb-exit ctx) (libusb_exit (usb-unwrap-context ctx)))
(define (usb-set-debug! ctx v) (libusb_set_debug (usb-unwrap-context ctx) v))

;; handle

(define-record-type usb-handle
  (usb-wrap-handle handle dev)
  usb-handle?
  (handle usb-unwrap-handle)
  (dev usb-unwrap-handle-dev))

(define (usb-open dev)
  (usb-wrap-handle (libusb_open (usb-unwrap-device dev)) dev))

(define (usb-claim-interface! handle)
  (let ((ret (libusb_claim_interface (usb-unwrap-handle handle) 0)))
    ; FIXME: we should probably raise an exception
    (if (= 0 ret) #t #f)))

(define (usb-release-interface! handle)
  (let ((ret (libusb_release_interface (usb-unwrap-handle handle) 0)))
    ; FIXME: we should probably raise an exception
    (if (= 0 ret) #t #f)))

(define (usb-control-transfer dev requesttype request value index bytes timeout)
  (let ((size (string-length bytes)))
  (libusb_control_transfer (usb-unwrap-handle dev)
                   requesttype
                   request
                   value
                   index
                   bytes
                   size
                   timeout)))

;; devices

(define-record-type usb-device
  (usb-wrap-device dev ctx)
  usb-device?
  (dev usb-unwrap-device)
  (ctx usb-unwrap-device-ctx))

(define (usb-devices ctx)
        (map (lambda (dev)
               (set-finalizer! dev libusb_unref_device)
               (usb-wrap-device dev ctx))
             (libusb_get_device_list (usb-unwrap-context ctx) '())))

(define (usb-device-bus-number dev)
  (libusb_get_bus_number (usb-unwrap-device dev)))

(define (usb-device-address dev)
  (libusb_get_device_address (usb-unwrap-device dev)))

(define-record-type usb-device-descriptor
  (usb-make-device-descriptor bLength
                              bDescriptorType
                              bcdUSB
                              bDeviceClass
                              bDeviceSubClass
                              bDeviceProtocol
                              bMaxPacketSize0
                              idVendor
                              idProduct
                              bcdDevice
                              iManufacturer
                              iProduct
                              iSerialNumber
                              bNumConfigurations)
  usb-device-descriptor?
  (bLength            usb-device.bLength)
  (bDescriptorType    usb-device.bDescriptorType)
  (bcdUSB             usb-device.bcdUSB)
  (bDeviceClass       usb-device.bDeviceClass)
  (bDeviceSubClass    usb-device.bDeviceSubClass)
  (bDeviceProtocol    usb-device.bDeviceProtocol)
  (bMaxPacketSize0    usb-device.bMaxPacketSize0)
  (idVendor           usb-device.idVendor)
  (idProduct          usb-device.idProduct)
  (bcdDevice          usb-device.bcdDevice)
  (iManufacturer      usb-device.iManufacturer)
  (iProduct           usb-device.iProduct)
  (iSerialNumber      usb-device.iSerialNumber)
  (bNumConfigurations usb-device.bNumConfigurations))

(define (usb-device-descriptor dev)
  (libusb_get_device_descriptor (usb-unwrap-device dev)
                                usb-make-device-descriptor))

;; C integration

;; devices
(define libusb_get_device_list (foreign-safe-lambda* scheme-object
                                                     ((c-pointer ctx)
                                                      (scheme-object seed))
"
ssize_t count;
ssize_t i;
libusb_device ** devices;
count = libusb_get_device_list(ctx, &devices);
for(i = 0; i < count; i++) {
  libusb_device *device = devices[i];
  C_word *_pair = C_alloc(C_SIZEOF_PAIR);
  C_word *a = C_alloc(C_SIZEOF_POINTER);
  C_word ptr = C_mpointer(&a, device);
  seed = C_pair(&_pair, ptr, seed);
}
libusb_free_device_list(devices, 0);
C_return(seed);
"))

(define libusb_open (foreign-lambda*
                   (c-pointer libusb_device_handle) ((c-pointer dev))
"
libusb_device_handle * handle;
if (!libusb_open(dev, &handle)) {
  C_return(handle);
} else {
  C_return(C_SCHEME_FALSE);
}
"))

(define libusb_unref_device (foreign-lambda void
                                            "libusb_unref_device"
                                            libusb_device))

(define libusb_claim_interface (foreign-lambda int
                                               "libusb_claim_interface"
                                               libusb_device_handle
                                               int))

(define libusb_release_interface (foreign-lambda int
                                                 "libusb_release_interface"
                                                 libusb_device_handle
                                                 int))

(define libusb_init (foreign-lambda*
                   c-pointer ()
                   "libusb_context * ctx;\n"
                   "libusb_init(&ctx);\n"
                   "C_return(ctx);\n"))

(define libusb_exit (foreign-lambda void
                                    "libusb_exit"
                                    libusb_context))

(define libusb_set_debug (foreign-lambda void
                                         "libusb_set_debug"
                                         libusb_context
                                         int))

(define libusb_get_device_descriptor (foreign-safe-lambda*
                                       scheme-object
                                       ((libusb_device dev)
                                        (scheme-object init))
"
struct libusb_device_descriptor desc;
if (!libusb_get_device_descriptor(dev, &desc)) {
  C_save(C_fix(desc.bLength));
  C_save(C_fix(desc.bDescriptorType));
  C_save(C_fix(desc.bcdUSB));
  C_save(C_fix(desc.bDeviceClass));
  C_save(C_fix(desc.bDeviceSubClass));
  C_save(C_fix(desc.bDeviceProtocol));
  C_save(C_fix(desc.bMaxPacketSize0));
  C_save(C_fix(desc.idVendor));
  C_save(C_fix(desc.idProduct));
  C_save(C_fix(desc.bcdDevice));
  C_save(C_fix(desc.iManufacturer));
  C_save(C_fix(desc.iProduct));
  C_save(C_fix(desc.iSerialNumber));
  C_save(C_fix(desc.bNumConfigurations));
  C_return(C_callback(init, 14));
} else {
  C_return(C_SCHEME_FALSE);
}
"))

(define libusb_get_bus_number (foreign-lambda int
                                              "libusb_get_bus_number"
                                              libusb_device))

(define libusb_get_device_address (foreign-lambda int
                                               "libusb_get_device_address"
                                               libusb_device))

;; devices
(define libusb_control_transfer (foreign-lambda
                          int
                          "libusb_control_transfer"
                          libusb_device_handle
                          int
                          int
                          int
                          int
                          scheme-pointer
                          int
                          int))
)
