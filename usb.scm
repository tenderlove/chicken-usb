;;;; usb.scm
;;;; Bindings to libusb

(module usb
  (usb-devices
   usb-init
   usb-set-debug!
   usb-open)

(import scheme chicken foreign ports)

#>
#include <usb.h>
#include <libusb.h>
#include <errno.h>
<#

;;; support routines

;; constants

(define usb-type-class (foreign-value "USB_TYPE_CLASS" int))
(define usb-recip-device (foreign-value "USB_RECIP_DEVICE" int))
(define usb-endpoint-out (foreign-value "USB_ENDPOINT_OUT" int))

;; context

(define-record-type usb-context
  (usb-wrap-context context)
  usb-context
  (context usb-unwrap-context))

(define (usb-init)
  (set-finalizer!
    (usb-wrap-context (libusb_init)) usb-exit))

(define (usb-exit ctx) (libusb_exit (usb-unwrap-context ctx)))
(define (usb-set-debug! ctx v) (libusb_set_debug (usb-unwrap-context ctx) v))

;; handle

(define-record-type usb-handle
  (usb-wrap-handle context)
  usb-handle?
  (context usb-unwrap-handle))

(define (usb-open dev)
  (usb-wrap-handle (usb_open (usb-unwrap-device dev))))

(define (usb-control-msg dev requesttype request value index bytes timeout)
  (let ((size (string-length bytes)))
  (usb_control_msg (usb-unwrap-handle dev)
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
        (map (lambda (dev) (usb-wrap-device dev ctx))
             (libusb_get_device_list (usb-unwrap-context ctx) '())))

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

(define usb_device->next
 (foreign-lambda*
   (c-pointer usb_device) ((c-pointer dev))
   "C_return(((struct usb_device *)dev)->next);\n"))

(define usb_device->idVendor
 (foreign-lambda*
   int ((c-pointer dev))
   "C_return(((struct usb_device *)dev)->descriptor.idVendor);\n"))

(define usb_device->idProduct
 (foreign-lambda*
   int ((c-pointer dev))
   "C_return(((struct usb_device *)dev)->descriptor.idProduct);\n"))

(define usb_open (foreign-lambda*
                   (c-pointer usb_dev_handle) ((c-pointer dev))
                   "C_return(usb_open((struct usb_device *)dev));\n"))

(define libusb_init (foreign-lambda*
                   c-pointer ()
                   "libusb_context * ctx;\n"
                   "libusb_init(&ctx);\n"
                   "C_return(ctx);\n"))

(define libusb_exit (foreign-lambda*
                   void ((c-pointer ctx)) "libusb_exit(ctx);\n"))

(define libusb_set_debug (foreign-lambda*
                   void ((c-pointer ctx) (int value))
                                         "libusb_set_debug(ctx, value);\n"))


;; busses
(define usb_bus->next
 (foreign-lambda*
   (c-pointer usb_bus) ((c-pointer bus))
   "C_return(((struct usb_bus *)bus)->next);\n"))

(define usb_bus->devices
 (foreign-lambda*
   (c-pointer usb_device) ((c-pointer bus))
   "C_return(((struct usb_bus *)bus)->devices);\n"))

(define usb_get_busses (foreign-lambda (c-pointer usb_bus) "usb_get_busses"))

;; devices
(define usb_control_msg (foreign-lambda
                          int
                          "usb_control_msg"
                          c-pointer
                          int
                          int
                          int
                          int
                          scheme-pointer
                          int
                          int))
)
