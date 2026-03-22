;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (nonguix transformations)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix channels)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu system)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services xorg)
  #:use-module (nongnu services nvidia)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)
  #:export (nonguix-transformation-guix
            nonguix-transformation-linux
            nonguix-transformation-nvidia)
  #:re-export (replace-mesa))

(define* (nonguix-transformation-guix #:key (substitutes? #t)
                                      (channel? #t)
                                      (guix-source? #f))
  "Return a procedure that transforms an operating system, setting up Nonguix
signing key for the Guix daemon.

Additionally, SUBSTITUTES? (default: #t) sets up the substitute server,
CHANNEL? (default: #t) adds Nonguix channel specification into
'/etc/guix/channels.scm' and GUIX-SOURCE? (default: #f) builds Nonguix channel
into the default Guix.

FIXME: GUIX-SOURCE? is disabled by default due to performance issue."

  (define %nonguix-signing-key
    (plain-file "nonguix.pub" "
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

  (define %nonguix-channel
    (channel
      (name 'nonguix)
      (url "https://gitlab.com/nonguix/nonguix")
      ;; Enable signature verification:
      (introduction
       (make-channel-introduction
        "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
        (openpgp-fingerprint
         "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

  (lambda (os)
    (operating-system
      (inherit os)
      (services
       (modify-services (operating-system-user-services os)
         (guix-service-type
          config => (guix-configuration
                     (inherit config)
                     (channels
                      (let ((configured-channels
                             (guix-configuration-channels config)))
                        (if channel?
                            (cons %nonguix-channel
                                  (or configured-channels %default-channels))
                            configured-channels)))
                     (guix
                      (if guix-source?
                          (guix-for-channels channels)
                          (guix-configuration-guix config)))
                     (authorized-keys
                      (cons %nonguix-signing-key
                            (guix-configuration-authorized-keys config)))
                     (substitute-urls
                      (delete-duplicates
                       `(,@(guix-configuration-substitute-urls config)
                         ,@(if substitutes?
                               '("https://substitutes.nonguix.org")
                               '())))))))))))

(define* (nonguix-transformation-linux #:key (linux linux)
                                       (firmware (list linux-firmware))
                                       (initrd microcode-initrd))
  "Return a procedure that transforms an operating system, setting up
LINUX (default: linux) kernel, with FIRMWARE (default: (list linux-firmware))
and INITRD (default: microcode-initrd)."
  (lambda (os)
    (operating-system
      (inherit os)
      (kernel linux)
      (firmware
       (delete-duplicates
        (append firmware
                (operating-system-firmware os))))
      (initrd initrd))))

(define* (nonguix-transformation-nvidia #:key (driver nvda)
                                        (open-source-kernel-module? #f)
                                        (kernel-mode-setting? #t)
                                        (configure-xorg? #f)
                                        ;; Deprecated.
                                        (s0ix-power-management? #f))
  "Return a procedure that transforms an operating system, setting up DRIVER
(default: nvda) for NVIDIA GPU.

OPEN-SOURCE-KERNEL-MODULE? (default: #f) is supported since Turing and required
since Blackwell.

KERNEL-MODE-SETTING? (default: #t) is required for Wayland and rootless Xorg
support.

CONFIGURE-XORG? (default: #f) is required for Xorg display managers.  It accepts
a display manager service type, or #t when using '%desktop-services'."

  (define %driver
    (if (member driver
                (list nvda-beta
                      nvda-590
                      nvda-580
                      nvda-470
                      nvda-390))
        driver
        (leave (G_ "'~a': no driver configuration available for '~a'~%")
               "nonguix-transformation-nvidia"
               driver)))

  (define %firmware
    (assoc-ref
     `((,nvda-beta . ,nvidia-firmware-beta)
       (,nvda-590  . ,nvidia-firmware-590)
       (,nvda-580  . ,nvidia-firmware-580)
       (,nvda-470  . ,nvidia-firmware-470))
     driver))

  (define %module
    (assoc-ref
     `((,nvda-beta . ,(if open-source-kernel-module?
                          nvidia-module-open-beta
                          nvidia-module-beta))
       (,nvda-590  . ,(if open-source-kernel-module?
                          nvidia-module-open-590
                          nvidia-module-590))
       (,nvda-580  . ,(if open-source-kernel-module?
                          nvidia-module-open-580
                          nvidia-module-580))
       (,nvda-470  . ,nvidia-module-470)
       (,nvda-390  . ,nvidia-module-390))
     driver))

  (define %modprobe
    (assoc-ref
     `((,nvda-beta . ,nvidia-modprobe-beta)
       (,nvda-590  . ,nvidia-modprobe-590)
       (,nvda-580  . ,nvidia-modprobe-580)
       (,nvda-470  . ,nvidia-modprobe-470)
       (,nvda-390  . ,nvidia-modprobe-390))
     driver))

  (define %settings
    (and configure-xorg?
         (assoc-ref
          `((,nvda-beta . ,nvidia-settings-beta)
            (,nvda-590  . ,nvidia-settings-590)
            (,nvda-580  . ,nvidia-settings-580)
            (,nvda-470  . ,nvidia-settings-470)
            (,nvda-390  . ,nvidia-settings-390))
          driver)))

  (define %xorg-extension
    (and=> configure-xorg?
           (match-lambda
             (#t
              (set-xorg-configuration
               (xorg-configuration
                 (modules (list %driver)))))
             (display-manager
              (set-xorg-configuration
               (xorg-configuration
                 (modules (list %driver)))
               display-manager)))))

  (lambda (os)
    (operating-system
      (inherit os)
      (kernel-arguments
       `("modprobe.blacklist=nouveau"
         "modprobe.blacklist=nova_core,nova_drm"
         ,@(if s0ix-power-management?
               (begin
                 ;; 2026-03
                 (warning
                  (G_ "'~a': argument '~a' is deprecated, the transformation \
won't add kernel arguments other than the minimum necessary in the future.~%")
                  "nonguix-transformation-nvidia"
                  "#:s0ix-power-management?")
                 '("mem_sleep_default=s2idle"
                   "nvidia.NVreg_EnableS0ixPowerManagement=1"))
               '())
         ,(if kernel-mode-setting?
              "nvidia_drm.modeset=1"
              "nvidia_drm.modeset=0")
         ,@(operating-system-user-kernel-arguments os)))
      (packages
       (replace-mesa (operating-system-packages os) #:driver %driver))
      (services
       (replace-mesa
        `(,(service nvidia-service-type
             (nvidia-configuration
               (driver %driver)
               (firmware %firmware)
               (module %module)
               (modprobe %modprobe)
               (settings %settings)))
          ,@(if configure-xorg?
                (list %xorg-extension)
                '())
          ,@(operating-system-user-services os))
        #:driver %driver)))))
