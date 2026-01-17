;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (nonguix transformations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix channels)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (gnu system)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu services)
  #:use-module (gnu services base)
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
                                        (kernel-mode-setting? #t)
                                        (open-source-kernel-module? #f))
  "Return a procedure that transforms an operating system, setting up
DRIVER (default: nvda) for NVIDIA graphics card.

KERNEL-MODE-SETTING? (default: #t) is required for Wayland and rootless Xorg
support.

OPEN-SOURCE-KERNEL-MODULE? (default: #f) only supports Turing and later
architectures and is expected to work with 'linux-lts'.

For application setup, use 'replace-mesa'.

TODO: Xorg configuration."
  (define %presets
    `((,nvda . ,(service nvidia-service-type
                  (nvidia-configuration
                   (driver nvda)
                   (firmware nvidia-firmware)
                   (module
                    (if open-source-kernel-module?
                        nvidia-module-open
                        nvidia-module)))))
      (,nvdb . ,(service nvidia-service-type
                  (nvidia-configuration
                   (driver nvdb)
                   (firmware nvidia-firmware-beta)
                   (module
                    (if open-source-kernel-module?
                        nvidia-module-open-beta
                        nvidia-module-beta)))))))
  (lambda (os)
    (operating-system
      (inherit os)
      (kernel-arguments
       (delete-duplicates
        (cons* "modprobe.blacklist=nouveau"
               (string-append
                "nvidia_drm.modeset=" (if kernel-mode-setting? "1" "0"))
               (remove
                (cut string-prefix? "nvidia_drm.modeset=" <>)
                (operating-system-user-kernel-arguments os)))))
      (packages
       (replace-mesa (operating-system-packages os) #:driver driver))
      (services
       (replace-mesa
        `(,(or (assoc-ref %presets driver)
               (leave
                (G_ "no NVIDIA service configuration available for '~a'~%")
                (package-name driver)))
          ,@(operating-system-user-services os))
        #:driver driver)))))
