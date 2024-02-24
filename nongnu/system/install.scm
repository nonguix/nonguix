;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>

;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system image --image-type=iso9660 nongnu/system/install.scm

(define-module (nongnu system install)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages zile)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

;; https://substitutes.nonguix.org/signing-key.pub
(define %signing-key
  (plain-file "nonguix.pub" "\
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define %channels
  (cons* (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix")
          ;; Enable signature verification:
          (introduction
           (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
         %default-channels))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))
    (packages
      (append
        (list curl
              git
              neovim
              zile)
        (operating-system-packages installation-os)))
    (services
     (modify-services (operating-system-user-services installation-os)
       (guix-service-type
        config => (guix-configuration
                   (inherit config)
                   (guix (guix-for-channels %channels))
                   (authorized-keys
                    (cons* %signing-key
                           %default-authorized-guix-keys))
                   (substitute-urls
                    `(,@%default-substitute-urls
                      "https://substitutes.nonguix.org"))
                   (channels %channels)))))))

installation-os-nonfree
