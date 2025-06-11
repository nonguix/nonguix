;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2024, 2025 Hilton Chain <hako@ultrarare.space>

;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system image --image-type=iso9660 nongnu/system/install.scm

(define-module (nongnu system install)
  #:use-module (nonguix transformations)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages zile)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu system linux-initrd)
  #:export (installation-os-nonfree))

(define installation-os-nonfree
  ((compose (nonguix-transformation-guix #:guix-source? #t)
            ;; FIXME: ‘microcode-initrd’ results in unbootable live system.
            (nonguix-transformation-linux #:initrd base-initrd))
   (operating-system
     (inherit installation-os)
     (packages
      (append
       (list curl
             git
             neovim
             zile)
       (operating-system-packages installation-os))))))

installation-os-nonfree
