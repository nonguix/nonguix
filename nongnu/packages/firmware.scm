;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>

(define-module (nongnu packages firmware)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages firmware)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; fwupd with LVFS nonfree repositories enabled
(define-public fwupd-nonfree
  (package
    (inherit fwupd)
    (name "fwupd-nonfree")
    (arguments
     (substitute-keyword-arguments (package-arguments fwupd)
       ((#:configure-flags _
         #~'())
        #~(list "--wrap-mode=nofallback"
                "-Dsystemd=false"
                (string-append "-Defi_os_dir="
                               #$gnu-efi "/lib")
                "-Defi_binary=false"
                (string-append "-Dudevdir="
                               #$output "/lib/udev")
                "--localstatedir=/var"
                (string-append "--libexecdir="
                               #$output "/libexec")
                "-Dsupported_build=true"))))))
