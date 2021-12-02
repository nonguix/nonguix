;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022-2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>

(define-module (nongnu packages firmware)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages firmware)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix licenses))

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

(define-public ov5640-firmware
  (let ((commit "6e8e591e17e207644dfe747e51026967bb1edab5")
        (revision "1"))
    (package
      (name "ov5640-firmware")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://megous.com/git/linux-firmware")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19xmkdvlkczc6zgigy8jdbgnp37i6pc03m2cm3gilvzg8m7v18ad"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("ov5640_af.bin" "lib/firmware/"))))
      (synopsis "Firmware for the OV5640 sensor in the PinePhone")
      (description "This package provides binary firmware for the 0V5640 sensor
in the PinePhone.")
      (home-page "https://megous.com/git/linux-firmware")
      (license (nonfree (string-append "unknown"))))))

(define-public rtl8723bt-firmware
  (let ((commit "6e8e591e17e207644dfe747e51026967bb1edab5")
        (revision "1"))
    (package
      (name "rtl8723bt-firmware")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://megous.com/git/linux-firmware")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19xmkdvlkczc6zgigy8jdbgnp37i6pc03m2cm3gilvzg8m7v18ad"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("rtl_bt/rtl8723cs_xx_fw.bin" "lib/firmware/"))))
      (synopsis "Firmware for the RTL8723BS/CS")
      (description "This package provides binary firmware for the RTL8723BS/CS
WiFi/Bluetooth chip in the PinePhone.")
      (home-page "https://megous.com/git/linux-firmware")
      (license (nonfree (string-append "unknown"))))))
