;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022-2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Krzysztof Baranowski <pharcosyle@gmail.com>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>

(define-module (nongnu packages firmware)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages efi)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages golang-compression)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix guix-license:)
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
                "-Dsupported_build=true"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (delete 'ensure-all-remotes-are-disabled)))))))

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
       `(#:substitutable? #f
         #:install-plan '(("ov5640_af.bin" "lib/firmware/"))))
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
       `(#:substitutable? #f
         #:install-plan '(("rtl_bt/rtl8723cs_xx_fw.bin" "lib/firmware/"))))
      (synopsis "Firmware for the RTL8723BS/CS")
      (description "This package provides binary firmware for the RTL8723BS/CS
WiFi/Bluetooth chip in the PinePhone.")
      (home-page "https://megous.com/git/linux-firmware")
      (license (nonfree (string-append "unknown"))))))

(define-public anx7688-firmware
  (let ((commit "6e8e591e17e207644dfe747e51026967bb1edab5")
        (revision "1"))
    (package
      (name "anx7688-firmware")
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
       `(#:substitutable? #f
         #:install-plan '(("anx7688-fw.bin" "lib/firmware/"))))
      (synopsis "Firmware for the ANX7688")
      (description "This package provides binary firmware for the ANX7688
HDMI to USB Type-C Bridge in the PinePhone.")
      (home-page "https://megous.com/git/linux-firmware")
      (license (nonfree (string-append "unknown"))))))

(define-public ap6256-firmware
  (let ((commit "056d5f6776e515f90bbbbead1be06857aaef17d0")
        (revision "1"))
    (package
      (name "ap6256-firmware")
      (version (git-version "2020.02" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://gitlab.manjaro.org/manjaro-arm/packages/community/ap6256-firmware")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1s52rpikw0gysph5lq7vr6b3nsxczg4ikgil9zdgmcknjnxk9kbv"))))
      (build-system copy-build-system)
      (arguments
       `(#:substitutable? #f
         #:install-plan '(;;Bluetooth firmware
                          ("BCM4345C5.hcd" "/lib/firmware/brcm/")
                          ;; WiFi firmware
                          ("fw_bcm43456c5_ag.bin" "/lib/firmware/brcm/")
                          ("fw_bcm43456c5_ag.bin"
                           "/lib/firmware/brcm/brcmfmac43456-sdio.bin")
                          ("brcmfmac43456-sdio.clm_blob"
                           "/lib/firmware/brcm/")
                          ("brcmfmac43456-sdio.AP6256.txt"
                           "/lib/firmware/brcm/")
                          ("brcmfmac43456-sdio.AP6256.txt"
                           "/lib/firmware/brcm/brcmfmac43456-sdio.pine64,pinebook-pro.txt"))))
      (synopsis "Firmware for the wifi/bt module AP6256")
      (description
       "This package provides Firmware for the wifi/bt module AP6256,
found in Pinebook Pro.")
      (home-page "https://gitlab.manjaro.org/manjaro-arm/packages/community/ap6256-firmware")
      (license (nonfree (string-append "unknown"))))))

(define-public bluez-firmware
  (package
    (name "bluez-firmware")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://bluez.sf.net/download/" name "-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1v4yv6gvlvvwslpb0lj1nsp4r900zxpvxz2ab0sbvimbiw8rw4dn"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'install-license-files 'relocate-copyright
                          (lambda* _
                            (install-file (string-append #$output
                                           "/lib/firmware/BCM-LEGAL.txt")
                                          (string-append #$output
                                           "/share/doc/bluez-firmware-"
                                           #$(package-version bluez-firmware)
                                           "/BCM-LEGAL.txt")))))))
    (synopsis "Firmware for Broadcom BCM203x and STLC2300 Bluetooth chips")
    (description "This package provides firmware for Broadcom BCM203x
and STLC2300 Bluetooth chips.")
    (home-page "https://github.com/RPi-Distro/bluez-firmware")
    (license (list guix-license:gpl2+
                   (nonfree
                    "file:///share/doc/bluez-firmware-1.2/BCM-LEGAL.txt")))))

(define dump-file-chunk
  #~(lambda (in out count start)
      (use-modules (rnrs io ports))
      (call-with-output-file out
        (lambda (out-port)
          (put-bytevector
           out-port
           (call-with-input-file in
             (lambda (in-port)
               (seek in-port start SEEK_SET)
               (get-bytevector-n in-port count))))))))

(define-public facetimehd-firmware
  (package
    (name "facetimehd-firmware")
    (version "1.43")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://updates.cdn-apple.com/2019/cert"
             "/041-88431-20191011-e7ee7d98-2878-4cd9-bc0a-d98b3a1e24b1"
             "/OSXUpd10.11.5.dmg"))
       (sha256
        (base32
         "009kfk1nrrialgp69c5smzgbmd5xpvk35xmqr2fzb15h6pp33ka6"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan #~'(("firmware.bin" "/lib/firmware/facetimehd/"))
      #:phases
      (let ((dmg-subset-size 207733123)
            (dmg-subset-offset 204909802)
            (firmware-size 603715)
            (firmware-offset 81920))
        #~(modify-phases %standard-phases
            (add-before 'install 'extract
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((firmware-in
                       (string-append
                        "System/Library/Extensions/AppleCameraInterface.kext"
                        "/Contents/MacOS/AppleCameraInterface")))
                  (let ((dmg-subset "dmg-subset"))
                    (#$dump-file-chunk #$source
                                       dmg-subset
                                       #$dmg-subset-size
                                       #$dmg-subset-offset)
                    (system
                     (string-join
                      (list (search-input-file inputs "/bin/xz")
                            "--decompress"
                            "--stdout"
                            dmg-subset
                            "|"
                            (search-input-file inputs "/bin/cpio")
                            "--format=odc"
                            "--extract"
                            "--make-directories"
                            (string-append "./" firmware-in)))))
                  (let ((firmware-out "firmware.bin.gz"))
                    (#$dump-file-chunk firmware-in
                                       firmware-out
                                       #$firmware-size
                                       #$firmware-offset)
                    (invoke
                     (search-input-file inputs "/bin/gzip")
                     "--decompress"
                     (string-append firmware-out))))))))))
    (native-inputs
     (list cpio gzip xz))
    (synopsis "Firmware for the FacetimeHD (Broadcom 1570) PCIe webcam")
    (description "Firmware for the FacetimeHD webcam.  See
@uref{https://github.com/patjak/facetimehd/wiki/Get-Started#firmware-extraction,
patjak's facetimehd wiki} for more information.")
    (home-page "https://support.apple.com")
    (license (nonfree "https://www.apple.com/legal"))
    (supported-systems '("i686-linux" "x86_64-linux"))))

(define-public facetimehd-calibration
  (package
    (name "facetimehd-calibration")
    (version "5.1.5769")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://download.info.apple.com/Mac_OS_X"
             "/031-30890-20150812-ea191174-4130-11e5-a125-930911ba098f"
             "/bootcamp" version".zip"))
       (sha256
        (base32
         "07jbh6d0djcvcgj5hhkkw7d6mvcl228yb8rp0a2qqw20ya72rpjf"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("." "/lib/firmware/facetimehd/"
                          #:include-regexp ("[0-9]{4}_01XX\\.dat")))
      #:phases
      (let ((calibration-files
             '(("1771_01XX.dat" 19040 1644880)
               ("1871_01XX.dat" 19040 1606800)
               ("1874_01XX.dat" 19040 1625840)
               ("9112_01XX.dat" 33060 1663920))))
        #~(modify-phases %standard-phases
            (add-before 'install 'extract
              (lambda* (#:key inputs #:allow-other-keys)
                (invoke (search-input-file inputs "/bin/arc")
                        "-ext" ".rar"
                        "unarchive" "BootCamp/Drivers/Apple/AppleCamera64.exe")

                (for-each (lambda (spec)
                            (apply #$dump-file-chunk
                                   "AppleCamera64/AppleCamera.sys"
                                   spec))
                          '#$calibration-files)))))))
    (native-inputs
     (list go-arc unzip))
    (synopsis "Calibration files for the FacetimeHD (Broadcom 1570) PCIe webcam")
    (description "Calibration files for the FacetimeHD webcam.  These are
optional but make the colors look much better.  See
@uref{https://github.com/patjak/facetimehd/wiki/Extracting-the-sensor-calibration-files,
patjak's facetimehd wiki} for more information.")
    (home-page "https://support.apple.com/kb/DL1837")
    (license (nonfree "https://www.apple.com/legal"))
    (supported-systems '("i686-linux" "x86_64-linux"))))
