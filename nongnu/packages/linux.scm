;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (nongnu packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (nonguix licenses))

(define (linux-urls version)
  "Return a list of URLS for Linux VERSION."
  (list (string-append "https://www.kernel.org/pub/linux/kernel/v"
                       (version-major version) ".x/linux-" version ".tar.xz")))

(define (corrupt-linux freedo version hash)
  (package
    (inherit freedo)
    (name "linux")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (linux-urls version))
              (sha256 (base32 hash))))
    (home-page "https://www.kernel.org/")
    (synopsis "Linux kernel with nonfree binary blobs included")
    (description
     "The unmodified Linux kernel, including nonfree blobs, for running GuixSD
on hardware which requires nonfree software to function.")))

(define-public linux
  (corrupt-linux linux-libre "5.1.11"
                 "06fjjwbfky1vjf77km8fxvxqil214028njr8mn4j69whf8h97z8i"))

(define-public linux-4.19
  (corrupt-linux linux-libre-4.19 "4.19.53"
                 "00pbppnn57x7c86cnpv49iwyn2asxpkd9gkspp1p1b5ggq5p6iz6"))

(define-public linux-4.14
  (corrupt-linux linux-libre-4.14 "4.14.128"
                 "04r93x9lmx70p2dld0rryjkx6sk3m4krzzmj2h1n370z9xsjqg79"))

(define-public linux-4.9
  (corrupt-linux linux-libre-4.9 "4.9.182"
                 "15cnk5bmd7kd4ggjzrbs9fpc21wliqdrnfnpg623cf0639l14vmi"))

(define-public linux-4.4
  (corrupt-linux linux-libre-4.4 "4.4.182"
                 "09w5v06c2ghai2pyz04kbhgfnqr22v4dkm9npb5kg6ndc67xh2f4"))

(define-public linux-firmware
  (let ((commit "92e17d0dd2437140fab044ae62baf69b35d7d1fa")
        (revision "1"))
    (package
      (name "linux-firmware")
      (version (git-version "20190502" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url (string-append
                            "https://git.kernel.org/pub/scm/"
                            "linux/kernel/git/firmware/linux-firmware.git"))
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bsgp124jhs9bbjjq0fzmdsziwx1y5aivkgpj8v56ar0y2zmrw2d"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))
                     (let ((source (assoc-ref %build-inputs "source"))
                           (destination (string-append %output "/lib/firmware")))
                       (mkdir-p destination)
                       (copy-recursively source destination #:follow-symlinks? #t)
                       #t))))
      (home-page
       "http://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git")
      (synopsis "Nonfree firmware blobs for Linux")
      (description "Nonfree firmware blobs for enabling support for various
hardware in the Linux kernel.  This is a large package which may be overkill
if your hardware is supported by one of the smaller firmware packages.")
      (license
       (nonfree
        (string-append "https://git.kernel.org/pub/scm/linux/kernel/git/"
                       "firmware/linux-firmware.git/plain/WHENCE"))))))

(define-public amdgpu-firmware
  (package
    (inherit linux-firmware)
    (name "amdgpu-firmware")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (fw-dir (string-append %output "/lib/firmware/"))
                (bin-dir (string-append fw-dir "/amdgpu")))
           (mkdir-p bin-dir)
           (copy-recursively (string-append source "/amdgpu") bin-dir)
           (install-file (string-append source "/LICENSE.amdgpu") fw-dir)
           #t))))
    (home-page "http://support.amd.com/en-us/download/linux")
    (synopsis "Nonfree firmware for AMD graphics chips")
    (description "Nonfree firmware for AMD graphics chips.  While most AMD
graphics cards can be run with the free Mesa, many modern cards require a
nonfree kernel module to run properly and support features like hibernation and
advanced 3D.")
    (license
     (nonfree
      (string-append
       "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
       "/linux-firmware.git/plain/LICENSE.amdgpu")))))

(define-public ath3k-firmware
  (package
    (inherit linux-firmware)
    (name "ath3k-firmware")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (fw-dir (string-append %output "/lib/firmware")))
           (mkdir-p fw-dir)
           (for-each (lambda (file)
                       (copy-file (string-append source "/" file)
                                  (string-append fw-dir "/" file)))
                     (list "ath3k-1.fw"
                           "LICENCE.atheros_firmware"
                           "LICENSE.QualcommAtheros_ar3k"
                           "WHENCE"))
           (copy-recursively (string-append source "/ar3k")
                             (string-append fw-dir "/ar3k"))
           #t))))
    (synopsis "Nonfree firmware blobs for the ath3k Bluetooth driver")
    (description "Nonfree firmware blobs for the ath3k Bluetooth driver. ath3k
is the Linux Bluetooth driver for Atheros AR3011/AR3012 Bluetooth chipsets.")
    (license
     (list
      (nonfree
       (string-append
        "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
        "/linux-firmware.git/plain/LICENCE.atheros_firmware"))
      (nonfree
       (string-append
        "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
        "/linux-firmware.git/plain/LICENSE.QualcommAtheros_ar3k"))))))

(define-public iwlwifi-firmware
  (package
    (inherit linux-firmware)
    (name "iwlwifi-firmware")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source (assoc-ref %build-inputs "source"))
               (fw-dir (string-append %output "/lib/firmware/")))
           (mkdir-p fw-dir)
           (for-each (lambda (file)
                       (copy-file file
                                  (string-append fw-dir (basename file))))
                     (cons*
                      (string-append source "/LICENCE.iwlwifi_firmware")
                      (find-files source
                                  "iwlwifi-.*\\.ucode$")))
           #t))))
    (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
    (synopsis "Nonfree firmware for Intel wifi chips")
    (description "The proprietary iwlwifi kernel module is required by many
modern Intel wifi cards (commonly found in laptops).  This blob enables
support for 5GHz and 802.11ac, among others.")
    (license
     (nonfree (string-append
               "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
               "/linux-firmware.git/plain/LICENCE.iwlwifi_firmware")))))

(define-public broadcom-sta
  (package
    (name "broadcom-sta")
    (version "6.30.223.271")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://docs.broadcom.com/docs-and-downloads/"
                           "docs/linux_sta/hybrid-v35_64-nodebug-pcoem-"
                           (string-replace-substring version "." "_")
                           ".tar.gz"))
       (patches
        (parameterize
            ((%patch-path
              (map (lambda (directory)
                     (string-append directory "/nongnu/packages/patches"))
                   %load-path)))
          ;; https://github.com/NixOS/nixpkgs/tree/master/pkgs/os-specific/linux/broadcom-sta
          ;; https://git.archlinux.org/svntogit/community.git/tree/trunk?h=packages/broadcom-wl-dkms
          (search-patches "broadcom-sta-gcc.patch"
                          "broadcom-sta-license.patch"
                          "broadcom-sta-null-pointer-fix.patch"
                          "broadcom-sta-rdtscl.patch"
                          "broadcom-sta-linux-4.7.patch"
                          "broadcom-sta-linux-4.8.patch"
                          "broadcom-sta-debian-fix-kernel-warnings.patch"
                          "broadcom-sta-linux-4.11.patch"
                          "broadcom-sta-linux-4.12.patch"
                          "broadcom-sta-linux-4.15.patch"
                          "broadcom-sta-fix_mac_profile_discrepancy.patch"
                          "broadcom-sta-linux-5.1.patch")))
       (sha256
        (base32
         "1gj485qqr190idilacpxwgqyw21il03zph2rddizgj7fbd6pfyaz"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:linux ,linux
       #:tests? #f))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.broadcom.com/support/802.11")
    (synopsis "Broadcom 802.11 Linux STA wireless driver")
    (description "This package contains Broadcom's IEEE 802.11a/b/g/n/ac hybrid
Linux device driver for the following chipsets:
@itemize
@item BCM4311
@item BCM4312
@item BCM4313
@item BCM4321
@item BCM4322
@item BCM43224
@item BCM43225
@item BCM43227
@item BCM43228
@item BCM43142
@item BCM4331
@item BCM4352
@item BCM4360
@end itemize")
    (license (nonfree "https://www.broadcom.com/support/802.11"))))

(define-public broadcom-bt-firmware
  (package
    (name "broadcom-bt-firmware")
    (version "12.0.1.1012")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://download.windowsupdate.com/c/msdownload/update/driver/drvs/2017/04/"
         "852bb503-de7b-4810-a7dd-cbab62742f09_7cf83a4c194116648d17707ae37d564f9c70bec2"
         ".cab"))
       (file-name (string-append name "-" version ".cab"))
       (sha256
        (base32
         "1b1qjwxjk4y91l3iz157kms8601n0mmiik32cs6w9b1q4sl4pxx9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 rdelim)
                  (ice-9 regex))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 rdelim)
                      (ice-9 regex))
         (let ((PATH (string-append (assoc-ref %build-inputs "cabextract") "/bin:"
                                    (assoc-ref %build-inputs "bluez") "/bin"))
               (source (assoc-ref %build-inputs "source"))
               (firmware-dir (string-append %output "/lib/firmware/brcm/")))
           (setenv "PATH" PATH)
           (system* "cabextract" source)
           (mkdir-p firmware-dir)
           ;; process the inf file to get the correct filenames
           (with-input-from-file "bcbtums.inf"
             (lambda ()
               (do ((line (read-line) (read-line))
                    (devices '()))
                   ((eof-object? line) #t)
                 ;; these two `lets' are like awk patterns matching against
                 ;; records. link devices in this file with its vids and pids
                 (let ((rcrd (string-match "%.*%=(.*),.*VID_(....).*PID_(....)"
                                           line)))
                   (when rcrd
                     (set! devices
                           (assoc-set! devices (match:substring rcrd 1)
                                       `((vid . ,(match:substring rcrd 2))
                                         (pid . ,(match:substring rcrd 3)))))))
                 ;; find the hex file associated with each device, build the
                 ;; output file name
                 (let ((rcrd (string-match "\\[(RAMUSB.*)\\.CopyList\\]" line)))
                   (when rcrd
                     (let* ((key (match:substring rcrd 1))
                            (hex-file (begin (do ((line (read-line) (read-line)))
                                                 ((string-match "^([0-9A-Z_.]+\\.hex)" line)
                                                  (string-drop-right line 1)))))
                            (chipset (car (string-tokenize
                                           hex-file
                                           char-set:letter+digit)))
                            (vid (assoc-ref (assoc-ref devices key) 'vid))
                            (pid (assoc-ref (assoc-ref devices key) 'pid))
                            (hcd-file (string-append chipset "-"
                                                     (string-downcase vid) "-"
                                                     (string-downcase pid)
                                                     ".hcd")))
                       ;; finally convert the file, phew!
                       (system* "hex2hcd"
                                "-o" (string-append firmware-dir hcd-file)
                                hex-file)))))))))))
    (native-inputs
     `(("bluez" ,bluez)
       ("cabextract" ,cabextract)))
    (home-page "http://www.broadcom.com/support/bluetooth")
    (synopsis "Broadcom bluetooth firmware")
    (description
     "This package contains nonfree firmware for the following bluetooth
chipsets from Broadcom:
@itemize
@item BCM4335C0
@item BCM4350C5
@item BCM4356A2
@item BCM4371C2
@item BCM20702A1
@item BCM20702B0
@item BCM20703A1
@item BCM43142A0
@end itemize")
    (license
     (undistributable
      (string-append
       "https://raw.githubusercontent.com/winterheart/broadcom-bt-firmware"
       "/b60fa04881bf8f9b9d578f57d1dfa596cae2a82e"
       "/LICENSE.broadcom_bcm20702")))))

(define-public rtlwifi-firmware
  (package
    (inherit linux-firmware)
    (name "rtlwifi-firmware")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (fw-dir (string-append %output "/lib/firmware/"))
                (bin-dir (string-append fw-dir "/rtlwifi")))
           (mkdir-p bin-dir)
           (copy-recursively (string-append source "/rtlwifi") bin-dir)
           (install-file (string-append source "/LICENCE.rtlwifi_firmware.txt") fw-dir)
           #t))))
    (home-page "https://wireless.wiki.kernel.org/en/users/drivers/rtl819x")
    (synopsis "Nonfree firmware for Realtek wifi chips")
    (description
     "Nonfree firmware for Realtek wifi chips.  This package contains nonfree
firmware for the following chips:
@itemize
@item Realtek RTL8188EE firmware (rtlwifi/rtl8188efw.bin)
@item Realtek RTL8188EU firmware (rtlwifi/rtl8188eufw.bin)
@item Realtek RTL8192CE/RTL8188CE firmware (rtlwifi/rtl8192cfw.bin)
@item Realtek RTL8192CE/RTL8188CE B-cut firmware (rtlwifi/rtl8192cfwU_B.bin)
@item Realtek RTL8188CE A-cut firmware, version 4.816.2011 (rtlwifi/rtl8192cfwU.bin)
@item Realtek RTL8192CU/RTL8188CU UMC A-cut firmware (rtlwifi/rtl8192cufw_A.bin)
@item Realtek RTL8192CU/RTL8188CU UMC B-cut firmware (rtlwifi/rtl8192cufw_B.bin)
@item Realtek RTL8192CU/RTL8188CU TMSC firmware (rtlwifi/rtl8192cufw_TMSC.bin)
@item Realtek RTL8192CU/RTL8188CU fallback firmware (rtlwifi/rtl8192cufw.bin)
@item Realtek RTL8192DE firmware (rtlwifi/rtl8192defw.bin)
@item Realtek RTL8192EE wifi firmware (rtlwifi/rtl8192eefw.bin)
@item Realtek RTL8192EU non-WoWLAN firmware (rtlwifi/rtl8192eu_nic.bin)
@item Realtek RTL8192EU WoWLAN firmware (rtlwifi/rtl8192eu_wowlan.bin)
@item Realtek RTL8192SE/RTL8191SE firmware, version 4.816.2011 (rtlwifi/rtl8192sefw.bin)
@item Realtek RTL8192SU/RTL8712U firmware (rtlwifi/rtl8712u.bin)
@item Realtek RTL8723AU rev A wifi-with-BT firmware (rtlwifi/rtl8723aufw_A.bin)
@item Realtek RTL8723AU rev B wifi-with-BT firmware (rtlwifi/rtl8723aufw_B.bin)
@item Realtek RTL8723AU rev B wifi-only firmware (rtlwifi/rtl8723aufw_B_NoBT.bin)
@item Realtek RTL8723BE firmware, version 36 (rtlwifi/rtl8723befw_36.bin)
@item Realtek RTL8723BE firmware (rtlwifi/rtl8723befw.bin)
@item Realtek RTL8723BS BT firmware (rtlwifi/rtl8723bs_bt.bin)
@item Realtek RTL8723BS wifi non-WoWLAN firmware (rtlwifi/rtl8723bs_nic.bin)
@item Realtek RTL8723BS wifi WoWLAN firmware (rtlwifi/rtl8723bs_wowlan.bin)
@item Realtek RTL8723BU non-WoWLAN firmware (rtlwifi/rtl8723bu_nic.bin)
@item Realtek RTL8723BU WoWLAN firmware (rtlwifi/rtl8723bu_wowlan.bin)
@item Realtek RTL8723DE firmware (rtlwifi/rtl8723defw.bin)
@item Realtek RTL8723AE rev B firmware (rtlwifi/rtl8723fw_B.bin)
@item Realtek RTL8723AE rev A firmware (rtlwifi/rtl8723fw.bin)
@item Realtek RTL8821AE firmware, version 29 (rtlwifi/rtl8821aefw_29.bin)
@item Realtek RTL8821AE firmware (rtlwifi/rtl8821aefw_wowlan.bin)
@item Realtek RTL8821AE firmware (rtlwifi/rtl8821aefw.bin)
@item Realtek RTL8822BE firmware (rtlwifi/rtl8822befw.bin)
@end itemize")
    (license
     (nonfree
      (string-append
       "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
       "/linux-firmware.git/plain/LICENCE.rtlwifi_firmware.txt")))))
