;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Timotej Lazar <timotej.lazar@araneo.si>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
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

(define-public linux-5.4
  (corrupt-linux linux-libre-5.4 "5.4.8"
                 "0llss8hbzkwqxx92y419vj2jvq0d8k52iivh3f59wgqr0b4i9i22"))

(define-public linux-4.19
  (corrupt-linux linux-libre-4.19 "4.19.93"
                 "0d2qgwnmvnwbsijc8k85sgmywyrv55wpah6qwjk6kgx8h8zx5x9a"))

(define-public linux-4.14
  (corrupt-linux linux-libre-4.14 "4.14.162"
                 "0jwbr7q17khv0174hl0kp78bsyhjv0mljfvrnbl9qdfl9hi70lgn"))

(define-public linux-4.9
  (corrupt-linux linux-libre-4.9 "4.9.208"
                 "0njjw1i8dilihn1hz62zra4b9y05fb3r2k2sqlkd0wfn86c1rbdp"))

(define-public linux-4.4
  (corrupt-linux linux-libre-4.4 "4.4.208"
                 "03jj91z5dc0ybpjy9w6aanb3k53gcj7gsjc32h3ldf72hlmgz6aq"))

(define-public linux linux-5.4)

(define-public linux-firmware
  (package
    (name "linux-firmware")
    (version "20191215")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                                  "/git/firmware/linux-firmware.git/snapshot/"
                                  "linux-firmware-" version ".tar.gz"))
              (sha256
               (base32
                "024napabn3zwl8gr50w9cp4gdxhch8pkl2qx814c93ydkfj81znd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "install"
                       (string-append "DESTDIR=" out)))))
         (delete 'validate-runpath))))
    (home-page
     "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git")
    (synopsis "Nonfree firmware blobs for Linux")
    (description "Nonfree firmware blobs for enabling support for various
hardware in the Linux kernel.  This is a large package which may be overkill
if your hardware is supported by one of the smaller firmware packages.")
    (license
     (nonfree
      (string-append "https://git.kernel.org/pub/scm/linux/kernel/git/"
                     "firmware/linux-firmware.git/plain/WHENCE")))))

(define-public amdgpu-firmware
  (package
    (inherit linux-firmware)
    (name "amdgpu-firmware")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:license-file-regexp "LICENSE.amdgpu"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware"))
                    (bin-dir (string-append fw-dir "/amdgpu")))
               (mkdir-p bin-dir)
               (copy-recursively "./amdgpu" bin-dir)
               #t)))
         (delete 'validate-runpath))))
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
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:license-file-regexp
       "LICEN[CS]E\\.(atheros_firmware|QualcommAtheros_ar3k)"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware"))
                    (bin-dir (string-append fw-dir "/ar3k")))
               (mkdir-p bin-dir)
               (copy-recursively "./ar3k" bin-dir)
               (install-file "ath3k-1.fw" fw-dir)
               #t)))
         (delete 'validate-runpath))))
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
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:license-file-regexp "LICENCE.iwlwifi_firmware"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware")))
               (for-each (lambda (file)
                           (install-file file fw-dir))
                         (find-files "." "iwlwifi-.*\\.ucode$"))
               #t)))
         (delete 'validate-runpath))))
    (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
    (synopsis "Nonfree firmware for Intel wifi chips")
    (description "The proprietary iwlwifi kernel module is required by many
modern Intel wifi cards (commonly found in laptops).  This blob enables
support for 5GHz and 802.11ac, among others.")
    (license
     (nonfree (string-append
               "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
               "/linux-firmware.git/plain/LICENCE.iwlwifi_firmware")))))

(define-public realtek-firmware
  (package
    (inherit linux-firmware)
    (name "realtek-firmware")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:license-file-regexp "LICENCE.rtlwifi_firmware.txt"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware")))
               (for-each (lambda (dir)
                           (let ((bin-dir (string-append fw-dir "/" dir)))
                             (mkdir-p bin-dir)
                             (copy-recursively dir bin-dir)))
                         '("rtlwifi" "rtl_nic" "rtl_bt"))
               #t)))
         (delete 'validate-runpath))))
    (home-page "https://wireless.wiki.kernel.org/en/users/drivers/rtl819x")
    (synopsis "Nonfree firmware for Realtek ethernet, wifi, and bluetooth chips")
    (description
     "Nonfree firmware for Realtek ethernet, wifi, and Bluetooth chips.  This
package contains nonfree firmware for the following chips:
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
@item Realtek RTL8105E-1 firmware (rtl_nic/rtl8105e-1.fw)
@item Realtek RTL8106E-1 firmware, version 0.0.1 (rtl_nic/rtl8106e-1.fw)
@item Realtek RTL8106E-2 firmware, version 0.0.1 (rtl_nic/rtl8106e-2.fw)
@item Realtek RTL8107E-1 firmware, version 0.0.2 (rtl_nic/rtl8107e-1.fw)
@item Realtek RTL8107E-2 firmware, version 0.0.2 (rtl_nic/rtl8107e-2.fw)
@item Realtek RTL8111D-1/RTL8168D-1 firmware (rtl_nic/rtl8168d-1.fw)
@item Realtek RTL8111D-2/RTL8168D-2 firmware (rtl_nic/rtl8168d-2.fw)
@item Realtek RTL8168E-1 firmware (rtl_nic/rtl8168e-1.fw)
@item Realtek RTL8168E-2 firmware (rtl_nic/rtl8168e-2.fw)
@item Realtek RTL8168E-3 firmware, version 0.0.4 (rtl_nic/rtl8168e-3.fw)
@item Realtek RTL8168F-1 firmware, version 0.0.5 (rtl_nic/rtl8168f-1.fw)
@item Realtek RTL8168F-2 firmware, version 0.0.4 (rtl_nic/rtl8168f-2.fw)
@item Realtek RTL8168G-1 firmware, version 0.0.3 (rtl_nic/rtl8168g-1.fw)
@item Realtek RTL8168G-2 firmware, version 0.0.1 (rtl_nic/rtl8168g-2.fw)
@item Realtek RTL8168G-3 firmware, version 0.0.1 (rtl_nic/rtl8168g-3.fw)
@item Realtek RTL8168H-1 firmware, version 0.0.2 (rtl_nic/rtl8168h-1.fw)
@item Realtek RTL8168H-2 firmware, version 0.0.2 (rtl_nic/rtl8168h-2.fw)
@item Realtek RTL8402-1 firmware, version 0.0.1 (rtl_nic/rtl8402-1.fw)
@item Realtek RTL8411-1 firmware, version 0.0.3 (rtl_nic/rtl8411-1.fw)
@item Realtek RTL8411-2 firmware, version 0.0.1 (rtl_nic/rtl8411-2.fw)
@item Realtek RTL8192EE Bluetooth firmware (rtl_bt/rtl8192ee_fw.bin)
@item Realtek RTL8812AE Bluetooth firmware (rtl_bt/rtl8812ae_fw.bin)
@item Realtek RTL8761A Bluetooth firmware (rtl_bt/rtl8761a_fw.bin)
@item Realtek RTL8821A Bluetooth firmware (rtl_bt/rtl8821a_fw.bin)
@item Realtek RTL8192EU Bluetooth firmware (rtl_bt/rtl8192eu_fw.bin)
@item Realtek RTL8723AU rev A Bluetooth firmware (rtl_bt/rtl8723a_fw.bin)
@item Realtek RTL8723BU rev B Bluetooth firmware (rtl_bt/rtl8723b_fw.bin)
@item Realtek RTL8723D Bluetooth config (rtl_bt/rtl8723d_config.bin)
@item Realtek RTL8723D Bluetooth firmware (rtl_bt/rtl8723d_fw.bin)
@item Realtek RTL8821C Bluetooth config (rtl_bt/rtl8821c_config.bin)
@item Realtek RTL8821C Bluetooth firmware (rtl_bt/rtl8821c_fw.bin)
@item Realtek RTL8822B Bluetooth config (rtl_bt/rtl8822b_config.bin)
@item Realtek RTL8822B Bluetooth firmware (rtl_bt/rtl8822b_fw.bin)
@item Realtek RTL8822CU Bluetooth firmware (rtl_bt/rtl8822cu_fw.bin)
@end itemize")
    (license
     (nonfree
      (string-append
       "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
       "/linux-firmware.git/plain/LICENCE.rtlwifi_firmware.txt")))))

(define-public rtlwifi-firmware
  (deprecated-package "rtlwifi-firmware" realtek-firmware))

(define broadcom-sta-version "6.30.223.271")

(define broadcom-sta-x86_64-source
  (origin
    (method url-fetch/tarbomb)
    (uri (string-append "https://docs.broadcom.com/docs-and-downloads/"
                        "docs/linux_sta/hybrid-v35_64-nodebug-pcoem-"
                        (string-replace-substring broadcom-sta-version "." "_")
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

(define broadcom-sta-i686-source
  (origin
    (inherit broadcom-sta-x86_64-source)
    (uri (string-append "https://docs.broadcom.com/docs-and-downloads/"
                        "docs/linux_sta/hybrid-v35-nodebug-pcoem-"
                        (string-replace-substring broadcom-sta-version "." "_")
                        ".tar.gz"))
    (sha256
     (base32
      "1kaqa2dw3nb8k23ffvx46g8jj3wdhz8xa6jp1v3wb35cjfr712sg"))))

(define-public broadcom-sta
  (package
    (name "broadcom-sta")
    (version broadcom-sta-version)
    (source #f)
    (build-system linux-module-build-system)
    (arguments
     `(#:linux ,linux
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source (assoc-ref inputs "broadcom-sta-source")))
               (invoke "tar" "xf" source)
               (chdir ((@@ (guix build gnu-build-system) first-subdirectory) "."))
               #t))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (native-inputs
     `(("broadcom-sta-source"
        ,(match (or (%current-target-system) (%current-system))
           ("x86_64-linux" broadcom-sta-x86_64-source)
           (_ broadcom-sta-i686-source)))))
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

(define-public rtl-nic-firmware
  (package
    (inherit linux-firmware)
    (name "rtl-nic-firmware")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:license-file-regexp "LICENCE.rtlwifi_firmware.txt"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware"))
                    (bin-dir (string-append fw-dir "/rtl_nic")))
               (mkdir-p bin-dir)
               (copy-recursively "./rtl_nic" bin-dir)
               #t)))
         (delete 'validate-runpath))))
    (synopsis "Nonfree firmware for Realtek ethernet chips")
    (description
     "This package contains nonfree firmware for the following Realtek ethernet chips:
@itemize
 @item Realtek RTL8105E-1 firmware (rtl_nic/rtl8105e-1.fw)
 @item Realtek RTL8106E-1 firmware, version 0.0.1 (rtl_nic/rtl8106e-1.fw)
 @item Realtek RTL8106E-2 firmware, version 0.0.1 (rtl_nic/rtl8106e-2.fw)
 @item Realtek RTL8107E-1 firmware, version 0.0.2 (rtl_nic/rtl8107e-1.fw)
 @item Realtek RTL8107E-2 firmware, version 0.0.2 (rtl_nic/rtl8107e-2.fw)
 @item Realtek RTL8111D-1/RTL8168D-1 firmware (rtl_nic/rtl8168d-1.fw)
 @item Realtek RTL8111D-2/RTL8168D-2 firmware (rtl_nic/rtl8168d-2.fw)
 @item Realtek RTL8168E-1 firmware (rtl_nic/rtl8168e-1.fw)
 @item Realtek RTL8168E-2 firmware (rtl_nic/rtl8168e-2.fw)
 @item Realtek RTL8168E-3 firmware, version 0.0.4 (rtl_nic/rtl8168e-3.fw)
 @item Realtek RTL8168F-1 firmware, version 0.0.5 (rtl_nic/rtl8168f-1.fw)
 @item Realtek RTL8168F-2 firmware, version 0.0.4 (rtl_nic/rtl8168f-2.fw)
 @item Realtek RTL8168G-1 firmware, version 0.0.3 (rtl_nic/rtl8168g-1.fw)
 @item Realtek RTL8168G-2 firmware, version 0.0.1 (rtl_nic/rtl8168g-2.fw)
 @item Realtek RTL8168G-3 firmware, version 0.0.1 (rtl_nic/rtl8168g-3.fw)
 @item Realtek RTL8168H-1 firmware, version 0.0.2 (rtl_nic/rtl8168h-1.fw)
 @item Realtek RTL8168H-2 firmware, version 0.0.2 (rtl_nic/rtl8168h-2.fw)
 @item Realtek RTL8402-1 firmware, version 0.0.1 (rtl_nic/rtl8402-1.fw)
 @item Realtek RTL8411-1 firmware, version 0.0.3 (rtl_nic/rtl8411-1.fw)
 @item Realtek RTL8411-2 firmware, version 0.0.1 (rtl_nic/rtl8411-2.fw)
@end itemize")
    (license
     (nonfree
      (string-append
       "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
       "/linux-firmware.git/plain/LICENCE.rtlwifi_firmware.txt")))))

(define-public rtl-bt-firmware
  (package
    (inherit linux-firmware)
    (name "rtl-bt-firmware")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:license-file-regexp "LICENCE.rtlwifi_firmware.txt"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware"))
                    (bin-dir (string-append fw-dir "/rtl_bt")))
               (mkdir-p bin-dir)
               (copy-recursively "./rtl_bt" bin-dir)
               #t)))
         (delete 'validate-runpath))))
    (synopsis "Nonfree firmware for Realtek bluetooth chips")
    (description
     "This package contains nonfree firmware for the following Realtek bluetooth chips:
@itemize
 @item Realtek RTL8192EE Bluetooth firmware (rtl_bt/rtl8192ee_fw.bin)
 @item Realtek RTL8812AE Bluetooth firmware (rtl_bt/rtl8812ae_fw.bin)
 @item Realtek RTL8761A Bluetooth firmware (rtl_bt/rtl8761a_fw.bin)
 @item Realtek RTL8821A Bluetooth firmware (rtl_bt/rtl8821a_fw.bin)
 @item Realtek RTL8192EU Bluetooth firmware (rtl_bt/rtl8192eu_fw.bin)
 @item Realtek RTL8723AU rev A Bluetooth firmware (rtl_bt/rtl8723a_fw.bin)
 @item Realtek RTL8723BU rev B Bluetooth firmware (rtl_bt/rtl8723b_fw.bin)
 @item Realtek RTL8723D Bluetooth config (rtl_bt/rtl8723d_config.bin)
 @item Realtek RTL8723D Bluetooth firmware (rtl_bt/rtl8723d_fw.bin)
 @item Realtek RTL8821C Bluetooth config (rtl_bt/rtl8821c_config.bin)
 @item Realtek RTL8821C Bluetooth firmware (rtl_bt/rtl8821c_fw.bin)
 @item Realtek RTL8822B Bluetooth config (rtl_bt/rtl8822b_config.bin)
 @item Realtek RTL8822B Bluetooth firmware (rtl_bt/rtl8822b_fw.bin)
 @item Realtek RTL8822CU Bluetooth firmware (rtl_bt/rtl8822cu_fw.bin)
@end itemize")
    (license
     (nonfree
      (string-append
       "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
       "/linux-firmware.git/plain/LICENCE.rtlwifi_firmware.txt")))))

(define-public intel-microcode
  (package
    (name "intel-microcode")
    (version "20191115")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append
                   "https://github.com/intel/"
                   "Intel-Linux-Processor-Microcode-Data-Files.git"))
             (commit (string-append "microcode-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pzi5qmrcrdf6nsds4bvyq1hnvv9d1dlrvqrbzcrpxk84rcjwq1x"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (fw-dir (string-append %output "/lib/firmware"))
                (bin-dir (string-append fw-dir "/intel-ucode")))
           (mkdir-p bin-dir)
           (copy-recursively (string-append source "/intel-ucode") bin-dir)
           (copy-file
            (string-append source "/license")
            (string-append fw-dir "/LICENSE.intel-ucode"))
           #t))))
    (home-page
     "https://github.com/intel/Intel-Linux-Processor-Microcode-Data-Files")
    (synopsis "Processor microcode firmware for Intel CPUs")
    (description "Updated system processor microcode for Intel i686 and Intel
x86-64 processors.  Intel releases microcode updates to correct processor
behavior as documented in the respective processor specification updates.  The
@code{iucode-tool} package can be used to determine the appropriate file for
your CPU.")
    (license (nonfree "file://license"))))
