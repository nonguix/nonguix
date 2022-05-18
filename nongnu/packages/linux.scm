;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020, 2021 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2020, 2021, 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Risto Stevcev <me@risto.codes>
;;; Copyright © 2021 aerique <aerique@xs4all.nl>
;;; Copyright © 2022 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Remco van 't Veer <remco@remworks.net>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (nonguix licenses)
  #:export (corrupt-linux))

(define (linux-urls version)
  "Return a list of URLS for Linux VERSION."
  (list (string-append "https://www.kernel.org/pub/linux/kernel/v"
                       (version-major version) ".x/linux-" version ".tar.xz")))

(define* (corrupt-linux freedo version hash #:key (name "linux"))
  (package
    (inherit freedo)
    (name name)
    (version version)
    (source (origin
              (method url-fetch)
              (uri (linux-urls version))
              (sha256 (base32 hash))))
    (home-page "https://www.kernel.org/")
    (synopsis "Linux kernel with nonfree binary blobs included")
    (description
     "The unmodified Linux kernel, including nonfree blobs, for running Guix
System on hardware which requires nonfree software to function.")))

(define-public linux-5.17
  (corrupt-linux linux-libre-5.17 "5.17.9"
                 "0y2rmn86z3cvgv71b6sjjyafnlbanlib1kjpjjqzjbgg86y2890p"))

(define-public linux-5.15
  (corrupt-linux linux-libre-5.15 "5.15.41"
                 "07jrsr54rvhry3g401h58r1773zinq49dbrkb9v1p6q27gyb2z1w"))

(define-public linux-5.10
  (corrupt-linux linux-libre-5.10 "5.10.113"
                 "1z3dd5hrdbn2axsi2n70n41q1dq2dvg7s8aph1p6yiajpc16llc2"))

(define-public linux-5.4
  (corrupt-linux linux-libre-5.4 "5.4.145"
                 "1yb8vk5sbnyswylkpqw5i4n9cmnmlrfmbrnmy3nif579q8p7ixsw"))

(define-public linux-4.19
  (corrupt-linux linux-libre-4.19 "4.19.206"
                 "1h44lvzxd0cngj71bk8qba9dz7jlqj68ir6xjwfafglb81ppgsxp"))

(define-public linux-4.14
  (corrupt-linux linux-libre-4.14 "4.14.246"
                 "0fpgig84shpas1jc0h4s3aw9brkcq1as84gjbk4bfhc48bpi4mlw"))

(define-public linux-4.9
  (corrupt-linux linux-libre-4.9 "4.9.282"
                 "059fin4si93ya13xy831w84q496ksxidpd3kyw38918sfy4p6wk7"))

(define-public linux linux-5.17)
;; linux-lts points to the *newest* released long-term support version.
(define-public linux-lts linux-5.15)

(define-public linux-arm64-generic-5.17
  (corrupt-linux linux-libre-arm64-generic "5.17.1"
                 "092cx18va108lb27kxx2b00ma3l9g22nmkk81034apx26bacbmbw"
		 #:name "linux-arm64-generic"))

(define-public linux-arm64-generic-5.15
  (corrupt-linux linux-libre-arm64-generic "5.15.30"
                 "0ckiz985x88x68psg6wazyk7zpv34k8rbzpzyzj0gaph13za4ki5"
		 #:name "linux-arm64-generic"))

(define-public linux-arm64-generic linux-arm64-generic-5.17)

(define-public linux-arm64-generic-lts linux-arm64-generic-5.15)

(define-public linux-firmware
  (package
    (name "linux-firmware")
    (version "20211216")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                                  "/git/firmware/linux-firmware.git/snapshot/"
                                  "linux-firmware-" version ".tar.gz"))
              (sha256
               (base32
                "18qrlrkdzygmd9ihm7dziimkpzkfil50afnjwhfd88ic4gfkbxy0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
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

(define (select-firmware keep)
  "Modify linux-firmware copy list to retain only files matching KEEP regex."
  `(lambda _
     (use-modules (ice-9 regex))
     (substitute* "WHENCE"
       (("^(File|Link): *([^ ]*)(.*)" _ type file rest)
        (string-append (if (string-match ,keep file) type "Skip") ": " file rest)))))

(define-public amdgpu-firmware
  (package
    (inherit linux-firmware)
    (name "amdgpu-firmware")
    (arguments
     `(#:license-file-regexp "LICENSE.amdgpu"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware "^amdgpu/")))))))
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

(define-public radeon-firmware
  (package
    (inherit amdgpu-firmware)
    (name "radeon-firmware")
    (arguments
     `(#:license-file-regexp "LICENSE.radeon"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware "^radeon/")))))))
    (synopsis "Nonfree firmware for older AMD graphics chips")
    (description "Nonfree firmware for AMD graphics chips.  While most AMD
graphics cards can be run with the free Mesa, some cards require a nonfree
kernel module to run properly and support features like hibernation and
advanced 3D.")))

(define-public raspberrypi-firmware
(package
  (name "raspberrypi-firmware")
  (version "1.20220120")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/raspberrypi/firmware")
                  (commit version)))
            (modules '((guix build utils)
                       (ice-9 ftw)
                       (srfi srfi-26)))
            (snippet
             '(begin
                (for-each (lambda (name)
                            (delete-file-recursively name))
                          `("documentation" "extra" ".github" "hardfp" "modules" "opt" "README.md"
                            ,@(map (lambda (name)
                                     (string-append "boot/" name))
                                   (scandir "boot" (cut (file-name-predicate "^(kernel.*|COPYING\\.linux)$") <> #f)))))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0s75fw4n83bkh78xh5rdgpiyp1bkvv1v18pawl4cs9v4gjkn6pi2"))))
    (arguments
     '(#:install-plan
       '(("boot/" "."))))
  (build-system copy-build-system)
  (synopsis "Firmware for the Raspberry Pi boards")
  (description "Pre-compiled binaries of the current Raspberry Pi kernel
and modules, userspace libraries, and bootloader/GPU firmware.")
  (home-page "https://github.com/raspberrypi/firmware")
  (supported-systems '("armhf-linux" "aarch64-linux"))
  (license
    (list gpl2
	  (nonfree
	    (string-append "file://boot/LICENCE.broadcom"))
	  (nonfree
	    (string-append "file://opt/vc/LICENCE"))))))

(define-public atheros-firmware
  (package
    (inherit linux-firmware)
    (name "atheros-firmware")
    (arguments
     `(#:license-file-regexp "LICEN[CS]E.*[Aa]th"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware "^(ar[3579]|ath[1369]|htc_[79]|qca/|wil6)")))))))
    (synopsis "Nonfree firmware blobs for Atheros wireless cards")
    (description "Nonfree firmware blobs for Atheros wireless cards.  This
package contains nonfree firmware for the following chips:
@itemize
@item Atheros AR3012 rev 01020001 patch (ar3k/AthrBT_0x01020001.dfu)
@item Atheros AR3012 rev 01020200 patch (ar3k/AthrBT_0x01020200.dfu)
@item Atheros AR3012 rev 01020201 patch, version 170 (ar3k/AthrBT_0x01020201.dfu)
@item Atheros AR3012 rev 11020000 patch (ar3k/AthrBT_0x11020000.dfu)
@item Atheros AR3012 rev 11020100 patch (ar3k/AthrBT_0x11020100.dfu)
@item Atheros AR3012 rev 31010000 patch (ar3k/AthrBT_0x31010000.dfu)
@item Atheros AR3012 rev 31010100 patch (ar3k/AthrBT_0x31010100.dfu)
@item Atheros AR3012 rev 41020000 patch (ar3k/AthrBT_0x41020000.dfu)
@item Atheros AR3012 rev 01020001 config (ar3k/ramps_0x01020001_26.dfu)
@item Atheros AR3012 rev 01020200 26 MHz config (ar3k/ramps_0x01020200_26.dfu)
@item Atheros AR3012 rev 01020200 40 MHz config (ar3k/ramps_0x01020200_40.dfu)
@item Atheros AR3012 rev 01020201 26 MHz config (ar3k/ramps_0x01020201_26.dfu)
@item Atheros AR3012 rev 01020201 40 MHz config (ar3k/ramps_0x01020201_40.dfu)
@item Atheros AR3012 rev 11020000 config (ar3k/ramps_0x11020000_40.dfu)
@item Atheros AR3012 rev 11020100 config (ar3k/ramps_0x11020100_40.dfu)
@item Atheros AR3012 rev 31010000 config (ar3k/ramps_0x31010000_40.dfu)
@item Atheros AR3012 rev 31010100 config (ar3k/ramps_0x31010100_40.dfu)
@item Atheros AR3012 rev 41020000 config (ar3k/ramps_0x41020000_40.dfu)
@item Atheros AR5523 firmware (ar5523.bin)
@item Atheros AR7010 rev 1.0 firmware (ar7010.fw)
@item Atheros AR7010 rev 1.1 firmware (ar7010_1_1.fw)
@item Atheros AR9271 firmware (ar9271.fw)
@item Qualcomm Atheros QCA4019 rev 1.0 board configuration
(ath10k/QCA4019/hw1.0/board-2.bin)
@item Qualcomm Atheros QCA4019 rev 1.0 firmware, version 10.4-3.6-00140
(ath10k/QCA4019/hw1.0/firmware-5.bin)
@item Qualcomm Atheros QCA6174 rev 2.1 board configuration, version 1
(ath10k/QCA6174/hw2.1/board.bin)
@item Qualcomm Atheros QCA6174 rev 2.1 board configuration, version 2
(ath10k/QCA6174/hw2.1/board-2.bin)
@item Qualcomm Atheros QCA6174 rev 2.1 firmware, version
SW_RM.1.1.1-00157-QCARMSWPZ-1 (ath10k/QCA6174/hw2.1/firmware-5.bin)
@item Qualcomm Atheros QCA6174 rev 3.0 board configuration, version 1
(ath10k/QCA6174/hw3.0/board.bin)
@item Qualcomm Atheros QCA6174 rev 3.0 board configuration
(ath10k/QCA6174/hw3.0/board-2.bin)
@item Qualcomm Atheros QCA6174 rev 3.0 firmware, version
WLAN.RM.2.0-00180-QCARMSWPZ-1 (ath10k/QCA6174/hw3.0/firmware-4.bin)
@item Qualcomm Atheros QCA6174 rev 3.0 firmware, version
WLAN.RM.4.4.1-00079-QCARMSWPZ-1 (ath10k/QCA6174/hw3.0/firmware-6.bin)
@item Qualcomm Atheros QCA9377 rev 1.0 board configuration, version 1
(ath10k/QCA9377/hw1.0/board.bin)
@item Qualcomm Atheros QCA9377 rev 1.0 board configuration, version 2
(ath10k/QCA9377/hw1.0/board-2.bin)
@item Qualcomm Atheros QCA9377 rev 1.0 firmware, version WLAN.TF.1.0-00267-1
(ath10k/QCA9377/hw1.0/firmware-5.bin)
@item Qualcomm Atheros QCA9377 rev 1.0 firmware, version
WLAN.TF.2.1-00021-QCARMSWP-1 (ath10k/QCA9377/hw1.0/firmware-6.bin)
@item Qualcomm Atheros QCA9887 rev 1.0 board configuration, version 1
(ath10k/QCA9887/hw1.0/board.bin)
@item Qualcomm Atheros QCA9887 rev 1.0 firmware, version 10.2.4-1.0-00041
(ath10k/QCA9887/hw1.0/firmware-5.bin)
@item Qualcomm Atheros QCA9888 rev 2.0 board configuration, version 2
(ath10k/QCA9888/hw2.0/board-2.bin)
@item Qualcomm Atheros QCA9888 rev 2.0 firmware, version 10.4-3.9.0.2-00024
(ath10k/QCA9888/hw2.0/firmware-5.bin)
@item Qualcomm Atheros QCA988X board configuration, version 1
(ath10k/QCA988X/hw2.0/board.bin)
@item Qualcomm Atheros QCA988X firmware, version 10.2.4.45
(ath10k/QCA988X/hw2.0/firmware-4.bin)
@item Qualcomm Atheros QCA988X firmware, version 10.2.4-1.0-00043
(ath10k/QCA988X/hw2.0/firmware-5.bin)
@item Qualcomm Atheros QCA9984 rev 1.0 board configuration, version 2
(ath10k/QCA9984/hw1.0/board-2.bin)
@item Qualcomm Atheros QCA9984 rev 1.0 firmware, version 10.4-3.9.0.2-00021
(ath10k/QCA9984/hw1.0/firmware-5.bin)
@item Qualcomm Atheros QCA99X0 board configuration, version 1
(ath10k/QCA99X0/hw2.0/board.bin)
@item Qualcomm Atheros QCA99X0 firmware, version 10.4.1.00030-1
(ath10k/QCA99X0/hw2.0/firmware-5.bin)
@item Atheros AR3011 firmware (ath3k-1.fw)
@item ath6k/AR6003.1/hw2.1.1/athwlan.bin
@item ath6k/AR6003.1/hw2.1.1/bdata.SD31.bin
@item ath6k/AR6003.1/hw2.1.1/bdata.SD32.bin
@item ath6k/AR6003.1/hw2.1.1/bdata.WB31.bin
@item ath6k/AR6003.1/hw2.1.1/data.patch.bin
@item ath6k/AR6003.1/hw2.1.1/endpointping.bin
@item ath6k/AR6003.1/hw2.1.1/otp.bin
@item ath6k/AR6003/hw1.0/athwlan.bin.z77
@item ath6k/AR6003/hw1.0/bdata.SD31.bin
@item ath6k/AR6003/hw1.0/bdata.SD32.bin
@item ath6k/AR6003/hw1.0/bdata.WB31.bin
@item ath6k/AR6003/hw1.0/data.patch.bin
@item ath6k/AR6003/hw1.0/otp.bin.z77
@item ath6k/AR6003/hw2.0/athwlan.bin.z77
@item ath6k/AR6003/hw2.0/bdata.SD31.bin
@item ath6k/AR6003/hw2.0/bdata.SD32.bin
@item ath6k/AR6003/hw2.0/bdata.WB31.bin
@item ath6k/AR6003/hw2.0/data.patch.bin
@item ath6k/AR6003/hw2.0/otp.bin.z77
@item ath6k/AR6003/hw2.1.1/athwlan.bin
@item ath6k/AR6003/hw2.1.1/bdata.SD31.bin
@item ath6k/AR6003/hw2.1.1/bdata.SD32.bin
@item ath6k/AR6003/hw2.1.1/bdata.WB31.bin
@item ath6k/AR6003/hw2.1.1/data.patch.bin
@item ath6k/AR6003/hw2.1.1/endpointping.bin
@item ath6k/AR6003/hw2.1.1/fw-2.bin
@item ath6k/AR6003/hw2.1.1/fw-3.bin
@item ath6k/AR6003/hw2.1.1/otp.bin
@item ath6k/AR6004/hw1.2/bdata.bin
@item ath6k/AR6004/hw1.2/fw-2.bin
@item ath6k/AR6004/hw1.3/bdata.bin
@item ath6k/AR6004/hw1.3/fw-3.bin
@item Atheros AR7010 firmware, version 1.4.0 (ath9k_htc/htc_7010-1.4.0.fw)
@item Atheros AR9271 firmware, version 1.4.0 (ath9k_htc/htc_9271-1.4.0.fw)
@item Atheros AR7010 firmware, version 1.3.1 (htc_7010.fw)
@item Atheros AR9271 firmware, version 1.3.1 (htc_9271.fw)
@item Qualcomm WCN3990 Bluetooth firmware (qca/crbtfw21.tlv)
@item Qualcomm WCN3990 Bluetooth NVM configuration (qca/crnv21.bin)
@item Qualcomm Atheros QCA61x4 version 3.0 UART BT NVM configuration
(qca/nvm_00130300.bin)
@item Qualcomm Atheros QCA61x4 version 3.2 UART BT NVM configuration
(qca/nvm_00130302.bin)
@item Qualcomm Atheros QCA6174 BT NVM configuration (qca/nvm_00440302.bin)
@item Qualcomm Atheros QCA61x4 version 2.0 USB BT NVM configuration
(qca/nvm_usb_00000200.bin)
@item Qualcomm Atheros QCA61x4 version 2.1 USB BT NVM configuration
(qca/nvm_usb_00000201.bin)
@item Qualcomm Atheros QCA61x4 version 3.0 USB BT NVM configuration
(qca/nvm_usb_00000300.bin)
@item Qualcomm Atheros QCA61x4 version 3.2 USB BT NVM configuration
(qca/nvm_usb_00000302.bin)
@item Qualcomm Atheros QCA61x4 version 3.0 UART BT rampatch
(qca/rampatch_00130300.bin)
@item Qualcomm Atheros QCA61x4 version 3.2 UART BT rampatch
(qca/rampatch_00130302.bin)
@item Qualcomm Atheros QCA6174 BT rampatch (qca/rampatch_00440302.bin)
@item Qualcomm Atheros QCA61x4 version 2.0 USB BT rampatch
(qca/rampatch_usb_00000200.bin)
@item Qualcomm Atheros QCA61x4 version 2.1 USB BT rampatch
(qca/rampatch_usb_00000201.bin)
@item Qualcomm Atheros QCA61x4 version 3.0 USB BT rampatch
(qca/rampatch_usb_00000300.bin)
@item Qualcomm Atheros QCA61x4 version 3.2 USB BT rampatch
(qca/rampatch_usb_00000302.bin)
@item Qualcomm Atheros Wil62x0 default board parameters, version 5.2.0.18
(wil6210.brd)
@item Qualcomm Atheros Wil62x0 firmware, version 5.2.0.18 (wil6210.fw)
@end itemize")
    (license
     (list
      (nonfree
       (string-append
        "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
        "/linux-firmware.git/plain/LICENCE.atheros_firmware"))
      (non-copyleft
       (string-append
        "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
        "/linux-firmware.git/plain/LICENCE.open-ath9k-htc-firmware"))
      (nonfree
       (string-append
        "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
        "/linux-firmware.git/plain/LICENSE.QualcommAtheros_ar3k"))
      (nonfree
       (string-append
        "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
        "/linux-firmware.git/plain/LICENSE.QualcommAtheros_ath10k"))))))

(define-public ath3k-firmware
  (deprecated-package "ath3k-firmware" atheros-firmware))

(define-public ibt-hw-firmware
  (package
    (inherit linux-firmware)
    (name "ibt-hw-firmware")
    (arguments
     `(#:license-file-regexp "LICENCE.ibt_firmware"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware "^intel/ibt-")))))))
    (home-page "http://www.intel.com/support/wireless/wlan/sb/CS-016675.htm")
    (synopsis "Non-free firmware for Intel bluetooth chips")
    (description "This firmware is required by the btintel kernel module to
support many modern bluetooth Intel wireless cards (commonly found in
laptops).")
    (license
     (nonfree (string-append
               "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
               "/linux-firmware.git/plain/LICENCE.ibt_firmware")))))

(define-public iwlwifi-firmware
  (package
    (inherit linux-firmware)
    (name "iwlwifi-firmware")
    (arguments
     `(#:license-file-regexp "LICENCE.iwlwifi_firmware"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware "^iwlwifi-")))))))
    (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
    (synopsis "Nonfree firmware for Intel wifi chips")
    (description "The proprietary iwlwifi kernel module is required by many
modern Intel wifi cards (commonly found in laptops).  This blob enables
support for 5GHz and 802.11ac, among others.")
    (license
     (nonfree (string-append
               "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
               "/linux-firmware.git/plain/LICENCE.iwlwifi_firmware")))))

(define-public i915-firmware
  (package
    (inherit linux-firmware)
    (name "i915-firmware")
    (arguments
     `(#:license-file-regexp "LICENCE.i915"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware "^i915/")))))))
    (home-page "https://01.org/linuxgraphics/gfx-docs/drm/gpu/i915.html")
    (synopsis "Nonfree firmware for Intel integrated graphics")
    (description "This package contains the various firmware for Intel
integrated graphics chipsets, including GuC, HuC and DMC.")
    (license
     (nonfree (string-append
               "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
               "/linux-firmware.git/plain/LICENCE.i915")))))

(define-public realtek-firmware
  (package
    (inherit linux-firmware)
    (name "realtek-firmware")
    (arguments
     `(#:license-file-regexp "LICENCE.rtlwifi_firmware.txt"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware
                   "^(rtlwifi|rtl_nic|rtl_bt|rtw88|rtw89)/")))))))
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

(define-public rtl-nic-firmware
  (deprecated-package "rtl-nic-firmware" realtek-firmware))

(define-public rtl-bt-firmware
  (deprecated-package "rtl-bt-firmware" realtek-firmware))

(define-public rtl8192eu-linux-module
  (let ((commit "8396a4ebb4bde6b5c919d291838320f0e5b480dd")
        (revision "3"))
    (package
      (name "rtl8192eu-linux-module")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/clnhub/rtl8192eu-linux")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0kbfrvrfbi1r6if9vi7ccn0nc4lcqp85insiksyg3kg99mx78xhk"))))
      (build-system linux-module-build-system)
      (arguments
       `(#:make-flags
         (list "CC=gcc"
               (string-append "KSRC="
                              (assoc-ref %build-inputs "linux-module-builder")
                              "/lib/modules/build"))
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key (make-flags '()) #:allow-other-keys)
               (apply invoke "make" make-flags))))
         #:tests? #f))                  ; no test suite
      (home-page "https://github.com/clnhub/rtl8192eu-linux")
      (synopsis "Linux driver for Realtek RTL8192EU wireless network adapters")
      (description "This is Realtek's RTL8192EU Linux driver for wireless
network adapters.")
      ;; Rejected by Guix beause it contains a binary blob in:
      ;; hal/rtl8192e/hal8192e_fw.c
      (license gpl2))))

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
                       "broadcom-sta-linux-5.1.patch"
		       ;; source: https://github.com/NixOS/nixpkgs/commit/8ce65087c333097ab714d23800b69fc471ec48ca
                       "broadcom-sta-linux-5.6.patch"
                       "broadcom-sta-linux-5.9.patch"
                       "broadcom-sta-linux-5.10.patch")))
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
    (source
     (match (or (%current-target-system) (%current-system))
       ("x86_64-linux" broadcom-sta-x86_64-source)
       (_ broadcom-sta-i686-source)))
    (build-system linux-module-build-system)
    (arguments
     `(#:linux ,linux
       #:tests? #f))
    (supported-systems '("i686-linux" "x86_64-linux"))
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
@end itemize

It is recommended that anyone who uses this package stays with Linux LTS
releases.")
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

(define-public intel-microcode
  (package
    (name "intel-microcode")
    (version "20220419")
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
        (base32 "19gbprkjfanig5kaplafm69zm3vgqq1pn3qjgl4shaxj98wa2wwb"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       (let ((doc (string-append "share/doc/" ,name "-" ,version "/")))
         `(("intel-ucode" "lib/firmware/")
           ("README.md" ,doc)
           ("releasenote.md" ,doc)
           ("security.md" ,doc)))))
    (home-page
     "https://github.com/intel/Intel-Linux-Processor-Microcode-Data-Files")
    (synopsis "Processor microcode firmware for Intel CPUs")
    (description "Updated system processor microcode for Intel i686 and Intel
x86-64 processors.  Intel releases microcode updates to correct processor
behavior as documented in the respective processor specification updates.  The
@code{iucode-tool} package can be used to determine the appropriate file for
your CPU.")
    (license (nonfree "file://license"))))

(define-public amd-microcode
  (package
    (inherit linux-firmware)
    (name "amd-microcode")
    (arguments
     `(#:license-file-regexp "LICENSE.amd-ucode"
       ,@(substitute-keyword-arguments (package-arguments linux-firmware)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'select-firmware
                 ,(select-firmware "^amd-ucode/")))))))
    (synopsis "Processor microcode firmware for AMD CPUs")
    (description "Updated system processor microcode for AMD x86-64
processors.  AMD releases microcode updates to correct processor behavior as
documented in the respective processor revision guides.")
    (license
     (nonfree
      (string-append "https://git.kernel.org/pub/scm/linux/kernel/git/"
                     "firmware/linux-firmware.git/plain/LICENSE.amd-ucode")))))

(define-public sof-firmware
  (package
    (name "sof-firmware")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thesofproject/sof-bin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fb4rxgg3haxqg2gcm89g7af6v0a0h83c1ar2fyfa8h8pcf7hik7"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       (list (list (string-append "sof-v" ,version) "lib/firmware/intel/sof")
             (list (string-append "sof-tplg-v" ,version) "lib/firmware/intel/sof-tplg"))))
    (home-page "https://www.sofproject.org")
    (synopsis "Sound Open Firmware")
    (description "This package contains Linux firmwares and topology files for
audio DSPs that can be found on the Intel Skylake architecture.  Those
firmware can be built for source but need to be signed by Intel in order to be
loaded by Linux.")
    (license bsd-3)))
