;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020, 2021 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2020-2025 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020-2023, 2025 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Risto Stevcev <me@risto.codes>
;;; Copyright © 2021 aerique <aerique@xs4all.nl>
;;; Copyright © 2022 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2022, 2023, 2024, 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2022 Simen Endsjø <simendsjo@gmail.com>
;;; Copyright © 2022 Leo Famulari <leo@famulari.name>
;;; Copyright © 2023 Krzysztof Baranowski <pharcosyle@gmail.com>
;;; Copyright © 2023 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2023 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2023 Adam Kandur <rndd@tuta.io>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023, 2024, 2025 Ada Stevenson <adanskana@gmail.com>
;;; Copyright © 2023 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2023 PRESFIL <presfil@protonmail.com>
;;; Copyright © 2024, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 David Wilson <david@systemcrafters.net>
;;; Copyright © 2025 Murilo <murilo@disroot.org>
;;; Copyright © 2025 dan <i@dan.games>

(define-module (nongnu packages linux)
  #:use-module (gnu packages)
  #:use-module (nongnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages parallel)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (nonguix licenses)
  #:use-module (srfi srfi-1)
  #:export (corrupt-linux))

;; To customize the kernel with extra defconfig, use:
;;   (customize-linux #:linux linux
;;                    #:configs
;;                    '("CONFIG_A=y"
;;                      "CONFIG_B=m"
;;                      "# CONFIG_C is not set"))
;; See also the Guix Cookbook entry (search ‘customize-linux’):
;; https://guix.gnu.org/cookbook/en/html_node/Customizing-the-Kernel.html

(define (linux-url version)
  "Return a URL for Linux VERSION."
  (string-append "mirror://kernel.org"
                       "/linux/kernel/v" (version-major version) ".x"
                       "/linux-" version ".tar.xz"))

(define-public (nonguix-extra-linux-options linux-or-version)
  "Return a list containing additional options that nonguix sets by default
for a corrupted linux package of specified version.  linux-or-version can be
some freedo package or an output of package-version procedure."
  (define linux-version
    (if (package? linux-or-version)
        (package-version linux-or-version)
        linux-or-version))

  (reverse (fold (lambda (opt opts)
                   (if (version>=? linux-version (car opt))
                       (cons* (cdr opt) opts)
                       opts))
                 '()
                 ;; List of additional options for nonguix corrupted linux.
                 ;; Each member is a pair of a minimal version (>=) and the
                 ;; option itself.  Option has to be in a format suitable for
                 ;; (@ (guix build kconfig) modify-defconfig) procedure.
                 ;;
                 ;; Do note that this list is intended for enabling use of
                 ;; hardware requiring non-free firmware.  If a configuration
                 ;; option does work under linux-libre, it should go into Guix
                 ;; actual.
                 '(
                   ;; Driver for MT7612U-based wireless USB 3.0 dongles.
                   ("3.10" . "CONFIG_MT76x2U=m")
                   ;; Enable support for CIK (Sea Islands) asics
                   ;; Use module options to override this:
                   ;; radeon.cik_support=0 amdgpu.cik_support=1
                   ("3.10" . "CONFIG_DRM_AMDGPU_CIK=y")
                   ("3.10" . "CONFIG_DRM_AMDGPU_SI=y")
                   ;; Driver for MediaTek mt7921e wireless chipset
                   ("5.15" . "CONFIG_MT7921E=m")
                   ;; Activate driver module for RT1318/RT713 (Lunar Lake)
                   ;; Builds module named snd_soc_sof_sdw.ko
                   ("6.12" . "CONFIG_SND_SOC_INTEL_USER_FRIENDLY_LONG_NAMES=y")
                   ("6.12" . "CONFIG_SND_SOC_INTEL_SOUNDWIRE_SOF_MACH=m")))))

(define* (corrupt-linux freedo
                        #:key
                        (name "linux")
                        (configs "")
                        (defconfig "nonguix_defconfig")
                        (get-extra-configs nonguix-extra-linux-options)
                        modconfig)

  ;; TODO: This very directly depends on guix internals.
  ;; Throw it all out when we manage kernel hashes.
  (define gexp-inputs (@@ (guix gexp) gexp-inputs))

  (define linux-srcarch
    (@@ (gnu packages linux) linux-srcarch))

  (define extract-gexp-inputs
    (compose gexp-inputs force origin-uri))

  (define (find-source-hash sources url)
    (let ((versioned-origin
           (find (lambda (source)
                   (let ((uri (origin-uri source)))
                     (and (string? uri) (string=? uri url)))) sources)))
      (if versioned-origin
          (origin-hash versioned-origin)
          #f)))

  (let* ((version (package-version freedo))
         (url (linux-url version))
         (pristine-source (package-source freedo))
         (inputs (map gexp-input-thing (extract-gexp-inputs pristine-source)))
         (sources (filter origin? inputs))
         (hash (find-source-hash sources url)))
    (package
      (inherit
       (customize-linux
        #:name name
        #:linux
        (package
          (inherit freedo)
          (version version)
          (arguments
           (substitute-keyword-arguments (package-arguments freedo)
             ((#:phases phases '%standard-phases)
              #~(modify-phases #$phases
                  ;; Make sure the resulted package is compatible with
                  ;; ‘customize-linux’.
                  (add-before 'configure 'nonguix-configure
                    (lambda args
                      (let ((defconfig
                             (format #f "arch/~a/configs/nonguix_defconfig"
                                     #$(linux-srcarch))))
                        (apply (assoc-ref #$phases 'configure) args)
                        (modify-defconfig
                         ".config" '#$(get-extra-configs this-package))
                        (invoke "make" "savedefconfig")
                        (rename-file "defconfig" defconfig)))))))))
        #:source (origin
                   (method url-fetch)
                   (uri url)
                   (hash hash))
        #:configs configs
        #:defconfig defconfig
        #:modconfig modconfig))
      (home-page "https://www.kernel.org/")
      (synopsis "Linux kernel with nonfree binary blobs included")
      (description
       "The unmodified Linux kernel, including nonfree blobs, for running Guix System
on hardware which requires nonfree software to function."))))

(define-public linux-6.18
  (corrupt-linux linux-libre-6.18))

(define-public linux-6.17
  (corrupt-linux linux-libre-6.17))

(define-public linux-6.12
  (corrupt-linux linux-libre-6.12))

(define-public linux-6.6
  (corrupt-linux linux-libre-6.6))

(define-public linux-6.1
  (corrupt-linux linux-libre-6.1))

(define-public linux-5.15
  (corrupt-linux linux-libre-5.15))

(define-public linux-5.10
  (corrupt-linux linux-libre-5.10))

(define-public linux-5.4
  (corrupt-linux linux-libre-5.4))

(define-public linux linux-6.17)
;; linux-lts points to the *newest* released long-term support version.
(define-public linux-lts linux-6.12)

(define-public linux-arm64-generic-5.10
  (corrupt-linux linux-libre-arm64-generic-5.10 #:name "linux-arm64-generic"))

(define-public linux-arm64-generic-5.4
  (corrupt-linux linux-libre-arm64-generic-5.4 #:name "linux-arm64-generic"))

(define-public linux-arm64-generic
  (corrupt-linux linux-libre-arm64-generic #:name "linux-arm64-generic"))


;;;
;;; Linux-XanMod
;;;

(define* (make-linux-xanmod-source version xanmod-revision
                                   #:key xanmod-branch kernel-hash xanmod-hash)

  (define %upstream-linux-source
    (@@ (gnu packages linux) %upstream-linux-source))

  (define kernel-source
    (%upstream-linux-source (version-major+minor version) kernel-hash))

  (define xanmod-patch
    (origin
      (method url-fetch)
      (uri (string-append
            "mirror://sourceforge/xanmod/releases/" xanmod-branch "/"
            version "-" xanmod-revision "/patch-"
            version "-" xanmod-revision ".xz"))
      (sha256 xanmod-hash)))

  (origin
    (inherit kernel-source)
    (modules '((guix build utils)))
    (snippet
     #~(begin
         (let* ((xz-name (basename #+xanmod-patch))
                (patch-xz-name (string-append (string-drop-right xz-name 3)
                                              ".patch.xz"))
                (patch-name (string-drop-right patch-xz-name 3)))
           (copy-file #+xanmod-patch patch-xz-name)
           (invoke #+(file-append xz "/bin/unxz") patch-xz-name)
           (invoke #+(file-append patch "/bin/patch")
                   "--force" "--no-backup-if-mismatch"
                   #+@(origin-patch-flags kernel-source)
                   "--input" patch-name)
           (for-each delete-file
                     (list patch-name
                           ;; EXTRAVERSION is used instead.
                           "localversion")))))))

(define* (make-linux-xanmod version xanmod-revision source
                            #:key
                            (name "linux-xanmod")
                            (xanmod-defconfig "config_x86-64-v1"))

  (define %default-extra-linux-options
    ((@@ (gnu packages linux) default-extra-linux-options) version))

  (define config->string
    (@@ (gnu packages linux) config->string))

  (define base-kernel
    (customize-linux
     #:name name
     #:source source
     #:defconfig xanmod-defconfig
     ;; EXTRAVERSION is used instead.
     #:configs (config->string
                '(("CONFIG_LOCALVERSION" . "")))
     #:extra-version xanmod-revision))

  (package
    (inherit base-kernel)
    (version version)
    (arguments
     (substitute-keyword-arguments (package-arguments base-kernel)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Since `customize-linux' replaces the configure phase, we add
            ;; XanMod defconfig beforehand to ensure compatibility of the
            ;; resulting package with `customize-linux'.
            (add-before 'configure 'add-xanmod-defconfig
              (lambda _
                (rename-file
                 (string-append "CONFIGS/xanmod/gcc/" #$xanmod-defconfig)
                 ".config")

                ;; Adapted from `make-linux-libre*'.
                (chmod ".config" #o666)
                (let ((port (open-file ".config" "a"))
                      (extra-configuration
                       #$(config->string
                          (append %default-extra-linux-options
                                  ;; NOTE: These are configs expected by Guix
                                  ;; but missing from XanMod defconfig.
                                  '(("CONFIG_BLK_DEV_NVME" . #t))))))
                  (display extra-configuration port)
                  (close-port port))
                (invoke "make" "oldconfig")

                (rename-file
                 ".config"
                 (string-append "arch/x86/configs/" #$xanmod-defconfig))))))))
    (native-inputs
     (modify-inputs (package-native-inputs base-kernel)
       ;; cpio is needed for CONFIG_IKHEADERS.
       (prepend cpio zstd)))
    (home-page "https://xanmod.org/")
    (supported-systems '("x86_64-linux"))
    (synopsis "Linux kernel distribution with custom settings and new features")
    (description
     "This package provides XanMod kernel, a general-purpose Linux kernel
distribution with custom settings and new features.  It's built to provide a
stable, responsive and smooth desktop experience.")))

;; Linux-XanMod sources
(define-public linux-xanmod-version "6.12.15")
(define-public linux-xanmod-revision "xanmod1")
(define-public linux-xanmod-source
  (make-linux-xanmod-source
   linux-xanmod-version
   linux-xanmod-revision
   #:xanmod-branch "main"
   #:kernel-hash (base32 "1sr58vsh39hdwk0z27lg14isqwn4g8m4r7a8z2rsyhkfwlmmd8mi")
   #:xanmod-hash (base32 "00la3fqwjzb35k6rig59apzxw905f1sh6zwx0sxg7k3dhp1gys9d")))

(define-public linux-xanmod-lts-version "6.12.41")
(define-public linux-xanmod-lts-revision "xanmod1")
(define-public linux-xanmod-lts-source
  (make-linux-xanmod-source
   linux-xanmod-lts-version
   linux-xanmod-lts-revision
   #:xanmod-branch "lts"
   #:kernel-hash (base32 "1sr58vsh39hdwk0z27lg14isqwn4g8m4r7a8z2rsyhkfwlmmd8mi")
   #:xanmod-hash (base32 "1k9p8ap2ghzn7qkbfj6rgd75wqp23xh32am1rf3k1j1j4j1k6gxx")))

;; Linux-XanMod packages
(define-public linux-xanmod
  (deprecated-package "linux-xanmod" linux)
  ;; FIXME: Unable to boot.
  #;(make-linux-xanmod linux-xanmod-version
                     linux-xanmod-revision
                     linux-xanmod-source))

(define-public linux-xanmod-lts
  (make-linux-xanmod linux-xanmod-lts-version
                     linux-xanmod-lts-revision
                     linux-xanmod-lts-source))


;;;
;;; Firmwares
;;;

(define-public linux-firmware
  (package
    (name "linux-firmware")
    (version "20260110")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/kernel/firmware/"
                                  "linux-firmware-" version ".tar.xz"))
              (sha256
               (base32
                "0ca3f6yl4szyyrkajq51n6qwahj4m4vd5jcssbl4x7skvdk53q28"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f
           #:strip-binaries? #f
           #:validate-runpath? #f
           #:make-flags #~(list (string-append "DESTDIR=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-out-check_whence.py
                 (lambda _
                   ;; The 'check_whence.py' script requires git (and the
                   ;; repository metadata).
                   (substitute* "copy-firmware.sh"
                     (("./check_whence.py")
                      "true"))))
               (delete 'configure)
               (replace 'install
                 ;; Use Zstd compression to reduce space requirements.
                 (lambda* (#:key (parallel-build? #t) (make-flags '())
                           #:allow-other-keys)
                   (let ((num-jobs (if parallel-build?
                                       (number->string (parallel-job-count))
                                       "1")))
                     ;; Use the best 'standard' compression level.
                     (setenv "ZSTD_CLEVEL" "19")
                     ;; Compress using multiple threads.
                     (setenv "ZSTD_NBTHREADS" num-jobs)
                     (apply invoke "make" "install-zst" "-j" num-jobs
                            make-flags)))))))
    (native-inputs (list parallel rdfind zstd))
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
  #~(lambda _
      (use-modules (ice-9 regex))
      (substitute* "WHENCE"
        (("^(File|RawFile): *([^ ]*)(.*)" _ type file rest)
         (string-append (if (string-match #$keep file) type "Skip") ": " file rest))
        (("^Link: *(.*) *-> *(.*)" _ file target)
         (string-append (if (string-match #$keep target) "Link" "Skip")
                        ": " file " -> " target)))))

(define-public amdgpu-firmware
  (package
    (inherit linux-firmware)
    (name "amdgpu-firmware")
    (arguments
     (cons* #:license-file-regexp "LICENSE.amdgpu"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^amdgpu/")))))))
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
     (cons* #:license-file-regexp "LICENSE.radeon"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^radeon/")))))))
    (synopsis "Nonfree firmware for older AMD graphics chips")
    (description "Nonfree firmware for AMD graphics chips.  While most AMD
graphics cards can be run with the free Mesa, some cards require a nonfree
kernel module to run properly and support features like hibernation and
advanced 3D.")))

(define-public raspberrypi-firmware
(package
  (name "raspberrypi-firmware")
  (version "1.20220331")
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
              "1hd1vkghkgdlmw04fap28f68nsf7d7i8dq6h9r4xa0h9y4f6j6ag"))))
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
     (cons* #:license-file-regexp "LICEN[CS]E.*[Aa]th"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^(ar[3579]|ath[1369]|htc_[79]|qca/|wil6)")))))))
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
     (cons* #:license-file-regexp "LICENCE.ibt_firmware"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^intel/ibt-")))))))
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
     (cons* #:license-file-regexp "LICENCE.iwlwifi_firmware"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^intel/iwlwifi/")))))))
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
     (cons* #:license-file-regexp "LICENCE.i915"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^i915/")))))))
    (home-page "https://01.org/linuxgraphics/gfx-docs/drm/gpu/i915.html")
    (synopsis "Nonfree firmware for Intel integrated graphics")
    (description "This package contains the various firmware for Intel
integrated graphics chipsets, including GuC, HuC and DMC.")
    (license
     (nonfree (string-append
               "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
               "/linux-firmware.git/plain/LICENCE.i915")))))

(define-public mediatek-firmware
  (package
    (inherit linux-firmware)
    (name "mediatek-firmware")
    (arguments
     (cons* #:license-file-regexp "LICENCE\\.(mediatek\
|ralink_a_mediatek_company_firmware)"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^mediatek/")))))))
    (synopsis
     "Nonfree firmware for MediaTek chips")
    (description
     "Nonfree firmware for MediaTek Ethernet, WiFi, Bluetooth, Video Processing
Units (VPU), etc. chips.  This package contains nonfree firmware for the
following drivers/chips:
@table @code
@item mt7601u
MediaTek MT7601U firmware (mediatek/mt7601u.bin)
@item btmtk_usb
MediaTek Bluetooth USB driver firmware (mediatek/mt7650.bin,
mediatek/mt7622pr2h.bin, mediatek/mt7668pr2h.bin)
@item mtk-vpu
MediaTek VPU video processing unit driver (mediatek/mt8173/vpu_d.bin
and mediatek/mt8173/vpu_p.bin)
@item mtk_scp
MediaTek SCP System Control Processing Driver (mediatek/mt8183/scp.img,
mediatek/mt8186/scp.img, mediatek/mt8192/scp.img and mediatek/mt8195/scp.img)
@item mt76x0
MediaTek MT76x0 Wireless MACs (mediatek/mt7610u.bin,
mediatek/mt7610e.bin and mediatek/mt7650e.bin)
@item mt76x2e
MediaTek MT76x2 Wireless MACs
(mediatek/mt7662.bin and mediatek/mt7662_rom_patch.bin)
@item mt76x2u
MediaTek MT76x2u Wireless MACs
(mediatek/mt7662u.bin and mediatek/mt7662u_rom_patch.bin)
@item mt7615e
MediaTek MT7615e Wireless MACs (mediatek/mt7615_n9.bin,
mediatek/mt7615_cr4.bin and mediatek/mt7615_rom_patch.bin)
@item mt7622
MediaTek MT7622 Wireless MACs (mediatek/mt7622_n9.bin and
mediatek/mt7622_rom_patch.bin)
@item mt7663
MediaTek MT7663 Wireless MACs (mediatek/mt7663pr2h.bin,
mediatek/mt7663_n9_v3.bin, mediatek/mt7663pr2h_rebb.bin and
mediatek/mt7663_n9_rebb.bin)
@item mt7915e
MediaTek Wireless MACs for MT7915/MT7916/MT7986/MT7981
(mediatek/mt7915_wm.bin, mediatek/mt7915_wa.bin, mediatek/mt7915_rom_patch.bin,
mediatek/mt7915_eeprom.bin, mediatek/mt7915_eeprom_dbdc.bin,
mediatek/mt7916_wm.bin, mediatek/mt7916_wa.bin, mediatek/mt7916_rom_patch.bin,
mediatek/mt7916_eeprom.bin, mediatek/mt7986_wm.bin,
mediatek/mt7986_wm_mt7975.bin, mediatek/mt7986_wa.bin,
mediatek/mt7986_rom_patch.bin, mediatek/mt7986_rom_patch_mt7975.bin,
mediatek/mt7986_wo_0.bin, mediatek/mt7986_wo_1.bin,
mediatek/mt7986_eeprom_mt7976.bin, mediatek/mt7986_eeprom_mt7976_dbdc.bin,
mediatek/mt7986_eeprom_mt7976_dual.bin, mediatek/mt7986_eeprom_mt7975_dual.bin,
mediatek/mt7981_wm.bin, mediatek/mt7981_wa.bin, mediatek/mt7981_rom_patch.bin,
and mediatek/mt7981_wo.bin)
@item mt7921
MediaTek MT7921 Wireless MACs
(mediatek/WIFI_MT7961_patch_mcu_1_2_hdr.bin and
mediatek/WIFI_RAM_CODE_MT7961_1.bin)
@item mt7921
MediaTek MT7921 bluetooth chipset (BT_RAM_CODE_MT7961_1_2_hdr.bin)
@item mt7922
MediaTek MT7922 Wireless MACs
(mediatek/WIFI_MT7922_patch_mcu_1_1_hdr.bin and
mediatek/WIFI_RAM_CODE_MT7922_1.bin)
@item mt7922
MediaTek MT7922 bluetooth chipset
(mediatek/BT_RAM_CODE_MT7922_1_1_hdr.bin)
@item mt7925
MediaTek MT7925 Wireless MACs
(mediatek/mt7925/WIFI_MT7925_PATCH_MCU_1_1_hdr.bin and
mediatek/mt7925/WIFI_RAM_CODE_MT7925_1_1.bin)
@item mt7925
MediaTek MT7925 bluetooth chipset
(mediatek/mt7925/BT_RAM_CODE_MT7925_1_1_hdr.bin)
@item mt7988
MediaTek MT7988 Internal 2.5G Ethernet Phy
(mediatek/mt7988/i2p5ge-phy-pmb.bin)
@item mt7996e
MediaTek Wireless MACs for MT7996 (mediatek/mt7996/mt7996_wm.bin,
mediatek/mt7996/mt7996_wa.bin, mediatek/mt7996/mt7996_rom_patch.bin,
mediatek/mt7996/mt7996_dsp.bin and mediatek/mt7996/mt7996_eeprom.bin)
@item mtk-sof
MediaTek Sound Open Firmware driver (mediatek/sof/sof-mt8186.ri,
mediatek/sof/sof-mt8186.ri, mediatek/sof/sof-mt8186.ldc,
mediatek/sof-tplg/sof-mt8186.tplg, mediatek/sof/sof-mt8195.ri,
mediatek/sof/sof-mt8195.ldc and
mediatek/sof-tplg/sof-mt8195-mt6359-rt1019-rt5682.tplg)
@end table")
    (license
     (list (nonfree
            (string-append
             "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
             "/linux-firmware.git/plain/LICENCE.mediatek"))
           (nonfree
            (string-append
             "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
             "/linux-firmware.git/plain"
             "/LICENCE.ralink_a_mediatek_company_firmware"))))))

(define-public realtek-firmware
  (package
    (inherit linux-firmware)
    (name "realtek-firmware")
    (arguments
     (cons* #:license-file-regexp "LICENCE.rtlwifi_firmware.txt"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^(rtlwifi|rtl_nic|rtl_bt|rtw88|rtw89)/")))))))
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
  (let ((commit "ed045d0e04303dce7b7b174abcb27bd24d207ee6")
        (revision "9"))
    (package
      (name "rtl8192eu-linux-module")
      (version (git-version "5.11.2.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/clnhub/rtl8192eu-linux")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0lklpn1gl202i49xjrd735pl595ynh2wpkdk1nlpcvxssi3qi0l7"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:tests? #f                ; no test suite
             #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target)))
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'build
                   (lambda* (#:key (make-flags '()) (parallel-build? #t) inputs
                             #:allow-other-keys)
                     (apply invoke "make"
                            (string-append "KSRC="
                                           (search-input-directory
                                            inputs "lib/modules/build"))
                            `(,@(if parallel-build?
                                    `("-j" ,(number->string
                                             (parallel-job-count)))
                                    '())
                              ,@make-flags)))))))
      (home-page "https://github.com/clnhub/rtl8192eu-linux")
      (synopsis "Linux driver for Realtek RTL8192EU wireless network adapters")
      (description "This is Realtek's RTL8192EU Linux driver for wireless
network adapters.")
      ;; Rejected by Guix beause it contains a binary blob in:
      ;; hal/rtl8192e/hal8192e_fw.c
      (license gpl2))))

(define-public rtl8821ce-linux-module
  (let ((commit "4e6b887f0d8c4091a4df9da9fcead9a8294b41ad")
        (revision "15"))
    (package
      (name "rtl8821ce-linux-module")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tomaspinho/rtl8821ce")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0dn1cmp8gn2g42q67lny15rrf092kw2rrb3gxc1qn07hbklj73bx"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:tests? #f                ; no test suite
             #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target)))
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'build
                   (lambda* (#:key (make-flags '()) (parallel-build? #t) inputs
                             #:allow-other-keys)
                     (apply invoke "make"
                            (string-append "KSRC="
                                           (search-input-directory
                                            inputs "lib/modules/build"))
                            `(,@(if parallel-build?
                                    `("-j" ,(number->string
                                             (parallel-job-count)))
                                    '())
                              ,@make-flags)))))))
      (home-page "https://github.com/tomaspinho/rtl8821ce")
      (synopsis "Linux driver for Realtek RTL8821CE wireless network adapters")
      (description "This is Realtek's RTL8821CE Linux driver for wireless
network adapters.")
      ;; Rejected by Guix beause it contains a binary blob in:
      ;; hal/rtl8821c/hal8821c_fw.c
      (license gpl2))))

(define-public rtl8821cu-linux-module
  (let ((commit "7f63a9da2e8ed83403f6f920e9b1628a37b38ef4")
        (revision "4"))
    (package
      (name "rtl8821cu-linux-module")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/morrownr/8821cu-20210916")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ril9xnv3bbmfj9yynm3nzpdgjyh0a93rag51hia7ix6ppm8w0a6"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:tests? #f                     ; no test suite
             #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target)))
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'build
                   (lambda* (#:key (make-flags '()) (parallel-build? #t) inputs
                             #:allow-other-keys)
                     (apply invoke "make"
                            (string-append "KSRC="
                                           (search-input-directory
                                            inputs "lib/modules/build"))
                            `(,@(if parallel-build?
                                    `("-j" ,(number->string
                                             (parallel-job-count)))
                                    '())
                              ,@make-flags)))))))
      (home-page "https://github.com/morrownr/8821cu-20210916")
      (synopsis "Linux driver for Realtek USB WiFi adapters")
      (description
       "Linux driver for USB WiFi adapters that are based on the
Realtek RTL8811CU, RTL8821CU, RTL8821CUH and RTL8731AU chipsets.

To work, in addition to installing the driver, you need
to disable the conflicting rtw88 driver:

@example
(operating-system
  ;; ...
  ;; Blacklist conflicting kernel modules.
  (kernel-arguments '(\"modprobe.blacklist=rtw88_8821cu\"))
  (kernel-loadable-modules (list rtl8821cu-linux-module)))
@end example")
      ;; Rejected by Guix beause it contains a binary blob in:
      ;; hal/rtl8821c/hal8821c_fw.c
      (license (nonfree
                "https://github.com/morrownr/8821cu-20210916/blob/main/LICENSE")))))

(define-public rtl8812au-aircrack-ng-linux-module
  (let ((commit "c3fb89a2f7066f4bf4e4d9d85d84f9791f14c83e")
        (revision "14"))
    (package
      (inherit rtl8821ce-linux-module)
      (name "rtl8812au-aircrack-ng-linux-module")
      (version (git-version "5.6.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aircrack-ng/rtl8812au")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0skq0kr4h4nr8a4c7mfsf9rjar4q6cahwilgg2l0if844ib1rj02"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              ;; Remove bundled tarballs, APKs, word lists, speadsheets,
              ;; and other unnecessary unlicenced things.
              (for-each delete-file-recursively (list "android"
                                                      "docs"
                                                      "tools"))))))
      (supported-systems '("x86_64-linux" "i686-linux"))
      (home-page "https://github.com/aircrack-ng/rtl8812au")
      (synopsis "Linux driver for Realtek USB wireless network adapters")
      (description
       "This is Realtek's rtl8812au Linux driver for USB 802.11n wireless
network adapters, modified by the aircrack-ng project to support monitor mode
and frame injection.  It provides a @code{88XXau} kernel module that supports
RTL8812AU, RTL8821AU, and RTL8814AU chips.")
      ;; Rejected by Guix beause it contains a binary blob in:
      ;; hal/rtl8812a/hal8812a_fw.c
      (license gpl2+))))

(define-public r8168-linux-module
  (package
    (name "r8168-linux-module")
    (version "8.052.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mtorromeo/r8168")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01mi7hh92nc7jaxkfrpz7j0ci78djrhgmq0im4k1270mwmvr0yzj"))))
    (arguments
     (list #:tests? #f))
    (build-system linux-module-build-system)
    (home-page "https://github.com/mtorromeo/r8168")
    (synopsis "Linux driver for Realtek PCIe network adapters")
    (description
     "Linux driver for Realtek PCIe network adapters.  If the r8169 kernel module is
giving you trouble, you can try this module.")
    (license gpl2)))

(define broadcom-sta-version "6.30.223.271")

(define broadcom-sta-x86_64-source
  (origin
    (method url-fetch/tarbomb)
    (uri (string-append "https://docs.broadcom.com/docs-and-downloads/"
                        "docs/linux_sta/hybrid-v35_64-nodebug-pcoem-"
                        (string-replace-substring broadcom-sta-version "." "_")
                        ".tar.gz"))
    (patches
     ;; Keep these in sync with the patches at
     ;; https://github.com/rpmfusion/wl-kmod
     ;; They seem to be good about keeping broadcom patches up to date so updating
     ;; for a new kernel release should be as simple as chaging the commit to
     ;; the newest available and adding any new patches.
     (map (lambda (name hash)
            (let ((commit "7786b3a3e54962124d24b4b61a6472bb0c4bbd94"))
              (origin
                (method url-fetch)
                (uri (string-append
                      "https://raw.githubusercontent.com/rpmfusion/wl-kmod/"
                      commit "/" name))
                (sha256 (base32 hash)))))
          ;; find * -name '*.patch' -printf '"%p"\n'
          '("wl-kmod-001_wext_workaround.patch"
            "wl-kmod-002_kernel_3.18_null_pointer.patch"
            "wl-kmod-003_gcc_4.9_remove_TIME_DATE_macros.patch"
            "wl-kmod-004_kernel_4.3_rdtscl_to_rdtsc.patch"
            "wl-kmod-005_kernel_4.7_IEEE80211_BAND_to_NL80211_BAND.patch"
            "wl-kmod-006_gcc_6_fix_indentation_warnings.patch"
            "wl-kmod-007_kernel_4.8_add_cfg80211_scan_info_struct.patch"
            "wl-kmod-008_fix_kernel_warnings.patch"
            "wl-kmod-009_kernel_4.11_remove_last_rx_in_net_device_struct.patch"
            "wl-kmod-010_kernel_4.12_add_cfg80211_roam_info_struct.patch"
            "wl-kmod-011_kernel_4.14_new_kernel_read_function_prototype.patch"
            "wl-kmod-012_kernel_4.15_new_timer.patch"
            "wl-kmod-013_gcc8_fix_bounds_check_warnings.patch"
            "wl-kmod-014_kernel_read_pos_increment_fix.patch"
            "wl-kmod-015_kernel_5.1_get_ds_removed.patch"
            "wl-kmod-016_fix_unsupported_mesh_point.patch"
            "wl-kmod-017_fix_gcc_fallthrough_warning.patch"
            "wl-kmod-018_kernel_5.6_adaptations.patch"
            "wl-kmod-019_kernel_5.9_segment_eq_removed.patch"
            "wl-kmod-020_kernel_5.10_get_set_fs_removed.patch"
            "wl-kmod-021_kernel_5.17_adaptation.patch"
            "wl-kmod-022_kernel_5.18_adaptation.patch"
            "wl-kmod-023_kernel_6.0_adaptation.patch"
            "wl-kmod-024_kernel_6.1_adaptation.patch"
            "wl-kmod-025_kernel_6.5_adaptation.patch"
            "wl-kmod-026_kernel_6.10_fix_empty_body_in_if_warning.patch"
            "wl-kmod-027_wpa_supplicant-2.11_add_max_scan_ie_len.patch"
            "wl-kmod-028_kernel_6.12_adaptation.patch"
            "wl-kmod-029_kernel_6.13_adaptation.patch"
            "wl-kmod-030_kernel_6.14_adaptation.patch"
            "wl-kmod-031_replace_EXTRA_CFLAGS_EXTRA_LDFLAGS_with_ccflags-y_ldflags-y.patch"
            "wl-kmod-032_add_MODULE_DESCRIPTION_macro.patch"
            "wl-kmod-033_disable_objtool_add_warning_unmaintained.patch"
            "wl-kmod-034_kernel_6.15_adaptation_replace_del_timer_with_timer_delete.patch"
            "wl-kmod-035_kernel_6.17_adaptation_fix_functions_prototypes.patch")
          ;; find * -name '*.patch' -exec sh -c 'printf "\"%s\"\n" "$(guix hash {})"' \;
          '("0lxj36r90kxhlfm3r0byh3p2l1dw9ama658ysfwib08ln5322mrq"
            "0h8wck6akys7dxxwqgax9x7mninfx3ypy2af3drrarksmcvv4v2j"
            "0azlhznn9amq3dr90drmh3gqva8i18dg16h8hwfbv284m04zy7d1"
            "1x6py9p74hh3ic82gw41ixla13kxbm85a6zqphk0q3xl903d5ywm"
            "1fgpnsmy4388gls1mvq9zc83j29ypfcx61a3rnzczmcxi0rlx7jq"
            "0a5np0d03gpgacvl3s4dsk4pcqv2g61jcjn1h7m40gs2p48i78gc"
            "0mig5p5gf2als6hw0fj0h8d3m3415nnc8256y2spb3kbq95fgm6d"
            "1ab148xw5l9z0izzlkc9ffb4k65x7lj151m8kpqyy080dv1kb9bl"
            "0fw0vizl3y3884z97cli8xmfviqlljmximrd16ibxbv1zavj87bq"
            "1k9p8pylsik1byzv3rbzpcd31wiibclm7ldnwf3g3kdzcjrhg938"
            "0cxxmq4fggcg40pl3f7s2xk1j0l1kill5d8xwv0jp0m4y0qxs5hi"
            "06yxhh6fp4cvjpi4a2prp8ls0v2jid6jhpifwifskzribvfkfc5l"
            "11kfbaviipkw7w3yn0sacjr5hsgp6x0mg570yzkjn0kxzgnnchna"
            "1x25qpc4494qy80xxx17qa6kl1jshcbnbkc43pzhzb58xazl7ays"
            "1qf3gi57p7jhzd2fgy6r3alc4dl8kymwwq3lyrx5yg73zb5dbcg7"
            "0hx2xq9mz0ndljvn634fcivz9mmjj2ij6z8riy6fkbic11g1g6pq"
            "056k8ayi26nbd8bkn8cr20i6diykg8n7j3vwc6ay40ch66l3jpr1"
            "11w07p1qszdlg0pzac67s4fq380w5dskkg5afnxya3w74gi4bf65"
            "0rw0sn8x479xjvrbfrn3qykd6hrmyfz4j6bdjw62ainjslxigcaq"
            "13jw2vmc16zfvpzf73xzdyhkbihrn6yvwsd58vlbgzmygb5x2kp3"
            "1l9kmpn3lbpz2p7p8x8zqmpj3j6m64sl1y4s5mn4dgli051iiy62"
            "0nc3q6dayl6lyjkjs1qymvifbpymq1c4d9wzbj4bzb5wm3xq43mj"
            "0isib9aawasza0a1kiqpzi072df7i5mn25b6j6szxh4y0ranlna3"
            "0pmb2av9aryf38li3k86hsc4xdqd547yapj4ccqbhqnpsdyik22v"
            "11mx81ij702w60xv6wc7z5w5i81ay0a88rhwbhg0qw2wb9jh6jqa"
            "0ggdbqci5ic0way1bqlw5h45lszfnv28ayhiwiiwlw7wbxp9f5vs"
            "138w812969d54wa184jaghmi0wmkvfi63mnswcyxg10ykaglk1rn"
            "14wc1ba6kcg8b9prvzvhi0wplq5m3cnbghybi8i2kxnh1zyw53gk"
            "0ys1n01ydhafrqqv68c0ijm39m491xp9qacinzf6svpirbs0ns84"
            "0ky90dz8gkf5z1h0253a1g8z6ixypm4jfrzb1x9z415m2al4afc7"
            "17l6q016l1xw55l79v64qfgj50p5s8lskw0vid6785b9va0ifknj"
            "1kl77y3n73piyqdvr0brs53bydid1pjrkkvvaqyzz3pjrm69xvbg"
            "19xbxxfi4yqqcvf40xspidznr3f6f900rmp0b9nfb2727a51l4p7"
            "119ya966kg4ikjw51zsvbl9wkrjx82bhvznq9c8csr2v4n9h7h90"
            "0n5n6pasxqk8x40hjkginm4fb36bi3waqr2z3l2f30i0aqqmlsrx")))
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
     `(#:modules ((guix build utils))
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

(define-public facetimehd
  (package
    (name "facetimehd")
    (version "0.6.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/patjak/facetimehd")
             (commit version)))
       (file-name (git-file-name "facetimehd" version))
       (sha256
        (base32
         "0vv6f8k9p6kp9fgm1iwfagkdnbmvfi1049s8p1rfcd8xsd0ch46w"))))
    (build-system linux-module-build-system)
    (arguments
     '(#:tests? #f))
    (synopsis "Linux driver for the FacetimeHD (Broadcom 1570) PCIe webcam")
    (description "Linux driver for the FacetimeHD webcam.  According to Apple the
following models contain a Facetime HD camera and should be compatible with this
driver:
@itemize
@item iMac (21,5\", since mid 2011)
@item iMac (27\", since mid 2011)
@item MacBook Air (since mid 2011)
@item MacBook Pro (15\", since early 2011)
@item MacBook Pro (17\", since early 2011)
@item MacBook Pro (13\", since early 2011)
@item Thunderbolt display
@end itemize")
    (home-page "https://github.com/patjak/facetimehd")
    (license gpl2)
    (supported-systems '("i686-linux" "x86_64-linux"))))

(define-public intel-microcode
  (package
    (name "intel-microcode")
    (version "20250512")
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
        (base32 "0da4aj0hpj6xn9vfb0k7mznndv53ihbv3mhjz71skwmy1vbibay5"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~(let ((doc (string-append "share/doc/" #$name "-" #$version "/")))
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
     (cons* #:license-file-regexp "LICENSE.amd-ucode"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^amd-ucode/")))))))
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
    (version "2025.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/thesofproject/sof-bin/releases/download/v"
                           version "/sof-bin-" version ".tar.gz"))
       (sha256
        (base32
         "1k3bm2w5r49nl995jh0zjas4sjzqzdiii43j2vdwpirq1lxn1wp2"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("sof" "lib/firmware/intel/sof")
         ("sof-ace-tplg" "lib/firmware/intel/sof-ace-tplg")
         ("sof-ipc4" "lib/firmware/intel/sof-ipc4")
         ("sof-ipc4-tplg" "lib/firmware/intel/sof-ipc4-tplg")
         ("sof-tplg" "lib/firmware/intel/sof-tplg")
         ("sof-ipc4-lib" "lib/firmware/intel/sof-ipc4-lib"))))
    (home-page "https://www.sofproject.org")
    (synopsis "Sound Open Firmware")
    (description "This package contains Linux firmwares and topology files for
audio DSPs that can be found on the Intel Skylake architecture.  This
firmware can be built from source but need to be signed by Intel in order to be
loaded by Linux.")
    (license bsd-3)))

(define-public mali-csf-firmware
  (package
    (inherit linux-firmware)
    (name "mali-csf-firmware")
    (arguments
     (cons* #:license-file-regexp "LICENCE.mali_csffw"
            (substitute-keyword-arguments (package-arguments linux-firmware)
              ((#:phases phases #~%standard-phases)
               #~(modify-phases #$phases
                   (add-after 'unpack 'select-firmware
                     #$(select-firmware "^arm/mali/arch10.8/mali_csffw.bin")))))))
    (home-page "https://developer.arm.com/Architectures/Valhall")
    (synopsis "Nonfree firmware for ARM Mali Valhall 3rd generation GPUs")
    (description "Nonfree firmware for ARM Mali Valhall 3rd generation GPUs.
This package is required for the Panthor kernel driver.")
    (license
     (nonfree (string-append
               "https://git.kernel.org/pub/scm/linux/kernel/git/firmware"
               "/linux-firmware.git/plain/LICENCE.mali_csffw")))))
