;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Kahka F
;;; Copyright © 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages printers)
  #:use-module (gnu packages)
  #:use-module (gnu packages cups)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix download)
  #:use-module (guix build utils))

(define-public hplip-plugin
  (package
    (inherit hplip)
    (name "hplip-plugin")
    (description "Hewlett-Packard printer drivers with nonfree plugin.")
    (source (origin
              (inherit (package-source hplip))
              (snippet
               (delete '(for-each
                         delete-file
                         (find-files "." (lambda (file stat)
                                           (elf-file? file))))
                       (origin-snippet (package-source hplip))))))
    (inputs (alist-delete "python-pyqt" (package-inputs hplip)))
    (native-inputs
     (append
      `(("hplip-plugin"
         ,(origin
            (method url-fetch)
            (uri (string-append "https://developers.hp.com/sites/default/files/hplip-"
                                (package-version hplip) "-plugin.run"))
            ;; TODO: Since this needs to be updated on every update to Guix's
            ;; hplip in order to build, might be better to decouple this
            ;; package from hplip.  In the meantime, update this hash when
            ;; hplip is updated in Guix.
            (sha256
             (base32
              "1396d9skaq5c5vxxi331nc81yhm9daws7awq0rcn1faq89mvygps")))))
      (package-native-inputs hplip)))
    (arguments
     (substitute-keyword-arguments (package-arguments hplip)
       ((#:configure-flags cf)
        #~(delete "--enable-qt5" #$cf))
       ((#:phases ph)
        #~(modify-phases #$ph
           (replace 'fix-hard-coded-file-names
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     ;; FIXME: use merged ppds (I think actually only
                     ;; drvs need to be merged).
                     (cupsdir (assoc-ref inputs "cups-minimal")))
                 (substitute* "base/g.py"
                   (("'/usr/share;[^']*'")
                    (string-append "'" cupsdir "/share'"))
                   (("'/etc/hp/hplip.conf'")
                    (string-append "'" out
                                   "/etc/hp/hplip.conf" "'"))
                   (("/var/lib/hp")
                    (string-append
                     out
                     "/var/lib/hp")))

                 (substitute* "Makefile.in"
                   (("[[:blank:]]check-plugin\\.py[[:blank:]]") " ")
                   ;; FIXME Use beginning-of-word in regexp.
                   (("[[:blank:]]plugin\\.py[[:blank:]]") " ")
                   (("/usr/include/libusb-1.0")
                    (string-append (assoc-ref inputs "libusb")
                                   "/include/libusb-1.0"))
                   (("hplip_statedir =.*$")
                    ;; Don't bail out while trying to create
                    ;; /var/lib/hplip.  We can safely change its value
                    ;; here because it's hard-coded in the code anyway.
                    "hplip_statedir = $(prefix)/var/lib/hp\n")
                   (("hplip_confdir = /etc/hp")
                    ;; This is only used for installing the default config.
                    (string-append "hplip_confdir = " out
                                   "/etc/hp"))
                   (("halpredir = /usr/share/hal/fdi/preprobe/10osvendor")
                    ;; We don't use hal.
                    (string-append "halpredir = " out
                                   "/share/hal/fdi/preprobe/10osvendor"))
                   (("rulesdir = /etc/udev/rules.d")
                    ;; udev rules will be merged by base service.
                    (string-append "rulesdir = " out
                                   "/lib/udev/rules.d"))
                   (("rulessystemdir = /usr/lib/systemd/system")
                    ;; We don't use systemd.
                    (string-append "rulessystemdir = " out
                                   "/lib/systemd/system"))
                   (("/etc/sane.d")
                    (string-append out "/etc/sane.d")))

                 (substitute* "common/utils.h"
                   (("/var/lib/hp")
                    (string-append
                     out
                     "/var/lib/hp"))))))
           (add-after 'install-models-dat 'install-plugins
             (lambda* (#:key outputs system inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (state-dir (string-append out "/var/lib/hp"))
                      (hp-arch (assoc-ref
                                '(("i686-linux" . "x86_32")
                                  ("x86_64-linux" . "x86_64")
                                  ("armhf-linux" . "arm32")
                                  ("aarch64-linux" . "aarch64"))
                                system)))
                 (unless hp-arch
                   (error (string-append
                           "HPLIP plugin not supported on "
                           system)))
                 (invoke "sh" (assoc-ref inputs "hplip-plugin")
                         "--noexec" "--keep")
                 (chdir "plugin_tmp")
                 (install-file "plugin.spec"
                               (string-append out "/share/hplip/"))

                 (for-each
                  (lambda (file)
                    (install-file
                     file
                     (string-append out "/share/hplip/data/firmware")))
                  (find-files "." "\\.fw.gz$"))

                 (install-file "license.txt"
                               (string-append out "/share/hplip/data/plugins"))
                 (mkdir-p
                  (string-append out "/share/hplip/prnt/plugins"))
                 (for-each
                  (lambda (type plugins)
                    (for-each
                     (lambda (plugin)
                       (let ((file (string-append plugin "-" hp-arch ".so"))
                             (dir (string-append out "/share/hplip/"
                                                 type "/plugins")))
                         (install-file file dir)
                         (chmod (string-append dir "/" file) #o755)
                         (symlink (string-append dir "/" file)
                                  (string-append dir "/" plugin ".so"))))
                     plugins))
                  '("prnt" "scan")
                  '(("lj" "hbpl1")
                    ("bb_soap" "bb_marvell" "bb_soapht" "bb_escl")))
                 (mkdir-p state-dir)
                 (call-with-output-file
                     (string-append state-dir "/hplip.state")
                   (lambda (port)
                     (simple-format port "[plugin]
installed=1
eula=1
version=~A
" #$(package-version hplip))))

                 (substitute* (string-append out "/etc/hp/hplip.conf")
                   (("/usr") out)))))))))))
