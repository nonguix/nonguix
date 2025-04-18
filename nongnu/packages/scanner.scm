;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>

(define-module (nongnu packages scanner)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages package-management)
  #:use-module (nonguix licenses))

;; TODO: This packaged should reproduce what NixOS does, but I couldn't get
;; the Brother MFC-J5335DW to work.  There may be something wrong with my
;; scanner, or with Guix and Sane, or with this package.

;; See https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=brscan4
;; See https://nixos.wiki/wiki/Scanners

(define brscan4-version "0.4.8")

(define brscan4-i686-archive
  (origin
    (method url-fetch)
    (uri (string-append "https://download.brother.com/welcome/dlf006647/brscan4-"
                        brscan4-version "-1.i386.rpm"))
    (sha256
     (base32 "0amp5j1likv1slfvlj2y22dnimljp7pzzg1z94fiwhlvk98m80pz"))))

(define brscan4-x86_64-archive
  (origin
    (method url-fetch)
    (uri (string-append "https://download.brother.com/welcome/dlf006648/brscan4-"
                        brscan4-version "-1.x86_64.rpm"))
    (sha256
     (base32 "1bfdpf8z2ijr5gk3njffpy8zxw9fvyfw30ncrz4cfk15md7vwqfd"))))

(define (lib)
  (if (string=? (or (%current-target-system) (%current-system)) "x86_64-linux")
      "lib64" "lib"))

(define-public brscan4
  (package
    (name "brscan4")
    (version brscan4-version)
    (source #f)
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       `(("opt/brother/scanner/brscan4/brsaneconfig4")
         ("opt/brother/scanner/brscan4/brscan_cnetconfig"
          ("avahi"))
         ("opt/brother/scanner/brscan4/brscan_gnetconfig"
          ("avahi" "glib" "gtk")))
       #:install-plan
       `((,,(string-append "usr/" (lib) "/sane/libsane-brother4.so.1.0.7") "lib/sane/")
         ("opt/brother/scanner/brscan4/" "opt/brother/scanner/brscan4/"
          #:exclude ("setupSaneScan4" "udev_config.sh")))
       #:modules ((guix build utils)
                  (nonguix build utils)
                  (nonguix build binary-build-system)
                  (srfi srfi-1)
                  (ice-9 regex)
                  (ice-9 textual-ports))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (system (format #f "rpm2cpio ~a | cpio -idmv"
                             (assoc-ref inputs "archive")))
             #t))
         (add-after 'install 'symlink-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/sane/libsane-brother4.so.1.0.7"))
                    (link (string-append out "/lib/sane/libsane-brother4.so.1.0.7")))
               (symlink lib (string-append (dirname lib) "/libsane-brother4.so.1"))
               (symlink lib (string-append (dirname lib) "/libsane-brother4.so")))
             #t))
         (add-after 'install 'set-sane-dll-conf
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dll (string-append out "/etc/sane.d/dll.conf")))
               (mkdir-p (dirname dll))
               (with-output-to-file dll
                 (lambda ()
                   (display "brother4"))))
             #t))
         (add-after 'install 'set-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (define (file->lines file)
               (let ((file-string (call-with-input-file file
                                    (lambda (f)
                                      (get-string-all f)))))
                 (delete "" (string-split file-string #\newline))))
             (define (maybe-print-line line)
               (when (string-count line #\,)
                 (let* ((first-word (first (delete "" (string-split line #\,))))
                        (id-match (string-match "0[xX]([[:xdigit:]]+)" first-word)))
                   (when id-match
                     (format #t
                             (string-append "ATTR{idProduct}==\"~a\", "
                                            "MODE=\"0664\", GROUP=\"scanner\", "
                                            "ENV{libsane_matched}=\"yes\"~%")
                             (string-downcase (regexp-substitute #f id-match 1)))))))
             (let* ((out (assoc-ref outputs "out"))
                    (rules (string-append out "/lib/udev/rules.d/49-brother-brscan4-libsane-type1.rules")))
               (mkdir-p (dirname rules))
               (with-output-to-file rules
                 (lambda ()
                   (format #t "~a~%~a~%~a~%"
                           "ACTION==\"add\", ATTR{idVendor}==\"04f9\", GOTO=\"brscan4\""
                           "GOTO=\"brscan4_end\""
                           "LABEL=\"brscan4\"")
                   (for-each
                    (lambda (file)
                      (for-each
                       maybe-print-line
                       (file->lines file)))
                    (cons "opt/brother/scanner/brscan4/Brsane4.ini"
                          (find-files "opt/brother/scanner/brscan4/models4"
                                      "\\.ini$")))
                   (format #t "LABEL=\"brscan4_end\"~%"))))
             #t))
         (add-after 'install 'build-preload
           ;; Generate an LD_PRELOAD wrapper to redirect execvp(), open() and
           ;; open64() calls to `/opt/brother/scanner/brscan4`.
           (lambda* (#:key inputs outputs system #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (preload-src (assoc-ref inputs "preload"))
                    (preload (string-append
                              out "/libexec/brother/scanner/brscan4/libpreload.so")))
               (mkdir-p (dirname preload))
               (apply invoke "gcc"
                      (append
                       (if (string=? system "x86_64-linux")
                           ;; FIXME: For some reason glibc32 has higher
                           ;; priority than the 64-bit version.
                           (list (string-append "-L" (assoc-ref inputs "glibc64") "/lib"))
                           '())
                       (list
                        "-shared" preload-src "-o" preload "-ldl"
                        (string-append "-DOUT=\"" out "\"") "-fPIC"))))
             #t))
         (add-after 'build-preload 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (preload (string-append
                              out "/libexec/brother/scanner/brscan4/libpreload.so"))
                    (wrapper (string-append out "/bin/brsaneconfig4"))
                    (bin (string-append out "/opt/brother/scanner/brscan4/brsaneconfig4")))
               (mkdir-p (dirname wrapper))
               (make-wrapper wrapper bin
                             `("LD_PRELOAD" ":" prefix ,(list preload))))
             #t)))))
    (native-inputs
     `(("rpm" ,rpm)
       ("cpio" ,cpio)
       ("archive"
        ,(match (or (%current-target-system) (%current-system))
           ("x86_64-linux" brscan4-x86_64-archive)
           (_ brscan4-i686-archive)))

       ("gcc" ,gcc)
       ,@(if (string=? (or (%current-target-system) (%current-system)) "x86_64-linux")
             `(("glibc64" ,glibc))
             '())
       ("preload"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/NixOS/nixpkgs/"
                               "15b3d9d2773b6ff919b324d756e23f0f8bf9fc3f" ; 2020-05-16
                               "/pkgs/applications/graphics/sane/backends"
                               "/brscan4/preload.c"))
           (sha256
            (base32
             "0dgp2qy9j7qjf2ld5hh921iaizwxwyspfjfp5p8p0bd5q0n45g6k"))))))
    (inputs
     `(("avahi" ,avahi)
       ("glib" ,glib)
       ("gtk" ,gtk+-2)))
    (home-page "http://support.brother.com/")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (synopsis "Driver, detection and configuration tool for Brother scanners")
    (description "Driver, detection and configuration tool for Brother
scanners.  It also supports specifying an alternate file name for
@file{brsanenetdevice4.cfg} via the @command{BRSANENETDEVICE4_CFG_FILENAME}
environment variable, otherwise the file is invariably created at
@file{/etc/opt/brother/scanner/brscan4}.")
    (license (nonfree "No URL"))))
