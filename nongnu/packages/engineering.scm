;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nongnu packages engineering)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define-public lycheeslicer
  (package
    (name "lycheeslicer")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://mango-lychee.nyc3.cdn.digitaloceanspaces.com/LycheeSlicer-" version ".deb"))
       (sha256
        (base32 "1rv3f8d1sb5irn4y8hjzk2m7c9irw71ls8p1mqambxg79q9ffj9m"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:wrapper-plan
           #~'("lib/LycheeSlicer/lycheeslicer")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (invoke "ar" "x" #$source)
                   (invoke "tar" "xvf" "data.tar.xz")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   (mkdir-p "share")
                   (copy-recursively "usr/share" "share")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")
                   (delete-file "control.tar.gz")
                   (delete-file "data.tar.xz")
                   (delete-file "debian-binary")))
               (add-after 'unpack 'fix-desktop-file
                 (lambda _
                   ;; Fix the .desktop file binary location.
                   (substitute* '("share/applications/lycheeslicer.desktop")
                     (("/opt/LycheeSlicer")
                      (string-append #$output "/lib/LycheeSlicer")))))
               (add-before 'install-wrapper 'symlink-binary-file-and-cleanup
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/LycheeSlicer/lycheeslicer")
                            (string-append #$output "/bin/lycheeslicer"))))
               (add-after 'install-wrapper 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/LycheeSlicer/lycheeslicer")
                     `("LD_LIBRARY_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$output "/lib/LycheeSlicer"))
                          ":")))))))))
    (native-inputs (list tar))
    (inputs
     (list libxscrnsaver))
    (home-page "https://mango3d.io")
    (synopsis "Slicer for resin 3d printers of different manufacturers")
    (description "A user-friendly slicing software for resin 3d printers.  It
supports printers and resins of different manufacturers alongside
community-created profiles as well.  It offers a paid- and free version with a
reduced feature set.")
    (license (license:nonfree "https://mango3d.io/terms-and-conditions"))))
