;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages engineering)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system binary)
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
    (build-system binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:patchelf-plan
           #~'(("lib/LycheeSlicer/lycheeslicer"
                ("alsa-lib"
                 "at-spi2-atk"
                 "at-spi2-core"
                 "atk"
                 "cairo"
                 "cups"
                 "dbus"
                 "expat"
                 "eudev"
                 "fontconfig-minimal"
                 "gcc"
                 "gdk-pixbuf"
                 "glib"
                 "gtk+"
                 "libdrm"
                 "libnotify"
                 "libsecret"
                 "libx11"
                 "libxcb"
                 "libxcomposite"
                 "libxdamage"
                 "libxext"
                 "libxfixes"
                 "libxkbcommon"
                 "libxrandr"
                 "libxscrnsaver"
                 "libxshmfence"
                 "libxtst"
                 "mesa"
                 "nspr"
                 "pango")))
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
               (add-after 'install 'symlink-binary-file-and-cleanup
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/LycheeSlicer/lycheeslicer")
                            (string-append #$output "/bin/lycheeslicer"))))
               (add-after 'install 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/LycheeSlicer/lycheeslicer")
                     `("FONTCONFIG_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                           #$output)
                          ":")))
                     `("LD_LIBRARY_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "nss") "/lib/nss")
                           (string-append #$(this-package-input "mesa") "/lib")
                           (string-append #$(this-package-input "dbus") "/lib")
                           (string-append #$(this-package-input "gcc") "/lib")
                           (string-append #$(this-package-input "eudev") "/lib")
                           (string-append #$(this-package-input "libsecret") "/lib")
                           (string-append #$(this-package-input "libxscrnsaver") "/lib")
                           (string-append #$(this-package-input "libnotify") "/lib")
                           (string-append #$output "/lib/LycheeSlicer")
                           #$output)
                          ":")))))))))
    (native-inputs (list tar))
    (inputs
     (list alsa-lib
           at-spi2-atk
           at-spi2-core
           atk
           cairo
           cups
           dbus
           expat
           eudev
           fontconfig
           `(,gcc "lib")
           glib
           gtk+
           libdrm
           libnotify
           libsecret
           libx11
           libxcb
           libxcomposite
           libxdamage
           libxext
           libxfixes
           libxkbcommon
           libxrandr
           libxscrnsaver
           libxshmfence
           libxtst
           mesa
           nspr
           nss
           pango))
    (home-page "https://mango3d.io")
    (synopsis "Slicer for resin 3d printers of different manufacturers")
    (description "A user-friendly slicing software for resin 3d printers.  It
supports printers and resins of different manufacturers alongside
community-created profiles as well.  It offers a paid- and free version with a
reduced feature set.")
    (license (license:nonfree "https://mango3d.io/terms-and-conditions"))))
