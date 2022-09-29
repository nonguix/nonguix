;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 PantherX OS Team <team@pantherx.org>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.


(define-module (nongnu packages messaging)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match))

(define-public element-desktop
  (package
    (name "element-desktop")
    (version "1.11.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://packages.riot.im/debian/pool/main/e/" name "/" name "_" version
         "_amd64.deb"))
       (sha256
        (base32 "0vaicxjhmd6pvsp6vsjcplmd7rzvfcibs4pa3ylb190p1gnmnj4k"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:patchelf-plan
           #~'(("lib/Element/element-desktop"
                ("alsa-lib" "at-spi2-atk" "at-spi2-core" "atk" "cairo" "cups"
                 "dbus" "expat" "fontconfig-minimal" "gcc" "gdk-pixbuf" "glib"
                 "gtk+" "libdrm" "libnotify" "libsecret" "libx11" "libxcb"
                 "libxcomposite" "libxcursor" "libxdamage" "libxext" "libxfixes"
                 "libxi" "libxkbcommon" "libxkbfile" "libxrandr" "libxrender"
                 "libxtst" "mesa" "nspr" "pango" "zlib")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (invoke "ar" "x" #$source)
                   (invoke "tar" "xvf" "data.tar.xz")
                   (copy-recursively "usr/" ".")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")
                   (delete-file "control.tar.gz")
                   (delete-file "data.tar.xz")
                   (delete-file "debian-binary")
                   ;; Fix the .desktop file binary location.
                   (substitute* '("share/applications/element-desktop.desktop") 
                     (("/opt/Element/")
                      (string-append #$output "/lib/Element/")))))
               (add-after 'install 'symlink-binary-file-and-cleanup
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/Element/element-desktop")
                            (string-append #$output "/bin/element-desktop"))))
               (add-after 'install 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/Element/element-desktop")
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
                           (string-append #$(this-package-input "eudev") "/lib")
                           (string-append #$(this-package-input "gcc") "/lib")
                           (string-append #$(this-package-input "mesa") "/lib")
                           (string-append #$(this-package-input "libxkbfile") "/lib")
                           (string-append #$(this-package-input "zlib") "/lib")
                           (string-append #$(this-package-input "libsecret") "/lib")
                           (string-append #$(this-package-input "sqlcipher") "/lib")
                           (string-append #$(this-package-input "libnotify") "/lib")
                           (string-append #$output "/lib/Element")
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
           eudev
           expat
           fontconfig
           `(,gcc "lib")
           glib
           gtk+
           libdrm
           libnotify
           librsvg
           libsecret
           libx11
           libxcb
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxfixes
           libxi
           libxkbcommon
           libxkbfile
           libxrandr
           libxrender
           libxtst
           mesa
           nspr
           nss
           pango
           sqlcipher
           zlib))
    (home-page "https://github.com/vector-im/element-desktop")
    (synopsis "Matrix collaboration client for desktop")
    (description "Element Desktop is a Matrix client for desktop with Element Web at
its core.")
    ;; not working?
    (properties
     '((release-monitoring-url . "https://github.com/vector-im/element-desktop/releases")))
    (license license:asl2.0)))

(define-public signal-desktop
  (package
    (name "signal-desktop")
    (version "5.61.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://updates.signal.org/desktop/apt/pool/main/s/" name "/" name "_" version
         "_amd64.deb"))
       (sha256
        (base32 "0y2kwifj1szll79jscpmhdh8dvlrsqp0n82wh1zkiz55zfxcwhyl"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:patchelf-plan
           #~'(("lib/Signal/signal-desktop"
                ("alsa-lib" "at-spi2-atk" "at-spi2-core" "atk" "cairo" "cups"
                 "dbus" "expat" "fontconfig-minimal" "gcc" "gdk-pixbuf" "glib"
                 "gtk+" "libdrm" "libsecret" "libx11" "libxcb" "libxcomposite"
                 "libxcursor" "libxdamage" "libxext" "libxfixes" "libxi"
                 "libxkbcommon" "libxkbfile" "libxrandr" "libxshmfence" "libxtst"
                 "mesa" "nspr" "pango" "pulseaudio" "zlib")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (invoke "ar" "x" #$source)
                   (invoke "tar" "xvf" "data.tar.xz")
                   (copy-recursively "usr/" ".")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")
                   (delete-file "control.tar.gz")
                   (delete-file "data.tar.xz")
                   (delete-file "debian-binary")
                   (delete-file "environment-variables")
                   ;; Fix the .desktop file binary location.
                   (substitute* '("share/applications/signal-desktop.desktop") 
                     (("/opt/Signal/")
                      (string-append #$output "/lib/Signal/")))))
               (add-after 'install 'symlink-binary-file-and-cleanup
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/Signal/signal-desktop")
                            (string-append #$output "/bin/signal-desktop"))))
               (add-after 'install 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/Signal/signal-desktop")
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
                           (string-append #$(this-package-input "eudev") "/lib")
                           (string-append #$(this-package-input "gcc") "/lib")
                           (string-append #$(this-package-input "mesa") "/lib")
                           (string-append #$(this-package-input "libxkbfile") "/lib")
                           (string-append #$(this-package-input "pulseaudio") "/lib")
                           (string-append #$(this-package-input "zlib") "/lib")
                           (string-append #$(this-package-input "libsecret") "/lib")
                           (string-append #$output "/lib/Signal")
                           #$output)
                          ":")))))))))
    (native-inputs (list tar))
    (inputs (list alsa-lib
                  at-spi2-atk
                  at-spi2-core
                  atk
                  cairo
                  cups
                  dbus
                  eudev
                  expat
                  fontconfig
                  `(,gcc "lib")
                  glib
                  gtk+
                  libdrm
                  librsvg
                  libsecret
                  libx11
                  libxcb
                  libxcomposite
                  libxdamage
                  libxext
                  libxfixes
                  libxkbcommon
                  libxkbfile
                  libxrandr
                  libxshmfence
                  mesa
                  nspr
                  nss
                  pango
                  pulseaudio
                  zlib))
    (home-page "https://signal.org/")
    (synopsis "Private messenger using the Signal protocol")
    (description "Signal Desktop is an Electron application that links with Signal on Android
or iOS.")
    ;; doesn't work?
    (properties
     '((release-monitoring-url . "https://github.com/signalapp/Signal-Desktop/releases")))
    (license license:agpl3)))
