;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages chrome)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses)
  #:use-module (ice-9 string-fun))

(define-public (make-google-chrome repo version hash)
  (let* ((name (string-append "google-chrome-" repo))
         (appname (if (string=? repo "stable")
                      "chrome"
                      (string-replace-substring name "google-" "")))
         (patchelf-inputs (list "alsa-lib" "at-spi2-atk" "at-spi2-core" "atk" "cairo" "cups"
                                "dbus" "eudev" "expat" "freetype" "fontconfig-minimal" "gcc" "gdk-pixbuf"
                                "glib" "gtk" "harfbuzz" "libdrm" "libnotify" "libsecret" "libx11"
                                "libxcb" "libexif" "libxcomposite" "libxcursor" "libxdamage"
                                "libxext" "libxfixes" "libxi" "libxkbcommon" "libxkbfile" "libxrandr"
                                "libxrender" "libxtst" "libnotify" "mesa" "nspr" "pango" "pipewire"
                                "sqlcipher" "xdg-utils" "zlib")))
    (package
     (name name)
     (version version)
     (source (origin
               (method url-fetch)
               (uri
                (string-append
                 "https://dl.google.com/linux/chrome/deb/pool/main/g/"
                 name "/" name "_" version "-1_amd64.deb"))
               (sha256
                (base32 hash))))
     (build-system binary-build-system)
     (arguments
      (list
        ;; almost 300MB, faster to download and build from Google servers
        #:substitutable? #f
        #:patchelf-plan
         #~(let ((patchelf-inputs (list #$@patchelf-inputs))
                 (path (string-append "opt/google/" #$appname "/")))
             (map (lambda (file)
                    (cons (string-append path file) (list patchelf-inputs)))
                  '("chrome"
                    "chrome-sandbox"
                    "chrome_crashpad_handler"
                    "nacl_helper"
                    "libEGL.so"
                    "libGLESv2.so")))
        #:install-plan
         #~'(("opt/" "/share")
             ("usr/share/" "/share"))
        #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'unpack-deb
               (lambda* (#:key inputs #:allow-other-keys)
                 (invoke "ar" "x" #$source)
                 (invoke "rm" "-v" "control.tar.xz"
                                   "debian-binary"
                                   (string-append "google-chrome-" #$repo "_"
                                                  #$version
                                                  "-1_amd64.deb"))
                 (invoke "tar" "xf" "data.tar.xz")
                 (invoke "rm" "-vrf" "data.tar.xz" "etc")))
             (add-before 'install 'patch-assets
               ;; Many thanks to
               ;; https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/networking/browsers/google-chrome/default.nix
               (lambda _
                 (let* ((bin (string-append #$output "/bin"))
                        (share (string-append #$output "/share"))
                        (opt "./opt")
                        (usr/share "./usr/share")
                        (old-exe (string-append "/opt/google/" #$appname "/google-" #$appname))
                        (exe (string-append bin "/google-" #$appname)))
                   ;; This allows us to override CHROME_WRAPPER later.
                   (substitute* (string-append opt "/google/" #$appname "/google-" #$appname)
                     (("CHROME_WRAPPER") "WRAPPER"))
                   (substitute* (string-append usr/share "/applications/google-" #$appname ".desktop")
                     (("^Exec=.*") (string-append "Exec=" exe "\n")))
                   (substitute* (string-append usr/share "/gnome-control-center/default-apps/google-" #$appname ".xml")
                     ((old-exe) exe))
                   (substitute* (string-append usr/share "/menu/google-" #$appname ".menu")
                     (("/opt") share)
                     ((old-exe) exe))
                   #t)))
             (add-after 'install 'install-wrapper
              (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (exe (string-append bin "/google-" #$appname))
                       (share (string-append #$output "/share"))
                       (chrome-target (string-append share "/google/" #$appname "/google-" #$appname))
                       (patchelf-inputs-packages (list #$@(map (lambda (i) (this-package-input i)) patchelf-inputs)))
                       (ld-library-libs (map (lambda (input)
                                               (string-append input "/lib"))
                                             patchelf-inputs-packages)))
                  (mkdir-p bin)
                  (symlink chrome-target exe)
                  (wrap-program exe
                    `("FONTCONFIG_PATH" ":" prefix
                      (,(string-join
                         (list
                          (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                          #$output)
                         ":")))
                    `("LD_LIBRARY_PATH" ":" prefix
                      (,(string-join
                         (append
                          ld-library-libs
                          (list
                           (string-append #$(this-package-input "nss") "/lib/nss")
                           #$output))
                         ":")))
                    '("CHROME_WRAPPER" = (#$appname)))))))))
     (native-inputs (list tar))
     (inputs
      (list alsa-lib
            at-spi2-atk
            at-spi2-core
            atk
            bzip2
            cairo
            curl
            cups
            dbus
            eudev
            expat
            flac
            fontconfig
            freetype
            font-liberation
            `(,gcc "lib")
            gdk-pixbuf
            glib
            gtk
            harfbuzz
            libdrm
            libexif
            libglvnd
            libnotify
            libpng
            librsvg
            libsecret
            libva
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
            libxscrnsaver
            libxshmfence
            libxrender
            libxtst
            mesa
            nspr
            nss
            opus
            pango
            pciutils
            pipewire
            snappy
            sqlcipher
            util-linux
            xdg-utils
            wget
            zlib))
     (synopsis  "Freeware web browser")
     (supported-systems '("x86_64-linux"))
     (description "Google Chrome is a cross-platform web browser developed by Google.")
     (home-page "https://www.google.com/chrome/")
     (license (nonfree "https://www.google.com/intl/en/chrome/terms/")))))

(define-public google-chrome-stable
  (make-google-chrome "stable" "110.0.5481.77" "0jjdgfps6siy9hk2r553vvh0jmkn987ad77sv2zqs9gvx0vsrwgp"))

(define-public google-chrome-beta
  (make-google-chrome "beta" "110.0.5481.77" "0wnzgvwbpmb5ja4ba5mjk4bk0aaxzbw4zi509vw96q6mbqmr4iwr"))

(define-public google-chrome-unstable
  (make-google-chrome "unstable" "110.0.5464.2" "0hzv55bba4041400zjysgzz1n8svzvi156xyrayfr5ynapf7g2rd"))
