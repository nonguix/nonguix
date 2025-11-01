;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2022 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2025 Anderson Torres <anderson.torres.8519@gmail.com>

(define-module (nongnu packages chrome)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix licenses)
  #:use-module (ice-9 string-fun))

(define-public (make-google-chrome repo version hash)
  (let* ((name (string-append "google-chrome-" repo))
         (appname (if (string=? repo "stable")
                      "chrome"
                      (string-replace-substring name "google-" ""))))
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
     (build-system chromium-binary-build-system)
     (arguments
      (list
        ;; almost 300MB, faster to download and build from Google servers
        #:substitutable? #f
        #:wrapper-plan
         #~(let ((path (string-append "opt/google/" #$appname "/")))
             (map (lambda (file)
                    (string-append path file))
                  '("chrome"
                    "chrome-sandbox"
                    "chrome_crashpad_handler"
                    "libEGL.so"
                    "libGLESv2.so"
                    "liboptimization_guide_internal.so"
                    "libqt5_shim.so"
                    "libqt6_shim.so"
                    "libvk_swiftshader.so"
                    "libvulkan.so.1"
                    "WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so")))
        #:install-plan
         #~'(("opt/" "/share")
             ("usr/share/" "/share"))
        #:phases
         #~(modify-phases %standard-phases
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
                     ((old-exe) exe)))))
             (add-after 'install 'install-icons
                (lambda _
                  (define (format-icon-size name)
                    (car
                      (string-split
                       (string-drop-right (string-drop name 13) 4)
                       #\_)))
                  (let ((icons (string-append #$output "/share/icons/hicolor"))
                        (share (string-append #$output "/share/google/" #$appname)))
                    (for-each (lambda (icon)
                                (let* ((icon-name (basename icon))
                                       (icon-size (format-icon-size icon-name))
                                       (target (string-append icons "/" icon-size "x" icon-size "/apps/google-" #$appname ".png")))
                                  (mkdir-p (dirname target))
                                  (rename-file icon target)))
                              (find-files share "product_logo_.*\\.png")))))
             (add-before 'install-wrapper 'install-exe
              (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (exe (string-append bin "/google-" #$appname))
                       (share (string-append #$output "/share"))
                       (chrome-target (string-append share "/google/" #$appname "/google-" #$appname)))
                  (mkdir-p bin)
                  (symlink chrome-target exe)
                  (wrap-program exe
                    '("CHROME_WRAPPER" = (#$appname)))))))))
     (inputs
      (list bzip2
            curl
            flac
            font-liberation
            gdk-pixbuf
            gtk
            harfbuzz
            libexif
            libglvnd
            libpng
            libva
            libxscrnsaver
            opus
            pciutils
            pipewire
            qtbase-5
            qtbase
            snappy
            util-linux
            xdg-utils
            wget))
     (synopsis  "Freeware web browser")
     (supported-systems '("x86_64-linux"))
     (description "Google Chrome is a cross-platform web browser developed by Google.")
     (home-page "https://www.google.com/chrome/")
     (license (nonfree "https://www.google.com/intl/en/chrome/terms/")))))

(define-public google-chrome-stable
  (make-google-chrome "stable" "139.0.7258.127" "1zq0xy7n97z2vklagwh25wmgbhzfrl94jjnvdvr16a7v334zz17k"))

(define-public google-chrome-beta
  (make-google-chrome "beta" "140.0.7339.16" "0zyq0sigr5pxhylp3mr45f4i3xjsbj2rmgvymgkb407ffmlcywn2"))

(define-public google-chrome-unstable
  (make-google-chrome "unstable" "141.0.7354.0" "0b5n3flblzbfjmvkyj1lxicx48jzx47l5rba3i3j3p2n4p5hvk75"))

(define-public google-chrome-canary
  (make-google-chrome "canary" "141.0.7357.0" "0hc4k4nyxrvc07dmg5cbr795d2f56dff7br905pj9cxhg2ha7c5j"))
