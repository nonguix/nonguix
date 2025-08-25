;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Andre A. Gomes <andremegafone@gmail.com>
;;; Copyright © 2024 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025 Simen Endsjø <contact@simendsjo.me>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>

(define-module (nongnu packages electron)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages video))

(define (electron-source version hash)
  (origin
    (method url-fetch/zipbomb)
    (uri
     (string-append
      "https://github.com/electron/electron/releases/download/v"
      version "/electron-v" version "-"
      (match (or (%current-system) (%current-target-system))
        ("x86_64-linux" "linux-x64")
        ("i686-linux" "linux-ia32")
        ("aarch64-linux" "linux-arm64")
        ("armhf-linux" "linux-armv7l")
        ;; We need a default case
        (_ "unsupported"))
      ".zip"))
    (sha256 (base32 hash))))

(define-public electron-27
  (package
    (name "electron")
    (version "27.3.11")
    (source (electron-source version
                             "0qs5n6m0gj0rknjq5aqrbbpqwh2829a1cl51l6xj79p7aiggb9p3"))
    ;; TODO: Better multi-arch source support for binary pacakges.
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:wrapper-plan
      #~'(("electron" (("out" "/share/electron")
                       ("nss" "/lib/nss")))
          "chrome-sandbox"
          "chrome_crashpad_handler")
      #:install-plan
      #~'(("." "share/electron/" #:include
           ("electron"
            "chrome-sandbox"
            "chrome_100_percent.pak"
            "chrome_200_percent.pak"
            "chrome_crashpad_handler"
            "icudtl.dat"
            "resources.pak"
            "v8_context_snapshot.bin"
            "version"
            "libffmpeg.so"
            ;; electron seems to force-load these from its directory.
            "libEGL.so"
            "libGLESv2.so"))
          ("resources" "share/electron/")
          ("locales" "share/electron/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'symlink-binary-file
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/share/electron/electron")
                         (string-append bin "/electron"))))))))
    (native-inputs (list unzip))
    (inputs (list ffmpeg gdk-pixbuf nss))
    (home-page "https://www.electronjs.org/")
    (synopsis "Cross platform desktop application shell")
    (description "The Electron framework lets you write cross-platform desktop
applications using JavaScript, HTML and CSS.  It is based on Node.js and
Chromium and is used by the Atom editor and many other apps.")
    (license (license:nonfree
              (string-append "https://github.com/electron/electron/blob/v"
                             version "/LICENSE")))))

(define-public electron electron-27)

(define-public electron-28
  (package
    (inherit electron-27)
    (version "28.3.3")
    (source (electron-source version
                             "0inmcr9k03czq6fini9m8xzzkw8a94f4nx3f40j9jv5x7i4vxxi0"))))

(define-public electron-29
  (package
    (inherit electron-28)
    (version "29.4.6")
    (source (electron-source version
                             "12d332zppvvijkqlbbb0s919zhspyczxdiivbdpjf4ps5y736ii3"))))

(define-public electron-30
  (package
    (inherit electron-29)
    (version "30.5.1")
    (source (electron-source version
                             "00ql6vr47swlshg56a3zkifxsl4ywl5f7698yh2n1s1r7mw0fizc"))))

(define-public electron-31
  (package
    (inherit electron-30)
    (version "31.7.7")
    (source (electron-source version
                             "0ird9g43jzmzjlacb57f51yh2q7555vbvmy9rwvrrqrgypjyi8h0"))))

(define-public electron-32
  (package
    (inherit electron-31)
    (version "32.3.3")
    (source (electron-source version
                             "04z8p8w6b0wwgvjiwn0v9if9k69vdh4wb1szh2wf6j1f9501a7qf"))))

(define-public electron-33
  (package
    (inherit electron-32)
    (version "33.4.11")
    (source (electron-source version
                             "1ag8rrjgzvv2n4jcz0v9dsymlz26hhgxjz3r3hqr4qlighf46b91"))))

(define-public electron-34
  (package
    (inherit electron-33)
    (version "34.5.0")
    (source (electron-source version
                             "1qkmg7ycp7529r1h36pxd867d8dpk1vdx6g5fgsakmqjlmyhvfhi"))))

(define-public electron-35
  (package
    (inherit electron-34)
    (version "35.1.3")
    (source (electron-source version
                             "076xi67bj11rr80k60ca8pv50lfzf3pi5z8glkybx2jr68f0vp7z"))))
