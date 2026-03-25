;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Andre A. Gomes <andremegafone@gmail.com>
;;; Copyright © 2024 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2025,2026 Simen Endsjø <contact@simendsjo.me>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>

(define-module (nongnu packages electron)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (guix build-system copy)
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

(define (electron-node-headers-source version hash)
  (origin
    (method url-fetch/tarbomb)
    (uri (string-append "https://www.electronjs.org/headers/v" version
                        "/node-v" version "-headers.tar.gz"))
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

(define-public electron-27-node-headers
  (package
    (name "electron-node-headers")
    (version (package-version electron-27))
    (source (electron-node-headers-source
             version "0vrjdvqllfyz09sw2y078mds1di219hnmska8bw8ni7j35wxr2br"))
    (build-system copy-build-system)
    (home-page "https://www.electronjs.org/")
    (synopsis "nodejs headers for electron")
    (description "nodejs headers for electron.")
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

(define-public electron-28-node-headers
  (package
    (inherit electron-27-node-headers)
    (version (package-version electron-28))
    (source (electron-node-headers-source
             version "1d407g6yb81l9p0pbsidrsmnzkrwb4f2qb221kc9k2r7qdpf64px"))))

(define-public electron-29
  (package
    (inherit electron-28)
    (version "29.4.6")
    (source (electron-source version
                             "12d332zppvvijkqlbbb0s919zhspyczxdiivbdpjf4ps5y736ii3"))))

(define-public electron-29-node-headers
  (package
    (inherit electron-28-node-headers)
    (version (package-version electron-29))
    (source (electron-node-headers-source
             version "0swfh30yilw6w0qi6cl6ccm3rdvdmpr5s2vaxy5bbmizc88a4jkv"))))

(define-public electron-30
  (package
    (inherit electron-29)
    (version "30.5.1")
    (source (electron-source version
                             "00ql6vr47swlshg56a3zkifxsl4ywl5f7698yh2n1s1r7mw0fizc"))))

(define-public electron-30-node-headers
  (package
    (inherit electron-29-node-headers)
    (version (package-version electron-30))
    (source (electron-node-headers-source
             version "0db38ndw9rrd8ixa14761cbff6ns31b6302bzx5q4in8i4dkrrs3"))))

(define-public electron-31
  (package
    (inherit electron-30)
    (version "31.7.7")
    (source (electron-source version
                             "0ird9g43jzmzjlacb57f51yh2q7555vbvmy9rwvrrqrgypjyi8h0"))))

(define-public electron-31-node-headers
  (package
    (inherit electron-30-node-headers)
    (version (package-version electron-31))
    (source (electron-node-headers-source
             version "1dakbhv1f1cc8zr8rvhjgbmly43db1l1gcf0l8c7yn8h0lb17aq5"))))

(define-public electron-32
  (package
    (inherit electron-31)
    (version "32.3.3")
    (source (electron-source version
                             "04z8p8w6b0wwgvjiwn0v9if9k69vdh4wb1szh2wf6j1f9501a7qf"))))

(define-public electron-32-node-headers
  (package
    (inherit electron-31-node-headers)
    (version (package-version electron-32))
    (source (electron-node-headers-source
             version "0pb06wlx5zz0asrh05c90q0np14c4swkvhzrcqmcyfz7ihczqh5a"))))

(define-public electron-33
  (package
    (inherit electron-32)
    (version "33.4.11")
    (source (electron-source version
                             "1ag8rrjgzvv2n4jcz0v9dsymlz26hhgxjz3r3hqr4qlighf46b91"))))

(define-public electron-33-node-headers
  (package
    (inherit electron-32-node-headers)
    (version (package-version electron-33))
    (source (electron-node-headers-source
             version "0wi3iqsyvhfhhf1l1z1i60nyic380y4jralj15qnxazbhpggmjcs"))))

(define-public electron-34
  (package
    (inherit electron-33)
    (version "34.5.8")
    (source (electron-source version
                             "1ql0rxmw45p4vaxkxwl3pbgbqc0idz9fbifcnd0hsvwb0r0b0y6s"))))

(define-public electron-34-node-headers
  (package
    (inherit electron-33-node-headers)
    (version (package-version electron-34))
    (source (electron-node-headers-source
             version "0c4ij2lzamdmqpy5dygji31khj6xw13i2lrc28xrv1pq9lnnj2zd"))))

(define-public electron-35
  (package
    (inherit electron-34)
    (version "35.7.5")
    (source (electron-source version
                             "0ada9rafzi12bf9spxg1zik71rvr4xqlncqx25nhbqc945d1b39n"))))

(define-public electron-35-node-headers
  (package
    (inherit electron-34-node-headers)
    (version (package-version electron-35))
    (source (electron-node-headers-source
             version "15xivxw3ghr5zpwx3s3sp7maxp363vwckcmijnxkgnwk4k0l0zsi"))))

(define-public electron-36
  (package
    (inherit electron-35)
    (version "36.9.5")
    (source (electron-source version
                             "05l6cab4cq4cy5ajf8gz26h5s65dnvbzgmlc1wr1d0fnxr53dmjj"))))

(define-public electron-36-node-headers
  (package
    (inherit electron-35-node-headers)
    (version (package-version electron-36))
    (source (electron-node-headers-source
             version "1ygi0fayp2bpa2c1qvhk5krx5bh7sjnrnpybksn4zvq0qhm9bssv"))))

(define-public electron-37
  (package
    (inherit electron-36)
    (version "37.10.3")
    (source (electron-source version
                             "0q26c4svll88ph3qb9ycbwpcsgidkbk9kcksryjcsn4qppbfvd60"))))

(define-public electron-37-node-headers
  (package
    (inherit electron-36-node-headers)
    (version (package-version electron-37))
    (source (electron-node-headers-source
             version "1m7jhpm0p6gak3hpq7ggm460mbfmkn4y0s4rmapl0p9wg9x3hd1z"))))

(define-public electron-38
  (package
    (inherit electron-37)
    (version "38.8.6")
    (source (electron-source version
                             "1gp896wf6vpk581m6ds3b3xcyisrlr89bqcdvjs3ds0bgjyg5a21"))))

(define-public electron-38-node-headers
  (package
    (inherit electron-37-node-headers)
    (version (package-version electron-38))
    (source (electron-node-headers-source
             version "1z9fvxhm4y15p87q4dldap5sjj1dxjx3sx0znqqxxikdhjd5x2nb"))))

(define-public electron-39
  (package
    (inherit electron-38)
    (version "39.8.4")
    (source (electron-source version
                             "1sg4pc6za2imzpbjn9phqwbs716wznn4cqkc9xrdmplm4mqfmwb6"))))

(define-public electron-39-node-headers
  (package
    (inherit electron-38-node-headers)
    (version (package-version electron-39))
    (source (electron-node-headers-source
             version "1x1f4l0li3h26jm1aw23yl1xj9iir7gjvqhqn26v87grf59vpw8j"))))

(define-public electron-40
  (package
    (inherit electron-39)
    (version "40.6.1")
    (source (electron-source version
                             "01qfcq6wzgvi19a9czwc4z5i956q0kagnhz6s6g5jlx2rbd53r88"))))

(define-public electron-40-node-headers
  (package
    (inherit electron-39-node-headers)
    (version (package-version electron-40))
    (source (electron-node-headers-source
             version "0lvyb5j0y220la5a055g5mgg6bbpcfy4nkniz3mvgdvramknpajd"))))
