;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nongnu packages electron)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages video))

(define-public electron
  (package
    (name "electron")
    (version "27.1.0")
    (source (origin
              (method url-fetch/zipbomb)
              (uri
               (string-append
                "https://github.com/electron/electron/releases/download/v"
                version "/electron-v" version "-"
                (match (or (%current-system) (%current-target-system))
                  ("x86_64-linux" "linux-x64")
                  ("i686-linux" "linux-ia32")
                  ("aarch64-linux" "linux-arm64")
                  ("armhf-linux" "linux-armv7l"))
                ".zip"))
              (sha256 (base32 "08illknzcikzzsb6i7z1p2xgb20jjc5cx9hynll25f44q9pg48b6"))))
    (build-system chromium-binary-build-system)
    (arguments
     `(#:wrapper-plan
       `("electron"
         "libffmpeg.so"
         "libGLESv2.so"
         "libEGL.so")
       #:install-plan
       `(("." "share/electron/" #:include
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
       (modify-phases %standard-phases
         (add-before 'install-wrapper 'wrap-where-patchelf-does-not-work
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/share/electron/electron"))
                    (wrapper (string-append out "/bin/electron")))
               (mkdir-p (dirname wrapper))
               (make-wrapper wrapper bin
                             `("LD_LIBRARY_PATH" ":"
                               prefix
                               (,(string-join
                                  (list
                                   (string-append out "/share/electron"))
                                  ":")))))
             #t)))))
    (native-inputs `(("unzip" ,unzip)))
    (inputs `(("gdk-pixbuf" ,gdk-pixbuf)
              ("ffmpeg" ,ffmpeg)))
    (home-page "https://www.electronjs.org/")
    (synopsis "Cross platform desktop application shell")
    (description "The Electron framework lets you write cross-platform desktop
applications using JavaScript, HTML and CSS.  It is based on Node.js and
Chromium and is used by the Atom editor and many other apps.")
    (license (license:nonfree
              (string-append "https://github.com/electron/electron/blob/v"
                             version "/LICENSE")))))
