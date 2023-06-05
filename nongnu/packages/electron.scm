;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Pierre Neidhardt <mail@ambrevar.xyz>

(define-module (nongnu packages electron)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages video))

(define-public electron
  (package
    (name "electron")
    (version "24.2.0")
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
              (sha256 (base32 "04vmqr5547059751yxr729ljqahal57lymyglaa5xzpw5bfq0xwd"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       `(("electron"
          ("glib" "atk" "libx11" "dbus" "gdk-pixbuf" "gtk+" "pango"
           "cairo" "libxcomposite" "libxdamage" "libxext" "libxfixes"
           "libxrandr" "expat" "libdrm" "libxkbcommon" "mesa" "alsa-lib"
           "cups" "at-spi2-core" "gcc:lib" "libxcb" "at-spi2-atk" "nspr"))
         ("libffmpeg.so"
          ("gcc:lib"))
         ("libGLESv2.so"
          ("gcc:lib" "libx11" "libxcb" "libxext"))
         ("libEGL.so"
          ("gcc:lib")))
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
         (add-after 'install 'wrap-where-patchelf-does-not-work
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
                                   (string-append (assoc-ref inputs "nss") "/lib/nss")
                                   (string-append (assoc-ref inputs "eudev") "/lib")
                                   (string-append (assoc-ref inputs "mesa") "/lib")
                                   (string-append out "/share/electron"))
                                  ":")))))
             #t)))))
    (native-inputs `(("unzip" ,unzip)))
    (inputs `(("glib" ,glib)
              ("nss" ,nss)
              ("nspr" ,nspr)
              ("atk" ,atk)
              ("libx11" ,libx11)
              ("dbus" ,dbus)
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("gtk+" ,gtk+)
              ("pango" ,pango)
              ("cairo" ,cairo)
              ("ffmpeg" ,ffmpeg)
              ("libxcomposite" ,libxcomposite)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("libxfixes" ,libxfixes)
              ("libxrandr" ,libxrandr)
              ("expat" ,expat)
              ("libdrm" ,libdrm)
              ("libxkbcommon" ,libxkbcommon)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("cups" ,cups)
              ("at-spi2-core" ,at-spi2-core)
              ("gcc:lib" ,gcc "lib")
              ("libxcb" ,libxcb)
              ("at-spi2-atk" ,at-spi2-atk)
              ("eudev" ,eudev)))
    (home-page "https://www.electronjs.org/")
    (synopsis "Cross platform desktop application shell")
    (description "The Electron framework lets you write cross-platform desktop
applications using JavaScript, HTML and CSS.  It is based on Node.js and
Chromium and is used by the Atom editor and many other apps.")
    (license (license:nonfree
              (string-append "https://github.com/electron/electron/blob/v"
                             version "/LICENSE")))))
