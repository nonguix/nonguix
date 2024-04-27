;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nongnu packages productivity)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public anytype
  (package
    (name "anytype")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://anytype-release.fra1.cdn.digitaloceanspaces.com/"
                       name "_" version "_amd64.deb"))
       (file-name (string-append "anytype-" version ".deb"))
       (sha256
        (base32
         "01q6dzks8hjb2whdkj7c816fji7rn5dpx00ss7rxgvxb5rdz19gr"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; almost 300MB
      #:substitutable? #f
      #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~(map (lambda (file)
               (string-append "opt/Anytype/" file))
             '("anytype"
               "chrome-sandbox"
               "chrome_crashpad_handler"
               "libEGL.so"
               "libffmpeg.so"
               "libGLESv2.so"
               "libvk_swiftshader.so"
               "libvulkan.so.1"
               "resources/app.asar.unpacked/node_modules/keytar/build/Release/keytar.node"
               "resources/app.asar.unpacked/node_modules/keytar/build/Release/obj.target/keytar.node"))
      #:install-plan
      #~'(("opt/" "/share")
          ("usr/share/" "/share"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'binary-unpack 'strip-python
            (lambda _
              (delete-file
               (string-append "opt/Anytype/resources/app.asar.unpacked/"
                              "node_modules/keytar/build/node_gyp_bins/python3"))))
          (add-before 'install 'patch-assets
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (usr/share "./usr/share")
                     (old-exe "/opt/Anytype/anytype")
                     (exe (string-append bin "/anytype")))
                (substitute* (string-append usr/share "/applications/anytype.desktop")
                  (((string-append "^Exec=" old-exe)) (string-append "Exec=" exe))))))
          (add-before 'install-wrapper 'symlink-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (exe (string-append bin "/anytype"))
                     (share (string-append #$output "/share/Anytype"))
                     (target (string-append share "/anytype")))
                (mkdir-p bin)
                (symlink target exe)
                (wrap-program exe
                  `("LD_LIBRARY_PATH" = (,share)))))))))
    (inputs
     (list bzip2
           flac
           gdk-pixbuf
           harfbuzz
           libexif
           libglvnd
           libpng
           libva
           libxscrnsaver
           opus
           pciutils
           snappy
           util-linux
           xdg-utils
           wget))
    (synopsis "Productivity and note-taking app")
    (supported-systems '("x86_64-linux"))
    (description "Anytype is an E2E encrypted, cross-platform, productivity and
note taking app. It stores all the data locally and allows for peer-to-peer
synchronization.")
    (home-page "https://anytype.io")
    (license (license:nonfree
              "https://github.com/anyproto/anytype-ts/blob/main/LICENSE.md"))))
