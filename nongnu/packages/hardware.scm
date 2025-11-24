;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 James Smith <jsubuntuxp@disroot.org>

(define-module (nongnu packages hardware)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public steamvr-openhmd
  ;; Track controller-haptics-wip
  (let ((commit "19dabd2775ce28fc693824c176844c9adffa437d")
        (revision "0")
        (version "0.0.1"))
    (package
      (name "steamvr-openhmd")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url "https://github.com/thaytan/SteamVR-OpenHMD")
                             (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03hdvrj79fhyg7r1q2qbdy35lp1w2mbvbqhgpm4h01hq6hn5a65i"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (substitute* "meson.build"
               (("openhmd_subproject = subproject\\('openhmd', default_options: \\['default_library=static'\\]\\)")
                "")
               ((", fallback : \\['openhmd', 'openhmd_dep'\\]")
                ""))))))
      (build-system meson-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 ;; Default install target installs libraries in $PREFIX/lib
                 ;; without any resources, so replace it with custom install
                 ;; phase that sets up a proper SteamVR driver.
                 (replace 'install
                   (let ((source-dir "../source/")
                         (steamvr-openhmd
                          (string-append #$output "/share/steamvr-openhmd/")))
                     (lambda _
                       (install-file (string-append source-dir
                                                    "driver.vrdrivermanifest")
                                     steamvr-openhmd)
                       (mkdir-p (string-append steamvr-openhmd "bin/linux64"))
                       (copy-file "driver_openhmd.so.0.0.1"
                                  (string-append steamvr-openhmd "bin/linux64/"
                                                 "driver_openhmd.so"))
                       (copy-recursively (string-append source-dir "resources")
                                         (string-append steamvr-openhmd
                                                        "resources"))))))))
      (inputs (list hidapi libusb opencv openhmd))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/thaytan/SteamVR-OpenHMD")
      (synopsis "SteamVR plugin for OpenHMD drivers")
      (description
       "This package provides a SteamVR plugin for using OpenHMD drivers in
SteamVR.")
      (license license:boost1.0))))
