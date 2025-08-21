;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2025, 2026 Sughosha <sughosha@proton.me>
;;; Copyright © 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages music)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary))

(define-public reaper
  (package
    (name "reaper")
    (version "7.77")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.reaper.fm/files/"
                            (version-major version)
                            ".x/reaper"
                            (string-replace-substring version "." "")
                            "_"
                            (match (%current-system)
                              ("x86_64-linux" "linux_x86_64")
                              ("i686-linux" "linux_i686")
                              ("aarch64-linux" "linux_aarch64")
                              ("armhf-linux" "linux_armv7l")
                              ;; We need a default case
                              (_ "unsupported"))
                            ".tar.xz"))
        (sha256
         (base32
          (match (%current-system)
            ("x86_64-linux"
             "1w8dsrdca72c2cznww869f1j206d6743hjf3afp4p02lgr1rckf9")
            ("i686-linux"
             "0iz27k59plcz1rfgs2fdyyrd7l5g2hsk8m001vnnkysxgrmy6xz1")
            ("aarch64-linux"
             "1kfn9ybk8sfhrg4r9nqy06sawqx45gjc4f0b3arzbrpvl6qwywgz")
            ("armhf-linux"
             "0wzciamkz2nqqsi7xrckfdwjczhf0faidn4fyqv6s2k6hqnyb80d")
            (_ "0000000000000000000000000000000000000000000000000000"))))))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f ;allocated section `.dynsym' not in segment
           #:modules
           '((nonguix build binary-build-system)
             (guix build utils)
             (nonguix build utils)
             (ice-9 match))
           #:patchelf-plan
           #~(map (lambda (file)
                    `(,file
                      ,(append '("libc" "gcc")
                               (match (basename file)
                                 ("reaper" '("alsa-lib" "jack" "pulseaudio"))
                                 ((or "reaper_host_x86_64"
                                      "reaper_host_i686"
                                      "reaper_host_aarch64"
                                      "reaper_host_armv7l")
                                  '("alsa-lib"))
                                 ("reaper_video.so" '("mesa" "vlc"))
                                 ("reaper_mp3dec.so" '("lame"))
                                 (_ '())))))
                  `("REAPER/reaper"
                    ,#$(string-append "REAPER/Plugins/reaper_host_"
                                      (match (%current-system)
                                        ("x86_64-linux" "x86_64")
                                        ("i686-linux" "i686")
                                        ("aarch64-linux" "aarch64")
                                        ("armf-linux" "armv7l")
                                        (_ "unsupported")))
                    ,@(find-files "REAPER/Plugins" "\\.so$")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install
                 (lambda _
                   (setenv "HOME" "/tmp")
                   (setenv "XDG_DATA_HOME" (string-append #$output "/share"))
                   (invoke "sh" "./install-reaper.sh" "--install"
                           (string-append #$output "/opt")
                           "--integrate-user-desktop")
                   (delete-file (string-append #$output "/opt/REAPER/"
                                               "uninstall-reaper.sh"))))
               (add-after 'install 'replace-swell
                 (lambda* (#:key inputs #:allow-other-keys)
                   (with-directory-excursion (string-append #$output
                                                            "/opt/REAPER")
                     (delete-file "libSwell.so")
                     (symlink (search-input-file inputs "/lib/libSwell.so")
                              "libSwell.so"))))
               (add-after 'replace-swell 'wrap-program
                 (lambda _
                   (let ((wrapper (string-append #$output "/bin/reaper"))
                         (bin (string-append #$output "/opt/REAPER/reaper")))
                     (make-wrapper wrapper bin
                       `("PATH" ":" prefix
                         (,(string-append #$(this-package-input "ffmpeg")
                                          "/bin")))))))
               (add-after 'wrap-program 'fix-desktop-entry
                 (lambda _
                   (substitute* (string-append #$output
                                               "/share/applications/"
                                               "cockos-reaper.desktop")
                     (("opt/REAPER") "bin"))))
               (replace 'install-license-files
                 (lambda _
                   (install-file "REAPER/EULA.txt"
                                 (string-append #$output "/share/doc/reaper-"
                                                #$version)))))))
    (native-inputs
      (list xdg-utils))
    (inputs
     (list alsa-lib
           `(,gcc "lib")
           ffmpeg
           jack-1
           lame
           mesa
           pulseaudio
           python
           swell
           vlc))
    (supported-systems
      '("x86_64-linux" "i686-linux" "aarch64-linux" "armhf-linux"))
    (home-page "https://www.reaper.fm")
    (synopsis "Digital audio workstation")
    (description
     "REAPER is a digital audio production application offering multitrack
audio and MIDI recording, editing, processing, mixing and mastering toolset.
It supports a vast range of hardware, digital formats and plugins, and can be
comprehensively extended, scripted and modified.")
    (license (license:nonfree "file:///opt/REAPER/EULA.txt"))))
