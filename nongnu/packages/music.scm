;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Sughosha <sughosha@proton.me>
;;; Copyright © 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages music)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
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
    (version "6.73")
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
                              ("armhf-linux" "linux_armv7l"))
                            ".tar.xz"))
        (sha256
         (base32
          (match (%current-system)
            ("x86_64-linux" "1hd7fbk0px45fxhqa7nqcnij8ls2fhpjp60v840vy2zqs9fkcr52")
            ("i686-linux" "11vk32mxyda9xl08pp2ivd1vsahnw6w7d08in4syz6iannfwp19b")
            ("aarch64-linux" "0zpkaiwwxn8yh3s1d22qswshbgaxx5d8iy17hb3w256zgb722yjw")
            ("armhf-linux" "18174b1lgsk73gxhala471ppzbrpa1cs953b5par998yqgh74znk"))))))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f ;allocated section `.dynsym' not in segment
           #:patchelf-plan #~`(("REAPER/reaper" ("libc" "gcc" "alsa-lib"))
                               ("REAPER/reamote-server" ("libc" "gcc"))
                               ("REAPER/Plugins/reaper_host_x86_64" ("libc" "gcc")))
           #:phases #~(modify-phases %standard-phases
                        (replace 'install
                          (lambda* (#:key outputs inputs #:allow-other-keys)
                            (let* ((target (string-append #$output "/opt"))
                                   (bin (string-append #$output "/bin"))
                                   (libexec (string-append #$output "/libexec"))
                                   (data (string-append #$output "/share"))
                                   (doc (string-append data "/doc/reaper-"
                                                       #$version)))
                              (setenv "HOME" "/tmp")
                              (setenv "XDG_DATA_HOME" data)
                              (invoke "sh" "./install-reaper.sh" "--install"
                                      target "--integrate-user-desktop")
                              (delete-file (string-append target
                                            "/REAPER/uninstall-reaper.sh"))
                              (delete-file (string-append target
                                            "/REAPER/libSwell.so"))
                              (symlink (search-input-file inputs
                                                          "/lib/libSwell.so")
                                       (string-append target
                                                      "/REAPER/libSwell.so"))
                              (mkdir-p bin)
                              (symlink (string-append target "/REAPER/reaper")
                                       (string-append bin "/reaper"))
                              (mkdir-p libexec)
                              (symlink (string-append target
                                        "/REAPER/Plugins/reaper_host_x86_64")
                                       (string-append libexec
                                                      "/reaper_host_x86_64"))
                              (mkdir-p doc)
                              (symlink (string-append target
                                                      "/REAPER/EULA.txt")
                                       (string-append doc "/LICENSE"))))))))
    (native-inputs
      (list
        which
        xdg-utils))
    (inputs
      (list
        alsa-lib
        `(,gcc "lib")
        wdl))
    (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"
                         "armhf-linux"))
    (home-page "https://www.reaper.fm")
    (synopsis "Digital audio workstation")
    (description
     "REAPER is a digital audio production application offering multitrack
audio and MIDI recording, editing, processing, mixing and mastering toolset.
It supports a vast range of hardware, digital formats and plugins, and can be
comprehensively extended, scripted and modified.")
    (license (license:nonfree "file:///opt/REAPER/EULA.txt"))))
