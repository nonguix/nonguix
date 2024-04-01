;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2022 Attila Lendvai <attila@lendvai.name>
;;; Copyright © 2024 Timotej Lazar <timotej.lazar@araneo.si>

(define-module (nongnu packages game-development)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg))

(define nvidia-cg-toolkit-version "3.1")
(define nvidia-cg-toolkit-date "April2012")

(define nvidia-cg-toolkit-i686-archive
  (origin
    (method url-fetch)
    (uri
     (string-append "http://developer.download.nvidia.com/cg/Cg_"
                    nvidia-cg-toolkit-version
                     "/Cg-" nvidia-cg-toolkit-version "_" nvidia-cg-toolkit-date "_"
                     "x86" ".tgz"))
    (sha256
     (base32 "0yc8n6vpqyb6qhcv5kwvr3h21ya271fi930fvd98hlkg8cg5kwyf"))))

(define nvidia-cg-toolkit-x86_64-archive
  (origin
    (method url-fetch)
    (uri
     (string-append "http://developer.download.nvidia.com/cg/Cg_"
                    nvidia-cg-toolkit-version
                     "/Cg-" nvidia-cg-toolkit-version "_" nvidia-cg-toolkit-date "_"
                     "x86_64" ".tgz"))
    (sha256
     (base32 "0y4qms4lm9xiix93g45337rx5nrp0y3gb0x0avyv7l9qrkk03zz8"))))

(define (lib)
  (if (string=? (or (%current-target-system) (%current-system)) "x86_64-linux")
      "lib64" "lib"))

(define-public nvidia-cg-toolkit
  (package
    (name "nvidia-cg-toolkit")
    (version nvidia-cg-toolkit-version)
    (source #f)
    (build-system binary-build-system)
    (arguments
     `(#:strip-binaries? #f ; Fails with "allocated section `.dynstr' not in segment".
       #:patchelf-plan
       `(("bin/cgc"
          ("glibc" "out"))
         ("bin/cginfo"
          ("gcc:lib" "glibc"))
         ("bin/cgfxcat"
          ("out" "glibc" "glu" "mesa" "libice" "libsm"
           "libxmu" "libxt" "libxi" "libxext" "libx11"))
         (,,(string-append (lib) "/libCg.so")
          ("glibc"))
         (,,(string-append (lib) "/libCgGL.so")
          ("out" "glibc")))
       #:install-plan
       `(("bin" "./")
         (,,(lib) "lib")
         ("include" "./")
         ("local/" "share/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((archive (assoc-ref inputs "nvidia-cg-toolkit-archive")))
               (invoke "tar" "xf" archive)
               (chdir "usr")
               #t))))))
    (native-inputs
     `(("nvidia-cg-toolkit-archive"
        ,(match (or (%current-target-system) (%current-system))
           ("x86_64-linux" nvidia-cg-toolkit-x86_64-archive)
           (_ nvidia-cg-toolkit-i686-archive)))))
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ("glibc" ,glibc)
       ("glu" ,glu)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libxmu" ,libxmu)
       ("libxt" ,libxt)
       ("libxi" ,libxi)
       ("libxext" ,libxext)
       ("libx11" ,libx11)))
    (home-page "https://developer.nvidia.com/cg-toolkit")
    (synopsis "High-level shading language")
    (description "NVIDIA introduced programmable shading with Cg, which
supported dozens of different OpenGL and DirectX profile targets.  It allowed
developers to incorporate interactive effects within 3D applications and share
them among other Cg applications, across graphics APIs, and most operating
systems as well as balance effect complexities with client GPU capabilities.

The Cg Toolkit is a legacy NVIDIA toolkit no longer under active development
or support.  It is not recommended using it in new development projects
because future hardware features may not be supported.  Going forward, new
development should opt for GLSL rather than Cg.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license (license:nonfree "file://share/Cg/docs/license.txt"))))

(define-public libsteam
  (package
    (name "libsteam")
    (version "2013")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ValveSoftware/source-sdk-"
                           version
                           "/raw/master/sp/src/lib/public/linux32/libsteam_api.so"))
       (sha256
        (base32
         "1ivxvikm8i6mmmqvib8j5m7g5n1cdlki2sf4v7g13c7xba7aj438"))))
    (build-system binary-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:system "i686-linux"
       #:patchelf-plan
       `(("libsteam_api.so" ("gcc" "glibc")))
       #:install-plan
       `(("libsteam_api.so" "lib/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file (assoc-ref inputs "source") "libsteam_api.so")
             (chmod "libsteam_api.so" #o644)
             #t))
         (add-after 'install 'symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (symlink (string-append out "/lib/libsteam_api.so")
                        (string-append out "/lib/libsteam_api.so.1")))
             #t)))))
    (inputs
     (list (list gcc "lib") glibc))
    (home-page "https://developer.valvesoftware.com/wiki/SDK2013_GettingStarted")
    (synopsis "Redistribution binary needed by some video games")
    (description "")
    (license (license:nonfree
              "https://raw.githubusercontent.com/ValveSoftware/source-sdk-2013/master/LICENSE"))))

(define-public eduke32
  (package
    (name "eduke32")
    (version "20240316-10564-0bc78c53d")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dukeworld.com/eduke32/synthesis/"
                           version "/eduke32_src_" version ".tar.xz"))
       (sha256
        (base32 "1a9fw1kfriyrybjxl72b2434w3yiz2nxg6541lnyhzbdka2cp2lf"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libxmp and platform-specific stuff.
        #~(for-each delete-file-recursively '("platform" "source/libxmp-lite")))))
    (build-system gnu-build-system)
    (arguments
     (list #:license-file-regexp "buildlic.txt"
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'link-license
                 (lambda _
                   ;; Ensure the install-license-files phase can find it.
                   (link "package/common/buildlic.txt" "buildlic.txt")))
               (add-after 'unpack 'unbundle-libxmp
                 (lambda _
                   (substitute* "Common.mak"
                     (("^LIBS :=" match) (string-append match " -lxmp")))
                   (with-directory-excursion "source/audiolib/src"
                     (for-each (lambda (file) (substitute* file (("libxmp-lite/") "")))
                               '("multivoc.cpp" "xmp.cpp")))))
               (add-after 'unpack 'fix-share-path
                 (lambda _
                   (substitute* "source/duke3d/src/common.cpp"
                     (("/usr/local/share/games") (string-append #$output "/share")))))
               (delete 'configure)
               (replace 'install
                 (lambda _
                   (let ((bin (string-append #$output "/bin")))
                     (install-file "eduke32" bin)
                     (install-file "mapster32" bin)
                     (install-file "package/sdk/m32help.hlp"
                                   (string-append #$output "/share/eduke32"))))))))
    (inputs (list alsa-lib
                  flac
                  glu
                  gtk+-2
                  libvorbis
                  libvpx
                  libxmp
                  sdl2
                  sdl2-mixer))
    (native-inputs
     (list gdk-pixbuf pkg-config))
    (synopsis "Engine of the classic PC first person shooter Duke Nukem 3D")
    (description "EDuke32 is a free homebrew game engine and source port of
the classic PC first person shooter Duke Nukem 3D—Duke3D for short.  A
thousands of features and upgrades were added for regular players and
additional editing capabilities and scripting extensions for homebrew
developers and mod creators.  EDuke32 is open source but non-free software.

This package does not contain any game file.  You can either install packages
with game files or or put @file{.grp} game files manually in
@file{~/.config/eduke32/}.")
    (home-page "https://eduke32.com")
    (license
     (list license:gpl2
           (license:nonfree "file://package/common/buildlic.txt")))))

(define-public fury
  (package/inherit eduke32
    (name "fury")
    (arguments
     (substitute-keyword-arguments (package-arguments eduke32)
       ((#:make-flags flags #~'())
        #~(cons* "FURY=1" #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'disable-sdl-static
              (lambda _
                (substitute* "GNUmakefile"
                  (("SDL_STATIC := 1") ""))))
            (replace 'install
              (lambda _
                (install-file "fury" (string-append #$output "/bin"))))))))
    (inputs
       (alist-delete "libvpx" (package-inputs eduke32)))
    (synopsis "Game engine for the first-person shooter Ion Fury")
    (description
     "This is the @code{eduke32} engine built with support for the Ion Fury
game.  Game data is not provided.  Run @command{fury} with the option
@option{-j} to specify the directory containing @file{fury.grp}.")))
