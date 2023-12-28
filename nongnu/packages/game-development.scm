;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2022 Attila Lendvai <attila@lendvai.name>

(define-module (nongnu packages game-development)
  #:use-module (ice-9 match)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
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
         (delete 'binary-unpack)
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
  ;; There are no official releases.
  (let ((commit "188e14622cfe5c6f63b04b989b350bf2a29a893c")
        (revision "1")
        (duke-nukem-3d-directory "share/dukenukem3d"))
    (package
      (name "eduke32")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://voidpoint.io/terminx/eduke32.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0wy4bppiw4q2hn0v38msrjyvj2hzfvigakc23c2wqfnbl7rm0hrz"))
         ;; Unbundle libxmp.
         (modules '((guix build utils)))
         (snippet
          '(begin (delete-file-recursively "source/libxmp-lite") #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         ;; Add glu to rpath so that SDL can dlopen it.
         #:make-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                           (assoc-ref %build-inputs "glu") "/lib"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unbundle-libxmp
             (lambda _
               (substitute* "GNUmakefile"
                 (("-I\\$\\(libxmplite_inc\\)")
                  (string-append "-I" (assoc-ref %build-inputs "libxmp") "/include"))
                 (("^ *audiolib_deps \\+= libxmplite.*$") "")
                 (("-logg") "-logg -lxmp"))
               (with-directory-excursion "source/audiolib/src"
                 (for-each (lambda (file) (substitute* file (("libxmp-lite/") "")))
                                   '("multivoc.cpp" "xmp.cpp")))
               #t))
           (delete 'configure)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (glu (assoc-ref inputs "glu"))
                      (eduke (string-append out "/bin/eduke32"))
                      (eduke-real (string-append out "/bin/.eduke32-real")))
                 ;; TODO: Install custom .desktop file?  Need icon.
                 ;; See https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=eduke32.
                 (install-file "eduke32" (string-append out "/bin"))
                 (install-file "mapster32" (string-append out "/bin"))
                 (install-file "package/common/buildlic.txt"
                               (string-append out "/share/licenses"))
                 ;; Wrap program:
                 ;; - Make sure current directory is writable, else eduke32 will segfault.
                 ;; - Add ../share/dukenukem3d to the dir search list.
                 ;; TODO: Skip store duke3d.grp When ~/.config/eduke32/duke3d.grp is found.
                 (rename-file eduke eduke-real)
                 (call-with-output-file eduke
                   (lambda (p)
                     (format p "\
#!~a
mkdir -p ~~/.config/eduke32
cd ~~/.config/eduke32
exec -a \"$0\" ~a\
 -g \"${0%/*}\"/../~a/*.grp\
 -g \"${0%/*}\"/../~a/*.zip\
 -g \"${0%/*}\"/../~a/*.map\
 -g \"${0%/*}\"/../~a/*.con\
 -g \"${0%/*}\"/../~a/*.def\
 \"$@\"~%"
                             (which "bash") eduke-real
                             ,duke-nukem-3d-directory
                             ,duke-nukem-3d-directory
                             ,duke-nukem-3d-directory
                             ,duke-nukem-3d-directory
                             ,duke-nukem-3d-directory)))
                 (chmod eduke #o755)))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("sdl-union" ,(sdl-union (list sdl2 sdl2-mixer)))
         ("alsa-lib" ,alsa-lib)
         ("glu" ,glu)
         ("libvorbis" ,libvorbis)
         ("libvpx" ,libvpx)
         ("libxmp" ,libxmp)
         ("flac" ,flac)
         ("gtk+" ,gtk+-2)))
      (synopsis "Engine of the classic PC first person shooter Duke Nukem 3D")
      (description "EDuke32 is a free homebrew game engine and source port of the
classic PC first person shooter Duke Nukem 3D—Duke3D for short.  A thousands
of features and upgrades were added for regular players and additional editing
capabilities and scripting extensions for homebrew developers and mod
creators.  EDuke32 is open source but non-free software.

This package does not contain any game file.  You can either install packages
with game files or or put @file{.grp} game files manually in
@file{~/.config/eduke32/}.")
      (home-page "https://eduke32.com/")
      (license (license:nonfree
                "https://eduke32.com/buildlic.txt")))))

(define-public fury
  (package
    (inherit eduke32)
    (name "fury")
    (arguments
     (substitute-keyword-arguments (package-arguments eduke32)
       ((#:make-flags flags ''()) `(cons* "FURY=1" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda _
               (let* ((out (assoc-ref %outputs "out")))
                 (install-file "fury" (string-append out "/bin"))
                 (install-file "mapster32" (string-append out "/bin"))
                 (install-file "package/common/buildlic.txt"
                               (string-append out "/share/licenses")))
               #t))))))
    (synopsis "Game engine for the first-person shooter Ion Fury")
    (description
     (string-append
      "This is the @code{eduke32} engine built with support for the Ion Fury
game.  Game data is not provided.  Run @command{fury} with the option
@option{-j} to specify the directory containing @file{fury.grp}."))))
