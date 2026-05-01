;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2025 John Kehayias <john@guixotic.coop>
;;; Copyright © 2025 Robin Templeton <robin@guixotic.coop>
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>

(define-module (nongnu packages video)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nongnu packages)
  #:use-module (nongnu packages chromium)
  #:use-module (nongnu packages nvidia)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix nonguix-license:))

(define-public gmmlib
  (package
    (name "gmmlib")
    (version "22.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/gmmlib")
                    (commit (string-append "intel-gmmlib-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d52q2m0x4ys2w8b75q0hgn23q9474a5mj0xb6fmnw69a5gha5p6"))))
    (build-system cmake-build-system)
    (arguments
     ;; Tests are run as part of the normal build step
     '(#:tests? #f))
    (home-page "https://github.com/intel/gmmlib")
    (synopsis "Intel Graphics Memory Management Library")
    (description
     "This package provides device specific and buffer management for the
Intel Graphics Compute Runtime for OpenCL and the Intel Media Driver
for VAAPI.")
    (license license:expat)))

(define-public intel-media-driver
  (package
    (name "intel-media-driver")
    (version "25.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/media-driver")
                    (commit (string-append "intel-media-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rp4s9a4x26p07w36ywql7qz7gyk15mgp9yrdx9j2b9qbmr1w1zs"))))
    (build-system cmake-build-system)
    (inputs (list libva gmmlib))
    (native-inputs (list pkg-config))
    (arguments
     (list #:tests? #f ;Tests are run as part of the normal build step
           #:configure-flags
           #~(list "-DENABLE_NONFREE_KERNELS=OFF"
                   (string-append "-DLIBVA_DRIVERS_PATH="
                                  #$output "/lib/dri"))))
    ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
    ;; this to all VA-API back ends instead of once to libva.
    (native-search-paths
     (list (search-path-specification
            (variable "LIBVA_DRIVERS_PATH")
            (files '("lib/dri")))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://github.com/intel/media-driver")
    (synopsis "Intel Media Driver for VAAPI")
    (description
     "This package provides a VA-API user mode driver supporting hardware
accelerated decoding, encoding, and video post processing for the GEN based
graphics hardware.")
    (license (list license:expat license:bsd-3))))

(define-public intel-media-driver/nonfree
  (package
    (inherit intel-media-driver)
    (name "intel-media-driver-nonfree")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:configure-flags flags #~'())
        #~(cons "-DENABLE_NONFREE_KERNELS=ON"
                (delete "-DENABLE_NONFREE_KERNELS=OFF" #$flags)))))
    (synopsis
       (string-append
        (package-synopsis intel-media-driver)
        " with nonfree kernels"))
    (description
       (string-append
        (package-description intel-media-driver)
        "  This build of intel-media-driver includes nonfree blobs to fully enable the
video decode capabilities of supported Intel GPUs."))))

(define-public mpv-nvidia
  (package
    (inherit mpv)
    (name "mpv-nvidia")
    (inputs
     (modify-inputs inputs
       (prepend nv-codec-headers)))
    (synopsis
     "Audio and video player (with hardware acceleration for NVIDIA graphics \
cards)")))

(define-public obs-nvidia
  (package/inherit obs
    (name "obs-nvidia")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:configure-flags flags #~'())
        #~(append #$flags '("-DENABLE_NVENC=ON")))))
    (inputs
     (modify-inputs inputs
       (prepend nv-codec-headers)))
    (synopsis
     "Live streaming software (with hardware acceleration for NVIDIA graphics
cards)")))

(define-public obs-with-cef
  (package
    (inherit obs)
    (name "obs-with-cef")
    (inputs
     (modify-inputs inputs
       (append chromium-embedded-framework)))
    (arguments
     (substitute-keyword-arguments arguments
       ((#:configure-flags flags)
        #~(append #$flags
                  '("-DBUILD_BROWSER=ON"
                    "-DCEF_ROOT_DIR=../source/cef")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'add-cef
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((chromium-embedded-framework
                       #$(this-package-input "chromium-embedded-framework")))
                  (mkdir-p "cef/Release")
                  (mkdir-p "cef/Resources")
                  (for-each (lambda (file)
                              (symlink file (string-append "cef/Release/"
                                                           (basename file)))
                              (symlink file (string-append "cef/Resources/"
                                                           (basename file))))
                            (filter
                             (lambda (file)
                               (not (string= (basename (dirname file))
                                             "locales")))
                             (find-files
                              (string-append chromium-embedded-framework
                                             "/share/cef"))))
                  (symlink (string-append chromium-embedded-framework
                                          "/lib/libcef.so")
                           "cef/Release/libcef.so")
                  (mkdir-p "cef/libcef_dll_wrapper")
                  (symlink (string-append chromium-embedded-framework
                                          "/lib/libcef_dll_wrapper.a")
                           "cef/libcef_dll_wrapper/libcef_dll_wrapper.a")
                  (symlink (string-append chromium-embedded-framework
                                          "/include")
                           "cef/include"))))
            (add-after 'install 'symlink-obs-browser
              ;; Required for lib/obs-plugins/obs-browser.so file.
              (lambda* (#:key outputs #:allow-other-keys)
                (symlink
                 (string-append #$output
                                "/lib/libobs-frontend-api.so.0")
                 (string-append #$output
                                "/lib/obs-plugins/libobs-frontend-api.so.0"))
                (symlink
                 (string-append #$output
                                "/lib/libobs.so.0")
                 (string-append #$output
                                "/lib/obs-plugins/libobs.so.0"))))))))
    (description
     (string-append
      (package-description obs)
      "  This build of OBS includes embeded Chromium-based browser to enable
Browser source."))))

(define-public grayjay
  (package
    (name "grayjay")
    (version "12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://updater.grayjay.app/Apps/Grayjay.Desktop/"
                           version "/Grayjay.Desktop-linux-x64-v"
                           version ".zip"))
       (file-name (string-append name "-" version "-x86_64.zip"))
       (sha256
        (base32 "0m0sq3qwg21wgyplsgq421wryl22gg9c64jnpanzgzsm68aql05s"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f         ; prevent corruption of .NET programs
           #:patchelf-plan
           #~(let ((libs '("alsa-lib"
                           "at-spi2-core"
                           "cairo"
                           "cups"
                           "dbus"
                           "eudev"
                           "expat"
                           "fontconfig-minimal"
                           "gcc"
                           "glib"
                           "glibc"
                           "gtk+"
                           "icu4c"
                           "libdrm"
                           "libnotify"
                           "librsvg"
                           "libsecret"
                           "libx11"
                           "libxcb"
                           "libxcomposite"
                           "libxcursor"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxi"
                           "libxkbcommon"
                           "libxkbfile"
                           "libxrandr"
                           "libxrender"
                           "libxshmfence"
                           "libxtst"
                           "mesa"
                           "mit-krb5"
                           "nspr"
                           ("nss" "/lib/nss")
                           ("out" "/lib/grayjay/cef")
                           "openssl"
                           "pango"
                           "pulseaudio"
                           "sqlcipher"
                           "xcb-util"
                           "xcb-util-image"
                           "xcb-util-keysyms"
                           "xcb-util-renderutil"
                           "xcb-util-wm"
                           "xdg-utils"
                           "zlib")))
               `(("ClearScriptV8.linux-x64.so" ,libs)
                 ("Grayjay" ,libs)
                 ("cef/chrome-sandbox" ,libs)
                 ("cef/dotcefnative" ,libs)
                 ;; Some of these likely are not directly used after
                 ;; patchelf-ing the main binaries, other than libcef.so.
                 ;; This allows validate-runpath to pass though.
                 ("cef/libEGL.so" ,libs)
                 ("cef/libGLESv2.so" ,libs)
                 ;; XXX: Can replace with chromium-embedded-framework?
                 ("cef/libcef.so" ,libs)
                 ("cef/libsteam_api.so" ,libs)
                 ("cef/libvk_swiftshader.so" ,libs)
                 ("cef/libvulkan.so.1" ,libs)
                 ("libe_sqlite3.so" ,libs)
                 ("libfcast_sender_sdk.so" ,libs)
                 ("libsodium.so" ,libs)))
           #:install-plan ''(("." "lib/grayjay"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'remove-files
                 (lambda _
                   ;; Disable automatic updates, unbundle ffmpeg, and remove
                   ;; "Portable" which makes Grayjay try to (unsuccessfully)
                   ;; run from its installed path.  (Grayjay doesn't find the
                   ;; updater or ffmpeg when run outside of lib/grayjay.)
                   (delete-file "FUTO.Updater.Client")
                   (delete-file "ffmpeg")
                   (delete-file "Portable")))
               (add-before 'install 'install-entrypoint
                 (lambda _
                   (let* ((bin (string-append #$output "/bin")))
                     (mkdir-p bin)
                     (symlink (string-append #$output "/lib/grayjay/Grayjay")
                              (string-append bin "/Grayjay")))))
               (add-before 'install 'install-icon
                 (lambda _
                   (let ((dir (string-append
                               #$output
                               "/share/icons/hicolor/scalable/apps")))
                     (mkdir-p dir)
                     (copy-file "grayjay.png"
                                (string-append dir
                                               "/app.grayjay.Grayjay.png")))))
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs #:allow-other-keys)
                   (wrap-program (string-append #$output "/lib/grayjay/Grayjay")
                     `("PATH" prefix
                       (,(string-append #$(this-package-input "ffmpeg")
                                        "/bin"))))))
               (add-after 'install 'create-desktop-file
                 (lambda _
                   (make-desktop-entry-file
                    (string-append #$output "/share/applications/Grayjay.desktop")
                    #:name "Grayjay"
                    #:type "Application"
                    #:exec (string-append #$output "/bin/Grayjay")
                    #:icon "app.grayjay.Grayjay"
                    #:categories '("AudioVideo" "Player")
                    #:startup-w-m-class "Grayjay"
                    #:comment "Universal media aggregator"))))))
    (native-inputs (list unzip))
    (inputs (list alsa-lib
                  at-spi2-core
                  bash-minimal
                  cairo
                  cups
                  dbus
                  eudev
                  expat
                  ffmpeg
                  fontconfig
                  freetype
                  `(,gcc "lib")
                  glib
                  glibc
                  gtk+
                  icu4c-76
                  libdrm
                  libnotify
                  librsvg
                  libsecret
                  libx11
                  libxcb
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxkbcommon
                  libxkbfile
                  libxrandr
                  libxrender
                  libxshmfence
                  libxtst
                  mesa
                  mit-krb5
                  nspr
                  nss
                  openssl
                  pango
                  pulseaudio
                  sqlcipher
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-renderutil
                  xcb-util-wm
                  xdg-utils
                  zlib))
    (home-page "https://grayjay.app/")
    (synopsis "Universal media aggregator")
    (description "Grayjay is a media aggregator application that enables users
to stream and download multimedia content from various online sources, most
prominently YouTube.  It also offers an extensible plugin API to create and
import new integrations.")
    (license
     ;; "Source First License 1.1" which allows distribution, modification,
     ;; etc. but with a non-commercial prohibition.
     (nonguix-license:nonfree
      "https://gitlab.futo.org/videostreaming/grayjay/-/blob/master/LICENSE.md"))))

(define-public makemkv
  (package
    (name "makemkv")
    ;; This is not the last version, but newer ones like 1.18.3 have a bug
    ;; where the 'makemkvcon' process hang at 100% CPU when attempting to read
    ;; a Blu-ray disc, as reported by multiple users (see for example:
    ;; <https://forum.makemkv.com/forum/viewtopic.php?t=35897> and
    ;; <https://forum.makemkv.com/forum/viewtopic.php?p=178014>).
    (version "1.17.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.makemkv.com/download/old/"
                                  name "-oss-" version ".tar.gz"))
              (sha256
               (base32
                "1vx0sf8y5kl0l3szc3hd28anm7pxq2bpvjrdqskpbv7r8qnmabkn"))
              (patches (nongnu-patches "makemkv-app-id.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules (cons '(guix build qt-utils)
                               %default-gnu-imported-modules)
      #:modules (cons '(guix build qt-utils)
                      %default-gnu-modules)
      #:tests? #f                     ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-ldconfig-invocation
            (lambda _
              (substitute* "Makefile.in"
                (("\tldconfig.*") ""))))
          (add-after 'install 'install-makemkv-bin
            ;; This is the closed-source binary component of makemkv, which
            ;; contains e.g. the 'makemkvcon' executable for retrieving keys
            ;; from their server.
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (invoke "tar" "xf"
                      #$(this-package-native-input
                         (format #f "makemkv-bin-~a.tar.gz"
                                 (package-version this-package))))
              (with-directory-excursion #$(string-append "makemkv-bin-" version)
                (substitute* "Makefile"
                  ;; Automatically accept the EULA non-interactively.
                  (("@/bin/bash src/ask_eula.sh") "true"))
                (apply (assoc-ref %standard-phases 'install)
                       `(,@args #:make-flags
                                (,(string-append "PREFIX="
                                                 #$output))))
                (install-file "src/eula_en_linux.txt"
                              (string-append #$output "/share/MakeMKV")))
              ;; Fix the RUNPATH of the makemkvcon binary.
              (let ((makemkvcon (string-append #$output "/bin/makemkvcon")))
                (invoke "patchelf" "--set-rpath"
                        ;; libcurl is dlopen'ed from makemkvcon
                        (string-append #$output "/lib:"
                                       (dirname (search-input-file
                                                 inputs "lib/libcurl.so")))
                        makemkvcon)
                (invoke "patchelf" "--set-interpreter"
                        (search-input-file inputs #$(glibc-dynamic-linker))
                        makemkvcon))))
          (add-after 'install 'wrap-qt
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-qt-program "makemkv"
                               #:output #$output
                               #:inputs inputs))))))
    (native-inputs
     (list patchelf
           pkg-config
           (origin
             (method url-fetch)
             (uri (string-append "https://www.makemkv.com/download/old/"
                                 name "-bin-" version ".tar.gz"))
             (sha256
              (base32
               "1l2ii5k6bjgzy20d29mng4j0pnwjwdj0qif87j3iyawmphqwhnwc")))))
    (inputs (list curl ffmpeg-6 expat openssl qtbase-5 qtwayland-5 zlib))
    (home-page "https://www.makemkv.com")
    (synopsis "Video converter with support for Blu-ray and DVD encryption")
    (description "MakeMKV allows converting the video clips from
proprietary (and usually encrypted) discs into a set of MKV files, preserving
most information but not changing it in any way.  The MKV format can store
multiple video/audio tracks with all meta-information and preserve chapters.
There are many players that can play MKV files nearly on all platforms, and
there are tools to convert MKV files to many formats, including DVD and
Blu-ray discs.

Additionally, MakeMKV can instantly stream decrypted video without
intermediate conversion to wide range of players, so you may watch Blu-ray and
DVD discs with your favorite player.  This is made possible via its
@code{libmmdb} library, which can act as a replacement for the @code{libaacs}
library.  To use it with VLC for example, you can force its use instead of the
regular @code{libaacs} library by setting the following (@code{libbluray},
used by VLC) environment variable:

@example
guix install makemkv vlc
export MAKEMKVCON=$(which makemkvcon)
export LIBAACS_PATH=$HOME/.guix-profile/lib/libmmbd
export LIBBDPLUS_PATH=$HOME/.guix-profile/lib/libmmbd
vlc /dev/sr0
@end example

Among its features are:
@itemize
@item Reads DVD and Blu-ray discs
@item Reads Blu-ray discs protected with latest versions of AACS and BD+
@item Preserves all video and audio tracks, including HD audio
@item Preserves chapters information
@item Preserves all meta-information (track language, audio type)
@item Fast conversion -- converts as fast as your drive can read data
@item No additional software required for conversion or decryption.
@end itemize

IMPORTANT:
@itemize
@item
By installing this package, you agree to its end user license
agreement, which you can read at @file{share/MakeMKV/eula_en_linux.txt}.
@item
Purchasing a license key is required to use this older version.
@item
UHD (4K) Blu-ray playback requires LibreDrive compatibility.  Do your research
before buying a Blu-ray drive!
@end itemize")
    ;; Redistributable, with a proprietary license (shareware).
    (license (nonguix-license:nonfree "file://License.txt"))
    (supported-systems (list "x86_64-linux" "i686-linux"
                             "aarch64-linux" "armhf-linux"))))
