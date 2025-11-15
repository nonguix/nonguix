;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2025 John Kehayias <john@guixotic.coop>
;;; Copyright © 2025 Robin Templeton <robin@guixotic.coop>

(define-module (nongnu packages video)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
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
  #:use-module (nongnu packages chromium)
  #:use-module (nongnu packages nvidia)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix nonguix-license:))

(define-public ffmpeg/nvidia
  (package
    (inherit ffmpeg)
    (name "ffmpeg-nvidia")
    (inputs
     (modify-inputs
         (package-inputs ffmpeg)
       (prepend nv-codec-headers)))
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:configure-flags flags)
        ;; Currently only interested in NVENC.
        ;; Might be better to make a ffmpeg-nonfree with all nonfree codecs
        ;; in the future.
        #~(cons* "--enable-cuvid"
                 "--enable-ffnvcodec"
                 "--enable-encoder=hevc_nvenc"
                 "--enable-encoder=h264_nvenc"
                 #$flags))))
    (description
     (string-append
      (package-description ffmpeg)
      "  This build of FFmpeg includes the nonfree NVIDIA encoder for
@code{h264_nvenc} and @code{hevc_nvenc} hardware encoding on NVIDIA GPUs."))
    (properties '((upstream-name . "ffmpeg")))))

(define-deprecated-package ffmpeg-nvenc ffmpeg/nvidia)

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
     (substitute-keyword-arguments (package-arguments intel-media-driver)
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
     (modify-inputs (package-inputs mpv)
       (prepend nv-codec-headers)))
    (synopsis
     "Audio and video player (with hardware acceleration for NVIDIA graphics \
cards)")))

(define-public nv-codec-headers
  (package
    (name "nv-codec-headers")
    (version "13.0.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.videolan.org/git/ffmpeg/nv-codec-headers.git")
             (commit (string-append "n" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01p6bjbgm6hfc1snf0hw63b7f7hif40v7bb1xn84ic3cww2m2fcw"))))
    (arguments
     (list
      #:tests? #f ; No tests.
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (add-after 'unpack 'fix-paths
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "include/ffnvcodec/dynlink_loader.h"
                         (("lib.*\\.so\\.." lib)
                          (search-input-file
                           inputs (string-append "lib/" lib)))))))))
    (build-system gnu-build-system)
    (inputs (list nvidia-driver))
    (home-page "https://git.videolan.org/?p=ffmpeg/nv-codec-headers.git")
    (synopsis
     "FFmpeg version of headers required to interface with NVIDIA's codec APIs")
    (description
     "This package provides the necessary headers for interfacing with NVIDIA's
codec APIs.")
    (license license:expat)))

(define-public nvidia-vaapi-driver
  (package
    (name "nvidia-vaapi-driver")
    (version "0.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elFarto/nvidia-vaapi-driver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ycrik4sdiy14miqvin5vg79776p7p2pazm0s8la4kngbgss1qr9"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'fix-install-path
                 (lambda _
                   (substitute* "meson.build"
                     (("(nvidia_install_dir = ).*" _ prefix)
                      (format #f "~a'~a/lib/dri'" prefix #$output))))))))
    (native-inputs (list pkg-config))
    (inputs (list libva mesa nv-codec-headers))
    ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
    ;; this to all VA-API back ends instead of once to libva.
    (native-search-paths
     (list (search-path-specification
            (variable "LIBVA_DRIVERS_PATH")
            (files '("lib/dri")))))
    (home-page "https://github.com/elFarto/nvidia-vaapi-driver")
    (synopsis "VA-API implemention using NVIDIA's NVDEC.")
    (description
     "This is an VA-API implementation that uses NVDEC as a backend,
specifically designed to be used by Firefox for accelerated decoding of web
content.")
    (license license:expat)))

(define-public obs-nvidia
  (package/inherit obs
    (name "obs-nvidia")
    (arguments
     (substitute-keyword-arguments (package-arguments obs)
       ((#:configure-flags flags #~'())
        #~(append #$flags '("-DENABLE_NVENC=ON")))))
    (inputs
     (modify-inputs (package-inputs obs)
       (prepend nv-codec-headers)))
    (synopsis
     "Live streaming software (with hardware acceleration for NVIDIA graphics
cards)")))

(define-public obs-with-cef
  (package
    (inherit obs)
    (name "obs-with-cef")
    (inputs
     (append (package-inputs obs)
             `(("chromium-embedded-framework" ,chromium-embedded-framework))))
    (arguments
     (substitute-keyword-arguments (package-arguments obs)
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
