;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Murilo <murilo@disroot.org>

(define-module (nongnu packages video)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages video)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nongnu packages chromium)
  #:use-module (nongnu packages nvidia))

(define-public ffmpeg-nvenc
  (package/inherit ffmpeg
    (name "ffmpeg-nvenc")
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
@code{h264_nvenc} and @code{hevc_nvenc} hardware encoding on NVIDIA GPUs."))))

(define-public replace-ffmpeg-nvenc
  (package-input-rewriting
   `((,ffmpeg . ,ffmpeg-nvenc))))

(define-public gmmlib
  (package
    (name "gmmlib")
    (version "22.3.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/gmmlib")
                    (commit (string-append "intel-gmmlib-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p3wp6xcvpb4jzw4fsf6554qy91iblmq9y50ph3iy29m19q6nznb"))))
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
    (version "24.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/media-driver")
                    (commit (string-append "intel-media-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jm4imld48scj0j499wq5zbdjv4gg7hg2sawljqnjvy09dmp09bs"))))
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

(define-public nv-codec-headers
  (package
    (name "nv-codec-headers")
    (version "12.1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.videolan.org/git/ffmpeg/nv-codec-headers.git")
             (commit (string-append "n" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sp4giwbhai9blgd2k7sb571xwmz2yx17w32vy0nyj86ccb2x5jq"))))
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

(define-public obs-nvenc
  (let ((obs-ffmpeg-nvenc (replace-ffmpeg-nvenc obs)))
    (package/inherit obs-ffmpeg-nvenc
      (name "obs-nvenc")
      (description
       (string-append
        (package-description obs)
        "  This build of OBS includes the nonfree NVIDIA encoder for FFmpeg
@code{h264_nvenc} and @code{hevc_nvenc} hardware encoding on NVIDIA GPUs.")))))
