;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022, 2023 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2022-2026 Hilton Chain <hako@ultrarare.space>

(define-module (nongnu packages nvidia)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license-gnu:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (nongnu packages)
  #:use-module (nongnu packages game-client)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (replace-mesa))

(define %nvidia-environment-variable-regexps
  '("^__NV_"
    "^__GL_"                            ; NVIDIA OpenGL settings.
    "^__GLX_VENDOR_LIBRARY_NAME$"       ; For GLVND.
    ;; NVIDIA PRIME Render Offload.
    "^__VK_LAYER_NV_optimus$"
    ;; NVIDIA NGX.
    "^__NGX_CONF_FILE$"
    "^PROTON_ENABLE_NGX_UPDATER$"
    ;; NVIDIA Smooth Motion.
    "^NVPRESENT_"
    ;; NVIDIA VDPAU settings.
    "^VDPAU_NVIDIA_"
    ;; GSYNC control for Vulkan direct-to-display applications.
    "^VKDirectGSYNC(Compatible)?Allowed$"))

(define (add-architecture-to-filename)
  #~(lambda (path)
      (let* ((out #$output)
             (system #$(or (%current-target-system)
                           (%current-system)))
             (dash (string-index system #\-))
             (arch (string-take system dash))

             (dot  (string-index-right path #\.))
             (base (string-take path dot))
             (ext  (string-drop path (+ 1 dot))))
        ;; <...>/50_mesa.json -> <...>/50_mesa.x86_64.json
        (rename-file
         (string-append out path)
         (string-append out base "." arch "." ext)))))


;;;
;;; NVIDIA driver checkouts
;;;

(define* (make-nvidia-source version arch hash #:key (patches '()) snippet)
  (define installer
    (origin
      (method url-fetch)
      (uri (list (string-append
                  "https://international.download.nvidia.com/XFree86/"
                  arch "/" version "/NVIDIA-Linux-" arch "-" version ".run")
                 (string-append
                  "https://download.nvidia.com/XFree86/Linux-"
                  arch "/" version "/NVIDIA-Linux-" arch "-" version ".run")))
      (sha256 hash)))
  (package
    (inherit %binary-source)
    (version version)
    (source
     (origin
       (method (@@ (guix packages) computed-origin-method))
       (file-name (string-append "nvidia-driver-source-" version "-checkout"))
       (sha256 #f)
       (patches patches)
       (modules '((guix build utils)))
       (snippet snippet)
       (uri
        (delay
          (with-imported-modules '((guix build utils))
            #~(begin
                (use-modules (guix build utils))
                (set-path-environment-variable
                 "PATH" '("bin")
                 '#+(list bash-minimal
                          coreutils-minimal
                          gawk
                          grep
                          tar
                          which
                          xz
                          zstd))
                (invoke "sh" #+installer
                        "--extract-only" "--target" "extractdir")
                ;; We'll build open source kernel modules from git.
                (when (file-exists? "extractdir/kernel-open")
                  (delete-file-recursively "extractdir/kernel-open"))
                (for-each delete-file
                          (find-files "extractdir"
                                      (string-join
                                       '(;; egl-gbm
                                         "libnvidia-egl-gbm\\.so\\."
                                         ;; egl-wayland
                                         "libnvidia-egl-wayland\\.so\\."
                                         ;; egl-wayland2
                                         "libnvidia-egl-wayland2\\.so\\."
                                         ;; egl-x11
                                         "libnvidia-egl-xcb\\.so\\."
                                         "libnvidia-egl-xlib\\.so\\."
                                         ;; libglvnd
                                         "libEGL\\.so\\."
                                         "libGL\\.so\\."
                                         "libGLESv1_CM\\.so\\."
                                         "libGLESv2\\.so\\."
                                         "libGLX\\.so\\."
                                         "libGLdispatch\\.so\\."
                                         "libOpenGL\\.so\\."
                                         ;; nvidia-settings
                                         "libnvidia-gtk[23]\\.so\\."
                                         ;; opencl-icd-loader
                                         "libOpenCL\\.so\\.")
                                       "|")))
                (copy-recursively "extractdir" #$output)))))))))

(define %nvidia-patches-390
  (let ((commit "caed47174d2c835921d9f23ea08c630ef5cdea06"))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://aur.archlinux.org/nvidia-390xx-utils.git/")
             (commit commit)))
      (file-name (string-append "nvidia-patches." (string-take commit 7)))
      (sha256
       (base32 "0bsdb8lhycpiqf32lnxm7nnivh4hyfikwd2lii357pv12ix6z6v7")))))

(define %nvidia-patches-470
  (let ((commit "23ccd6e8d9b27256d4f491666b2779c663ab9f39"))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/joanbm/nvidia-470xx-linux-mainline")
             (commit commit)))
      (file-name (string-append "nvidia-patches." (string-take commit 7)))
      (sha256
       (base32 "09l7qmlgi27ydjzb6w6pksc36qs2c6g8aai5kf8y97bw5m83viww"))
      (modules '((guix build utils)))
      (snippet
       '(substitute* (find-files "." "\\.patch$")
          (("^--- a/" all)
           (string-append all "kernel/"))
          (("^\\+\\+\\+ b/" all)
           (string-append all "kernel/")))))))

(define nvidia-source-390-x86_64-linux
  (make-nvidia-source
   "390.157"
   "x86_64"
   (base32 "12ijkc5zvs3ivk5m69cm6k2ys60z6nggnw0hv2wxdmgyx2kbrssv")
   #:patches
   (map (cut file-append %nvidia-patches-390 "/" <>)
        '("kernel-4.16+-memory-encryption.patch"
          "kernel-6.2.patch"
          "kernel-6.3.patch"
          "kernel-6.4.patch"
          "kernel-6.5.patch"
          "kernel-6.6.patch"
          "kernel-6.8.patch"
          "gcc-14.patch"
          "kernel-6.10.patch"
          "kernel-6.12.patch"
          "kernel-6.13.patch"
          "kernel-6.14.patch"
          "gcc-15.patch"
          "kernel-6.15.patch"
          "kernel-6.17.patch"
          "kernel-6.19.patch"
          "kernel-6.18-nv_workqueue_flush.patch"))
   #:snippet
   #~(rename-file "nvidia_icd.json.template" "nvidia_icd.json")))

(define nvidia-source-470-x86_64-linux
  (make-nvidia-source
   "470.256.02"
   "x86_64"
   (base32 "1pmi949s0gzzjw2w3qhhihb82gppd1icvdzk8w2bp5dnvri1hifn")
   #:patches
   (map (cut file-append %nvidia-patches-470 "/patches/" <>)
        '("0001-Fix-conftest-to-ignore-implicit-function-declaration.patch"
          "0002-Fix-conftest-to-use-a-short-wchar_t.patch"
          "0003-Fix-conftest-to-use-nv_drm_gem_vmap-which-has-the-se.patch"
          "kernel-6.10.patch"
          "kernel-6.12.patch"
          "nvidia-470xx-fix-gcc-15.patch"
          "nvidia-470xx-fix-linux-6.13.patch"
          "nvidia-470xx-fix-linux-6.14.patch"
          "nvidia-470xx-fix-linux-6.15.patch"
          "nvidia-470xx-fix-linux-6.17.patch"
          "nvidia-470xx-fix-linux-6.19-part1.patch"
          "nvidia-470xx-fix-linux-6.19-part2.patch"
          "nvidia-470xx-fix-linux-7.0.patch"
          "disable-objtool-override.patch"
          "enable-drm-modeset-by-default.patch"))))

;; FIXME: The kernel module doesn't build on aarch64-linux currently.
(define nvidia-source-470-aarch64-linux
  (make-nvidia-source
   "470.256.02"
   "aarch64"
   (base32 "138dg91zq1a8syrp8rax0braw82aacn6ggd08v4zs5mpwh9jzr3v")))

(define nvidia-source-580-x86_64-linux
  (make-nvidia-source
   "580.142"
   "x86_64"
   (base32 "0qvm8hh3d90i3674dqlj1lam6m189ah60fzr1iaw72gy7z7mz490")))

(define nvidia-source-580-aarch64-linux
  (make-nvidia-source
   "580.142"
   "aarch64"
   (base32 "0cqi2wgvyxid0dwav8c1awmgq7wcs0naxxf3wdx88kd9qkrnnywf")))

(define nvidia-source-590-x86_64-linux
  (make-nvidia-source
   "590.48.01"
   "x86_64"
   (base32 "12fnddljvgxksil6n3d5a35wwg8kkq82kkglhz63253qjc3giqmr")))

(define nvidia-source-590-aarch64-linux
  (make-nvidia-source
   "590.48.01"
   "aarch64"
   (base32 "107xpshd3rn6sdcrprd32a7n5crdzarr3y7yv66d3m2nm9zzpv0l")))

(define nvidia-source-595-x86_64-linux
  (make-nvidia-source
   "595.58.03"
   "x86_64"
   (base32 "1y99b0h3cv8panjsz4icf052nf83h7p2l9qlaymw8ckrgfb4y3cc")))

(define nvidia-source-595-aarch64-linux
  (make-nvidia-source
   "595.58.03"
   "aarch64"
   (base32 "0grx380xy9d7idd6i02lmlf7yl6lcizgi48m10jc8yskillwhg47")))

(define nvidia-source-beta-x86_64-linux
  (make-nvidia-source
   "595.45.04"
   "x86_64"
   (base32 "0plg9vsim8252c7k3slxblvrspy4xqa6q719flxjmfkc4i4najfd")))

(define nvidia-source-beta-aarch64-linux
  (make-nvidia-source
   "595.45.04"
   "aarch64"
   (base32 "1jff1jahw3bj3bdnr793xjpps4hix61qa03bvfdaq5r0dd0saplf")))


;;;
;;; NVIDIA driver dependencies.
;;;

;; Define dependencies before NVIDIA driver definitions, since
;; binary-package-from-sources accesses non-delayed package fields.

(define-public egl-gbm
  (package
    (name "egl-gbm")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/egl-gbm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p9w7xc7zdrwxxiwmmhmsqf0jlzcgnrkgci2j5da4xzjasyf109s"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-library-reference
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((dir "share/egl/egl_external_platform.d"))
                (with-directory-excursion (in-vicinity #$output dir)
                  (substitute* "15_nvidia_gbm.json"
                    (("libnvidia-egl-.*\\.so\\.." lib)
                     (search-input-file
                      outputs (in-vicinity "lib" lib)))))))))))
    (native-inputs (list pkg-config))
    (inputs (list eglexternalplatform mesa-for-nvda))
    (synopsis "GBM EGL external platform library")
    (description
     "This package provides an EGL External Platform library implementation for
GBM EGL support.")
    (home-page "https://github.com/NVIDIA/egl-gbm")
    (license license-gnu:expat)))

(define-public egl-wayland2
  (package
    (inherit egl-wayland)
    (name "egl-wayland2")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/egl-wayland2")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15n6jf8kxkha0bxhjj9x720i88nqar8k6wkirav2izbi52vgxnji"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-library-reference
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((dir "share/egl/egl_external_platform.d"))
                (with-directory-excursion (in-vicinity #$output dir)
                  (substitute* "09_nvidia_wayland2.json"
                    (("libnvidia-egl-.*\\.so\\.." lib)
                     (search-input-file
                      outputs (in-vicinity "lib" lib)))))))))))
    (synopsis "Dma-buf-based Wayland external platform library")
    (description
     "This is a new implementation of the EGL External Platform Library for
Wayland (@code{EGL_KHR_platform_wayland}), using the NVIDIA driver's new
platform surface interface, which simplifies a lot of the library and improves
window resizing.")
    (home-page "https://github.com/NVIDIA/egl-wayland2")
    (license license-gnu:asl2.0)))

(define-public egl-x11
  (package
    (name "egl-x11")
    (version "1.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/egl-x11")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07d72z4dm2w9ys01li2v770j51zciahn0m5yn4bxrns7gxrylpsa"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-library-reference
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((dir "share/egl/egl_external_platform.d"))
                (with-directory-excursion (in-vicinity #$output dir)
                  (substitute* '("20_nvidia_xcb.json"
                                 "20_nvidia_xlib.json")
                    (("libnvidia-egl-.*\\.so\\.." lib)
                     (search-input-file
                      outputs (in-vicinity "lib" lib)))))))))))
    (native-inputs (list pkg-config))
    (inputs (list eglexternalplatform mesa-for-nvda))
    (synopsis "X11 and XCB EGL external platform library")
    (description
     "This package provides an EGL platform library for the NVIDIA driver to
support XWayland via xlib (using @code{EGL_KHR_platform_x11}) or xcb (using
@code{EGL_EXT_platform_xcb}).")
    (home-page "https://github.com/NVIDIA/egl-x11")
    (license license-gnu:expat)))


;;;
;;; NVIDIA drivers
;;;

;; Avoid inheriting unpacking phases from binary-package-from-sources.
(define (%nvidia-driver-arguments-390)
  (list
   #:imported-modules
   `((guix build copy-build-system)
     ,@%default-gnu-imported-modules)
   #:modules
   `((ice-9 popen)
     (ice-9 rdelim)
     (ice-9 regex)
     (srfi srfi-26)
     ((guix build copy-build-system) #:prefix copy:)
     ,@%default-gnu-modules)
   #:phases
   #~(modify-phases %standard-phases
       (delete 'configure)
       (delete 'build)
       (delete 'check)
       (delete 'strip)
       (add-after 'unpack 'create-files
         (lambda* (#:key inputs #:allow-other-keys)
           ;; EGL external platform configuraiton
           (substitute* "10_nvidia_wayland.json"
             (("libnvidia-egl-.*\\.so\\.." all)
              (search-input-file inputs (in-vicinity "lib" all))))
           ;; EGL vendor ICD configuration
           (substitute* "10_nvidia.json"
             (("libEGL_nvidia\\.so\\." all)
              (string-append #$output "/lib/" all)))
           ;; OpenCL vendor ICD configuration
           (substitute* "nvidia.icd"
             (("libnvidia-opencl\\.so\\." all)
              (string-append #$output "/lib/" all)))
           ;; Vulkan ICD configuraiton
           (substitute* "nvidia_icd.json"
             (("__NV_VK_ICD__")
              (in-vicinity #$output "lib/libGLX_nvidia.so.0")))))
       (replace 'install
         (lambda args
           (apply (assoc-ref copy:%standard-phases 'install)
                  #:install-plan
                  `((#$(if (target-x86-32?) "32" ".") "lib/" #:include-regexp ("^./[^/]+\\.so"))
                    ("." "share/nvidia/" #:include-regexp ("nvidia-application-profiles-.*"))
                    ("." "bin/"
                     #:include
                     ,(if #$(target-64bit?)
                          '("nvidia-cuda-mps-control"
                            "nvidia-cuda-mps-server"
                            "nvidia-smi")
                          '()))
                    ("." "share/man/man1/"
                     #:include
                     ,(if #$(target-64bit?)
                          '("nvidia-cuda-mps-control.1.gz"
                            "nvidia-smi.1.gz")
                          '()))
                    ("10_nvidia_wayland.json" "share/egl/egl_external_platform.d/")
                    ("10_nvidia.json" "share/glvnd/egl_vendor.d/")
                    ("nvidia-drm-outputclass.conf" "share/X11/xorg.conf.d/")
                    ("nvidia.icd" "etc/OpenCL/vendors/")
                    ("nvidia_icd.json" "share/vulkan/icd.d/"))
                  args)))
       (add-after 'install 'add-architecture
         (lambda _
           (for-each
            #$(add-architecture-to-filename)
            '("/etc/OpenCL/vendors/nvidia.icd"
              "/share/egl/egl_external_platform.d/10_nvidia_wayland.json"
              "/share/glvnd/egl_vendor.d/10_nvidia.json"
              "/share/vulkan/icd.d/nvidia_icd.json"))))
       (add-after 'install 'patch-elf
         (lambda* (#:key inputs #:allow-other-keys)
           (let* ((ld.so (search-input-file
                          inputs #$(glibc-dynamic-linker)))
                  (rpath (string-join
                          (cons* (dirname ld.so)
                                 (string-append #$output "/lib")
                                 (map (lambda (name)
                                        (dirname
                                         (search-input-file
                                          inputs (in-vicinity "lib" name))))
                                      '("libGL.so.1"
                                        "libX11.so.6"
                                        "libXext.so.6"
                                        "libcrypto.so.1.1"
                                        "libcrypto.so.3"
                                        "libdbus-1.so.3"
                                        "libdrm.so.2"
                                        "libgbm.so.1"
                                        "libgcc_s.so.1"
                                        "libnvidia-egl-wayland.so.1"
                                        "libwayland-client.so.0"
                                        "libxcb.so.1")))
                          ":"))
                  (patch-elf
                   (lambda (file)
                     (when (elf-file? file)
                       (format #t "Patching ~a ..." file)
                       (unless (string-contains file ".so")
                         (invoke "patchelf" "--set-interpreter" ld.so file))
                       (invoke "patchelf" "--set-rpath" rpath file)
                       (display " done\n")))))
             (for-each patch-elf (find-files #$output)))))
       (add-before 'patch-elf 'relocate-libraries
         (lambda _
           (let* ((libdir     (in-vicinity #$output "lib"))
                  (vdpaudir   (in-vicinity libdir "vdpau"))
                  (xorgmoddir (in-vicinity libdir "xorg/modules"))
                  (xorgdrvdir (in-vicinity xorgmoddir "drivers"))
                  (xorgextdir (in-vicinity xorgmoddir "extensions"))
                  (move-to-dir
                   (lambda (file dir)
                     (install-file file dir)
                     (delete-file file))))
             ;; Driver for X
             (for-each
              (cut move-to-dir <> xorgdrvdir)
              (find-files libdir "nvidia_drv\\.so$"))
             ;; GLX extension module for X
             (for-each
              (cut move-to-dir <> xorgextdir)
              (find-files libdir "libglx\\.so\\."))
             ;; X module for wrapped software rendering
             (for-each
              (cut move-to-dir <> xorgmoddir)
              (find-files libdir "libnvidia-wfb\\.so\\."))
             ;; VDPAU library
             (for-each
              (cut move-to-dir <> vdpaudir)
              (find-files libdir "libvdpau_nvidia\\.so\\.")))))
       (add-after 'patch-elf 'create-short-name-symlinks
         (lambda _
           (define (get-soname file)
             (when (elf-file? file)
               (let* ((cmd (string-append "patchelf --print-soname " file))
                      (port (open-input-pipe cmd))
                      (soname (read-line port)))
                 (close-pipe port)
                 soname)))
           (for-each
            (lambda (lib)
              (let ((lib-soname (get-soname lib)))
                (when (string? lib-soname)
                  (let* ((soname (in-vicinity (dirname lib) lib-soname))
                         (base (string-append
                                (regexp-substitute
                                 #f (string-match "(.*)\\.so.*" soname) 1)
                                ".so"))
                         (source (basename lib)))
                    (for-each
                     (lambda (target)
                       (unless (file-exists? target)
                         (format #t "Symlinking ~a -> ~a..." target source)
                         (symlink source target)
                         (display " done\n")))
                     (list soname base))))))
            (find-files #$output "\\.so\\.")))))))

(define-public nvidia-driver-390
  (package
    (name "nvidia-driver")
    (version "390.157")
    (source (package-source nvidia-source-390-x86_64-linux))
    (build-system gnu-build-system)
    (arguments (%nvidia-driver-arguments-390))
    (supported-systems '("x86_64-linux" "i686-linux"))
    (native-inputs (list patchelf-0.16))
    (inputs
     (list dbus
           egl-wayland
           `(,gcc "lib")
           glibc
           mesa-for-nvda
           openssl
           openssl-1.1
           wayland))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver (libraries), legacy 390.xx series")
    (description
     "This package provides libraries of the proprietary NVIDIA driver.  It's
mainly used as a dependency of other packages.  For user-facing purpose, use
@code{nvda} instead.")
    (license
     (license:nonfree
      (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define (%nvidia-driver-arguments-470)
  (substitute-keyword-arguments (%nvidia-driver-arguments-390)
    ((#:phases phases)
     #~(modify-phases #$phases
         (add-after 'create-files 'create-files-470
           (lambda _
             ;; Vulkan ICD configuraiton
             (substitute* "nvidia_icd.json"
               (("libGLX_nvidia\\.so\\." all)
                (string-append #$output "/lib/" all)))
             ;; Vulkan layer configuraiton
             (substitute* "nvidia_layers.json"
               (("libGLX_nvidia\\.so\\." all)
                (string-append #$output "/lib/" all)))))
         (add-after 'install 'install-470
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("." "lib/nvidia/wine/" #:include-regexp ("_?nvngx.*?\\.dll$"))
                      ("nvidia_layers.json" "share/vulkan/implicit_layer.d/"))
                    args)))
         (add-after 'add-architecture 'add-architecture-470
           (lambda _
             (for-each
              #$(add-architecture-to-filename)
              '("/share/vulkan/implicit_layer.d/nvidia_layers.json"))))
         (add-after 'relocate-libraries 'relocate-libraries-470
           (lambda _
             (let* ((libdir     (in-vicinity #$output "lib"))
                    (xorgextdir (in-vicinity libdir "xorg/modules/extensions"))
                    (move-to-dir
                     (lambda (file dir)
                       (install-file file dir)
                       (delete-file file))))
               ;; GLX extension module for X
               (for-each
                (lambda (file)
                  (move-to-dir file xorgextdir)
                  (with-directory-excursion xorgextdir
                    (symlink (basename file) "libglxserver_nvidia.so")))
                (find-files libdir "libglxserver_nvidia\\.so\\.")))))))))

(define-public nvidia-driver-470
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-470-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-470-aarch64-linux))
   (package
     (inherit nvidia-driver-390)
     (arguments (%nvidia-driver-arguments-470))
     (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"))
     (synopsis "Proprietary NVIDIA driver (libraries), legacy 470.xx series"))))

(define (%nvidia-driver-arguments-580)
  (substitute-keyword-arguments (%nvidia-driver-arguments-470)
    ((#:phases phases)
     #~(modify-phases #$phases
         (add-after 'create-files 'create-files-580
           (lambda* (#:key inputs #:allow-other-keys)
             ;; EGL external platform configuraiton
             (substitute* '("15_nvidia_gbm.json"
                            "20_nvidia_xcb.json"
                            "20_nvidia_xlib.json")
               (("libnvidia-egl-.*\\.so\\.." all)
                (search-input-file inputs (in-vicinity "lib" all))))
             ;; Vulkan layer configuraiton
             (substitute* "nvidia_layers.json"
               (("libnvidia-present\\.so\\." all)
                (string-append #$output "/lib/" all)))
             ;; VulkanSC ICD configuration
             (substitute* (find-files "." "nvidia_icd_vksc\\.json$")
               (("libnvidia-vksc-core\\.so\\." all)
                (string-append #$output "/lib/" all)))))
         (add-after 'install 'install-580
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    `(("." "bin/"
                       #:include
                       ,(append (if #$(target-x86-64?) '("nvidia-pcc")    '())
                                (if #$(target-64bit?)  '("nvidia-powerd") '())))
                      ("." "etc/vulkansc/icd.d/" #:include-regexp ("nvidia_icd_vksc\\.json$"))
                      ("." "share/egl/egl_external_platform.d/"
                       #:include ("15_nvidia_gbm.json"
                                  "20_nvidia_xcb.json"
                                  "20_nvidia_xlib.json"))
                      ("nvidia-dbus.conf" "etc/dbus-1/system.d/")
                      ("nvoptix.bin" "share/nvidia/")
                      ("sandboxutils-filelist.json" "share/nvidia/files.d/"))
                    args)))
         (add-after 'add-architecture 'add-architecture-580
           (lambda _
             (for-each
              #$(add-architecture-to-filename)
              '("/share/egl/egl_external_platform.d/15_nvidia_gbm.json"
                "/share/egl/egl_external_platform.d/20_nvidia_xcb.json"
                "/share/egl/egl_external_platform.d/20_nvidia_xlib.json"))))
         (add-after 'relocate-libraries 'relocate-libraries-580
           (lambda _
             (let* ((libdir (in-vicinity #$output "lib"))
                    (gbmdir (in-vicinity libdir "gbm")))
               (for-each
                (lambda (file)
                  (mkdir-p gbmdir)
                  (with-directory-excursion gbmdir
                    (symlink (in-vicinity ".." (basename file))
                             "nvidia-drm_gbm.so")))
                (find-files libdir "libnvidia-allocator\\.so\\.")))))
         (add-after 'patch-elf 'wrap-program-580
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((nvidia-powerd (string-append #$output "/bin/nvidia-powerd")))
               (when (file-exists? nvidia-powerd)
                 (wrap-program nvidia-powerd
                   `("PATH" = (,(dirname (search-input-file inputs "bin/lscpu")))))))))))))

(define-public nvidia-driver-580
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-580-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-580-aarch64-linux))
   (package
     (inherit nvidia-driver-470)
     (arguments (%nvidia-driver-arguments-580))
     (inputs
      (modify-inputs inputs
        (prepend bash-minimal egl-gbm egl-x11 util-linux)))
     (synopsis "Proprietary NVIDIA driver (libraries), production branch"))))

(define (%nvidia-driver-arguments-590)
  (substitute-keyword-arguments (%nvidia-driver-arguments-580)
    ((#:phases phases)
     #~(modify-phases #$phases
         (add-after 'create-files 'create-files-590
           (lambda* (#:key inputs #:allow-other-keys)
             ;; EGL external platform configuraiton
             (substitute* "99_nvidia_wayland2.json"
               (("libnvidia-egl-.*\\.so\\.." all)
                (search-input-file inputs (in-vicinity "lib" all))))))
         (add-after 'install 'install-590
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("99_nvidia_wayland2.json" "share/egl/egl_external_platform.d/"))
                    args)))
         (add-after 'add-architecture 'add-architecture-590
           (lambda _
             (for-each
              #$(add-architecture-to-filename)
              '("/share/egl/egl_external_platform.d/99_nvidia_wayland2.json"))))))))

(define-public nvidia-driver-590
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-590-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-590-aarch64-linux))
   (package
     (inherit nvidia-driver-580)
     (arguments (%nvidia-driver-arguments-590))
     (inputs
      (modify-inputs inputs
        (prepend egl-wayland2)))
     (synopsis "Proprietary NVIDIA driver (libraries), new feature branch"))))

(define (%nvidia-driver-arguments-595)
  (substitute-keyword-arguments (%nvidia-driver-arguments-580)
    ((#:phases phases)
     #~(modify-phases #$phases
         (add-after 'create-files 'create-files-595
           (lambda* (#:key inputs #:allow-other-keys)
             ;; EGL external platform configuraiton
             (substitute* "09_nvidia_wayland2.json"
               (("libnvidia-egl-.*\\.so\\.." all)
                (search-input-file inputs (in-vicinity "lib" all))))))
         (add-after 'install 'install-595
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("09_nvidia_wayland2.json" "share/egl/egl_external_platform.d/"))
                    args)))
         (add-after 'add-architecture 'add-architecture-595
           (lambda _
             (for-each
              #$(add-architecture-to-filename)
              '("/share/egl/egl_external_platform.d/09_nvidia_wayland2.json"))))))))

(define-public nvidia-driver-595
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-595-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-595-aarch64-linux))
   (package
     (inherit nvidia-driver-580)
     (arguments (%nvidia-driver-arguments-595))
     (inputs
      (modify-inputs inputs
        (prepend egl-wayland2)))
     (synopsis "Proprietary NVIDIA driver (libraries), production branch"))))

(define (%nvidia-driver-arguments-beta)
  (substitute-keyword-arguments (%nvidia-driver-arguments-580)
    ((#:phases phases)
     #~(modify-phases #$phases
         (add-after 'create-files 'create-files-beta
           (lambda* (#:key inputs #:allow-other-keys)
             ;; EGL external platform configuraiton
             (substitute* "09_nvidia_wayland2.json"
               (("libnvidia-egl-.*\\.so\\.." all)
                (search-input-file inputs (in-vicinity "lib" all))))))
         (add-after 'install 'install-beta
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("09_nvidia_wayland2.json" "share/egl/egl_external_platform.d/"))
                    args)))
         (add-after 'add-architecture 'add-architecture-beta
           (lambda _
             (for-each
              #$(add-architecture-to-filename)
              '("/share/egl/egl_external_platform.d/09_nvidia_wayland2.json"))))))))

(define-public nvidia-driver-beta
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-beta-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-beta-aarch64-linux))
   (package
     (inherit nvidia-driver-580)
     (name "nvidia-driver-beta")
     (arguments (%nvidia-driver-arguments-beta))
     (inputs
      (modify-inputs inputs
        (prepend egl-wayland2)))
     (synopsis "Proprietary NVIDIA driver (libraries), beta"))))

(define-public nvidia-driver nvidia-driver-580)


;;;
;;; NVIDIA firmware
;;;

(define (%nvidia-firmware-arguments version)
  (list #:install-plan
        #~'(("firmware" #$(in-vicinity "lib/firmware/nvidia" version)))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'strip))))

(define-public nvidia-firmware-470
  (package
    (inherit nvidia-driver-470)
    (name "nvidia-firmware")
    (source (package-source nvidia-source-470-x86_64-linux))
    (build-system copy-build-system)
    (arguments (%nvidia-firmware-arguments (package-version this-package)))
    (propagated-inputs '())
    (inputs '())
    (native-inputs '())
    (supported-systems '("x86_64-linux"))
    (synopsis "Proprietary NVIDIA driver (GPU System Processor firmware), legacy 470.xx series")
    (description
     "This package provides @acronym{GSP, GPU System Processor} firmware of
the proprietary NVIDIA driver.

For free driver (@code{nouveau}) support, use @code{linux-firmware}
instead.")))

(define-public nvidia-firmware-580
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-580-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-580-aarch64-linux))
   (package
     (inherit nvidia-firmware-470)
     (arguments (%nvidia-firmware-arguments (package-version this-package)))
     (supported-systems '("x86_64-linux" "aarch64-linux"))
     (synopsis "Proprietary NVIDIA driver (GPU System Processor firmware), production branch"))))

(define-public nvidia-firmware-590
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-590-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-590-aarch64-linux))
   (package
     (inherit nvidia-firmware-580)
     (arguments (%nvidia-firmware-arguments (package-version this-package)))
     (synopsis "Proprietary NVIDIA driver (GPU System Processor firmware), new feature branch"))))

(define-public nvidia-firmware-595
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-595-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-595-aarch64-linux))
   (package
     (inherit nvidia-firmware-580)
     (arguments (%nvidia-firmware-arguments (package-version this-package)))
     (synopsis "Proprietary NVIDIA driver (GPU System Processor firmware), production branch"))))

(define-public nvidia-firmware-beta
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-beta-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-beta-aarch64-linux))
   (package
     (inherit nvidia-firmware-580)
     (name "nvidia-firmware-beta")
     (arguments (%nvidia-firmware-arguments (package-version this-package)))
     (synopsis "Proprietary NVIDIA driver (GPU System Processor firmware), beta"))))

(define-public nvidia-firmware nvidia-firmware-580)


;;;
;;; NVIDIA kernel modules
;;;

(define (%nvidia-module-arguments)
  (list #:source-directory "kernel"
        #:tests? #f
        #:make-flags
        #~(list (string-append "CC=" #$(cc-for-target)))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'fixpath
              (lambda* (#:key (source-directory ".") #:allow-other-keys)
                (substitute* (string-append source-directory "/Kbuild")
                  (("/bin/sh") (which "sh")))))
            (replace 'build
              (lambda* (#:key (make-flags '()) (parallel-build? #t)
                        (source-directory ".")
                        inputs
                        #:allow-other-keys)
                (apply invoke "make" "-C" (canonicalize-path source-directory)
                       (string-append "SYSSRC=" (search-input-directory
                                                 inputs "/lib/modules/build"))
                       `(,@(if parallel-build?
                               `("-j" ,(number->string
                                        (parallel-job-count)))
                               '())
                         ,@make-flags)))))))

(define-public nvidia-module-390
  (package
    (inherit nvidia-driver-390)
    (source (package-source nvidia-source-390-x86_64-linux))
    (name "nvidia-module")
    (build-system linux-module-build-system)
    (arguments (%nvidia-module-arguments))
    (propagated-inputs '())
    (inputs '())
    (native-inputs '())
    (supported-systems '("x86_64-linux"))
    (synopsis "Proprietary NVIDIA driver (kernel modules), legacy 390.xx series")
    (description
     "This package provides proprietary kernel modules of the proprietary NVIDIA
driver.")))

(define-public nvidia-module-470
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-470-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-470-aarch64-linux))
   (package
     (inherit nvidia-module-390)
     (arguments (%nvidia-module-arguments))
     (supported-systems '("x86_64-linux" "aarch64-linux"))
     (synopsis "Proprietary NVIDIA driver (kernel modules), legacy 470.xx series"))))

(define-public nvidia-module-580
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-580-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-580-aarch64-linux))
   (package
     (inherit nvidia-module-470)
     (arguments (%nvidia-module-arguments))
     (synopsis "Proprietary NVIDIA driver (kernel modules), production branch"))))

(define-public nvidia-module-590
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-590-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-590-aarch64-linux))
   (package
     (inherit nvidia-module-580)
     (arguments (%nvidia-module-arguments))
     (synopsis "Proprietary NVIDIA driver (kernel modules), new feature branch"))))

(define-public nvidia-module-595
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-595-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-595-aarch64-linux))
   (package
     (inherit nvidia-module-580)
     (arguments (%nvidia-module-arguments))
     (synopsis "Proprietary NVIDIA driver (kernel modules), production branch"))))

(define-public nvidia-module-beta
  (binary-package-from-sources
   `(("x86_64-linux"  . ,nvidia-source-beta-x86_64-linux)
     ("aarch64-linux" . ,nvidia-source-beta-aarch64-linux))
   (package
     (inherit nvidia-module-580)
     (name "nvidia-module-beta")
     (arguments (%nvidia-module-arguments))
     (synopsis "Proprietary NVIDIA driver (kernel modules), beta"))))

(define-public nvidia-module nvidia-module-580)


;;;
;;; NVIDIA open source kernel modules.
;;;

(define-public nvidia-module-open-580
  (package
    (name "nvidia-module-open")
    (version "580.142")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/open-gpu-kernel-modules")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01nq1hmb0kcd7wx38z5a1ivc6r1z3vbwp1zcyz0wijvanhnvrpmz"))
       (patches
        (nongnu-patches "nvidia-module-open-add-ibt-support.patch"))))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:source-directory "kernel-open"
      #:tests? #f
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-reference
            (lambda* (#:key source-directory #:allow-other-keys)
              (substitute* (string-append source-directory "/Kbuild")
                (("/bin/sh") (which "sh")))))
          (replace 'build
            (lambda* (#:key inputs make-flags (parallel-build? #t)
                      #:allow-other-keys)
              (apply invoke "make"
                     (string-append
                      "SYSSRC="
                      (search-input-directory inputs "/lib/modules/build"))
                     `(,@(if parallel-build?
                             `("-j" ,(number->string (parallel-job-count)))
                             '())
                       ,@make-flags
                       "modules")))))))
    (home-page "https://github.com/NVIDIA/open-gpu-kernel-modules")
    (synopsis "Proprietary NVIDIA driver (open source kernel modules), production branch")
    (description
     "This package provides open source kernel modules of the proprietary
NVIDIA driver.")
    (license license-gnu:gpl2)))

(define-public nvidia-module-open-590
  (package
    (inherit nvidia-module-open-580)
    (name "nvidia-module-open")
    (version "590.48.01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/open-gpu-kernel-modules")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13izbl0npxc6mxaq7123sj7cqksqwcha8fgsgj2dphdk1dz8fh44"))
       (patches
        (nongnu-patches "nvidia-module-open-add-ibt-support.patch"))))
    (synopsis "Proprietary NVIDIA driver (open source kernel modules), new feature branch")))

(define-public nvidia-module-open-595
  (package
    (inherit nvidia-module-open-580)
    (name "nvidia-module-open")
    (version "595.58.03")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/open-gpu-kernel-modules")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zz2297icklbjk4301vahsfkxdznbp48f0yxvf972c8w7p4wkfz8"))
       (patches
        (nongnu-patches "nvidia-module-open-add-ibt-support.patch"))))
    (synopsis "Proprietary NVIDIA driver (open source kernel modules), production branch")))

(define-public nvidia-module-open-beta
  (package
    (inherit nvidia-module-open-580)
    (name "nvidia-module-open-beta")
    (version "595.45.04")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/open-gpu-kernel-modules")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "108faqi446ck42gc9q10dbl0779yagyp853phay14ahkdhi5z8xs"))
       (patches
        (nongnu-patches "nvidia-module-open-add-ibt-support.patch"))))
    (synopsis "Proprietary NVIDIA driver (open source kernel modules), beta")))

(define-public nvidia-module-open nvidia-module-open-580)


;;;
;;; nvidia-modprobe
;;;

(define-public nvidia-modprobe-390
  (package
    (name "nvidia-modprobe")
    (version "390.157")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-modprobe")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mny1vv81f00w71cp8ffnyx0sv20p339dravrs3gxwawac5m64a7"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;No test suite
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "STRIP_CMD=" #$(strip-for-target))
                   "HOST_CC=gcc")
           #:phases
           #~(modify-phases %standard-phases
               ;; No configure script.
               (delete 'configure))))
    (native-inputs (list m4 pkg-config))
    (home-page "https://github.com/NVIDIA/nvidia-modprobe")
    (synopsis "Create NVIDIA character device files, legacy 390.xx series")
    (description
     "The @command{nvidia-modprobe} utility is used by user-space NVIDIA driver
components to make sure the NVIDIA kernel module is loaded, the NVIDIA character
device files are present and configure certain runtime settings in the kernel.")
    (license license-gnu:gpl2)))

(define-public nvidia-modprobe-470
  (package
    (inherit nvidia-modprobe-390)
    (name "nvidia-modprobe")
    (version "470.256.02")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-modprobe")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lybamqj6c9c79vv8gz1j3ppr95wqph8r8fs06qwcvz386gwqrn7"))))
    (synopsis "Create NVIDIA character device files, legacy 470.xx series")))

(define-public nvidia-modprobe-580
  (package
    (inherit nvidia-modprobe-470)
    (name "nvidia-modprobe")
    (version "580.142")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-modprobe")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rbl52d40q86y9dbj5qlm5k3rindg5fqh121wxfzrc68fl3gjila"))))
    (synopsis "Create NVIDIA character device files, production branch")))

(define-public nvidia-modprobe-590
  (package
    (inherit nvidia-modprobe-580)
    (name "nvidia-modprobe")
    (version "590.48.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-modprobe")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y6kqhvjfpq0zssjsbkwkav2khsb7x63nxgd1lnvrkg660a7knjn"))))
    (synopsis "Create NVIDIA character device files, new feature branch")))

(define-public nvidia-modprobe-595
  (package
    (inherit nvidia-modprobe-580)
    (name "nvidia-modprobe")
    (version "595.58.03")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-modprobe")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l1kjp5kgy4f319205k3l2i3nvvi088baxsz8n3jb799pqjwd2f4"))))
    (synopsis "Create NVIDIA character device files, production branch")))

(define-public nvidia-modprobe-beta
  (package
    (inherit nvidia-modprobe-580)
    (name "nvidia-modprobe-beta")
    (version "595.45.04")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-modprobe")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s1p89js7f65pzss7fqglddz052lq357xjxyqwpca9kmljxr4dqc"))))
    (synopsis "Create NVIDIA character device files, beta")))

(define-public nvidia-modprobe nvidia-modprobe-580)


;;;
;;; ‘nvidia-settings’ packages
;;;

(define %nvidia-settings-patches-390
  (let ((commit "6104269b087751509b904d9282be28440e514c9e"))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://aur.archlinux.org/nvidia-390xx-settings.git")
             (commit commit)))
      (file-name
       (string-append "nvidia-settings-patches." (string-take commit 7)))
      (sha256
       (base32 "0y8zalpymrzxlmh25bqh4x29a4qix3a50qvvykg4hv07mmn0gckx")))))

(define-public nvidia-settings-390
  (package
    (name "nvidia-settings")
    (version "390.157")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/nvidia-settings")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "170nx61spd6psly55ghyp46139c9a9r7al0g9nggrhrzm7hlx5mq"))
       (patches
        (map (lambda (name)
               (file-append %nvidia-settings-patches-390 "/" name))
             '("0001-nvidia-settings-Make-VDPAUDeviceFunctions-static-to-.patch")))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/jansson"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no test suite
      #:make-flags
      #~(list "NV_USE_BUNDLED_LIBJANSSON=0"
              (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'fix-application-profile-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/gtk+-2.x/ctkappprofile.c"
                (("/usr") "/run/booted-system/profile"))))
          (add-after 'install 'install-desktop-file
            (lambda _
              (substitute* "doc/nvidia-settings.desktop"
                (("^Exec=.*") "Exec=nvidia-settings\n")
                (("__NVIDIA_SETTINGS_DESKTOP_CATEGORIES__") "Settings")
                (("__PIXMAP_PATH__")
                 (in-vicinity #$output "share/icons/hicolor/128x128/apps")))
              (install-file
               "doc/nvidia-settings.desktop"
               (in-vicinity #$output "share/applications"))
              (install-file
               "doc/nvidia-settings.png"
               (in-vicinity #$output "share/icons/hicolor/128x128/apps"))))
          (add-after 'install 'wrap-program
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-program (string-append out "/bin/nvidia-settings")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append out "/lib/"))))))))))
    (native-inputs (list m4
                         pkg-config))
    (inputs (list bash-minimal
                  dbus
                  glu
                  gtk+
                  gtk+-2
                  jansson
                  libvdpau
                  libx11
                  libxext
                  libxrandr
                  libxv
                  libxxf86vm
                  vulkan-headers))
    (synopsis "NVIDIA proprietary driver control panel, legacy 390.xx series")
    (description
     "This package provides NVIDIA driver control panel for monitor
configuration, application profiles, GPU monitoring and more.")
    (home-page "https://github.com/NVIDIA/nvidia-settings")
    (license license-gnu:gpl2)))

(define-public nvidia-settings-470
  (package
    (inherit nvidia-settings-390)
    (name "nvidia-settings")
    (version "470.256.02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/nvidia-settings")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "1sc2h3gglqvhc5m0nz97gp70nz1jjkzppndzy922k2blj3h51ywi"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/jansson"))))
    (synopsis "NVIDIA proprietary driver control panel, legacy 470.xx series")))

(define-public nvidia-settings-580
  (package
    (inherit nvidia-settings-470)
    (name "nvidia-settings")
    (version "580.142")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/nvidia-settings")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "00sdrka3mslqgyhpnxyr6165nbrrfqdp1shgmbgp9ga07sbchyh6"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/jansson"))))
    (synopsis "NVIDIA proprietary driver control panel, production branch")))

(define-public nvidia-settings-590
  (package
    (inherit nvidia-settings-580)
    (name "nvidia-settings")
    (version "590.48.01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/nvidia-settings")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "0h9059gkibyiidg5s9cakbg369y9nwfd17vycpsqfswgr18jlsrm"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/jansson"))))
    (synopsis "NVIDIA proprietary driver control panel, new feature branch")))

(define-public nvidia-settings-595
  (package
    (inherit nvidia-settings-580)
    (name "nvidia-settings")
    (version "595.58.03")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/nvidia-settings")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "0xqg9rrrynlw8qzhs9ggm1d6jyv36bjx4s028nnkxn759gjcbwns"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/jansson"))))
    (synopsis "NVIDIA proprietary driver control panel, production branch")))

(define-public nvidia-settings-beta
  (package
    (inherit nvidia-settings-580)
    (name "nvidia-settings-beta")
    (version "595.45.04")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NVIDIA/nvidia-settings")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "0w7ndc2p2131h1wh3rj1dhhs59ihrdfl8ni44x9sdywc5jpnk3k3"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/jansson"))))
    (synopsis "NVIDIA proprietary driver control panel, beta")))

(define-public nvidia-settings nvidia-settings-580)


;;;
;;; ‘nvda’ packages
;;;

(define (replace-nvidia-driver driver)
  (package-input-rewriting `((,nvidia-driver . ,driver))))

(define-public libglvnd-for-nvda
  (hidden-package
   (package
     (inherit libglvnd)
     (arguments
      (substitute-keyword-arguments arguments
        ((#:configure-flags flags #~'())
         #~(cons* "-Dc_link_args=-Wl,-rpath=$ORIGIN" #$flags))
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (delete 'shrink-runpath))))))))

(define-public mesa-for-nvda
  (hidden-package
   (package
     (inherit mesa)
     (propagated-inputs
      (modify-inputs propagated-inputs
        (prepend libglvnd-for-nvda)))
     (arguments
      (substitute-keyword-arguments arguments
        ((#:configure-flags flags #~'())
         #~(cons* "-Dglvnd=true" #$flags))
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (add-after 'install 'fix-egl-vendor-icd
               (lambda _
                 (substitute* (string-append
                               #$output "/share/glvnd/egl_vendor.d/50_mesa.json")
                   (("libEGL_mesa\\.so\\.." all)
                    (string-append #$output "/lib/" all)))))
             (add-after 'set-layer-path-in-manifests 'add-architecture-to-filename
               (lambda _
                 (for-each
                  #$(add-architecture-to-filename)
                  '("/share/glvnd/egl_vendor.d/50_mesa.json"
                    "/share/vulkan/explicit_layer.d/VkLayer_MESA_overlay.json"
                    "/share/vulkan/implicit_layer.d/VkLayer_MESA_device_select.json")))))))))))

;; nvda is used as a name because it has the same length as mesa which is
;; required for grafting
(define (make-nvda driver)
  ((replace-nvidia-driver driver)
   (package
     (name "nvda")
     (version (string-pad-right
               (package-version driver)
               (string-length (package-version mesa-for-nvda))
               #\0))
     (source #f)
     (build-system trivial-build-system)
     (arguments
      (list #:modules '((guix build union))
            #:builder
            #~(begin
                (use-modules (guix build union))
                (union-build
                 #$output
                 '#$(list (this-package-input "libglvnd")
                          (this-package-input "mesa")
                          (this-package-input "nvidia-driver")
                          (this-package-input "nvidia-vaapi-driver"))))))
     (native-search-paths
      (list
       ;; https://github.com/NVIDIA/egl-wayland/issues/39
       (search-path-specification
         (variable "__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS")
         (files '("share/egl/egl_external_platform.d")))
       ;; https://gitlab.freedesktop.org/glvnd/libglvnd/-/blob/master/src/EGL/icd_enumeration.md
       (search-path-specification
         (variable "__EGL_VENDOR_LIBRARY_DIRS")
         (files '("share/glvnd/egl_vendor.d")))
       ;; See also: ‘src/gbm/main/backend.c’ in mesa source.
       (search-path-specification
         (variable "GBM_BACKENDS_PATH")
         (files '("lib/gbm")))
       ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
       ;; this to all VA-API back ends instead of once to libva.
       (search-path-specification
         (variable "LIBVA_DRIVERS_PATH")
         (files '("lib/dri")))
       (search-path-specification
         (variable "VDPAU_DRIVER_PATH")
         (files '("lib/vdpau"))
         (separator #f))
       ;; https://github.com/KhronosGroup/Vulkan-Loader/blob/main/docs/LoaderLayerInterface.md
       (search-path-specification
         (variable "XDG_DATA_DIRS")
         (files '("share")))))
     (synopsis "Nonguix's user-facing NVIDIA driver package")
     (description
      "This package provides a @code{mesa} variant with NVIDIA proprietary driver
support.  For dependency of other packages, use @code{nvidia-driver} instead.")
     (native-inputs '())
     (propagated-inputs
      (append
       (package-propagated-inputs mesa-for-nvda)
       (package-propagated-inputs driver)))
     (inputs (list mesa-for-nvda nvidia-driver nvidia-vaapi-driver))
     (outputs '("out"))
     (license (package-license driver))
     (home-page (package-home-page driver)))))

(define-syntax define-nvda-package
  (syntax-rules ()
    ((_ name driver)
     (define-public name
       (package
         (inherit (hidden-package (make-nvda driver)))
         (location (package-location driver)))))
    ((_ name driver alias)
     (define-public name
       (let ((nvda (make-nvda driver)))
         (package
           (inherit (package-with-alias alias nvda))
           (version (package-version driver))))))))

(define-nvda-package nvda-390 nvidia-driver-390)
(define-nvda-package nvda-470 nvidia-driver-470)
(define-nvda-package nvda-580 nvidia-driver-580)
(define-nvda-package nvda-590 nvidia-driver-590)
(define-nvda-package nvda-595 nvidia-driver-595)
(define-nvda-package nvda-beta nvidia-driver-beta)
(define-nvda-package nvda-user-alias-390 nvidia-driver-390 "nvda")
(define-nvda-package nvda-user-alias-470 nvidia-driver-470 "nvda")
(define-nvda-package nvda-user-alias-580 nvidia-driver-580 "nvda")
(define-nvda-package nvda-user-alias-590 nvidia-driver-590 "nvda")
(define-nvda-package nvda-user-alias-595 nvidia-driver-595 "nvda")
(define-nvda-package nvda-user-alias-beta nvidia-driver-beta "nvda-beta")
(define-public nvda nvda-580)
;; 2026-03
(define-deprecated-package nvdb nvda-beta)


;;;
;;; Package variants for NVIDIA proprietary driver
;;;

(define-syntax define-nvidia-container
  (syntax-rules ()
    ((_ name container-builder driver)
     (define-public name
       (hidden-package
        (package
          (inherit (nonguix-container->package
                    (nonguix-container
                      (inherit (container-builder driver))
                      (preserved-env %nvidia-environment-variable-regexps))))
          (location (package-location driver))))))
    ((_ name alias container alias-version)
     (define-public name
       (package
         (inherit (package-with-alias alias container))
         (version alias-version))))))

(define-nvidia-container steam-nvidia-390
  steam-container-for nvda-390)
(define-nvidia-container steam-nvidia-470
  steam-container-for nvda-470)
(define-nvidia-container steam-nvidia-580
  steam-container-for nvda-580)
(define-nvidia-container steam-nvidia-590
  steam-container-for nvda-590)
(define-nvidia-container steam-nvidia-595
  steam-container-for nvda-595)
(define-nvidia-container steam-nvidia-beta
  steam-container-for nvda-beta)
(define-public steam-nvidia steam-nvidia-580)

(define-nvidia-container steam-nvidia-user-alias-390 "steam-nvidia"
  steam-nvidia-390
  (package-version nvidia-driver-390))
(define-nvidia-container steam-nvidia-user-alias-470 "steam-nvidia"
  steam-nvidia-470
  (package-version nvidia-driver-470))
(define-nvidia-container steam-nvidia-user-alias-580 "steam-nvidia"
  steam-nvidia-580
  (package-version nvidia-driver-580))
(define-nvidia-container steam-nvidia-user-alias-590 "steam-nvidia"
  steam-nvidia-590
  (package-version nvidia-driver-590))
(define-nvidia-container steam-nvidia-user-alias-595 "steam-nvidia"
  steam-nvidia-595
  (package-version nvidia-driver-595))
(define-nvidia-container steam-nvidia-user-alias-beta "steam-nvidia-beta"
  steam-nvidia-beta
  (package-version nvidia-driver-beta))

(define-nvidia-container heroic-nvidia-390
  heroic-container-for nvda-390)
(define-nvidia-container heroic-nvidia-470
  heroic-container-for nvda-470)
(define-nvidia-container heroic-nvidia-580
  heroic-container-for nvda-580)
(define-nvidia-container heroic-nvidia-590
  heroic-container-for nvda-590)
(define-nvidia-container heroic-nvidia-595
  heroic-container-for nvda-595)
(define-nvidia-container heroic-nvidia-beta
  heroic-container-for nvda-beta)
(define-public heroic-nvidia heroic-nvidia-580)

(define-nvidia-container heroic-nvidia-user-alias-390 "heroic-nvidia"
  heroic-nvidia-390
  (package-version nvidia-driver-390))
(define-nvidia-container heroic-nvidia-user-alias-470 "heroic-nvidia"
  heroic-nvidia-470
  (package-version nvidia-driver-470))
(define-nvidia-container heroic-nvidia-user-alias-580 "heroic-nvidia"
  heroic-nvidia-580
  (package-version nvidia-driver-580))
(define-nvidia-container heroic-nvidia-user-alias-590 "heroic-nvidia"
  heroic-nvidia-590
  (package-version nvidia-driver-590))
(define-nvidia-container heroic-nvidia-user-alias-595 "heroic-nvidia"
  heroic-nvidia-595
  (package-version nvidia-driver-595))
(define-nvidia-container heroic-nvidia-user-alias-beta "heroic-nvidia-beta"
  heroic-nvidia-beta
  (package-version nvidia-driver-beta))

(define (make-ffmpeg-nvidia ffmpeg driver)
  ((replace-nvidia-driver driver)
   (package
     (inherit ffmpeg)
     (inputs
      (modify-inputs inputs
        (prepend nv-codec-headers)))
     (arguments
      (substitute-keyword-arguments arguments
        ((#:configure-flags flags)
         #~(cons* "--enable-cuvid"
                  "--enable-ffnvcodec"
                  "--enable-encoder=hevc_nvenc"
                  "--enable-encoder=h264_nvenc"
                  #$flags)))))))

(define-syntax define-ffmpeg-nvidia
  (syntax-rules ()
    ((_ name ffmpeg driver)
     (define-public name
       (package
         (inherit (hidden-package (make-ffmpeg-nvidia ffmpeg driver)))
         (location (package-location driver)))))
    ((_ name alias ffmpeg-nvidia alias-version)
     (define-public name
       (package
         (inherit (package-with-alias alias ffmpeg-nvidia))
         (version alias-version))))))

(define-ffmpeg-nvidia ffmpeg/nvidia-390 ffmpeg nvda-390)
(define-ffmpeg-nvidia ffmpeg/nvidia-470 ffmpeg nvda-470)
(define-ffmpeg-nvidia ffmpeg/nvidia-580 ffmpeg nvda-580)
(define-ffmpeg-nvidia ffmpeg/nvidia-590 ffmpeg nvda-590)
(define-ffmpeg-nvidia ffmpeg/nvidia-595 ffmpeg nvda-595)
(define-ffmpeg-nvidia ffmpeg/nvidia-beta ffmpeg nvda-beta)
(define-public ffmpeg/nvidia ffmpeg/nvidia-580)

(define-ffmpeg-nvidia ffmpeg-6/nvidia-390 ffmpeg-6 nvda-390)
(define-ffmpeg-nvidia ffmpeg-6/nvidia-470 ffmpeg-6 nvda-470)
(define-ffmpeg-nvidia ffmpeg-6/nvidia-580 ffmpeg-6 nvda-580)
(define-ffmpeg-nvidia ffmpeg-6/nvidia-590 ffmpeg-6 nvda-590)
(define-ffmpeg-nvidia ffmpeg-6/nvidia-595 ffmpeg-6 nvda-595)
(define-ffmpeg-nvidia ffmpeg-6/nvidia-beta ffmpeg-6 nvda-beta)
(define-public ffmpeg-6/nvidia ffmpeg-6/nvidia-580)

(define-ffmpeg-nvidia ffmpeg-nvidia-user-alias-390 "ffmpeg-nvidia"
  ffmpeg/nvidia-390
  (package-version nvidia-driver-390))
(define-ffmpeg-nvidia ffmpeg-nvidia-user-alias-470 "ffmpeg-nvidia"
  ffmpeg/nvidia-470
  (package-version nvidia-driver-470))
(define-ffmpeg-nvidia ffmpeg-nvidia-user-alias-580 "ffmpeg-nvidia"
  ffmpeg/nvidia-580
  (package-version nvidia-driver-580))
(define-ffmpeg-nvidia ffmpeg-nvidia-user-alias-590 "ffmpeg-nvidia"
  ffmpeg/nvidia-590
  (package-version nvidia-driver-590))
(define-ffmpeg-nvidia ffmpeg-nvidia-user-alias-595 "ffmpeg-nvidia"
  ffmpeg/nvidia-595
  (package-version nvidia-driver-595))
(define-ffmpeg-nvidia ffmpeg-nvidia-user-alias-beta "ffmpeg-nvidia-beta"
  ffmpeg/nvidia-beta
  (package-version nvidia-driver-beta))
(define-ffmpeg-nvidia ffmpeg-6-nvidia-user-alias-390 "ffmpeg-6-nvidia"
  ffmpeg-6/nvidia-390
  (package-version nvidia-driver-390))
(define-ffmpeg-nvidia ffmpeg-6-nvidia-user-alias-470 "ffmpeg-6-nvidia"
  ffmpeg-6/nvidia-470
  (package-version nvidia-driver-470))
(define-ffmpeg-nvidia ffmpeg-6-nvidia-user-alias-580 "ffmpeg-6-nvidia"
  ffmpeg-6/nvidia-580
  (package-version nvidia-driver-580))
(define-ffmpeg-nvidia ffmpeg-6-nvidia-user-alias-590 "ffmpeg-6-nvidia"
  ffmpeg-6/nvidia-590
  (package-version nvidia-driver-590))
(define-ffmpeg-nvidia ffmpeg-6-nvidia-user-alias-595 "ffmpeg-6-nvidia"
  ffmpeg-6/nvidia-595
  (package-version nvidia-driver-595))
(define-ffmpeg-nvidia ffmpeg-6-nvidia-user-alias-beta "ffmpeg-6-nvidia-beta"
  ffmpeg-6/nvidia-beta
  (package-version nvidia-driver-beta))

(define* (replace-mesa obj #:key (driver nvda))
  (let ((rebuild (replace-nvidia-driver driver)))
    (with-transformation
        (package-input-grafting
         `((,mesa . ,driver)
           (,nvidia-driver . ,driver)
           (,ffmpeg . ,(rebuild ffmpeg/nvidia))
           (,ffmpeg-6 . ,(rebuild ffmpeg-6/nvidia))))
      obj)))


;;;
;;; Other packages
;;;

(define-public nvidia-prime
  (package
    (name "nvidia-prime")
    (version "1.0-5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.archlinux.org/archlinux/packaging/packages/nvidia-prime.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "017xjk4lp25ib6jn3lpf80x7bfybj6lfam7xd1byyjj5rd60jp7f"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'build)
               (delete 'check)
               (replace 'install
                 (lambda _
                   (chmod "prime-run" #o555)
                   (install-file "prime-run" (in-vicinity #$output "bin")))))))
    (inputs (list bash-minimal))
    (home-page "https://www.archlinux.org/packages/extra/any/nvidia-prime/")
    (synopsis "NVIDIA PRIME render offload configuration and utilities")
    (description
     "This package provides @command{prime-run} to run a program on the NVIDIA
GPU in switchable graphics setup.")
    (license license-gnu:gpl3+)))

(define-public gpustat
  (package
    (name "gpustat")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/wookayin/gpustat")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d1ln9wrb4ij0yl3fz03vyby598y5hwlxbvcjw33pnndg8hvwi2v"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-version
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                           #$(package-version this-package)))))))
    (propagated-inputs
     (list python-blessed python-nvidia-ml-py python-psutil))
    (native-inputs
     (list python-mockito
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (home-page "https://github.com/wookayin/gpustat")
    (synopsis "Utility to monitor NVIDIA GPU status and usage")
    (description
     "This package provides an utility to monitor NVIDIA GPU status
and usage.")
    (license license-gnu:expat)))

(define-public nvidia-exec
  (package
    (name "nvidia-exec")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pedro00dk/nvidia-exec")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "079alqgz3drv5mvx059fzhj3f20rnljl7r4yihfd5qq7djgmvv0v"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~`(("nvx" "bin/"))
           #:modules #~((guix build copy-build-system)
                        (guix build utils)
                        (srfi srfi-1))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'wrap-nvx
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (wrap-program (string-append #$output "/bin/nvx")
                              `("PATH" ":" prefix
                                ,(fold (lambda (input paths)
                                         (let* ((in (assoc-ref
                                                     inputs input))
                                                (bin (string-append
                                                      in "/bin")))
                                           (append (filter
                                                    file-exists?
                                                    (list bin))
                                                   paths)))
                                       '()
                                       '("jq" "lshw" "lsof")))))))))
    (inputs (list bash-minimal jq lshw lsof))
    (home-page "https://github.com/pedro00dk/nvidia-exec")
    (synopsis "GPU switching without login out for Nvidia Optimus laptops")
    (description
     "This package provides GPU switching without login out for Nvidia Optimus
laptops.")
    (license license-gnu:gpl3+)))

(define-public nvidia-htop
  (package
    (name "nvidia-htop")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/peci1/nvidia-htop")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d5cd4cp7swq5np8b9ryibhg2zpfwzh2dzbsvsrp0gx33krxjvyj"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda args
                   (with-directory-excursion "test"
                     (apply (assoc-ref %standard-phases 'check) args))))
               (add-after 'unpack 'fix-libnvidia
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "nvidia-htop.py"
                     (("nvidia-smi" file)
                      (search-input-file inputs (in-vicinity "bin" file)))))))))
    (native-inputs (list python-pytest python-setuptools))
    (inputs (list nvidia-driver))
    (propagated-inputs (list python-termcolor))
    (home-page "https://github.com/peci1/nvidia-htop")
    (synopsis "Enriched nvidia-smi output")
    (description
     "This package provides a tool for enriching the output of
@command{nvidia-smi}.")
    (license license-gnu:bsd-3)))

(define-public nvidia-nvml
  (package
    (name "nvidia-nvml")
    (version "352.79")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://developer.download.nvidia.com/compute/cuda/7.5/Prod/gdk/"
                           (format #f "gdk_linux_amd64_~a_release.run"
                                   (string-replace-substring version "." "_"))))
       (sha256
        (base32
         "1r2cwm0j9svaasky3qw46cpg2q6rrazwzrc880nxh6bismyd3a9z"))
       (file-name (string-append "nvidia-nvml-" version "-checkout"))))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "sh" source "--tar" "xvf"))))
           #:install-plan
           ''(("payload/nvml/lib" "lib")
              ("payload/nvml/include" "include/nvidia/gdk")
              ("payload/nvml/example" "src/gdk/nvml/examples")
              ("payload/nvml/doc/man" "share/man")
              ("payload/nvml/README.txt" "README.txt")
              ("payload/nvml/COPYRIGHT.txt" "COPYRIGHT.txt"))))
    (home-page "https://www.nvidia.com")
    (synopsis "The NVIDIA Management Library (NVML)")
    (description "C-based programmatic interface for monitoring and managing various
states within NVIDIA Tesla GPUs.  It is intended to be a platform for
building 3rd party applications, and is also the underlying library for the
NVIDIA-supported nvidia-smi tool.  NVML is thread-safe so it is safe to make
simultaneous NVML calls from multiple threads.")
    ;; Doesn't have any specific LICENSE file, but see COPYRIGHT.txt for details.
    (license (license:nonfree "file://COPYRIGHT.txt"))))

(define-public nvidia-system-monitor
  (package
    (name "nvidia-system-monitor")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/congard/nvidia-system-monitor-qt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aghdqljvjmc02g9jpc7sb3yhha738ywny51riska56hkxd3jg2l"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-nvidia-smi
                          (lambda _
                            (let ((nvidia-smi (string-append #$(this-package-input
                                                                "nvidia-driver")
                                                             "/bin/nvidia-smi")))
                              (substitute* "src/core/InfoProvider.cpp"
                                (("nvidia-smi")
                                 nvidia-smi))
                              (substitute* "src/main.cpp"
                                (("which nvidia-smi")
                                 (string-append "which " nvidia-smi))
                                (("exec..nvidia-smi")
                                 (string-append "exec(\"" nvidia-smi))))))
                        (replace 'install
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let ((bin (string-append #$output "/bin")))
                              (mkdir-p bin)
                              (install-file "qnvsm" bin)))))))
    (inputs (list qtbase-5 qtdeclarative-5 nvidia-driver))
    (home-page "https://github.com/congard/nvidia-system-monitor-qt")
    (synopsis "Task manager for Nvidia graphics cards")
    (description
     "This package provides a task manager for Nvidia graphics cards.")
    (license license-gnu:expat)))

(define-public python-nvidia-ml-py
  (package
    (name "python-nvidia-ml-py")
    (version "11.495.46")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nvidia-ml-py" version))
              (sha256
               (base32
                "09cnb7xasd7brby52j70y7fqsfm9n6gvgqf769v0cmj74ypy2s4g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                       ;No tests in PyPi archive.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-libnvidia
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "pynvml.py"
                (("libnvidia-ml.so.1" file)
                 (search-input-file inputs (in-vicinity "lib" file)))))))))
    (native-inputs (list python-setuptools))
    (inputs (list nvidia-driver))
    (home-page "https://forums.developer.nvidia.com")
    (synopsis "Python bindings to NVIDIA Management Library")
    (description
     "This package is a wrapper around @acronym{NVML, NVIDIA Management
Library}.  It provides a Python interface to GPU management and monitoring
functions.")
    (license license-gnu:bsd-3)))

(define-public python-py3nvml
  (package
    (name "python-py3nvml")
    (version "0.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/fbcotter/py3nvml")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "012j13jg8qbl5lvr4gww0jvl9i8yk42za4fxvdg2h1sy866yk49m"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           ;; These tests require the NVIDIA driver to be loaded.
           #~(list "-k" (string-join (list "not test_readme1"
                                           "test_grabgpus2"
                                           "test_grabgpus3")
                                     " and not "))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-libnvidia
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "py3nvml/py3nvml.py"
                     (("libnvidia-ml.so.1" file)
                      (search-input-file inputs (in-vicinity "lib" file)))))))))
    (native-inputs (list python-numpy python-pytest python-setuptools))
    (propagated-inputs (list nvidia-driver python-xmltodict))
    (home-page "https://github.com/fbcotter/py3nvml")
    (synopsis "Python bindings to NVIDIA Management Library")
    (description
     "This package provides unofficial Python bindings to @acronym{NVML, NVIDIA
Management Library}.

See @code{python-nvidia-ml-py} package for the official bindings provided by
NVIDIA.")
    (license license-gnu:bsd-3)))
