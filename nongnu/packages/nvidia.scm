;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022, 2023 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2022, 2023, 2024 Hilton Chain <hako@ultrarare.space>

(define-module (nongnu packages nvidia)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license-gnu:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (ice-9 match))

(define-public %nvidia-environment-variable-regexps
  '("^__GL_"                            ; NVIDIA OpenGL settings.
    "^__GLX_VENDOR_LIBRARY_NAME$"       ; For GLVND.
    ;; NVIDIA PRIME Render Offload.
    "^__NV_PRIME_RENDER_OFFLOAD(_PROVIDER)?$"
    "^__VK_LAYER_NV_optimus$"
    ;; NVIDIA NGX.
    "^__NGX_CONF_FILE$"
    "^__NV_SIGNED_LOAD_CHECK$"
    "^PROTON_ENABLE_NGX_UPDATER$"
    ;; NVIDIA VDPAU settings.
    "^VDPAU_NVIDIA_"
    ;; GSYNC control for Vulkan direct-to-display applications.
    "^VKDirectGSYNC(Compatible)?Allowed$"))


;;;
;;; NVIDIA driver checkouts
;;;

(define nvidia-driver-snippet
  ;; Note: delay to cope with cyclic module imports at the top level.
  (delay
    #~(begin
        (use-modules (guix build utils) (ice-9 ftw) (srfi srfi-1))
        (set-path-environment-variable
         "PATH" '("bin")
         '#+(list bash-minimal coreutils-minimal grep tar zstd))
        (let* ((this-file (last (scandir (getcwd)))))
          (invoke "sh" this-file "--extract-only" "--target" "extractdir")
          (for-each delete-file
                    (find-files "extractdir"
                                (string-join
                                 '(;; egl-gbm
                                   "libnvidia-egl-gbm\\.so\\."
                                   ;; egl-wayland
                                   "libnvidia-egl-wayland\\.so\\."
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
          (with-directory-excursion "extractdir"
            (invoke "tar" "cvfa" (string-append this-file ".tar")
                    "--mtime=1" "--owner=root:0" "--group=root:0" ;determinism
                    "--sort=name" ".")
            (invoke "zstd" (string-append this-file ".tar")))
          (rename-file
           (string-append "extractdir/" this-file ".tar.zst") this-file)))))

(define (nvidia-source version hash)
  "Given VERSION of an NVIDIA driver installer, return an <origin> for
its unpacked checkout."
  (origin
    (method url-fetch)
    (uri (string-append
          "https://us.download.nvidia.com/XFree86/Linux-x86_64/"
          version "/NVIDIA-Linux-x86_64-" version ".run"))
    (file-name (string-append "NVIDIA-Linux-x86_64-" version))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (snippet (force nvidia-driver-snippet))))


;;;
;;; NVIDIA drivers
;;;


(define %nvidia-script-create-device-nodes
  (program-file
   "create-device-nodes.scm"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (ice-9 regex)
                      (rnrs io ports)
                      (srfi srfi-1)
                      (guix build utils))

         (define %nvidia-character-devices
           (call-with-input-file "/proc/devices"
             (lambda (port)
               (filter-map
                (lambda (line)
                  (if (string-contains line "nvidia")
                      (apply cons (reverse (string-tokenize line)))
                      #f))
                (string-split (get-string-all port) #\newline)))))

         (define %nvidia-driver-device-minors
           (let ((device-minor-regexp (make-regexp "^Device Minor: \t (.*)")))
             (append-map
              (lambda (file)
                (call-with-input-file file
                  (lambda (port)
                    (filter-map
                     (lambda (line)
                       (let ((matched (regexp-exec device-minor-regexp line)))
                         (if matched
                             (match:substring matched 1)
                             #f)))
                     (string-split (get-string-all port) #\newline)))))
              (find-files "/proc/driver/nvidia/gpus/" "information$"))))

         (define (create-device-node path name minor)
           (let ((major
                  (or (assoc-ref %nvidia-character-devices name)
                      (assoc-ref %nvidia-character-devices "nvidia-frontend")))
                 (mknod #$(file-append coreutils "/bin/mknod")))
             (system* mknod "-Zm0666" path "c" major minor)))

         (define (main args)
           (case (string->symbol (first args))
             ((nvidia_modeset)
              (create-device-node "/dev/nvidia-modeset" "nvidia-modeset" "254"))
             ((nvidia_uvm)
              (begin
                (create-device-node "/dev/nvidia-uvm" "nvidia-uvm" "0")
                (create-device-node "/dev/nvidia-uvm-tools" "nvidia-uvm" "1")))
             ((nvidia)
              (begin
                (create-device-node "/dev/nvidiactl" "nvidiactl" "255")
                (for-each
                 (lambda (minor)
                   (create-device-node
                    (string-append "/dev/nvidia" minor) "nvidia" minor))
                 %nvidia-driver-device-minors)))))

         (main (cdr (command-line)))))))

;; Adapted from <https://github.com/Frogging-Family/nvidia-all/blob/master/60-nvidia.rules>
(define %nvidia-udev-rules
  (mixed-text-file
   "90-nvidia.rules" "\
# Make sure device nodes are present even when the DDX is not started for the Wayland/EGLStream case
KERNEL==\"nvidia\", RUN+=\"" %nvidia-script-create-device-nodes " nvidia\"
KERNEL==\"nvidia_modeset\", RUN+=\"" %nvidia-script-create-device-nodes " nvidia_modeset\"
KERNEL==\"nvidia_uvm\", RUN+=\"" %nvidia-script-create-device-nodes " nvidia_uvm\"

# Enable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA Audio devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"auto\"
# Enable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"bind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"auto\"

# Disable runtime PM for NVIDIA VGA/3D controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x03[0-9]*\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA Audio devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x040300\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB xHCI Host Controller devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c0330\", TEST==\"power/control\", ATTR{power/control}=\"on\"
# Disable runtime PM for NVIDIA USB Type-C UCSI devices
ACTION==\"unbind\", SUBSYSTEM==\"pci\", ATTR{vendor}==\"0x10de\", ATTR{class}==\"0x0c8000\", TEST==\"power/control\", ATTR{power/control}=\"on\"
"))

(define-public nvidia-driver
  (package
    (name "nvidia-driver")
    (version "570.124.04")
    (source (nvidia-source
             version "1i2k1phmx0b3j68qs53c3667fkwad03l1bjqdhhw9mr2f55nly0v"))
    (build-system copy-build-system)
    (arguments
     (list #:modules '((guix build copy-build-system)
                       (guix build utils)
                       (ice-9 popen)
                       (ice-9 rdelim)
                       (ice-9 regex)
                       (srfi srfi-26))
           #:install-plan
           #~`((#$(match (or (%current-target-system) (%current-system))
                    ("i686-linux" "32")
                    ("x86_64-linux" ".")
                    (_ "."))
                "lib/" #:include-regexp ("^./[^/]+\\.so"))
               ("." "lib/nvidia/wine/" #:include-regexp ("_?nvngx\\.dll$"))
               ("." "share/nvidia/" #:include-regexp ("nvidia-application-profiles"))
               ("." "share/egl/egl_external_platform.d/" #:include-regexp ("(gbm|wayland|xcb|xlib)\\.json"))
               ("10_nvidia.json" "share/glvnd/egl_vendor.d/")
               ("90-nvidia.rules" "lib/udev/rules.d/")
               ("nvidia-drm-outputclass.conf" "share/X11/xorg.conf.d/")
               ("nvidia-dbus.conf" "share/dbus-1/system.d/")
               ("nvidia.icd" "etc/OpenCL/vendors/")
               ("nvidia_icd.json" "share/vulkan/icd.d/")
               ("nvidia_icd_vksc.json" "etc/vulkansc/icd.d/")
               ("nvidia_layers.json" "share/vulkan/implicit_layer.d/")
               ("sandboxutils-filelist.json" "share/nvidia/files.d/"))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "tar" "xvf" source)))
               (delete 'strip)
               (add-after 'unpack 'create-misc-files
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; EGL external platform configuraiton
                   (substitute* '("10_nvidia_wayland.json"
                                  "15_nvidia_gbm.json"
                                  "20_nvidia_xcb.json"
                                  "20_nvidia_xlib.json")
                     (("libnvidia-egl-(wayland|gbm|xcb|xlib)\\.so\\.." all)
                      (search-input-file inputs (string-append "lib/" all))))

                   ;; EGL vendor ICD configuration
                   (substitute* "10_nvidia.json"
                     (("libEGL_nvidia\\.so\\.." all)
                      (string-append #$output "/lib/" all)))

                   ;; OpenCL vendor ICD configuration
                   (substitute* "nvidia.icd"
                     (("libnvidia-opencl\\.so\\.." all)
                      (string-append #$output "/lib/" all)))

                   ;; Vulkan ICD & layer configuraiton
                   (substitute* '("nvidia_icd.json"
                                  "nvidia_layers.json")
                     (("libGLX_nvidia\\.so\\.." all)
                      (string-append #$output "/lib/" all)))

                   ;; VulkanSC ICD configuration
                   (substitute* "nvidia_icd_vksc.json"
                     (("libnvidia-vksc-core\\.so\\.." all)
                      (string-append #$output "/lib/" all)))

                   ;; Add udev rules
                   (symlink #$%nvidia-udev-rules "90-nvidia.rules")))
               (add-after 'install 'add-architecture-to-filename
                 (lambda _
                   (for-each
                    (lambda (path)
                      (let* ((out #$output)
                             (system #$(or (%current-target-system)
                                           (%current-system)))
                             (dash (string-index system #\-))
                             (arch (string-take system dash))

                             (dot  (string-index-right path #\.))
                             (base (string-take path dot))
                             (ext  (string-drop path (+ 1 dot))))
                        ;; <...>/nvidia.icd -> <...>/nvidia.x86_64.icd
                        ;; <...>/nvidia_icd.json -> <...>/nvidia_icd.x86_64.json
                        (rename-file
                         (string-append out path)
                         (string-append out base "." arch "." ext))))
                    '("/etc/OpenCL/vendors/nvidia.icd"
                      "/share/egl/egl_external_platform.d/10_nvidia_wayland.json"
                      "/share/egl/egl_external_platform.d/15_nvidia_gbm.json"
                      "/share/egl/egl_external_platform.d/20_nvidia_xcb.json"
                      "/share/egl/egl_external_platform.d/20_nvidia_xlib.json"
                      "/share/glvnd/egl_vendor.d/10_nvidia.json"
                      "/share/vulkan/icd.d/nvidia_icd.json"
                      "/share/vulkan/implicit_layer.d/nvidia_layers.json"))))
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
                                                  inputs
                                                  (string-append "lib/" name))))
                                              '("libX11.so.6"
                                                "libXext.so.6"
                                                "libcrypto.so.1.1"
                                                "libcrypto.so.3"
                                                "libdrm.so.2"
                                                "libgbm.so.1"
                                                "libgcc_s.so.1"
                                                "libwayland-client.so.0"
                                                "libxcb.so.1")))
                                  ":")))
                     (define (patch-elf file)
                       (format #t "Patching ~a ..." file)
                       (unless (string-contains file ".so")
                         (invoke "patchelf" "--set-interpreter" ld.so file))
                       (invoke "patchelf" "--set-rpath" rpath file)
                       (display " done\n"))

                     (for-each (lambda (file)
                                 (when (elf-file? file)
                                   (patch-elf file)))
                               (find-files #$output)))))
               (add-before 'patch-elf 'install-commands
                 (lambda _
                   (when (string-match
                          "x86_64-linux"
                          (or #$(%current-target-system) #$(%current-system)))
                     (for-each
                      (lambda (binary)
                        (let ((bindir (string-append #$output "/bin"))
                              (manual (string-append binary ".1.gz"))
                              (mandir (string-append #$output "/share/man/man1")))
                          (install-file binary bindir)
                          (when (file-exists? manual)
                            (install-file manual mandir))))
                      '("nvidia-cuda-mps-control"
                        "nvidia-cuda-mps-server"
                        "nvidia-pcc"
                        "nvidia-smi")))))
               (add-before 'patch-elf 'relocate-libraries
                 (lambda _
                   (let* ((version #$(package-version this-package))
                          (libdir (string-append #$output "/lib"))
                          (gbmdir (string-append libdir "/gbm"))
                          (vdpaudir (string-append libdir "/vdpau"))
                          (xorgmoddir (string-append libdir "/xorg/modules"))
                          (xorgdrvdir (string-append xorgmoddir "/drivers"))
                          (xorgextdir (string-append xorgmoddir "/extensions"))
                          (move-to-dir (lambda (file dir)
                                         (install-file file dir)
                                         (delete-file file))))
                     (for-each
                      (lambda (file)
                        (mkdir-p gbmdir)
                        (with-directory-excursion gbmdir
                          (symlink file "nvidia-drm_gbm.so")))
                      (find-files libdir "libnvidia-allocator\\.so\\."))

                     (for-each
                      (cut move-to-dir <> vdpaudir)
                      (find-files libdir "libvdpau_nvidia\\.so\\."))

                     (for-each
                      (cut move-to-dir <> xorgdrvdir)
                      (find-files libdir "nvidia_drv\\.so$"))

                     (for-each
                      (lambda (file)
                        (move-to-dir file xorgextdir)
                        (with-directory-excursion xorgextdir
                          (symlink (basename file)
                                   "libglxserver_nvidia.so")))
                      (find-files libdir "libglxserver_nvidia\\.so\\.")))))
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
                          (let* ((soname (string-append
                                          (dirname lib) "/" lib-soname))
                                 (base (string-append
                                        (regexp-substitute
                                         #f (string-match "(.*)\\.so.*" soname) 1)
                                        ".so"))
                                 (source (basename lib)))
                            (for-each
                             (lambda (target)
                               (unless (file-exists? target)
                                 (format #t "Symlinking ~a -> ~a..."
                                         target source)
                                 (symlink source target)
                                 (display " done\n")))
                             (list soname base))))))
                    (find-files #$output "\\.so\\.")))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (native-inputs (list patchelf-0.16))
    (inputs
     (list egl-gbm
           egl-wayland
           egl-x11
           `(,gcc "lib")
           glibc
           mesa-for-nvda
           openssl
           openssl-1.1
           wayland))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver (libraries)")
    (description
     "This package provides libraries of the proprietary NVIDIA driver.  It's
mainly used as a dependency of other packages.  For user-facing purpose, use
@code{nvda} instead.")
    (license
     (license:nonfree
      (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-driver-beta
  (package
    (inherit nvidia-driver)
    (name "nvidia-driver-beta")
    (version "570.86.16")
    (source (nvidia-source
             version "1mfbc59g5v1c6dqissg1mfawvaknqrr7r985214py92lnr5ylqs5"))))

(define-public nvidia-libs
  (deprecated-package "nvidia-libs" nvidia-driver))


;;;
;;; NVIDIA firmwares
;;;


(define-public nvidia-firmware
  (let ((base nvidia-driver))
    (package
      (inherit base)
      (name "nvidia-firmware")
      (arguments
       (list #:install-plan
             #~'(("firmware" #$(string-append "lib/firmware/nvidia/"
                                              (package-version this-package))))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'strip)
                 (replace 'unpack
                   (lambda* (#:key source #:allow-other-keys)
                     (invoke "tar" "xvf" source))))))
      (propagated-inputs '())
      (inputs '())
      (native-inputs '())
      (synopsis "Proprietary NVIDIA driver (GSP firmwares)")
      (description
       "This package provides firmwares for NVIDIA's GPU System Processor.
Firmware installation can be done with @code{nvidia-service-type}, however
whether GSP mode is enabled by default or not depends on the specific GPU
product.

To enable GSP mode manually, add @code{\"NVreg_EnableGpuFirmware=1\"} to
@code{kernel-arguments} field of the @code{operating-system} configuration."))))

(define-public nvidia-firmware-beta
  (package
    (inherit nvidia-firmware)
    (name "nvidia-firmware-beta")
    (version (package-version nvidia-driver-beta))
    (source (package-source nvidia-driver-beta))))


;;;
;;; NVIDIA kernel modules
;;;


(define-public nvidia-module
  (package
    (name "nvidia-module")
    (version (package-version nvidia-driver))
    (source (package-source nvidia-driver))
    (build-system linux-module-build-system)
    (arguments
     (list #:linux linux-lts
           #:source-directory "kernel"
           #:tests? #f
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "tar" "xvf" source)))
               (delete 'strip)
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
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver (kernel modules)")
    (description
     "This package provides kernel modules of the proprietary NVIDIA driver.
Module setup can be done with @code{nvidia-service-type}, to actually use these
modules, also add @code{modprobe.blacklist=nouveau} to @code{kernel-arguments}
field of the @code{operating-system} configuration.

If the NVIDIA card is not used for displaying, or on a Wayland environment,
add @code{nvidia_drm.modeset=1} to @code{kernel-arguments} as well.")
    (license
     (license:nonfree
      (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-module-beta
  (package
    (inherit nvidia-module)
    (name "nvidia-module-beta")
    (version (package-version nvidia-driver-beta))
    (source (package-source nvidia-driver-beta))))

(define-public nvidia-module-open
  (let ((base nvidia-module))
    (package
      (inherit base)
      (name "nvidia-module-open")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ;; NOTE: Kernels compiled with CONFIG_LTO_CLANG_THIN would cause an
         ;; error here.  See also:
         ;; <https://github.com/NVIDIA/open-gpu-kernel-modules/issues/214>
         ;; <https://github.com/llvm/llvm-project/issues/55820>
         ((#:source-directory _) "kernel-open")))
      (home-page "https://github.com/NVIDIA/open-gpu-kernel-modules")
      (synopsis "Open source NVIDIA kernel modules")
      (description
       "This package provides open source NVIDIA kernel modules, however
proprietary firmware and libraries are still necessary, and these modules
require GPU System Processor to be present (Turing or later architectures) and
enabled (see also the description of @code{nvidia-firmware} package).

Module setup can be done with @code{nvidia-service-type} (with @code{module}
field of @code{nvidia-configuration} set to @code{nvidia-module-open}), to
actually use these modules, also add @code{modprobe.blacklist=nouveau} to
@code{kernel-arguments} field of the @code{operating-system} configuration.

If the NVIDIA card is not used for displaying, or on a Wayland environment,
add @code{nvidia_drm.modeset=1} to @code{kernel-arguments} as well.")
      (license license-gnu:gpl2))))

(define-public nvidia-module-open-beta
  (package
    (inherit nvidia-module-open)
    (name "nvidia-module-open-beta")
    (version (package-version nvidia-driver-beta))
    (source (package-source nvidia-driver-beta))))


;;;
;;; ‘nvidia-settings’ packages
;;;

(define (nvidia-settings-source name version hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/NVIDIA/nvidia-settings")
          (commit version)))
    (file-name (git-file-name name version))
    (modules '((guix build utils)))
    (snippet '(delete-file-recursively "src/jansson"))
    (sha256 (base32 hash))))

(define-public nvidia-settings
  (package
    (name "nvidia-settings")
    (version "570.124.04")
    (source (nvidia-settings-source
             name version "13xj3b1xbmclk085vz73vhra17bg5l7xf589d8pky70qzckz9lic"))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no test suite
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
                     (("__NVIDIA_SETTINGS_DESKTOP_CATEGORIES__") "Settings"))
                   (install-file "doc/nvidia-settings.desktop"
                                 (string-append
                                  #$output "/share/applications"))
                   (install-file "doc/nvidia-settings.png"
                                 (string-append
                                  #$output "/share/icons/hicolor/128x128/apps"))))
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
    (synopsis "Nvidia driver control panel")
    (description
     "This package provides Nvidia driver control panel for monitor
configuration, creating application profiles, gpu monitoring and more.")
    (home-page "https://github.com/NVIDIA/nvidia-settings")
    (license license-gnu:gpl2)))

(define-public nvidia-settings-beta
  (package
    (inherit nvidia-settings)
    (name "nvidia-settings-beta")
    (version "570.86.16")
    (source (nvidia-settings-source
             name version
             "0gs5iml7yp5sd6vybj3alb6cg06ylljn4h7iwp2i1jhkms3nmfzn"))))


;;;
;;; ‘nvda’ packages
;;;


(define-public libglvnd-for-nvda
  (hidden-package
   (package
     (inherit libglvnd)
     (arguments
      (substitute-keyword-arguments (package-arguments libglvnd)
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
      (modify-inputs (package-propagated-inputs mesa)
        (prepend libglvnd-for-nvda)))
     (arguments
      (substitute-keyword-arguments (package-arguments mesa)
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
                  (lambda (path)
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
                       (string-append out base "." arch "." ext))))
                  '("/share/glvnd/egl_vendor.d/50_mesa.json"
                    "/share/vulkan/explicit_layer.d/VkLayer_MESA_overlay.json"
                    "/share/vulkan/implicit_layer.d/VkLayer_MESA_device_select.json")))))))))))

;; nvda is used as a name because it has the same length as mesa which is
;; required for grafting
(define-public nvda
  (package
    (name "nvda")
    (version (string-pad-right
              (package-version nvidia-driver)
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
     "This package provides a drop-in replacement for @code{mesa} and is
intended to be installed by @code{nvidia-service-type}.

To actually use the NVIDIA card, replacement must be applied for individual
packages, this can be done either by rewriting inputs with
@code{--with-input=mesa=nvda} or grafting with @code{--with-graft=mesa=nvda}.
For a programmatical way, the procedure @code{replace-mesa} can be used.

Additionally, if the NVIDIA card is not used for displaying, environment
variables @code{__GLX_VENDOR_LIBRARY_NAME=nvidia} and
@code{__NV_PRIME_RENDER_OFFLOAD=1} may be set.")
    (native-inputs '())
    (propagated-inputs
     (append
      (package-propagated-inputs mesa-for-nvda)
      (package-propagated-inputs nvidia-driver)))
    (inputs (list mesa-for-nvda nvidia-driver nvidia-vaapi-driver))
    (outputs '("out"))
    (license (package-license nvidia-driver))
    (home-page (package-home-page nvidia-driver))))

(define-public nvdb
  ((package-input-rewriting `((,nvidia-driver . ,nvidia-driver-beta)))
   (package
     (inherit nvda)
     (name "nvdb")
     (version (string-pad-right
               (package-version nvidia-driver-beta)
               (string-length (package-version mesa-for-nvda))
               #\0)))))

(define mesa/fake
  (package
    (inherit mesa)
    (replacement nvda)))

(define-public mesa/fake-beta
  (hidden-package
   (package
     (inherit mesa)
     (replacement nvdb))))

(define-public replace-mesa
  (package-input-rewriting `((,mesa . ,mesa/fake))))


;;;
;;; Other packages
;;;


(define-public egl-gbm
  (package
    (name "egl-gbm")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/egl-gbm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rfgfi06ry7c7hnzdm4b0dc8r3hmbfn2rd37z3mc4wn38sgz5l3a"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list eglexternalplatform mesa-for-nvda))
    (synopsis "GBM EGL external platform library")
    (description
     "This package provides an EGL External Platform library implementation for
GBM EGL support.")
    (home-page "https://github.com/NVIDIA/egl-gbm")
    (license license-gnu:expat)))

(define-public egl-x11
  (package
    (name "egl-x11")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/egl-x11")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15zqzx061cpzcs0mxc7nnsv9rabfszfxxmwr5v7flxi4m9j6hshc"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list eglexternalplatform mesa-for-nvda))
    (synopsis "X11 and XCB EGL external platform library")
    (description
     "This package provides an EGL platform library for the NVIDIA driver to
support XWayland via xlib (using @code{EGL_KHR_platform_x11}) or xcb (using
@code{EGL_EXT_platform_xcb}).")
    (home-page "https://github.com/NVIDIA/egl-x11")
    (license license-gnu:expat)))

(define-public gpustat
  (package
    (name "gpustat")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gpustat" version))
              (sha256
               (base32
                "1wg3yikkqdrcxp5xscyb9rxifgfwv7qh73xv4airab63b3w8y7jq"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs (list python-blessed python-nvidia-ml-py python-psutil
                             python-six))
    (native-inputs (list python-mock python-pytest python-pytest-runner))
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
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nvidia-htop" version))
              (sha256
               (base32
                "0lv9cpccpkbg0d577irm1lp9rx6pacyk2pk9v41k9s9hyl4b7hvx"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-libnvidia
                          (lambda _
                            (substitute* "nvidia-htop.py"
                              (("nvidia-smi")
                               (string-append #$(this-package-input
                                                 "nvidia-driver")
                                              "/bin/nvidia-smi"))))))))
    (inputs (list nvidia-driver))
    (propagated-inputs (list python-termcolor))
    (home-page "https://github.com/peci1/nvidia-htop")
    (synopsis "Tool to enrich the output of nvidia-smi")
    (description "This package provides tool for enriching the output of
nvidia-smi.")
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
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-libnvidia
                          (lambda _
                            (substitute* "pynvml.py"
                              (("libnvidia-ml.so.1")
                               (string-append #$(this-package-input
                                                 "nvidia-driver")
                                              "/lib/libnvidia-ml.so.1"))))))))
    (inputs (list nvidia-driver))
    (home-page "https://forums.developer.nvidia.com")
    (synopsis "Python Bindings for the NVIDIA Management Library")
    (description "This package provides official Python Bindings for the NVIDIA
Management Library")
    (license license-gnu:bsd-3)))

(define-public python-py3nvml
  (package
    (name "python-py3nvml")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "py3nvml" version))
              (sha256
               (base32
                "0wxxky9amy38q7qjsdmmznk1kqdzwd680ps64i76cvlab421vvh9"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-libnvidia
                          (lambda _
                            (substitute* "py3nvml/py3nvml.py"
                              (("libnvidia-ml.so.1")
                               (string-append #$(this-package-input
                                                 "nvidia-driver")
                                              "/lib/libnvidia-ml.so.1"))))))))
    (propagated-inputs (list nvidia-driver python-xmltodict))
    (home-page "https://github.com/fbcotter/py3nvml")
    (synopsis "Unoffcial Python 3 Bindings for the NVIDIA Management Library")
    (description "This package provides unofficial Python 3 Bindings for the
NVIDIA Management Library")
    (license license-gnu:bsd-3)))
