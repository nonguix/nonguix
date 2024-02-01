;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022, 2023 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>

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
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (nongnu packages linux)
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

(define-public nvidia-version "515.76")


;;;
;;; NVIDIA driver checkouts
;;;


(define (nvidia-source-unbundle-libraries version)
  #t)

(define* (make-nvidia-source
          version hash
          #:optional (get-cleanup-snippet nvidia-source-unbundle-libraries))
  "Given VERSION and HASH of an NVIDIA driver installer, return an <origin> for
its unpacked checkout.  GET-CLEANUP-SNIPPET is a procedure that accepts the
VERSION as argument and returns a G-expression."
  (define installer
    (origin
      (method url-fetch)
      (uri (string-append
            "https://us.download.nvidia.com/XFree86/Linux-x86_64/"
            version "/NVIDIA-Linux-x86_64-" version ".run"))
      (sha256 hash)))
  (origin
    (method (@@ (guix packages) computed-origin-method))
    (file-name (string-append "nvidia-driver-" version "-checkout"))
    (sha256 #f)
    (snippet (get-cleanup-snippet version))
    (uri
     (delay
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils)
                          (ice-9 ftw))
             (set-path-environment-variable
              "PATH" '("bin")
              '#+(list bash-minimal
                       coreutils
                       gawk
                       grep
                       tar
                       which
                       xz))
             (setenv "XZ_OPT" (string-join (%xz-parallel-args)))
             (invoke "sh" #$installer "-x")
             (copy-recursively
              (car (scandir (canonicalize-path (getcwd))
                            (lambda (file)
                              (not (member file '("." ".."))))))
              #$output)))))))

(define-public nvidia-source
  (make-nvidia-source
   nvidia-version
   (base32 "0i5zyvlsjnfkpfqhw6pklp0ws8nndyiwxrg4pj04jpwnxf6a38n6")))


;;;
;;; NVIDIA drivers
;;;


(define-public nvidia-driver
  (package
    (name "nvidia-driver")
    (version nvidia-version)
    (source nvidia-source)
    (build-system copy-build-system)
    (arguments
     (list #:modules '((guix build copy-build-system)
                       (guix build utils)
                       (ice-9 ftw)
                       (ice-9 popen)
                       (ice-9 rdelim)
                       (ice-9 regex)
                       (ice-9 textual-ports))
           #:install-plan
           #~`((,#$(match (or (%current-target-system) (%current-system))
                     ("i686-linux" "32")
                     ("x86_64-linux" ".")
                     (_ "."))
                "lib/" #:include-regexp ("^./[^/]+\\.so") #:exclude-regexp ("nvidia_drv\\.so" "libglxserver_nvidia\\.so\\..*"))
               ("." "share/nvidia/" #:include-regexp ("nvidia-application-profiles.*"))
               ("." "share/egl/egl_external_platform.d/" #:include-regexp (".*_nvidia_.*\\.json"))
               ("90-nvidia.rules" "lib/udev/rules.d/")
               ("nvidia-drm-outputclass.conf" "share/x11/xorg.conf.d/")
               ("nvidia-dbus.conf" "share/dbus-1/system.d/")
               ("nvidia-smi.1.gz" "share/man/man1/")
               ("nvidia.icd" "etc/OpenCL/vendors/")
               ("nvidia_drv.so" "lib/xorg/modules/drivers/")
               ("nvidia_icd.json" "share/vulkan/icd.d/")
               ("nvidia_layers.json" "share/vulkan/implicit_layer.d/")
               (,(string-append "libglxserver_nvidia.so." #$version) "lib/xorg/modules/extensions/"))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'strip)
               (add-after 'unpack 'create-misc-files
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Vulkan layer configuration
                   (for-each (lambda (file)
                               (substitute* file
                                 (("lib(GLX|nvidia).*\\.so\\..*" all)
                                  (string-append #$output "/lib/" all))))
                             (scandir "." (lambda (name)
                                            (string-contains name ".json"))))

                   ;; OpenCL vendor ICD configuration
                   (substitute* "nvidia.icd"
                     ((".*" all) (string-append #$output "/lib/" all)))

                   ;; Add udev rules for nvidia
                   (let ((rules "90-nvidia.rules"))
                     (call-with-output-file rules
                       (lambda (port)
                         (put-string port (format #f "~
KERNEL==\"nvidia\", RUN+=\"@sh@ -c '@mknod@ -m 666 /dev/nvidiactl c $$(@grep@ nvidia-frontend /proc/devices | @cut@ -d \\  -f 1) 255'\"
KERNEL==\"nvidia_modeset\", RUN+=\"@sh@ -c '@mknod@ -m 666 /dev/nvidia-modeset c $$(@grep@ nvidia-frontend /proc/devices | @cut@ -d \\  -f 1) 254'\"
KERNEL==\"card*\", SUBSYSTEM==\"drm\", DRIVERS==\"nvidia\", RUN+=\"@sh@ -c '@mknod@ -m 666 /dev/nvidia0 c $$(@grep@ nvidia-frontend /proc/devices | @cut@ -d \\  -f 1) 0'\"
KERNEL==\"nvidia_uvm\", RUN+=\"@sh@ -c '@mknod@ -m 666 /dev/nvidia-uvm c $$(@grep@ nvidia-uvm /proc/devices | @cut@ -d \\  -f 1) 0'\"
KERNEL==\"nvidia_uvm\", RUN+=\"@sh@ -c '@mknod@ -m 666 /dev/nvidia-uvm-tools c $$(@grep@ nvidia-uvm /proc/devices | @cut@ -d \\  -f 1) 0'\"
"))))
                     (substitute* rules
                       (("@\\<(sh|grep|mknod|cut)\\>@" all cmd)
                        (search-input-file inputs (string-append "/bin/" cmd)))))))
               (add-after 'install 'patch-elf
                 (lambda _
                   (let* ((ld.so (string-append #$(this-package-input "glibc")
                                                #$(glibc-dynamic-linker)))
                          (rpath (string-join
                                  (list "$ORIGIN"
                                        (string-append #$output "/lib")
                                        (string-append #$gcc:lib "/lib")
                                        (string-append #$gtk+-2 "/lib")
                                        (string-append #$(this-package-input "atk") "/lib")
                                        (string-append #$(this-package-input "cairo") "/lib")
                                        (string-append #$(this-package-input "gdk-pixbuf") "/lib")
                                        (string-append #$(this-package-input "glib") "/lib")
                                        (string-append #$(this-package-input "glibc") "/lib")
                                        (string-append #$(this-package-input "gtk+") "/lib")
                                        (string-append #$(this-package-input "libdrm") "/lib")
                                        (string-append #$(this-package-input "libx11") "/lib")
                                        (string-append #$(this-package-input "libxext") "/lib")
                                        (string-append #$(this-package-input "mesa") "/lib")
                                        (string-append #$(this-package-input "pango") "/lib")
                                        (string-append #$(this-package-input "wayland") "/lib"))
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
                               (append (find-files #$output  ".*\\.so")
                                       (find-files (string-append #$output "/bin")))))))
               (add-before 'patch-elf 'install-nvidia-smi
                 (lambda _
                   (if (string-match "x86_64-linux"
                        (or #$(%current-target-system) #$(%current-system)))
                     (install-file "nvidia-smi" (string-append #$output "/bin")))))
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
                    (find-files #$output "\\.so"))
                   (symlink (string-append "libglxserver_nvidia.so." #$version)
                            (string-append #$output "/lib/xorg/modules/extensions/" "libglxserver_nvidia.so"))
                   (symlink (string-append "libnvidia-allocator.so." #$version)
                            (string-append #$output "/lib/nvidia-drm_gbm.so" )))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (native-inputs (list patchelf))
    (inputs
     (list `(,gcc "lib")
           atk
           bash-minimal
           cairo
           coreutils
           gdk-pixbuf
           glib
           glibc
           grep
           gtk+
           gtk+-2
           kmod
           libdrm
           libx11
           libxext
           linux-lts
           mesa
           pango
           wayland))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA driver")
    (description
     "This is the evil NVIDIA driver.  Don't forget to add @code{service
nvidia-service-type} to your @file{config.scm}.  Further xorg should be
configured by adding: @code{(modules (cons* nvidia-driver
%default-xorg-modules)) (drivers '(\"nvidia\"))} to @code{xorg-configuration}.
")
    (license
     (license:nonfree
      (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-libs
  (deprecated-package "nvidia-libs" nvidia-driver))


;;;
;;; NVIDIA frimwares
;;;


(define-public nvidia-firmware
  (let ((base nvidia-driver))
    (package
      (inherit base)
      (name "nvidia-firmware")
      (arguments
       (list #:install-plan
             #~'(("firmware" #$(string-append
                                "lib/firmware/nvidia/" (package-version base))))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'strip))))
      (inputs '())
      (native-inputs '()))))


;;;
;;; NVIDIA kernel modules
;;;


(define-public nvidia-module
  (package
    (name "nvidia-module")
    (version nvidia-version)
    (source nvidia-source)
    (build-system linux-module-build-system)
    (arguments
     (list #:linux linux-lts
           #:source-directory "kernel"
           #:tests? #f
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
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
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary NVIDIA kernel modules")
    (description
     "This package provides the evil NVIDIA proprietary kernel modules.")
    (license
     (license:nonfree
      (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-module-open
  (let ((base nvidia-module))
    (package/inherit base
      (name "nvidia-module-open")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ;; NOTE: Kernels compiled with CONFIG_LTO_CLANG_THIN would cause an
         ;; error here.  See also:
         ;; <https://github.com/NVIDIA/open-gpu-kernel-modules/issues/214>
         ;; <https://github.com/llvm/llvm-project/issues/55820>
         ((#:source-directory _) "kernel-open")))
      (home-page "https://github.com/NVIDIA/open-gpu-kernel-modules")
      (synopsis "NVIDIA kernel module")
      (description
       "This package provides NVIDIA open-gpu-kernel-modules.  However, they
are only for the latest GPU architectures Turing and Ampere.  Also they still
require firmware file @code{gsp.bin} to be loaded as well as closed source
userspace tools from the corresponding driver release.")
      (license license-gnu:gpl2))))


;;;
;;; ‘nvidia-settings’ packages
;;;


(define-public nvidia-settings
  (package
    (name "nvidia-settings")
    (version nvidia-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/nvidia-settings")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hplc42115c06cc555cjmw3c9371qn7ibwjpqjybcf6ixfd6lryq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no test suite
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
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
                  libvdpau
                  libx11
                  libxext
                  libxrandr
                  libxv
                  libxxf86vm))
    (synopsis "Nvidia driver control panel")
    (description
     "This package provides Nvidia driver control panel for monitor
configuration, creating application profiles, gpu monitoring and more.")
    (home-page "https://github.com/NVIDIA/nvidia-settings")
    (license license-gnu:gpl2)))


;;;
;;; ‘nvda’ packages
;;;


;; nvda is used as a name because it has the same length as mesa which is
;; required for grafting
(define-public nvda
  (package
    (inherit nvidia-driver)
    (name "nvda")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build union))
           #:builder
           #~(begin
               (use-modules (guix build union)
                            (srfi srfi-1)
                            (ice-9 regex))
               (union-build #$output
                            (list #$(this-package-input "mesa")
                                  #$(this-package-input "nvidia-driver"))
                            #:resolve-collision
                            (lambda (files)
                              (let ((file (if (string-match "nvidia-driver"
                                                            (first files))
                                              (first files)
                                              (last files))))
                                (format #t "chosen ~a ~%" file)
                                file))))))
    (description
     "These are the libraries of the evil NVIDIA driver, packaged in such a
way that you can use the transformation option @code{--with-graft=mesa=nvda}
to use the NVIDIA driver with a package that requires mesa.")
    (inputs (list mesa nvidia-driver))
    (outputs '("out"))))

(define mesa/fake
  (package
    (inherit mesa)
    (replacement nvda)))

(define-public replace-mesa
  (package-input-rewriting `((,mesa . ,mesa/fake))))


;;;
;;; Other packages
;;;


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
                 (lambda _
                   (invoke "sh" #$source "--tar" "xvf"))))
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
