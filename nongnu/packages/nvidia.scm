;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (nongnu packages nvidia)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license-gnu:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (nongnu packages linux)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

; Used for closed-source packages
(define nvidia-version "470.86")

; Used for the open-source kernel module package
(define nversion "515.48.07")

(define-public nvidia-driver
  (package
    (name "nvidia-driver")
    (version nvidia-version)
    (source
     (origin
       (uri (format #f "http://us.download.nvidia.com/XFree86/Linux-x86_64/~a/~a.run"
                    version
                    (format #f "NVIDIA-Linux-x86_64-~a" version)))
       (sha256 (base32 "0krwcxc0j19vjnk8sv6mx1lin2rm8hcfhc2hg266846jvcws1dsg"))
       (method url-fetch)
       (file-name (string-append "nvidia-driver-" version "-checkout"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:linux ,linux-lts
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys #:rest r)
             (let ((source (assoc-ref inputs "source")))
               (invoke "sh" source "--extract-only")
               (chdir ,(format #f "NVIDIA-Linux-x86_64-~a" version))
               #t)))
         (replace 'build
           (lambda*  (#:key inputs outputs #:allow-other-keys)
             ;; We cannot use with-directory-excursion, because the install
             ;; phase needs to be in the kernel folder. Otherwise no .ko
             ;; would be installed.
             (chdir "kernel")
             ;; Patch Kbuild
             (substitute* "Kbuild"
               (("/bin/sh") (string-append (assoc-ref inputs "bash-minimal") "/bin/sh")))
             (invoke "make"
                     "-j"
                     (string-append "SYSSRC="
                                    (assoc-ref inputs "linux-module-builder")
                                    "/lib/modules/build")
                     "CC=gcc")
             #t))
         (delete 'check)
         (delete 'strip)
         (add-after 'install 'install-copy
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             (chdir "..")
             (use-modules (ice-9 ftw)
                          (ice-9 regex)
                          (ice-9 textual-ports))
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib"))
                    (bindir (string-append out "/bin"))
                    (etcdir (string-append out "/etc")))
               ;; ------------------------------
               ;; Copy .so files
               (for-each
                (lambda (file)
                  (format #t "Copying '~a'...~%" file)
                  (install-file file libdir))
                (scandir "." (lambda (name)
                               (string-contains name ".so"))))

               (install-file "nvidia_drv.so" (string-append out "/lib/xorg/modules/drivers/"))
               (install-file ,(string-append "libglxserver_nvidia.so." version) (string-append out "/lib/xorg/modules/extensions/"))

               ;; ICD Loader for OpenCL
               (let ((file (string-append etcdir "/OpenCL/vendors/nvidia.icd")))
                 (mkdir-p (string-append etcdir "/OpenCL/vendors/"))
                 (call-with-output-file file
                   (lambda (port)
                     (display (string-append out "/lib/libnvidia-opencl.so.1") port)))
                 (chmod file #o555))

               ;; Add udev rules for nvidia
               (let ((rulesdir (string-append out "/lib/udev/rules.d/"))
                     (rules    (string-append out "/lib/udev/rules.d/90-nvidia.rules"))
                     (sh       (string-append (assoc-ref inputs "bash-minimal") "/bin/sh"))
                     (mknod    (string-append (assoc-ref inputs "coreutils") "/bin/mknod"))
                     (cut     (string-append (assoc-ref inputs "coreutils") "/bin/cut"))
                     (grep     (string-append (assoc-ref inputs "grep") "/bin/grep")))
                 (mkdir-p rulesdir)
                 (call-with-output-file rules
                   (lambda (port)
                     (put-string port
                                 (string-append
                                  "KERNEL==\"nvidia\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidiactl c $$(" grep " nvidia-frontend /proc/devices | " cut " -d \\  -f 1) 255'\"" "\n"
                                  "KERNEL==\"nvidia_modeset\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia-modeset c $$(" grep " nvidia-frontend /proc/devices | " cut " -d \\  -f 1) 254'\"" "\n"
                                  "KERNEL==\"card*\", SUBSYSTEM==\"drm\", DRIVERS==\"nvidia\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia0 c $$(" grep " nvidia-frontend /proc/devices | " cut " -d \\  -f 1) 0'\"" "\n"
                                  "KERNEL==\"nvidia_uvm\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia-uvm c $$(" grep " nvidia-uvm /proc/devices | " cut " -d \\  -f 1) 0'\"" "\n"
                                  "KERNEL==\"nvidia_uvm\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia-uvm-tools c $$(" grep " nvidia-uvm /proc/devices | " cut " -d \\  -f 1) 0'\"" "\n" )))))

               ;; ------------------------------
               ;; Add a file to load nvidia drivers
               (mkdir-p bindir)
               (let ((file (string-append bindir "/nvidia-insmod"))
                     (out (assoc-ref outputs "out"))
                     (moddir (string-append "/lib/modules/" (utsname:release (uname)) "-gnu/extra")))
                 (call-with-output-file file
                   (lambda (port)
                     (put-string port (string-append "#!" (assoc-ref inputs "bash-minimal") "/bin/sh" "\n"
                                                     "modprobe ipmi_devintf"                   "\n"
                                                     "insmod " out moddir "/nvidia.ko"         "\n"
                                                     "insmod " out moddir "/nvidia-modeset.ko" "\n"
                                                     "insmod " out moddir "/nvidia-uvm.ko"     "\n"
                                                     "insmod " out moddir "/nvidia-drm.ko"     "\n"))))
                 (chmod file #o555))
               (let ((file (string-append bindir "/nvidia-rmmod")))
                 (call-with-output-file file
                   (lambda (port)
                     (put-string port (string-append "#!" (assoc-ref inputs "bash-minimal") "/bin/sh" "\n"
                                                     "rmmod " "nvidia-drm"     "\n"
                                                     "rmmod " "nvidia-uvm"     "\n"
                                                     "rmmod " "nvidia-modeset" "\n"
                                                     "rmmod " "nvidia"         "\n"
                                                     "rmmod " "ipmi_devintf"   "\n"))))
                 (chmod file #o555))

               ;; ------------------------------
               ;;  nvidia-smi

               (install-file "nvidia-smi" bindir)

               ;; ------------------------------
               ;; patchelf
               (let* ((libc (assoc-ref inputs "libc"))
                      (ld.so (string-append libc ,(glibc-dynamic-linker)))

                      (out (assoc-ref outputs "out"))
                      (rpath (string-join
                              (list "$ORIGIN"
                                    (string-append out "/lib")
                                    (string-append libc "/lib")
                                    (string-append (assoc-ref inputs "libx11") "/lib")
                                    (string-append (assoc-ref inputs "libxext") "/lib")
                                    (string-append (assoc-ref inputs "pango") "/lib")
                                    (string-append (assoc-ref inputs "gtk+") "/lib")
                                    (string-append (assoc-ref inputs "gtk2") "/lib")
                                    (string-append (assoc-ref inputs "atk") "/lib")
                                    (string-append (assoc-ref inputs "glib") "/lib")
                                    (string-append (assoc-ref inputs "cairo") "/lib")
                                    (string-append (assoc-ref inputs "gdk-pixbuf") "/lib")
                                    (string-append (assoc-ref inputs "wayland") "/lib")
                                    (string-append (assoc-ref inputs "gcc:lib") "/lib"))
                              ":")))
                 (define (patch-elf file)
                   (format #t "Patching ~a ...~%" file)
                   (unless (string-contains file ".so")
                     (invoke "patchelf" "--set-interpreter" ld.so file))
                   (invoke "patchelf" "--set-rpath" rpath file))
                 (for-each (lambda (file)
                             (when (elf-file? file)
                               (patch-elf file)))
                           (find-files out  ".*\\.so"))
                 (patch-elf (string-append bindir "/" "nvidia-smi")))

               ;; ------------------------------
               ;; Create short name symbolic links
               (for-each (lambda (file)
                           (let* ((short (regexp-substitute
                                          #f
                                          (string-match "([^/]*\\.so).*" file)
                                          1))
                                  (major (cond
                                          ((or (string=? short "libGLX.so")
                                               (string=? short "libGLX_nvidia.so")
                                               (string=? short "libEGL_nvidia.so")) "0")
                                          ((string=? short "libGLESv2.so") "2")
                                          (else "1")))
                                  (mid (string-append short "." major))
                                  (short-file (string-append libdir "/" short))
                                  (mid-file (string-append libdir "/" mid)))
                             ;; FIXME the same name, print out warning at least
                             ;; [X] libEGL.so.1.1.0
                             ;; [ ] libEGL.so.435.21
                             (when (not (file-exists? short-file))
                               (format #t "Linking ~a to ~a ...~%" short file)
                               (symlink (basename file) short-file))
                             (when (not (file-exists? mid-file))
                               (format #t "Linking ~a to ~a ...~%" mid file)
                               (symlink (basename file) mid-file))))
                         (find-files libdir "\\.so\\."))
               (symlink ,(string-append "libglxserver_nvidia.so." version)
                        (string-append out "/lib/xorg/modules/extensions/" "libglxserver_nvidia.so")))
             #t)))))
    (supported-systems '("x86_64-linux"))
    (native-inputs
     `(("patchelf" ,patchelf)
       ("perl" ,perl)
       ("python" ,python-2)
       ("which" ,which)
       ("xz" ,xz)))
    (inputs
     `(("atk" ,atk)
       ("bash-minimal" ,bash-minimal)
       ("cairo" ,cairo)
       ("coreutils" ,coreutils)
       ("gcc:lib" ,gcc "lib")
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("grep" ,grep)
       ("gtk+" ,gtk+)
       ("gtk2" ,gtk+-2)
       ("kmod" ,kmod)
       ("libc" ,glibc)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("linux" ,linux-lts)
       ("pango" ,pango)
       ("wayland" ,wayland)))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary Nvidia driver")
    (description "This is the evil Nvidia driver.  Don't forget to add
nvidia-driver to the udev-rules in your config.scm:
@code{(simple-service 'custom-udev-rules udev-service-type (list nvidia-driver))}
Further xorg should be configured by adding:
@code{(modules (cons* nvidia-driver %default-xorg-modules))
(drivers '(\"nvidia\"))} to @code{xorg-configuration}.")
    (license (license:nonfree (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-libs
  (package
    (name "nvidia-libs")
    (version nvidia-version)
    (source
     (origin
       (uri (format #f "http://us.download.nvidia.com/XFree86/Linux-x86_64/~a/~a.run"
                    version
                    (format #f "NVIDIA-Linux-x86_64-~a" version)))
       (sha256 (base32 "0krwcxc0j19vjnk8sv6mx1lin2rm8hcfhc2hg266846jvcws1dsg"))
       (method url-fetch)
       (file-name (string-append "nvidia-driver-" version "-checkout"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys #:rest r)
             (let ((source (assoc-ref inputs "source")))
               (invoke "sh" source "--extract-only")
               (chdir ,(format #f "NVIDIA-Linux-x86_64-~a" version))
               #t)))
         (delete 'build)
         (delete 'check)
         (add-after 'install 'patch-symlink
             (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             (use-modules (ice-9 ftw)
                          (ice-9 regex)
                          (ice-9 textual-ports))
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib"))
                    (bindir (string-append out "/bin"))
                    (etcdir (string-append out "/etc")))
               ;; ------------------------------
               ;; patchelf
               (let* ((libc (assoc-ref inputs "libc"))
                      (ld.so (string-append libc ,(glibc-dynamic-linker)))

                      (out (assoc-ref outputs "out"))
                      (rpath (string-join
                              (list "$ORIGIN"
                                    (string-append out "/lib")
                                    (string-append libc "/lib")
                                    (string-append (assoc-ref inputs "atk") "/lib")
                                    (string-append (assoc-ref inputs "cairo") "/lib")
                                    (string-append (assoc-ref inputs "gcc:lib") "/lib")
                                    (string-append (assoc-ref inputs "gdk-pixbuf") "/lib")
                                    (string-append (assoc-ref inputs "glib") "/lib")
                                    (string-append (assoc-ref inputs "gtk+") "/lib")
                                    (string-append (assoc-ref inputs "gtk2") "/lib")
                                    (string-append (assoc-ref inputs "libx11") "/lib")
                                    (string-append (assoc-ref inputs "libxext") "/lib")
                                    (string-append (assoc-ref inputs "pango") "/lib")
                                    (string-append (assoc-ref inputs "wayland") "/lib"))
                              ":")))
                 (define (patch-elf file)
                   (format #t "Patching ~a ...~%" file)
                   (unless (string-contains file ".so")
                     (invoke "patchelf" "--set-interpreter" ld.so file))
                   (invoke "patchelf" "--set-rpath" rpath file))
                 (for-each (lambda (file)
                             (when (elf-file? file)
                               (patch-elf file)))
                           (find-files out  ".*\\.so")))

               ;; ------------------------------
               ;; Create short name symbolic links
               (for-each (lambda (file)
                           (let* ((short (regexp-substitute
                                          #f

                                          (string-match "([^/]*\\.so).*" file)
                                          1))
                                  (major (cond
                                          ((or (string=? short "libGLX.so")
                                               (string=? short "libGLX_nvidia.so")
                                               (string=? short "libEGL_nvidia.so")) "0")
                                          ((string=? short "libGLESv2.so") "2")
                                          (else "1")))
                                  (mid (string-append short "." major))
                                  (short-file (string-append libdir "/" short))
                                  (mid-file (string-append libdir "/" mid)))
                             ;; FIXME the same name, print out warning at least
                             ;; [X] libEGL.so.1.1.0
                             ;; [ ] libEGL.so.435.21
                             (when (not (file-exists? short-file))
                               (format #t "Linking ~a to ~a ...~%" short file)
                               (symlink (basename file) short-file))
                             (when (not (file-exists? mid-file))
                               (format #t "Linking ~a to ~a ...~%" mid file)
                               (symlink (basename file) mid-file))))
                         (find-files libdir "\\.so\\."))
           #t))))
       #:install-plan
        ,@(match (%current-system)
           ("x86_64-linux" '(`(("." "lib" #:include-regexp ("^./[^/]+\\.so")))))
           ("i686-linux" '(`(("32" "lib" #:include-regexp ("^./[^/]+\\.so")))))
           (_ '()))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (native-inputs
     `(("patchelf" ,patchelf)
       ("perl" ,perl)
       ("python" ,python-2)
       ("which" ,which)
       ("xz" ,xz)))
    (inputs
     `(("atk" ,atk)
       ("cairo" ,cairo)
       ("gcc:lib" ,gcc "lib")
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk2" ,gtk+-2)
       ("libc" ,glibc)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("wayland" ,wayland)))
    (home-page "https://www.nvidia.com")
    (synopsis "Libraries of the proprietary Nvidia driver")
    (description "These are the libraries of the evil Nvidia driver compatible
with the ones usually provided by Mesa.  To use these libraries with
packages that have been compiled with a mesa output, take a look at the nvda
package.")
    (license (license:nonfree (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-module
  (package
    (name "nvidia-module")
    (version nversion)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/open-gpu-kernel-modules")
                    (commit nversion)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bpyq1dn14l7g32aalxy20zsxsay2yzibq8m8chy1byglrsjnqhh"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:linux linux
           #:source-directory "kernel-open"
           #:tests?  #f
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "SYSSRC=" (assoc-ref %build-inputs
                                             "linux-module-builder")
                                  "/lib/modules/build"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fixpath
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (substitute* "kernel-open/Kbuild"
                     (("/bin/sh") (string-append #$bash-minimal "/bin/sh")))))
               (replace 'build
                 (lambda* (#:key make-flags outputs #:allow-other-keys)
                   (apply invoke
                          `("make" "-j"
                            ,@make-flags "modules")))))))
    (inputs (list bash-minimal))
    (home-page "https://github.com/NVIDIA/open-gpu-kernel-modules")
    (synopsis "Nvidia kernel module")
    (description
     "This package provides Nvidia open-gpu-kernel-modules.  However,
they are only for the latest GPU architectures Turing and Ampere.  Also they
still require firmware file @code{gsp.bin} to be loaded as well as closed
source userspace tools from the corresponding driver release.")
    (license license-gnu:gpl2)))

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
                "1lnj5hwmfkzs664fxlhljqy323394s1i7qzlpsjyrpm07sa93bky"))))
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

;; nvda is used as a name because it has the same length as mesa which is
;; required for grafting
(define-public nvda
  (package
    (inherit nvidia-libs)
    (name "nvda")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (guix build union)
                                (srfi srfi-1)
                                (ice-9 regex))
                      (union-build (assoc-ref %outputs "out")
                                   (list (assoc-ref %build-inputs "mesa") (assoc-ref %build-inputs "nvidia-libs"))
                                   #:resolve-collision (lambda (files) (let ((file
                                                                         (if (string-match "nvidia-libs" (first files))
                                                                             (first files)
                                                                             (last files))))
                                                                         (format #t "chosen ~a ~%" file)
                                                                         file)))
                      #t)))
    (description "These are the libraries of the evil Nvidia driver,
packaged in such a way that you can use the transformation option
@code{--with-graft=mesa=nvda} to use the nvidia driver with a package that requires mesa.")
    (inputs
     `(("nvidia-libs" ,nvidia-libs)
       ("mesa" ,mesa)))
    (outputs '("out"))))

(define mesa/fake
  (package
    (inherit mesa)
    (replacement nvda)))

(define-public replace-mesa
  (package-input-rewriting `((,mesa . ,mesa/fake))))
