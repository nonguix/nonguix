;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
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
  #:use-module (guix utils)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (nongnu packages linux)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1))

(define-public nvidia-driver
  (package
    (name "nvidia-driver")
    (version "455.45.01")
    (source
     (origin
       (uri (format #f "http://us.download.nvidia.com/XFree86/Linux-x86_64/~a/~a.run"
                    version
                    (format #f "NVIDIA-Linux-x86_64-~a" version)))
       (sha256 (base32 "0lx10g0wh7bhf9a4jvq6cbz4ddcz63c4k8pnlw7m8r9g11z8rp7a"))
       (method url-fetch)
       (file-name (string-append "nvidia-driver-" version "-checkout"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:phases
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

               ;; TODO: What happened with those Xorg files
               ;(install-file "nvidia_drv.so" (string-append out "/lib/xorg/modules/drivers/"))
               ;; TODO: Use version variable
               (install-file "libglxserver_nvidia.so.455.45.01" (string-append out "/lib/xorg/modules/extensions/"))

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
                           (find-files out  ".*\\.so")))

               ;; ------------------------------
               ;; Create short name symbolic links
               (for-each (lambda (file)
                           (let* ((short (regexp-substitute
                                          #f
                                          (string-match "([^/]*\\.so).*" file)
                                          1))
                                  (major (if (or (string=? short "libEGL.so")
                                                 (string=? short "libEGL_nvidia.so")
                                                 (string=? short "libGLX.so")
                                                 (string=? short "libGLX_nvidia.so"))
                                             "0" "1"))
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
               ;; TODO: Use version variable
               (symlink "libglxserver_nvidia.so.455.45.01"
                        (string-append out "/lib/xorg/modules/extensions/" "libglxserver_nvidia.so")))
             #t)))))
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
       ("linux" ,linux)
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
    ;; TODO Use version variable
    (license (license:nonfree "file:///share/doc/nvidia-driver-455.45.01/LICENSE"))))
