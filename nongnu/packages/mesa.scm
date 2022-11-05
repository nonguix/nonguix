;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (nongnu packages mesa)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages vulkan)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;; Taken from (gnu packages xdisorg) on the current core-updates branch.
;; Remove once merged to Guix's master branch.
(define libdrm-next
  (package
    (inherit libdrm)
    (version "2.4.112")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1zr0hi7k5s7my4q9hyj6ryzg89zyjx24zbqfv3c5rcq9pl87gc00"))))))

;; Copied from (gnu packages gl) without changes, neede for mesa.
(define libva-without-mesa
  ;; Delay to work around circular import problem.
  (delay
    (package
      (inherit libva)
      (name "libva-without-mesa")
      (inputs (fold alist-delete (package-inputs libva)
                    '("mesa" "wayland")))
      (arguments
       (strip-keyword-arguments
        '(#:make-flags)
        (substitute-keyword-arguments (package-arguments libva)
          ((#:configure-flags flags)
           '(list "--disable-glx"))))))))

;; Copied from (gnu packages gl) on the current core-updates branch. Due to
;; the number of changes from master, easier to copy the whole definition than
;; try to modify with an inherit. Once this has been merged to Guix's master
;; branch, modify this to inherit with the latest source version.
(define-public mesa-next
  (package
    (name "mesa")
    (version "22.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://mesa.freedesktop.org/archive/"
                                 "mesa-" version ".tar.xz")
                  (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                 "mesa-" version ".tar.xz")
                  (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
                                 version "/mesa-" version ".tar.xz")))
       (sha256
        (base32
         "1ya8i7kz98h6vdyfjmlpnvcd237a7hhgwjcfh4dngk659yvizq9d"))))
    (build-system meson-build-system)
    (propagated-inputs
     ;; The following are in the Requires.private field of gl.pc.
     (list libdrm-next
           libvdpau
           libx11
           libxdamage
           libxfixes
           libxshmfence
           libxxf86vm
           xorgproto))
    (inputs
     (list elfutils                  ;libelf required for r600 when using llvm
           expat
           (force libva-without-mesa)
           libxml2
           libxrandr
           libxvmc
           ;; Note: update the 'clang' input of mesa-opencl when bumping this.
           llvm
           wayland
           wayland-protocols-next))
    (native-inputs
     (list bison
           flex
           gettext-minimal
           glslang
           pkg-config
           python-libxml2               ;for OpenGL ES 1.1 and 2.0 support
           python-mako
           python-wrapper
           (@ (gnu packages base) which)))
    (outputs '("out" "bin"))
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ("aarch64-linux"
              ;; TODO: Fix svga driver for non-Intel architectures.
              '("-Dgallium-drivers=etnaviv,freedreno,kmsro,lima,nouveau,\
panfrost,r300,r600,swrast,tegra,v3d,vc4,virgl"))
             ("armhf-linux"
              ;; Freedreno FTBFS when built on a 64-bit machine.
              '("-Dgallium-drivers=etnaviv,kmsro,lima,nouveau,panfrost,\
r300,r600,swrast,tegra,v3d,vc4,virgl"))
             ((or "powerpc64le-linux" "powerpc-linux" "riscv64-linux")
              '("-Dgallium-drivers=nouveau,r300,r600,radeonsi,swrast,virgl"))
             (_
              '("-Dgallium-drivers=iris,nouveau,r300,r600,radeonsi,\
svga,swrast,virgl")))
         ;; Enable various optional features.  TODO: opencl requires libclc,
         ;; omx requires libomxil-bellagio
         "-Dplatforms=x11,wayland"
         "-Dglx=dri"               ;Thread Local Storage, improves performance
         ;; "-Dopencl=true"
         ;; "-Domx=true"
         "-Dosmesa=true"
         "-Dgallium-xa=enabled"

         ;; features required by wayland
         "-Dgles2=enabled"
         "-Dgbm=enabled"
         "-Dshared-glapi=enabled"

         ;; Explicitly enable Vulkan on some architectures.
         ,@(match (%current-system)
             ((or "i686-linux" "x86_64-linux")
              '("-Dvulkan-drivers=intel,amd"))
             ((or "powerpc64le-linux" "powerpc-linux")
              '("-Dvulkan-drivers=amd,swrast"))
             ("aarch64-linux"
              '("-Dvulkan-drivers=freedreno,amd,broadcom,swrast"))
             ("riscv64-linux"
              '("-Dvulkan-drivers=amd,swrast"))
             (_
              '("-Dvulkan-drivers=auto")))

         ;; Enable the Vulkan overlay layer on all architectures.
         "-Dvulkan-layers=device-select,overlay"

         ;; Also enable the tests.
         "-Dbuild-tests=true"

         "-Dllvm=enabled")              ; default is x86/x86_64 only

       ;; XXX: 'debugoptimized' causes LTO link failures on some drivers.  The
       ;; documentation recommends using 'release' for performance anyway.
       #:build-type "release"

       #:modules ((ice-9 match)
                  (srfi srfi-1)
                  (guix build utils)
                  (guix build meson-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-test
           (lambda _
             ;; Disable the intel vulkan (anv_state_pool) tests, as they may
             ;; fail in a nondeterministic fashion (see:
             ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/5446).
             (substitute* "src/intel/vulkan/meson.build"
               (("if with_tests")
                "if false"))
             ,@(match (%current-system)
                 ("riscv64-linux"
                  ;; According to the test logs the llvm JIT is not designed
                  ;; for this architecture and the llvmpipe tests all segfault.
                  ;; The same is true for mesa:gallium / osmesa-render.
                  `((substitute* '("src/gallium/drivers/llvmpipe/meson.build"
                                   "src/gallium/targets/osmesa/meson.build")
                      (("if with_tests") "if false"))))
                 ("powerpc64le-linux"
                  ;; Disable some of the llvmpipe tests.
                  `((substitute* "src/gallium/drivers/llvmpipe/lp_test_arit.c"
                      (("0\\.5, ") ""))))
                 ("powerpc-linux"
                  ;; There are some tests which fail specifically on powerpc.
                  `((substitute* '(;; LLVM ERROR: Relocation type not implemented yet!
                                   "src/gallium/drivers/llvmpipe/meson.build"
                                   ;; This is probably a big-endian test failure.
                                   "src/gallium/targets/osmesa/meson.build")
                      (("if with_tests") "if not with_tests"))
                    (substitute* "src/util/tests/format/meson.build"
                      ;; This is definately an endian-ness test failure.
                      (("'u_format_test', ") ""))
                    ;; It is only this portion of the test which fails.
                    (substitute* "src/mesa/main/tests/meson.build"
                      ((".*mesa_formats.*") ""))
                    ;; This test times out and receives SIGTERM.
                    (substitute* "src/amd/common/meson.build"
                      (("and not with_platform_windows") "and with_platform_windows"))))
                 ("i686-linux"
                  ;; This test is known to fail on i686 (see:
                  ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/4091).
                  `((substitute* "src/util/meson.build"
                      ((".*'tests/u_debug_stack_test.cpp',.*") ""))))
                 ("aarch64-linux"
                  ;; The ir3_disasm test segfaults.
                  ;; The simplest way to skip it is to run a different test instead.
                  `((substitute* "src/freedreno/ir3/meson.build"
                      (("disasm\\.c'") "delay.c',\n    link_args: ld_args_build_id"))))
                 ("armhf-linux"
                  ;; Disable some of the llvmpipe tests.
                  `((substitute* "src/gallium/drivers/llvmpipe/meson.build"
                      (("'lp_test_arit', ") ""))))
                 (_
                  '((display "No tests to disable on this architecture.\n"))))))
         ,@(if (string=? (%current-system) "i686-linux")
               '((add-after 'disable-failing-test 'fix-instrfromstring-test
                   (lambda _
                     ;; The instrfromstring test fails on i686, which has been already
                     ;; fixed upstream but not in 22.2.1.
                     ;; TODO: remove on update
                     (substitute* "src/gallium/drivers/r600/sfn/sfn_instr_export.cpp"
                       (("buf\\[6\\]") "buf[6] = {0}")))))
               '())
         (add-before 'configure 'fix-dlopen-libnames
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Remain agnostic to .so.X.Y.Z versions while doing
               ;; the substitutions so we're future-safe.
               (substitute* "src/glx/meson.build"
                 (("-DGL_LIB_NAME=\"lib@0@\\.so\\.@1@\"")
                  (string-append "-DGL_LIB_NAME=\"" out
                                 "/lib/lib@0@.so.@1@\"")))
               (substitute* "src/gbm/backends/dri/gbm_dri.c"
                 (("\"libglapi\\.so")
                  (string-append "\"" out "/lib/libglapi.so")))
               (substitute* "src/gbm/main/backend.c"
                 ;; No need to patch the gbm_gallium_drm.so reference;
                 ;; it's never installed since Mesa removed its
                 ;; egl_gallium support.
                 (("\"gbm_dri\\.so")
                  (string-append "\"" out "/lib/dri/gbm_dri.so"))))))
         (add-after 'install 'split-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (assoc-ref outputs "bin")))
               ;; Not all architectures have the Vulkan overlay control script.
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file (string-append out "/bin/.empty")
                 (const #t))
               (copy-recursively (string-append out "/bin")
                                 (string-append bin "/bin"))
               (delete-file-recursively (string-append out "/bin")))))
         (add-after 'install 'symlinks-instead-of-hard-links
           (lambda* (#:key outputs #:allow-other-keys)
             ;; All the drivers and gallium targets create hard links upon
             ;; installation (search for "hardlink each megadriver instance"
             ;; in the makefiles).  This is no good for us since we'd produce
             ;; nars that contain several copies of these files.  Thus, turn
             ;; them into symlinks, which saves ~124 MiB.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (files  (find-files lib
                                        (lambda (file stat)
                                          (and (string-contains file ".so")
                                               (eq? 'regular
                                                    (stat:type stat))))))
                    (inodes (map (compose stat:ino stat) files)))
               (for-each (lambda (inode)
                           (match (filter-map (match-lambda
                                                ((file ino)
                                                 (and (= ino inode) file)))
                                              (zip files inodes))
                             ((_)
                              #f)
                             ((reference others ..1)
                              (format #t "creating ~a symlinks to '~a'~%"
                                      (length others) reference)
                              (for-each delete-file others)
                              (for-each (lambda (file)
                                          (if (string=? (dirname file)
                                                        (dirname reference))
                                              (symlink (basename reference)
                                                       file)
                                              (symlink reference file)))
                                        others))))
                         (delete-duplicates inodes))))))))
    (home-page "https://mesa3d.org/")
    (synopsis "OpenGL and Vulkan implementations")
    (description "Mesa is a free implementation of the OpenGL and Vulkan
specifications - systems for rendering interactive 3D graphics.  A variety of
device drivers allows Mesa to be used in many different environments ranging
from software emulation to complete hardware acceleration for modern GPUs.")
    (license license:x11)))
