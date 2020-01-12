;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nongnu packages game-development)
  #:use-module (ice-9 match)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
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
       `(("bin" (".") "bin/")
         (,,(lib) (".") "lib/")
         ("include" (".") "include/")
         ("local" (".") "share/"))
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
