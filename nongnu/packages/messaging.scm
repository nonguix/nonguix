;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
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

(define-module (nongnu packages messaging)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk))

(define-public skype
  (package
    (name "skype")
    (version "8.66.0.74-1")
    (source
     (origin
           (method url-fetch)
           (uri (string-append "https://repo.skype.com/rpm/stable/skypeforlinux_"
                                version ".x86_64.rpm"))
           (sha256
            (base32 "1cv42d5pv2d7ydr2wqicp2kdbn9wi0mrsinziwdm5jx6nvr8ij6k"))))
    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f ;fails on shipped .so files
       #:patchelf-plan
       `(("share/skypeforlinux/skypeforlinux"
          ("alsa-lib" "atk" "at-spi2-atk" "at-spi2-core" "cairo" "cups" "dbus"
           "expat" "gcc:lib" "gdk-pixbuf" "glib" "gtk+" "libx11" "libxcb"
           "libxcomposite" "libxcursor" "libxdamage" "libxext" "libxfixes"
           "libxi" "libxrandr" "libxrender" "libxscrnsaver" "libxtst"
           "nspr" "nss" "out" "pango"))
         ("share/skypeforlinux/libGLESv2.so"
           ("libx11" "libxcb" "gcc:lib"))
         ("share/skypeforlinux/libffmpeg.so"
           ("gcc:lib"))
         ("share/skypeforlinux/swiftshader/libGLESv2.so"
           ("gcc:lib"))
         ("share/skypeforlinux/swiftshader/libEGL.so"
           ("gcc:lib"))
         ("share/skypeforlinux/libEGL.so"
           ("gcc:lib"))
         ("share/skypeforlinux/libvk_swiftshader.so"
           ("gcc:lib")))
       #:install-plan
       `(("share/" "share")
         ("bin/" "bin")
         ("share/skypeforlinux/swiftshader/libEGL.so" "lib/swiftshader/")
         ("share/skypeforlinux/swiftshader/libGLESv2.so" "lib/swiftshader/")
         ("share/skypeforlinux/libffmpeg.so" "lib/")
         ("share/skypeforlinux/libGLESv2.so" "lib/")
         ("share/skypeforlinux/libvk_swiftshader.so" "lib/")
         ("share/skypeforlinux/libEGL.so" "lib/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((source (assoc-ref inputs "source")))
             (system (format #f "rpm2cpio ~a | cpio -idmv"
                             source))
             (chdir "usr")
             (mkdir-p "lib/swiftshader/")
             #t))))))
    (native-inputs
     `(("cpio" ,cpio)
       ("rpm" ,rpm)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("atk" ,atk)
       ("at-spi2-atk" ,at-spi2-atk)
       ("at-spi2-core" ,at-spi2-core)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("dbus" ,dbus)
       ("expat" ,expat)
       ("gcc:lib" ,gcc "lib")
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxtst" ,libxtst)
       ("nspr" ,nspr)
       ("nss" ,nss)
       ("pango" ,pango)))
    (home-page "https://www.skype.com")
    (synopsis "Video chat")
    (description "Skype is a video chat tool from Microsoft")
    (supported-systems '("x86_64-linux"))
    (license #f)))
