;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Korytov Pavel <thexcloud@gmail.com>
;;; Copyright © 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>
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

(define-module (nongnu packages anydesk)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses))

(define-public anydesk
  (package
    (name "anydesk")
    (version "6.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.anydesk.com/linux/anydesk-"
                                  version "-amd64.tar.gz"))
              (sha256
               (base32
                "1ai58fsivb8al1279bayl800qavy0kfj40rjhf87g902ap3p4bhh"))))
    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f
       #:strip-binaries? #f             ;; For some reason it breaks the program
       #:patchelf-plan
       `(("anydesk" ("atk"
                     "cairo"
                     "fontconfig"
                     "freetype"
                     "gcc:lib"
                     "gdk-pixbuf"
                     "glib"
                     "glu"
                     "gtk+"
                     "gtkglext"
                     "libice"
                     "libsm"
                     "libx11"
                     "libxcb"
                     "libxdamage"
                     "libxext"
                     "libxfixes"
                     "libxi"
                     "libxkbfile"
                     "libxmu"
                     "libxrandr"
                     "libxrender"
                     "libxsts"
                     "libxt"
                     "mesa"
                     "minizip"
                     "pango"
                     "pangox-compat"
                     "polkit"
                     "polkit-gnome"
                     "pulseaudio")))
       #:install-plan
       `(("anydesk" "/bin/")
         ("polkit-1/com.anydesk.anydesk.policy" "/etc/polkit-1/actions/")
         ("icons/" "/share/icons/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "-xvzf" (assoc-ref inputs "source") "--strip-components" "1")))
         (add-after 'wrap-program 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((desktop-file "anydesk.desktop")
                    (out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (substitute* desktop-file
                 (("^Exec=.*") (string-append "Exec=" out "/bin/anydesk\n"))
                 (("^Icon=.*") "Icon=anydesk\n"))
               (install-file desktop-file applications))
             #t)))))
    (inputs
     `(("atk" ,atk)
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gcc:lib" ,gcc "lib")
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("glu" ,glu)
       ("gtk+" ,gtk+-2)
       ("gtkglext" ,gtkglext)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxkbfile" ,libxkbfile)
       ("libxmu" ,libxmu)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxsts" ,libxtst)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("minizip" ,minizip)
       ("pango" ,pango)
       ("pangox-compat" ,pangox-compat)
       ("polkit" ,polkit)
       ("polkit-gnome" ,polkit-gnome)
       ("pulseaudio" ,pulseaudio)))
    (synopsis  "Remote desktop software")
    (supported-systems '("x86_64-linux"))
    (description "Connect to a computer remotely, be it from the other end of
the office or halfway around the world.  AnyDesk ensures secure and reliable
remote desktop connections for IT professionals and on-the-go individuals alike.")
    (home-page "https://anydesk.com/")
    (license (nonfree "https://anydesk.com/en/terms"))))
