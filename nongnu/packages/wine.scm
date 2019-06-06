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

(define-module (nongnu packages wine)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages wine))

(define-public winetricks
  (package
    (name "winetricks")
    (version "20181203")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Winetricks/winetricks")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1as5h5xibpplm619b1i73g974p96q2jnd7fqm28xj3zkqy7qjdm3"))))
    (build-system gnu-build-system)
    (inputs
     `(("wget" ,wget)
       ("cabextract" ,cabextract)
       ("p7zip" ,p7zip)
       ("unzip" ,unzip)
       ;; ("unrar" ,unrar) ; TODO: Include unrar?  It is referenced in the source.
       ("zenity" ,zenity)))
    (arguments
     `(#:tests? #f
       ;; TODO: Checks need bashate, shellcheck (in Guix), and checkbashisms.
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-zenity-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((zenity (assoc-ref inputs "zenity"))
                    (zenity-bin (string-append zenity "/bin/zenity")))
               (substitute* "src/winetricks"
                 ;; TODO: There might need more command substitution needed, e.g. wget, 7z, etc.
                 ;; TODO: Add coreutils to the input?
                 (("command -v zenity") (string-append "command -v " zenity-bin))
                 (("WINETRICKS_GUI=zenity") (string-append "WINETRICKS_GUI=" zenity-bin))
                 (("WINETRICKS_GUI_VERSION=\"\\$\\(zenity") (string-append "WINETRICKS_GUI_VERSION=\"$(" zenity-bin)))))))))
    (home-page "https://github.com/Winetricks/winetricks")
    (synopsis "Easy way to work around problems in Wine")
    (description "Winetricks is an easy way to work around problems in Wine.
It has a menu of supported games/apps for which it can do all the workarounds
automatically.  It also allows the installation of missing nonfree DLLs and
tweaking of various Wine settings.")
    (license license:lgpl2.1)))
