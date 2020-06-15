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
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
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

(define-public dxvk-1.7 ; Upstream Guix dxvk does not build anymore because of missing mingw compiler.
  (package
    (name "dxvk")
    (version "1.7")
    (home-page "https://github.com/doitsujin/dxvk/")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/doitsujin/dxvk/releases/download/v"
                    version "/dxvk-" version ".tar.gz") )
              (sha256
               (base32
                "18f7lj6b08abywidsq3s98kiwwn1jbbjzg7clm8bs93cj0wq5mv7"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(,,@(if (string=? (or (%current-target-system) (%current-system))
                           "x86_64-linux")
               ''("x64" "share/dxvk/lib")
               '())
         ("x32" ,,(if (string=? (or (%current-target-system) (%current-system))
                                "i686-linux")
                       "share/dxvk/lib"
                       "share/dxvk/lib32"))
         ("setup_dxvk.sh" "bin/setup_dxvk"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-setup
           (lambda* (#:key inputs outputs system #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libs "../share/dxvk")
                    (wine (assoc-ref inputs "wine")))
               (substitute* (string-append out "/bin/setup_dxvk")
                 (("wine=\"wine\"")
                  (string-append "wine=" wine "/bin/wine"))
                 (("wine64=\"wine64\"")
                  (string-append "wine64=" wine "/bin/wine64"))
                 (("wineboot=\"wineboot\"")
                  (string-append "wineboot=" wine "/bin/wineboot"))
                 (("\"\\$wine_path/\\$wine\"")
                  "\"$wine_path/wine\"")
                 (("x32") (if (string=? system "x86_64-linux")
                              (string-append libs "/lib32")
                              (string-append libs "/lib")))
                 (("x64") (string-append libs "/lib")))))))))
    (inputs
     `(("wine" ,(match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux" wine64-staging)
                  (_ wine-staging)))))
    (synopsis "Vulkan-based D3D9, D3D10 and D3D11 implementation for Wine")
    (description "A Vulkan-based translation layer for Direct3D 9/10/11 which
allows running complex 3D applications with high performance using Wine.

Use @command{setup_dxvk} to install the required libraries to a Wine prefix.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:zlib)))
