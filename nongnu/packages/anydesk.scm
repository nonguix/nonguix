;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021 Korytov Pavel <thexcloud@gmail.com>
;;; Copyright © 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>

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
    (version "6.3.2-1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.anydesk.com/linux/anydesk_"
                                  version "_amd64.deb"))
              (sha256
               (base32
                "13b5ab4a889vz39d36f45mhv3mlaxb305wsh3plk3dbjcrkkkirb"))))
    (build-system binary-build-system)
    (arguments
     `(#:validate-runpath? #f
       #:strip-binaries? #f             ;; For some reason it breaks the program
       #:patchelf-plan
       `(("usr/bin/anydesk" ("atk"
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
       `(("usr/bin/anydesk" "/bin/")
         ("usr/share/polkit-1/actions/com.anydesk.anydesk.policy" "/etc/polkit-1/actions/")
         ("usr/share/icons" "/share/icons/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'binary-unpack
           (lambda* (#:key source #:allow-other-keys)
             (let* ((files (filter (lambda (f)
                          (not (string=? (basename f) "environment-variables")))
                        (find-files (getcwd))))
         (binary-file (car files)))
    (when (= 1 (length files))
      (mkdir "binary")
      (chdir "binary")
      (invoke "ar" "x" binary-file)
      (invoke "tar" "xvf" "data.tar.gz")
      (invoke "rm" "-rfv" "control.tar.gz"
          "data.tar.gz"
          binary-file
          "debian-binary")))))
         (add-after 'install 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((desktop-file "usr/share/applications/anydesk.desktop")
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
