;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021, 2022 PantherX OS Team <team@pantherx.org>
;;; Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>

(define-module (nongnu packages messaging)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define-public element-desktop
  (package
    (name "element-desktop")
    (version "1.11.30")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://packages.riot.im/debian/pool/main/e/" name "/" name "_" version
         "_amd64.deb"))
       (sha256
        (base32 "1grxhz1c7g9r141qjs6jifm8ni15gy73jbsvbl1zzlfwb969xrki"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:patchelf-plan
           #~'(("lib/Element/element-desktop"
                ("alsa-lib" "at-spi2-atk" "at-spi2-core" "atk" "cairo" "cups"
                 "dbus" "expat" "fontconfig-minimal" "gcc" "gdk-pixbuf" "glib"
                 "gtk+" "libdrm" "libnotify" "libsecret" "libx11" "libxcb"
                 "libxcomposite" "libxcursor" "libxdamage" "libxext" "libxfixes"
                 "libxi" "libxkbcommon" "libxkbfile" "libxrandr" "libxrender"
                 "libxtst" "mesa" "nspr" "pango" "zlib")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (invoke "ar" "x" #$source)
                   (invoke "tar" "xvf" "data.tar.xz")
                   (copy-recursively "usr/" ".")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")
                   (delete-file "control.tar.gz")
                   (delete-file "data.tar.xz")
                   (delete-file "debian-binary")
                   ;; Fix the .desktop file binary location.
                   (substitute* '("share/applications/element-desktop.desktop") 
                     (("/opt/Element/")
                      (string-append #$output "/lib/Element/")))))
               (add-after 'install 'symlink-binary-file-and-cleanup
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/Element/element-desktop")
                            (string-append #$output "/bin/element-desktop"))))
               (add-after 'install 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/Element/element-desktop")
                     `("FONTCONFIG_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                           #$output)
                          ":")))
                     `("LD_LIBRARY_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "nss") "/lib/nss")
                           (string-append #$(this-package-input "eudev") "/lib")
                           (string-append #$(this-package-input "gcc") "/lib")
                           (string-append #$(this-package-input "mesa") "/lib")
                           (string-append #$(this-package-input "libxkbfile") "/lib")
                           (string-append #$(this-package-input "zlib") "/lib")
                           (string-append #$(this-package-input "libsecret") "/lib")
                           (string-append #$(this-package-input "sqlcipher") "/lib")
                           (string-append #$(this-package-input "libnotify") "/lib")
                           (string-append #$output "/lib/Element")
                           #$output)
                          ":")))))))))
    (native-inputs (list tar))
    (inputs
     (list alsa-lib
           at-spi2-atk
           at-spi2-core
           atk
           cairo
           cups
           dbus
           eudev
           expat
           fontconfig
           `(,gcc "lib")
           glib
           gtk+
           libdrm
           libnotify
           librsvg
           libsecret
           libx11
           libxcb
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxfixes
           libxi
           libxkbcommon
           libxkbfile
           libxrandr
           libxrender
           libxtst
           mesa
           nspr
           nss
           pango
           sqlcipher
           zlib))
    (home-page "https://github.com/vector-im/element-desktop")
    (synopsis "Matrix collaboration client for desktop")
    (description "Element Desktop is a Matrix client for desktop with Element Web at
its core.")
    ;; not working?
    (properties
     '((release-monitoring-url . "https://github.com/vector-im/element-desktop/releases")))
    (license license:asl2.0)))

(define-public signal-desktop
  (package
    (name "signal-desktop")
    (version "6.14.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://updates.signal.org/desktop/apt/pool/main/s/" name "/" name "_" version
         "_amd64.deb"))
       (sha256
        (base32 "1gfvyw094wg6ch5bn69gvd8304nq22aiqc6jvvpykizpxg6flbvp"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:patchelf-plan
           #~'(("lib/Signal/signal-desktop"
                ("alsa-lib" "at-spi2-atk" "at-spi2-core" "atk" "cairo" "cups"
                 "dbus" "expat" "fontconfig-minimal" "gcc" "gdk-pixbuf" "glib"
                 "gtk+" "libdrm" "libsecret" "libx11" "libxcb" "libxcomposite"
                 "libxcursor" "libxdamage" "libxext" "libxfixes" "libxi"
                 "libxkbcommon" "libxkbfile" "libxrandr" "libxshmfence" "libxtst"
                 "mesa" "nspr" "pango" "pulseaudio" "zlib")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (invoke "ar" "x" #$source)
                   (invoke "tar" "xvf" "data.tar.xz")
                   (copy-recursively "usr/" ".")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")
                   (delete-file "control.tar.gz")
                   (delete-file "data.tar.xz")
                   (delete-file "debian-binary")
                   (delete-file "environment-variables")
                   ;; Fix the .desktop file binary location.
                   (substitute* '("share/applications/signal-desktop.desktop") 
                     (("/opt/Signal/")
                      (string-append #$output "/lib/Signal/")))))
               (add-after 'install 'symlink-binary-file-and-cleanup
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/Signal/signal-desktop")
                            (string-append #$output "/bin/signal-desktop"))))
               (add-after 'install 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/Signal/signal-desktop")
                     `("FONTCONFIG_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                           #$output)
                          ":")))
                     `("LD_LIBRARY_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "nss") "/lib/nss")
                           (string-append #$(this-package-input "eudev") "/lib")
                           (string-append #$(this-package-input "gcc") "/lib")
                           (string-append #$(this-package-input "mesa") "/lib")
                           (string-append #$(this-package-input "libxkbfile") "/lib")
                           (string-append #$(this-package-input "pulseaudio") "/lib")
                           (string-append #$(this-package-input "zlib") "/lib")
                           (string-append #$(this-package-input "libsecret") "/lib")
                           (string-append #$output "/lib/Signal")
                           #$output)
                          ":")))))))))
    (native-inputs (list tar))
    (inputs (list alsa-lib
                  at-spi2-atk
                  at-spi2-core
                  atk
                  cairo
                  cups
                  dbus
                  eudev
                  expat
                  fontconfig
                  `(,gcc "lib")
                  glib
                  gtk+
                  libdrm
                  librsvg
                  libsecret
                  libx11
                  libxcb
                  libxcomposite
                  libxdamage
                  libxext
                  libxfixes
                  libxkbcommon
                  libxkbfile
                  libxrandr
                  libxshmfence
                  mesa
                  nspr
                  nss
                  pango
                  pulseaudio
                  zlib))
    (home-page "https://signal.org/")
    (synopsis "Private messenger using the Signal protocol")
    (description "Signal Desktop is an Electron application that links with Signal on Android
or iOS.")
    ;; doesn't work?
    (properties
     '((release-monitoring-url . "https://github.com/signalapp/Signal-Desktop/releases")))
    (license license:agpl3)))

(define-public zoom
  (package
    (name "zoom")
    (version "5.13.11.1288")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.zoom.us/prod/" version "/zoom_x86_64.tar.xz"))
       (file-name (string-append name "-" version "-x86_64.tar.xz"))
       (sha256
        (base32 "1ngj15j9d7i0z4d0wraziyv75whp0digh8rg1jzfmi0ws3ir2d6b"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:patchelf-plan
           ;; Note: it seems like some (all?) of these only do anything in
           ;; LD_LIBRARY_PATH, or at least needed there as well.
           #~(let ((libs '("alsa-lib"
                           "at-spi2-atk"
                           "at-spi2-core"
                           "atk"
                           "cairo"
                           "cups"
                           "dbus"
                           "eudev"
                           "expat"
                           "fontconfig-minimal"
                           "gcc"
                           "glib"
                           "gtk+"
                           "libdrm"
                           "libx11"
                           "libxcb"
                           "libxcomposite"
                           "libxcursor"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxi"
                           "libxkbcommon"
                           "libxkbfile"
                           "libxrandr"
                           "libxshmfence"
                           "libxtst"
                           "mesa"
                           "nspr"
                           "pango"
                           "pulseaudio"
                           "xcb-util-image"
                           "xcb-util-keysyms"
                           "zlib")))
               `(("lib/zoom/ZoomLauncher"
                 ,libs)
                ("lib/zoom/zoom"
                 ,libs)
                ("lib/zoom/zopen"
                 ,libs)
                ("lib/zoom/aomhost"
                 ,libs)))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (invoke "tar" "xvf" #$source)
                   ;; Use the more standard lib directory for everything.
                   (mkdir-p "lib")
                   (rename-file "zoom/" "lib/zoom")))
               (add-after 'install 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/zoom/zopen")
                     `("LD_LIBRARY_PATH" prefix
                       ,(list #$@(map (lambda (pkg)
                                        (file-append (this-package-input pkg) "/lib"))
                                      '("fontconfig-minimal"
                                        "freetype"
                                        "gcc"
                                        "glib"
                                        "libxcomposite"
                                        "libxdamage"
                                        "libxkbcommon"
                                        "libxkbfile"
                                        "libxrandr"
                                        "libxrender"
                                        "zlib")))))
                   (wrap-program (string-append #$output "/lib/zoom/zoom")
                     `("FONTCONFIG_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                           #$output)
                          ":")))
                     `("LD_LIBRARY_PATH" prefix
                       ,(list (string-append #$(this-package-input "nss") "/lib/nss")
                              #$@(map (lambda (pkg)
                                        (file-append (this-package-input pkg) "/lib"))
                                      ;; TODO: Reuse this long list as it is
                                      ;; needed for aomhost.  Or perhaps
                                      ;; aomhost has a shorter needed list,
                                      ;; but untested.
                                      '("alsa-lib"
                                        "atk"
                                        "at-spi2-atk"
                                        "at-spi2-core"
                                        "cairo"
                                        "cups"
                                        "dbus"
                                        "eudev"
                                        "expat"
                                        "gcc"
                                        "glib"
                                        "mesa"
                                        "mit-krb5"
                                        "nspr"
                                        "libxcb"
                                        "libxcomposite"
                                        "libxdamage"
                                        "libxext"
                                        "libxkbcommon"
                                        "libxkbfile"
                                        "libxrandr"
                                        "libxshmfence"
                                        "pango"
                                        "pulseaudio"
                                        "xcb-util"
                                        "xcb-util-wm"
                                        "xcb-util-renderutil"
                                        "zlib")))))
                   (wrap-program (string-append #$output "/lib/zoom/aomhost")
                     `("FONTCONFIG_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                           #$output)
                          ":")))
                     `("LD_LIBRARY_PATH" prefix
                       ,(list (string-append #$(this-package-input "nss") "/lib/nss")
                              #$@(map (lambda (pkg)
                                        (file-append (this-package-input pkg) "/lib"))
                                      '("alsa-lib"
                                        "atk"
                                        "at-spi2-atk"
                                        "at-spi2-core"
                                        "cairo"
                                        "cups"
                                        "dbus"
                                        "eudev"
                                        "expat"
                                        "gcc"
                                        "glib"
                                        "mesa"
                                        "mit-krb5"
                                        "nspr"
                                        "libxcb"
                                        "libxcomposite"
                                        "libxdamage"
                                        "libxext"
                                        "libxkbcommon"
                                        "libxkbfile"
                                        "libxrandr"
                                        "libxshmfence"
                                        "pango"
                                        "pulseaudio"
                                        "xcb-util"
                                        "xcb-util-wm"
                                        "xcb-util-renderutil"
                                        "zlib")))))))
               (add-after 'wrap-where-patchelf-does-not-work 'rename-binary
                 ;; IPC (for single sign-on and handling links) fails if the
                 ;; name does not end in "zoom," so rename the real binary.
                 ;; Thanks to the Nix packagers for figuring this out.
                 (lambda _
                   (rename-file (string-append #$output "/lib/zoom/.zoom-real")
                                (string-append #$output "/lib/zoom/.zoom"))
                   (substitute* (string-append #$output "/lib/zoom/zoom")
                     (("zoom-real")
                      "zoom"))))
               (add-after 'rename-binary 'symlink-binaries
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/zoom/aomhost")
                            (string-append #$output "/bin/aomhost"))
                   (symlink (string-append #$output "/lib/zoom/zoom")
                            (string-append #$output "/bin/zoom"))
                   (symlink (string-append #$output "/lib/zoom/zopen")
                            (string-append #$output "/bin/zopen"))
                   (symlink (string-append #$output "/lib/zoom/ZoomLauncher")
                            (string-append #$output "/bin/ZoomLauncher"))))
               (add-after 'symlink-binaries 'create-desktop-file
                 (lambda _
                   (let ((apps (string-append #$output "/share/applications")))
                     (mkdir-p apps)
                     (make-desktop-entry-file
                      (string-append apps "/zoom.desktop")
                      #:name "Zoom"
                      #:generic-name "Zoom Client for Linux"
                      #:exec (string-append #$output "/bin/ZoomLauncher %U")
                      #:mime-type (list
                                   "x-scheme-handler/zoommtg"
                                   "x-scheme-handler/zoomus"
                                   "x-scheme-handler/tel"
                                   "x-scheme-handler/callto"
                                   "x-scheme-handler/zoomphonecall"
                                   "application/x-zoom")
                      #:categories '("Network" "InstantMessaging"
                                     "VideoConference" "Telephony")
                      #:startup-w-m-class "zoom"
                      #:comment
                      '(("en" "Zoom Video Conference")
                        (#f "Zoom Video Conference")))))))))
    (native-inputs (list tar))
    (inputs (list alsa-lib
                  at-spi2-atk
                  at-spi2-core
                  atk
                  bash-minimal
                  cairo
                  cups
                  dbus
                  eudev
                  expat
                  fontconfig
                  freetype
                  `(,gcc "lib")
                  glib
                  gtk+
                  libdrm
                  librsvg
                  libx11
                  libxcb
                  libxcomposite
                  libxdamage
                  libxext
                  libxfixes
                  libxkbcommon
                  libxkbfile
                  libxrandr
                  libxrender
                  libxshmfence
                  mesa
                  mit-krb5
                  nspr
                  nss
                  pango
                  pulseaudio
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-renderutil
                  xcb-util-wm
                  zlib))
    (home-page "https://zoom.us/")
    (synopsis "Video conference client")
    (description "The Zoom video conferencing and messaging client.  Zoom must be run via an
app launcher to use its .desktop file, or with @code{ZoomLauncher}.")
    (license (license:nonfree "https://explore.zoom.us/en/terms/"))))
