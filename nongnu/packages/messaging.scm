;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2021, 2022 PantherX OS Team <team@pantherx.org>
;;; Copyright © 2022, 2023, 2024, 2025 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Raven Hallsby <karl@hallsby.org>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>

(define-module (nongnu packages messaging)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
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
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define-public element-desktop
  (package
    (name "element-desktop")
    (version "1.11.89")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://packages.riot.im/debian/pool/main/e/" name "/" name "_" version
         "_amd64.deb"))
       (sha256
        (base32 "0riavl8qq59s45xxdk0s1n6i1wymsx1zfdz5dp3ccr8xpzw77abq"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:wrapper-plan
           #~'(("lib/Element/element-desktop" (("out" "/lib/Element"))))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'binary-unpack 'setup-cwd
                 (lambda _
                   (copy-recursively "usr/" ".")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")
                   ;; Fix the .desktop file binary location.
                   (substitute* '("share/applications/element-desktop.desktop")
                     (("/opt/Element/")
                      (string-append #$output "/bin/")))))
               (add-after 'install 'symlink-binary-file
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/Element/element-desktop")
                            (string-append #$output "/bin/element-desktop")))))))
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
    (version "7.45.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://updates.signal.org/desktop/apt/pool/s/" name "/" name "_" version
         "_amd64.deb"))
       (sha256
        (base32 "1587w27mb968dw36mvww21wqxzwr3nhdcp5wkriwlmz1wv235xxm"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:wrapper-plan
           #~'(("lib/Signal/signal-desktop" (("out" "/lib/Signal"))))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'binary-unpack 'setup-cwd
                 (lambda _
                   (copy-recursively "usr/" ".")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")
                   ;; Fix the .desktop file binary location.
                   (substitute* '("share/applications/signal-desktop.desktop")
                     (("/opt/Signal/")
                      (string-append #$output "/bin/")))))
               (add-after 'install 'symlink-binary-file
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/Signal/signal-desktop")
                            (string-append #$output "/bin/signal-desktop")))))))
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
    (version "5.17.5.2543")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.zoom.us/prod/" version "/zoom_x86_64.tar.xz"))
       (file-name (string-append name "-" version "-x86_64.tar.xz"))
       (sha256
        (base32 "06m53d3jrpiq1z5wd7m61lb3w8m8g72iaqx5sixnzn290gyyzgim"))))
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
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "tar" "xvf" source)
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
                     '("QML2_IMPORT_PATH" = ())
                     '("QT_PLUGIN_PATH" = ())
                     '("QT_SCREEN_SCALE_FACTORS" = ())
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
                                        "xcb-util-image"
                                        "xcb-util-keysyms"
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
                                        "xcb-util-image"
                                        "xcb-util-keysyms"
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

(define-public zulip-desktop
  (package
    (name "zulip-desktop")
    (version "5.11.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/zulip/zulip-desktop/releases/download/"
                       "v" version "/Zulip-" version "-x64.tar.xz"))
       (sha256
        (base32
         "0kshajcbajkq5jqygn85x12yy8c8b6rwc5r0flrvris474p1vzp0"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ; TODO: Fails on wrapped binary for NSS libs
      #:substitutable? #f
      #:wrapper-plan
      #~(list "chrome_crashpad_handler" "zulip")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install-wrapper 'install-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/zulip")
                         (string-append bin "/zulip")))))
          (add-after 'install 'create-desktop-file
            (lambda _
              (make-desktop-entry-file
               (string-append #$output "/share/applications/zulip-desktop.desktop")
               #:name "Zulip-Desktop"
               #:type "Application"
               #:generic-name "Zulip Chat"
               #:exec (string-join (list (string-append #$output "/bin/zulip") "%U"))
               #:icon "zulip"
               #:keywords '("zulip")
               #:categories '("Network" "InstantMessaging" "Chat")
               #:terminal #f
               #:startup-notify #t
               #:startup-w-m-class "zulip-desktop"
               #:comment
               '(("en" "Zulip Desktop Client")
                 (#f "Zulip Chat"))))))))
    (synopsis "Zulip Desktop client")
    (supported-systems '("x86_64-linux"))
    (description "Zulip Desktop is a client for the Zulip thread and
conversation platform.  Zulip is designed around conversations that are
labeled with topics, to make communication organized and efficient.  It’s easy
to get an overview of what conversations are happening, and to read one
conversation at a time.")
    (home-page "https://github.com/zulip/zulip-desktop")
    (license license:asl2.0)))
