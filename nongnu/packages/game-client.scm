;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 pkill-9
;;; Copyright © 2020, 2021 ison <ison@airmail.cc>
;;; Copyright © 2021 pineapples
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Kozo <kozodev@runbox.com>
;;; Copyright © 2021, 2022, 2023, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Elijah Malaby
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>

(define-module (nongnu packages game-client)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lsof)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define heroic-client
  (package
    (name "heroic-client")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Heroic-Games-Launcher/"
                           "HeroicGamesLauncher/releases/download/v"
                           version "/Heroic-" version "-linux-amd64.deb"))
       (sha256
        (base32
         "1w40wyjv9xnq9ip7lapr52adw89vhsi8a47skk04dqx8s5p85aq1"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:wrapper-plan
           #~'(("lib/Heroic/heroic" (("out" "/lib/Heroic"))))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'binary-unpack 'setup-cwd
                 (lambda _
                   (copy-recursively "usr/" ".")
                   ;; Use the more standard lib directory for everything.
                   (rename-file "opt/" "lib")
                   ;; Remove unneeded files.
                   (delete-file-recursively "usr")))
               ;; Fix the .desktop file "Exec" line to just be "heroic" in
               ;; order for this desktop file to be useful to launch heroic in
               ;; the container (heroic package) as well.
               (add-after 'patch-dot-desktop-files 'fix-desktop-file
                 (lambda _
                   (substitute*
                       (string-append #$output "/share/applications/heroic.desktop")
                     (("Exec=.*/heroic") "Exec=heroic"))))
               (delete 'patch-dot-desktop-files)
               (add-after 'install 'symlink-binary-file
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/Heroic/heroic")
                            (string-append #$output "/bin/heroic")))))))
    (home-page "https://heroicgameslauncher.com")
    (synopsis "Native GOG, Amazon and Epic Games Launcher")
    (description "Heroic is an Open Source Game Launcher.  Right now it supports launching
games from the Epic Games Store using Legendary, GOG Games using our custom
implementation with gogdl and Amazon Games using Nile.")
    (license license:gpl3)))

(define steam-client
  (package
    (name "steam-client")
    (version "1.0.0.78")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.steampowered.com/steam/archive/precise/steam_"
                           version ".tar.gz"))
       (sha256
        (base32
         "0390qy8vy7sx35hxl51yrbk6mvdz1vvpy96v07qva4bjbmsmjhhh"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:validate-runpath? #f ; Looks for bin/steam which doesn't exist.
       #:make-flags
       (list "PREFIX=" (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; Patch Makefile so it creates links to the store rather than /lib.
         (add-after 'unpack 'patch-makefile
           (lambda _
             (substitute* "Makefile"
               (("-fns ")
                "-fns $(DESTDIR)"))))
         (add-after 'unpack 'patch-startscript
           (lambda _
             (substitute* "bin_steam.sh"
               (("/usr") (assoc-ref %outputs "out")))))
         (add-after 'patch-dot-desktop-files 'patch-desktop-file
           (lambda _
             (let ((path (string-append (assoc-ref %outputs "out")
                                        "/share/applications/")))
               (substitute* (string-append path "steam.desktop")
                 (("Exec=.*/steam") "Exec=steam"))
               (copy-file (string-append path "steam.desktop")
                          (string-append path "steam-asound32.desktop"))
               (substitute* (string-append path "steam-asound32.desktop")
                 (("Exec=steam %U") "Exec=steam %U -- --asound32")
                 (("Name=Steam") "Name=Steam (32-bit ALSA)")))))
         (delete 'patch-dot-desktop-files)
         ;; Steamdeps installs missing packages, which doesn't work with Guix.
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out")))
               (delete-file (string-append out "/lib/steam/bin_steamdeps.py"))
               (delete-file (string-append out "/bin/steamdeps"))))))))
    (home-page "https://store.steampowered.com")
    (synopsis "Digital distribution platform for managing and playing games")
    (description "Steam is a digital software distribution platform created by Valve.")
    (license (license:nonfree "file:///share/doc/steam/steam_subscriber_agreement.txt"))))

(define steam-client-libs
  `(("at-spi2-core" ,at-spi2-core)      ; Required (often) for SteamVR interface.
    ("bash" ,bash)                      ; Required for steam startup.
    ("coreutils" ,coreutils)
    ("diffutils" ,diffutils)
    ("dbus-glib" ,dbus-glib)            ; Required for steam browser.
    ("elfutils" ,elfutils)              ; Required for capturing library dependencies in pv.
    ("eudev" ,eudev)                    ; Required for steamwebhelper/heavy runtime.
    ("fontconfig" ,fontconfig)          ; Required for steam client.
    ("file" ,file)                      ; Used for steam installation.
    ("find" ,findutils)                 ; Required at least for some logging.
    ("font-google-noto" ,font-google-noto) ; Not required but to match following fonts.
    ;; These next three fonts are to cover emoji and Chinese/Japanese/Korean
    ;; and related scripts.
    ("font-google-noto-emoji" ,font-google-noto-emoji)
    ("font-google-noto-sans-cjk" ,font-google-noto-sans-cjk)
    ("font-google-noto-serif-cjk" ,font-google-noto-serif-cjk)
    ("freetype" ,freetype)              ; Required for steam login.
    ("gawk" ,gawk)
    ("gdk-pixbuf" ,gdk-pixbuf)          ; Required for steam tray icon.
    ;; Required for steam startup; use newer version for better compatibility
    ;; with some games like Dwarf Fortress.
    ("gcc:lib" ,gcc-14 "lib")
    ("grep" ,grep)
    ("libbsd" ,libbsd)
    ("libcap" ,libcap)                  ; Required for SteamVR, but needs pkexec too.
    ("libusb" ,libusb)                  ; Required for SteamVR.
    ("libva" ,libva)                    ; Required for hardware video encoding/decoding.
    ("libvdpau" ,libvdpau)              ; Required for hardware video encoding/decoding.
    ("libvdpau-va-gl" ,libvdpau-va-gl)  ; Additional VDPAU support.
    ("llvm" ,llvm-for-mesa)             ; Required for mesa.
    ("lsof" ,lsof)                      ; Required for some friend's list actions.
    ("mesa" ,mesa)                      ; Required for steam startup.
    ("nss-certs" ,nss-certs)            ; Required for steam login.
    ("pciutils" ,pciutils)              ; Tries to run lspci at steam startup.
    ("procps" ,procps)
    ("sed" ,sed)
    ("tar" ,tar)
    ("usbutils" ,usbutils)              ; Required for SteamVR.
    ("util-linux" ,util-linux)          ; Required for steam login.
    ("wayland" ,wayland)                ; Required for mesa vulkan (e.g. libvulkan_radeon).
    ("xdg-user-dirs" ,xdg-user-dirs)    ; Suppress warning of missing xdg-user-dir.
    ("flatpak-xdg-utils" ,flatpak-xdg-utils)
    ("xz" ,xz)
    ("zenity" ,zenity)))                ; Required for progress dialogs.

(define steam-gameruntime-libs
  `(("alsa-lib" ,alsa-lib)              ; Required for audio in most games.
    ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio") ; Required for audio in most games.
    ("font-dejavu" ,font-dejavu)
    ("font-liberation" ,font-liberation)
    ("imgui" ,imgui-1.86)               ; Required for MangoHud.
    ("mangohud" ,mangohud)
    ("openal" ,openal)                  ; Prevents corrupt audio in Crypt of the Necrodancer.
    ("pulseaudio" ,pulseaudio)          ; Prevents corrupt audio in Sven Coop.
    ("python" ,python)                  ; Required for KillingFloor2 and Wreckfest.
    ("spdlog" ,spdlog)))                ; Required for MangoHud.

(define steam-container-libs
  (append steam-client-libs
          steam-gameruntime-libs
          fhs-min-libs))

(define steam-nvidia-container-libs
  (modify-inputs steam-container-libs
    (replace "mesa" nvda)))

(define heroic-extra-client-libs
  `(("curl" ,curl)                      ; Required for Heroic to download e.g. Wine.
    ("which" ,which)                    ; Heroic complains about trying to use which (though works).
    ("gtk+" ,gtk+)))                    ; Required for Heroic interface (gtk filechooser schema).

(define steam-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union steam-container-libs
                    #:name "fhs-union-64")
         (fhs-union steam-container-libs
                    #:name "fhs-union-32"
                    #:system "i686-linux"))))

(define steam-ld.so.cache
  (ld.so.conf->ld.so.cache steam-ld.so.conf))

(define steam-nvidia-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union steam-nvidia-container-libs
                    #:name "fhs-union-64")
         (fhs-union steam-nvidia-container-libs
                    #:name "fhs-union-32"
                    #:system "i686-linux"))))

(define steam-nvidia-ld.so.cache
  (ld.so.conf->ld.so.cache steam-nvidia-ld.so.conf))

(define-public steam-container
  (nonguix-container
   (name "steam")
   (wrap-package steam-client)
   (run "/bin/steam")
   (ld.so.conf steam-ld.so.conf)
   (ld.so.cache steam-ld.so.cache)
   (union64
    (fhs-union steam-container-libs
               #:name "fhs-union-64"))
   (union32
    (fhs-union steam-container-libs
               #:name "fhs-union-32"
               #:system "i686-linux"))
   (link-files '("share"))
   (description "Steam is a digital software distribution platform created by
Valve.  This package provides a script for launching Steam in a Guix container
which will use the directory @file{$HOME/.local/share/guix-sandbox-home} where
all games will be installed.")))

(define-public steam-nvidia-container
  (nonguix-container
   (inherit steam-container)
   (name "steam-nvidia")
   ;; Steam's .desktop files expect a "steam" executable, so provide that.
   (binary-name "steam")
   (ld.so.conf steam-nvidia-ld.so.conf)
   (ld.so.cache steam-nvidia-ld.so.cache)
   (union64
    (fhs-union steam-nvidia-container-libs
               #:name "fhs-union-64"))
   (union32
    (fhs-union steam-nvidia-container-libs
               #:name "fhs-union-32"
               #:system "i686-linux"))
   (preserved-env %nvidia-environment-variable-regexps)))

(define-public steam (nonguix-container->package steam-container))
(define-public steam-nvidia (nonguix-container->package steam-nvidia-container))

(define-public heroic-container
  (nonguix-container
   (name "heroic")
   (wrap-package heroic-client)
   (run "/bin/heroic")
   (ld.so.conf steam-ld.so.conf)
   (ld.so.cache steam-ld.so.cache)
   ;; TODO: Probably can remove some of the packages from these lists, at
   ;; least changing the client libraries as Heroic is rather different from
   ;; Steam.  However, a good number will be needed to run games anyway.  A
   ;; better separation and testing in Steam as well would be helpful to
   ;; differentiate what packages are needed for what in general.  For now,
   ;; this is easier and works.
   (union64
    (fhs-union `(,@heroic-extra-client-libs
                 ,@steam-container-libs)
               #:name "fhs-union-64"))
   ;; Don't include heroic-client-libs as they are not needed in 32-bit.
   (union32
    (fhs-union steam-container-libs
               #:name "fhs-union-32"
               #:system "i686-linux"))
   (link-files '("share"))
   (description "Heroic is an Open Source Game Launcher.  Right now it supports launching
games from the Epic Games Store using Legendary, GOG Games using our custom
implementation with gogdl and Amazon Games using Nile.  This package provides
a script for launching Heroic in a Guix container which will use the directory
@file{$HOME/.local/share/guix-sandbox-home} where all games will be
installed.")))

(define-public heroic-nvidia-container
  (nonguix-container
   (inherit heroic-container)
   (name "heroic-nvidia")
   (ld.so.conf steam-nvidia-ld.so.conf)
   (ld.so.cache steam-nvidia-ld.so.cache)
   (union64
    (fhs-union `(,@heroic-extra-client-libs
                 ,@steam-nvidia-container-libs)
               #:name "fhs-union-64"))
   (union32
    (fhs-union steam-nvidia-container-libs
               #:name "fhs-union-32"
               #:system "i686-linux"))
   (preserved-env %nvidia-environment-variable-regexps)))

(define-public heroic (nonguix-container->package heroic-container))
(define-public heroic-nvidia (nonguix-container->package heroic-nvidia-container))

(define-public protonup-ng
  (package
    (name "protonup-ng")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudishBenne/protonup-ng")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yd2mhhqxzarqxk85zf42s931jzc94f1cssn1hblsqghr79laa45"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f)) ; there are no tests
    (inputs
     (list python-configparser python-requests))
    (home-page "https://github.com/cloudishBenne/protonup-ng")
    (synopsis "Manage Proton-GE Installations")
    (description "ProtonUp-ng is a CLI program and API to automate the installation
and update of GloriousEggroll's Proton-GE.")
    (license license:gpl3)))
