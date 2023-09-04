;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 pkill-9
;;; Copyright © 2020, 2021 ison <ison@airmail.cc>
;;; Copyright © 2021 pineapples
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Kozo <kozodev@runbox.com>
;;; Copyright © 2021, 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Elijah Malaby

(define-module (nongnu packages steam-client)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
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
  #:use-module (nonguix multiarch-container)
  #:use-module (nonguix utils))

(define steam-client
  (package
    (name "steam-client")
    (version "1.0.0.75")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.steampowered.com/steam/archive/precise/steam_"
                           version ".tar.gz"))
       (sha256
        (base32
         "19rn29slsxv7b5fisr1jzn79bskzifbj5hmxqn2436ivwfjna9g5"))
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
         (delete 'patch-dot-desktop-files)
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
         ;; Steamdeps installs missing packages, which doesn't work with Guix.
         (add-after 'install-binaries 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out")))
               (delete-file (string-append out "/lib/steam/bin_steamdeps.py"))
               (delete-file (string-append out "/bin/steamdeps"))))))))
    (home-page "https://store.steampowered.com")
    (synopsis "Digital distribution platform for managing and playing games")
    (description "Steam is a digital software distribution platform created by Valve.")
    (license (license:nonfree "file:///share/doc/steam/steam_subscriber_agreement.txt"))))

;; After guix commit to add a replacement for libx11 (security fixes),
;; 5ff0c8997a2ddf71af477883584a5f9ccd9b757f, a profile collision happens with
;; the propagated libx11 (now grafted) from mesa and its propagated-input
;; libxdamage.  See previous upstream report (when this occurred for expat and
;; fontconfig) at <https://issues.guix.gnu.org/53406>.  So we define explicit
;; replacement packages to workaround this issue.
;; TODO: remove once upstream bug is fixed or libx11 is ungrafted.
(define libxdamage-fixed
  (package
    (inherit libxdamage)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs libxdamage)
       (replace "libx11" libx11-fixed)))))

(define mesa-fixed
  (package
    (inherit mesa)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs mesa)
       (replace "libx11" libx11-fixed)
       (replace "libxdamage" libxdamage-fixed)))))

(define steam-client-libs
  `(("bash" ,bash)                      ; Required for steam startup.
    ("coreutils" ,coreutils)
    ("diffutils" ,diffutils)
    ("dbus-glib" ,dbus-glib)            ; Required for steam browser.
    ("elfutils" ,elfutils)              ; Required for capturing library dependencies in pv.
    ("eudev" ,eudev)                    ; Required for steamwebhelper/heavy runtime.
    ("fontconfig" ,fontconfig)          ; Required for steam client.
    ("file" ,file)                      ; Used for steam installation.
    ("find" ,findutils)                 ; Required at least for some logging.
    ("freetype" ,freetype)              ; Required for steam login.
    ("gawk" ,gawk)
    ("gdk-pixbuf" ,gdk-pixbuf)          ; Required for steam tray icon.
    ("gcc:lib" ,gcc "lib")              ; Required for steam startup.
    ("grep" ,grep)
    ("libbsd" ,libbsd)
    ("libcap" ,libcap)                  ; Required for SteamVR, but needs pkexec too.
    ("libusb" ,libusb)                  ; Required for SteamVR.
    ("libva" ,libva)                    ; Required for hardware video encoding/decoding.
    ("libvdpau" ,libvdpau)              ; Required for hardware video encoding/decoding.
    ("libvdpau-va-gl" ,libvdpau-va-gl)  ; Additional VDPAU support.
    ("llvm" ,llvm-for-mesa)             ; Required for mesa.
    ("lsof" ,lsof)                      ; Required for some friend's list actions.
    ;; TODO: Set back to mesa once libx11 is ungrafted upstream or once
    ;; <https://issues.guix.gnu.org/53406> is fixed.
    ("mesa" ,mesa-fixed)                ; Required for steam startup.
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

(define steam-ld.so.conf
  (packages->ld.so.conf
   (list (fhs-union `(,@steam-client-libs
                      ,@steam-gameruntime-libs
                      ,@fhs-min-libs)
                    #:name "fhs-union-64")
         (fhs-union `(,@steam-client-libs
                      ,@steam-gameruntime-libs
                      ,@fhs-min-libs)
                    #:name "fhs-union-32"
                    #:system "i686-linux"))))

(define steam-ld.so.cache
  (ld.so.conf->ld.so.cache steam-ld.so.conf))

(define-public steam-container
  (nonguix-container
   (name "steam")
   (wrap-package steam-client)
   (run "/bin/steam")
   (ld.so.conf steam-ld.so.conf)
   (ld.so.cache steam-ld.so.cache)
   (union64
    (fhs-union `(,@steam-client-libs
                 ,@steam-gameruntime-libs
                 ,@fhs-min-libs)
               #:name "fhs-union-64"))
   (union32
    (fhs-union `(,@steam-client-libs
                 ,@steam-gameruntime-libs
                 ,@fhs-min-libs)
               #:name "fhs-union-32"
               #:system "i686-linux"))
   (link-files '("share/applications/steam.desktop"
                 "share/applications/steam-asound32.desktop"))
   (description "Steam is a digital software distribution platform created by
Valve.  This package provides a script for launching Steam in a Guix container
which will use the directory @file{$HOME/.local/share/guix-sandbox-home} where
all games will be installed.")))

(define-public steam-nvidia-container
  (nonguix-container
   (inherit steam-container)
   (name "steam-nvidia")
   (union64 (replace-mesa (ngc-union64 steam-container)))
   (union32 (replace-mesa (ngc-union32 steam-container)))))

(define-public steam (nonguix-container->package steam-container))
(define-public steam-nvidia (nonguix-container->package steam-nvidia-container))

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
