;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 pkill-9, ison <ison@airmail.cc>
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

;;; The steam script provided by this package may optionally be started as
;;; a shell instead of automatically launching Steam by setting the
;;; environment variable DEBUG=1.  If the sandbox is started this way then
;;; Steam should subsequently be launched via fhs-internal-script.

;;; The sandbox shell aids in debugging missing container elements.  For
;;; example a missing symlink may be created manually before launching Steam
;;; to verify that the fix works before filing a bug report.

(define-module (nongnu packages steam-client)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-record-type* <nonguix-container>
  nonguix-container make-nonguix-container
  nonguix-container? this-nonguix-container
  (name          ngc-name)
  (version       ngc-version (default #f))
  (wrap-package  ngc-wrap-package)
  (run           ngc-run)
  (wrapper-name  ngc-wrapper-name (default "nonguix-container-wrapper"))
  (manifest-name ngc-manifest-name (default "nonguix-container-manifest.scm"))
  (internal-name ngc-internal-name (default "fhs-internal"))
  (sandbox-home  ngc-sandbox-home (default ".local/share/guix-sandbox-home"))
  (union64       ngc-union64 (default '()))
  (union32       ngc-union32 (default '()))
  (preserved-env ngc-preserved-env (default '()))
  (exposed       ngc-exposed (default '()))
  (shared        ngc-shared (default '()))
  (modules       ngc-modules (default '()))
  (packages      ngc-packages (default '()))
  (home-page     ngc-home-page (default #f))
  (synopsis      ngc-synopsis (default #f))
  (description   ngc-description (default #f))
  (license       ngc-license (default #f)))

(define glibc-for-fhs
  (package
    (inherit glibc)
    (name "glibc-for-fhs")
    (source (origin
              (inherit (package-source glibc))
              (snippet #f)))))          ; Re-enable ldconfig.

(define libgcrypt-1.5.4                 ; Half-Life needs libgcrypt.so.11.
  (package
    (inherit libgcrypt)
    (version "1.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/libgcrypt/libgcrypt-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0czvqxkzd5y872ipy6s010ifwdwv29sqbnqc4pf56sd486gqvy6m"))))))

(define libffi-3.2
  (package
    (inherit libffi)
    (version "3.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://sourceware.org/pub/libffi/"
                              (package-name libffi) "-" version ".tar.gz"))
              (sha256
               (base32
                "1vylvsrbzrpqk298i4g1p82jxqkxhl2qf941sf0j775fyvxq09kb"))))))

(define steam-client
  (package
    (name "steam-client")
    (version "1.0.0.61")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.steampowered.com/steam/archive/precise/steam_"
                           version ".tar.gz"))
       (sha256
        (base32
         "0c5xy57gwr14vp3wy3jpqi5dl6y7n01p2dy4jlgl9bf9x7616r6n"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "PREFIX=" (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-startscript
           (lambda _
             (substitute* "steam"
               (("/usr") (assoc-ref %outputs "out")))
             #t))
         (add-after 'unpack 'patch-desktop-file
           (lambda _
             (substitute* "steam.desktop"
               (("Exec=/usr/bin/steam") "Exec=steam"))
             #t))
         ;; Steamdeps installs missing packages, which doesn't work with Guix.
         (add-after 'install-binaries 'remove-unneccessary-file
           (lambda _
             (delete-file (string-append (assoc-ref %outputs "out")
                                         "/bin/steamdeps"))
             #t)))))
    (home-page "https://store.steampowered.com")
    (synopsis "Digital distribution platform for managing and playing games")
    (description "Steam is a digital software distribution platform created by Valve.")
    (license (license:nonfree "file:///share/doc/steam/steam_subscriber_agreement.txt"))))

(define fhs-min-libs
  `(("glibc-for-fhs" ,glibc-for-fhs)
    ("glibc-locales" ,glibc-locales)))

(define steam-client-libs
  `(("alsa-lib" ,alsa-lib)
    ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")
    ("at-spi2-atk" ,at-spi2-atk)        ; Required by steam client beta.
    ("at-spi2-core" ,at-spi2-core)      ; Required by steam client beta.
    ("atk" ,atk)
    ("bash" ,bash)
    ("bzip2" ,bzip2)
    ("cairo" ,cairo)
    ("coreutils" ,coreutils)
    ("cups" ,cups)
    ("curl" ,curl)
    ("dbus" ,dbus)
    ("dbus-glib" ,dbus-glib)
    ("eudev" ,eudev)
    ("expat" ,expat)
    ("fontconfig" ,fontconfig)
    ("freetype" ,freetype)
    ("gcc:lib" ,gcc "lib")
    ("gconf" ,gconf)
    ("gdk-pixbuf" ,gdk-pixbuf)
    ("glib" ,glib)
    ("gtk+" ,gtk+-2)
    ("libappindicator" ,libappindicator)
    ("libcap" ,libcap)
    ("libffi-3.2" ,libffi-3.2)
    ("libice" ,libice)
    ("libselinux" ,libselinux)
    ("libsm" ,libsm)
    ("libusb" ,libusb)
    ("libva" ,libva)
    ("libx11" ,libx11)
    ("libxcomposite" ,libxcomposite)
    ("libxcursor" ,libxcursor)
    ("libxdamage" ,libxdamage)
    ("libxext" ,libxext)
    ("libxfixes" ,libxfixes)
    ("libxi" ,libxi)
    ("libxinerama" ,libxinerama)
    ("libxrandr" ,libxrandr)
    ("libxrender" ,libxrender)
    ("libxscrnsaver" ,libxscrnsaver)
    ("libxtst" ,libxtst)
    ("libxxf86vm" ,libxxf86vm)
    ("mesa" ,mesa)
    ("network-manager" ,network-manager)
    ("nspr" ,nspr)
    ("nss" ,nss)
    ("openal" ,openal)
    ("pango" ,pango)
    ("pulseaudio" ,pulseaudio)
    ("sdl2" ,sdl2)
    ("vulkan-loader" ,vulkan-loader)
    ("zlib" ,zlib)))

(define steam-gameruntime-libs
  `(("ffmpeg" ,ffmpeg)
    ("flac" ,flac)
    ("font-dejavu" ,font-dejavu)
    ("font-liberation" ,font-liberation)
    ("freeglut" ,freeglut)
    ("glew" ,glew)
    ("glu" ,glu)
    ("gst-plugins-base" ,gst-plugins-base)
    ("gst-plugins-ugly" ,gst-plugins-ugly)
    ("gstreamer" ,gstreamer)
    ("libcaca" ,libcaca)
    ("libcanberra" ,libcanberra)
    ("libdrm" ,libdrm)
    ("libgcrypt-1.5.4" ,libgcrypt-1.5.4)
    ("libgpg-error" ,libgpg-error)
    ("libidn" ,libidn)
    ("libjpeg-turbo" ,libjpeg-turbo)
    ("libmikmod" ,libmikmod)
    ("libogg" ,libogg)
    ("libpciaccess" ,libpciaccess)
    ("libpng" ,libpng)
    ("libpng-1.2" ,libpng-1.2)
    ("librsvg" ,librsvg)
    ("libsamplerate" ,libsamplerate)
    ("libtheora" ,libtheora)
    ("libtiff" ,libtiff)
    ("libvdpau" ,libvdpau)
    ("libvorbis" ,libvorbis)
    ("libvpx" ,libvpx)
    ("libxcb" ,libxcb)
    ("libxft" ,libxft)
    ("libxml2" ,libxml2)
    ("libxmu" ,libxmu)
    ("openssl" ,openssl)
    ("pixman" ,pixman)
    ("sdl" ,sdl)
    ("sdl-image" ,sdl-image)
    ("sdl-mixer" ,sdl-mixer)
    ("sdl-ttf" ,sdl-ttf)
    ("sdl2-image" ,sdl2-image)
    ("sdl2-mixer" ,sdl2-mixer)
    ("sdl2-ttf" ,sdl2-ttf)
    ("speex" ,speex)
    ("sqlite" ,sqlite)
    ("tbb" ,tbb)
    ("util-linux" ,util-linux)
    ("xkeyboard-config" ,xkeyboard-config)))

;;; Building ld.so.conf using find-files from package union results in error
;;; "Argument list too long" when launching Steam.
(define (fhs-ld.so.conf)
  "Return a file-like object for ld.so.conf."
  (plain-file
   "ld.so.conf"
   (let ((dirs '("/lib"
                 "/lib/alsa-lib"
                 "/lib/dri"
                 "/lib/nss"
                 "/lib/vdpau"
                 "/lib64"
                 "/lib64/alsa-lib"
                 "/lib64/dri"
                 "/lib64/nss"
                 "/lib64/vdpau")))
     (string-join dirs "\n"))))

(define (ld.so.conf->ld.so.cache ld-conf)
  "Create a ld.so.cache file-like object from an ld.so.conf file."
  (computed-file
   "ld.so.cache"
   (with-imported-modules
       `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((ldconfig (string-append #$glibc-for-fhs "/sbin/ldconfig")))
           (invoke ldconfig
                   "-X"                 ; Don't update symbolic links.
                   "-f" #$ld-conf       ; Use #$configuration as configuration file.
                   "-C" #$output))))))  ; Use #$output as cache file.

(define* (fhs-union inputs #:key (name "fhs-union") (version "0.0") (system "x86_64-linux"))
  "Create a package housing the union of inputs."
  (package
    (name name)
    (version version)
    (source #f)
    (inputs inputs)
    (build-system trivial-build-system)
    (arguments
     `(#:system ,system
       #:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories)
            #t)))))
    (home-page #f)
    (synopsis "Libraries used for FHS")
    (description "Libraries needed to build a guix container FHS.")
    (license #f)))

(define (nonguix-container->package container)
  "Return a package with wrapper script to launch the supplied container object
in a sandboxed FHS environment."
  (let* ((fhs-internal (make-container-internal container))
         (fhs-manifest (make-container-manifest container fhs-internal))
         (fhs-wrapper (make-container-wrapper container fhs-manifest fhs-internal))
         (container-name (ngc-name container))
         (union64 (ngc-union64 container))
         (union32 (ngc-union32 container))
         (pkg (ngc-wrap-package container)))
    (package
      (name container-name)
      (version (or (ngc-version container)
                   (package-version pkg)))
      (source #f)
      (inputs `(,@(if (null? union64)
                      '() `(("fhs-union-64" ,union64)))
                ,@(if (null? union32)
                      '() `(("fhs-union-32" ,union32)))
                ("fhs-wrapper" ,fhs-wrapper)))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                  (wrapper-target (assoc-ref %build-inputs "fhs-wrapper"))
                  (wrapper-dest (string-append bin "/" ,container-name)))
             (mkdir-p bin)
             (symlink wrapper-target wrapper-dest)))))
      (home-page (or (ngc-home-page container)
                     (package-home-page pkg)))
      (synopsis (or (ngc-synopsis container)
                    (package-synopsis pkg)))
      (description (or (ngc-description container)
                       (package-description pkg)))
      (license (or (ngc-license container)
                   (package-license pkg))))))

(define (make-container-wrapper container fhs-manifest fhs-internal)
  "Return a script file-like object that launches the supplied container object
in a sandboxed FHS environment."
  (program-file
   (ngc-wrapper-name container)
   (with-imported-modules
       `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (define (preserve-var var)
           (string-append "--preserve=" var))
         (define* (add-path path #:key writable?)
           (let ((opt (if writable?
                          "--share="
                          "--expose=")))
             (if (pair? path)
                 (string-append opt (car path) "=" (cdr path))
                 (string-append opt path))))
         (define (exists-> file)
           (if (and file (file-exists? file))
               `(,file) '()))
         (let* ((UID (number->string (passwd:uid (getpwnam (getenv "USER")))))
                (run #$(file-append fhs-internal "/bin/" (ngc-internal-name container)))
                (manifest-file #$(file-append fhs-manifest))
                (home (getenv "HOME"))
                (sandbox-home (string-append home "/" #$(ngc-sandbox-home container)))
                (preserved-env '("DISPLAY"
                                 "SDL_AUDIODRIVER"
                                 "STEAM_RUNTIME"
                                 "STEAM_RUNTIME_HEAVY"
                                 "STEAM_RUNTIME_PREFER_HOST_LIBRARIES"
                                 "XAUTHORITY"
                                 "XDG_DATA_HOME"
                                 "XDG_RUNTIME_DIR"))
                (expose `("/dev/dri"
                          "/dev/input"  ; Needed for controller input.
                          ,@(exists-> "/etc/machine-id")
                          "/sys/class/input" ; Needed for controller input.
                          "/sys/dev"
                          "/sys/devices"
                          "/var/run/dbus"))
                (share `("/dev/shm"
                         ,(string-append sandbox-home "=" home)
                         ,@(exists-> (string-append home "/.config/pulse"))
                         ,@(exists-> (string-append "/run/user/" UID "/pulse"))
                         ,@(exists-> (string-append "/run/user/" UID "/bus"))
                         ,@(exists-> (getenv "XAUTHORITY"))))
                (DEBUG (equal? (getenv "DEBUG") "1"))
                (command (if DEBUG '()
                             `("--" ,run "\"$@\""))))
           (format #t "\n* Launching ~a in sandbox: ~a.\n\n"
                   #$(package-name (ngc-wrap-package container)) sandbox-home)
           (when DEBUG
             (format #t "* DEBUG set to 1: Starting shell. Launch application manually with: ~a.\n\n"
                     #$(ngc-internal-name container)))
           (mkdir-p sandbox-home)
           (system "pulseaudio -D > /dev/null 2>&1")
           (apply system*
                  `("guix" "environment"
                    "--ad-hoc" "--container" "--no-cwd" "--network"
                    ,@(map preserve-var preserved-env)
                    ,@(map add-path expose)
                    ,@(map (lambda (item)
                             (add-path item #:writable? #t))
                           share)
                    "-m" ,manifest-file
                    ,@command)))))))

(define (make-container-manifest container fhs-internal)
  "Return a scheme file-like object to be used as package manifest for FHS
containers. This manifest will use the modules and packages specified in the
container, and will also include the exact store paths of the containers wrapped
package and unions, and the fhs-inernal package."
  (scheme-file
   (ngc-manifest-name container)
   #~(begin
       (use-package-modules
        #$@(ngc-modules container))
       (use-modules (guix gexp)
                    (guix utils)
                    (guix profiles)
                    (guix store)
                    (guix scripts package)
                    (srfi srfi-11))

       ;; Copied from guix/scripts/package.scm.
       (define (store-item->manifest-entry item)
         "Return a manifest entry for ITEM, a \"/gnu/store/...\" file name."
         (let-values (((name version)
                       (package-name->name+version (store-path-package-name item)
                                                   #\-)))
           (manifest-entry
             (name name)
             (version version)
             (output "out")             ;XXX: wild guess
             (item item))))

       (manifest-add
        (packages->manifest (list #$@(ngc-packages container)))
        (map store-item->manifest-entry
             '(#$(file-append (ngc-wrap-package container))
               #$(file-append (ngc-union64 container))
               #$(file-append (ngc-union32 container))
               #$(file-append fhs-internal)))))))

(define (make-container-internal container)
  "Return a dummy package housing the fhs-internal script."
  (package
    (name (ngc-internal-name container))
    (version (or (ngc-version container)
                 (package-version (ngc-wrap-package container))))
    (source #f)
    (inputs `(("fhs-internal-script" ,(make-internal-script container))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                (internal-target (assoc-ref %build-inputs "fhs-internal-script"))
                (internal-dest (string-append bin "/" ,(ngc-internal-name container))))
           (mkdir-p bin)
           (symlink internal-target internal-dest)))))
    (home-page #f)
    (synopsis "Script used ot set up sandbox")
    (description "Script used inside the FHS guix container to setup the
environment.")
    (license #f)))

(define (make-internal-script container)
  "Return an fhs-internal script which is used to perform additional steps to
set up the environment inside an FHS container before launching the desired
application."
  (let* ((ld.so.conf (fhs-ld.so.conf))
         (ld.so.cache (ld.so.conf->ld.so.cache ld.so.conf))
         (pkg (ngc-wrap-package container))
         (run (ngc-run container)))
    (program-file
     (ngc-internal-name container)
     (with-imported-modules
         `((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (define (new-symlink target dest)
             (unless (file-exists? dest)
               (symlink target dest)))
           (for-each mkdir-p '("/sbin" "/usr/bin" "/usr/share"
                               "/run/current-system/profile/etc"
                               "/run/current-system/profile/share"))
           (let ((guix-env (getenv "GUIX_ENVIRONMENT"))
                 (union64 #$(file-append (ngc-union64 container)))
                 (union32 #$(file-append (ngc-union32 container)))
                 (ld.so.conf #$(file-append ld.so.conf))
                 (ld.so.cache #$(file-append ld.so.cache))
                 (args (cdr (command-line))))
             (new-symlink (string-append union64 "/lib/locale") "/run/current-system/locale")
             (new-symlink (string-append union64 "/share/fonts") "/run/current-system/profile/share/fonts")
             (new-symlink (string-append guix-env "/etc/ssl") "/run/current-system/profile/etc/ssl")
             (new-symlink (string-append guix-env "/etc/ssl") "/etc/ssl")
             (new-symlink (string-append union64 "/bin/env") "/usr/bin/env")
             (new-symlink (string-append union64 "/bin/bash") "/bin/bash")
             (new-symlink (string-append union64 "/bin/pulseaudio") "/bin/pulseaudio")
             (new-symlink (string-append union32 "/lib") "/run/current-system/profile/lib")
             (new-symlink (string-append union64 "/lib") "/run/current-system/profile/lib64")
             (new-symlink (string-append union32 "/lib") "/lib")
             (new-symlink (string-append union64 "/lib") "/lib64")
             (new-symlink ld.so.conf "/etc/ld.so.conf")
             (new-symlink ld.so.cache "/etc/ld.so.cache")
             (new-symlink (string-append union64 "/sbin/ldconfig") "/sbin/ldconfig")
             (new-symlink (string-append union64 "/share/vulkan") "/usr/share/vulkan")
             (apply system* `(#$(file-append pkg run) ,@args))))))))

(define-public steam
  (nonguix-container->package
   (nonguix-container
    (name "steam")
    (wrap-package steam-client)
    (run "/bin/steam")
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
    (modules `(base certs compression file gawk gnome linux python))
    (packages
     `(coreutils
       diffutils
       file
       findutils
       gawk
       grep
       gzip
       nss-certs
       python
       sed
       strace
       tar
       util-linux+udev
       which
       xz
       zenity))
    (description "Steam is a digital software distribution platform created by
Valve.  This package provides a script for launching Steam in a Guix container
which will use the directory @file{$HOME/.local/share/guix-sandbox-home} where
all games will be installed."))))
