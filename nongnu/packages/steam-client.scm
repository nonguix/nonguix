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
;;; Steam should subsequently be launched via the .sandbox-helper script.

;;; The sandbox shell aids in debugging missing container elements.  For
;;; example a missing symlink may be created manually before launching Steam
;;; to verify that the fix works before filing a bug report.

(define-module (nongnu packages steam-client)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
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

(define glibc-for-fhs
  (package
    (inherit glibc)
    (name "glibc-for-fhs") ;; Maybe rename this to "glibc-with-ldconfig-for-fhs"
    (source (origin
              (inherit (package-source glibc))
              (snippet #f))))) ;; Re-enable ldconfig

(define glibc-for-fhs-32
  (package
    (inherit glibc-for-fhs)
    (arguments (append (package-arguments glibc)
                       `(#:system "i686-linux")))))

(define (packages->ld.so.conf packages)
  "Takes a list of package objects and returns a file-like object for ld.so.conf
in the Guix store"
  (computed-file
   "ld.so.conf"
   (with-imported-modules
       `((guix build union)
         (guix build utils))
     #~(begin
         (use-modules (guix build union)
                      (guix build utils))
         (let* ((packages '#$packages) ;; Need to quote "#$packages" as #$packages tries to "apply" the first item to the rest, like a procedure.
                (find-lib-directories-in-single-package
                 (lambda (package)
                   (find-files (string-append package "/lib")
                               (lambda (file stat)
                                 ;; Setting keyword "stat" to "stat" means it will follow
                                 ;; symlinks, unlike what it's set to by default ("lstat").
                                 (eq? 'directory (stat:type stat)))
                               #:stat stat
                               #:directories? #t)))
                (find-lib-directories-in-all-packages
                 (lambda (packages)
                   (apply append ;; Concatenate the directory lists from "map" into one list
                          (map (lambda (package)
                                 (find-lib-directories-in-single-package package))
                               packages))))
                (fhs-lib-dirs
                 (find-lib-directories-in-all-packages packages)))
           (with-output-to-file
               #$output
             (lambda _
               (display (string-join '("/lib"
                                       "/lib/dri"
                                       "/lib/vdpau"
                                       "/lib/nss"
                                       "/lib/alsa-lib"
                                       "/lib64"
                                       "/lib64/dri"
                                       "/lib64/vdpau"
                                       "/lib64/nss"
                                       "/lib64/alsa-lib")
                                     "\n"))
               #$output)))))))

(define (ld.so.conf->ld.so.cache ld-conf)
  (computed-file
   "ld.so.cache"
   (with-imported-modules
       `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let* ((ldconfig (string-append #$glibc-for-fhs "/sbin/ldconfig")))
           (invoke ldconfig
                   "-X" ;; Don't update symbolic links
                   "-f" #$ld-conf ;; Use #$configuration as configuration file
                   "-C" #$output)))))) ;; Use #$output as cache file

(define libgcrypt-1.5.4 ; Half-Life needs libgcrypt.so.11.
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

(define steam-client-libs
  `(("alsa-lib" ,alsa-lib)
    ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")
    ("at-spi2-atk" ,at-spi2-atk)        ; Required by steam client beta.
    ("at-spi2-core" ,at-spi2-core)      ; Required by steam client beta.
    ("atk" ,atk)
    ("bzip2" ,bzip2)
    ("cairo" ,cairo)
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
    ("glibc" ,glibc-for-fhs)
    ("glibc-32" ,glibc-for-fhs-32)
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

(define steam-libs-32
  (package
    (name "steam-libs-32")
    (version "0.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:system "i686-linux"
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
    (inputs (append steam-client-libs steam-gameruntime-libs))
    (home-page #f)
    (synopsis "32-bit libraries used for Steam")
    (description "32-bit libraries needed to build the Steam sandbox FHS.")
    (license #f)))

(define steam-libs-64
  (package
    (inherit steam-libs-32)
    (name "steam-libs-64")
    (arguments
     (substitute-keyword-arguments (package-arguments steam-libs-32)
       ((#:system _)
        "x86_64-linux")))
    (synopsis "64-bit libraries used for Steam")
    (description "64-bit libraries needed to build the Steam sandbox FHS.")))

(define steam-ld.so.conf
  (packages->ld.so.conf `(,steam-libs-64 ,steam-libs-32)))

(define steam-ld.so.cache
  (ld.so.conf->ld.so.cache steam-ld.so.conf))

(define-public steam
  (package
    (name "steam")
    (version "1.0.0.61")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://repo.steampowered.com/steam/archive/precise/steam_"
             version ".tar.gz"))
       (sha256
        (base32
         "0c5xy57gwr14vp3wy3jpqi5dl6y7n01p2dy4jlgl9bf9x7616r6n"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (inputs `(("coreutils" ,coreutils)
              ("pulseaudio" ,pulseaudio)
              ("python" ,python-3)
              ("steam-libs-32" ,steam-libs-32)
              ("steam-libs-64" ,steam-libs-64)
              ("steam-ld.so.conf" ,steam-ld.so.conf)
              ("steam-ld.so.cache" ,steam-ld.so.cache)))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list "PREFIX=" (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-startscript
           ;; The script uses its own name to determine the package, wrap-program interferes with this however.
           (lambda _
             (substitute* "steam"
               (("STEAMPACKAGE=.*") "STEAMPACKAGE=steam\n"))
             ;; Change references of /usr to the store path.
             (substitute* "steam"
               (("/usr") (assoc-ref %outputs "out")))
             #t))
         (add-after 'unpack 'patch-desktop-file
           (lambda _
             (substitute* "steam.desktop"
               (("Exec=/usr/bin/steam") "Exec=steam"))
             #t))
         ;; /bin/steamdeps allows Steam to install missing packages, which doesn't play well with Guix, so remove it.
         (add-after 'install-binaries 'remove-unneccessary-file
           (lambda _
             (delete-file (string-append (assoc-ref %outputs "out") "/bin/steamdeps"))
             #t))
         (add-after 'install-binaries 'wrap-startscript
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (define (move-file old new)
               (rename-file old new)
               new)
             (define (write-file path data)
               (let ((str (if (list? data)
                              (format #f "~{~y~}" data)
                              data)))
                 (with-output-to-file path
                   (lambda ()
                     (let loop ((ls1 (string->list str)))
                       (unless (null? ls1)
                         (begin
                           (write-char (car ls1))
                           (loop (cdr ls1)))))))))
             (let* ((out (assoc-ref outputs "out"))
                    (shebang (string-append "#!" (which "bash")))
                    (steam-real (move-file (string-append out "/bin/steam")
                                           (string-append out "/bin/.steam-real")))
                    (manifest-dir (string-append out "/etc"))
                    (manifest-path (string-append manifest-dir "/manifest.scm"))
                    (sandbox (string-append out "/bin/steam"))
                    (sandbox-helper (string-append out "/bin/.sandbox-helper"))
                    (steam-libs-32 (assoc-ref inputs "steam-libs-32"))
                    (steam-libs-64 (assoc-ref inputs "steam-libs-64"))
                    (steam-ld.so.conf (assoc-ref inputs "steam-ld.so.conf"))
                    (steam-ld.so.cache (assoc-ref inputs "steam-ld.so.cache"))
                    (bash (assoc-ref inputs "bash"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (pulseaudio (assoc-ref inputs "pulseaudio"))
                    (python (assoc-ref inputs "python")))

               (mkdir-p manifest-dir)
               (write-file
                manifest-path
                `((use-package-modules
                   base certs compression file fonts gawk gnome linux)
                  (use-modules (guix utils)
                               (guix profiles)
                               (guix store)
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
                        (output "out")                              ;XXX: wild guess
                        (item item))))

                  (manifest-add
                   (packages->manifest
                    (list coreutils
                          diffutils
                          file
                          findutils
                          font-dejavu
                          font-liberation
                          gawk
                          glibc-locales
                          grep
                          gzip
                          nss-certs
                          sed
                          strace
                          tar
                          util-linux+udev
                          which
                          xz
                          zenity))
                   `(,(store-item->manifest-entry ,out)))))

               (write-file sandbox
                           (string-append shebang "
echo -e \"\\n* Starting Steam in sandbox: $HOME/.local/share/guix-sandbox-home\\n\"
mkdir -p $HOME/.local/share/guix-sandbox-home
if [ \"$DEBUG\" == \"1\" ]; then
    shell_command=()
else
    shell_command=(\"--\" \"" sandbox-helper "\" \"$@\")
fi
if [ -z ${XAUTHORITY+x} ]; then
    xauth=()
else
    xauth=(\"--preserve=XAUTHORITY\" \"--share=$XAUTHORITY\")
fi
# Make sure pulseaudio is running, if it starts first time inside the sandbox it will be broken
pulseaudio -D > /dev/null 2>&1
# Start sandbox
guix environment --ad-hoc --container --no-cwd --network \\
     --preserve=DISPLAY \\
     --preserve=SDL_AUDIODRIVER \\
     --preserve=XDG_DATA_HOME \\
     --preserve=XDG_RUNTIME_DIR \\
     \"${xauth[@]}\" \\
     --share=$HOME/.local/share/guix-sandbox-home=$HOME \\
     $(if [ -e \"/run/user/$UID/pulse\" ]; then echo -n \"--share=/run/user/$UID/pulse\"; else echo -n \"\"; fi) \\
     $(if [ -e \"/etc/machine-id\" ]; then echo -n \"--expose=/etc/machine-id\"; else echo -n ; fi) \\
     $(if [ -e \"/run/user/$UID/bus\" ]; then echo -n \"--share=/run/user/$UID/bus\"; else echo -n ; fi) \\
     $(if [ -e \"$HOME/.config/pulse\" ]; then echo -n \"--share=$HOME/.config/pulse\"; else echo -n ""; fi) \\
     --expose=/var/run/dbus \\
     --expose=/sys/dev \\
     --expose=/sys/devices \\
     --expose=/dev/dri \\
     --share=/dev/shm \\
     -m \"" manifest-path "\" \\
     \"${shell_command[@]}\"\n"))
               (chmod sandbox #o555)

               ;; Script sandbox-helper is needed to set-up the environment inside the container.
               (write-file sandbox-helper
                           (string-append "#!" (which "bash") "
mkdir -p /sbin
mkdir -p /usr/{bin,share}
mkdir -p /run/current-system/profile/{etc,share}
#FIXME: Setting up the below symlink should not require find.
find /gnu/store/ -maxdepth 1 -name '*glibc-locales*' -exec ln -s \"{}\"/lib/locale /run/current-system/locale \\;
ln -s \"$GUIX_ENVIRONMENT\"/share/fonts /run/current-system/profile/share/fonts
ln -s \"$GUIX_ENVIRONMENT\"/etc/ssl /run/current-system/profile/etc/ssl
ln -s \"$GUIX_ENVIRONMENT\"/etc/ssl /etc/ssl
ln -s " coreutils "/bin/env /usr/bin/env
ln -s " bash "/bin/bash /bin/bash
ln -s " pulseaudio "/bin/pulseaudio /bin/pulseaudio
ln -s " steam-libs-32 "/lib /run/current-system/profile/lib
ln -s " steam-libs-64 "/lib /run/current-system/profile/lib64
ln -s " steam-libs-32 "/lib /lib
ln -s " steam-libs-64 "/lib /lib64
ln -s " steam-ld.so.conf " /etc/ld.so.conf
ln -s " steam-ld.so.cache " /etc/ld.so.cache
ln -s " steam-libs-32 "/sbin/ldconfig /sbin/ldconfig
ln -s " steam-libs-64 "/share/vulkan /usr/share/vulkan
export PATH=" steam-libs-32 "/bin:" python "/bin:/bin:/sbin:/usr/bin${PATH:+:}$PATH
export STEAM_RUNTIME=1
export STEAM_RUNTIME_PREFER_HOST_LIBRARIES=1
" steam-real " \"$@\"\n"))
               (chmod sandbox-helper #o555)
               #t))))))

    (home-page "https://store.steampowered.com")
    (synopsis "Digital distribution platform for managing and playing games")
    (description "Steam is a digital software distribution platform created by
Valve.  This package provides the script steam-sandbox for launching Steam in
a Guix container which will use the directory
@file{$HOME/.local/share/guix-sandbox-home} where all games will be installed.")
    (license (license:nonfree "file:///share/doc/steam/steam_subscriber_agreement.txt"))))
