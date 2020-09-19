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
;;; Steam should subsequently be launched via fhs-internal.

;;; The sandbox shell aids in debugging missing container elements.  For
;;; example a missing symlink may be created manually before launching Steam
;;; to verify that the fix works before filing a bug report.

;;; A container wrapper creates the following store items:
;;; * Main container package [nonguix-container->package] (basically a dummy
;;;   package with symlink to wrapper script)
;;;   - Wrapper script [make-container-wrapper] (runs "guix environment")
;;;     References:
;;;     -> manifest.scm [make-container-manifest] (used by wrapper to guarantee
;;;        exact store items)
;;;     -> container-internal [make-container-internal] {inside container}
;;;        (dummy package added to container with symlink to internal-script)
;;;        - internal-script [make-internal-script] {inside container}
;;;          (script run in-container which performs additional setup before
;;;          launching the desired application)
;;;          References:
;;;          -> Wrapped package {inside container} (in this case Steam).

;;; Note: The extra container-internal package is necessary because there is no
;;; way to add the container package's own store path to its own manifest unless
;;; the manifest is printed inside the build phases. However, the (guix gexp)
;;; module is apparently disallowed inside build phases.

(define-module (nongnu packages steam-client)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (nonguix utils))

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
  (link-files    ngc-link-files (default '()))
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
  `(("bash" ,bash)                      ; Required for steam startup.
    ("coreutils" ,coreutils)
    ("diffutils" ,diffutils)
    ("dbus-glib" ,dbus-glib)            ; Required for steam browser.
    ("fontconfig" ,fontconfig)          ; Required for steam client.
    ("file" ,file)                      ; Used for steam installation.
    ("freetype" ,freetype)              ; Required for steam login.
    ("gawk" ,gawk)
    ("gcc:lib" ,gcc "lib")              ; Required for steam startup.
    ("grep" ,grep)
    ("mesa" ,mesa)                      ; Required for steam startup.
    ("nss-certs" ,nss-certs)            ; Required for steam login.
    ("sed" ,sed)
    ("tar" ,tar)
    ("util-linux" ,util-linux)          ; Required for steam login.
    ("xz" ,xz)))

(define steam-gameruntime-libs
  `(("alsa-lib" ,alsa-lib)              ; Required for audio in most games.
    ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio") ; Required for audio in most games.
    ("font-dejavu" ,font-dejavu)
    ("font-liberation" ,font-liberation)
    ("openal" ,openal)                  ; Prevents corrupt audio in Crypt of the Necrodancer.
    ("pulseaudio" ,pulseaudio)          ; Prevents corrupt audio in Sven Coop.
    ("python" ,python)))                ; Required for KillingFloor2 and Wreckfest.

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
           (((_ . directories) ...)
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
  (let* ((alsa-config ((@@ (gnu services sound) alsa-config-file)
                       ((@ (gnu services sound) alsa-configuration)
                        (alsa-plugins (to32 alsa-plugins)))))
         (fhs-internal (make-container-internal container alsa-config))
         (fhs-manifest (make-container-manifest container fhs-internal))
         (fhs-wrapper (make-container-wrapper container fhs-manifest fhs-internal))
         (pkg (ngc-wrap-package container)))
    (package
      (name (ngc-name container))
      (version (or (ngc-version container)
                   (package-version pkg)))
      (source #f)
      (inputs `(("alsa-config" ,alsa-config)
                ("wrap-package" ,(ngc-wrap-package container))
                ,@(if (null? (ngc-union64 container))
                      '()
                      `(("fhs-union-64" ,(ngc-union64 container))))
                ,@(if (null? (ngc-union32 container))
                      '()
                      `(("fhs-union-32" ,(ngc-union32 container))))
                ("fhs-internal" ,fhs-internal)
                ("fhs-wrapper" ,fhs-wrapper)
                ("fhs-manifest" ,fhs-manifest)))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (assoc-ref %outputs "out"))
                  (alsa-target (assoc-ref %build-inputs "alsa-config"))
                  (alsa-dest (string-append out "/etc/asound.conf"))
                  (internal-target (string-append (assoc-ref %build-inputs "fhs-internal")
                                                  "/bin/" ,(ngc-internal-name container)))
                  (internal-dest (string-append out "/sbin/" ,(ngc-internal-name container)))
                  (manifest-target (assoc-ref %build-inputs "fhs-manifest"))
                  (manifest-dest (string-append out "/etc/" ,(ngc-manifest-name container)))
                  (wrapper-target (assoc-ref %build-inputs "fhs-wrapper"))
                  (wrapper-dest (string-append out "/bin/" ,(ngc-name container)))
                  (link-files ',(ngc-link-files container)))
             (mkdir-p (string-append out "/sbin"))
             (mkdir-p (string-append out "/etc"))
             (mkdir-p (string-append out "/bin"))
             (symlink alsa-target alsa-dest)
             (symlink internal-target internal-dest)
             (symlink wrapper-target wrapper-dest)
             (symlink manifest-target manifest-dest)
             (for-each
              (lambda (link)
                (mkdir-p (dirname (string-append out "/" link)))
                (symlink (string-append (assoc-ref %build-inputs "wrap-package")
                                        "/" link)
                         (string-append out "/" link)))
              link-files)))))
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
                (args (cdr (command-line)))
                (command (if DEBUG '()
                             `("--" ,run ,@args))))
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
containers.  This manifest will use the 'modules' and 'packages' fields
specified in the container object, and will also include the exact store paths
of the containers 'wrap-package', 'union32', and 'union64' fields, as well as
the exact path for the fhs-internal package."
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

(define (make-container-internal container alsa-config)
  "Return a dummy package housing the fhs-internal script."
  (package
    (name (ngc-internal-name container))
    (version (or (ngc-version container)
                 (package-version (ngc-wrap-package container))))
    (source #f)
    (inputs `(("alsa-config" ,alsa-config)
              ("fhs-internal-script" ,(make-internal-script container alsa-config))))
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
    (synopsis "Script used to set up sandbox")
    (description "Script used inside the FHS Guix container to set up the
environment.")
    (license #f)))

(define (make-internal-script container alsa-config)
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
           (define (path->str path)
             (if (list? path)
                 (string-join path "/")
                 path))
           (define (new-symlink pair)
             (let ((target (path->str (car pair)))
                   (dest (path->str (cdr pair))))
               (unless (file-exists? dest)
                 (symlink target dest))))
           (define (icd-symlink file)
             (new-symlink
              `(,file . ("/usr/share/vulkan/icd.d" ,(basename file)))))
           (let ((guix-env (getenv "GUIX_ENVIRONMENT"))
                 (alsa-config #$(file-append alsa-config))
                 (union64 #$(file-append (ngc-union64 container)))
                 (union32 #$(file-append (ngc-union32 container)))
                 (ld.so.conf #$(file-append ld.so.conf))
                 (ld.so.cache #$(file-append ld.so.cache))
                 (args (cdr (command-line))))
             (delete-file "/bin/sh")
             (rmdir "/bin")
             (for-each
              mkdir-p
              '("/run/current-system/profile/etc"
                "/run/current-system/profile/share"
                "/sbin"
                "/usr/bin"
                "/usr/share/vulkan/icd.d"))
             (for-each
              new-symlink
              `((,alsa-config . "/etc/asound.conf")
                (,ld.so.cache . "/etc/ld.so.cache")
                (,ld.so.conf . "/etc/ld.so.conf")
                ((,guix-env "etc/ssl") . "/etc/ssl")
                ((,guix-env "etc/ssl") . "/run/current-system/profile/etc/ssl")
                ((,union32 "lib") . "/lib")
                ((,union32 "lib") . "/run/current-system/profile/lib")
                ((,union64 "bin") . "/bin")
                ((,union64 "bin/env") . "/usr/bin/env")
                ((,union64 "lib") . "/lib64")
                ((,union64 "lib") . "/run/current-system/profile/lib64")
                ((,union64 "lib/locale") . "/run/current-system/locale")
                ((,union64 "sbin/ldconfig") . "/sbin/ldconfig")
                ((,union64 "share/drirc.d") . "/usr/share/drirc.d")
                ((,union64 "share/fonts") . "/run/current-system/profile/share/fonts")
                ((,union64 "share/vulkan/explicit_layer.d") .
                 "/usr/share/vulkan/explicit_layer.d")))
             (for-each
              icd-symlink
              `(,@(find-files (string-append union32 "/share/vulkan/icd.d")
                              #:directories? #t)
                ,@(find-files (string-append union64 "/share/vulkan/icd.d")
                              #:directories? #t)))
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
    (link-files '("share/applications/steam.desktop"))
    (description "Steam is a digital software distribution platform created by
Valve.  This package provides a script for launching Steam in a Guix container
which will use the directory @file{$HOME/.local/share/guix-sandbox-home} where
all games will be installed."))))
