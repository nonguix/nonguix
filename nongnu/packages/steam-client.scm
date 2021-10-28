;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 pkill-9
;;; Copyright © 2020, 2021 ison <ison@airmail.cc>
;;; Copyright © 2021 pineapples
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
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
;;;   - Wrapper script [make-container-wrapper] (runs "guix shell")
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
  #:use-module (guix transformations)
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
  #:use-module (nongnu packages nvidia)
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
         (replace 'configure
           (lambda _
             (mkdir-p "bootstrap-temp")
             (invoke "tar" "xfa" "bootstraplinux_ubuntu12_32.tar.xz"
                     "-C" "bootstrap-temp")
             (substitute* "bootstrap-temp/steam.sh"
               (("export LD_LIBRARY_PATH=\"")
                "export LD_LIBRARY_PATH=\"${LD_LIBRARY_PATH-}:"))
             (substitute* "bootstrap-temp/ubuntu12_32/steam-runtime/run.sh"
               (("^export LD_LIBRARY_PATH=.*")
                "export LD_LIBRARY_PATH=\"${LD_LIBRARY_PATH-}:$steam_runtime_library_paths\""))
             (invoke "tar" "cfJ" "bootstraplinux_ubuntu12_32.tar.xz" "-C" "bootstrap-temp"
                     "linux32" "ubuntu12_32" "steam.sh" "steamdeps.txt")
             (delete-file-recursively "bootstrap-temp")))
         (add-after 'unpack 'patch-startscript
           (lambda _
             (substitute* "steam"
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
               ;; Steamdeps installs missing packages, which doesn't work with Guix.
               (delete-file (string-append out "/bin/steamdeps"))
               (wrap-program (string-append out "/bin/steam")
                 '("LD_LIBRARY_PATH" prefix
                   ("/lib"
                    "/lib/alsa-lib"
                    "/lib/dri"
                    "/lib/nss"
                    "/lib/vdpau"
                    "/lib64"
                    "/lib64/alsa-lib"
                    "/lib64/dri"
                    "/lib64/nss"
                    "/lib64/vdpau")))
               ;; .steam-real will fail unless it is renamed to exactly "steam".
               (rename-file (string-append out "/bin/steam")
                            (string-append out "/bin/steam-wrapper"))
               (rename-file (string-append out "/bin/.steam-real")
                            (string-append out "/bin/steam"))
               (substitute* (string-append out "/bin/steam-wrapper")
                 (("\\.steam-real") "steam"))))))))
    (home-page "https://store.steampowered.com")
    (synopsis "Digital distribution platform for managing and playing games")
    (description "Steam is a digital software distribution platform created by Valve.")
    (license (license:nonfree "file:///share/doc/steam/steam_subscriber_agreement.txt"))))

(define fhs-min-libs
  `(("glibc" ,glibc)
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
  (let* ((fhs-internal (make-container-internal container))
         (fhs-manifest (make-container-manifest container fhs-internal))
         (fhs-wrapper (make-container-wrapper container fhs-manifest fhs-internal))
         (pkg (ngc-wrap-package container)))
    (package
      (name (ngc-name container))
      (version (or (ngc-version container)
                   (package-version pkg)))
      (source #f)
      (inputs `(("wrap-package" ,(ngc-wrap-package container))
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
         (let* ((run #$(file-append fhs-internal "/bin/" (ngc-internal-name container)))
                (manifest-file #$(file-append fhs-manifest))
                (xdg-runtime (getenv "XDG_RUNTIME_DIR"))
                (home (getenv "HOME"))
                (sandbox-home (or (getenv "GUIX_SANDBOX_HOME")
                                  (string-append home "/" #$(ngc-sandbox-home container))))
                (preserved-env '("^DBUS_"
                                 "^DISPLAY$"
                                 "^DRI_PRIME$"
                                 "_PROXY$"
                                 "_proxy$"
                                 "^SDL_"
                                 "^STEAM_"
                                 "^XAUTHORITY$"
                                 ;; Matching all ^XDG_ vars causes issues
                                 ;; discussed in 80decf05.
                                 "^XDG_DATA_HOME$"
                                 "^XDG_RUNTIME_DIR$"))
                (expose `("/dev/dri"
                          "/dev/input"  ; Needed for controller input.
                          ,@(exists-> "/etc/machine-id")
                          "/sys/class/input" ; Needed for controller input.
                          "/sys/dev"
                          ,@(exists-> "/dev/nvidia0") ; needed for nvidia proprietary driver
                          ,@(exists-> "/dev/nvidiactl")
                          ,@(exists-> "/dev/nvidia-modeset")
                          "/sys/devices"
                          ,@(exists-> "/var/run/dbus")))
                (share `("/dev/shm"
                         ,(string-append sandbox-home "=" home)
                         ,@(exists-> (string-append home "/.config/pulse"))
                         ,@(exists-> (string-append xdg-runtime "/pulse"))
                         ,@(exists-> (string-append xdg-runtime "/bus"))
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
           (invoke #$(file-append pulseaudio "/bin/pulseaudio")
                   "--start"
                   "--exit-idle-time=60")
           (apply invoke
                  `("guix" "shell"
                    "--container" "--no-cwd" "--network"
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
    (synopsis "Script used to set up sandbox")
    (description "Script used inside the FHS Guix container to set up the
environment.")
    (license #f)))

(define (make-internal-script container)
  "Return an fhs-internal script which is used to perform additional steps to
set up the environment inside an FHS container before launching the desired
application."
  (let* ((pkg (ngc-wrap-package container))
         (run (ngc-run container)))
    (program-file
     (ngc-internal-name container)
     (with-imported-modules
         `((guix build utils)
           (ice-9 getopt-long))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 getopt-long))
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
           (define fhs-option-spec
             '((asound32 (value #f))))
           (let* ((guix-env (getenv "GUIX_ENVIRONMENT"))
                  (union64 #$(file-append (ngc-union64 container)))
                  (union32 #$(file-append (ngc-union32 container)))
                  (all-args (cdr (command-line)))
                  (fhs-args (member "--" all-args))
                  (steam-args (if fhs-args
                                  (reverse (cdr (member "--" (reverse all-args))))
                                  all-args)))
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
              `(((,guix-env "etc/ssl") . "/etc/ssl")
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

             ;; Process FHS-specific command line options
             (let* ((options (getopt-long (or fhs-args '("")) fhs-option-spec))
                    (asound32-opt (option-ref options 'asound32 #f))
                    (asound-lib (if asound32-opt "lib" "lib64")))
               (if asound32-opt
                   (display "\n\n/etc/asound.conf configured for 32-bit.\n\n\n")
                   (display "\n\n/etc/asound.conf configured for 64-bit.\nLaunch steam with \"steam -- --asound32\" to use 32-bit instead.\n\n\n"))
               (with-output-to-file "/etc/asound.conf"
                 (lambda _ (format (current-output-port) "# Generated by steam-client

# Use PulseAudio by default
pcm_type.pulse {
  lib \"/~a/alsa-lib/libasound_module_pcm_pulse.so\"
}

ctl_type.pulse {
  lib \"/~a/alsa-lib/libasound_module_ctl_pulse.so\"
}

pcm.!default {
  type pulse
  fallback \"sysdefault\"
  hint {
    show on
    description \"Default ALSA Output (currently PulseAudio Sound Server)\"
  }
}

ctl.!default {
  type pulse
  fallback \"sysdefault\"
}\n\n" asound-lib asound-lib))))

             (apply system* `(#$(file-append pkg run) ,@steam-args))))))))

(define-public steam
  (nonguix-container->package
   (nonguix-container
    (name "steam")
    (wrap-package steam-client)
    (run "/bin/steam-wrapper")
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
all games will be installed."))))

(define-public steam-nvidia
  (nonguix-container->package
   (nonguix-container
    (name "steam-nvidia")
    (wrap-package steam-client)
    (run "/bin/steam-wrapper")
    (union64
     (replace-mesa
      (fhs-union `(,@steam-client-libs
                  ,@steam-gameruntime-libs
                  ,@fhs-min-libs)
                #:name "fhs-union-64")))
    (union32
     (replace-mesa
      (fhs-union `(,@steam-client-libs
                  ,@steam-gameruntime-libs
                  ,@fhs-min-libs)
                #:name "fhs-union-32"
                #:system "i686-linux")))
    (link-files '("share/applications/steam.desktop"
                  "share/applications/steam-asound32.desktop"))
    (description "Steam is a digital software distribution platform created by
Valve.  This package provides a script for launching Steam in a Guix container
which will use the directory @file{$HOME/.local/share/guix-sandbox-home} where
all games will be installed."))))
