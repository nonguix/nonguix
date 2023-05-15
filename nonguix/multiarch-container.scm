;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 pkill-9
;;; Copyright © 2020, 2021 ison <ison@airmail.cc>
;;; Copyright © 2021 pineapples
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Kozo <kozodev@runbox.com>
;;; Copyright © 2021, 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

;;; The script provided by this package may optionally be started as
;;; a shell instead of automatically launching the wrapped entrypoint by setting
;;; the environment variable DEBUG=1.  If the sandbox is started this way then
;;; the package should subsequently be launched via fhs-internal.

;;; The sandbox shell aids in debugging missing container elements.  For
;;; example a missing symlink may be created manually before launching the
;;; package to verify that the fix works before filing a bug report.

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
;;;          -> Wrapped package {inside container}.

;;; Note: The extra container-internal package is necessary because there is no
;;; way to add the container package's own store path to its own manifest unless
;;; the manifest is printed inside the build phases. However, the (guix gexp)
;;; module is apparently disallowed inside build phases.

(define-module (nonguix multiarch-container)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pulseaudio)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)

  #:export (nonguix-container
            nonguix-container?
            ngc-name
            ngc-version
            ngc-wrap-package
            ngc-run
            ngc-wrapper-name
            ngc-manifest-name
            ngc-internal-name
            ngc-sandbox-home
            ngc-ld.so.conf
            ngc-ld.so.cache
            ngc-union64
            ngc-union32
            ngc-preserved-env
            ngc-exposed
            ngc-shared
            ngc-modules
            ngc-packages
            ngc-link-files
            ngc-home-page
            ngc-synopsis
            ngc-description
            ngc-license

            fhs-min-libs
            fhs-union
            ld.so.conf->ld.so.cache
            packages->ld.so.conf
            nonguix-container->package))

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
  (ld.so.conf    ngc-ld.so.conf)
  (ld.so.cache   ngc-ld.so.cache)
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
    (source (origin (inherit (package-source glibc))
                    ;; Remove Guix's patch to read ld.so.cache from /gnu/store
                    ;; directories, re-enabling the default /etc/ld.so.cache
                    ;; behavior.
                    (patches (delete (car (search-patches "glibc-dl-cache.patch"))
                                     (origin-patches (package-source glibc))))))))

(define fhs-min-libs
  `(("glibc" ,glibc-for-fhs)
    ("glibc-locales" ,glibc-locales)))

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

(define (ld.so.conf->ld.so.cache ld-conf)
  "Create a ld.so.cache file-like object from an ld.so.conf file."
  (computed-file
   "ld.so.cache"
   (with-imported-modules
       `((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((ldconfig (string-append #$glibc "/sbin/ldconfig")))
           (invoke ldconfig
                   "-X"                 ; Don't update symbolic links.
                   "-f" #$ld-conf       ; Use #$ld-conf as configuration file.
                   "-C" #$output))))))  ; Use #$output as cache file.

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
         ;; Need to quote "#$packages" as #$packages tries to "apply" the first item to the rest, like a procedure.
         (let* ((packages '#$packages)
                ;; Add "/lib" to each package.
                ;; TODO Make this more general for other needed directories.
                (dirs-lib
                 (lambda (packages)
                   (map (lambda (package)
                          (string-append package "/lib"))
                        packages)))
                (fhs-lib-dirs
                 (dirs-lib packages)))
           (call-with-output-file #$output
               (lambda (port)
                 (for-each (lambda (directory)
                             (display directory port)
                             (newline port))
                           fhs-lib-dirs)))
           #$output)))))

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
                                 "^GDK_SCALE$" ; For UI scaling.
                                 "^PRESSURE_VESSEL_" ; For pressure vessel options.
                                 "_PROXY$"
                                 "_proxy$"
                                 "^SDL_"
                                 "^STEAM_"
                                 "^VDPAU_DRIVER_PATH$" ; For VDPAU drivers.
                                 "^XAUTHORITY$"
                                 ;; Matching all ^XDG_ vars causes issues
                                 ;; discussed in 80decf05.
                                 "^XDG_DATA_HOME$"
                                 "^XDG_RUNTIME_DIR$"
                                 ;; The following are useful for debugging.
                                 "^CAPSULE_DEBUG$"
                                 "^G_MESSAGES_DEBUG$"
                                 "^LD_DEBUG$"
                                 "^LIBGL_DEBUG$"))
                (expose `("/dev/bus/usb" ; Needed for libusb.
                          "/dev/dri"
                          "/dev/input"  ; Needed for controller input.
                          "/dev/uinput" ; Needed for Steam Input.
                          ,@(exists-> "/dev/nvidia0") ; needed for nvidia proprietary driver
                          ,@(exists-> "/dev/nvidiactl")
                          ,@(exists-> "/dev/nvidia-modeset")
                          ,@(exists-> "/etc/machine-id")
                          "/etc/localtime" ; Needed for correct time zone.
                          "/sys/class/drm" ; Needed for hw monitoring like MangoHud.
                          "/sys/class/hwmon" ; Needed for hw monitoring like MangoHud.
                          "/sys/class/hidraw" ; Needed for devices like the Valve Index.
                          "/sys/class/input" ; Needed for controller input.
                          ,@(exists-> "/sys/class/power_supply") ; Needed for power monitoring like MangoHud.
                          ,@(exists-> "/sys/class/powercap") ; Needed for power monitoring like MangoHud.
                          "/sys/dev"
                          "/sys/devices"
                          ,@(exists-> "/var/run/dbus")))
                ;; /dev/hidraw is needed for SteamVR to access the HMD, although here we
                ;; share all hidraw devices. Instead we could filter to only share specific
                ;; device. See, for example, this script:
                ;; https://arvchristos.github.io/post/matching-dev-hidraw-devices-with-physical-devices/
                (share `(,@(find-files "/dev" "hidraw")
                         "/dev/shm"
                         ;; "/tmp/.X11-unix" is needed for bwrap, and "/tmp" more generally
                         ;; for writing things like crash dumps and "steam_chrome_shm".
                         "/tmp"
                         ,(string-append sandbox-home "=" home)
                         ,@(exists-> (string-append home "/.config/pulse"))
                         ,@(exists-> (string-append xdg-runtime "/pulse"))
                         ,@(exists-> (string-append xdg-runtime "/bus"))
                         ,@(exists-> (getenv "XAUTHORITY"))))
                (DEBUG (equal? (getenv "DEBUG") "1"))
                (args (cdr (command-line)))
                (command (if DEBUG '()
                             `("--" ,run ,@args))))
           ;; TODO: Remove once upstream change is merged and in stable pressure-vessel
           ;; (although may want to hold off for anyone using older pressure-vessel versions
           ;; for whatever reason), see:
           ;; https://gitlab.steamos.cloud/steamrt/steam-runtime-tools/-/merge_requests/406
           (setenv "PRESSURE_VESSEL_FILESYSTEMS_RO" "/gnu/store")
           ;; By default VDPAU drivers are searched for in libvdpau's store
           ;; path, so set this path to where the drivers will actually be
           ;; located in the container.
           (setenv "VDPAU_DRIVER_PATH" "/lib64/vdpau")
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
    (inputs `(("fhs-internal-script"
               ,(make-internal-script container))))
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
  ;; The ld cache is not created inside the container, meaning the paths it
  ;; contains are directly to /gnu/store/. Instead, it could be generated with
  ;; a generic ld.so.conf and result in paths more typical in an FHS distro,
  ;; like /lib within the container. This may be useful for future compatibility.
  (let* ((ld.so.conf (ngc-ld.so.conf container))
         (ld.so.cache (ngc-ld.so.cache container))
         (pkg (ngc-wrap-package container))
         (run (ngc-run container)))
    (program-file
     (ngc-internal-name container)
     (with-imported-modules
         `((guix build utils))
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
                  (ld.so.conf #$(file-append ld.so.conf))
                  (ld.so.cache #$(file-append ld.so.cache))
                  (all-args (cdr (command-line)))
                  (fhs-args (member "--" all-args))
                  (package-args (if fhs-args
                                    (reverse (cdr (member "--" (reverse all-args))))
                                    all-args)))
             (delete-file "/bin/sh")
             (rmdir "/bin")
             (for-each
              mkdir-p
              '("/run/current-system/profile/etc"
                "/run/current-system/profile/share"
                "/sbin"
                "/usr/share/vulkan/icd.d"
                "/usr/share/vulkan/implicit_layer.d")) ; Implicit layers like MangoHud
             (for-each
              new-symlink
              `((,ld.so.cache . "/etc/ld.so.cache")
                (,ld.so.conf . "/etc/ld.so.conf") ;; needed?
                ((,guix-env "etc/ssl") . "/etc/ssl")
                ((,guix-env "etc/ssl") . "/run/current-system/profile/etc/ssl")
                ((,union32 "lib") . "/lib")
                ((,union32 "lib") . "/run/current-system/profile/lib")
                ((,union64 "bin") . "/bin")
                ((,union64 "bin") . "/usr/bin") ; Steam hardcodes some paths like xdg-open.
                ((,union64 "lib") . "/lib64")
                ((,union64 "lib") . "/run/current-system/profile/lib64")
                ((,union64 "lib/locale") . "/run/current-system/locale")
                ((,union64 "sbin/ldconfig") . "/sbin/ldconfig")
                ((,union64 "share/drirc.d") . "/usr/share/drirc.d")
                ((,union64 "share/fonts") . "/run/current-system/profile/share/fonts")
                ((,union64 "etc/fonts") . "/etc/fonts")
                ((,union64 "share/vulkan/explicit_layer.d") .
                 "/usr/share/vulkan/explicit_layer.d")
                ;; The MangoHud layer has the same file name for 64- and 32-bit,
                ;; so create links with different names.
                ((,union64 "share/vulkan/implicit_layer.d/MangoHud.json") .
                 "/usr/share/vulkan/implicit_layer.d/MangoHud.json")
                ((,union32 "share/vulkan/implicit_layer.d/MangoHud.json") .
                 "/usr/share/vulkan/implicit_layer.d/MangoHud.x86.json")))
             (for-each
              icd-symlink
              ;; Use stat to follow links from packages like MangoHud.
              `(,@(find-files (string-append union32 "/share/vulkan/icd.d")
                              #:directories? #t #:stat stat)
                ,@(find-files (string-append union64 "/share/vulkan/icd.d")
                              #:directories? #t #:stat stat)))
             ;; TODO: This is not the right place for this.
             ;; Newer versions of Steam won't startup if they can't copy to here
             ;; (previous would output this error but continue).
             (if (file-exists? ".steam/root/bootstrap.tar.xz")
                 (chmod ".steam/root/bootstrap.tar.xz" #o644))

             ;; Process FHS-specific command line options.
             (let* ((options (getopt-long (or fhs-args '("")) fhs-option-spec))
                    (asound32-opt (option-ref options 'asound32 #f))
                    (asound-lib (if asound32-opt "lib" "lib64")))
               (if asound32-opt
                   (display "\n\n/etc/asound.conf configured for 32-bit.\n\n\n")
                   (display (string-append "\n\n/etc/asound.conf configured for 64-bit.\nLaunch "
                                           #$(ngc-name container)
                                           " with \""
                                           (basename #$(ngc-run container))
                                           " -- --asound32\" to use 32-bit instead.\n\n\n")))
               (with-output-to-file "/etc/asound.conf"
                 (lambda _ (format (current-output-port) "# Generated by nonguix's internal script

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

             (apply system* `(#$(file-append pkg run) ,@package-args))))))))
