;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 ng0 <gillmann@infotropique.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020-2025 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 pineapples <guixuser6392@protonmail.com>
;;; Copyright © 2021, 2024 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021, 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2023-2025 Tomas Volf <wolf@wolfsden.cz>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>

(define-module (nongnu packages mozilla)
  #:use-module (srfi srfi-26)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))

  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages wasm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

;;; Define the versions of rust needed to build firefox, trying to match
;;; upstream.  See table at [0], `Uses' column for the specific version.
;;; Using `rust' will likely lead to a newer version then listed in the table,
;;; but since in Guix only the latest packaged Rust is officially supported,
;;; it is a tradeoff worth making.
;;; 0: https://firefox-source-docs.mozilla.org/writing-rust-code/update-policy.html
;; The `rust' package is too old.
(define-public rust-firefox-esr rust-1.77)
(define-public rust-firefox     rust-1.77)

;; Update this id with every firefox update to its release date.
;; It's used for cache validation and therefore can lead to strange bugs.
(define %firefox-esr-build-id "20250303134822")

(define-public firefox-esr
  (package
    (name "firefox-esr")
    (version "128.8.0esr")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mozilla.org/pub/firefox/releases/"
                           version "/source/firefox-" version ".source.tar.xz"))
       (sha256
        (base32 "1grjk8r9bdapi7pqcbimbl65wvfrww7hy1fz7hmhyiw5q89cn7r0"))
       (patches
        (map (lambda (patch)
               (search-path
                (map (cut string-append <> "/nongnu/packages/patches")
                     %load-path)
                patch))
             '("firefox-esr-compare-paths.patch"
               "firefox-esr-use-system-wide-dir.patch")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(let ((clang #$(this-package-native-input "clang"))
              (wasi-sysroot #$(this-package-native-input "wasm32-wasi-clang-toolchain")))
          `("--enable-application=browser"

            ;; Configuration
            "--with-system-jpeg"
            "--with-system-zlib"
            ;; "--with-system-png" ;require libpng-apng >= 1.6.35
            "--with-system-icu"
            "--enable-system-ffi"
            "--enable-system-pixman"
            "--enable-jemalloc"

            ;; see https://bugs.gnu.org/32833
            "--with-system-nspr"
            ;; "--with-system-nss"

            ,(string-append "--with-clang-path="
                            clang "/bin/clang")
            ,(string-append "--with-libclang-path="
                            clang "/lib")
            ,(string-append "--with-wasi-sysroot=" wasi-sysroot "/wasm32-wasi")

            ;; Distribution
            "--with-distribution-id=org.nonguix"
            "--disable-official-branding"

            ;; Do not require addons in the global app or system directories to
            ;; be signed by Mozilla.
            "--allow-addon-sideload"
            "--with-unsigned-addon-scopes=app,system"

            ;; Features
            "--disable-tests"
            "--disable-updater"
            "--enable-pulseaudio"
            "--disable-crashreporter"

            ;; Build details
            "--disable-debug"
            "--enable-rust-simd"
            "--enable-release"
            "--enable-optimize"
            "--enable-strip"
            "--disable-elf-hack"))
      #:imported-modules %cargo-utils-modules
      #:modules `((ice-9 regex)
                  (ice-9 string-fun)
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs bytevectors)
                  (rnrs io ports)
                  (guix elf)
                  (guix build gremlin)
                  ,@%default-gnu-imported-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-preferences
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((port (open-file "browser/app/profile/firefox.js" "a")))
                (define (write-setting key value)
                  (format port "~%pref(\"~a\", ~a);~%"
                          key value)
                  (format #t "fix-preferences: setting value of ~a to ~a~%"
                          key value))

                ;; We should allow Firefox sandbox to read the store directory,
                ;; because Firefox sandbox have access to /usr on FHS distros.
                (write-setting "security.sandbox.content.read_path_whitelist"
                               (string-append "\"" (%store-directory) "/\""))

                ;; XDG settings should be managed by Guix.
                (write-setting "browser.shell.checkDefaultBrowser" "false")

                ;; It defaults to Google Location Services, but misses a necessary
                ;; API key.
                (write-setting "geo.provider.network.url"
                               "\"https://api.beacondb.net/v1/geolocate?key=firefox_nonguix.org\"")
                (close-port port))))
          (add-after 'fix-preferences 'fix-ffmpeg-runtime-linker
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((ffmpeg (assoc-ref inputs "ffmpeg"))
                     (libavcodec (string-append ffmpeg "/lib/libavcodec.so")))
                ;; Arrange to load libavcodec.so by its absolute file name.
                (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                  (("libavcodec\\.so")
                   libavcodec)))))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash
                     ;; This is the SHA256 output of an empty string.
                     "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                (for-each
                 (lambda (file)
                   (format #t "patch-cargo-checksums: patching checksums in ~a~%"
                           file)
                   (substitute* file
                     (("(checksum = )\".*\"" all name)
                      (string-append name "\"" null-hash "\""))))
                 (find-files "." "Cargo\\.lock$"))
                (for-each generate-all-checksums
                          '("build"
                            "dom/media"
                            "dom/webauthn"
                            "gfx"
                            "intl"
                            "js"
                            "media"
                            "modules"
                            "mozglue/static/rust"
                            "netwerk"
                            "remote"
                            "security/manager/ssl"
                            "servo"
                            "storage"
                            "third_party/rust"
                            "toolkit"
                            "xpcom/rust"
                            "services")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invokation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("args.append\\(\"--frozen\"\\)") "pass"))))
          (delete 'bootstrap)
          (add-before 'configure 'patch-SpeechDispatcherService.cpp
            (lambda _
              (let* ((lib "libspeechd.so.2")
                     (file "dom/media/webspeech/synth/speechd/SpeechDispatcherService.cpp")
                     (old-content (call-with-input-file file get-string-all)))
                (substitute
                 file
                 `((,(format #f "~s" lib)
                    . ,(lambda (line _)
                         (string-replace-substring
                          line
                          lib
                          (string-append #$speech-dispatcher "/lib/" lib))))))
                (if (string=? old-content
                              (call-with-input-file file get-string-all))
                    (error "substitute did nothing, phase requires an update")))))
          (add-before 'configure 'set-build-id
            ;; Firefox will write the timestamp to output, which is harmful
            ;; for reproducibility, so change it to a fixed date.  Use a
            ;; separate phase for easier modification with inherit.
            (lambda _
              (setenv "MOZ_BUILD_DATE" #$%firefox-esr-build-id)))
          (replace 'configure
            (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
              (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                                "/bin/autoconf"))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
              ;; This should use the host info probably (does firefox build on
              ;; non-x86_64 though?)
              (setenv "GUIX_PYTHONPATH"
                      (string-append (getcwd)
                                     "/obj-x86_64-pc-linux-gnu/_virtualenvs/build"))

              ;; Use Clang, Clang is 2x faster than GCC
              (setenv "AR" "llvm-ar")
              (setenv "NM" "llvm-nm")
              (setenv "CC" "clang")
              (setenv "CXX" "clang++")
              (setenv "WASM_CC"
                      (string-append
                       (assoc-ref inputs "wasm32-wasi-clang-toolchain")
                       "/bin/clang"))
              (setenv "WASM_CXX"
                      (string-append
                       (assoc-ref inputs "wasm32-wasi-clang-toolchain")
                       "/bin/clang++"))

              (setenv "MOZ_NOSPAM" "1")

              ;; WM_CLASS (default is "$MOZ_APP_NAME-$MOZ_UPDATE_CHANNEL").
              (setenv "MOZ_APP_REMOTINGNAME" "Firefox")

              (setenv "MOZBUILD_STATE_PATH" (getcwd))

              (let* ((mozconfig (string-append (getcwd) "/mozconfig"))
                     (out (assoc-ref outputs "out"))
                     (flags (cons (string-append "--prefix=" out)
                                  configure-flags)))
                (format #t "build directory: ~s~%" (getcwd))
                (format #t "configure flags: ~s~%" flags)

                (define write-flags
                  (lambda flags
                    (display (string-join
                              (map (cut string-append "ac_add_options " <>)
                                   flags)
                              "\n"))
                    (display "\n")))
                (with-output-to-file mozconfig
                  (lambda ()
                    (apply write-flags flags)
                    ;; The following option unsets Telemetry Reporting. With the Addons Fiasco,
                    ;; Mozilla was found to be collecting user's data, including saved passwords and
                    ;; web form data, without users consent. Mozilla was also found shipping updates
                    ;; to systems without the user's knowledge or permission.
                    ;; As a result of this, use the following command to permanently disable
                    ;; telemetry reporting in Firefox.
                    (display "unset MOZ_TELEMETRY_REPORTING\n")))
                (setenv "MOZCONFIG" mozconfig))
              (invoke "./mach" "configure")))
          (replace 'build
            (lambda* (#:key (make-flags '()) (parallel-build? #t)
                      #:allow-other-keys)
              (apply invoke "./mach" "build"
                     `(,@(if parallel-build?
                             `(,(string-append
                                 "-j" (number->string (parallel-job-count))))
                             '("-j1"))
                       ,@make-flags))))
          (add-after 'build 'neutralise-store-references
            (lambda _
              ;; Mangle the store references to compilers & other build tools in
              ;; about:buildconfig, reducing Firefox's closure by 1 GiB on x86-64.
              (let* ((build-dir (car (scandir "." (cut string-prefix? "obj-" <>))))
                     (file (string-append build-dir "/dist/bin/chrome/toolkit/content/global/buildconfig.html")))
                (substitute* file
                  (((format #f "(~a/)([0-9a-df-np-sv-z]{32})"
                            (regexp-quote (%store-directory)))
                    _ store hash)
                   (string-append store
                                  (string-take hash 8)
                                  "<!-- Guix: not a runtime dependency -->"
                                  (string-drop hash 8)))))))
          (replace 'install
            (lambda _ (invoke "./mach" "install")))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; The following two functions are from Guix's icecat package in
              ;; (gnu packages gnuzilla).  See commit
              ;; b7a0935420ee630a29b7e5ac73a32ba1eb24f00b.
              (define (runpath-of lib)
                (call-with-input-file lib
                  (compose elf-dynamic-info-runpath
                           elf-dynamic-info
                           parse-elf
                           get-bytevector-all)))
              (define (runpaths-of-input label)
                (let* ((dir (string-append (assoc-ref inputs label) "/lib"))
                       (libs (find-files dir "\\.so$")))
                  (append-map runpath-of libs)))
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib"))
                     ;; TODO: make me a loop again
                     (mesa-lib (string-append (assoc-ref inputs "mesa") "/lib"))
                     ;; For the integration of native notifications
                     (libnotify-lib (string-append (assoc-ref inputs "libnotify")
                                                   "/lib"))
                     ;; For hardware video acceleration via VA-API
                     (libva-lib (string-append (assoc-ref inputs "libva")
                                               "/lib"))
                     ;; VA-API is run in the RDD (Remote Data Decoder) sandbox
                     ;; and must be explicitly given access to files it needs.
                     ;; Rather than adding the whole store (as Nix had
                     ;; upstream do, see
                     ;; <https://github.com/NixOS/nixpkgs/pull/165964> and
                     ;; linked upstream patches), we can just follow the
                     ;; runpaths of the needed libraries to add everything to
                     ;; LD_LIBRARY_PATH.  These will then be accessible in the
                     ;; RDD sandbox.
                     (rdd-whitelist
                      (map (cut string-append <> "/")
                           (delete-duplicates
                            (append-map runpaths-of-input
                                        '("mesa" "ffmpeg")))))
                     (pulseaudio-lib (string-append (assoc-ref inputs "pulseaudio")
                                                    "/lib"))
                     ;; For sharing on Wayland
                     (pipewire-lib (string-append (assoc-ref inputs "pipewire")
                                                    "/lib"))
                     ;; For U2F and WebAuthn
                     (eudev-lib (string-append (assoc-ref inputs "eudev") "/lib"))
                     (gtk-share (string-append (assoc-ref inputs "gtk+")
                                               "/share")))
                (wrap-program (car (find-files lib "^firefox$"))
                  `("LD_LIBRARY_PATH" prefix (,mesa-lib ,libnotify-lib ,libva-lib
                                              ,pulseaudio-lib ,eudev-lib ,@rdd-whitelist
                                              ,pipewire-lib))
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  `("MOZ_LEGACY_PROFILES" = ("1"))
                  `("MOZ_ALLOW_DOWNGRADE" = ("1"))))))
          (add-after 'wrap-program 'install-desktop-entry
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((desktop-file "taskcluster/docker/firefox-snap/firefox.desktop")
                     (applications (string-append #$output "/share/applications")))
                (substitute* desktop-file
                  (("^Exec=firefox") (string-append "Exec=" #$output "/bin/firefox"))
                  (("Icon=.*") "Icon=firefox\n")
                  (("NewWindow") "new-window")
                  (("NewPrivateWindow") "new-private-window")
                  (("StartupNotify=true")
                   "StartupNotify=true\nStartupWMClass=Firefox"))
                (install-file desktop-file applications))))
          (add-after 'install-desktop-entry 'install-icons
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((icon-source-dir
                     (string-append
                      #$output "/lib/firefox/browser/chrome/icons/default")))
                (for-each
                 (lambda (size)
                   (let ((dest (string-append #$output "/share/icons/hicolor/"
                                              size "x" size "/apps")))
                     (mkdir-p dest)
                     (symlink (string-append icon-source-dir
                                             "/default" size ".png")
                              (string-append dest "/firefox.png"))))
                 '("16" "32" "48" "64" "128"))))))

      ;; Test will significantly increase build time but with little rewards.
      #:tests? #f

      ;; Some dynamic lib was determined at runtime, so rpath check may fail.
      #:validate-runpath? #f))
    (inputs
      (list
        bzip2
        cairo
        cups
        dbus-glib
        freetype
        ffmpeg
        gdk-pixbuf
        glib
        gtk+
        gtk+-2
        hunspell
        icu4c
        jemalloc
        libcanberra
        libevent
        libffi
        libgnome
        libjpeg-turbo
        libnotify
        ;; libpng-apng
        libva
        libvpx
        libxcomposite
        libxft
        libxinerama
        libxscrnsaver
        libxt
        mesa
        mit-krb5
        nspr-4.32
        ;; nss
        pango
        pipewire
        pixman
        pulseaudio
        speech-dispatcher
        sqlite
        startup-notification
        eudev
        unzip
        zip
        zlib))
    (native-inputs
      (list
        alsa-lib
        autoconf-2.13
        `(,rust-firefox-esr "cargo")
        clang-18
        llvm
        wasm32-wasi-clang-toolchain
        m4
        nasm
        node-lts
        perl
        pkg-config
        python
        rust-firefox-esr
        rust-cbindgen-0.26
        which
        yasm))
    (native-search-paths
     (list (search-path-specification
            (variable "ICECAT_SYSTEM_DIR")
            (separator #f)              ;single entry
            (files '("lib/icecat")))))
    (home-page "https://mozilla.org/firefox/")
    (synopsis "Trademarkless version of Firefox")
    (description
     "Full-featured browser client built from Firefox source tree, without
the official icon and the name \"firefox\".  This is the Extended Support
Release (ESR) version.")
    (license license:mpl2.0)))

(define-public firefox-esr/wayland
  (package
    (inherit firefox-esr)
    (name "firefox-esr-wayland")
    (native-inputs '())
    (inputs
     `(("bash" ,bash-minimal)
       ("firefox-esr" ,firefox-esr)))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((bash    (assoc-ref %build-inputs "bash"))
                (firefox (assoc-ref %build-inputs "firefox-esr"))
                (out     (assoc-ref %outputs "out"))
                (exe     (string-append out "/bin/firefox")))
           (mkdir-p (dirname exe))

           (call-with-output-file exe
             (lambda (port)
               (format port "#!~a
MOZ_ENABLE_WAYLAND=1 exec ~a $@\n"
                       (string-append bash "/bin/bash")
                       (string-append firefox "/bin/firefox"))))
           (chmod exe #o555)

           ;; Provide the manual and .desktop file.
           (copy-recursively (string-append firefox "/share")
                             (string-append out "/share"))
           (substitute* (string-append
                         out "/share/applications/firefox.desktop")
             ((firefox) out))
           #t))))))

;; Update this id with every firefox update to its release date.
;; It's used for cache validation and therefore can lead to strange bugs.
(define %firefox-build-id "20250303134749")

(define-public firefox
  (package
    (inherit firefox-esr)
    (name "firefox")
    (version "136.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mozilla.org/pub/firefox/releases/"
                           version "/source/firefox-" version ".source.tar.xz"))
       (patches
        (map (lambda (patch)
               (search-path
                (map (cut string-append <> "/nongnu/packages/patches")
                     %load-path)
                patch))
             '("firefox-restore-desktop-files.patch"
               "firefox-esr-compare-paths.patch"
               "firefox-use-system-wide-dir.patch")))
       (sha256
        (base32 "0mvg53fr9zi6pq2pwa6qzqi88brqig1wlzic9sz52i4knx733viv"))))
    (arguments
     (substitute-keyword-arguments (package-arguments firefox-esr)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'set-build-id
              (lambda _
                (setenv "MOZ_BUILD_DATE" #$%firefox-build-id)))
            ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1927380
            (add-before 'configure 'patch-icu-lookup
              (lambda _
                (let* ((file "js/moz.configure")
                       (old-content (call-with-input-file file get-string-all)))
                  (substitute* file
                    (("icu-i18n >= 76.1" all)
                     (string-append all ", icu-uc >= 76.1")))
                  (if (string=? old-content
                                (pk (call-with-input-file file get-string-all)))
                      (error "substitute did nothing, phase requires an update")))))))))
    (inputs
     (modify-inputs (package-inputs firefox-esr)
       (replace "icu4c" icu4c-76)))
    (native-inputs
     (modify-inputs (package-native-inputs firefox-esr)
       (replace "rust" rust-firefox)
       (replace "rust:cargo" `(,rust-firefox "cargo"))))
    (description
     "Full-featured browser client built from Firefox source tree, without
the official icon and the name \"firefox\".")))

;; As of Firefox 121.0, Firefox uses Wayland by default. This means we no
;; longer need a seperate package for Firefox on Wayland.
(define-public firefox-wayland
  (deprecated-package "firefox-wayland" firefox))
