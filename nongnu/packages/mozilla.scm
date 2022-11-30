;;; GNU Guix --- Functional package management for GNU
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
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 pineapples <guixuser6392@protonmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021, 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
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

(define-module (nongnu packages mozilla)
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
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libreoffice) ;for hunspell
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
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages wasm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

;; Define the versions of rust needed to build firefox, trying to match
;; upstream.  See the file taskcluster/ci/toolchain/rust.yml at
;; https://searchfox.org under the particular firefox release, like
;; mozilla-esr102.
(define-public rust-firefox-esr rust) ; 1.60 is the default in Guix
(define-public rust-firefox (@@ (gnu packages rust) rust-1.61)) ; 1.63 is also listed, but 1.61 is the minimum needed

;; rust-cbindgen-0.23/0.24 dependencies
(define-public rust-unicode-ident-1
  (package
    (name "rust-unicode-ident")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unicode-ident" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bqswc96ws8l6k7xx56dg521a3l5imi3mhlcz7rsi6a92mxb7xf4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/unicode-ident")
    (synopsis
     "Better optimized implementation of the older unicode-xid crate")
    (description
     "Determine whether characters have the XID_Start or XID_Continue properties
according to Unicode Standard Annex #31")
    (license (list license:unicode license:expat))))

(define-public rust-textwrap-0.15
  (package
    (inherit rust-textwrap-0.12)
    (name "rust-textwrap")
    (version "0.15.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "textwrap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yw513k61lfiwgqrfvsjw1a5wpvm0azhpjr2kr0jhnq9c56is55i"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hyphenation" ,rust-hyphenation-0.8)
                       ("rust-smawk" ,rust-smawk-0.3)
                       ("rust-terminal-size" ,rust-terminal-size-0.1)
                       ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))))

(define-public rust-clap-lex-0.2
  (package
    (name "rust-clap-lex")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_lex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-os-str-bytes" ,rust-os-str-bytes-6))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
    (synopsis "Minimal, flexible command line parser")
    (description "Minimal, flexible command line parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-3.2.15
  (package
    (inherit rust-clap-derive-3)
    (name "rust-clap-derive")
    (version "3.2.15")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1d2c4vs345fwihkd8cc7m6acbiydcwramkd5mnp36p0a7g6jm9cv"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))))

(define-public rust-clap-3.2.16
  (package
    (inherit rust-clap-3)
    (name "rust-clap")
    (version "3.2.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1af06z8z7m3327yz1xvzxfjanclgpvvy3lssb745rig7adkbpnx3"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-clap-derive" ,rust-clap-derive-3.2.15)
                       ("rust-clap-lex" ,rust-clap-lex-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-terminal-size" ,rust-terminal-size-0.1)
                       ("rust-textwrap" ,rust-textwrap-0.15)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-yaml-rust" ,rust-yaml-rust-0.4))))))

(define-public rust-cbindgen-0.24
  (package
    (inherit rust-cbindgen-0.19)
    (name "rust-cbindgen")
    (version "0.24.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cbindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yqxqsz2d0cppd8zwihk2139g5gy38wqgl9snj6rnk8gyvnqsdd6"))))
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-3.2.16)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs (("rust-serial-test" ,rust-serial-test-0.5))))))

;; Bug with firefox build (v101-102) with cbindgen-0.24, see
;; https://bugzilla.mozilla.org/show_bug.cgi?id=1773259#c5 for possible patch
;; (untested)
(define-public rust-cbindgen-0.23
  (package
    (inherit rust-cbindgen-0.24)
    (name "rust-cbindgen")
    (version "0.23.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cbindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "006rn3fn4njayjxr2vd24g1awssr9i3894nbmfzkybx07j728vav"))))))

;; Update this id with every firefox update to it's release date.
;; It's used for cache validation and therefor can lead to strange bugs.
(define %firefox-esr-build-id "20221115000000")

(define-public firefox-esr
  (package
    (name "firefox-esr")
    (version "102.5.0esr")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mozilla.org/pub/firefox/releases/"
                           version "/source/firefox-" version ".source.tar.xz"))
       (sha256
        (base32 "1n2pq165fxmvgcr5mv3hhaid2vn7lh3jg03lf13kz4c5295x8z81"))))
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
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs bytevectors)
                  (rnrs io ports)
                  (guix elf)
                  (guix build gremlin)
                  ,@%gnu-build-system-modules)
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
                (("\"--frozen\",") ""))))
          (delete 'bootstrap)
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
                     ;; mach will use parallel build if possible by default
                     `(,@(if parallel-build?
                             '()
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
                     ;; For U2F and WebAuthn
                     (eudev-lib (string-append (assoc-ref inputs "eudev") "/lib"))
                     (gtk-share (string-append (assoc-ref inputs "gtk+")
                                               "/share")))
                (wrap-program (car (find-files lib "^firefox$"))
                  `("LD_LIBRARY_PATH" prefix (,mesa-lib ,libnotify-lib ,libva-lib
                                              ,pulseaudio-lib ,eudev-lib ,@rdd-whitelist))
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
                   "StartupNotify=true\nStartupWMClass=Navigator"))
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

      ;; WARNING: Parallel build will consume lots of memory!
      ;; If you have encountered OOM issue in build phase, try disable it.
      ;; #:parallel-build? #f

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
        icu4c-71
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
        startup-notification
        sqlite
        eudev
        unzip
        zip
        zlib))
    (native-inputs
      (list
        alsa-lib
        autoconf-2.13
        `(,rust-firefox-esr "cargo")
        clang
        llvm
        wasm32-wasi-clang-toolchain
        m4
        nasm
        node
        perl
        pkg-config
        python
        rust-firefox-esr
        rust-cbindgen-0.23
        which
        yasm))
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

;; Update this id with every firefox update to it's release date.
;; It's used for cache validation and therefor can lead to strange bugs.
(define %firefox-build-id "20221129000000")

(define-public firefox
  (package
    (inherit firefox-esr)
    (name "firefox")
    (version "107.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mozilla.org/pub/firefox/releases/"
                           version "/source/firefox-" version ".source.tar.xz"))
       (sha256
        (base32 "0iq67r9ik6zng9m8zzrsaf1d1fvhpsdpf66whgbb0hwipawm16g2"))))
    (arguments
     (substitute-keyword-arguments (package-arguments firefox-esr)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'set-build-id
              (lambda _
                (setenv "MOZ_BUILD_DATE" #$%firefox-build-id)))))))
    (native-inputs
     (modify-inputs (package-native-inputs firefox-esr)
       (replace "rust" rust-firefox)
       (replace "rust:cargo" `(,rust-firefox "cargo"))
       (replace "node" node-lts)
       (replace "rust-cbindgen" rust-cbindgen-0.24)))
    (description
     "Full-featured browser client built from Firefox source tree, without
the official icon and the name \"firefox\".")))

(define-public firefox/wayland
  (package
    (inherit firefox)
    (name "firefox-wayland")
    (native-inputs '())
    (inputs
     `(("bash" ,bash-minimal)
       ("firefox" ,firefox)))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((bash    (assoc-ref %build-inputs "bash"))
                (firefox (assoc-ref %build-inputs "firefox"))
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
