;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright (C) 2017, 2018 ng0 <gillmann@infotropique.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright (C) 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020, 2021 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 pineapples <guixuser6392@protonmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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
  #:use-module (guix packages)
  #:use-module (guix utils)

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
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

;; Update this id with every firefox update to it's release date.
;; It's used for cache validation and therefor can lead to strange bugs.
(define %firefox-build-id "20211017000000")

(define-public firefox
  (package
    (name "firefox")
    (version "93.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mozilla.org/pub/firefox/releases/"
                           version "/source/firefox-" version ".source.tar.xz"))
       (sha256
        (base32 "00kiz6hnwmz659cqndpalxhyj4jajd03b7r9hi5jig29b07hi3x7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((clang (assoc-ref %build-inputs "clang")))
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
           ;; "--with-system-nspr"
           ;; "--with-system-nss"

           ,(string-append "--with-clang-path="
                           clang "/bin/clang")
           ,(string-append "--with-libclang-path="
                           clang "/lib")

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
       #:imported-modules ,%cargo-utils-modules
       #:modules ((ice-9 regex)
                  (ice-9 ftw)
                  (srfi srfi-26)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
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
               (close-port port))
             #t))
         (add-after 'fix-preferences 'fix-ffmpeg-runtime-linker
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((ffmpeg (assoc-ref inputs "ffmpeg"))
                    (libavcodec (string-append ffmpeg "/lib/libavcodec.so")))
               ;; Arrange to load libavcodec.so by its absolute file name.
               (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                 (("libavcodec\\.so")
                  libavcodec))
               #t)))

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
                           "services"))
             #t)))
         (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
           (lambda _
             ;; Remove --frozen flag from cargo invokation, otherwise it'll
             ;; complain that it's not able to change Cargo.lock.
             ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
             (substitute* "build/RunCbindgen.py"
               (("\"--frozen\",") ""))
             #t))
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                               "/bin/autoconf"))
             (setenv "SHELL" (which "bash"))
             (setenv "CONFIG_SHELL" (which "bash"))
             (setenv "MACH_USE_SYSTEM_PYTHON" "1")

             ;; Use Clang, Clang is 2x faster than GCC
             (setenv "AR" "llvm-ar")
             (setenv "NM" "llvm-nm")
             (setenv "CC" "clang")
             (setenv "CXX" "clang++")

             (setenv "MOZ_NOSPAM" "1")
             ;; Firefox will write the timestamp to output, which is harmful for
             ;; reproducibility, so change it to a fixed date.
             (setenv "MOZ_BUILD_DATE" ,%firefox-build-id)

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
                                 (string-drop hash 8)))))
             #t))
         (replace 'install
           (lambda _ (invoke "./mach" "install")))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (ld-libs
                     (map (lambda (x)
                            (string-append (assoc-ref inputs x)
                                           "/lib"))
                          '("pulseaudio" "mesa"
                            "udev"      ;; For U2F and WebAuthn
                            ;; For hardware video acceleration via VA-API
                            "libva"
                            ;; For the integration of native notifications
                            "libnotify")))
                    (gtk-share (string-append (assoc-ref inputs "gtk+")
                                              "/share")))
               (wrap-program (car (find-files lib "^firefox$"))
                 `("LD_LIBRARY_PATH" prefix ,ld-libs)
                 `("XDG_DATA_DIRS" prefix (,gtk-share))
                 `("MOZ_LEGACY_PROFILES" = ("1"))
                 `("MOZ_ALLOW_DOWNGRADE" = ("1")))
               #t)))
         (add-after 'wrap-program 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((desktop-file "taskcluster/docker/firefox-snap/firefox.desktop")
                    (out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (substitute* desktop-file
                 (("^Exec=firefox") (string-append "Exec=" out "/bin/firefox"))
                 (("Icon=.*") "Icon=firefox\n")
                 (("NewWindow") "new-window")
                 (("NewPrivateWindow") "new-private-window"))
               (install-file desktop-file applications))
             #t))
         (add-after 'install-desktop-entry 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (icon-source-dir
                     (string-append
                      out "/lib/firefox/browser/chrome/icons/default")))
               (for-each
                (lambda (size)
                  (let ((dest (string-append out "/share/icons/hicolor/"
                                             size "x" size "/apps")))
                    (mkdir-p dest)
                    (symlink (string-append icon-source-dir
                                            "/default" size ".png")
                             (string-append dest "/firefox.png"))))
                '("16" "32" "48" "64" "128"))
               #t))))

       ;; Test will significantly increase build time but with little rewards.
       #:tests? #f

       ;; WARNING: Parallel build will consume lots of memory!
       ;; If you have encountered OOM issue in build phase, try disable it.
       ;; #:parallel-build? #f

       ;; Some dynamic lib was determined at runtime, so rpath check may fail.
       #:validate-runpath? #f))
    (inputs
     `(("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("freetype" ,freetype)
       ("ffmpeg" ,ffmpeg)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("hunspell" ,hunspell)
       ("icu4c" ,icu4c)
       ("jemalloc" ,jemalloc)
       ("libcanberra" ,libcanberra)
       ("libevent" ,libevent)
       ("libffi" ,libffi)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libnotify" ,libnotify)
       ;; ("libpng-apng" ,libpng-apng)
       ("libva" ,libva)
       ("libvpx" ,libvpx)
       ("libxcomposite" ,libxcomposite)
       ("libxft" ,libxft)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ;; ("nspr" ,nspr)
       ;; ("nss" ,nss)
       ("pango" ,pango)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("startup-notification" ,startup-notification)
       ("sqlite" ,sqlite)
       ("udev" ,eudev)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf-2.13)
       ("cargo" ,rust "cargo")
       ("clang" ,clang-10)
       ("llvm" ,llvm-10)
       ("m4" ,m4)
       ("nasm" ,nasm)
       ("node" ,node)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("rust" ,rust)
       ("rust-cbindgen" ,rust-cbindgen-0.19)
       ("which" ,which)
       ("yasm" ,yasm)))
    (home-page "https://mozilla.org/firefox/")
    (synopsis "Trademarkless version of Firefox")
    (description
     "Full-featured browser client built from Firefox source tree, without
the official icon and the name \"firefox\".")
    (license license:mpl2.0)))

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
