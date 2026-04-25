;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024, 2025 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2024 antlers <antlers@illucid.net>

(define-module (nongnu packages productivity)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix multiarch-container)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module ((guix licenses) #:prefix free-license:))

(define anki-client
  (package
    (name "anki-client")
    (version "25.09")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ankitects/anki/releases/download/"
                       version "/anki-launcher-" version "-linux.tar.zst"))
       (file-name (string-append name "-" version ".tar.zst"))
       (sha256
        (base32
         "0adhpcmraqywsqnh3had184czxval3fvp2dc1hv48jg4fjpdvjwp"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      `'(("anki" "/share/anki/")
         ("launcher.amd64" "/share/anki/")
         ("uv.amd64" "/share/anki/")
         ("pyproject.toml" "/share/anki/")
         ("versions.py" "/share/anki/")
         (".python-version" "/share/anki/")
         ("anki.xpm" "/share/pixmaps/")
         ("anki.png" "/share/pixmaps/")
         ("anki.desktop" "/share/applications/")
         ("anki.1" "/share/man/man1/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/share/anki/anki")
                         (string-append bin "/anki"))))))))
    (synopsis "Spaced repetition flashcard program")
    (supported-systems '("x86_64-linux"))
    (description "Anki is a flashcard program that helps you spend more time
on challenging material, and less on what you already know.

Note: The first run of Anki needs to be done from a terminal in order for it
to fully install itself.")
    (home-page "https://apps.ankiweb.net/")
    (license free-license:agpl3+)))

(define-public anki
  (nonguix-container->package
   (nonguix-container
     (name "anki")
     (wrap-package anki-client)
     (run "/bin/anki")
     (union64
      (fhs-union
       (append fhs-min-libs
               `(("alsa-lib" ,alsa-lib)
                 ("dbus" ,dbus)
                 ("coreutils" ,coreutils) ; needs uname to check arch
                 ("expat" ,expat)
                 ("gcc:lib" ,gcc "lib")
                 ("flatpak-xdg-utils" ,flatpak-xdg-utils)
                 ("fontconfig" ,fontconfig)
                 ("freetype" ,freetype)
                 ("glib" ,glib)
                 ("libdrm" ,libdrm)
                 ("libx11" ,libx11)
                 ("libxcb" ,libxcb)
                 ("libxcomposite" ,libxcomposite)
                 ("libxdamage" ,libxdamage)
                 ("libxext" ,libxext)
                 ("libxfixes" ,libxfixes)
                 ("libxi" ,libxi)
                 ("libxkbcommon" ,libxkbcommon)
                 ("libxkbfile" ,libxkbfile)
                 ("libxrandr" ,libxrandr)
                 ("libxrender" ,libxrender)
                 ("libxshmfence" ,libxshmfence)
                 ("libxtst" ,libxtst)
                 ("mesa" ,mesa)
                 ("mit-krb5" ,mit-krb5)
                 ("mpv" ,mpv)
                 ("nspr" ,nspr)
                 ("nss" ,nss)
                 ("nss-certs" ,nss-certs)
                 ("eudev" ,eudev)
                 ("xcb-util-image" ,xcb-util-image)
                 ("xcb-util-cursor" ,xcb-util-cursor)
                 ("xcb-util-keysyms" ,xcb-util-keysyms)
                 ("xcb-util-renderutil" ,xcb-util-renderutil)
                 ("xcb-util-wm" ,xcb-util-wm)
                 ("zlib" ,zlib)
                 ("zstd:lib" ,zstd "lib")))
       #:system "x86_64-linux")))))

(define-public anytype
  (package
    (name "anytype")
    (version "0.52.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://anytype-release.fra1.cdn.digitaloceanspaces.com/"
                       name "_" version "_amd64.deb"))
       (file-name (string-append "anytype-" version ".deb"))
       (sha256
        (base32
         "0b6x20wqi428qki6379sjrvq7xfp7g4ghcxc0d2j9nv7vspqmyy6"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; almost 300MB
      #:substitutable? #f
      #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~(map (lambda (file)
               (string-append "opt/Anytype/" file))
             '("anytype"
               "chrome-sandbox"
               "chrome_crashpad_handler"
               "libEGL.so"
               "libffmpeg.so"
               "libGLESv2.so"
               "libvk_swiftshader.so"
               "libvulkan.so.1"
               "resources/app.asar.unpacked/node_modules/keytar/build/Release/keytar.node"
               "resources/app.asar.unpacked/node_modules/keytar/build/Release/obj.target/keytar.node"))
      #:install-plan
      #~'(("opt/" "/share")
          ("usr/share/" "/share"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'binary-unpack 'disable-auto-updates
            (lambda _
              (delete-file "opt/Anytype/resources/app-update.yml")))
          ;; We don't need regedit, a node library to interact with Windows
          ;; hosts.
          (add-after 'binary-unpack 'strip-regedit
            (lambda _
              (delete-file-recursively
               (string-append "opt/Anytype/resources/app.asar.unpacked/"
                              "node_modules/regedit"))))
          (add-after 'binary-unpack 'strip-python
            (lambda _
              (delete-file
               (string-append "opt/Anytype/resources/app.asar.unpacked/"
                              "node_modules/keytar/build/node_gyp_bins/python3"))))
          (add-before 'install 'patch-assets
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (usr/share "./usr/share")
                     (old-exe "/opt/Anytype/anytype")
                     (exe (string-append bin "/anytype")))
                (substitute* (string-append usr/share "/applications/anytype.desktop")
                  (((string-append "^Exec=" old-exe)) (string-append "Exec=" exe))))))
          (add-before 'install-wrapper 'symlink-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (exe (string-append bin "/anytype"))
                     (share (string-append #$output "/share/Anytype"))
                     (target (string-append share "/anytype")))
                (mkdir-p bin)
                (symlink target exe)
                (wrap-program exe
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,share)))))))))
    (inputs
     (list bzip2
           flac
           `(,gcc-14 "lib")
           gdk-pixbuf
           harfbuzz
           libexif
           libglvnd
           libpng
           libva
           libxscrnsaver
           opus
           pciutils
           snappy
           util-linux
           xdg-utils
           wget))
    (synopsis "Productivity and note-taking app")
    (supported-systems '("x86_64-linux"))
    (description "Anytype is an E2E encrypted, cross-platform, productivity and
note taking app. It stores all the data locally and allows for peer-to-peer
synchronization.")
    (home-page "https://anytype.io")
    (license (license:nonfree
              "https://github.com/anyproto/anytype-ts/blob/main/LICENSE.md"))))

(define-public zotero
  (package
    (name "zotero")
    (version "9.0.1")
    (source
     (origin
       ;; Can switch to git-fetch from Github too!
       (method url-fetch)
       (uri
        (string-append "https://download.zotero.org/client/release/"
                       version "/Zotero-" version "_linux-x86_64.tar.xz"))
       (sha256
        (base32
         "1m2yg4r5ipx8sm6cf6i43cz0c3xvrsaf2ip1hasc5ykh8as7pw5q"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      ;; ~70 MiB
      #:substitutable? #f
      #:validate-runpath? #t
      #:wrapper-plan
      #~'("zotero-bin")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-nonreproducible-features
            (lambda _
              ;; Disable Zotero's automatic update feature. Must unzip a
              ;; compressed file to make this change and re-zip it.
              (invoke "unzip" "app/omni.ja" "defaults/preferences/zotero.js")
              (substitute* "defaults/preferences/zotero.js"
                (("pref\\(\"app.update.enabled\", true\\)")
                 "pref(\"app.update.enabled\", false)")
                (("pref\\(\"app.update.auto\", true\\)")
                 "pref(\"app.update.auto\", false)")
                (("pref\\(\"extensions.zoteroOpenOfficeIntegration.skipInstallation\", false\\)")
                 "pref(\"extensions.zoteroOpenOfficeIntegration.skipInstallation\", true)"))
              (invoke "zip" "-mf" "app/omni.ja" "defaults/preferences/zotero.js")
              ;; Clean up after ourselves
              (delete-file-recursively "defaults/preferences")
              (delete-file-recursively "defaults")))
          (add-before 'install-wrapper 'install-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/zotero")
                         (string-append bin "/zotero")))))
          (add-after 'install 'create-desktop-file
            (lambda _
              (make-desktop-entry-file
               (string-append #$output "/share/applications/zotero.desktop")
               #:name "Zotero"
               #:type "Application"
               #:generic-name "Reference Management"
               #:exec (string-append #$output "/bin/zotero -url %U")
               #:icon "zotero"
               #:keywords '("zotero")
               #:categories '("Office" "Database")
               #:terminal #f
               #:startup-notify #t
               #:startup-w-m-class "zotero"
               ;; MIME-type list taken from Zotero's shipped .desktop file
               #:mime-type '("x-scheme-handler/zotero" "text/plain"
                             "application/x-research-info-systems"
                             "text/x-research-info-systems"
                             "text/ris"
                             "application/x-endnote-refer"
                             "application/x-inst-for-Scientific-info"
                             "application/mods+xml"
                             "application/rdf+xml"
                             "application/x-bibtex"
                             "text/x-bibtex"
                             "application/marc"
                             "application/vnd.citationstyles.style+xml")
               #:comment
               '(("en" "Collect, organize, cite, and share your research sources")
                 (#f "Collect, organize, cite, and share your research sources")))))
          (add-after 'install 'install-icons
            (lambda _
              (let ((icon-sizes (list 32 64 128)))
                (for-each
                 (lambda (size)
                   (mkdir-p (string-append #$output "/share/icons/hicolor/"
                                           size "x" size "/apps"))
                   (copy-file
                    (string-append "icons/icon" size ".png")
                    (string-append #$output "/share/icons/hicolor/"
                                   size "x" size "/apps/zotero.png")))
                 (map number->string icon-sizes))))))))
          ;; The zotero script that we wrap (which produces .zotero-real), has
          ;; this open file limit step done for us. If that script ever goes
          ;; away, then we can just uncomment this one.
          ;; (add-after 'install-wrapper 'raise-open-file-limit
          ;;   (lambda _
          ;;     (let ((file (string-append #$output "/bin/zotero")))
          ;;       (with-output-to-file file
          ;;         (lambda _
          ;;           (display
          ;;            (string-append
          ;;             "#!/bin/sh\n"
          ;;             ;; Raise the open files limit because Mozilla file
          ;;             ;; functions leave files open for a tiny bit longer than
          ;;             ;; necessary, so an installation with many translators and
          ;;             ;; styles can exceed the default 1024 file limit. ulimit
          ;;             ;; is a shell built-in, so we cannot use Guix's
          ;;             ;; program-file function.
          ;;             "ulimit -n 4096\n"
          ;;             #$output "/bin/zotero-bin" " -app " #$output "/application.ini" " \"$@\""))))
          ;;       (chmod file #o755))))
    (native-inputs (list zip unzip))
    (inputs (list dbus-glib libxt))
    (synopsis "Collect, organize, cite, and share your research sources")
    ;; If we build from source, then we may be able to support more
    ;; architectures. But Zotero is a Firefox/Electron app that uses a lot of
    ;; JavaScript, which may be problematic when packaging using Guix.
    (supported-systems '("x86_64-linux"))
    (description "Zotero is a research reference and bibliography tool.
Zotero helps you organize your research any way you want.  You can sort items
into collections and tag them with keywords.  Zotero instantly creates
references and bibliographies for any text editor, and directly inside Word,
LibreOffice, and Google Docs for over 10,000 citation styles.")
    (home-page "https://www.zotero.org")
    (license free-license:agpl3)))

(define-public obsidian
  (package
    (name "obsidian")
    (version "1.8.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/obsidianmd/obsidian-releases/releases/download/"
                       "v" version "/obsidian-" version
                       (match (or (%current-target-system) (%current-system))
                         ("x86_64-linux" "") ; x86_64 does not have any special indication
                         ("aarch64-linux" "-arm64")
                         ;; We should provide a default case.
                         (_ "unsupported"))
                       ".tar.gz"))
       (file-name (string-append "obsidian-" version ".tar.gz"))
       (sha256
        (base32
         (match (or (%current-target-system) (%current-system))
           ("x86_64-linux" "1kwhi5c56l97brp590f4qbi1z45ljm7g03wl3wdbz64mfn8zgqxl")
           ("aarch64-linux" "0gk34q3bjbxyihmji9qkpypzby2jy607iz2jdwk14sp9riz31zr5")
           ;; We need a valid base case for base32
           (_ "0000000000000000000000000000000000000000000000000000"))))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f ; TODO: fails on wrapped binary (.obsidian-real)
      #:substitutable? #f
      #:wrapper-plan
      #~(list "obsidian")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install-wrapper 'install-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (symlink (string-append #$output "/obsidian")
                         (string-append bin "/obsidian")))))
          ;; NOTE: Obsidian's icon SVG does not conform to SVG standards.
          (add-after 'install 'create-desktop-icons
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((convert (search-input-file inputs "/bin/convert"))
                    (svg (assoc-ref inputs "obsidian-logo-gradient.svg"))
                    (sizes (list "32x32" "48x48" "64x64" "128x128" "256x256"
                                 "512x512")))
                (for-each
                 (lambda (size)
                   (mkdir-p (string-append #$output "/share/icons/hicolor/"
                                           size
                                           "/apps"))
                   (invoke convert
                           "-background" "none"
                           "-resize" size
                           svg
                           (string-append #$output "/share/icons/hicolor/"
                                          size
                                          "/apps/obsidian.png")))
                 sizes))))
          (add-after 'install 'create-desktop-file
            (lambda _
              (make-desktop-entry-file
               (string-append #$output "/share/applications/obsidian.desktop")
               #:name "Obsidian"
               #:type "Application"
               #:generic-name "Markdown Editor"
               #:exec (string-append #$output "/bin/obsidian")
               #:icon "obsidian"
               #:keywords '("obsidian")
               #:categories '("Application" "Office")
               #:terminal #f
               #:startup-notify #t
               #:startup-w-m-class "obsidian"
               #:mime-type "x-scheme-handler/obsidian"
               #:comment
               '(("en" "Knowledge base")
                 (#f "Knowledge base"))))))))
    (native-inputs
     ;; imagemagick & inkscape needed to create desktop icons. We use the
     ;; stable versions because we only need them for generating icons.
     (list imagemagick/stable inkscape/pinned))
    (inputs
     (list
      (origin
        (method url-fetch)
        (uri "https://obsidian.md/images/obsidian-logo-gradient.svg")
        (sha256
         (base32 "100j8fcrc5q8zv525siapminffri83s2khs2hw4kdxwrdjwh36qi")))))
    (synopsis "Markdown-based knowledge base")
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (description "Obsidian is a powerful knowledge base that works on top of a
local folder of plain text Markdown files.  Obsidian makes following
connections frictionless, and with the connections in place, you can explore
all of your knowledge in the interactive graph view.  Obsidian supports
CommonMark and GitHub Flavored Markdown (GFM), along with other useful
notetaking features such as tags, LaTeX mathematical expressions, mermaid
diagrams, footnotes, internal links and embedding Obsidian notes or external
files.  Obsidian also has a plugin system to expand its capabilities.")
    (home-page "https://obsidian.md")
    (license (license:nonfree "https://obsidian.md/license"))))
