;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>

(define-module (nongnu packages wine)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages wine))

(define-public winetricks
  (package
    (name "winetricks")
    (version "20250102")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Winetricks/winetricks")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02hdask7wn9vk4i0s43cyzg2xa9aphskbrn8slywsbic6rasyv9a"))))
    (build-system gnu-build-system)
    (inputs
     (list cabextract
           p7zip
           perl
           ;; unrar ; TODO: Include unrar?  It is referenced in the source.
           unzip
           wget
           zenity))
    (arguments
     (list
      #:tests? #f
      ;; TODO: Checks need bashate, shellcheck (in Guix), and checkbashisms.
      #:make-flags #~(list (string-append "DESTDIR=" #$output)
                           "PREFIX=")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'install 'wrap-program
            ;; The script relies on WINETRICKS_GUI being exactly "zenity", so
            ;; we can't patch the path directly.  Probably same for other dependencies.
            (lambda _
              (let* ((winetricks (string-append #$output "/bin/winetricks"))
                     (paths (map
                             (lambda (p) (string-append p "/bin"))
                             (list #$(this-package-input "cabextract")
                                   #$(this-package-input "p7zip")
                                   #$(this-package-input "perl")
                                   #$(this-package-input "unzip")
                                   #$(this-package-input "wget")
                                   #$(this-package-input "zenity")))))
                (wrap-program winetricks
                  `("PATH" prefix ,paths)))))
          (add-after 'install 'patch-perl-path
            (lambda _
              (let* ((perl (string-append #$(this-package-input "perl")
                                          "/bin/perl"))
                     (winetricks (string-append #$output "/bin/winetricks")))
                (substitute* winetricks
                  (("#!/usr/bin/env perl") (string-append "#!" perl)))))))))
    (home-page "https://github.com/Winetricks/winetricks")
    (synopsis "Easy way to work around problems in Wine")
    (description "Winetricks is an easy way to work around problems in Wine.
It has a menu of supported games/apps for which it can do all the workarounds
automatically.  It also allows the installation of missing nonfree DLLs and
tweaking of various Wine settings.")
    (license license:lgpl2.1)))

;; Upstream Guix dxvk does not build anymore because of missing mingw compiler.
(define-public dxvk-1.7 ; TODO: Can we remove this in favour of `dxvk' without breaking `guix pull'?
  (package
    (name "dxvk")
    (version "1.7.3")
    (home-page "https://github.com/doitsujin/dxvk/")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/doitsujin/dxvk/releases/download/v"
                    version "/dxvk-" version ".tar.gz") )
              (sha256
               (base32
                "185b80h7l62nv8k9rp32fkn00aglwcw9ccm6bx2n7bdpar149hp4"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(,@,(if (string=? (or (%current-target-system) (%current-system))
                          "x86_64-linux")
                '(list '("x64" "share/dxvk/lib"))
                ''())
         ("x32" ,,(if (string=? (or (%current-target-system) (%current-system))
                                "i686-linux")
                       "share/dxvk/lib"
                       "share/dxvk/lib32"))
         ("setup_dxvk.sh" "bin/setup_dxvk"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-setup
           (lambda* (#:key inputs outputs system #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libs "../share/dxvk")
                    (wine (assoc-ref inputs "wine")))
               (substitute* (string-append out "/bin/setup_dxvk")
                 (("wine=\"wine\"")
                  (string-append "wine=" wine "/bin/wine"))
                 (("wine64=\"wine64\"")
                  (string-append "wine64=" wine "/bin/wine64"))
                 (("wineboot=\"wineboot\"")
                  (string-append "wineboot=" wine "/bin/wineboot"))
                 (("\"\\$wine_path/\\$wine\"")
                  "\"$wine_path/wine\"")
                 (("x32") (if (string=? system "x86_64-linux")
                              (string-append libs "/lib32")
                              (string-append libs "/lib")))
                 (("x64") (string-append libs "/lib")))))))))
    (inputs
     `(("wine" ,(match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux" wine64-staging)
                  (_ wine-staging)))))
    (synopsis "Vulkan-based D3D9, D3D10 and D3D11 implementation for Wine")
    (description "A Vulkan-based translation layer for Direct3D 9/10/11 which
allows running complex 3D applications with high performance using Wine.

Use @command{setup_dxvk} to install the required libraries to a Wine prefix.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:zlib)))

(define-public dxvk-next
  (package
    (name "dxvk")
    (version "2.0")
    (home-page "https://github.com/doitsujin/dxvk/")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/doitsujin/dxvk/releases/download/v"
                    version "/dxvk-" version ".tar.gz") )
              (sha256
               (base32
                "0xr4lq4zdmqwxh5x19am2y4lvy6q6s6bl1nfr4ixfgy2l2sghliq"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(,@,(if (string=? (or (%current-target-system) (%current-system))
                          "x86_64-linux")
                '(list '("x64" "share/dxvk/lib"))
                ''())
         ("x32" ,,(if (string=? (or (%current-target-system) (%current-system))
                                "i686-linux")
                       "share/dxvk/lib"
                       "share/dxvk/lib32"))
         ("setup_dxvk.sh" "bin/setup_dxvk"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-setup
           (lambda* (#:key inputs outputs system #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libs "../share/dxvk")
                    (wine (assoc-ref inputs "wine")))
               (substitute* (string-append out "/bin/setup_dxvk")
                 (("wine=\"wine\"")
                  (string-append "wine=" wine "/bin/wine"))
                 (("wine64=\"wine64\"")
                  (string-append "wine64=" wine "/bin/wine64"))
                 (("wineboot=\"wineboot\"")
                  (string-append "wineboot=" wine "/bin/wineboot"))
                 (("\"\\$wine_path/\\$wine\"")
                  "\"$wine_path/wine\"")
                 (("x32") (if (string=? system "x86_64-linux")
                              (string-append libs "/lib32")
                              (string-append libs "/lib")))
                 (("x64") (string-append libs "/lib")))))))))
    (inputs
     `(("wine" ,(match (or (%current-target-system)
                           (%current-system))
                  ("x86_64-linux" wine64-staging)
                  (_ wine-staging)))))
    (synopsis "Vulkan-based D3D9, D3D10 and D3D11 implementation for Wine")
    (description "A Vulkan-based translation layer for Direct3D 9/10/11 which
allows running complex 3D applications with high performance using Wine.

Use @command{setup_dxvk} to install the required libraries to a Wine prefix.")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:zlib)))

;; Keep 1.10 since it's backward-compatible with older hardware unlike 2.*
;; See https://github.com/doitsujin/dxvk/releases/tag/v2.0
(define-public dxvk-1.10
  (package
    (inherit dxvk-1.7)
    (version "1.10.3")
    (home-page "https://github.com/doitsujin/dxvk/")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/doitsujin/dxvk/releases/download/v"
                    version "/dxvk-" version ".tar.gz"))
              (sha256
               (base32
                "1ijkznb8asqg18blhs6f82g67xpncjp7i17rg7451d314y8kq6ld"))))))
