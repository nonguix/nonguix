;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nongnu packages password-utils)
  #:use-module (gnu packages gnome)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((guix licenses) #:prefix free-license:))

(define-public bitwarden-desktop
  (package
    (name "bitwarden-desktop")
    (version "2025.6.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/bitwarden/clients/releases/download/"
                       "desktop-v" version "/Bitwarden-" version "-amd64.deb"))
       (file-name (string-append name "-" version ".deb"))
       (sha256
        (base32
         "15zd3prfkgh52mjjw1904hpdakmp8gwn0151x5rqi4m29j2cmmrq"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
      #:wrapper-plan
      #~(map (lambda (file)
               (string-append "opt/Bitwarden/" file))
             '("bitwarden-app"
               "chrome-sandbox"
               "chrome_crashpad_handler"
               "libEGL.so"
               "libffmpeg.so"
               "libGLESv2.so"
               "libvk_swiftshader.so"
               "libvulkan.so.1"
               "resources/app.asar.unpacked/node_modules/@bitwarden/desktop-napi/desktop_napi.linux-x64-musl.node"))
      #:install-plan
      #~'(("opt/" "/share")
          ("usr/share/" "/share"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'patch-assets
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (usr/share "./usr/share")
                     (old-exe "/opt/Bitwarden/bitwarden")
                     (exe (string-append bin "/bitwarden")))
                (substitute* (string-append usr/share "/applications/bitwarden.desktop")
                  (((string-append "^Exec=" old-exe)) (string-append "Exec=" exe))))))
          (add-before 'install-wrapper 'symlink-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (share (string-append #$output "/share/Bitwarden")))
                (mkdir-p bin)
                (for-each
                 (lambda (entrypoint)
                   (define exe (string-append bin "/" entrypoint))
                   (define target (string-append share "/" entrypoint))
                   (symlink target exe)
                   (wrap-program exe
                     `("LD_LIBRARY_PATH" = (,share))))
                 '("bitwarden" "bitwarden-app"))))))))
    (inputs
     (list libsecret))
    (synopsis
     "Access your sensitive information on any device with secure cloud sync")
    (supported-systems '("x86_64-linux"))
    (description
     "Bitwarden is a password manager for securely storing, managing, and
sharing sensitive online data such as passwords, passkeys, and credit cards.")
    (home-page "https://bitwarden.com")
    (license free-license:gpl3)))
