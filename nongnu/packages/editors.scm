;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nongnu packages editors)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gtk)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (ice-9 match))

(define-public vscodium
  (package
    (name "vscodium")
    (version "1.98.2.25078")
    (source
     (let ((arch (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux" "arm64")
                   ("armhf-linux" "armhf")
                   (_ "x64")))
           (hash (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux"
                    "1x2b0x7cf7mhgdq16ywlb6qw2aypqp2pvr7an056ns3466spmb61")
                   ("armhf-linux"
                    "00fy5rifmy3f6fw3ikyr1l230cx6i38bmw4lkpnh5xr2s10kzvnp")
                   (_
                    "02mrksh3fn4k04aprcadg40cib2j9sm8vsamqpldfglwyjn8495a"))))
       (origin
        (method url-fetch)
        (uri
         (string-append
          "https://github.com/VSCodium/vscodium/releases/download/" version
          "/VSCodium-linux-" arch "-" version ".tar.gz"))
        (sha256
         (base32 hash)))))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:substitutable? #f
           #:wrapper-plan
           #~'(("opt/vscodium/codium" (("out" "/opt/vscodium"))))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (mkdir-p "opt/vscodium")
                   (invoke "tar" "-xvf" source "-C" "opt/vscodium")))
               (add-before 'install-wrapper 'install-entrypoint
                 (lambda _
                   (let* ((bin (string-append #$output "/bin")))
                     (delete-file (string-append #$output "/environment-variables"))
                     (mkdir-p bin)
                     (symlink (string-append #$output "/opt/vscodium/codium")
                              (string-append bin "/codium")))))
               (add-after 'install-entrypoint 'install-resources
                 (lambda _
                   (let* ((icons
                           (string-append #$output
                                          "/share/icons/hicolor/512x512/apps"))
                          (icon.png
                           (string-append #$output
                                          "/opt/vscodium/resources/app/"
                                          "resources/linux/code.png"))
                          (apps (string-append #$output "/share/applications")))
                     (mkdir-p icons)
                     (symlink icon.png
                              (string-append icons "/code.png"))
                     (mkdir-p apps)
                     (make-desktop-entry-file
                      (string-append apps "/" #$name ".desktop")
                      #:name "VSCodium"
                      #:generic-name "Text Editor"
                      #:exec (string-append #$output "/bin/codium --ozone-platform-hint=auto")
                      #:icon "code"
                      #:type "Application"
                      #:actions '("new-empty-window")
                      #:keywords '("vscode")
                      #:categories '("TextEditor" "Development"
                                     "IDE")
                      #:startup-notify #t
                      #:startup-w-m-class "Code"
                      #:comment
                      '(("en" "Code Editing. Redefined.")
                        (#f "Code Editing. Redefined.")))))))))
    (supported-systems '("armhf-linux" "aarch64-linux" "x86_64-linux"))
    (native-inputs
     (list tar))
    (inputs
     (list gdk-pixbuf))
    (home-page "https://vscodium.com/")
    (synopsis "Community-driven, freely-licensed binary distribution of VSCode")
    (description "VSCodium is a community-driven, freely-licensed binary
distribution of Microsoft’s editor VSCode.")
    (license license:expat)))
