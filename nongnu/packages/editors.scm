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
    (version "1.92.1.24225")
    (source
     (let ((arch (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux" "arm64")
                   ("armhf-linux" "armhf")
                   (_ "x64")))
           (hash (match (or (%current-target-system) (%current-system))
                   ("aarch64-linux"
                    "0m5x9v577h8n16ypzb1y2066alc59v5bw7jiqp2xr7g20s9kb0vy")
                   ("armhf-linux"
                    "047gz12gx8pa5aglykd0785l6i9ivsn4kkgfhi5l0y4jh8hjys8c")
                   (_
                    "1w1rhbbk177yz85csck3sax51qnvgaip9w238dmzb4a50ikfnp23"))))
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
           #~'("opt/vscodium/codium")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (mkdir-p "opt/vscodium")
                   (invoke "tar" "-xvf" #$source "-C" "opt/vscodium")))
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
                        (#f "Code Editing. Redefined."))))))
               (add-after 'install-wrapper 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/bin/codium")
                     `("LD_LIBRARY_PATH" ":"
                       prefix
                       (,(string-join
                          (list (string-append #$output "/opt/vscodium"))
                          ":")))))))))
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
