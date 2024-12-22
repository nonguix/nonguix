;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nonguix build chromium-binary-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((nonguix build binary-build-system) #:prefix binary:)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            chromium-binary-build))

;; Commentary:
;;
;; Builder-side code of the Chromium binary build procedure.
;;
;; Code:

(define* (install-wrapper #:key inputs outputs #:allow-other-keys)
  (let* ((output (assoc-ref outputs "out"))
         (bin (string-append output "/bin"))
         (fontconfig-minimal (assoc-ref inputs "fontconfig"))
         (nss (assoc-ref inputs "nss"))
         (wrap-inputs (map cdr inputs))
         (lib-directories
          (search-path-as-list '("lib") wrap-inputs))
         (bin-directories
          (search-path-as-list
           '("bin" "sbin" "libexec")
           wrap-inputs)))
    (for-each
     (lambda (exe)
       (display (string-append "Wrapping " exe "\n"))
       (wrap-program exe
        `("FONTCONFIG_PATH" ":" prefix
          (,(string-join
             (list
              (string-append fontconfig-minimal "/etc/fonts")
              output)
             ":")))
        `("PATH" ":" prefix
          (,(string-join
             (append
              bin-directories
              (list
               bin))
             ":")))
        `("LD_LIBRARY_PATH" ":" prefix
          (,(string-join
             (append
              lib-directories
              (list
               (string-append nss "/lib/nss")
               output))
             ":")))
        ;; Give a hint to Electron-based apps to detect if Wayland or X11 should
        ;; be used.
        ;; NOTE: The env-var version of this CLI arg was added in Electron >=28
        `("ELECTRON_OZONE_PLATFORM_HINT" ":" =
          ("auto"))))
     (map
      (lambda (exe) (string-append bin "/" exe))
      (filter
       (lambda (exe) (not (string-prefix? "." exe)))
       (scandir bin))))
    #t))

(define %standard-phases
  ;; Everything is as with the binary-build-system except for the
  ;; `install-wrapper' phase.
  (modify-phases binary:%standard-phases
    (add-after 'install 'install-wrapper install-wrapper)))

(define* (chromium-binary-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; chromium-binary-build-system.scm ends here
