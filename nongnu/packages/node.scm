;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nongnu packages node)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages node)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:))

(define-public yarn
  (package
    (name "yarn")
    (version "1.22.22")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/yarnpkg/yarn/releases/download/v"
                                  version
                                  "/yarn-v"
                                  version
                                  ".tar.gz"))

              (sha256
               (base32
                "181nvynhhrbga3c209v8cd9psk6lqjkc1s9wyzy125lx35j889l8"))))
    (build-system copy-build-system)
    (inputs (list coreutils bash-minimal node-lts sed))
    (arguments
     (list #:install-plan
           #~`((,(string-append "yarn-v" #$version "/bin") "bin")
               (,(string-append "yarn-v" #$version "/lib") "lib")
               (,(string-append "yarn-v" #$version "/package.json")
                "lib/package.json"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'delete-powershell-entrypoints
                 (lambda _
                   (delete-file (string-append #$output "/bin/yarn.cmd"))
                   (delete-file (string-append #$output "/bin/yarnpkg.cmd"))))
               (add-after 'delete-powershell-entrypoints 'wrap-entrypoints
                 (lambda _
                   (for-each
                    (lambda (entrypoint)
                      (wrap-program (string-append #$output "/bin/" entrypoint)
                        `("PATH" = (,(string-append
                                      #$output "/bin:"
                                      #$(this-package-input "bash-minimal") "/bin:"
                                      #$(this-package-input "coreutils") "/bin:"
                                      #$(this-package-input "sed") "/bin:"
                                      #$(this-package-input "node") "/bin")))))
                    '("yarn" "yarnpkg")))))))
    (home-page "https://yarnpkg.com/")
    (synopsis "Dependency management tool for JavaScript")
    (description
     "Yarn is a dependency management tool for JavaScript.  It acts as a
drop-in replacement for @code{node}'s @command{npm}.")
    (license license:bsd-2)))
