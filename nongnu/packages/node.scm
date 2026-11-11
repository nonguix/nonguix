;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>

(define-module (nongnu packages node)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages node)
  #:use-module (gnu packages node-xyz)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system node)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:))

(define-public node-t1ckbase-vscode-langservers-extracted
  (package
    (name "node-t1ckbase-vscode-langservers-extracted")
    (version "2.0.5")
    (source
     (origin
       ;; Requires 'bun' to build, which fetches pre-built files from
       ;; the https://github.com/microsoft/vscode/tree/main/extensions
       ;; project, which requires 'gulp' to build.
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@t1ckbase/vscode-langservers-extracted"
             "/-/vscode-langservers-extracted-" version ".tgz"))
       (sha256
        (base32 "1bilhh4c7r7y746598gaymr7jk6vcr31v43lf1ml9ga4lijc2lnn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f                       ;requires 'bun'
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies)))))))
    (native-inputs (list node-typescript))
    (inputs (list node-vscode-markdown-languageserver))
    (home-page "https://github.com/T1ckbase/vscode-langservers-extracted")
    (synopsis "Language servers for CSS, HTML, JSON, Markdown and ESLint")
    (description "This package provides the CSS, HTML, JSON and Markdown and
ESLint language servers extracted from the Visual Studio Code sources.")
    (license license:expat)))

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
               (add-after 'delete-powershell-entrypoints 'patch-shebang-in-cli
                 (lambda _
                   (substitute* (string-append #$output "/lib/cli.js")
                     (("#!/bin/sh")
                      (string-append "#!" #$(this-package-input "bash-minimal")
                                     "/bin/sh")))))
               (add-after 'patch-shebang-in-cli 'wrap-entrypoints
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
