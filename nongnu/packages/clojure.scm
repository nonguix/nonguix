;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2025 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2025 Mathieu Lirzin <mthl@reuz.fr>

(define-module (nongnu packages clojure)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

;; This is a hidden package, as it does not really serve a purpose on its own.
(define leiningen-jar
  (package
    (name "leiningen-jar")
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://codeberg.org/leiningen/leiningen/releases/download/"
                    version "/leiningen-" version "-standalone.jar"))
              (file-name "leiningen-standalone.jar")
              (sha256
               (base32
                "18hsm37px3yk2v9mdbai76fpsa6iwqcyflnbgkr885v3mxrsa8dp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (jar-dir (string-append %output "/share/")))
                     (mkdir-p jar-dir)
                     (copy-file source
                                (string-append jar-dir "leiningen-standalone.jar"))))))
    (home-page "https://leiningen.org")
    (synopsis "Automate Clojure projects without setting your hair on fire")
    (description "Leiningen is a Clojure tool with a focus on project
automation and declarative configuration.  It gets out of your way and
lets you focus on your code.")
    (license license:epl1.0)))

(define-public leiningen
  (package
    (inherit leiningen-jar)
    (name "leiningen")
    (version "2.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/leiningen/leiningen.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0alr5hahfc4wzfkhzm7cdbk10p2z0rjv0nbnr6nwf29sa650b8cf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (replace 'install
                    (lambda _
                      (let* ((lein-pkg (string-append (assoc-ref %build-inputs "source") "/bin/lein-pkg"))
                             (lein-jar (string-append (assoc-ref  %build-inputs "leiningen-jar")
                                                      "/share/leiningen-standalone.jar"))
                             (bin-dir (string-append %output "/bin"))
                             (lein (string-append bin-dir "/lein")))
                        (mkdir-p bin-dir)
                        (copy-file lein-pkg lein)
                        (patch-shebang lein)
                        (chmod lein #o555)
                        (substitute* lein
                          (("LEIN_JAR=.*") (string-append "LEIN_JAR=" lein-jar)))
                        #t))))))
    (inputs
     `(("leiningen-jar" ,leiningen-jar)))))

(define-public clj-kondo
 (package
   (name "clj-kondo")
   (version "2026.01.12")
   (source (origin
             (method url-fetch/zipbomb)
             (uri (string-append
                   "https://github.com/clj-kondo/clj-kondo/releases/download/v"
                   version "/clj-kondo-" version "-linux-amd64.zip"))
             (sha256
              (base32
               "1yangvsa469d884hw36j6qv102lhq809m3w5fwmry1qqd7x5bzg4"))))
   (build-system binary-build-system)
   (arguments
    (list #:patchelf-plan `'(("clj-kondo" ("gcc" "zlib")))
          #:install-plan `'(("./clj-kondo" "/bin/"))
          #:phases #~(modify-phases %standard-phases
                       (add-after 'unpack 'chmod
                         (lambda _
                           (chmod "clj-kondo" #o755))))))
   (native-inputs
    (list unzip))
   (inputs
    (list `(,gcc "lib")
          zlib))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/clj-kondo/clj-kondo")
   (synopsis  "Linter for Clojure code")
   (description "Clj-kondo performs static analysis on Clojure, ClojureScript
and EDN, without the need of a running REPL.")
   (license license:epl1.0)))

(define-public clojure-lsp
  (package
    (name "clojure-lsp")
    (version "2025.04.23-18.16.46")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/clojure-lsp/clojure-lsp"
                                  "/releases/download/" version
                                  "/clojure-lsp-native-static-linux-amd64.zip"))
              (sha256
               (base32
                "0d5cmdjx576109vnsrmzavjl9iqp72wzbvck42bd54lzyw2skmgj"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       '(("./clojure-lsp" "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chmod
           (lambda _
             (chmod "./clojure-lsp" #o755))))))
    (inputs (list `(,gcc "lib") zlib))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/clojure-lsp/clojure-lsp")
    (synopsis "Clojure & ClojureScript Language Server (LSP) implementation")
    (description "This package provides a Language Server for Clojure and ClojureScript
languages.  The goal of this project is to bring great editing tools for
Clojure/Clojurescript to all editors and programatically via its CLI and API.
It aims to work alongside you to help you navigate, identify and fix errors,
perform refactors and more.")
    (license license:expat)))

(define-public clojure-tools-bin
  (package
    (inherit clojure-tools)
    (name "clojure-tools-bin")
    (source
     (origin
       (inherit (package-source clojure-tools))
       (snippet #f)))
    (arguments
     (substitute-keyword-arguments (package-arguments clojure-tools)
       ((#:install-plan plan)
        #~(cons (list (format #f "clojure-tools-~a.jar"
                              #$(package-version this-package))
                      "lib/clojure/libexec/")
                #$plan))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'copy-tools-deps-alpha-jar)))))))

(define-public babashka-clojure-tools
  ;; Babashka code is using upstream `clojure-tools-VERSION.jar` containing
  ;; bytecode, which is removed Guix clojure-tools output.
  (package
    (inherit clojure-tools-bin)
    (name "babashka-clojure-tools")
    ;; Version must match the one hardcoded in #'borkdude.deps/version.
    (version "1.12.4.1582")
    (source
     (origin
       (inherit (package-source clojure-tools-bin))
       (uri (string-append "https://download.clojure.org/install/clojure-tools-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "1k5s3sdz72j2faz4rsbgqq4i5f30npf6ms0sg3lp764bx0x53y5v"))))))

(define-public babashka
  (package
    (name "babashka")
    (version "1.12.213")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/babashka/babashka"
                                  "/releases/download/v" version "/babashka-"
                                  version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "14vrjddia92cz1da67nyjbq0y0jsdnn0235xma5jnkppqvy9jxqn"))))
    (build-system binary-build-system)
    (arguments
     (list #:patchelf-plan
           ''(("bb" ("zlib")))
           #:install-plan
           ''(("bb" "/bin/"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'chmod
                 (lambda _
                   (chmod "bb" #o755)))
               (add-after 'patch-shebangs 'wrap-programs
                 (lambda _
                   (let ((openjdk #$(this-package-input "openjdk"))
                         (clojure-tools #$(this-package-input
                                           "babashka-clojure-tools")))
                     (wrap-program (string-append #$output "/bin/bb")
                       `("DEPS_CLJ_TOOLS_DIR" =
                         ,(list (string-append clojure-tools "/lib/clojure")))
                       `("JAVA_HOME" = ,(list openjdk))))))
               (add-after 'validate-runpath 'validate-classpath
                 (lambda _
                   (let ((bb (string-append #$output "/bin/bb")))
                     (and (invoke bb "-e" "(System/exit 0)")
                          (invoke bb "-e" "(+ 1 1)"))))))))
    (inputs (list babashka-clojure-tools openjdk zlib))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/babashka/babashka")
    (synopsis "Native, fast starting Clojure interpreter for scripting")
    (description "Babashka is a native Clojure interpreter for scripting with
fast startup.  Its main goal is to leverage Clojure in places where you would
be using bash otherwise.")
    (license license:epl1.0)))
