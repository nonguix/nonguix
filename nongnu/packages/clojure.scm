;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2025 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2025, 2026 Mathieu Lirzin <mthl@reuz.fr>

(define-module (nongnu packages clojure)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

(define-public leiningen
  (package
    (name "leiningen")
    (version "2.13.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://codeberg.org/leiningen/leiningen.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c4b74k3w6i1kkinf5lr3fyw5977iyznch3qa4xgss2rq4aj7z15"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key source #:allow-other-keys)
              (let* ((lein-pkg (string-append source "/bin/lein-pkg"))
                     (bin-dir (string-append #$output "/bin"))
                     (lein (string-append bin-dir "/lein")))
                (mkdir-p bin-dir)
                (copy-file lein-pkg lein)
                (patch-shebang lein)
                (chmod lein #o555)
                (substitute* lein
                  (("LEIN_JAR=.*")
                   (string-append
                    "LEIN_JAR="
                    #+(this-package-native-input "leiningen-standalone.jar"))))))))))
    (native-inputs
     ;; While the source code is hosted on Codeberg, GitHub remains the
     ;; canonical download location for the standalone jar, see:
     ;; <https://codeberg.org/leiningen/leiningen/issues/114>.
     (list (origin
             (method url-fetch)
             (uri (string-append "https://github.com/technomancy/leiningen"
                                 "/releases/download/" version
                                 "/leiningen-" version "-standalone.jar"))
             (file-name "leiningen-standalone.jar")
             (sha256
              (base32
               "0rbm44kjypic7lvymagqvm2gnxsmi19glg8y4kij8y9wdkq32ljz")))))
    (home-page "https://leiningen.org")
    (synopsis "Automate Clojure projects without setting your hair on fire")
    (description "Leiningen is a Clojure tool with a focus on project
automation and declarative configuration.  It gets out of your way and
lets you focus on your code.")
    (license license:epl1.0)))

(define-public clj-kondo
 (package
   (name "clj-kondo")
   (version "2026.08.04")
   (source (origin
             (method url-fetch/zipbomb)
             (uri (string-append
                   "https://github.com/clj-kondo/clj-kondo/releases/download/v"
                   version "/clj-kondo-" version "-linux-amd64.zip"))
             (sha256
              (base32
               "1lb19rh0amj8n3l6gyi5lvi8z6mnd2cvmjggpglkif5wj7nb6fyx"))))
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

(define-public cljfmt
  (package
    (name "cljfmt")
    (version "0.16.6")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append
                    "https://github.com/weavejester/cljfmt/releases/download/"
                    version "/cljfmt-" version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "0ds2yfrxiibh7m014300kry729wp96dxyaaxammaxnxc3sm2mnrr"))))
    (build-system binary-build-system)
    (arguments
     (list #:patchelf-plan `'(("cljfmt" ("gcc" "zlib")))
           #:install-plan `'(("./cljfmt" "/bin/"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chmod
                          (lambda _
                            (chmod "cljfmt" #o755))))))
    (native-inputs
     (list unzip))
    (inputs
     (list `(,gcc "lib")
           zlib))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/weavejester/cljfmt")
    (synopsis  "Formatter for Clojure code")
    (description "cljfmt is a tool for detecting and fixing formatting errors
in Clojure code.")
    (license license:epl1.0)))

(define-public clojure-lsp
  (package
    (name "clojure-lsp")
    (version "2026.07.06-14.34.19")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/clojure-lsp/clojure-lsp"
                                  "/releases/download/" version
                                  "/clojure-lsp-native-static-linux-amd64.zip"))
              (sha256
               (base32
                "160r24kqcncy81zzb3qm0sn9x3b46m9bxhsqh1fl00p6j0jfl134"))))
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
     (substitute-keyword-arguments arguments
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
    (version "1.12.6.1673")
    (source
     (origin
       (inherit (package-source clojure-tools-bin))
       (uri (string-append "https://download.clojure.org/install/clojure-tools-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "0g02xj14vnd7j0pv9wh5a6ydax06f4nzkbz2q89szmbmis2r94gy"))))))

(define-public babashka
  (package
    (name "babashka")
    (version "1.13.226")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/babashka/babashka"
                                  "/releases/download/v" version "/babashka-"
                                  version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "1yva3zkz80bvpff1cr37cllsgs9qn903p84nwqk0gw37wiyly5y0"))))
    (build-system binary-build-system)
    (arguments
     (list #:patchelf-plan
           ;; Only set the interpreter: every library except glibc is
           ;; statically linked.
           ''(("bb"))
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
    (inputs (list babashka-clojure-tools openjdk))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/babashka/babashka")
    (synopsis "Native, fast starting Clojure interpreter for scripting")
    (description "Babashka is a native Clojure interpreter for scripting with
fast startup.  Its main goal is to leverage Clojure in places where you would
be using bash otherwise.")
    (license license:epl1.0)))
