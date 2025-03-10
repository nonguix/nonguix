;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages clojure)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

;; This is a hidden package, as it does not really serve a purpose on its own.
(define leiningen-jar
  (package
    (name "leiningen-jar")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri "https://codeberg.org/attachments/43cebda5-a7c2-405b-b641-5143a00051b5")
              (file-name "leiningen-standalone.jar")
              (sha256
               (base32
                "0d5vmpyp9ddxpj1s5c60fv2f5iimz1chbgfhchlaqxa0sfx9jwnj"))))
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
    (version "2.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/leiningen/leiningen.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xvniav6iy1yrbamvbg8i3dq8issiczv3rbig2yc3nm08d2q0rig"))))
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
   (version "2025.01.16")
   (source (origin
             (method url-fetch/zipbomb)
             (uri (string-append
                   "https://github.com/clj-kondo/clj-kondo/releases/download/v"
                   version "/clj-kondo-" version "-linux-amd64.zip"))
             (sha256
              (base32
               "0w9a2lz9xr5446bav2fp70jfinlm9zrl0iv9s0h0b6jczqx2f977"))))
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
    (version "2024.03.13-13.11.00")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/clojure-lsp/clojure-lsp"
                                  "/releases/download/" version
                                  "/clojure-lsp-native-static-linux-amd64.zip"))
              (sha256
               (base32
                "1l6w55aragyf8rzy087iqw97xnpih5syjwhf0jwbgrqps2k44ms5"))))
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

(define-public babashka
  (package
    (name "babashka")
    (version "1.3.189")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://github.com/babashka/babashka"
                                  "/releases/download/v" version "/babashka-"
                                  version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "1gzra3y5iljjqi4rj1qxr3yniqla3qnhv881gkzrp788fwsvlmwv"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       '(("bb" ("gcc" "zlib")))
       #:install-plan
       '(("./bb" "/bin/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chmod
           (lambda _
             (chmod "bb" #o755))))))
    (inputs (list `(,gcc "lib") zlib))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/babashka/babashka")
    (synopsis "Native, fast starting Clojure interpreter for scripting")
    (description "Babashka is a native Clojure interpreter for scripting with
fast startup.  Its main goal is to leverage Clojure in places where you would
be using bash otherwise.")
    (license license:epl1.0)))
