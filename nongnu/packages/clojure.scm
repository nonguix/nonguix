;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>

(define-module (nongnu packages clojure)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

;; This is a hidden package, as it does not really serve a purpose on its own.
(define leiningen-jar
  (package
    (name "leiningen-jar")
    (version "2.9.10")
    (source (origin
              (method url-fetch)
              (uri "https://codeberg.org/attachments/895a0a0d-f20d-4580-a277-e06b5eec3b6b")
              (file-name "leiningen-standalone.jar")
              (sha256
               (base32
                "1ja9q8lav83h5qhayjgc39f6yyvk1n5f6gfwznn561xm007m6a52"))))
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
    (version "2.9.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/leiningen/leiningen.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hl62ykq7sckfpgg5l3wpzq5smh6s572xnadr988vpl97l2x1g4y"))))
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
   (version "2022.06.22")
   (source (origin
             (method url-fetch/zipbomb)
             (uri (string-append
                   "https://github.com/clj-kondo/clj-kondo/releases/download/v"
                   version "/clj-kondo-" version "-linux-amd64.zip"))
             (sha256
              (base32
               "057h48kf14pdnnyvgmbqkga1bspbr4ag22q2279s14c2c9bcinzz"))))
   (build-system binary-build-system)
   (arguments
    `(#:patchelf-plan
      '(("clj-kondo" ("gcc:lib" "zlib")))
      #:install-plan
      '(("clj-kondo" "/bin/"))
      #:phases
      (modify-phases %standard-phases
         (add-after 'unpack 'chmod
           (lambda _
             (chmod "clj-kondo" #o755))))))
   (native-inputs
    `(("unzip" ,unzip)))
   (inputs
    `(("gcc:lib" ,gcc "lib")
      ("zlib" ,zlib)))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/clj-kondo/clj-kondo")
   (synopsis  "Linter for Clojure code")
   (description "Clj-kondo performs static analysis on Clojure, ClojureScript
and EDN, without the need of a running REPL.")
   (license license:epl1.0)))
