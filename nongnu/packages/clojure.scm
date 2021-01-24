;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nongnu packages clojure)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

;; This is a hidden package, as it does not really serve a purpose on its own.
(define leiningen-jar
  (package
    (name "leiningen-jar")
    (version "2.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/technomancy/leiningen/releases/download/"
                                  version "/leiningen-" version "-standalone.zip"))
              (file-name "leiningen-standalone.jar")
              (sha256
               (base32
                "1shyvg1471sc3bv4h3ax51626xw8a8w05f43bny6gmp8pyc0qjfz"))))
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
    (version "2.9.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/technomancy/leiningen.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w6f1wg6qpa8fa7zmbp3zj5rgvy8jb4mx8885g1cxbn021r4npis"))))
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
