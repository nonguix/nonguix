;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 John Kehayias <john@guixotic.coop>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>

(define-module (nongnu packages gradle)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public gradle
  (package
    (name "gradle")
    (version "9.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://services.gradle.org/distributions/gradle-"
             version "-bin.zip"))
       (sha256
        (base32 "0gxwqmv60yxgyjrqlimkk1449id999ffwicghd1szcdwisglrx3j"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan ''(("bin" "./")
                             ("lib" "./"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (wrap-program (string-append #$output "/bin/gradle")
                     `("JAVA_HOME" =
                       (,(dirname
                          (dirname
                           (search-input-file inputs "bin/javac")))))))))))
    (native-inputs (list unzip))
    (inputs (list `(,openjdk "jdk")))
    (home-page "https://gradle.org/")
    (synopsis "Flexible build automation tool for JVM")
    (description "Gradle is a build tool with a focus on build automation and
support for multi-language development, with an elegant and extensible
declarative build language.  Gradle supports build automation across multiple
languages and platforms including Java, Scala, Kotlin, Javascript, Android,
C/C++, and Groovy, and is closely integrated with development tools and
continuous integration servers including Eclipse, IntelliJ, and Jenkins.")
    (license asl2.0)))
