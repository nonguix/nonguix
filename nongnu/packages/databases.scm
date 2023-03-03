;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Krzysztof Baranowski <pharcosyle@gmail.com>

(define-module (nongnu packages databases)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:prefix license:))

(define-public datomic-cli-tools
  (package
    (name "datomic-cli-tools")
    (version "1.0.91")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://datomic-releases-1fc2183a.s3.amazonaws.com"
                           "/tools/datomic-cli/datomic-cli-" version ".zip"))
       (sha256
        (base32
         "1xicmbsig8f1p5r9rxkhndi0f9l9w421zf49rbx44yc6v0db523b"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "bin" #:include-regexp ("^\\./datomic"))
         ("README.txt" "share/doc/datomic/"))
       #:phases
       ,#~(modify-phases %standard-phases
            (add-after 'install 'make-scripts-executable
              (lambda _
                (for-each (lambda (f) (chmod f #o555))
                          (find-files (string-append #$output "/bin"))))))))
    (native-inputs
     (list unzip))
    (home-page "https://docs.datomic.com/cloud/index.html")
    (synopsis "Command-line tools for Datomic Cloud")
    (description "View and Manage analytics, logs, and access control gateways
for a Datomic Cloud instance on AWS.")
    (license license:asl2.0)
    (properties
     `((release-monitoring-url
        . "https://docs.datomic.com/cloud/releases.html")
       (upstream-name . "datomic-cli")))))
