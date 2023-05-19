;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (nongnu packages k8s)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:))

(define-public k9s
  (package
    (name "k9s")
    (version "0.27.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/derailed/k9s/releases/download/v"
                    version "/k9s_Linux_amd64.tar.gz"))
              (sha256
               (base32
                "14j37kl8pbmbid4np48cfv5k8vic6ngnc4pjh01qr6szplg861z5"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("k9s" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "tar" "-xvf" #$source))))))
    (home-page "https://k9scli.io")
    (supported-systems '("x86_64-linux"))
    (synopsis "Kubernetes CLI To Manage Your Clusters In Style")
    (description
     "K9s provides a terminal UI to interact with your Kubernetes clusters.  The
aim of this project is to make it easier to navigate, observe and manage your
applications in the wild.  K9s continually watches Kubernetes for changes and
offers subsequent commands to interact with your observed resources.")
    (license license:asl2.0)))
