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

(define-public katenary
  (package
    (name "katenary")
    (version "2.0.0-beta2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/metal3d/katenary/releases/download/"
                    version "/katenary-linux-amd64"))
              (sha256
               (base32
                "0vk5c82bf5aasrgz2b7qdjlbmlcjha0r3swmrbs9y5mms18y7m3i"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("katenary" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (copy-file #$source "./katenary")
              (chmod "katenary" #o644)))
          (add-before 'install 'chmod
            (lambda _
              (chmod "katenary" #o555))))))
    (home-page "https://github.com/metal3d/katenary")
    (supported-systems '("x86_64-linux"))
    (synopsis "Convert docker-compose to a configurable helm chart")
    (description
     "Katenary is a tool to help to transform docker-compose files to a working
Helm Chart for Kubernetes.")
    (license license:expat)))

(define-public kubectl
  (package
    (name "kubectl")
    (version "1.28.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dl.k8s.io/release/v" version "/bin/linux/amd64/kubectl"))
              (sha256
               (base32
                "1qbl4a2xv795apvbwahdb9kzcm2wys0am1c72as3iavgs3wxd9z7"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("kubectl" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (copy-file #$source "./kubectl")
              (chmod "kubectl" #o644)))
          (add-before 'install 'chmod
            (lambda _
              (chmod "kubectl" #o555))))))
    (home-page "https://github.com/kubernetes/kubectl")
    (supported-systems '("x86_64-linux"))
    (synopsis "Kubernetes command line tool")
    (description
     "kubectl allows you to run commands against Kubernetes clusters. You can
use kubectl to deploy applications, inspect and manage cluster resources, and
view logs.")
    (license license:asl2.0)))
