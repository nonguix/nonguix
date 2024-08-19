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
    (version "0.32.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/derailed/k9s/releases/download/v"
                    version "/k9s_Linux_amd64.tar.gz"))
              (sha256
               (base32
                "18yf4vr4pgdl5ssijmpf45amdasjrd3mbgnsp1cjnadszvsiphrk"))))
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
    (version "1.31.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dl.k8s.io/release/v" version "/bin/linux/amd64/kubectl"))
              (sha256
               (base32
                "0dr40ckdj65ka6ndp8knyprh1k0nx6vg8yyg7p6c1lc49b3as9vw"))))
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

(define-public kompose
  (package
    (name "kompose")
    (version "1.30.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/kubernetes/kompose/releases/download/v"
                    version "/kompose-linux-amd64"))
              (sha256
               (base32
                "0sy3ci7s2dkjigasyv01nm1vg30wwhmdc0cmglzb23ws8bfrfjlh"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("kompose" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (copy-file #$source "./kompose")))
          (add-before 'install 'chmod
            (lambda _
              (chmod "kompose" #o555))))))
    (home-page "https://kompose.io")
    (supported-systems '("x86_64-linux"))
    (synopsis "Go from Docker Compose to Kubernetes")
    (description
     "Kompose is a conversion tool for Docker Compose to container orchestrators
such as Kubernetes (or OpenShift).")
    (license license:expat)))

(define-public helm-kubernetes
  (package
    (name "helm-kubernetes")
    (version "3.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://get.helm.sh/helm-v" version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "1d99c506shnz5cr9xhkrla5r82nan7v3hz631jqflicd376i68qv"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("linux-amd64/helm" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "tar" "-xvf" #$source)))
          (add-before 'install 'chmod
            (lambda _
              (chmod "linux-amd64/helm" #o555))))))
    (home-page "https://helm.sh")
    (supported-systems '("x86_64-linux"))
    (synopsis "The package manager for Kubernetes")
    (description
     "Helm helps you manage Kubernetes applications - Helm Charts help you
define, install, and upgrade Kubernetes applications.")
    (license license:asl2.0)))

(define-public kind
  (package
    (name "kind")
    (version "0.23.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://kind.sigs.k8s.io/dl/v" version "/kind-linux-amd64"))
              (sha256
               (base32
                "1356qhxkcbgs1lnqhf7qfl6f0y67rfp1i1li3agxmqzvkw3f71hx"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("kind" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (copy-file #$source "./kind")
              (chmod "kind" #o644)))
          (add-before 'install 'chmod
            (lambda _
              (chmod "kind" #o555))))))
    (home-page "https://kind.sigs.k8s.io")
    (synopsis "Tool for running local Kubernetes clusters using Docker containers")
    (description "kind (Kubernetes in Docker) is designed for creating and managing
local Kubernetes clusters using Docker containers as nodes. It provides
a fast and straightforward way to run Kubernetes for development and
testing tasks.

Supporting multi-node cluster configurations, kind is ideal for testing
more complex, real-world scenarios without demanding extensive resources.
It’s a lightweight, portable, and configurable solution useful in
continuous integration (CI) workflows.")
    (license license:asl2.0)))
