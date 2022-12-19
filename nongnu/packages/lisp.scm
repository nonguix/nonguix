;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Pierre Neidhardt <mail@ambrevar.xyz>

(define-module (nongnu packages lisp)
  #:use-module (ice-9 match)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix license:))

;; TODO: Split into differents outputs:
;; - emacs: for ELI.  Or just remove it and use our own up-to-date package?
;; - doc: For the doc. (> 40+10 MiB).
;; - gui: For the express GUI (including all the JS stuff).
;; - out: Everything else: alisp, alisp.dxl, etc.
(define-public allegro-cl
  (package
    (name "allegro-cl")
    (version "10.1")
    (source (origin
              (method url-fetch)
              (uri
               (let ((arch1 (match (or (%current-target-system) (%current-system))
                              ("armhf-linux" "linuxarm64")
                              ("i686-linux" "linux86")
                              (s "linuxamd64.64")))
                     (arch2 (match (or (%current-target-system) (%current-system))
                              ("armhf-linux" "linux-aarch64")
                              ("i686-linux" "linux-x86")
                              (s "linux-x64"))))
                 (string-append "https://franz.com/ftp/pub/acl"
                                version "express/" arch1
                                "/acl" version "express-" arch2 ".tbz2")))
              (sha256
               (base32
                "0ir1irpq5hhcmy0yp5p2jpnq5if1gr1fgxybqyvppx1j1jdfkcsp"))))
    (build-system binary-build-system)
    (inputs (list bash-minimal zlib openssl))
    (arguments
     ;; FIXME: Patchelf the .so files as well?  Does not seem necessary.
     `(#:patchelf-plan '(("alisp")
                         ("allegro-express"))
       #:install-plan
       '(("." "share/allegro-cl/"
          ;; The "eli" Emacs interface is outdated and broken.
          ;; Use the Nonguix emacs-eli package instead.
          #:exclude ("update.sh" "eli")))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           ;; Guix does not know how to extract .tbz2.
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "source")
             (invoke "tar"
                     "--directory=source" "-xvf" (assoc-ref inputs "source")
                     "--strip-components" "1")
             (chdir "source")))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zlib-lib (string-append (assoc-ref inputs "zlib") "/lib"))
                    (openssl-lib (string-append (assoc-ref inputs "openssl") "/lib"))
                    (openssl-bin (string-append (assoc-ref inputs "openssl") "/bin"))
                    (bash-bin (string-append (assoc-ref inputs "bash") "/bin"))
                    (wrapper (string-append out "/bin/allegro-express"))
                    (bin (string-append out "/share/allegro-cl/"
                                        "allegro-express"))
                    (cli-wrapper (string-append out "/bin/alisp"))
                    (cli-bin (string-append out "/share/allegro-cl/"
                                            "alisp")))
               (mkdir-p (dirname wrapper))
               (pk 'BIN bin)
               (make-wrapper wrapper bin
                             `("LD_LIBRARY_PATH" ":" prefix
                               ,(list zlib-lib openssl-lib))
                             `("PATH" ":" prefix
                               ,(list openssl-bin bash-bin)))
               (make-wrapper cli-wrapper cli-bin))
             #t)))))
    (native-search-paths
     ;; XDG_DATA_DIRS is required to find the cl-* libraries.
     (list
      (search-path-specification
       (variable "XDG_DATA_DIRS")
       (files '("share")))))
    (synopsis "Commercial software implementation of the language Common Lisp")
    (description
     "This is a Lisp implementation by Franz Inc.
It includes AllegroCache, a command line based REPL as well as a web-based GUI.

The Express Edition has a heap limitation compared to the commercial versions
of Allegro CL.

You can run the GUI from a container, for instance:

  guix shell --container --network allegro-cl -- allegro-express

then open a browser at http://localhost:PORT, where PORT is the indicated port.")
    (home-page "https://franz.com/products/allegrocl/")
    (license (license:nonfree
              "https://franz.com/ftp/pub/legal/ACL-Express-20170301.pdf"))))
