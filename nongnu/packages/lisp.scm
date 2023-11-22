;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2023 André A. Gomes <andremegafone@gmail.com>

(define-module (nongnu packages lisp)
  #:use-module (ice-9 match)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages node)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  ;; #:use-module (gnu packages lisp-check)
  #:use-module (nongnu packages electron)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system asdf)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:)
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
                "1zxajn238aibsv0qknm5kiqjiplb4ggynjsxar390rwznh57qc46"))))
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
         (add-after 'install 'update-license
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute*
                 (string-append (assoc-ref outputs "out")
                                "/share/allegro-cl/devel.lic")
               ((";; License created on January 25, 2021, 8:32:19\\.")
                ";; License created on January 17, 2023, 10:42:54.")
               ((";; Expiration date: 2023-1-31 00:00:00")
                ";; Expiration date: 2024-1-31 00:00:00"))
             #t))
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

(define-public sbcl-cl-electron
  (let ((commit "458a60d8c9baae71906294ffae891c3d0686c672")
        (revision "2"))
    (package
      (name "sbcl-cl-electron")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atlas-engineer/cl-electron")
               (commit commit)))
         (file-name (git-file-name "cl-electron" version))
         (sha256
          (base32 "0bmnh0xl5pvjv4pdb4a37x87zlyzr5fy7cyaws69p4p4rgzszzv8"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list ;; sbcl-lisp-unit2
                           sbcl))
      (inputs (list electron node
                    sbcl-cl-json sbcl-iolib sbcl-cl-str sbcl-nclasses
                    sbcl-parenscript sbcl-bordeaux-threads))
      (synopsis "Common Lisp interface to Electron")
      (home-page "https://github.com/atlas-engineer/cl-electron")
      (description "@command{cl-electron} is a binding to Electron for
Common Lisp.")
      (license license:bsd-3))))

(define-public cl-electron
  (sbcl-package->cl-source-package sbcl-cl-electron))

(define-public ecl-cl-electron
  (sbcl-package->ecl-package sbcl-cl-electron))
