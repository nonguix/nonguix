(define-module (nongnu packages sml)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license-gnu:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl))

(define-public mosml
  (let ((commit "13c581aec46eea134e478f2e2b6456278e36ecce")
        (revision "0"))
    (package
      (name "mosml")
      (version (git-version "2.10.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kfl/mosml")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "191x0kcpybw5zvxsgl5if9x53b3w8zm0z72xfg1x5jwqslmk9bpr"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target) " --std=gnu89")
                     (string-append "PREFIX=" #$output)
                     "-C" "src")
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)    ;No configure script.
                 (add-after 'unpack 'fix-makefile-bin-sh
                   (lambda _
                     (substitute* "src/Makefile.inc"
                       (("SHELL=/bin/sh")
                        (string-append "SHELL=" (which "sh"))))))
                 (delete 'check)
                 (add-after 'install 'check
                   (lambda* (#:key tests? #:allow-other-keys)
                     (when tests?
                       (invoke "make" "-C" "src/test"
                               (string-append
                                "MOSML=" #$output "/bin/mosml"))))))))
      (native-inputs (list perl))
      (inputs (list gmp))
      (home-page "https://mosml.org")
      (synopsis
       "Moscow ML implementation of the Standard ML programming language")
      (description
       "Moscow ML is a light-weight implementation of Standard ML (SML), a
strict functional language used in teaching and research")
      (license
       (list (license-gnu:fsf-free
              "file://copyright/copyrght.att"
              "Standard ML of New Jersey License")
             ;; XXX: License issue: the two licenses below are conflict.
             (license:nonfree
              "file://copyright/copyrght.cl"
              "Copyright notice from INRIA, no commercial distribution, apply
same conditions to derivative works")
             license-gnu:gpl2)))))
