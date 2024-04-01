;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Isaac Young <isyoung@pm.me>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>

(define-module (nongnu packages coq)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages coq)
  #:use-module (gnu packages ocaml)
  #:use-module (nonguix licenses))

(define-public compcert
  (package
    (name "compcert")
    (version "3.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/AbsInt/compcert")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yvj9g144p26k7674vcai12sh3jahs64ny9pana9zla16nxxpmcm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'allow-newer-coq-version
           (lambda _
             (substitute* "configure"
               (("8.15.2") "8.17.1"))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((system ,(match (or (%current-target-system) (%current-system))
                              ("armhf-linux" "arm-eabihf")
                              ("i686-linux" "x86_32-linux")
                              (s s))))
               (format #t "Building for ~a~%" system)
               (invoke "./configure" system "-prefix"
                       (assoc-ref outputs "out")))
             #t))
         (add-after 'install 'install-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (install-file
                   file
                   (string-append
                     (assoc-ref outputs "out")
                     "/lib/coq/user-contrib/compcert/" (dirname file))))
               (find-files "." ".*.vo$"))
             #t)))
       #:tests? #f))
    ;; MIPS is not supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))
    (native-inputs
     (list coq
           ocaml
           ocaml-findlib)) ; for menhir --suggest-menhirlib
    (inputs
     (list ocaml-menhir))
    (home-page "http://compcert.inria.fr")
    (synopsis "Certified C compiler")
    (description "The CompCert project investigates the formal verification of
realistic compilers usable for critical embedded software.  Such verified
compilers come with a mathematical, machine-checked proof that the generated
executable code behaves exactly as prescribed by the semantics of the source
program. By ruling out the possibility of compiler-introduced bugs, verified
compilers strengthen the guarantees that can be obtained by applying formal
methods to source programs.

The main result of the project is the CompCert C verified compiler, a
high-assurance compiler for almost all of the C language (ISO C99), generating
efficient code for the PowerPC, ARM, RISC-V and x86 processors.")
    ;; actually the "INRIA Non-Commercial License Agreement"
    ;; a non-free license.
    (license (nonfree "file:///LICENSE"))))
