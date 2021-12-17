;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Isaac Young <isyoung@pm.me> 
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
    (version "3.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/AbsInt/compcert")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19rmx8r8v46101ij5myfrz60arqjy7q3ra3fb8mxqqi3c8c4l4j6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
     `(("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib); for menhir --suggest-menhirlib
       ("coq" ,coq)))
    (inputs
     `(("menhir" ,ocaml-menhir)))
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
